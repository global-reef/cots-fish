library(brms)
library(posterior)
library(dplyr)
library(purrr)
library(tibble)
library(fishualize)
library(ggplot2)
library(rfishbase)
library(cowplot)
library(ggtext)
modeled_species <- c("Angelfish", "Butterflyfish", "Cleaner_Wrasse", "Emperorfish",
                        "Parrotfish", "Rabbitfish", "Red_Breast", "Slingjaw",
                        "Squirrel.Soldier", "Sweetlips", "Thicklip", "Trevally",
                        "Triggerfish", "lrg_Grouper", "lrg_Snapper", "sml_Grouper", "sml_Snapper")

spp_wide <- cotsxfish %>%
  pivot_wider(
    id_cols = c(Survey_ID, Site, cots_density_ha),
    names_from = Species,
    values_from = fish_density_ha
  )

# Create separate formulas for each species
form_list <- lapply(modeled_species, function(sp) {
  bf(as.formula(paste0("`", sp, "` ~ s(cots_density_ha) + (1 | Site)")), family = negbinomial())
})

#### run the model #### 
fit_spp <- brm(
  formula = do.call(mvbf, form_list),
  data = spp_wide,
  chains = 4, cores = 4,
  iter = 2000, warmup = 500,
  control = list(adapt_delta = 0.99)
)
summary(fit_spp)
plot(fit_spp)

#### plot conditional effects ####

# Ensure Species names match those in the model
valid_responses <- c("Angelfish", "Butterflyfish", "CleanerWrasse", "Emperorfish",
                     "Parrotfish", "Rabbitfish", "RedBreast", "Slingjaw",
                     "SquirrelSoldier", "Sweetlips", "Thicklip", "Trevally",
                     "Triggerfish", "lrgGrouper", "lrgSnapper", "smlGrouper", "smlSnapper")

# If necessary, add this as a column to functional_taxa
functional_taxa <- functional_taxa |>
  dplyr::mutate(Resp = gsub("_", "", Species)) |>  # assumes Species was e.g., Red_Breast
  dplyr::filter(Resp %in% valid_responses)

# Extract conditional effects using the correct names
ce_list4 <- lapply(functional_taxa$Resp, function(resp) {
  ce <- conditional_effects(fit_spp, resp = resp, effects = "cots_density_ha")[[1]]
  ce$Resp <- resp
  return(ce)
})

# Combine and rejoin sci_name and group
effects_df4 <- bind_rows(ce_list4) %>%
  left_join(functional_taxa, by = "Resp") 



# add raw observations to the plots 
# Convert wide data to long for raw observations
spp_long <- spp_wide %>%
  pivot_longer(cols = all_of(modeled_species),
               names_to = "Species",
               values_to = "fish_density") %>%
  mutate(Resp = gsub("_", "", Species))

library(tibble)
library(fishualize)
library(png)
library(grid)

fish_shapes <- tibble::tribble(
  ~sci_name,                                 ~family,           ~option,
  "Scarus spp.",                             "Labridae",        "Scarus.oviceps",
  "Siganus spp.",                            "Siganidae",       "Siganus.virgatus",
  "Chaetodon spp.",                          "Chaetodontidae",  "Chaetodon.trifasciatus",
  "Pomacanthus spp.",                        "Pomacanthidae",   "Pomacanthus.imperator",
  "Labroides dimidiatus",                    "Labridae",        "Labroides.dimidiatus",
  "Hemigymnus melapterus",                   "Labridae",        "Chlorurus.sordidus",        # closest match
  "Cheilinus fasciatus",                     "Labridae",        "Epibulus.insidiator",       # closest match
  "Epibulus insidiator",                     "Labridae",        "Epibulus.insidiator",
  "Diagramma/ Plectorhinchus spp.",          "Haemulidae",      "Plectorhinchus.gibbosus",
  "Holocentridae spp.",                      "Holocentridae",   "Myripristis.violacea",
  "Balistidae spp.",                         "Balistidae",      "Balistapus.undulatus",
  "Lutjanus (<30cm) spp.",                   "Lutjanidae",      "Lutjanus.gibbus",
  "Lutjanus (>30 cm) spp.",                  "Lutjanidae",      "Lutjanus.gibbus",
  "Caranx spp.",                             "Carangidae",      "Caranx.melampygus",
  "Lethrinus spp.",                          "Lethrinidae",     "Lethrinus.nebulosus",
  "Cephalopholis/ Epinephelus spp.",         "Serranidae",      "Epinephelus.merra",
  "Epinephelus (>30cm)/Plectropomus spp.",   "Serranidae",      "Epinephelus.malabaricus"
)

# Construct shape filename from family and option
fish_shapes <- fish_shapes %>%
  mutate(
    shape_file = paste0(family, "_", option, ".png"),
    shape_path = file.path("shapes", shape_file)  # adjust path if needed
  )

effects_df4 <- effects_df4 %>%
  mutate(display_name = case_when(
    sci_name == "Epinephelus (>30cm)/Plectropomus spp." ~ "Epinephelus /\nPlectropomus spp.",
    sci_name == "Cephalopholis/ Epinephelus spp." ~ "Cephalopholis /\nEpinephelus spp.",
    sci_name == "Diagramma/ Plectorhinchus spp." ~ "Diagramma /\nPlectorhinchus spp.",
    TRUE ~ sci_name
  ))


add_fish_shape <- function(sci_name, x = 0.95, y = 0.95, base_height = 0.2, alpha = 0.6) {
  shape_row <- fish_shapes %>% filter(sci_name == !!sci_name)
  if (nrow(shape_row) == 0 || !file.exists(shape_row$shape_path)) return(NULL)
  
  img <- tryCatch(readPNG(shape_row$shape_path, native = FALSE), error = function(e) NULL)
  if (is.null(img)) return(NULL)
  
  # Add or modify alpha layer
  if (length(dim(img)) == 3) {
    if (dim(img)[3] == 3) {
      alpha_layer <- array(alpha, dim = c(dim(img)[1], dim(img)[2], 1))
      img <- abind::abind(img, alpha_layer, along = 3)
    } else if (dim(img)[3] == 4) {
      img[,,4] <- img[,,4] * alpha
    }
  }
  
  # Use original aspect ratio to compute width from height
  img_height <- dim(img)[1]
  img_width  <- dim(img)[2]
  aspect_ratio <- img_width / img_height  # width = height * aspect_ratio
  
  annotation_custom(
    rasterGrob(img, x = x, y = y,
               width = unit(base_height * aspect_ratio, "npc"),
               height = unit(base_height, "npc"),
               just = c("right", "top"),
               interpolate = TRUE),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )
}



plots_by_group6 <- lapply(unique(functional_taxa$Functional_Group), function(group_name) {
  df_ce <- effects_df4 %>% filter(Functional_Group == group_name)
  df_raw <- spp_long %>%
    left_join(functional_taxa, by = "Resp") %>%
    filter(Functional_Group == group_name)
  
  plots <- lapply(split(df_ce, df_ce$sci_name), function(df_species) {
    species <- unique(df_species$sci_name)
    df_raw_species <- df_raw %>% filter(sci_name == species)
    
    p <- ggplot(df_species, aes(x = cots_density_ha, y = estimate__)) +
      geom_point(
        data = df_raw_species,
        aes(x = cots_density_ha, y = fish_density),
        inherit.aes = FALSE,
        alpha = 0.4, size = 1.3, color = "#008080"
      ) +
      geom_line(color = "#007A87", lwd = 1) + 
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "#66BFA6", alpha = 0.2) +
      labs(
        x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
        y = expression("Fish density (" * ha^{-1} * ")"),
        title = species
      ) +
      theme_minimal(base_size = 16) + theme_clean +
      theme(
        axis.title.x = element_text(size = 14),  # X-axis title
        axis.title.y = element_text(size = 14),  # Y-axis title
        strip.text = element_text(face = "italic", size = 16),  # Facet strip titles
        plot.title = element_text(face = "italic", size = 16, hjust = 0.5)
      ) +
      add_fish_shape(species)
    
    return(p)
  })
  
  # Combine into patchwork layout if desired, or return list
  patchwork::wrap_plots(plots)
})



names(plots_by_group6) <- unique(functional_taxa$Functional_Group)

# Access individual groups 
herb_spp <- plots_by_group6$Herbivore
print(herb_spp)
invert_spp <- plots_by_group6$Invertivore
meso_spp<- plots_by_group6$Mesopredator
htlp_spp <- plots_by_group6$HTLP

print(herb_spp)
print(invert_spp)
print(meso_spp)
print(htlp_spp)



library(patchwork)

# Remove individual axis labels inside each plot
spp_plots <- lapply(split(effects_df4, effects_df4$sci_name), function(df_spp) {
  sci <- unique(df_spp$sci_name)
  label <- unique(df_spp$display_name)
  
  base_plot <- ggplot(df_spp, aes(x = cots_density_ha, y = estimate__)) +
    geom_line(color = "#007A87", lwd = 1) + 
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "#66BFA6", alpha = 0.2) +
    labs(title = label) +
    theme_minimal(base_size = 13) + theme_clean +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(face = "italic", size = 13, hjust = 0.5)
    )
  
  fish_layer <- add_fish_shape(sci)
  if (!is.null(fish_layer)) {
    base_plot + fish_layer
  } else {
    base_plot
  }
})

# Combine all plots
plot_all_spp2 <- wrap_plots(spp_plots, guides = "collect") +
  plot_annotation(
    title = " ",
    theme = theme(
      plot.title = element_text(face = "italic", size = 16, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    )
  )

  # optional, only if you want Markdown-style italics

final_plot <- plot_all_spp2 +
  plot_annotation(
    theme = theme(
      axis.title.x = element_text(family = "Times New Roman", size = 15, margin = margin(t = 10)),
      axis.title.y = element_text(family = "Times New Roman", size = 15, margin = margin(r = 10))
    )
  ) &
  labs(
    x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
    y = expression("Fish density (" * ha^{-1} * ")")
  )

# Print it
print(final_plot)

###########################################################

###### extract posterior predictions ###### 
# Extract posterior draws as a data frame
post <- as_draws_df(fit_spp)

# Define species of interest
spp_terms <- c(
  "Angelfish", "Butterflyfish", "CleanerWrasse", "Emperorfish", "Parrotfish",
  "Rabbitfish", "RedBreast", "Slingjaw", "SquirrelSoldier", "Sweetlips",
  "Thicklip", "Trevally", "Triggerfish", "lrgGrouper", "lrgSnapper", "smlGrouper", "smlSnapper"
)

# Function to compute posterior probabilities
get_posterior_probs <- function(fit, species) {
  post <- as_draws_df(fit)
  
  results <- lapply(species, function(sp) {
    term <- paste0("bs_", sp, "_scots_density_ha_1")
    if (!term %in% colnames(post)) return(NULL)
    samples <- post[[term]]
    data.frame(
      Species = sp,
      P_gt_0 = mean(samples > 0, na.rm = TRUE),
      P_lt_0 = mean(samples < 0, na.rm = TRUE)
    )
  })
  
  do.call(rbind, results)
}

# Run function
posterior_probs <- get_posterior_probs(fit_spp, spp_terms)
print(posterior_probs)
# print(post_probs_summary)
summary(fit_spp)
