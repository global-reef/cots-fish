library(brms)
library(posterior)
library(dplyr)
library(purrr)
library(tibble)
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

# Plot per group
plots_by_group4 <- split(effects_df4, effects_df4$Functional_Group)

plots_by_group4 <- lapply(plots_by_group4, function(df) {
  ggplot(df, aes(x = cots_density_ha, y = estimate__)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
    facet_wrap(~ sci_name, scales = "free_y") +
    labs(
      x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
      y = expression("Predicted fish density (" * ha^{-1} * ")")
    ) +
    theme_minimal(base_size = 13) + theme_clean + 
    theme(strip.text = element_text(face = "italic", size = 14))
})
herb_spp <- plots_by_group4$Herbivore
invert_spp <- plots_by_group4$Invertivore
meso_spp <- plots_by_group4$Mesopredator
htlp_spp <- plots_by_group4$HTLP

plot_all_spp <- ggplot(effects_df4, aes(x = cots_density_ha, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.2) +
  facet_wrap(~ sci_name, scales = "free_y") +
  labs(
    x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
    y = expression("Predicted fish density (" * ha^{-1} * ")"),
    title = "Species-specific responses to Crown-of-Thorns density"
  ) +
  theme_minimal(base_size = 13) + theme_clean + 
  theme(
    strip.text = element_text(face = "italic", size = 12)
  )
print(plot_all_spp)
print(herb_spp)
print(invert_spp)
print(meso_spp)
print(htlp_spp)

# add raw observations to the plots 
# Convert wide data to long for raw observations
spp_long <- spp_wide %>%
  pivot_longer(cols = all_of(modeled_species),
               names_to = "Species",
               values_to = "fish_density") %>%
  mutate(Resp = gsub("_", "", Species))

plots_by_group5 <- lapply(unique(functional_taxa$Functional_Group), function(group_name) {
  df_ce <- effects_df4 %>% filter(Functional_Group == group_name)
  df_raw <- spp_long %>%
    left_join(functional_taxa, by = "Resp") %>%
    filter(Functional_Group == group_name)
  
  ggplot(df_ce, aes(x = cots_density_ha, y = estimate__)) +
    geom_point(
      data = df_raw,
      aes(x = cots_density_ha, y = fish_density),
      inherit.aes = FALSE,
      alpha = 0.4,
      size = 1.3,
      color = "black"
    ) +
    geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.2) +
    facet_wrap(~ sci_name, scales = "free_y") +
    labs(
      x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
      y = expression("Predicted fish density (" * ha^{-1} * ")")
    ) +
    theme_minimal(base_size = 13) + theme_clean +
    theme(strip.text = element_text(face = "italic", size = 14))
})

spp_long_full <- spp_long %>%
  left_join(functional_taxa, by = "Resp")



# Access individual groups as before
herb_spp <- plots_by_group5$Herbivore
invert_spp <- plots_by_group5$Invertivore
meso_spp<- plots_by_group5$Mesopredator
htlp_spp <- plots_by_group5$HTLP

print(plot_all_spp)
print(herb_spp)
print(invert_spp)
print(meso_spp)
print(htlp_spp)

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
