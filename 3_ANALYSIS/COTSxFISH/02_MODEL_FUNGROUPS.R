# Load packages
library(brms)
library(cmdstanr)
library(tidybayes)
library(tidyverse)
library(ggplot2)
library(loo)

# Set cmdstanr as backend
options(brms.backend = "cmdstanr")

# Model formula
fish_gam_formula <- bf(fish_density_ha ~ s(cots_density_ha) + (1 | Site))


# test with one fun group first to check modelling framework 
herb_data <- cotsxfish %>% 
  filter(Functional_Group == "Herbivore")

fit_herb <- brm(
  formula = bf(fish_density_ha ~ cots_density_ha + (1 | Site)),
  data = herb_data,
  family = negbinomial(),
  backend = "cmdstanr",
  chains = 4, cores = 4, iter = 2000,
  control = list(adapt_delta = 0.99)
)
summary(fit_herb)
plot(fit_herb)
pp_check(fit_herb)
plot(conditional_effects(fit_herb))
#### ~~~~ full multivariate model ~~~~ ####
# pivot to wide first 
cotsxfish_wide <- cotsxfish %>%
  group_by(Site, Date, Month, Functional_Group, cots_density_ha) %>%
  summarise(fish_density_ha = sum(fish_density_ha), .groups = "drop") %>%
  pivot_wider(
    names_from = Functional_Group,
    values_from = fish_density_ha,
    values_fill = 0
  ) %>%
  rename(
    fish_herb = Herbivore,
    fish_invert = Invertivore,
    fish_meso = Mesopredator,
    fish_htlp = HTLP
  )


# run the model  
fit_mv <- brm(
  bf(fish_herb ~ s(cots_density_ha) + (1 | Site), family = negbinomial()) +
    bf(fish_invert ~ s(cots_density_ha) + (1 | Site), family = negbinomial()) +
    bf(fish_meso ~ s(cots_density_ha) + (1 | Site), family = negbinomial()) +
    bf(fish_htlp ~ s(cots_density_ha) + (1 | Site), family = negbinomial()) +
    set_rescor(FALSE),  # assumes residuals across responses are uncorrelated
  
  data = cotsxfish_wide,
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  iter = 2000,
  control = list(adapt_delta = 0.99)
)

summary(fit_mv)
# plot(fit_mv)
pp_check(fit_mv, resp = "fishherb")  # etc. for each group


### plotting results ####
# Extract conditional effects using actual model response names
effects_list <- list(
  herb = conditional_effects(fit_mv, effects = "cots_density_ha", resp = "fishherb", 
                             resolution = 200, method = "posterior_epred")$fishherb,
  invert = conditional_effects(fit_mv, effects = "cots_density_ha", resp = "fishinvert", 
                               resolution = 200, method = "posterior_epred")$fishinvert,
  meso = conditional_effects(fit_mv, effects = "cots_density_ha", resp = "fishmeso", 
                             resolution = 200, method = "posterior_epred")$fishmeso,
  htlp = conditional_effects(fit_mv, effects = "cots_density_ha", resp = "fishhtlp", 
                             resolution = 200, method = "posterior_epred")$fishhtlp
)


# Clean up facet labels
label_map <- c(
  herb = "Herbivores",
  invert = "Invertivores",
  meso = "Mesopredators",
  htlp = "HTLP"
)
# Combine with proper facet labels and enforce facet order
effects_df <- bind_rows(
  lapply(names(effects_list), function(name) {
    effects_list[[name]] %>%
      mutate(Functional_Group = factor(name, levels = c("herb", "invert", "meso", "htlp")))
  })
)

# Plot conditional effects (ie actual predicted values)
ce_mv_plot <- ggplot(effects_df, aes(x = cots_density_ha, y = estimate__)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
  facet_wrap(~ Functional_Group, labeller = labeller(Functional_Group = label_map), scales = "free_y") +
  labs(
    x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
    y = expression("Predicted fish density (" * ha^{-1} * ")"),
    title = "Predicted Effect of CoTS Density on Fish Functional Groups"
  ) +
  theme_minimal(base_size = 13) +
  theme_clean +  theme(strip.text = element_text(size = 12))
print(ce_mv_plot)

## adding raw data to plots 
raw_for_plot <- cotsxfish %>%
  filter(Functional_Group %in% c("Herbivore", "Invertivore", "Mesopredator", "HTLP")) %>%
  group_by(Survey_ID, cots_density_ha, Functional_Group) %>%
  summarise(fish_density = sum(fish_density_ha), .groups = "drop") %>%
  mutate(
    Functional_Group = factor(Functional_Group, 
                              levels = c("Herbivore", "Invertivore", "Mesopredator", "HTLP"))
  )
effects_df <- bind_rows(
  lapply(names(effects_list), function(name) {
    effects_list[[name]] %>%
      mutate(Functional_Group = factor(name,
                                       levels = c("herb", "invert", "meso", "htlp"),
                                       labels = c("Herbivore", "Invertivore", "Mesopredator", "HTLP")
      ))
  })
)


ce_with_raw <- ggplot() +
  geom_point(data = raw_for_plot,
             aes(x = cots_density_ha, y = fish_density),
             alpha = 0.5, size = 1) +
  geom_line(data = effects_df,
            aes(x = cots_density_ha, y = estimate__),
            color = "#007A87", linewidth = 1) +
  geom_ribbon(data = effects_df,
              aes(x = cots_density_ha, ymin = lower__, ymax = upper__),
              fill = "#66BFA6", alpha = 0.2) +
  facet_wrap(~ Functional_Group, scales = "free_y") +
  labs(
    x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
    y = expression("Fish density (" * ha^{-1} * ")"),
    title = "CoTS Effects on Fish Functional Groups (Model vs. Observed)"
  ) +
  theme_minimal(base_size = 15) +
  theme_clean +  theme(strip.text = element_text(size = 15))
print(ce_with_raw)
print(ce_mv_plot)


### posterior predictions ####

plot_functional_group_predictions <- function(model, data_long, data_wide) {
  # Define response names and pretty labels
  responses <- c("fishherb", "fishinvert", "fishmeso", "fishhtlp")
  labels <- c("Herbivores", "Invertivores", "Mesopredators", "HTLP")
  
  # Generate predicted draws for each response
  draws <- map2_dfr(
    responses, labels,
    ~ add_predicted_draws(data_wide, model, resp = .x, re_formula = NULL, allow_new_levels = TRUE) %>%
      mutate(Functional_Group = .y)
  )
  
  # Add observed data (long format, already includes raw fish_density_ha and group labels)
  observed <- data_long %>%
    filter(Functional_Group %in% labels)  # ensure no typos
  
  # Plot
  ggplot(draws, aes(x = cots_density_ha, y = .prediction)) +
    stat_lineribbon(aes(fill = after_stat(.width)), .width = c(0.66, 0.89, 0.97), alpha = 0.2) +
    facet_wrap(~Functional_Group, scales = "free_y") +
    scale_fill_distiller(palette = "Blues", direction = 2) +
    theme_clean +
    labs(
      x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
      y = expression("Posterior predicted fish density (" * ha^{-1} * ")"),
      title = "Posterior Predictions by Functional Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
}
plot_functional_group_predictions(fit_mv, cotsxfish, cotsxfish_wide)




compute_posterior_probs <- function(fit, terms) {
  draws <- as_draws_df(fit)
  term_names <- paste0("bs_", terms, "_scots_density_ha_1")
  
  probs <- purrr::map_dfr(term_names, function(term) {
    if (!term %in% names(draws)) {
      warning(paste("Missing term:", term))
      return(NULL)
    }
    samples <- draws[[term]]
    tibble(
      Term = term,
      P_gt_0 = mean(samples > 0, na.rm = TRUE),
      P_lt_0 = mean(samples < 0, na.rm = TRUE)
    )
  })
  
  probs <- probs %>%
    mutate(Trend = case_when(
      P_gt_0 > 0.75 ~ "Positive",
      P_lt_0 > 0.75 ~ "Negative",
      TRUE ~ "Uncertain"
    ))
  
  return(probs)
}
fg_terms <- c("fishherb", "fishinvert", "fishmeso", "fishhtlp")
posterior_probs_fg <- compute_posterior_probs(fit_mv, fg_terms)

# final things
print(ce_with_raw)
summary(fit_mv)
print(posterior_probs_fg)
