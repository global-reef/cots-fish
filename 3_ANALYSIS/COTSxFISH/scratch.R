spp_wide <- spp_wide %>%
  mutate(Site_clean = tolower(Site),
         CoTS_group = case_when(
           Site_clean %in% c("green rock", "red rock", "twins") ~ "High",
           Site_clean %in% c("aow leuk", "shark island") ~ "Low",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(CoTS_group))


library(tidybayes)

trigger_preds <- spp_wide %>%
  add_epred_draws(fit_spp, resp = "Triggerfish", ndraws = 1000)


ggplot(trigger_preds, aes(x = .epred, fill = CoTS_group)) +
  geom_density(alpha = 0.5) +
  labs(x = "Predicted Triggerfish Density", y = "Density",
       title = "Posterior predicted triggerfish densities by CoTS site group") +
  xlim(0, 75) +
  scale_fill_manual(
    values = c("High" = "#006400",  # dark green
               "Low" = "#56B4E9"),  # light blue
    labels = c("High Density Sites", "Low Density Sites")
  ) +
  theme_minimal() + theme_clean

trigger_preds %>%
  group_by(CoTS_group) %>%
  summarise(
    mean = mean(.epred),
    median = median(.epred),
    ci_lower = quantile(.epred, 0.025),
    ci_upper = quantile(.epred, 0.975)
  )
