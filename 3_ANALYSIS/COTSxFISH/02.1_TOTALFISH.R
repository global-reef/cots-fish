## modeling total fish as a function of cots 

total_fish_df <- cotsxfish %>%
  group_by(Survey_ID, cots_density_ha, Site) %>%
  summarise(total_fish_density = sum(fish_density_ha), .groups = "drop")

library(brms)

fit_total <- brm(
  total_fish_density ~ s(cots_density_ha) + (1 | Site),
  data = total_fish_df,
  family = negbinomial(),
  chains = 4, iter = 2000, warmup = 1000,
  control = list(adapt_delta = 0.99),
  cores = 4
)

summary(fit_total)
ce_total <- conditional_effects(fit_total, effects = "cots_density_ha")$cots_density_ha

allfish <- ggplot() +
  geom_point(data = total_fish_df,
             aes(x = cots_density_ha, y = total_fish_density),
             alpha = 0.6, size = 1) +
  geom_line(data = ce_total,
            aes(x = cots_density_ha, y = estimate__),
            color = "black", linewidth = 1) +
  geom_ribbon(data = ce_total,
              aes(x = cots_density_ha, ymin = lower__, ymax = upper__),
              fill = "grey", alpha = 0.2) +
  theme_minimal(base_size = 13)  +  theme_clean + theme(strip.text = element_text(size = 13)) +
  labs(
    x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
    y = expression("Total fish density (" * ha^{-1} * ")"),
    title = "Effect of CoTS Density on Total Fish Density"
  ) 

print(allfish)



compute_total_fish_posterior <- function(fit) {
  draws <- as_draws_df(fit)
  term <- "bs_scots_density_ha_1"
  if (!term %in% names(draws)) stop("Coefficient not found.")
  samples <- draws[[term]]
  
  tibble(
    Term = "Total Fish",
    P_gt_0 = mean(samples > 0, na.rm = TRUE),
    P_lt_0 = mean(samples < 0, na.rm = TRUE),
    Trend = case_when(
      mean(samples > 0, na.rm = TRUE) > 0.75 ~ "Positive",
      mean(samples < 0, na.rm = TRUE) > 0.75 ~ "Negative",
      TRUE ~ "Uncertain"
    )
  )
}
posterior_probs_total <- compute_total_fish_posterior(fit_total)
print(posterior_probs_total)
summary(fit_total)
