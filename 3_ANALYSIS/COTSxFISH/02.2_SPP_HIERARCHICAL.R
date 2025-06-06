## heriarchichal model 

cotsxfish <- cotsxfish %>% mutate(Species = factor(Species))
# Use partial pooling to model all species in one formula, with species-specific smooth terms.
fit_heir <- brm(
  fish_density_ha ~ s(cots_density_ha, by= Species, bs = "fs") + (1 | Site)+ (1 | Species),
  data = cotsxfish,
  family = negbinomial(),  
  backend = "cmdstanr",
  chains = 4, cores = 4, iter = 2000,
  control = list(adapt_delta = 0.99)
)
summary(fit_heir)

library(tidyverse)
library(brms)

# Your modeled species
modeled_species <- unique(cotsxfish$Species)

# Step 1: Extract conditional effects per species
effects_list2 <- lapply(modeled_species, function(sp) {
  ce <- conditional_effects(fit_heir, effects = "cots_density_ha",
                            conditions = data.frame(Species = sp))[[1]]
  ce$Species <- sp
  return(ce)
})
# Combine and join scientific names
effects_df2 <- bind_rows(effects_list2) %>%
  left_join(functional_taxa %>% select(Species, sci_name, Functional_Group), by = "Species")

# Plot
library(ggplot2)
ggplot(effects_df2, aes(x = cots_density_ha, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
  facet_wrap(~ sci_name, scales = "free_y") +
  labs(
    x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
    y = expression("Predicted fish density (" * ha^{-1} * ")"),
    title = "Species-specific responses to Crown-of-Thorns density"
  ) +
  theme_minimal(base_size = 13) + theme_clean + 
  theme(strip.text = element_text(size = 12, face = "italic"))



species_groups <- cotsxfish %>% distinct(Species, sci_name, Functional_Group)
effects_df3 <- bind_rows(effects_list) %>%
  left_join(species_groups, by = "Species")
plots_by_group <- effects_df3 %>%
  group_split(Functional_Group) %>%
  setNames(unique(effects_df3$Functional_Group)) %>%
  lapply(function(df) {
    ggplot(df, aes(x = cots_density_ha, y = estimate__)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
      facet_wrap(~ sci_name, scales = "free_y") +
      labs(
        x = expression("Crown-of-Thorns density (" * ha^{-1} * ")"),
        y = expression("Predicted fish density (" * ha^{-1} * ")"),
        title = paste0("Species-specific response: ", unique(df$Functional_Group))
      ) +
      theme_minimal(base_size = 13) + theme_clean + 
      theme(strip.text = element_text(size = 12, face = "italic"))
  })


# View one at a time
plots_by_group$Herbivore
plots_by_group$Invertivore
plots_by_group$Mesopredator
plots_by_group$HTLP
