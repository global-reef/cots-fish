## exploratory 



# to run it use : your_plot + theme_clean

# 1. Summary: number of surveys per site-month
cotsxfish %>%
  distinct(Site, Month) %>%
  count(Site) %>%
  arrange(desc(n))

# 2. Visualize fish density vs CoTS density
ggplot(cotsxfish, aes(x = cots_density_ha, y = fish_density_ha)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Functional_Group) +
  labs(x = "CoTS density (ha⁻¹)", y = "Fish density (ha⁻¹, log scale)",
       title = "Fish vs CoTS Density by Functional Group") +
  scale_y_log10()

# 3. Mean fish density by functional group
cotsxfish %>%
  group_by(Functional_Group) %>%
  summarise(
    mean_fish_density = mean(fish_density_ha, na.rm = TRUE),
    sd_fish_density = sd(fish_density_ha, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(mean_fish_density))

# 4. Fish-CoTS relationship with smoothing
ggplot(cotsxfish, aes(x = cots_density_ha, y = fish_density_ha)) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.5) +
  geom_smooth(method = "loess", span=1.5,  se = TRUE, color = "black") +
  facet_wrap(~ Functional_Group, scales = "free_y") +
  labs(x = "CoTS density (ha⁻¹)", y = "Fish density (ha⁻¹)",
       title = "Fish-CoTS Relationship by Functional Group")

# 5. Site-level spatial patterns
cotsxfish %>%
  group_by(Site) %>%
  summarise(
    mean_cots_density = mean(cots_density_ha, na.rm = TRUE),
    mean_fish_density = mean(fish_density_ha, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_cots_density))

# 6. Seasonality: fish density over time
ggplot(cotsxfish, aes(x = Month, y = fish_density_ha, group = Species)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ Functional_Group, scales = "free_y") +
  labs(title = "Fish Density Over Time", y = "Fish density (ha⁻¹)", x = "Month")

# Ensure Month is in Date format (e.g., "2025-04-01")
cotsxfish <- cotsxfish %>%
  mutate(Month = as.Date(Month))  # adjust if not already Date

# Plot CoTS density over time by site
ggplot(cotsxfish, aes(x = Month, y = cots_density_ha)) +
  geom_line(alpha = 0.6, color = "darkred") +
  facet_wrap(~ Site, scales = "free_y") +
  labs(
    title = "Seasonal Variation in CoTS Density by Site",
    y = "CoTS density (ha⁻¹)",
    x = "Month"
  ) +
  theme_minimal()

# 7. Distribution of fish density
hist(cotsxfish$fish_density_ha, breaks = 20, main = "Fish Density Distribution", xlab = "Fish density (ha⁻¹)")
summary(cotsxfish$fish_density_ha)





# 8. Functional group trends over time
cotsxfish %>%
  group_by(Month, Functional_Group) %>%
  summarise(total_density = sum(fish_density_ha, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Month, y = total_density, color = Functional_Group)) +
  geom_line() +
  labs(title = "Functional Group Densities Over Time", y = "Total Fish Density (ha⁻¹)", x = "Month")

# avg cots densities 
cotsxfish %>%
  select(Site, Month, cots_density_ha) %>%
  distinct() %>%  # avoid duplicates from multiple fish entries per month
  group_by(Site) %>%
  summarise(
    mean_cots_density = mean(cots_density_ha, na.rm = TRUE),
    sd_cots_density = sd(cots_density_ha, na.rm = TRUE),
    n_months = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_cots_density))

# total numbers 
total_cots <- nrow(raw_cots)
total_fish <- raw_fish %>%
     select(where(is.numeric)) %>%
     rowSums(na.rm = TRUE) %>%
     sum()
n_surveys <- cotsxfish %>%
  distinct(Site, Date) %>%
  nrow()
n_fish_surveys <- clean_fish %>%
     distinct(Site, Date) %>%
     nrow()
n_cots_surveys <- raw_cots %>%
  distinct(Survey_ID) %>%
  nrow()
cat("In total", total_fish, "fish and", total_cots, "CoTs were counted as part of this study, over", n_surveys,  "surveys")



### prep for gam 
# inspect distribtions 
hist(cotsxfish$fish_density_ha)
# likely need negative binomial or zero-inflated - will check both 

#colinearity check 

ggplot(cotsxfish, aes(x = Site, y = cots_density_ha)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "CoTS Density by Site", y = "CoTS density (ha⁻¹)")
kruskal.test(cots_density_ha ~ Site, data = cotsxfish) # significant, should def use site as random effect 
# this is frequentist but a good "quick checking tool" 

