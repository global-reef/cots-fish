# setup and data cleaning

# Set analysis date
analysis_date <- "2025.06.05"  # Update this for each new analysis
base_dir <- "~/Documents/1_GLOBAL REEF/0_PROJECTS/COTSxFISH"

# Define paths to raw data files inside 2_DATA
cots_path <- file.path(base_dir, "2_DATA", paste0(analysis_date, "_COTs_Abund_MASTER.csv"))
fish_path <- file.path(base_dir, "2_DATA", paste0(analysis_date, "_COTS_Fish_MASTER.csv"))
fish_path_timed <- file.path(base_dir, "2_DATA", paste0(analysis_date, "_TimedFishSurveys_Shallow_MASTER.csv"))


# Create a folder named with the date inside the working directory
output_dir <- file.path(getwd(), paste0("Analysis_", analysis_date))
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# create custom ggplot2 theme 
theme_clean <- theme_minimal(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

theme_set(theme_minimal(base_family = "Times New Roman"))

####### run the models #######
## set up 
# data clean 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/COTSxFISH/3_ANALYSIS/COTSxFISH/01_CLEAN.R")
# exploratory analysis 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/COTSxFISH/3_ANALYSIS/COTSxFISH/01.1_EXPLORE.R")
# install cmdrstan backend 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/COTSxFISH/3_ANALYSIS/COTSxFISH/99_INSTALL.R")


## modelling
# functional groups 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/COTSxFISH/3_ANALYSIS/COTSxFISH/02_MODEL_FUNGROUPS.R")
# total fish
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/COTSxFISH/3_ANALYSIS/COTSxFISH/02.1_TOTALFISH.R")
# species-specific analysis 
source("~/Documents/1_GLOBAL REEF/0_PROJECTS/COTSxFISH/3_ANALYSIS/COTSxFISH/02.3_MODEL_SPECIES.R")


##### final plot calls #####

# fungroups 
print(ce_with_raw)
print(ce_mv_plot)
# total fish
print(allfish)
# spp specific
print(plot_all_spp)
print(herb_spp)
print(invert_spp)
print(meso_spp)
print(htlp_spp)

save_analysis_plots <- function() {
  # Define the list of plots and filenames
  plots <- list(
    "fungroup_conditional_effects_raw.png" = ce_with_raw,
    "fungroup_conditional_effects_mv.png"  = ce_mv_plot,
    "total_fish.png"                       = allfish,
    "species_all_facets.png"              = plot_all_spp,
    "species_herbivores.png"              = herb_spp,
    "species_invertivores.png"            = invert_spp,
    "species_mesopredators.png"           = meso_spp,
    "species_htlp.png"                    = htlp_spp
  )
  
  # Loop through and save each one
  for (filename in names(plots)) {
    ggsave(
      filename = file.path(output_dir, filename),
      plot = plots[[filename]],
      width = 10, height = 6, dpi = 300
    )
  }
}

save_analysis_plots()
# ==== Completion Message ====
cat("✅ Analysis complete! Data and plots saved in:", output_dir, "\n")




## results summaries 
summary(fit_total)
summary(fit_mv)
summary(fit_spp)
