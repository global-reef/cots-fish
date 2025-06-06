library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)

# ==== Load raw data ====
raw_cots <- read_csv(cots_path)
raw_fish <- read_csv(fish_path)
raw_fish_timed <- read_csv(fish_path_timed)

# ==== Fix types in timed fish ====
raw_fish_timed <- raw_fish_timed %>%
  mutate(
    Eel = as.numeric(Eel),
    Site = tolower(trimws(Site))
  )

# ==== Merge timed and original fish ====
raw_fish <- raw_fish %>%
  mutate(Site = tolower(trimws(Site)))
raw_fish <- bind_rows(raw_fish, raw_fish_timed)

# ==== Functional Group Reference ====
functional_taxa <- tibble::tribble(
  ~Species,            ~Functional_Group, ~Genus,                ~Species_epithet, ~sci_name,
  "Parrotfish",        "Herbivore",       "Scarus",              "spp.",           "Scarus spp.",
  "Rabbitfish",        "Herbivore",       "Siganus",             "spp.",           "Siganus spp.",
  "Butterflyfish",     "Invertivore",       "Chaetodon",           "spp.",          "Chaetodon spp.",
  "Angelfish",         "Invertivore",     "Pomacanthus",         "spp.",           "Pomacanthus spp.",
  "Cleaner_Wrasse",    "Invertivore",     "Labroides",           "dimidiatus",     "Labroides dimidiatus",
  "Batfish",           "Invertivore",     "Ephippidae",          "spp.",           "Ephippidae spp.",
  "Thicklip",          "Invertivore",     "Hemigymnus",          "melapterus",     "Hemigymnus melapterus",
  "Red_Breast",        "Invertivore",     "Cheilinus",           "fasciatus",      "Cheilinus fasciatus",
  "Slingjaw",          "Invertivore",     "Epibulus",            "insidiator",     "Epibulus insidiator",
  "Sweetlips",         "Invertivore",     "Diagramma/Plectorhinchus", "spp.",     "Diagramma/Plectorhinchus spp.",
  "Squirrel.Soldier",  "Invertivore",     "Holocentridae",       "spp.",           "Holocentridae spp.",
  "Triggerfish",       "Invertivore",     "Balistidae",          "spp.",           "Balistidae spp.",
  "Porcupine.Puffer",  "Invertivore",     "Diodon/Tetraodon",    "spp.",           "Diodon/Tetraodon spp.",
  "Ray",               "Mesopredator",    "Taeniura/Neotrygon",  "spp.",           "Taeniura/Neotrygon spp.",
  "sml_Snapper",       "Mesopredator",    "Lutjanus",            "spp.",           "Lutjanus (<30cm) spp.",
  "lrg_Snapper",       "HTLP",            "Lutjanus",            "spp.",           "Lutjanus (>30 cm) spp.",
  "Eel",               "Mesopredator",    "Gymnothorax",         "spp.",           "Gymnothorax spp.",
  "Trevally",          "HTLP",            "Caranx",              "spp.",           "Caranx spp.",
  "Emperorfish",       "Mesopredator",    "Lethrinus",           "spp.",           "Lethrinus spp.",
  "sml_Grouper",       "Mesopredator",    "Cephalopholis/Epinephelus", "spp.",     "Cephalopholis/Epinephelus spp.",
  "lrg_Grouper",       "HTLP",            "Epinephelus",         "spp.",           "Epinephelus (>30cm) spp.",
  "Barracuda",         "HTLP",            "Sphyraena",           "spp.",           "Sphyraena spp."
)

# ==== Optional check for zero-inflated species ====
raw_fish %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ mean(.x == 0, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Zero_Proportion") %>%
  arrange(desc(Zero_Proportion))

zero_inflated <- c("Ray", "Eel", "Barracuda", "Batfish", "Porcupine.Puffer")  

# ==== Clean and pivot fish ====
fish_long <- raw_fish %>%
  filter(Researcher != "Keisha") %>%
  rename(
    sml_Grouper = `Grouper<30`,
    lrg_Grouper = `Grouper>30`,
    lrg_Snapper = `Snapper>30`,
    sml_Snapper = `Snapper<30`,
    Squirrel.Soldier = `Squirrel/Soldier`,
    Porcupine.Puffer = `Porcupine/Puffer`
  ) %>%
  mutate(Date = mdy(`Date_mm/dd/yy`)) %>%
  filter(!Date %in% as.Date(c("2025-01-25", "2024-04-01", "2024-05-24"))) %>%
  pivot_longer(
    cols = all_of(functional_taxa$Species),
    names_to = "Species", values_to = "Count"
  ) %>%
  filter(!Species %in% zero_inflated) %>%
  left_join(functional_taxa, by = "Species") %>%
  mutate(Month = floor_date(Date, "month"))

# ==== Aggregate fish ====
clean_fish <- fish_long %>%
  group_by(Site, Date, Month, Species, Functional_Group, sci_name) %>%
  summarise(fish = ceiling(mean(Count, na.rm = TRUE)), .groups = "drop")

# ==== Aggregate CoTS per Site-Month ====
cots_monthly <- raw_cots %>%
  mutate(Date = mdy(Date_mm.dd.yyyy),
         Month = floor_date(Date, "month"),
         Site = tolower(trimws(Site))) %>%
  mutate(Site = case_when(
    Site == "green rock wall" ~ "green rock",
    Site == "red rock wall" ~ "red rock",
    TRUE ~ Site
  )) %>%
  group_by(Site, Date) %>%
  summarise(CoTS_count = n(), .groups = "drop") %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Site, Month) %>%
  summarise(cots = ceiling(mean(CoTS_count)), .groups = "drop") %>%
  mutate(cots_density_ha = cots / 0.2)

# ==== Final Join: Preserve full fish survey resolution ====
cotsxfish <- clean_fish %>%
  mutate(Month = floor_date(Date, "month")) %>%
  left_join(cots_monthly, by = c("Site", "Month")) %>%
  filter(!is.na(cots_density_ha)) %>%
  mutate(fish_density_ha = fish / 0.2,
         Survey_ID = paste(Site, format(Date, "%d-%b-%y"), sep = "-"))

# ==== Save cleaned dataset ====
write_csv(cotsxfish, file.path(output_dir, "cotsxfish.csv"))

# ==== Completion Message ====
cat("✅ Analysis complete! Data and plots saved in:", output_dir, "\n")
