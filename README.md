COTSxFISH
=========

This repository contains data, code, and analysis for the COTSxFISH project, which investigates the relationship between crown-of-thorns sea star (Acanthaster spp.) outbreaks and reef fish assemblages in the Gulf of Thailand.

Overview
--------

The project aims to determine whether increased CoTS densities are associated with changes in the abundance and composition of reef fish communities, particularly across different trophic groups (e.g., herbivores, invertivores, mesopredators, predators). Analysis is based on diver surveys conducted at multiple reef sites with varying CoTS densities.

Repository Structure
--------------------

1_BACKGROUND/         - Literature (TBA)

2_DATA/               - Master survey data (CoTS and fish)  

3_ANALYSIS/           - Scripts: Data cleaning, Statistical models and outputs, Plots and Figures 

99_INSTALL.R          - Script to install/load required packages  'cmdrstan' - will need your own github token 

README.md             - This file  

Main Features
-------------

- Negative Binomial Bayesian GAMs (via `brms`) to model fish–CoTS relationships
- Functional group and species-level analyses
- Posterior visualization and uncertainty quantification
- Integrated CoTS and fish survey datasets

Getting Started
---------------

1. Clone the repository:
   git clone https://github.com/global-reef/cots-fish.git

2. Install R dependencies:
   source("99_INSTALL.R") # will need to update with your own GIS token ##

3. Run the analysis:
   source("3_ANALYSIS/00_RUN.R")

Notes
-----

- This project is maintained by the Global Reef team in Koh Tao, Thailand.
- Underwater survey data is subject to quality control and ongoing updates.
- GIS layers and raster products (e.g., bathymetry, habitat maps) are stored separately.

License
-------

This project is private and not licensed for redistribution. 
For collaboration inquiries, please contact scarlett@global-reef.com
