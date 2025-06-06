# Install remotes if you don't already have it
install.packages("remotes")

## Sys.setenv(GITHUB_PAT = " ## TOKEN ### ") # github classic token 
remotes::install_github("stan-dev/cmdstanr")


library(cmdstanr)
install_cmdstan()
cmdstanr::cmdstan_version()
