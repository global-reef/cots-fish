install.packages("gitcreds")
library(gitcreds)
gitcreds::gitcreds_set()



# Install remotes if you don't already have it
install.packages("remotes")

## Sys.setenv(GITHUB_PAT = " ## TOKEN ### ") # github classic token 
remotes::install_github("stan-dev/cmdstanr")


library(cmdstanr)
install_cmdstan()
cmdstanr::cmdstan_version()



options(download.file.method = "libcurl")
remotes::install_github("nschiett/fishualize", force = TRUE)
library(fishualize)
