## 01. Download the raw data
##
## Downloads, once, the public dumps this project uses from
## github.com/shufinskiy/nba_data (season key 2023 = 2023-24, 2024 = 2024-25):
##   pbpstats  one row per possession (start type, start/end clock, score)
##   nbastats  one row per play-by-play event (stats.nba.com format)
##
## Files land in data/raw/, which is not tracked by git. Running this script
## again does nothing for files that are already there.
##
## Open the RProject (transition-after-makes.Rproj) before running this script.

# Packages ----------------------------------------------------------------

library(here)
library(tidyverse)
source(here("R", "download.R"))


# 1. Download -------------------------------------------------------------

seasons  <- c(2023, 2024)
datasets <- c("pbpstats", "nbastats")

files <- expand_grid(dataset = datasets, season = seasons) %>%
  mutate(path = map2_chr(dataset, season, download_nba_data))

files


# 2. First look -----------------------------------------------------------

poss <- read_csv(files$path[files$dataset == "pbpstats" & files$season == 2024])
glimpse(poss)
