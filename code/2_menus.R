# SCRIPT 2 OF XX
# [CITATION HERE]
# All code in this script written by Daniel A. Zaltz, PhD, MPH and Caroline Middleton, MS, HBSc

# This script loads the menu algorithm which we developed in the first script and generates
# the total simulated sample for further analysis


# 0. setup --------------------------------------------------------------------------
library(here)
library(tidyverse)
meals <- read_rds(here("data/[DATALOCATIONHERE].rds"))
ltc_make_menu <- read_rds(here("functions/ltc_make_menu.rds"))

# 1. 10k days ------------------------------------------------------------------
ltc_1e4 <- map(1:1e4, \(x){
   ltc_make_menu(meals) |>
      mutate(day=x, .before=1)
}, .progress = TRUE)

write_rds(ltc_1e4, here("data/[DATALOCATIONHERE].rds"))
