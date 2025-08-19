# SCRIPT 4 OF XX
# [CITATION HERE]
# All code in this script written by Caroline Middleton, MS, HBSc

# This script calculates the proporation of items each day with different categories
# of CFSS scores

# For more on CFSS, see Lee et al. 2024. Applied Physiology, Nutrition, and Metabolism, 49(10), 1363-1376.


# 0. setup --------------------------------------------------------------------------

library(here)
library(tidyverse)

day <- read_rds(here("data/[DATALOCATIONHERE].rds"))
alldat <- read_rds(here("data/[DATALOCATIONHERE].rds"))

cfss_byday <- alldat |>
   mutate(
      cfss_cat = case_when(final_quint%in%c(1,2) ~ "num_q1q2",
                           final_quint%in%c(3) ~ "num_q3",
                           final_quint%in%c(4,5) ~ "num_q4q5")
   ) |>
   group_by(id) |>
   count(cfss_cat) |>
   mutate(n = ifelse(is.na(n), 0, n)) |>
   pivot_wider(names_from=cfss_cat, values_from=n) |>
   ungroup() |>
   mutate(tot_comps = num_q1q2+num_q3+num_q4q5) |>
   mutate(prop_q1q2=num_q1q2/tot_comps, .after = 2) |>
   mutate(prop_q3=num_q3/tot_comps, .after = 4) |>
   mutate(prop_q4q5=num_q4q5/tot_comps, .after = 6)

cfss_bymeal <- alldat |>
   mutate(
      cfss_cat = case_when(final_quint%in%c(1,2) ~ "num_q1q2",
                           final_quint%in%c(3) ~ "num_q3",
                           final_quint%in%c(4,5) ~ "num_q4q5")
   ) |>
   group_by(id, meal_type) |>
   count(cfss_cat) |>
   pivot_wider(names_from=c(cfss_cat), values_from=n) |>
   mutate(across(everything(), \(x) ifelse(is.na(x), 0, x))) |>
   ungroup() |>
   mutate(tot_comps = num_q1q2+num_q3+num_q4q5) |>
   mutate(prop_q1q2=num_q1q2/tot_comps, .after = 3) |>
   mutate(prop_q3=num_q3/tot_comps, .after = 5) |>
   mutate(prop_q4q5=num_q4q5/tot_comps, .after = 7) |>
   pivot_wider(names_from=meal_type, values_from = 3:9) |>
   select(1, 2, 8, 14, 20, 26, 32, 38, 3, 9, 15, 21, 27, 33, 39,
          4, 10, 16, 22, 28, 34, 40, 5, 11, 17, 23, 29, 35, 41,
          6, 12, 18, 24, 30, 36, 42, 7, 13, 19, 25, 31, 37, 43)

cfss_data <- left_join(cfss_byday, cfss_bymeal)

day <- day |> left_join(cfss_data)

write_rds(day, here("data/[DATALOCATIONHERE].rds"))
