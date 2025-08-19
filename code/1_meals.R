# SCRIPT 1 OF XX
# [CITATION HERE]
# All code in this script written by Daniel A. Zaltz, PhD, MPH and Caroline Middleton, MS, HBSc

# This is the first step in process to simulate potential meals and menus from a list of
# individual food items (e.g., main dishes, sides, desserts, etc) provided by a major
# food provider for long term care homes in Canada.

# This code provides an example of how we accomplished this process for the study
# [INSERT STUDY CITATION HERE]. However, it is not a fully reproducible example as
# the data used to generate the meals and menus are confidential. To create a fully reproducible
# program of these data, please contact the study author for access to a limited sample of
# the data

# this code loads the data, tidies it, separates options by meal type, and then creates
# two functions which are integral to the rest of the analysis

# 0. setup ---------------------------------------------------------------------------
library(tidyverse)
library(here)

# dat <- read_csv(here("data/[DATALOCATIONHERE].csv")) # add data here

# 1. tidy data ---------------------------------------------------------------------------
dat <- dat |>
   janitor::clean_names() |>
   mutate(
      component = case_when(
         meal_assumptions_cde=="Drink-Juice" ~ "bev_juice",
         meal_assumptions_cde=="Main entree" ~ "main",
         meal_assumptions_cde=="Side-Protein" ~ "side_prot",
         meal_assumptions_cde=="Side-Carbohydrate" ~ "side_carb",
         meal_assumptions_cde=="Misc-Jelly/Jam" ~ "misc_sauce",
         meal_assumptions_cde=="Side-Vegetables & Fruit" ~ "side_fv",
         meal_assumptions_cde=="Drink-Milk" ~ "bev_milk",
         meal_assumptions_cde=="Drink-Hot Beverage" ~ "bev_hot",
         meal_assumptions_cde=="Drink-Water" ~ "bev_water",
         meal_assumptions_cde=="Main entree-sauce" ~ "main",
         meal_assumptions_cde=="Misc-Gravy/Sauce/Condiment" ~ "misc_sauce",
         meal_assumptions_cde=="Dessert" ~ "dessert",
         meal_assumptions_cde=="Side-Other" ~ "side_oth"
      ),
      meal_type = case_when(
         meal_type=="Breakfast" ~ "bfast",
         meal_type=="Morning Snack" ~ "snack_morn",
         meal_type=="Lunch" ~ "lun",
         meal_type=="Afternoon Snack" ~ "snack_aft",
         meal_type=="Dinner" ~ "din",
         meal_type=="Evening Snack" ~ "snack_ev"
      ),
      .before = 3
   ) |>
   rename(food = menu) |>
   mutate(
      id = as.factor(id),
      meal = tolower(meal_type) |> factor(),
      type = as.factor(type_b_vs_f_vs_r),
      component = as.factor(component)
   ) |>
   group_by(meal, sauce_matches) |>
   distinct(food, .keep_all = TRUE) |>
   ungroup()


# 2. separate options by meal, re-do snack options ---------------------------------

# separate by meal type
meals <- list(
   bfast = dat |> filter(meal=="bfast"),
   lun = dat |> filter(meal=="lun"),
   din = dat |> filter(meal=="din"),
   snack_morn = dat |> filter(meal=="snack_morn"),
   snack_aft = dat |> filter(meal=="snack_aft"),
   snack_ev = dat |> filter(meal=="snack_ev")
)

# isolate all beverage options that will be used for snacks
tmp1 <- dat |>
   filter(type_b_vs_f_vs_r=="B") |>
   distinct(fid_cde, .keep_all=TRUE)

# isolate all side options that will be used for snacks
tmp2 <- dat |>
   filter(str_starts(meal_type, "snack")) |> # all snack options (morning, afternoon, evening)
   filter(type_b_vs_f_vs_r!="B") |> # not beverages
   distinct(fid_cde, .keep_all = TRUE)

# bind
tmp3 <- bind_rows(tmp1, tmp2) |>
   mutate(meal_type = "snack_all") # rename mealtype

# replace in meals list
meals$snack_morn <- tmp3
meals$snack_aft <- tmp3
meals$snack_ev <- tmp3

# clean up environment
rm(tmp1, tmp2, tmp3)

# write new meals data
write_rds(meals, here("data/[DATALOCATIONHERE].rds"))

# 3. explore number of options per meal/component -------------------------------------------
map(meals, \(x) janitor::tabyl(x$component)) |>
   as.data.frame() |> tibble() |>
   select(1,2,5,8,11,14,17) |>
   rename(component=1, bfast=2, lun=3, din=4, snack_morn=5, snack_aft=6, snack_ev=7)

# 4. function to make a menu for the day ----------------------------------------------


ltc_make_menu <- function(x){

   # make breakfast -----------------------------------------------------

   # subset data
   bfast <- x[["bfast"]]

   # set params for random pull
   bfast_rs_main <- 1 # 1 main
   bfast_rs_side_carb <- sample(1:2, 1) # 1-2 side carb
   bfast_rs_side_fv <- sample(1:1, 1) # 1 side FV MAX 1
   bfast_rs_side_prot <- sample(1:1, 1) # 0-1 side prot 1-1
   bfast_rs_bev1 <- 1 # one water
   bfast_rs_bev2 <- 1 # one milk
   bfast_rs_bev3 <- 1 # one bev either hot or juice

   # pull components
   tmp_main <- slice_sample(bfast[bfast$component=="main",], n=bfast_rs_main)
   tmp_side_carb <- slice_sample(bfast[bfast$component=="side_carb",], n=bfast_rs_side_carb)
   tmp_side_fv <- slice_sample(bfast[bfast$component=="side_fv",], n=bfast_rs_side_fv)
   tmp_side_prot <- slice_sample(bfast[bfast$component=="side_prot",], n=bfast_rs_side_prot)
   tmp_bev1 <- slice_sample(bfast[bfast$component=="bev_water",], n=bfast_rs_bev1)
   tmp_bev2 <- slice_sample(bfast[bfast$component=="bev_milk",], n=bfast_rs_bev2)
   tmp_bev3 <- slice_sample(bfast[bfast$component%in%c("bev_hot", "bev_juice"),], n=bfast_rs_bev3)


   # specific rules for the side carbs
   bf_sidecarb <- bfast[bfast$component=="side_carb",]
   bfast_breads <- c("WW Toast w/Margarine", "Bagel w/Cream Cheese", "Raisin Toast w/Margarine")
   bfast_muffins <- c("Blueberry Passion Muffin", "Bran Muffin", "Mini Danish Pastry",
                      "Muffin Lemon Cranberry", "Mini Croissant")

   # if there are two side carbs, they cant be 2 breads or 2 muffins/pastries
   if(bfast_rs_side_carb==2 & all(tmp_side_carb$food %in% bfast_breads)){
      tmp_side_carb <- bind_rows(
         slice_sample(tmp_side_carb, n=1),
         slice_sample(bf_sidecarb[!bf_sidecarb$food%in%bfast_breads,], n=1)
      )
   } else if(bfast_rs_side_carb==2 & all(tmp_side_carb$food %in% bfast_muffins)){
      tmp_side_carb <- bind_rows(
         slice_sample(tmp_side_carb, n=1),
         slice_sample(bf_sidecarb[!bf_sidecarb$food%in%bfast_muffins,], n=1)
      )
   } else{
      tmp_side_carb <- tmp_side_carb
   }

   # if there's toast, add the jam
   if(any(tmp_side_carb$food %in% c("WW Toast w/Margarine"))){
      tmp_side_carb <- bind_rows(
         tmp_side_carb, bfast[bfast$food=="Assorted Jams & Spreads",]
      )
   }else{
      tmp_side_carb <- tmp_side_carb
   }

   # bind together
   meal_bfast <- bind_rows(
      tmp_main, tmp_side_carb, tmp_side_fv, tmp_side_prot, tmp_bev1, tmp_bev2, tmp_bev3
   )


   # make morning snack ----------------------------------------------
   snack1 <- x[["snack_morn"]]
   snack_rs_bev1 <- 1 # water
   snack_rs_bev2 <- 1 # one of either hot bev, milk, or juice
   snack_rs_side <- 1 # exactly one side

   tmp_bev1 <- slice_sample(snack1[snack1$component=="bev_water",], n=snack_rs_bev1)
   tmp_bev2 <- slice_sample(snack1[snack1$component%in%c("bev_hot", "bev_juice", "bev_milk"),], n=snack_rs_bev2)
   tmp_side <- slice_sample(snack1[snack1$component%in%c("side_fv", "side_prot"),], n=snack_rs_side)

   meal_snack_morn <- bind_rows(tmp_bev1, tmp_bev2, tmp_side) |> mutate(meal_type = "snack_morn")

   # make lunch ---------------------------------------------------------

   # subset data
   lun <- x[["lun"]]
   lun_sauce <- lun[!is.na(lun$sauce_matches) & lun$component=="misc_sauce",]

   # set params for random pull
   lun_rs_main <- 1 # 1 entree (can be an entree served w sauce or without, will add sauce if needed later)
   lun_rs_side_carb <- sample(1:2, 1) # 1-2 side carb
   lun_rs_side_fv <- sample(2:2, 1) # 2-2 side FV, 2-2
   lun_rs_side_prot <- sample(1:1, 1) # 0-1 side protein 1-1
   lun_rs_dessert <- 1 # 1 dessert
   lun_rs_bev1 <- 1 # one water
   lun_rs_bev2 <- 1 # one milk
   lun_rs_bev3 <- 1 # one bev either hot or juice

   # get main course
   tmp_main <- slice_sample(lun[lun$component=="main",], n=lun_rs_main)

   # check if main course needs a sauce
   if(is.na(tmp_main$sauce_matches)){
      tmp_main <- tmp_main
   }else if(!is.na(tmp_main$sauce_matches)){
      tmp_main <- tmp_main |>
         bind_rows(lun_sauce[lun_sauce$sauce_matches==tmp_main$sauce_matches,])
   }

   # sides
   tmp_side_carb <- slice_sample(lun[lun$component=="side_carb",], n=lun_rs_side_carb)
   tmp_side_fv <- slice_sample(lun[lun$component=="side_fv",], n=lun_rs_side_fv)
   tmp_side_prot <- slice_sample(lun[lun$component=="side_prot",], n=lun_rs_side_prot)
   tmp_dessert <- slice_sample(lun[lun$component=="dessert", ], n=lun_rs_dessert)

   # specific rules for the side carbs
   lun_sidecarb <- lun[lun$component=="side_carb",]
   lun_bread <- c("Bread WW w/Margarine", "Garlic Bread", "WW Dinner Roll",
                  "Multi Grain Bread w/Margarine", "Hamburger Bun")
   lun_pot <- c("Sweet Potato Fries", "French Fries", "Savoury Diced Potatoes")

   # if there are two side carbs, they cant be 2 breads or 2 potatoes
   if(lun_rs_side_carb==2 & all(tmp_side_carb$food %in% lun_bread)){
      tmp_side_carb <- bind_rows(
         slice_sample(tmp_side_carb, n=1),
         slice_sample(lun_sidecarb[!lun_sidecarb$food%in%lun_bread,], n=1)
      )
   } else if(lun_rs_side_carb==2 & all(tmp_side_carb$food %in% lun_pot)){
      tmp_side_carb <- bind_rows(
         slice_sample(tmp_side_carb, n=1),
         slice_sample(lun_sidecarb[!lun_sidecarb$food%in%lun_pot,], n=1)
      )
   } else{
      tmp_side_carb <- tmp_side_carb
   }

   # bevs
   tmp_bev1 <- slice_sample(lun[lun$component=="bev_water",], n=lun_rs_bev1)
   tmp_bev2 <- slice_sample(lun[lun$component=="bev_milk",], n=lun_rs_bev2)
   tmp_bev3 <- slice_sample(lun[lun$component%in%c("bev_hot", "bev_juice"),], n=lun_rs_bev3)

   # bind together
   meal_lunch <- bind_rows(
      tmp_main, tmp_side_carb, tmp_side_fv, tmp_side_prot, tmp_dessert, tmp_bev1, tmp_bev2, tmp_bev3
   )

   # make afternoon snack--------------------------------
   snack2 <- x[["snack_aft"]]
   snack_rs_bev1 <- 1 # water
   snack_rs_bev2 <- 1 # one of either hot bev, milk, or juice
   snack_rs_side <- 1 # exactly one side

   tmp_bev1 <- slice_sample(snack2[snack2$component=="bev_water",], n=snack_rs_bev1)
   tmp_bev2 <- slice_sample(snack2[snack2$component%in%c("bev_hot", "bev_juice", "bev_milk"),], n=snack_rs_bev2)
   tmp_side <- slice_sample(snack2[snack2$component%in%c("side_fv", "side_prot", "side_oth"),], n=snack_rs_side)

   meal_snack_aft <- bind_rows(tmp_bev1, tmp_bev2, tmp_side) |> mutate(meal_type = "snack_aft")

   # make dinner ----------------------------------------

   # subset
   din <- x[["din"]]
   din_sauce <- din[!is.na(din$sauce_matches) & din$component=="misc_sauce",]

   # set params
   din_rs_main <- 1 # one main
   din_rs_side_carb <- sample(1:1, 1) # 1-2 side carb 1-1
   din_rs_side_fv <- sample(2:2, 1) # 1-2 side fv 2-2
   din_rs_dessert <- 1 # one dessert
   din_rs_bev1 <- 1 # one water
   din_rs_bev2 <- 1 # one milk
   din_rs_bev3 <- 1 # one bev either hot or juice

   # pull components
   # main
   tmp_main <- slice_sample(din[din$component=="main",], n=din_rs_main)
   # check if main course needs a sauce
   if(is.na(tmp_main$sauce_matches)){
      tmp_main <- tmp_main
   }else if(!is.na(tmp_main$sauce_matches)){
      tmp_main <- tmp_main |>
         bind_rows(din_sauce[din_sauce$sauce_matches==tmp_main$sauce_matches,])
   }

   # sides
   tmp_side_carb <- slice_sample(din[din$component=="side_carb",], n=din_rs_side_carb)
   tmp_side_fv <- slice_sample(din[din$component=="side_fv",], n=din_rs_side_fv)
   tmp_dessert <- slice_sample(din[din$component=="dessert", ], n=din_rs_dessert)

   # specific rules for side carbs
   din_sidecarb <- din[din$component=="side_carb",]
   din_bread <- c("Bread WW w/Margarine", "Garlic Bread")
   din_pot <- c("Savoury Diced Potatoes", "Garlic Mashed Potatoes", "Mashed Potatoes",
                "Lemon Roasted Potatoes", "Mini Roasted Potatoes", "French Fries",
                "Baked Potato", "Baked Potato Wedges", "Sweet Potato Fries", "Scalloped Potato")
   din_pasta <- c("Buttered Pasta Noodles", "Penne w/Parmesan & Tomato Basil Sc",
                  "Springtime Pasta", "Cheese Ravioli w/Alfredo Sauce")

   # if there are two side carbs, they cant be 2 breads or 2 potatoes or 2 pastas
   if(din_rs_side_carb==2 & all(tmp_side_carb$food %in% din_bread)){
      tmp_side_carb <- bind_rows(
         slice_sample(tmp_side_carb, n=1),
         slice_sample(din_sidecarb[!din_sidecarb$food%in%din_bread,], n=1)
      )
   } else if(din_rs_side_carb==2 & all(tmp_side_carb$food %in% din_pot)){
      tmp_side_carb <- bind_rows(
         slice_sample(tmp_side_carb, n=1),
         slice_sample(din_sidecarb[!din_sidecarb$food%in%din_pot,], n=1)
      )
   } else if(din_rs_side_carb==2 & all(tmp_side_carb$food %in% din_pasta)){
      tmp_side_carb <- bind_rows(
         slice_sample(tmp_side_carb, n=1),
         slice_sample(din_sidecarb[!din_sidecarb$food%in%din_pasta,], n=1)
      )
   } else {
      tmp_side_carb <- tmp_side_carb
   }


   # bevs
   tmp_bev1 <- slice_sample(din[din$component=="bev_water",], n=din_rs_bev1)
   tmp_bev2 <- slice_sample(din[din$component=="bev_milk",], n=din_rs_bev2)
   tmp_bev3 <- slice_sample(din[din$component%in%c("bev_hot", "bev_juice"),], n=din_rs_bev3)

   # bind together
   meal_din <- bind_rows(
      tmp_main, tmp_side_carb, tmp_side_fv, tmp_dessert, tmp_bev1, tmp_bev2, tmp_bev3
   )


   # make evening snack ---------------------------------
   snack3 <- x[["snack_ev"]]
   snack_rs_bev1 <- 1 # water
   snack_rs_bev2 <- 1 # one of either hot bev, milk, or juice
   snack_rs_side <- 1 # exactly one side

   tmp_bev1 <- slice_sample(snack3[snack3$component=="bev_water",], n=snack_rs_bev1)
   tmp_bev2 <- slice_sample(snack3[snack3$component%in%c("bev_hot", "bev_juice", "bev_milk"),], n=snack_rs_bev2)
   tmp_side <- slice_sample(snack3[snack3$component%in%c("side_fv", "side_prot", "side_oth"),], n=snack_rs_side)

   meal_snack_ev <- bind_rows(tmp_bev1, tmp_bev2, tmp_side) |> mutate(meal_type = "snack_ev")

   # put it all together --------------------------------
   menu <- bind_rows(
      meal_bfast, meal_snack_morn, meal_lunch, meal_snack_aft, meal_din, meal_snack_ev
   ) |>
      select(-c(day, week))

   return(menu)

}

write_rds(ltc_make_menu, here("functions/ltc_make_menu.rds"))


# 5. test/export sample week  -----------------------------------------------------------


# here is where we tested the function, reviewed its output, and refined as needed
# currently commented out as no need to repeat

# runs <- 7
# testrun <- map_df(1:runs, \(x){ltc_make_menu(meals) |> mutate(day=x, .before=1)}, .progress=TRUE)
# write_csv(testrun, here("data/[DATALOCATIONHERE].csv"))


