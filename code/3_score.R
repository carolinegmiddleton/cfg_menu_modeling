# SCRIPT 3 OF XX
# [CITATION HERE]
# All code in this script written by Daniel A. Zaltz, PhD, MPH and Caroline Middleton, MS, HBSc

# This script loads the simulated data and applies the scoring threshholds described in the
# citation above


# 0. setup --------------------------------------------------------------------------

library(here)
library(tidyverse)
#devtools::install_github("didierbrassard/hefi2019")
library(hefi2019)

days <- read_rds(here("data/[DATALOCATIONHERE].rds")) # data generated in the second script

# 1. score days   ----------------------------------------------

ltc_daily_UNIQUEDAYS <- function(x,
                                 kcal_lo=2400,
                                 kcal_hi=3000,
                                 fluid_targ=2000,
                                 prot=100,
                                 pkcalfat_lo = 30,
                                 pkcalfat_hi = 35,
                                 fibre = 30,
                                 sod = 3500,
                                 carb = 312,
                                 chol = 300,
                                 calc = 1200,
                                 iron = 8,
                                 vita = 900,
                                 vitb1 = 1.2,
                                 vitb2 = 1.3,
                                 vitb3 = 16,
                                 vitb6 = 1.7,
                                 vitb12 = 2.4,
                                 vitc = 90,
                                 vitd = 20,
                                 vite = 15,
                                 fol = 400,
                                 mag = 420,
                                 panto = 5,
                                 copper = 0.9,
                                 sod_rda = 2300,
                                 pot = 4700,
                                 mang = 2.3,
                                 phos = 700,
                                 sel = 55,
                                 zinc = 11,
                                 pct_rda = 0.75,
                                 cfss_target = 75,
                                 hefi_target = 75)
{

  # daily totals ----
  x <- x |>
    # group by day
    group_by(day) |>
    # rename vitamins for easier coding
    rename(
      vitb1_mg = thiamin, vitb2_mg = riboflavin,
      vitb3_mg = niacin_nicotinic_acid_preformed, vitb6_mg = vitamin_b_6,
      vitb12_mcg = vitamin_b_12, vitd_mcg = vitamin_d_d2_d3, vite_mg = alpha_tocopherol,
      folate_mcg = total_folacin, potas_mg = potassium, panto_mg = pantothenic_acid,
      copper_mg = copper, magnesium_mg = magnesium, manganese_mg = manganese,
      phos_mg = phosphorus, selenium_mcg = selenium, zinc_mg = zinc, pufa_g = pufa,
      mufa_g = mufa
    ) |>
    # get totals per day
    summarise(
      across(
        c(kcal, pro_g, total_fat_g, satfat_g, pufa_g, mufa_g,
          fibre_g, sod_mg, cho_g, free_sug_g, chol_mg, calcium_mg, iron_mg,
          vitc_mg, vitare_mg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg,
          vitb12_mcg, vitd_mcg, vite_mg, folate_mcg, potas_mg, panto_mg, copper_mg,
          magnesium_mg, manganese_mg, phos_mg, selenium_mcg, zinc_mg
        ),
        \(x){sum(x, na.rm = TRUE)}
      )
    ) |>
    ungroup() |>
    # merge in fluids (needs to be calculated for beverages only)
    left_join(
      x |>
        filter(type=="B") |>
        group_by(day) |>
        summarise(
          fluid = sum(weight_g_calculated, na.rm = TRUE)
        ) |>
        ungroup()
    ) |>
    relocate(fluid, .after=kcal) |>
    # calculate pct kcal from fat
    mutate(
      fat_pctkcal = (total_fat_g*9)/kcal*100,
      .after=pro_g
    ) |>
    # calculate cfss, see Lee et al. 2024. Applied Physiology, Nutrition, and Metabolism, 49(10), 1363-1376.
    left_join(
      x |>
        group_by(day) |>
        summarise(
          cfss = sum(final_cfg*(weight_g_calculated/tra_ss), na.rm = TRUE)/
            sum(weight_g_calculated/tra_ss, na.rm = TRUE)
        ) |>
        ungroup()

    ) |>
    # HEFI components
    left_join(
      x |>
        transmute(
          day = day,
          VEGFRUITS = ifelse(hefi_cfg_food_group == 'vegfruits', weight_g_calculated / tra_ss, 0),
          WHOLEGRFOODS = ifelse(hefi_cfg_food_group == 'wholegrfoods', weight_g_calculated / tra_ss, 0),
          NONWHOLEGRFOODS = ifelse(hefi_cfg_food_group == 'nonwholegrfoods', weight_g_calculated / tra_ss, 0),
          PROFOODSANIMAL = ifelse(hefi_cfg_food_group == 'profoodsanimal', weight_g_calculated / tra_ss, 0),
          PROFOODSPLANT = ifelse(hefi_cfg_food_group == 'profoodsplant', weight_g_calculated / tra_ss, 0),
          OTHERFOODS = ifelse(hefi_cfg_food_group == 'otherfoods', weight_g_calculated / tra_ss, 0),
          WATERHEALTHYBEV = ifelse(hefi_cfg_food_group == 'waterhealthybev', weight_g_calculated, 0),
          UNSWEETPLANTBEVPRO = ifelse(hefi_cfg_food_group == 'unsweetplantbevpro', weight_g_calculated, 0),
          UNSWEETMILK = ifelse(hefi_cfg_food_group == 'unsweetmilk', weight_g_calculated, 0),
          OTHERBEVERAGES = ifelse(hefi_cfg_food_group == 'otherbeverages', weight_g_calculated, 0)
        ) |>
        group_by(day) |>
        summarise(
          across(
            everything(), sum
          )
        ) |>
        ungroup()

    )

  # HEFI ----
  x <- hefi2019(
    indata = x,
    vegfruits = VEGFRUITS,
    wholegrfoods = WHOLEGRFOODS,
    nonwholegrfoods = NONWHOLEGRFOODS,
    profoodsanimal = PROFOODSANIMAL,
    profoodsplant = PROFOODSPLANT,
    otherfoods = OTHERFOODS,
    waterhealthybev = WATERHEALTHYBEV,
    unsweetmilk = UNSWEETMILK,
    unsweetplantbevpro = UNSWEETPLANTBEVPRO,
    otherbeverages = OTHERBEVERAGES,
    mufat = mufa_g,
    pufat = pufa_g,
    satfat = satfat_g,
    freesugars = free_sug_g,
    sodium = sod_mg,
    energy = kcal
  )

  x <- x |>
    mutate(hefi100 = (HEFI2019_TOTAL_SCORE/80)*100)

  # check if meets daily thresholds ----
  x <- x |>
    mutate(
      s1_kcal = ifelse(kcal>=kcal_lo & kcal<=kcal_hi, "yes", "no"),
      s1_fluid = ifelse(fluid>=fluid_targ, "yes", "no"),
      s1_prot = ifelse(pro_g>=prot, "yes", "no"),
      s1_pcalfat = ifelse(fat_pctkcal>=pkcalfat_lo & fat_pctkcal<=pkcalfat_hi, "yes", "no"),
      s1_fibre = ifelse(fibre_g>=fibre, "yes", "no"),
      s1_sod = ifelse(sod_mg<sod, "yes", "no"),
      s1_carb = ifelse(cho_g>=carb, "yes", "no"),
      s1_chol = ifelse(chol_mg<chol, "yes", "no"),


      s2_calc = ifelse(calcium_mg>=(calc*pct_rda), "yes", "no"),
      s2_iron = ifelse(iron_mg>=(iron*pct_rda), "yes", "no"),
      s2_vitc = ifelse(vitc_mg>=(vitc*pct_rda), "yes", "no"),
      s2_vita = ifelse(vitare_mg>=(vita*pct_rda), "yes", "no"),
      s2_vitb1 = ifelse(vitb1_mg>=(vitb1*pct_rda), "yes", "no"),
      s2_vitb2 = ifelse(vitb2_mg>=(vitb2*pct_rda), "yes", "no"),
      s2_vitb3 = ifelse(vitb3_mg>=(vitb3*pct_rda), "yes", "no"),
      s2_vitb6 = ifelse(vitb6_mg>=(vitb6*pct_rda), "yes", "no"),
      s2_vitb12 = ifelse(vitb12_mcg>=(vitb12*pct_rda), "yes", "no"),
      s2_vitd = ifelse(vitd_mcg>=(vitd*pct_rda), "yes", "no"),
      s2_vite = ifelse(vite_mg>=(vite*pct_rda), "yes", "no"),
      s2_folate = ifelse(folate_mcg>=(fol*pct_rda), "yes", "no"),
      s2_potas = ifelse(potas_mg>=(pot*pct_rda), "yes", "no"),
      s2_copper = ifelse(copper_mg>=(copper*pct_rda), "yes", "no"),
      s2_panto = ifelse(panto_mg>=(panto*pct_rda), "yes", "no"),
      s2_sod = ifelse(sod_mg<sod_rda, "yes", "no"),
      s2_magnesium = ifelse(magnesium_mg>=(mag*pct_rda), "yes", "no"),
      s2_manganese = ifelse(manganese_mg>=(mang*pct_rda), "yes", "no"),
      s2_selenium = ifelse(selenium_mcg>=(sel*pct_rda), "yes", "no"),
      s2_zinc = ifelse(zinc_mg>=(zinc*pct_rda), "yes", "no"),
      s2_phos = ifelse(phos_mg>=(phos*pct_rda), "yes", "no"),
      s3_cfss = ifelse(cfss>=cfss_target, "yes", "no"),
      s4_hefi = ifelse(hefi100>=hefi_target, "yes", "no")
    )



  # return object ----
  return(x)
}
write_rds(ltc_daily, here("functions/ltc_daily_score.rds"))

ltc_daily(tmp) # pilot works


# 2. run it on full data ---------------------------------------------------------------

# full data
months <- read_rds(here("data/[DATALOCATIONHERE].rds")) # come back here to note where months are generated
# check first if it works with map
options(tidyverse.quiet = TRUE)
test <- months[1:10]
tmp <- map_dfr(test, ltc_daily)
# run it
ltc_scores_daily <- map_dfr(months, ltc_daily, .progress = TRUE)
# unique ID
ltc_scores_daily <- ltc_scores_daily |>
   mutate(
      id = paste0(month, week, day),
      .before = 1
   )
write_rds(ltc_scores_daily, here("data/[DATALOCATIONHERE].rds"))

# 3. score months ----


ltc_monthly <- function(x,
                        month_var = "month",
                        calc = 1200,
                        iron = 8,
                        sod = 3500,
                        carb = 312,
                        chol = 300,
                        vita = 900,
                        vitb1 = 1.2,
                        vitb2 = 1.3,
                        vitb3 = 16,
                        vitb6 = 1.7,
                        vitb12 = 2.4,
                        vitc = 90,
                        vitd = 20,
                        vite = 15,
                        fol = 400,
                        mag = 420,
                        panto = 5,
                        copper = 0.9,
                        sod_rda = 2300,
                        pot = 4700,
                        mang = 2.3,
                        phos = 700,
                        sel = 55,
                        zinc = 11)
{

   # check month ----
   if(distinct(x[month_var]) |> nrow()!=1){
      stop("More than one distinct month in data, this only works for one specific monthly cycle")
   } else {
      month <- distinct(x[month_var]) |> pull()
   }

   # get day totals and then month average ----
   x |>
      rename(
         vitb1_mg = thiamin, vitb2_mg = riboflavin,
         vitb3_mg = niacin_nicotinic_acid_preformed, vitb6_mg = vitamin_b_6,
         vitb12_mcg = vitamin_b_12, vitd_mcg = vitamin_d_d2_d3, vite_mg = alpha_tocopherol,
         folate_mcg = total_folacin, potas_mg = potassium, panto_mg = pantothenic_acid,
         copper_mg = copper, magnesium_mg = magnesium, manganese_mg = manganese,
         phos_mg = phosphorus, selenium_mcg = selenium, zinc_mg = zinc, pufa_g = pufa,
         mufa_g = mufa
      ) |>
      group_by(week, day) |>
      summarise(
         across(
            c(kcal, pro_g, total_fat_g, satfat_g, pufa_g, mufa_g, calcium_mg, iron_mg,
              fibre_g, sod_mg, cho_g, free_sug_g, chol_mg,
              vitc_mg, vitare_mg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg,
              vitb12_mcg, vitd_mcg, vite_mg, folate_mcg, potas_mg, panto_mg, copper_mg,
              magnesium_mg, manganese_mg, phos_mg, selenium_mcg, zinc_mg
            ),
            \(x){sum(x, na.rm = TRUE)}
         )
      ) |>
      ungroup() |>
      # get month averages ----
      summarise(
         across(
            c(kcal, pro_g, total_fat_g, satfat_g, pufa_g, mufa_g, calcium_mg, iron_mg,
              fibre_g, sod_mg, cho_g, free_sug_g, chol_mg,
              vitc_mg, vitare_mg, vitb1_mg, vitb2_mg, vitb3_mg, vitb6_mg,
              vitb12_mcg, vitd_mcg, vite_mg, folate_mcg, potas_mg, panto_mg, copper_mg,
              magnesium_mg, manganese_mg, phos_mg, selenium_mcg, zinc_mg
            ),
            \(x){mean(x, na.rm = TRUE)}
         )
      ) |>
      mutate(
         s5_calc = ifelse(calcium_mg>=calc, "yes", "no"),
         s5_iron = ifelse(iron_mg>=iron, "yes", "no"),
         s5_vitc = ifelse(vitc_mg>=(vitc), "yes", "no"),
         s5_vita = ifelse(vitare_mg>=(vita), "yes", "no"),
         s5_vitb1 = ifelse(vitb1_mg>=(vitb1), "yes", "no"),
         s5_vitb2 = ifelse(vitb2_mg>=(vitb2), "yes", "no"),
         s5_vitb3 = ifelse(vitb3_mg>=(vitb3), "yes", "no"),
         s5_vitb6 = ifelse(vitb6_mg>=(vitb6), "yes", "no"),
         s5_vitb12 = ifelse(vitb12_mcg>=(vitb12), "yes", "no"),
         s5_vitd = ifelse(vitd_mcg>=(vitd), "yes", "no"),
         s5_vite = ifelse(vite_mg>=(vite), "yes", "no"),
         s5_folate = ifelse(folate_mcg>=(fol), "yes", "no"),
         s5_magnesium = ifelse(magnesium_mg>=(mag), "yes", "no"),
         s5_panto = ifelse(panto_mg>=(panto), "yes", "no"),
         s5_copper = ifelse(copper_mg>=(copper), "yes", "no"),
         s5_sod = ifelse(sod_mg<sod_rda, "yes", "no"),
         s5_potas = ifelse(potas_mg>=(pot), "yes", "no"),
         s5_manganese = ifelse(manganese_mg>=(mang), "yes", "no"),
         s5_phos = ifelse(phos_mg>=(phos), "yes", "no"),
         s5_selenium = ifelse(selenium_mcg>=(sel), "yes", "no"),
         s5_zinc = ifelse(zinc_mg>=(zinc), "yes", "no"),
      ) |>
      mutate(
         month = month,
         .before = 1
      )
}

write_rds(ltc_monthly, here("functions/ltc_monthly_score.rds"))



# 4. run it on full data ---------------------------------------------------------------
test <- months[1:10]
tmp <- map_dfr(test, ltc_monthly)
ltc_scores_month <- map_dfr(months, ltc_monthly, .progress = TRUE)
write_rds(ltc_scores_month, here("data/[DATALOCATIONHERE].rds"))
