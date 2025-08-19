# Evaluating and simulating long-term care menus to meet nutrient- and food-based standards

## **Summary**
The purpose of this repository is to evaluate and simulate long-term care (LTC) menus against nutrient- and food-based standards in order to identify and characterize menus that achieve compliance. To do this, we developed a simulation framework that generates thousands of potential meals and menus from real-world LTC food service data, which are then assessed against both nutrient-based targets (e.g., nutrients of concern in LTC and Dietary Reference Intakes for adults aged 70 and older) and food-based targets (e.g., Canada’s Food Guide 2019).

## **Data**
The primary data used in this repository is derived from long-term care menu datasets provided by a major food service provider in Canada. These data are not publicly available due to confidentiality agreements. However, a limited sample dataset can be made available to simulate the analysis upon reasonable request to the study authors.

## **Workflow**
The code is divided into four scripts: 
1. `/code/1_meals.R`: this shows how LTC food items are loaded, tidied, and organized by meal type, and defines core functions
2. `/code/2_menus.R1`: this shows how meals are combined into complete menus and used to simulate thousands of possibilities
3. `/code/3_score.R`: this shows how nutrient- and food-based scoring thresholds are applied to evaluate compliance
4. `/code/4_add_cfss_props_to_data.R`: this shows how simulated menus are analyzed to calculate daily Canadian Food Scoring System score categories, as described in Lee et al. (2024, *Applied Physiology, Nutrition, and Metabolism*).
