############################
######Sankey Diagrams#######
############################

library(here)
library(tidyverse)
library(survey)
library(srvyr)
library(networkD3)
library(janitor)


library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("here", "here")

####Data Preparation####
#load diary data
food_diary1_4 <- read_tsv(here("Raw Data", "ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.tab"))
food_diary5_6 <- read_tsv(here("Raw Data", "ndns_rp_yr5-6a_foodleveldietarydata_v2.tab"))
food_diary7_8 <- read_tsv(here("Raw Data", "ndns_rp_yr7-8a_foodleveldietarydata.tab"))
food_diary9 <- read_tsv(here("Raw Data", "ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab"))
food_diary10 <- read_tsv(here("Raw Data", "ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab"))
food_diary11 <- read_tsv(here("Raw Data", "ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab"))

#investigate differences in column names between dataframes
compare_df_cols(food_diary1_4, food_diary7_8)
compare_df_cols(food_diary1_4, food_diary9)
compare_df_cols(food_diary7_8, food_diary9)

#tidy column names
food_diary9 <- food_diary9 %>% 
  rename(Age = AgeR)
food_diary10 <- food_diary10 %>% 
  rename(Age = AgeR)
food_diary11 <- food_diary11 %>% 
  rename(Age = AgeR)

#Append all years to one food diary dataframe
food_diary_all <- bind_rows(food_diary1_4, food_diary5_6, food_diary7_8, food_diary9, food_diary10, food_diary11)

#keep only variables of interest
food_diary_all <- food_diary_all %>% select(c(seriali, SurveyYear, DayNo, Age, Sex, Country, RecipeMainFoodGroupDesc, RecipeMainFoodGroupCode,
                                              RecipeSubFoodGroupDesc, RecipeSubFoodGroupCode, Energykcal, Proteing, Fatg, Carbohydrateg, Alcoholg,
                                              Sodiummg, Totalsugarsg, Where))

#create calories based on macronutrients
food_diary_all <- food_diary_all %>%  add_column(protein_calories = food_diary_all$Proteing * 4,
             carb_calories = food_diary_all$Carbohydrateg * 4,
             fat_calories = food_diary_all$Fatg * 9,
             alcohol_calories = food_diary_all$Alcoholg * 7)

#compare these calorie values to the officially provided values
food_diary_all <- food_diary_all %>% add_column(calculated_calories = food_diary_all$protein_calories + food_diary_all$carb_calories + food_diary_all$fat_calories + food_diary_all$alcohol_calories)
food_diary_all <- food_diary_all %>% add_column(calorie_difference = food_diary_all$Energykcal - food_diary_all$calculated_calories)
summary(food_diary_all$calorie_difference)
boxplot(food_diary_all$calorie_difference)
hist(food_diary_all$calorie_difference)
#there were some large discrepancies, adding alcohol removed these.
#most values are very close to 0
#Work may be needed to understand why it differs at all and t understand some outliers.

#tidy food group names
food_diary_all$RecipeMainFoodGroupDesc <- str_to_sentence(food_diary_all$RecipeMainFoodGroupDesc)
food_diary_all$RecipeMainFoodGroupDesc <- gsub("&", "and", food_diary_all$RecipeMainFoodGroupDesc)

#order food groups alphabetically
food_diary_all <- food_diary_all[order(food_diary_all$RecipeMainFoodGroupDesc),]
unique(food_diary_all$RecipeMainFoodGroupDesc)



#put food groups into categories
food_diary_all <- food_diary_all %>% 
  mutate(Food_Category = case_when(RecipeMainFoodGroupDesc %in% c("Pasta rice and other cereals", "White bread", "Wholemeal bread",
                                                                  "Brown granary and wheatgerm bread", "Other bread", "High fibre breakfast cereals",
                                                                  "Other breakfast cereals", "Biscuits", "Buns cakes pastries and fruit pies", "Puddings") ~ "Cereals and cereal products", 
         RecipeMainFoodGroupDesc %in% c("1% fat milk", "Whole milk", "Semi skimmed milk", "Skimmed milk", "Other milk and cream", "Cheese", "Yogurt fromage frais and dairy desserts",
                                        "Ice cream") ~ "Milk and milk products",
         RecipeMainFoodGroupDesc %in% c("Butter", "Reduced fat spread", "Low fat spread", "Other margarine fats and oils", "Pufa margarine and oils") ~ "Fat spreads",
         RecipeMainFoodGroupDesc %in% c("Bacon and ham", "Beef veal and dishes", "Lamb and dishes", "Pork and dishes", "Coated chicken", 
                                        "Chicken and turkey dishes", "Liver and dishes", "Burgers and kebabs", "Sausages", "Meat pies and pastries", 
                                        "Other meat and meat products") ~ "Meat and meat products",
         RecipeMainFoodGroupDesc %in% c("White fish coated or fried", "Other white fish shellfish and fish dishes", "Oily fish") ~ "Fish and fish dishes", 
         RecipeMainFoodGroupDesc %in% c("Salad and other raw vegetables", "Vegetables not raw", "Chips fried and roast potatoes and potato products",
                                        "Other potatoes potato salads and dishes") ~ "Vegetables and Potatoes",
         RecipeMainFoodGroupDesc %in% c("Sugar confectionery", "Chocolate confectionery", "Sugars preserves and sweet spreads") ~ "Sugars, preserves, and confectionery",
         RecipeMainFoodGroupDesc %in% c("Fruit juice", "Soft drinks not low calorie", "Soft drinks low calorie","Smoothies 100% fruit and/or juice",
                                        "Tea coffee and water" ) ~ "Non-alcoholic beverages",
         RecipeMainFoodGroupDesc %in% c("Spirits and liqueurs", "Wine","Beer lager cider and perry") ~ "Alcoholic Beverages",
         RecipeMainFoodGroupDesc == "Eggs and egg dishes" ~ "Eggs and egg dishes",
         RecipeMainFoodGroupDesc == "Crisps and savoury snacks" ~ "Crisps and savoury snacks", 
         RecipeMainFoodGroupDesc == "Nuts and seeds"  ~ "Nuts and seeds", 
         RecipeMainFoodGroupDesc == "Fruit"  ~ "Fruit", 
         RecipeMainFoodGroupDesc %in% c("Miscellaneous", "Artificial sweeteners", "Commercial toddlers foods and drinks", "Dietary supplements") ~ "Miscellaneous"))

#check for all data being included
sum(is.na(food_diary_all$RecipeMainFoodGroupDesc))
sum(is.na(food_diary_all$Food_Category))

na_food_cat <- food_diary_all %>% 
  select(RecipeMainFoodGroupDesc, Food_Category) %>% 
  filter(is.na(Food_Category))

unique(na_food_cat$RecipeMainFoodGroupDesc)

#Calculate data for links in sankey - calorie consumption per day
calorie_daily_data <- food_diary_all %>% 
  group_by(seriali, DayNo) %>% 
  dplyr::summarise(carb_cals_pd = sum(carb_calories), protein_cals_pd = sum(protein_calories), fat_cals_pd = sum(fat_calories), alcohol_cals_pd = sum(alcohol_calories)) %>% 
  ungroup()


link_data <- data.frame(carb_calories = mean(calorie_daily_data$carb_cals_pd),
                        protein_calories = mean(calorie_daily_data$protein_cals_pd),
                        fat_calories = mean(calorie_daily_data$fat_cals_pd),
                        alcohol_calories = mean(calorie_daily_data$alcohol_cals_pd))

#save prepared data
write_csv(food_diary_all, here("Processed Data", "food_diary_all_yrs_sankey.csv"))
write_csv(link_data, here("Processed Data", "food_diary_link_data.csv"))


