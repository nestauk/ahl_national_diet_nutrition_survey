############################
###Table 6.1 Replication####
############################

library(here)
library(tidyverse)
library(scales)
library(fuzzyjoin)
library(survey)
library(srvyr)

library(conflicted)
conflict_prefer("filter", "dplyr")

#load diary data
food_diary9 <- read_tsv(here("Raw Data", "ndns_rp_yr9a_foodleveldietarydata_uk_20210831.tab"))
food_diary10 <- read_tsv(here("Raw Data", "ndns_rp_yr10a_foodleveldietarydata_uk_20210831.tab"))
food_diary11 <- read_tsv(here("Raw Data", "ndns_rp_yr11a_foodleveldietarydata_uk_20210831.tab"))

#Append three waves of food diary data
#check if columns are identical
identical(sort(names(food_diary9)), sort(names(food_diary11)))

#Append 
food_diary_911 <- bind_rows(food_diary9, food_diary10, food_diary11)
dim(food_diary_911)

#keep only adults
food_diary_911 <- food_diary_911 %>% dplyr::filter(AgeR>18 & AgeR<65)
summary(food_diary_911$AgeR)

####1. Calculate Overall (unweighted) Calorie Contributions of Various Food Groups####
#calculate total number of calories by each food group
fgroup_calories <- food_diary_911 %>%
  group_by(RecipeMainFoodGroupDesc) %>%
  summarise(total_calories = sum(Energykcal)) #official table notes say totalEMJ is used as calorie measure however this is only available in the personal level data 
  
#calculate total calories consumed
overall_calories <- sum(fgroup_calories$total_calories)

#add total calories as column
fgroup_calories <- add_column(fgroup_calories, overall_calories)

#calculate percentage calorie contribution of each food group
fgroup_calories <- add_column(fgroup_calories, calorie_percentage = (fgroup_calories$total_calories/fgroup_calories$overall_calories))

###Compare data to official tables###
replication6.1 <- read_csv(here("Raw Data", "Table6.1_replication.csv"))

#rename column name for merging
fgroup_calories <- rename(fgroup_calories, food_group = RecipeMainFoodGroupDesc)

#convert food group names to lowercase
fgroup_calories$food_group <- str_to_lower(fgroup_calories$food_group)
replication6.1$food_group <- str_to_lower(replication6.1$food_group)

#drop redundant rows
replication6.1 <- filter(replication6.1, food_group != "of which:")

#remove commas, brackets from food group names
replication6.1$food_group <- gsub(",", "", replication6.1$food_group)
replication6.1$food_group <- gsub("\\s*\\([^\\)]+\\)","", replication6.1$food_group)

#replace &s
fgroup_calories$food_group <- gsub("&", "and", fgroup_calories$food_group)

#merge our data to official tables
rep_check <- merge(fgroup_calories, replication6.1, by="food_group", all = TRUE) ##the values found here are similar to the official tables but far from precise

#get rid of exponential formatting
options("scipen"=100, "digits"=4)


####2. Calculate weighted calorie percentage by person####

#calculate number of calories per person, by food group
ind_fgroup_calories <- food_diary_911 %>%
  group_by(seriali, RecipeMainFoodGroupDesc) %>%
  summarise(total_calories = sum(Energykcal)) %>% 
  ungroup()


#calculate total calories per person 
ind_fgroup_calories <- ind_fgroup_calories %>% 
  group_by(seriali) %>% 
  mutate(ind_total_calories = sum(total_calories)) %>% 
  ungroup()

#calculate % contribution of each food group to individuals total calorie consumption
ind_fgroup_calories <- ind_fgroup_calories %>% 
  mutate(calorie_percentage = total_calories/ind_total_calories)

##get strata and weight data
ind <- read_tsv(here("Raw Data", "ndns_rp_yr9-11a_indiv_20211020.tab"))

#keep relevant vars
ind <- select(ind, c(seriali, astrata1, wti_Y911))

#merge survey vars into dataset
ind_fgroup_calories <- merge(ind_fgroup_calories, ind, by = "seriali", all.x = TRUE)

#rename column name for merging
ind_fgroup_calories <- rename(ind_fgroup_calories, food_group = RecipeMainFoodGroupDesc)

#convert food group names to lowercase
ind_fgroup_calories$food_group <- str_to_lower(ind_fgroup_calories$food_group)

#replace &s
ind_fgroup_calories$food_group <- gsub("&", "and", ind_fgroup_calories$food_group)

#set survey design
fgroup_design <- ind_fgroup_calories %>%
  as_survey_design(strata  = astrata1,
                   weights = wti_Y911) %>% 
  srvyr::group_by(food_group)

#get weighted means
wgt_fgroup_calories <- fgroup_design %>% 
  summarise(weighted_calories = survey_mean(calorie_percentage)) %>% 
  select(!weighted_calories_se)

#merge weighted calories with official tables
rep_check2 <- merge(wgt_fgroup_calories, replication6.1, by="food_group", all = TRUE) ##These values are way off official data

##Check to see if increased discrepancy between ours and official is caused by weighting process or earlier error
#Try unweighted combination of data to calculate means
unwgt_fgroup_calories <- ind_fgroup_calories %>% 
  group_by(food_group) %>% 
  summarise(unwgt_calories = mean(calorie_percentage))


####3. Calculate weighted calorie percentage at overall level####

#combine diary data with weighting data
food_diary_911_wgt <- merge(food_diary_911, ind, by = "seriali", all.x = TRUE)

#multiply out calories by weighting
food_diary_911_wgt$weighted_calories <- food_diary_911_wgt$Energykcal * food_diary_911_wgt$wti_Y911

#calculate total number of calories by each food group
wgt_fgroup_calories <- food_diary_911_wgt %>%
  group_by(RecipeMainFoodGroupDesc) %>%
  summarise(total_calories = sum(weighted_calories)) 

#calculate total calories consumed
overall_calories <- sum(wgt_fgroup_calories$total_calories)

#add total calories as column
wgt_fgroup_calories <- add_column(wgt_fgroup_calories, overall_calories)

#calculate percentage calorie contribution of each food group
wgt_fgroup_calories <- add_column(wgt_fgroup_calories, calorie_percentage = (wgt_fgroup_calories$total_calories/fgroup_calories$overall_calories))

#rename column name for merging
wgt_fgroup_calories <- rename(wgt_fgroup_calories, food_group = RecipeMainFoodGroupDesc)

#convert food group names to lowercase
wgt_fgroup_calories$food_group <- str_to_lower(wgt_fgroup_calories$food_group)

#replace &s
wgt_fgroup_calories$food_group <- gsub("&", "and", wgt_fgroup_calories$food_group)

#merge our data to official tables
rep_check3 <- merge(wgt_fgroup_calories, replication6.1, by="food_group", all = TRUE) ##the values are not very close to official data
