############################
###Table 6.2 Replication####
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

####1. Calculate Overall (unweighted) protein Contributions of Various Food Groups####
#calculate total number of protein by each food group
fgroup_protein <- food_diary_911 %>%
  group_by(RecipeMainFoodGroupDesc) %>%
  summarise(total_protein = sum(Proteing)) 

#calculate total protein consumed
overall_protein <- sum(fgroup_protein$total_protein)

#add total protein as column
fgroup_protein <- add_column(fgroup_protein, overall_protein)

#calculate percentage protein contribution of each food group
fgroup_protein <- add_column(fgroup_protein, protein_percentage = (fgroup_protein$total_protein/fgroup_protein$overall_protein))

###Compare data to official tables###
replication6.2 <- read_csv(here("Raw Data", "Table6.2_replication.csv"))

#rename column name for merging
fgroup_protein <- rename(fgroup_protein, food_group = RecipeMainFoodGroupDesc)

#convert food group names to lowercase
fgroup_protein$food_group <- str_to_lower(fgroup_protein$food_group)
replication6.2$food_group <- str_to_lower(replication6.2$food_group)

#drop redundant rows
replication6.2 <- filter(replication6.2, food_group != "of which:")

#remove commas, brackets from food group names
replication6.2$food_group <- gsub(",", "", replication6.2$food_group)
replication6.2$food_group <- gsub("\\s*\\([^\\)]+\\)","", replication6.2$food_group)

#replace &s
fgroup_protein$food_group <- gsub("&", "and", fgroup_protein$food_group)

#merge our data to official tables
rep_check <- merge(fgroup_protein, replication6.2, by="food_group", all = TRUE) ##the values found here are very similar to official tables

#get rid of exponential formatting
options("scipen"=100, "digits"=4)


####2. Calculate weighted protein percentage by person####

#calculate number of protein per person, by food group
ind_fgroup_protein <- food_diary_911 %>%
  group_by(seriali, RecipeMainFoodGroupDesc) %>%
  summarise(total_protein = sum(Proteing)) %>% 
  ungroup()


#calculate total protein per person 
ind_fgroup_protein <- ind_fgroup_protein %>% 
  group_by(seriali) %>% 
  mutate(ind_total_protein = sum(total_protein)) %>% 
  ungroup()

#calculate % contribution of each food group to individuals total protein consumption
ind_fgroup_protein <- ind_fgroup_protein %>% 
  mutate(protein_percentage = total_protein/ind_total_protein)

##get strata and weight data
ind <- read_tsv(here("Raw Data", "ndns_rp_yr9-11a_indiv_20211020.tab"))

#keep relevant vars
ind <- select(ind, c(seriali, astrata1, wti_Y911))

#merge survey vars into dataset
ind_fgroup_protein <- merge(ind_fgroup_protein, ind, by = "seriali", all.x = TRUE)

#rename column name for merging
ind_fgroup_protein <- rename(ind_fgroup_protein, food_group = RecipeMainFoodGroupDesc)

#convert food group names to lowercase
ind_fgroup_protein$food_group <- str_to_lower(ind_fgroup_protein$food_group)

#replace &s
ind_fgroup_protein$food_group <- gsub("&", "and", ind_fgroup_protein$food_group)

#set survey design
fgroup_design <- ind_fgroup_protein %>%
  as_survey_design(strata  = astrata1,
                   weights = wti_Y911) %>% 
  srvyr::group_by(food_group)

#get weighted means
wgt_fgroup_protein <- fgroup_design %>% 
  summarise(weighted_protein = survey_mean(protein_percentage)) %>% 
  select(!weighted_protein_se)

#merge weighted protein with official tables
rep_check2 <- merge(wgt_fgroup_protein, replication6.2, by="food_group", all = TRUE) ##These values are way off offocial data

##Check to see if increased discrepancy between ours and official is caused by weighting process or earlier error
#Try unweighted combination of data to calculate means
unwgt_fgroup_protein <- ind_fgroup_protein %>% 
  group_by(food_group) %>% 
  summarise(unwgt_protein = mean(protein_percentage))


####3. Calculate weighted protein percentage at overall level####

#combine diary data with weighting data
food_diary_911_wgt <- merge(food_diary_911, ind, by = "seriali", all.x = TRUE)

#multiply out protein by weighting
food_diary_911_wgt$weighted_protein <- food_diary_911_wgt$Proteing * food_diary_911_wgt$wti_Y911

#calculate grams protein by each food group
wgt_fgroup_protein <- food_diary_911_wgt %>%
  group_by(RecipeMainFoodGroupDesc) %>%
  summarise(total_protein = sum(weighted_protein)) 

#calculate total protein consumed
overall_protein <- sum(wgt_fgroup_protein$total_protein)

#add total protein as column
wgt_fgroup_protein <- add_column(wgt_fgroup_protein, overall_protein)

#calculate percentage protein contribution of each food group
wgt_fgroup_protein <- add_column(wgt_fgroup_protein, protein_percentage = (wgt_fgroup_protein$total_protein/fgroup_protein$overall_protein))

#rename column name for merging
wgt_fgroup_protein <- rename(wgt_fgroup_protein, food_group = RecipeMainFoodGroupDesc)

#convert food group names to lowercase
wgt_fgroup_protein$food_group <- str_to_lower(wgt_fgroup_protein$food_group)

#replace &s
wgt_fgroup_protein$food_group <- gsub("&", "and", wgt_fgroup_protein$food_group)

#merge our data to official tables
rep_check3 <- merge(wgt_fgroup_protein, replication6.2, by="food_group", all = TRUE) 
