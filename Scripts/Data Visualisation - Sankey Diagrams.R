library(here)
library(tidyverse)
library(networkD3)

food_diary_all <- read.csv(here("Processed Data", "food_diary_all_yrs_sankey.csv"))
link_data <- read.csv(here("Processed Data", "food_diary_link_data.csv"))

nodes = data.frame("name" = c("Total Calories", "Carbohydrate", "Fat", "Protein", "Milk and milk products", "Miscellaneous", "Meat and meat products",
                              "Alcoholic Beverages", "Cereals and cereal products", "Fat spreads", "Vegetables and Potatoes", "Sugars, preserves, and confectionery",
                              "Crisps and savoury s NAcks", "Eggs and egg dishes", "Fruit", "Non-alcoholic beverages", "Nuts and seeds", "Fish and fish dishes"))

links = as.data.frame(matrix(c(
  0, 1,  NA, 0, 2,  NA, 0, 3,  NA, 1, 4, NA, 1, 5, NA, 1, 6, NA, 1, 7, NA, 1, 8, NA, 1, 9, NA, 1, 10, NA, 1, 11, NA, 1, 12, NA, 1, 13, NA, 1, 14, NA, 1, 15, NA, 1, 16, NA, 1, 17, NA,
  2, 4, NA, 2, 5, NA, 2, 6, NA, 2, 7, NA, 2, 8, NA, 2, 9, NA, 2, 10, NA, 2, 11, NA, 2, 12, NA, 2, 13, NA, 2, 14, NA, 2, 15, NA, 2, 16, NA, 2, 17, NA,
  3, 4, NA, 3, 5, NA, 3, 6, NA, 3, 7, NA, 3, 8, NA, 3, 9, NA, 3, 10, NA, 3, 11, NA, 3, 12, NA, 3, 13, NA, 3, 14, NA, 3, 15, NA, 3, 16, NA, 3, 17, NA),
  byrow = TRUE, ncol = 3))


names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

nodes = data.frame("name" = c("Total Calories", "Carbohydrate", "Fat", "Protein"))

links = as.data.frame(matrix(c(0, 1, link_data$carb_calories[[1]], 0, 2, link_data$fat_calories[[1]], 0, 3, link_data$protein_calories[[1]]),
                             byrow = TRUE, ncol = 3))

names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

