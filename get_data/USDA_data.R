#there are 190 unique nutrients
#about 8400 foods with nutrients that are research backed
#looks like 220,113 foods listed in the USDA database with ingredients and nutrients based off of food labels and research

library(jsonlite)
library(dplyr)
library(tidyr)

options(scipen=999)

#source API key
source("USDA_api_key.R")

#######                          #######
#######     Find Nutrient Codes  #######
#######                          #######

#unnest nutrients, and reshape into a format to join to food list
#start by getting a list of nutrient codes from the USDA API 
#paste URL with API key


nutrient_codes_raw <- fromJSON(paste0("https://api.nal.usda.gov/ndb/list?format=json&lt=n&sort=id&api_key=", api_key, "&max=190"))

nutrient_codes_clean <- nutrient_codes_raw$list$item %>%
  select(2:3)

#######                          #######
#######     Find Food Codes      #######
#######                          #######

#start by getting a list of food codes from the USDA API 
#max of 500 items to return into a single call so build loop to build list of string URLs to use as arguments
#which you use as inputs for the function that will unnest each JSON file and merge into a master dataframe of clean food codes
#these codes are then used in following section to get nutrient/ingredient information

#create  dataframe  of URL strings to pass to for loop, every 500 as an offset value
food_codes_urls <- data.frame(base_url = paste0("https://api.nal.usda.gov/ndb/list?format=json&lt=f&sort=n&api_key=", api_key, "&max=500&offset="), offset = as.character(seq(from = 0, to = 220113, by = 500))) %>%
  unite(url, base_url, offset, sep = "") 
  
#initalize empty clean dataframe that for loop will append each resulting api call and manipulation to
food_codes_clean <- data.frame(id = NA, name = NA)

#for loop that pulls out each URL, cleans the data into a two column dataframe of food names and IDs, and binds it to initalized data frame
for (i in 1:nrow(food_codes_urls)) {
  
food_codes_raw <- fromJSON(food_codes_urls[i, ])

food_codes_temp <- food_codes_raw$list$item %>%
  select(2:3)

food_codes_clean <- bind_rows(food_codes_clean, food_codes_temp) 

print(i)
}

#remove uneeded variables
rm(food_codes_temp)
rm(i)
rm(food_codes_urls)

#######                                                              #######
#######     extract nutrients and ingredients, using food codes      #######
#######                                                              #######

#repeat similar steps to food code section, by first getting a list of URLs, using 50 food codes at once (max number you can ask for in each call)
food_data_urls <- food_codes_clean %>%
  select(1) %>%
  rename(code = id) %>%
  filter(!is.na(code)) %>%
  mutate(ndbno = "&ndbno=", 
         index = as.numeric(row.names(.)),
         group = cut(index, breaks = 220150 / 49.5)) %>% #round up to 50 so the bins divide evenly 
  unite(ndbno_code, ndbno, code, sep = "") %>%
  group_by(group) %>% 
  select(1, 3) %>%
  mutate(ndbno_grouped = paste0(ndbno_code, collapse = "")) %>%
  ungroup() %>%
  select(3) %>%
  distinct() %>%
  mutate(url = paste0("https://api.nal.usda.gov/ndb/V2/reports?type=f&format=json&api_key=", api_key)) %>%
  unite(food_data_url, url, ndbno_grouped, sep = "") %>%
  select(1)

food_data_urls <- as.data.frame(food_data_urls) 

#initalize empty clean dataframe that for loop will append each resulting api call and manipulation to for ingredients
food_ingredients_clean <- data.frame(ndbno = NA, ingredients = NA)

#initialize empty clean dataframe that for loop will append each resulting api call and manipulation to for nutrients
food_nutrients_clean <- data.frame(ndbno = NA, nutrient = NA, value = NA, unit = NA)

#for loop that iterates over each food data url to pull out the nutrients and ingredients into individual tables
for (i in 1:nrow(food_data_urls)) {
  
food_data_raw <- fromJSON(food_data_urls[i, ])
print(i)

#get list of foods to iterate over in next two for loops
food_iterate <- food_data_raw$foods$food

#get list of food ingredients for all 50 foods in API link
food_data_unpacked_ingredients <- food_data_raw$foods$food$ing

#some foods don't have any ingredients in the database so this if statement skips them
if (!is.null(food_data_unpacked_ingredients)) {
  
for (i in 1:nrow(food_data_unpacked_ingredients)) {
  
food_ingredients <- food_data_unpacked_ingredients %>%
  select(1) %>%
  mutate(id = as.numeric(row.names(.))) %>%
  filter(id == i) %>%
  mutate(ndbno = food_data_raw$foods$food$desc$ndbno[[i]]) %>%
  select(3, 1) %>%
  rename(ingredients = desc)

food_ingredients_clean <- bind_rows(food_ingredients_clean, food_ingredients)
print(i)
}
}

#get food nutrients for all 50 foods in API link
food_data_unpacked_nutrients <- food_data_raw$foods$food$nutrients

for (i in 1:nrow(food_iterate)) {
  
if (nrow(food_data_unpacked_nutrients[[i]]) > 0) { #some foods don't have any nutrients in the database so this if statement just skips them
  
food_nutrients <- food_data_unpacked_nutrients[[i]] %>%
  select(name, value, unit) %>%
  mutate(ndbno = food_data_raw$foods$food$desc$ndbno[[i]],
         value = as.character(value)) %>%
  rename(nutrient = name)

food_nutrients_clean <- bind_rows(food_nutrients_clean, food_nutrients)
}
print(i)
}
}


