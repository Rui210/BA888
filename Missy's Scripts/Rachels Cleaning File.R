library(tidyverse)
library(tidyverse)
library(jsonlite)
library(tidytext)
library(stringr)

data <- read.csv("888/Data as a CSV/yelpbusiness (1).csv")

glimpse(data)

data = data %>%
  mutate(categories = str_to_lower(categories))

## filter business which has "restaurants" and "food" in the categories ##
df = data %>%
   select(1:12) %>%
   filter(str_detect(categories, "restaurants"))
 
 data_check = data %>%
   select(1:12)
 
 df_check = anti_join(data_check, df)
 
 df_food = df_check %>%
   filter(str_detect(categories, "food"))
 
# this is the dataset that includes all rows with "restaurant" or "food"
 df_new = rbind(df, df_food)
 
## Only RESTAURANT Now ##
restaurants = inner_join(data, df_new)
glimpse(restaurants)

## delet the coloumn that have more than 50% NAs.
#restaurants = restaurants[colSums(!is.na(restaurants)) > 0.5*nrow(restaurants)]














