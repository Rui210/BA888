library(tidyverse)
library(jsonlite)
library(tidytext)
library(stringr)
yelp = stream_in(file("business.json"))
yelp_flat = flatten(yelp)
yelp_buss = as.data.frame(yelp_flat)

glimpse(yelp_buss)
write_csv(yelp_buss, "yelpbusiness.csv")

data = read_csv("yelpbusiness.csv")
View(data)

################################ sample ###############################
data1 = data[1:500, ]
View(data1)
df1 = data1 %>%
 select(1:12) %>%
 mutate(categories = str_to_lower(categories)) %>%
 filter(str_detect(categories, "restaurants"))

df2 = data1 %>%
 select(1:12) %>%
 mutate(categories = str_to_lower(categories))

df3 = anti_join(df2, df1)
df1_1 = df3 %>%
  filter(str_detect(categories,"food"))
df1_2 = rbind(df1, df1_1)

## check the rest businesses
df3_1 = anti_join(df2, df1_2)
df4 = df3_1 %>%
  separate(categories, into = paste0("v", 1:100))
df4 = df4[colSums(!is.na(df4)) > 0] 
words = df4 %>%
  pivot_longer(cols = v1:v24,
               names_to = "pos",
               values_to = "token",
               values_drop_na = TRUE) %>% 
  filter(str_length(token) > 0) %>%
  group_by(token) %>%
  count(sort = T)
## combine restaurants to the original dataset
data1_1 = data1 %>%
  mutate(categories = str_to_lower(categories))
newdata1 = inner_join(data1_1, df1_2)
######################################################################
######################################################################
########################### Let's try it! ############################
######################################################################
######################################################################


## filter business which has "restaurants" and "food" in the categories ##
df = data %>%
  select(1:12) %>%
  filter(str_detect(categories, "restaurants"))

data_check = data %>%
  select(1:12)
df_check = anti_join(data_check, df)

df_food = df_check %>%
  filter(str_detect(categories, "food"))
df_new = rbind(df, df_food)  

df_check1 = anti_join(data_check, df_new)

words_left = df_check1 %>%
  select(categories) %>%
  separate(categories, into = paste0("v", 1:100)) %>%
  pivot_longer(cols = v1:v100,
               names_to = "pos",
               values_to = "token",
               values_drop_na = TRUE) %>% 
  filter(str_length(token) > 0) %>%
  group_by(token) %>%
  count(sort = T)

## Only RESTAURANT Now ##
restaurants = inner_join(data, df_new)
## delet the coloumn that have more than 50% NAs.
restaurants = restaurants[colSums(!is.na(restaurants)) > 0.5*nrow(restaurants)]
## only open restaurants ##
open = restaurants %>%
  filter(is_open == 1)

## write out the clean dataset
write_csv(restaurants, "restaurants.csv")

#################### working on restaurant ##################
restaurants = read_csv("restaurants.csv")
restaurant_in_state = restaurants %>%
  mutate(x = 1) %>%
  select(x, name, state, stars, review_count, is_open, 
         attributes.RestaurantsPriceRange2) %>%
  na.omit(.) %>%
  group_by(state) %>%
  summarise(num = sum(x)) %>%
  filter(num > 50)

is.data.frame(restaurant_in_state)

stars_by_state = restaurants %>%
  select(name, state, stars, review_count, is_open, 
         attributes.RestaurantsPriceRange2) %>%
  na.omit(.) %>%
  group_by(state) %>%
  summarise(stars = mean(stars))


