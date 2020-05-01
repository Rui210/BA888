library(sentimentr)
library(topicmodels)
library(tidyverse)
library(quanteda)
library(topicmodels)
library(tidytext)
library(factoextra)
library(wordcloud)
library(quanteda)
library(tidyverse)
library(dplyr)

reviews <- read.csv("Data/canada_reviews_ms.csv")
reviews$text <- as.character(reviews$text)

#glimpse(reviews)
restaurants <- read.csv("Data/canada_restaurants_ms_edits.csv")
#glimpse(restaurants)

restaurantprice <- restaurants %>% select(business_id, attributes_RestaurantsPriceRange2)


reviewsWPrice <- left_join(reviews, restaurantprice, by = "business_id")
reviewsWPrice <- reviewsWPrice %>% rename(resPrice = attributes_RestaurantsPriceRange2)


sw <- c(stopwords(),"ã","brã","lã","e")
swPhrases <- c("ice_cream","bubble_tea","milk_tea","green_tea","noodle_soup","milk_tea","spring_rolls","fried_chicken",
               "red_bean","foie_gras","caesar_salad","come_back","coconut_cream","cream_pie","coconut_cream_pie",
               "duck_fat","highly_recommend","highly_recommended","holt_renfrew",
               "jerk_chicken","banh_mi","pork_belly","hot_sauce","pulled_pork","chicken_shawarma",
               "kensington_market","coffee_shop","ordered_chicken","burrito_boyz",
               "pad_thai","one_star","never_come","never_go","coming_back","going_back",
               "next_time","really_good","definitely_recommend","go_back","ruth_chris",
               "make_sure","coming_back","recommend_place","never_going","even_though",
               "butter_chicken","kimchi_fries","black_sesame","definitely_back","feel_like",
               "felt_like","baked_goods")

###############################################################33
totalFourDollars <- reviewsWPrice %>%
  filter(resPrice == 4)

total_four_dollar_tokens = tokens(totalFourDollars$text, 
                            remove_punct = T,
                            remove_numbers = T)

total_four_dollar_tokens <- tokens_remove(total_four_dollar_tokens, pattern = sw)

total_four_dollar_dfm = dfm(total_four_dollar_tokens,
                      remove = swPhrases,
                      ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .8,
           docfreq_type = "prop")
           #min_termfreq = 10,
           #termfreq_type = "count")

topfeatures(total_four_dollar_dfm,25)

#sdfm

total_four_dtm <- convert(total_four_dollar_dfm, "topicmodels") ###

total_four_tm2 = LDA(total_four_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(total_four_tm2, 20)

total_four_tm3 = LDA(total_four_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(total_four_tm3, 20)

total_four_tm4 = LDA(total_four_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(total_four_tm4, 20)

#########################################################################################
#########################################################################################

one_dollar_tokens = tokens(oneDollar$text, 
                           remove_punct = T,
                           remove_numbers = T)

one_dollar_tokens <- tokens_remove(one_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(one_dollar_tokens,
           remove = swPhrases,
           ngrams = 3) %>%
  dfm_trim(max_docfreq = .8,
           docfreq_type = "prop",
           min_termfreq = 5,
           termfreq_type = "count")

topfeatures(sdfm,15)

one_dollar_dtm <- convert(sdfm, "topicmodels") ###

one_tm2 = LDA(one_dollar_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(one_tm2, 15)

one_tm3 = LDA(one_dollar_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(one_tm3, 15)

one_tm4 = LDA(one_dollar_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(one_tm4, 15)

##############################################################################################
##############################################################################################

fourDollarPositive <- reviewsWPrice %>%
  filter(stars %in% c(5)) %>% 
  filter(resPrice == 4)

nrow(fourDollarPositive)

pos_four_dollar_tokens = tokens(fourDollarPositive$text, 
                            remove_punct = T,
                            remove_numbers = T)

pos_four_dollar_tokens <- tokens_remove(pos_four_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
pos_four_sdfm = dfm(pos_four_dollar_tokens,
           remove = swPhrases,
           ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .6,
           docfreq_type = "prop",
           min_termfreq = 5,
           termfreq_type = "count")

topfeatures(pos_four_sdfm,25)

pos_four_dollar_dtm <- convert(pos_four_sdfm, "topicmodels") ###

pos_four_tm2 = LDA(pos_four_dollar_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(pos_four_tm2, 15)

pos_four_tm3 = LDA(pos_four_dollar_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(pos_four_tm3, 15)

pos_four_tm4 = LDA(pos_four_dollar_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(pos_four_tm4, 15)

no_tm2beta <- tidy(pos_four_tm2, matrix = "beta")

no_tm2beta %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(-beta) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term,beta,topic)) %>% 
  ggplot(aes(term,beta)) +
  geom_col(fill = "#F15C4F" ) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Topic Model, Positive Reviews, $$$$ Restaurants ") +
  theme_minimal()


#########################################################################################
swPhrases <- c("ice_cream","bubble_tea","milk_tea","green_tea","noodle_soup","milk_tea","spring_rolls","fried_chicken",
               "red_bean","foie_gras","caesar_salad","come_back","coconut_cream","cream_pie","coconut_cream_pie",
               "duck_fat","highly_recommend","highly_recommended","holt_renfrew",
               "jerk_chicken","banh_mi","pork_belly","hot_sauce","pulled_pork","chicken_shawarma",
               "kensington_market","coffee_shop","ordered_chicken","burrito_boyz",
               "pad_thai","one_star","never_come","never_go","coming_back","going_back",
               "next_time","really_good","definitely_recommend","go_back","ruth_chris",
               "make_sure","coming_back","recommend_place","never_going","even_though",
               "butter_chicken","kimchi_fries","black_sesame","definitely_back","feel_like",
               "felt_like","baked_goods","much_better","prime_rib", "steak_house","four_seasons")

fourDollarNegative <- reviewsWPrice %>%
  filter(stars %in% c(1)) %>% 
  filter(resPrice == 4)


neg_four_dollar_tokens = tokens(fourDollarNegative$text, 
                            remove_punct = T,
                            remove_numbers = T)

neg_four_dollar_tokens <- tokens_remove(neg_four_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
neg_four_sdfm = dfm(neg_four_dollar_tokens,
           remove = swPhrases,
           ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .7,
           docfreq_type = "prop",
           #min_termfreq = 5,
           #termfreq_type = "count")
  )

topfeatures(neg_four_sdfm,15)

neg_four_dollar_dtm <- convert(neg_four_sdfm, "topicmodels") ###

neg_four_tm2 = LDA(neg_four_dollar_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(neg_four_tm2, 15)

neg_four_tm3 = LDA(neg_four_dollar_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(neg_four_tm3, 15)

neg_four_tm4 = LDA(neg_four_dollar_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(neg_four_tm4, 15)

neg_four_beta <- tidy(neg_four_tm2, matrix = "beta")


 neg_four_beta%>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  arrange(-beta) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term,beta,topic)) %>% 
  ggplot(aes(term,beta)) +
  geom_col(fill = "#5DBCD2" ) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Negative Four Beta") +
  theme_minimal()


#####################################################################################
#####################################################################################
swPhrases <- c("ice_cream","bubble_tea","milk_tea","green_tea","noodle_soup","milk_tea","spring_rolls","fried_chicken",
                "red_bean","foie_gras","caesar_salad","come_back","coconut_cream","cream_pie","coconut_cream_pie",
                "duck_fat","highly_recommend","highly_recommended","holt_renfrew",
                "jerk_chicken","banh_mi","pork_belly","hot_sauce","pulled_pork","chicken_shawarma",
                "kensington_market","coffee_shop","ordered_chicken","burrito_boyz",
                "pad_thai","one_star","never_come","never_go","coming_back","going_back",
                "next_time","really_good","definitely_recommend","go_back","ruth_chris",
                "make_sure","coming_back","recommend_place","never_going","even_though",
                "butter_chicken","kimchi_fries","black_sesame","definitely_back","feel_like",
                "felt_like","baked_goods","much_better","prime_rib", "steak_house","four_seasons",
                "love_place","great_place","one_best","place_great","pretty_good","love_love",
               "can_get","can_say","hk_milk_tea")
 
 
oneDollarPositive <- reviewsWPrice %>%
  filter(stars == 5) %>% 
  filter(resPrice == 1)

nrow(oneDollarPositive)

one_dollar_tokens = tokens(oneDollarPositive$text, 
                           remove_punct = T,
                           remove_numbers = T)

one_dollar_tokens <- tokens_remove(one_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(one_dollar_tokens,
           remove = swPhrases,
           ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .6,
           docfreq_type = "prop",
           min_termfreq = 4,
           termfreq_type = "count")

topfeatures(sdfm,20)

one_dollar_dtm <- convert(sdfm, "topicmodels") ###

one_tm2 = LDA(one_dollar_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(one_tm2, 15)

one_tm3 = LDA(one_dollar_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(one_tm3, 15)

one_tm4 = LDA(one_dollar_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(one_tm4, 15)

no_tm2beta <- tidy(one_tm2, matrix = "beta")

no_tm2beta %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(-beta) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term,beta,topic)) %>% 
  ggplot(aes(term,beta)) +
  geom_col(fill = "#F15C4F" ) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Topic Model, Positive Reviews, $ Restaurants ") +
  theme_minimal()

#########################################################################################
swPhrases <- c("ice_cream","bubble_tea","milk_tea","green_tea","noodle_soup","milk_tea","spring_rolls","fried_chicken",
               "red_bean","foie_gras","caesar_salad","come_back","coconut_cream","cream_pie","coconut_cream_pie",
               "duck_fat","highly_recommend","highly_recommended","holt_renfrew",
               "jerk_chicken","banh_mi","pork_belly","hot_sauce","pulled_pork","chicken_shawarma",
               "kensington_market","coffee_shop","ordered_chicken","burrito_boyz",
               "pad_thai","one_star","never_come","never_go","coming_back","going_back",
               "next_time","really_good","definitely_recommend","go_back","ruth_chris",
               "make_sure","coming_back","recommend_place","never_going","even_though",
               "butter_chicken","kimchi_fries","black_sesame","definitely_back","feel_like",
               "felt_like","baked_goods","much_better","prime_rib", "steak_house","four_seasons",
               "love_place","great_place","one_best","place_great","pretty_good","love_love",
               "can_get","can_say","hk_milk_tea", "avoid_place","never_coming","stay_away",
               "give_place", "never_go_back","last_night")


oneDollarNegative <- reviewsWPrice %>%
  filter(stars == 1) %>% 
  filter(resPrice == 1)

nrow(oneDollarNegative)

one_dollar_tokens = tokens(oneDollarNegative$text, 
                           remove_punct = T,
                           remove_numbers = T)

one_dollar_tokens <- tokens_remove(one_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(one_dollar_tokens,
           remove = swPhrases,
           ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .6,
           docfreq_type = "prop",
           min_termfreq = 5,
           termfreq_type = "count")

topfeatures(sdfm,15)

one_dollar_dtm <- convert(sdfm, "topicmodels") ###

one_tm2 = LDA(one_dollar_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(one_tm2, 15)

no_tm2beta <- tidy(one_tm2, matrix = "beta")

no_tm2beta %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  arrange(-beta) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term,beta,topic)) %>% 
  ggplot(aes(term,beta)) +
  geom_col(fill = "#F15C4F" ) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Topic Model, Negative Reviews, $ Restaurants ") +
  theme_minimal()

one_tm3 = LDA(one_dollar_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(one_tm3, 15)

one_tm4 = LDA(one_dollar_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(one_tm4, 15)


#########################################################################################
#looking at reviews with a certain phrase

#dplyr::filter(df, !grepl("RTB",TrackingPixel))


neg_text <- dplyr::filter(oneDollarNegative, !grepl('came | back',text))

head(neg_text$text,10)

