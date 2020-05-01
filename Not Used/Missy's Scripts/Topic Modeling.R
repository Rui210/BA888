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

#dividing reviews into categories to analyze################################################

#creating tables for one dollar sign restaurants
negativeOneDollar <- reviewsWPrice %>%
  filter(stars %in% c(1,2)) %>% 
  filter(resPrice == 1)

positiveOneDollar <- reviewsWPrice %>%
  filter(stars %in% c(4,5)) %>% 
  filter(resPrice == 1)

#creating tables for two dollar sign restaurants
negativeTwoDollar <- reviewsWPrice %>%
  filter(stars %in% c(1,2)) %>% 
  filter(resPrice == 2)

positiveTwoDollar <- reviewsWPrice %>%
  filter(stars %in% c(4,5)) %>% 
  filter(resPrice == 2)

#creating tables for three dollar sign restaurants
negativeThreeDollar <- reviewsWPrice %>%
  filter(stars %in% c(1,2)) %>% 
  filter(resPrice == 3)

positiveThreeDollar <- reviewsWPrice %>%
  filter(stars %in% c(4,5)) %>% 
  filter(resPrice == 3)

#creating tables for four dollar sign restaurants
negativeFourDollar <- reviewsWPrice %>%
  filter(stars %in% c(1,2)) %>% 
  filter(resPrice == 4)

positiveFourDollar <- reviewsWPrice %>%
  filter(stars %in% c(4,5)) %>% 
  filter(resPrice == 4)

##################### Topic Modeling by Category #######################################

sw1 <- c(stopwords(),"just", "one", "really","much","us","like","much","go",
         "restaurant","also","can","want","went", "get", "ordered", "definitely", "friend",
         "try","tried")

########### one dollar sign, negative reviews #########################################

no_dfm <- dfm(negativeOneDollar$text,
             remove_punct = T, 
             remove_numbers = T,
             remove = sw1, 
             stem = T) %>% 
  dfm_trim(min_termfreq = 10,
           termfreq_type = "count",
           max_docfreq =  .7,
           docfreq_type = "prop") 

no_dtm <- convert(no_dfm, "topicmodels")

no_tm2 = LDA(no_dtm, k = 3, control = list(seed = 888))

no_tm2beta <- tidy(no_tm2, matrix = "beta")
no_tm2gamma <- tidy(no_tm2, matrix = "gamma")

no_tm2beta %>% 
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
  labs(title = "Topic Model, 3 Topics, Negative, One Dollar Sign") +
  theme_minimal()

################################### One dollar sign positive

po_dfm <- dfm(positiveOneDollar$text,
              remove_punct = T, 
              remove_numbers = T,
              remove = sw1, 
              stem = T) %>% 
  dfm_trim(min_termfreq = 10,
           termfreq_type = "count",
           max_docfreq =  .7,
           docfreq_type = "prop") 

po_dtm <- convert(po_dfm, "topicmodels")

po_tm2 = LDA(po_dtm, k = 3, control = list(seed = 888))

po_tm2beta <- tidy(po_tm2, matrix = "beta")
po_tm2gamma <- tidy(po_tm2, matrix = "gamma")

po_tm2beta %>% 
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
  labs(title = "Topic Model, 3 Topics, Positive, One Dollar Sign") +
  theme_minimal()

########### one dollar sign, all reviews #########################################

oneDollar <- reviewsWPrice %>% filter(reviewsWPrice$resPrice == 1)
#glimpse(oneDollar)

o_dfm <- dfm(oneDollar$text,
              remove_punct = T, 
              remove_numbers = T,
              remove = sw1) %>% 
  dfm_trim(min_termfreq = 10,
           termfreq_type = "count",
           max_docfreq =  .7,
           docfreq_type = "prop") 

o_dtm <- convert(o_dfm, "topicmodels")

o_tm2 = LDA(o_dtm, k = 3, control = list(seed = 888))

o_tm2beta <- tidy(o_tm2, matrix = "beta")
o_tm2gamma <- tidy(o_tm2, matrix = "gamma")

o_tm2beta %>% 
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
  labs(title = "Topic Model, 2 Topics, All Reviews, One Dollar Sign") +
  theme_minimal()

#####################################################################################

#####################################################################################

########### one dollar sign, negative reviews #########################################

ntwo_dfm <- dfm(negativeTwoDollar$text,
              remove_punct = T, 
              remove_numbers = T,
              remove = sw1, 
              stem = T) %>% 
  dfm_trim(min_termfreq = 10,
           termfreq_type = "count",
           max_docfreq =  .7,
           docfreq_type = "prop") 

ntwo_dtm <- convert(ntwo_dfm, "topicmodels")

ntwo_tm2 = LDA(ntwo_dtm, k = 3, control = list(seed = 888))

ntwo_tm2beta <- tidy(ntwo_tm2, matrix = "beta")
ntwo_tm2gamma <- tidy(ntwo_tm2, matrix = "gamma")

ntwo_tm2beta %>% 
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
  labs(title = "Topic Model, Negative, Two Dollar Sign") +
  theme_minimal()

##################################### Positive two dollar sign reviews
ntwo_dfm <- dfm(positiveTwoDollar$text,
                remove_punct = T, 
                remove_numbers = T,
                remove = sw1, 
                stem = T) %>% 
  dfm_trim(min_termfreq = 10,
           termfreq_type = "count",
           max_docfreq =  .7,
           docfreq_type = "prop") 

ntwo_dtm <- convert(ntwo_dfm, "topicmodels")

ntwo_tm2 = LDA(ntwo_dtm, k = 3, control = list(seed = 888))

ntwo_tm2beta <- tidy(ntwo_tm2, matrix = "beta")
ntwo_tm2gamma <- tidy(ntwo_tm2, matrix = "gamma")

ntwo_tm2beta %>% 
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
  labs(title = "Topic Model, Positive, Two Dollar Sign") +
  theme_minimal()

################################## all two dollar sign reviews
twoDollar <- reviewsWPrice %>% filter(resPrice == 2)

two_dfm <- dfm(positiveTwoDollar$text, ####
                remove_punct = T, 
                ngrams = 2,
                remove_numbers = T,
                remove = sw1, 
                stem = T) %>% 
  dfm_trim(min_termfreq = 10,
           termfreq_type = "count",
           max_docfreq =  .7,
           docfreq_type = "prop") 

two_dtm <- convert(two_dfm, "topicmodels") ###

two_tm2 = LDA(two_dtm, k = 2, control = list(seed = 888)) ###   ###

two_tm2beta <- tidy(two_tm2, matrix = "beta") ###   ###
two_tm2gamma <- tidy(two_tm2, matrix = "gamma") ###    ####

two_tm2beta %>%             ###
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
  labs(title = "Topic Model, All Two Dollar Sign") +
  theme_minimal()

########## examfple of topic modeling with bigrams
nrow(oneDollar)
set.seed(888)
#train_index <- sample(1:nrow(adult), 0.8 * nrow(adult))

sample <- sample(1:nrow(oneDollar), (.2*nrow(oneDollar)))
oneDollar_sample <- oneDollar[sample,"text"]
length(oneDollar_sample)


one_dollar_tokens1 = tokens(oneDollar_sample, 
                 remove_punct = T,
                 remove_numbers = T)
                #ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(one_dollar_tokens1,
           remove = sw,
          ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .8,
           docfreq_type = "prop",
           min_termfreq = 5,
           termfreq_type = "count")

#sdfm

two_dtm <- convert(sdfm, "topicmodels") ###

two_tm2 = LDA(two_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(two_tm2, 15)

two_tm3 = LDA(two_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(two_tm3, 15)

two_tm4 = LDA(two_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(two_tm4, 15)


######################################
fourDollars <- reviewsWPrice %>% filter(resPrice == 4)
nrow(fourStars)

four_dollar_tokens = tokens(fourDollars$text, 
                            remove_punct = T,
                            remove_numbers = T)

head(four_dollar_tokens)

## dfm and combine two smartword sources
four_dollar_dfm = dfm(four_dollar_tokens,
           remove = sw,
           ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .8,
           docfreq_type = "prop",
           min_termfreq = 5,
           termfreq_type = "count")

#sdfm

four_dtm <- convert(four_dollar_dfm, "topicmodels") ###

four_tm2 = LDA(four_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(four_tm2, 15)

four_tm3 = LDA(four_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(four_tm3, 15)

four_tm4 = LDA(four_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(four_tm4, 15)

################################################################################

###############################################################33

four_dollar_dfm_nosw = dfm(four_dollar_tokens,
                      remove = sw)

four_dollar_tokens = tokens(fourDollars$text, 
                            remove_punct = T,
                            remove_numbers = T)


sw <- c(stopwords(),"ã")

four_dollar_tokens <- tokens_remove(four_dollar_tokens, pattern = sw)

four_dollar_dfm = dfm(four_dollar_tokens,
                      remove = sw,
                      ngrams = 3) %>%
  dfm_trim(max_docfreq = .7,
           docfreq_type = "prop",
           min_termfreq = 5,
           termfreq_type = "count")

topfeatures(four_dollar_dfm,15)

#sdfm

four_dtm <- convert(four_dollar_dfm, "topicmodels") ###

four_tm2 = LDA(four_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(four_tm2, 15)

four_tm3 = LDA(four_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(four_tm3, 15)

four_tm4 = LDA(four_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(four_tm4, 15)

#########################################################################################
#########################################################################################

oneDollar_sample <- oneDollar[sample,"text"]
length(oneDollar_sample)


one_dollar_tokens = tokens(oneDollar$text, 
                            remove_punct = T,
                            remove_numbers = T)

one_dollar_tokens <- tokens_remove(one_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(one_dollar_tokens,
           remove = sw,
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
terms

##############################################################################################
##############################################################################################
sw <- c(stopwords(),"ã","brã","lã","e")

fourDollarPositive <- reviewsWPrice %>%
  filter(stars %in% c(4,5)) %>% 
  filter(resPrice == 4)

nrow(fourDollarPositive)

four_dollar_tokens = tokens(fourDollarPositive$text, 
                           remove_punct = T,
                           remove_numbers = T)

four_dollar_tokens <- tokens_remove(four_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(four_dollar_tokens,
           remove = sw,
           ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .8,
           docfreq_type = "prop",
           min_termfreq = 5,
           termfreq_type = "count")

topfeatures(sdfm,15)

four_dollar_dtm <- convert(sdfm, "topicmodels") ###

four_tm2 = LDA(four_dollar_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(four_tm2, 15)

four_tm3 = LDA(four_dollar_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(four_tm3, 15)

four_tm4 = LDA(four_dollar_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(four_tm4, 15)

#########################################################################################
fourDollarNegative <- reviewsWPrice %>%
  filter(stars %in% c(1,2)) %>% 
  filter(resPrice == 4)

nrow(fourDollarNegative)

four_dollar_tokens = tokens(fourDollarNegative$text, 
                            remove_punct = T,
                            remove_numbers = T)

four_dollar_tokens <- tokens_remove(four_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(four_dollar_tokens,
           remove = sw,
           ngrams = 2:4) %>%
  dfm_trim(max_docfreq = .8,
           docfreq_type = "prop",
           min_termfreq = 5,
           termfreq_type = "count")

topfeatures(sdfm,15)

four_dollar_dtm <- convert(sdfm, "topicmodels") ###

four_tm2 = LDA(four_dollar_dtm, k = 2, control = list(seed = 888)) ###   ###
terms(four_tm2, 15)

four_tm3 = LDA(four_dollar_dtm, k = 3, control = list(seed = 888)) ###   ###
terms(four_tm3, 15)

four_tm4 = LDA(four_dollar_dtm, k = 4, control = list(seed = 888)) ###   ###
terms(four_tm4, 15)

#####################################################################################
#####################################################################################

oneDollarPositive <- reviewsWPrice %>%
  filter(stars %in% c(4,5)) %>% 
  filter(resPrice == 1)

nrow(oneDollarPositive)

one_dollar_tokens = tokens(oneDollarPositive$text, 
                            remove_punct = T,
                            remove_numbers = T)

one_dollar_tokens <- tokens_remove(one_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(one_dollar_tokens,
           remove = sw,
           ngrams = 2:4) %>%
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

#########################################################################################
oneDollarNegative <- reviewsWPrice %>%
  filter(stars %in% c(1,2)) %>% 
  filter(resPrice == 1)

one_dollar_tokens = tokens(oneDollarNegative$text, 
                            remove_punct = T,
                            remove_numbers = T)

one_dollar_tokens <- tokens_remove(one_dollar_tokens, pattern = sw)
#ngrams = 2)

## dfm and combine two smartword sources
sdfm = dfm(one_dollar_tokens,
           remove = sw,
           ngrams = 2:4) %>%
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
