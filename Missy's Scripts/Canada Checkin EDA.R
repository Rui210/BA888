library(tidyverse)
library(lubridate)
library(ggplot2)

checkins <- read.csv("../Classes/888/Canada CSVs/canada_checkin.csv")

checkins$date <- as_date(checkins$updatedcheckins)
checkins$year <- year(checkins$date)

#min(checkins$date)
#max(checkins$date)

checkins$month_year <- format(as.Date(checkins$updatedcheckins), "%Y-%m")

365*10

checkinsforchart <- checkins %>% 
  select(month_year) %>% 
  group_by(month_year) %>% 
  add_count() %>%
  distinct()

checkinsforchart %>% 
  ggplot() +
  geom_line(aes(x = month_year, y = n, group = 1), color = "#D32323", size = 2) + 
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(title = "Check-In by Day", x = "date", y="count")


restaurants <- read.csv("Data/canada_restaurants_ms_edits.csv")
checkins <- read.csv("Data/canada_checkins_ms.csv")

glimpse(checkins)
checkins$date <- as_date(checkins$updatedcheckins)
checkins$year <- year(checkins$date)

restaurantprice <- restaurants %>% select(business_id, attributes_RestaurantsPriceRange2)
restaurantprice$business_id <- as.character(restaurantprice$business_id)
checkins$business_id <- as.character(checkins$business_id)

reviewsWPrice <- left_join(checkins, restaurantprice, by = "business_id")

reviewsWPrice <- reviewsWPrice %>% rename(resPrice = attributes_RestaurantsPriceRange2)

### Distribution of star ratings by city

restaurants %>% 
  count(city, stars) %>% 
  group_by(city) %>% 
  mutate(percent = n / sum(n) ) %>% 
  ungroup() %>% 
  ggplot(aes(x = city, y = percent, fill = as.factor(stars))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Distribution of Star Ratings by City", x = "City", y = "Percent of Restaurants", fill = "Star Rating") +
  scale_fill_manual(values = c("#D32323", "#F15C4F", "#F8ADA8", "#FCD6D3", "#E6E6E6", "#999999", "#CCCCCC","#666666","#4D4D4D","#333333")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 13)) +
  theme(axis.title = element_text(size = 13)) 

### top unique words for reviews

library(quanteda)

sampleReviews <- sample_n(reviews, 200000)

tokens = tokens(reviews$text, 
                remove_punct = T,
                remove_numbers = T)

sw <- c(stopwords(),"ã","brã","lã","e")

tokens <- tokens_remove(tokens, pattern = sw)

sdfm = dfm(tokens,
           ngrams = 2)

topBigrams <- topfeatures(sdfm,10)
topBigrams <- as.data.frame(topBigrams)

names(topBigrams)[1] <- "Count"
bigrams <- rownames(topBigrams)
rownames(topBigrams) <- NULL
topBigrams$Bigrams <- bigrams

topBigrams %>% 
  arrange(desc(Count)) %>% 
  #sort(Count, decreasing = TRUE) %>% 
  ggplot() +
  geom_col(aes(x = reorder(Bigrams, Count), y = Count), fill = "#F15C4F") +
  coord_flip() +
  theme_minimal() +
  labs(title="Customer Review Top Bigrams", x = "Bigram", y = "Count") +
  theme(axis.text = element_text(size = 13)) +
  theme(axis.title = element_text(size = 13))





