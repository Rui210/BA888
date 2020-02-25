library(tidyverse)
library(ggplot2)
library(lubridate)
restaurants <- read_csv("/Users/Adil/Documents/RStudio/capstoneData/cananda_restaurant.csv")
dim(restaurants)
reviews <- read_csv("/Users/Adil/Documents/RStudio/capstoneData/canada_reviews.csv")
checkIns <- read_csv("/Users/Adil/Documents/RStudio/capstoneData/canada_checkin.csv")

dim(reviews)
summary(reviews)

review_length <- sapply(strsplit(reviews$text, " "), length)
review_length <- as.data.frame(review_length)

ggplot(review_length) + geom_histogram(aes(x=review_length)) +
  labs(x="review length") +
  theme_minimal() 




  