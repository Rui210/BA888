library(tidyverse)
library(ggplot2)
library(lubridate)
restaurants <- read_csv("Data/cananda_restaurant.csv")
reviews <- read_csv("Data/canada_reviews.csv")
checkin <- read_csv("Data/canada_checkin.csv")

dim(reviews)
summary(reviews)

review_length <- sapply(strsplit(reviews$text, " "), length)
review_length <- as.data.frame(review_length)

# Dist count
ggplot(review_length) + geom_histogram(aes(x=review_length),bins=25,
                                       fill="#D32323") +
  labs(title="Distribution of Review Length",x="review length") +
  theme_minimal()

# Dist freq 
ggplot(data=review_length, aes(x=review_length)) +
geom_histogram(aes(y = stat(count) / sum(count)),
               bins = 30, fill="#D32323") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Distribution of Review Length",
       x="Review Length",y="Relative Frequency") +
  theme_minimal() + scale_x_continuous(breaks=seq(0,1000,60))

print(paste0("mean ", (mean(review_length$review_length))))
print(paste0("median ", (median(review_length$review_length))))

## The distribution of the review length is right-skewed with
## a mode of around 30 to 60 words, which represents the word count
## of over 20% of all reviews. More than 50% of all the reviews
## are between 30 to 120 words. The average review length is
## around 130 words while the median is around 100 words. 



  