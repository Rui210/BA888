library(tidyverse)
library(zoo)

restaurants_lm <- read_csv("Data/canada_restaurants_ms.csv")

restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="None"] <- 0
restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="'quiet'"] <- 1
restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="u'quiet'"] <- 1
restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="u'average'"] <- 2
restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="'average'"] <- 2
restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="'loud'"] <- 3
restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="u'loud'"] <- 3
restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="u'very_loud'"] <- 4
restaurants_lm$attributes_NoiseLevel[restaurants_lm$attributes_NoiseLevel=="'very_loud'"] <- 4
unique(restaurants_lm$attributes_NoiseLevel)

restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="u'full_bar'"]="full_bar"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="'full_bar'"]="full_bar"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="u'beer_and_wine'"]="beer_and_wine"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="'beer_and_wine'"]="beer_and_wine"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="'none'"]="None"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="u'none'"]="None"
unique(restaurants_lm$attributes_Alcohol)

restaurants_lm %>% 
  mutate(attributes_GoodForKids=1*attributes_GoodForKids,
         attributes_RestaurantsReservations=1*attributes_RestaurantsReservations,
         attributes_RestaurantsTakeOut=1*attributes_RestaurantsTakeOut,
         attributes_OutdoorSeating=1*attributes_OutdoorSeating,
         attributes_HasTV=1*attributes_HasTV,
         attributes_RestaurantsGoodForGroups=1*attributes_RestaurantsGoodForGroups,
         attributes_RestaurantsDelivery=1*attributes_RestaurantsDelivery,
         attributes_BikeParking=1*attributes_BikeParking,
         attributes_NoiseLevel=as.numeric(attributes_NoiseLevel),
         attributes_full_bar=ifelse(attributes_Alcohol=="full_bar",1,0),
         attributes_beer_and_wine=ifelse(attributes_Alcohol=="beer_and_wine",1,0)) %>%
  # Replace NAs with column median
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% 
  select(is_open, attributes_GoodForKids, attributes_RestaurantsReservations,
         attributes_RestaurantsTakeOut, attributes_OutdoorSeating,
         attributes_HasTV, attributes_RestaurantsGoodForGroups,
         attributes_RestaurantsDelivery, attributes_NoiseLevel,
         attributes_BikeParking, attributes_full_bar,
         attributes_beer_and_wine) -> lm_df

# Train/Test Split 
set.seed(999)
sample.size <- floor(0.75 * nrow(lm_df))
train.index <- sample(seq_len(nrow(lm_df)), size = sample.size)
train_lm <- lm_df[train.index, ]
test_lm <- lm_df[- train.index, ]

# Fit Linear Regression Model 
lm.fit = lm(is_open ~ ., data = train_lm)

# LM Diagnostics
lmSum <- summary(lm.fit)

# Prediction
is_openPredict <- predict(lm.fit, test_lm)

actuals_preds <- data.frame(cbind(actuals=test_lm$is_open, predicteds=is_openPredict))
(correlation_accuracy <- cor(actuals_preds))
head(actuals_preds,15)

# Calculate MSE
x <- test_lm[,-1]
p <- predict(lm.fit, data.frame(x))

sum((test_lm$is_open - predict(lm.fit,data.frame(x)))^2)
(mse_test_value <- mean((test_lm$is_open - predict(lm.fit,data.frame(x)))^2))

# Another MSE calculation
mean(lmSum$residuals^2)
