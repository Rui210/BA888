library(tidyverse)
library(zoo)

restaurants_lm <- read_csv("Data/canada_restaurants_ms.csv")

############################DATA CLEANING#################################

# Check NA count of columns
View(as.data.frame(colSums(is.na(restaurants_lm))))
# attributes_BusinessAcceptsCreditCards	7158  -- too many NAs to keep col
# attributes_Caters	2296  -- over 30% NAs, not worth keeping
# attributes_NoiseLevel	1592
# attributes_Alcohol	1436
# attributes_HasTV	1335
# attributes_RestaurantsAttire	1329  -- make 0.None 1.casual 2.dressy 3.formal
# attributes_Ambience	1296  -- {x:y} format
# attributes_WiFi	1267  -- 0.no/none 1.paid 2.free
# attributes_RestaurantsReservations	1196
# attributes_GoodForKids	1193
# attributes_RestaurantsGoodForGroups	1126
# attributes_RestaurantsDelivery	1074
# attributes_BikeParking	1005

# Make dummies for NoiseLevel
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

# Make dummies for Alcohol 
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="u'full_bar'"]="full_bar"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="'full_bar'"]="full_bar"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="u'beer_and_wine'"]="beer_and_wine"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="'beer_and_wine'"]="beer_and_wine"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="'none'"]="None"
restaurants_lm$attributes_Alcohol[restaurants_lm$attributes_Alcohol=="u'none'"]="None"
unique(restaurants_lm$attributes_Alcohol)

# Make dummies for RestaurantsAttire
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="None"] <- 0
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="'casual'"] <- 1
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="u'casual'"] <- 1
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="'dressy'"] <- 2
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="u'dressy'"] <- 2
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="'formal'"] <- 3
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="u'formal'"] <- 3

# Make dummies for WiFi
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="None"] <- 0
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="u'no'"] <- 0
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="'no'"] <- 0
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="u'paid'"] <- 1
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="'paid'"] <- 1
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="u'free'"] <- 2
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="'free'"] <- 2

# Make new df w/ columns for LM -- lm_df
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
         attributes_WiFi=as.numeric(attributes_WiFi),
         attributes_RestaurantsAttire=as.numeric(attributes_RestaurantsAttire),
         attributes_full_bar=ifelse(attributes_Alcohol=="full_bar",1,0),
         attributes_beer_and_wine=ifelse(attributes_Alcohol=="beer_and_wine",1,0)) %>%
  # Replace NAs with column median
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% 
  select(is_open, attributes_GoodForKids, attributes_RestaurantsReservations,
         attributes_RestaurantsPriceRange2, attributes_RestaurantsTakeOut, 
         attributes_OutdoorSeating, attributes_HasTV, stars,
         attributes_RestaurantsGoodForGroups, attributes_WiFi,
         attributes_RestaurantsAttire, attributes_RestaurantsDelivery,
         attributes_NoiseLevel, attributes_BikeParking, 
         attributes_full_bar, attributes_beer_and_wine) -> lm_df

############################LINEAR REGRESSION MODEL#################################

# Train/Test Split 
set.seed(999)
sample.size <- floor(0.75 * nrow(lm_df))
train.index <- sample(seq_len(nrow(lm_df)), size = sample.size)
train_lm <- lm_df[train.index, ]
test_lm <- lm_df[- train.index, ]

y_train <- train_lm$is_open
y_test <- test_lm$is_open

# Fit Linear Regression Model 
lm.fit = lm(is_open ~ ., data = train_lm)

# Prediction
is_openPredict <- predict(lm.fit, test_lm)

actuals_preds <- data.frame(cbind(actuals=test_lm$is_open, predicteds=is_openPredict))
(correlation_accuracy <- cor(actuals_preds))
head(actuals_preds,15)

# Calculate Train MSE
y_hat_train <- predict(lm.fit, train_lm)
mse_train_lm <- mean((y_hat_train - y_train)^2)

# Calculate Test MSE
y_hat_test <- predict(lm.fit, test_lm)
mse_test_lm <- mean((y_hat_test - y_test)^2)

# Compare MSEs
mse_test_lm
mse_train_lm

# Linear Model Diagnostics 
options(scipen = 999)
round(coef(lm.fit),3)

lmSum <- summary(lm.fit) 
lmSum

# Positive significant variables:
#### GoodForKids, NoiseLevel, WiFi

# Negative significant variables:
#### full_bar, BikeParking

### USE KNN 
### FIX AMBIANCE and BUSINESSPARKING
