library(tidyverse)
library(zoo)
library(randomForest)
library(gbm)

restaurants_lm <- read_csv("canada_restaurants_ms.csv")

############################DATA CLEANING#################################

# Check NA count of columns
View(as.data.frame(colSums(is.na(restaurants_lm))))
# attributes_BusinessAcceptsCreditCards	7158  -- too many NAs to keep col -- DELETE
# attributes_Caters	2296  -- over 30% NAs, not worth keeping -- DELETE
# attributes_NoiseLevel	1592
# attributes_Alcohol	1436
# attributes_HasTV	1335
# attributes_RestaurantsAttire	1329  -- make 0.None 1.casual 2.dressy 3.formal

# attributes_Ambience	1296  -- {x:y} format
unique(unlist(str_split(restaurants_lm$attributes_Ambience, ' ' )))
## casual - trendy - hipster - divey - touristy - romantic - intimate - classy - upscale

# attributes_WiFi	1267  -- 0.no/none 1.paid 2.free
# attributes_RestaurantsReservations	1196
# attributes_GoodForKids	1193
# attributes_RestaurantsGoodForGroups	1126
# attributes_RestaurantsDelivery	1074
# attributes_BikeParking	1005

# attributes_BusinessParking	390 --{x:y} format
unique(unlist(str_split(restaurants_lm$attributes_BusinessParking, ' ' )))
## garage - street - validated - lot - valet 

# Turn unique ambience values into columns
restaurants_lm$amb_casual <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'casual': True"), 1, 0)
restaurants_lm$amb_trendy <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'trendy': True"), 1, 0)
restaurants_lm$amb_hipster <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'hipster': True"), 1, 0)
restaurants_lm$amb_divey <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'divey': True"), 1, 0)
restaurants_lm$amb_touristy <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'touristy': True"), 1, 0)
restaurants_lm$amb_romantic <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'romantic': True"), 1, 0)
restaurants_lm$amb_intimate <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'intimate': True"), 1, 0)
restaurants_lm$amb_classy <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'classy': True"), 1, 0)
restaurants_lm$amb_upscale <- ifelse(str_detect(restaurants_lm$attributes_Ambience, "'upscale': True"), 1, 0)

# Turn 5 different parking options into separate columns and create dummies
restaurants_lm$park_garage <- ifelse(str_detect(restaurants_lm$attributes_BusinessParking, "'garage': True"), 1, 0)
restaurants_lm$park_street <- ifelse(str_detect(restaurants_lm$attributes_BusinessParking, "'street': True"), 1, 0)
restaurants_lm$park_validated <- ifelse(str_detect(restaurants_lm$attributes_BusinessParking, "'validated': True"), 1, 0)
restaurants_lm$park_lot <- ifelse(str_detect(restaurants_lm$attributes_BusinessParking, "'lot': True"), 1, 0)
restaurants_lm$park_valet <- ifelse(str_detect(restaurants_lm$attributes_BusinessParking, "'valet': True"), 1, 0)

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
# Make two columns from alcohol attribute: full_bar and beer_and_wine
restaurants_lm$full_bar <- ifelse(restaurants_lm$attributes_Alcohol=="full_bar", 1, 0)
restaurants_lm$beer_wine <- ifelse(restaurants_lm$attributes_Alcohol=="beer_and_wine", 1, 0)

# Make dummies for RestaurantsAttire: none = 0; casual = 1; dressy = 2; formal = 3
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="None"] <- 0
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="'casual'"] <- 1
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="u'casual'"] <- 1
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="'dressy'"] <- 2
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="u'dressy'"] <- 2
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="'formal'"] <- 3
restaurants_lm$attributes_RestaurantsAttire[restaurants_lm$attributes_RestaurantsAttire=="u'formal'"] <- 3

# Make dummies for WiFi: none/no = 0; paid = 1; free = 2
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="None"] <- 0
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="u'no'"] <- 0
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="'no'"] <- 0
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="u'paid'"] <- 1
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="'paid'"] <- 1
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="u'free'"] <- 2
restaurants_lm$attributes_WiFi[restaurants_lm$attributes_WiFi=="'free'"] <- 2

# Make new df w/ columns for ML application -- ml_df
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
         price_range=attributes_RestaurantsPriceRange2 ) %>%
  # Replace NAs with column median
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% 
  select(is_open, price_range, stars, review_count, 
         attributes_RestaurantsReservations, attributes_RestaurantsTakeOut,
         attributes_RestaurantsDelivery, attributes_HasTV, attributes_WiFi,
         amb_casual, amb_trendy, amb_hipster, amb_touristy, amb_divey,
         amb_romantic, amb_intimate, amb_classy, amb_upscale,
         full_bar, beer_wine, attributes_RestaurantsAttire,
         attributes_GoodForKids, attributes_RestaurantsGoodForGroups,
         attributes_BikeParking, attributes_OutdoorSeating,
         park_street, park_validated, park_lot, park_garage, park_valet
  ) -> ml_df

############################SPLIT DATA FOR TRAIN/TEST############################

# Train/Test Split 
set.seed(007)

# Create formula where y = is+_open
formula_isOpen <- as.formula(is_open ~ ., ml_df)

# Split train (70%) and test (30%)
train <- round(0.7 * nrow(ml_df))
test <- nrow(ml_df) - train

train_index <- sample(nrow(ml_df), train) # index of random rows 

# Train and test data 
data_train <- ml_df[train_index,]
data_test <- ml_df[-train_index,]

# Split Xs and Ys for Lasso, Random Forest, and Ridge
x_train_isOpen <- model.matrix(formula_isOpen, data_train)[, -1]
x_test_isOpen <- model.matrix(formula_isOpen, data_test)[, -1]

y_train_isOpen <- data_train$is_open
y_test_isOpen <- data_test$is_open
view(y_train_isOpen)

############################Forward Stepwise Regression############################
xnames=colnames(ml_df)
xnames = xnames[!xnames %in% "is_open"]
fit_fw <- lm(is_open ~ 1, data = data_train)
yhat_train <- predict(fit_fw, data_train)
mse_train <- mean((data_train$is_open - yhat_train)^2)
yhat_test <- predict(fit_fw, data_test)
mse_test <- mean((data_test$is_open - yhat_test)^2)
mse_test

log_fw <-
  tibble(
    xname = "xname",
    model = paste0(deparse(fit_fw$call), collapse = ""),
    mse_train = mse_train,
    mse_test = mse_test
  )
xnamesfw <- xnames
while (length(xnamesfw) > 0) {
  best_mse_train <- NA
  best_mse_test <- NA
  best_fit_fw <- NA
  best_xname <- NA
  # select the next best predictor
  for (xname in xnamesfw) {
    fit_fw_tmp <- update(fit_fw, as.formula(paste0(". ~ . + ", xname)))
    yhat_train_tmp <- predict(fit_fw_tmp, data_train)
    mse_train_tmp <- mean((data_train$is_open - yhat_train_tmp) ^ 2)
    yhat_test_tmp <- predict(fit_fw_tmp, data_test)
    mse_test_tmp <- mean((data_test$is_open - yhat_test_tmp) ^ 2)
    if (is.na(best_mse_test) | mse_test_tmp < best_mse_test) {
      best_xname <- xname
      best_fit_fw <- fit_fw_tmp
      best_mse_train <- mse_train_tmp
      best_mse_test <- mse_test_tmp
    }
  }
  log_fw <- log_fw %>% add_row(
    xname = best_xname,
    model = paste0(deparse(best_fit_fw$call), collapse = ""),
    mse_train = best_mse_train,
    mse_test = best_mse_test )
  fit_fw <- best_fit_fw
  xnamesfw <- xnamesfw[xnamesfw!=best_xname]
}
log_fw

ggplot(log_fw, aes(seq_along(xname), mse_test)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y=mse_train), color="blue") +
  geom_line(aes(y=mse_train), color="blue") +
  scale_x_continuous("Variables", labels = log_fw$xname, breaks = seq_along(log_fw$xname)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Forward Selection Train MSE and Test MSE") +
  ylab("MSE")

## Minimum test MSE for forward selection:
log_fw[which.min(log_fw$mse_test),]$mse_test
## The full model for forward selection:
log_fw[which.min(log_fw$mse_test),]$model
## Forward selection coefficients:
best_fw_model <- as.formula("is_open ~ review_count + full_bar + park_lot + stars +
                              attributes_WiFi + park_street + amb_trendy + beer_wine +
                              amb_romantic + attributes_RestaurantsReservations + amb_casual+
                              attributes_BikeParking + park_valet + amb_touristy + park_garage +
                              amb_hipster + amb_divey + amb_upscale + park_validated")
best_fw_model <- lm(best_fw_model, data = data_train)
coef(best_fw_model)### one problem here: the coefficient for amb_divery is NA ###

# The final forward selection model contained 19 variables(please notice that the coefficient for amb_divery is NA). 
# As expected, many of the variables describing the characteristics of the restaurants were included in the model. 
# From the above results, it is clear that park_street, park_validated,park_lot, park_valet, attributes_BikeParking,amb_romantic, stars have higher correlations to whether the restaurant is open or not.
# I would say that customers pay more attention to parking in restaurants. Therefore, the hotel's open and close have a strong relationship with parking.

############################Random Forecast############################
lf <- "is_open ~ price_range"
for (i in 2:length(xnames)) {
  as.character(xnames[i])
  lf <- paste(lf, "+", xnames[i], sep = " ")
}
f <- as.formula(lf)
x_train <- model.matrix(f,data_train)[ ,-1]
y_train <- data_train$is_open
x_test <- model.matrix(f,data_test)[ , -1]
y_test <- data_test[ ,"is_open"]

x_train_isOpen <- model.matrix(formula_isOpen, data_train)[, -1]
x_test_isOpen <- model.matrix(formula_isOpen, data_test)[, -1]

y_train_isOpen <- data_train$is_open
y_test_isOpen <- data_test$is_open


fit_rf <- randomForest(f,
                       data_train,
                       ntree=500,
                       do.trace=F)
varImpPlot(fit_rf)
yhat_train_rf <- predict(fit_rf,data_train)
mse_rf_train <- mean((yhat_train_rf-y_train_isOpen)^2)
mse_rf_train
yhat_test_rf <- predict(fit_rf,data_test)
mse_rf_test <-mean((yhat_test_rf-y_test_isOpen)^2)
mse_rf_test

# The final random forest model included 29 variables. 
# Using the variable importance plot we can see that the variables related to the attributes of the restaurant are the most predictive. 
# On the plot we can see “review_count”, “star”, “price_range”, “attributes_WiFi”, “attributes_Has TV”, “attributes_RestaurantsDelivey”, “attributes_OutdoorSeating”, and “park_street” are the most predictive with
# IncNodepurity all higher than 30. We got MSE for test dataset is only 0.1712097.