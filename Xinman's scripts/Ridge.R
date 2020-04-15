options(stringsAsFactors = FALSE)
options(digits=3)
library(Ckmeans.1d.dp)
library(xgboost)
library(cluster)
library(tidyverse)  
library(Matrix)
library(ggplot2)


library(zoo)
library(DescTools)
library(lubridate)

restaurants_lm <- read_csv("Data/canada_restaurants_ms.csv")
checkIns <- read_csv("Data/canada_checkins_ms.csv")

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

# Create month columns for checkins 
checkIns %>% group_by(business_id) %>% 
  mutate(j=ifelse(month(updatedcheckins)==1,1,0)) %>%
  mutate(f=ifelse(month(updatedcheckins)==2,1,0)) %>%
  mutate(mr=ifelse(month(updatedcheckins)==3,1,0)) %>%
  mutate(ap=ifelse(month(updatedcheckins)==4,1,0)) %>%
  mutate(m=ifelse(month(updatedcheckins)==5,1,0)) %>%
  mutate(ju=ifelse(month(updatedcheckins)==6,1,0)) %>%
  mutate(jl=ifelse(month(updatedcheckins)==7,1,0)) %>%
  mutate(au=ifelse(month(updatedcheckins)==8,1,0)) %>%
  mutate(s=ifelse(month(updatedcheckins)==9,1,0)) %>%
  mutate(o=ifelse(month(updatedcheckins)==10,1,0)) %>%
  mutate(n=ifelse(month(updatedcheckins)==11,1,0)) %>%
  mutate(d=ifelse(month(updatedcheckins)==12,1,0)) %>%
  summarize(jan=sum(j),feb=sum(f),mar=sum(mr),apr=sum(ap),may=sum(m),jun=sum(ju),
            jul=sum(jl),aug=sum(au),sep=sum(s),oct=sum(o),nov=sum(n),dec=sum(d)) -> checkIn_months

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
         price_range=attributes_RestaurantsPriceRange2,
         review_count=Winsorize(review_count, probs = c(0.05, 0.95))) %>%
  # Replace NAs with column median
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% 
  select(business_id, is_open, price_range, stars, review_count,
         attributes_RestaurantsReservations, attributes_RestaurantsTakeOut,
         attributes_RestaurantsDelivery, attributes_HasTV, attributes_WiFi,
         amb_casual, amb_trendy, amb_hipster, amb_touristy,
         amb_romantic, amb_intimate, amb_classy, amb_upscale,
         full_bar, beer_wine, attributes_RestaurantsAttire,
         attributes_GoodForKids, attributes_RestaurantsGoodForGroups,
         attributes_BikeParking, attributes_OutdoorSeating,
         park_street, park_validated, park_lot, park_garage, park_valet
  ) -> ml_df

ml_df %>% left_join(checkIn_months,by='business_id') %>% 
  select(-business_id) -> ml_df

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
view(data_train)
# Split Xs and Ys for Lasso, Random Forest, and Ridge
x_train_isOpen <- model.matrix(formula_isOpen, data_train)[, -1]
x_test_isOpen <- model.matrix(formula_isOpen, data_test)[, -1]

y_train_isOpen <- data_train$is_open
y_test_isOpen <- data_test[-1,]$is_open

################################# ridge
fit <- cv.glmnet(x_train_isOpen, y_train_isOpen, alpha = 0,family = "binomial")


#  find a value for best lambda to fit model
lambdas <- 10^seq(3, -2, by = -.1)
cv_fit <- cv.glmnet(x_train_isOpen, y_train_isOpen, alpha = 0, lambda = lambdas,family = "binomial")
plot(cv_fit)

opt_lambda <- cv_fit$lambda.min
opt_lambda

# final model
fit <- cv_fit$glmnet.fit

# predict
y_predicted <- predict(fit, s = opt_lambda, newx = x_test_isOpen )
y_predicted <- ifelse(y_predicted >= 0.5, 1, 0)
y_predicted <- as.numeric(y_predicted)

#  accuracy 74.4%
ac = table(y_test_isOpen, y_predicted)
yardstick::accuracy(ac)

# coefficient
coef(fit_ridge)


#### positive variable
# stars, review_count, attributes_RestaurantsTakeout, attributes_RestaurantsDelivery, attributes_hasTV, attributes_WIFI
# amb_casual, amb_touristy, amb_upscale, attributes_GoodForkids, attributes_RestaurantsGoodForGroups, park_lot,
# And month are all positive and Sep are highest
