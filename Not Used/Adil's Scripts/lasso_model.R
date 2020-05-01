############################LASSO LOGISTIC REGRESSION MODEL#################################
library(glmnet)
library(dplyr)

## Find the best lambda using cross-validation
set.seed(007) 
cv.lasso <- cv.glmnet(x_train_isOpen, y_train_isOpen, alpha = 1, family = "binomial")

# Fit the final model on the training data
model <- glmnet(x_train_isOpen, y_train_isOpen, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

# Make predictions on the test data
x.test <- model.matrix(is_open ~.,data_test)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy
observed.classes <- data_test$is_open
mean(predicted.classes == observed.classes) * 100

set.seed(007)
cv.lasso <- cv.glmnet(x_train_isOpen, y_train_isOpen, alpha = 1, family = "binomial")
plot(cv.lasso)

############################ Final model with lambda.min ############################
lasso.model <- glmnet(x_train_isOpen, y_train_isOpen, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)

# Make prediction on test data
x.test <- model.matrix(is_open ~., data_test)[,-1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy based on min
observed.classes <- data_test$is_open
mean(predicted.classes == observed.classes) * 100

############################ Final model with lambda.1se ############################
lasso.model <- glmnet(x_train_isOpen, y_train_isOpen, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.1se)

# Make prediction on test data
x.test <- model.matrix(is_open ~., data_test)[,-1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy based on 1se
observed.classes <- data_test$is_open
mean(predicted.classes == observed.classes) * 100

####### lambda.1se model is the most accurate with 77% accuracy #######
coef(cv.lasso, cv.lasso$lambda.1se)

# Positive significant variables:
#### park_lot - attributes_GoodForKids - stars - attributes_RestaurantsTakeOut
#### attributes_WiFi - review_count - summer

# Negative significant variables:
#### attributes_OutdoorSeating - amb_classy - attributes_RestaurantsReservations - amb_trendy
#### attributes_RestaurantsAttire - attributes_BikeParking - full_bar - park_street
