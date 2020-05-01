############################LINEAR REGRESSION MODEL#################################

# Fit Linear Regression Model 
lm.fit = lm(is_open ~ ., data = data_train)

# Model accuracy
xtest <- model.matrix(is_open ~.,data_test)[,-1]
probabilities <- lm.fit %>% predict(newx = xtest)
pred.classes <- ifelse(probabilities > 0.5, 1, 0)

obs.classes <- data_test$is_open

mean(pred.classes == obs.classes) * 100 # 73.9% accuracy 

# Linear Model Diagnostics 
options(scipen = 999)

lmSum <- summary(lm.fit) 
#lmSum
stargazer::stargazer(lm.fit, type="text")

# Positive significant variables (5% - 1%):
#### stars - review_count - WiFi - park_lot - Delivary

# Negative significant variables (5% - 1%):
#### WiFi - full_bar - beer_wine - BikeParking - park_street - winter - spring - reservations
