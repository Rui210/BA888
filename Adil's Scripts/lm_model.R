############################LINEAR REGRESSION MODEL#################################

# Fit Linear Regression Model 
lm.fit = lm(is_open ~ ., data = train)

# Calculate Train MSE
yhat_train_lm <- predict(lm.fit)
mse_train_lm <- mean((y_train_isOpen - yhat_train_lm)^2)

# Calculate Test MSE
yhat_test_lm <- predict(lm.fit, test)
mse_test_lm <- mean((y_test_isOpen - yhat_test_lm)^2)

# Compare MSEs
mse_train_lm
mse_test_lm

# Linear Model Diagnostics 
options(scipen = 999)
round(coef(lm.fit),3)

lmSum <- summary(lm.fit) 
lmSum

# Positive significant variables:
#### 

# Negative significant variables:
#### 

### USE KNN 