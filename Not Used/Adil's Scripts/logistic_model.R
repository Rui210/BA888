############################BINOMIAL LOGISTIC REGRESSION MODEL#################################

# Confirm there are no missing values 
sapply(data_train,function(x) sum(is.na(x)))
sapply(data_train, function(x) length(unique(x)))

# Fit binomial logistic regression model
model <- glm(is_open ~.,family=binomial(link='logit'),data=data_train)

# Make predictions on the test data
x.test <- model.matrix(is_open ~.,data_test)[,-1]
probabilities <- predict(model, newx = x.test, type='response')
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy
observed.classes <- data_test$is_open
mean(predicted.classes == observed.classes) * 100  # 72.4% accuracy

# Logistic model diagnostics 
options(scipen = 999)

#summary(model)
stargazer::stargazer(model, type='text')
