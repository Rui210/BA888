############################LOGISTIC REGRESSION MODEL#################################
library(smbinning) # converts a continuous vars into a categoricals w/ recursive partitioning

inputData <- ml_df

# Convert categorical vars
inputData$attributes_RestaurantsReservations = ifelse(
  inputData$attributes_RestaurantsReservations==1, "True", "False")
inputData$attributes_RestaurantsTakeOut = ifelse(
  inputData$attributes_RestaurantsTakeOut==1, "True", "False")
inputData$attributes_RestaurantsDelivery = ifelse(
  inputData$attributes_RestaurantsDelivery==1, "True", "False")
inputData$attributes_HasTV = ifelse(
  inputData$attributes_HasTV==1, "True", "False")

inputData$amb_casual = ifelse(
  inputData$amb_casual==1, "True", "False")
inputData$amb_trendy = ifelse(
  inputData$amb_trendy==1, "True", "False")
inputData$amb_hipster = ifelse(
  inputData$amb_hipster==1, "True", "False")
inputData$amb_touristy = ifelse(
  inputData$amb_touristy==1, "True", "False")
inputData$amb_divey = ifelse(
  inputData$amb_divey==1, "True", "False")
inputData$amb_romantic = ifelse(
  inputData$amb_romantic==1, "True", "False")
inputData$amb_intimate = ifelse(
  inputData$amb_intimate==1, "True", "False")
inputData$amb_classy = ifelse(
  inputData$amb_classy==1, "True", "False")
inputData$amb_upscale = ifelse(
  inputData$amb_upscale==1, "True", "False")
inputData$full_bar = ifelse(
  inputData$full_bar==1, "True", "False")
inputData$beer_wine = ifelse(
  inputData$beer_wine==1, "True", "False")
inputData$attributes_GoodForKids = ifelse(
  inputData$attributes_GoodForKids==1, "True", "False")
inputData$attributes_RestaurantsGoodForGroups = ifelse(
  inputData$attributes_RestaurantsGoodForGroups==1, "True", "False")
inputData$attributes_BikeParking = ifelse(
  inputData$attributes_BikeParking==1, "True", "False")
inputData$attributes_OutdoorSeating = ifelse(
  inputData$attributes_OutdoorSeating==1, "True", "False")
inputData$park_street = ifelse(
  inputData$park_street==1, "True", "False")
inputData$park_validated = ifelse(
  inputData$park_validated==1, "True", "False")
inputData$park_lot = ifelse(
  inputData$park_lot==1, "True", "False")
inputData$park_garage = ifelse(
  inputData$park_garage==1, "True", "False")
inputData$park_valet = ifelse(
  inputData$park_valet==1, "True", "False")

inputData$attributes_WiFi[inputData$attributes_WiFi==0] <- "none"
inputData$attributes_WiFi[inputData$attributes_WiFi==1] <- "paid"
inputData$attributes_WiFi[inputData$attributes_WiFi==2] <- "free"

inputData$attributes_RestaurantsAttire[inputData$attributes_RestaurantsAttire==0] <- "none"
inputData$attributes_RestaurantsAttire[inputData$attributes_RestaurantsAttire==1] <- "casual"
inputData$attributes_RestaurantsAttire[inputData$attributes_RestaurantsAttire==2] <- "dressy"
inputData$attributes_RestaurantsAttire[inputData$attributes_RestaurantsAttire==3] <- "formal"

table(inputData$is_open)

# Create Training Data (70%)
input_ones <- inputData[which(inputData$is_open == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$is_open == 0), ]  # all 0's
set.seed(007)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))  # 0's for training
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data (30%)
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's

# WOE 
# for(factor_var in factor_vars){
#   inputData[[factor_var]] <- WOE(X=inputData[, factor_var], Y=inputData$ABOVE50K)
# }

# segregate continuous and factor variables
factor_vars <- c ("attributes_RestaurantsReservations", "attributes_RestaurantsTakeOut", "attributes_RestaurantsDelivery",
                  "attributes_HasTV", "attributes_WiFi", "amb_casual","amb_trendy","amb_hipster",
                  "amb_touristy","amb_divey","amb_romantic","amb_intimate","amb_classy","amb_upscale",
                  "full_bar","beer_wine","attributes_RestaurantsAttire","attributes_GoodForKids",
                  "attributes_RestaurantsGoodForGroups","attributes_BikeParking",
                  "attributes_OutdoorSeating","park_street","park_validated","park_lot",
                  "park_garage","park_valet")
continuous_vars <- c("price_range","stars","review_count" )

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(29))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="is_open", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # check if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df