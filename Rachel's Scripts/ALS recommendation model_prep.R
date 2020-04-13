library(tidyverse)
library(lubridate)
options(scipen=999)

reviews = read_csv('canada_reviews_ms.csv')
data = reviews[,c(10,3,9)]
write_csv(data, "ALS_data.csv")
