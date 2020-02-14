library("rjson")
library("tidyverse")

library(jsonlite)
yelpbusiness <- fromJSON("888/yelp_academic_dataset_business.json")
yelpbusiness <- stream_in(file("888/yelp_academic_dataset_business.json"))
yelpbusiness <- as.data.frame(flatten(yelpbusiness))

glimpse(yelpbusiness)

write.csv(yelpcheckin, file = "888/Data as a CSV/yelpbusiness.csv")



############## check in file
yelpcheckin <- stream_in(file("888/yelp_academic_dataset_checkin.json"))
yelpcheckin <- as.data.frame(flatten(yelpcheckin))

glimpse(yelpcheckin)

write.csv(yelpcheckin, file = "888/Data as a CSV/yelpcheckin.csv")
View(yelpcheckin)

############## tip file
yelptip <- stream_in(file("888/yelp_academic_dataset_tip.json"))
yelptip <- as.data.frame(flatten(yelptip))

dim(yelptip)
glimpse(yelptip)

write.csv(yelpcheckin, file = "888/Data as a CSV/yelptip.csv")

############## user file
yelpuser <- stream_in(file("888/yelp_academic_dataset_user.json"))
yelpuser <- as.data.frame(flatten(yelpuser))

dim(yelpuser)
glimpse(yelpuser)
colnames(yelpuser)

write.csv(yelpcheckin, file = "888/Data as a CSV/yelpuser.csv")

############## business file

yelpuser <- stream_in(file("888/yelp_academic_dataset_business.json"))
yelpuser <- as.data.frame(flatten(yel))

dim(yelpuser)
glimpse(yelpuser)
colnames(yelpuser)

write.csv(yelpcheckin, file = "888/Data as a CSV/yelpuser.csv")

####################### review
yelpreview <- stream_in(file("888/yelp_academic_dataset_review.json"))
yelpreview <- as.data.frame(flatten(yelpreview))

dim(yelpreview)
glimpse(yelpreview)

write.csv(yelpcheckin, file = "888/Data as a CSV/yelpreview.csv")
# yelpcheckin is the data frame name
# then the /../.... is a file name you create 

########################################### user file
yelpuser <- stream_in(file("888/yelp_academic_dataset_user.json"))
yelpuser <- as.data.frame(flatten(yelpuser))

dim(yelpuser)
glimpse(yelpuser)

write.csv(yelpuser, file = "888/Data as a CSV/yelpuser1.30.csv")
# yelpcheckin is the data frame name
# then the /../.... is a file name you create 

######################################### check in data from sql
yelpcheckinsql <- stream_in(file("888/checkins_from_sql.json"))
yelpcheckinsql <- as.data.frame(flatten(yelpcheckinsql))

dim(yelpcheckinsql)
glimpse(yelpcheckinsql)




