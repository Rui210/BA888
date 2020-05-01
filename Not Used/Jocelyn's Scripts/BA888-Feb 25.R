library(tidyverse)
checkin=read.csv("Data/canada_chickin.csv")
restaurant=read.csv("Data/cananda_restaurant.csv")
dim(checkin)
dim(restaurant)
view(restaurant)
glimpse(checkin)
glimpse(restaurant)

### Distribution of check-in count
checkin$business_id=as.character(checkin$business_id)
restauranu_count=checkin %>% group_by(business_id) %>% count(.)

ggplot(checkin, aes(x=business_id)) +
  geom_histogram(stat="count",fill="#D32323") +
  labs(title="Distribution of check-in count")+
  theme_classic()

ggplot(restauranu_count, aes(x=business_id,y=n)) +
  geom_line(color="#D32323") +
  labs(title="Distribution of check-in count")+
  theme_classic()
# histogram could show on my screen, but geom_line show nothing.

### Checkins by Star Ratings
restaurant$business_id=as.character(restaurant$business_id)
new_table=left_join(x=checkin,y=restaurant)
checkin_count=new_table %>% group_by(stars) %>% count(.)
glimpse(new_table)
ggplot(new_table, aes(x=stars)) +
  geom_histogram(stat="count",fill="#D32323") +
  labs(title="Checkins by Star Ratings")+
  theme_classic()
##4 stars have the largest numbers of checkins, followed by 3.5 and 3. While 5 and 1 stars
##have the smallest number of checkins maybe because it is too expensive.

### Distribution of star ratings
ggplot(restaurant)+
  geom_bar(aes(x=city,fill=factor(stars)),position="dodge")+
  labs(title="Distribution of star ratings in Different City",
       fill=guide_legend(title="Stars"))
##Toronto has the largest number of hotels, and Toronto also has the largest number of 5, 4 
##and 3 stars resutaurants.
