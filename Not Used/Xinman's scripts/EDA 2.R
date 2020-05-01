library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
restaurant <- read_csv("cananda_restaurant.csv")
skimr::skim(restaurant)
View(restaurant)

# The total restaurants are in the dataset is 6514. 
# There are some same restaurant in different address, if we count all these restaurant, the total number is 7777.
length(unique(restaurant$name))
length(restaurant$name)


# restaurants are in each city
## We can see that there are 1422 restaurants in Calgary, 808 restaurants in Mississauga and 5547 restaurants in Toronto. 
restaurant %>% group_by(city) %>% count(city)
restaurant %>% 
  ggplot(aes(x=city))+
  geom_bar(fill=c("#F15C4F", "#FCD6D3","#F8ADA8"))+
  theme_classic()+
  labs(title = "The Number of Restaurant in Each City",
       y="Amount")

# Restaurants are opened or closed
# There are 1993 restaurants closed and there are 5784 restaurants opened.
open <- restaurant %>% group_by(is_open) %>% count(is_open)
open[open$is_open==0,1]="no"
open[open$is_open==1,1]="yes"
open
open %>% 
  ggplot(aes(is_open,n))+
  geom_col(fill=c("#F15C4F", "#F8ADA8"))+
  labs(title = "Restaurants are Opened or Closed",
       x="is open?",
       y="Amount")+
  theme_classic()

# Distribution of Price Range
# There are almost restaurants are in price range 2.
restaurant %>% 
  ggplot(aes(attributes_RestaurantsPriceRange2))+
  geom_histogram(binwidth=1,fill="#F15C4F",color="#4D4D4D")+
  labs(title = "Distribution of Price Range",
       x="Price Range")+
  theme_classic()


