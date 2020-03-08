library(tidyverse)

restaurants <- read_csv("Data/cananda_restaurant.csv")
reviews <- read_csv("Data/canada_reviews.csv")


ggplot(restaurants)+
  geom_bar(aes(x=city,fill=factor(stars)),position="dodge")+
  scale_fill_manual(values=c("#990000","#D32323","#F15C4F","#FF3300","#FF3366","#FF6666","#FFCCCC","#F8ADA8","#FCD6D3")) +
  #scale_fill_gradient(low="yellow", high="red")+
  #geom_bar(fill=c("#F15C4F", "#FCD6D3","#F8ADA8"
  labs(title="Distribution of star ratings in Different City",
       fill=guide_legend(title="Stars"))

  