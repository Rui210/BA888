library(tidyverse)
library(lubridate)
library(ggplot2)

checkins <- read.csv("../Classes/888/Canada CSVs/canada_checkin.csv")

checkins$date <- as_date(checkins$updatedcheckins)
checkins$year <- year(checkins$date)

#min(checkins$date)
#max(checkins$date)

checkins$month_year <- format(as.Date(checkins$updatedcheckins), "%Y-%m")

365*10

checkinsforchart <- checkins %>% 
  select(month_year) %>% 
  group_by(month_year) %>% 
  add_count() %>%
  distinct()

checkinsforchart %>% 
  ggplot() +
  geom_line(aes(x = month_year, y = n, group = 1)) + 
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90, hjust=1))
  
