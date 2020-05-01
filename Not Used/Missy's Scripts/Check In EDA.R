library(tidyverse)
library(stringr)

checkin <- read.csv("888/Data as a CSV/yelpcheckin.csv")

checkin2 %>% 
  filter(state !in (""))

nrow(restaurants)
nrow(checkin)

glimpse(restaurants)
glimpse(checkin)

checkin <- inner_join(restaurants,checkin, by = "business_id")

glimpse(checkin)

checkin <- checkin %>% 
  select(business_id, name, city, stars, review_count, is_open, categories, date)

glimpse(checkin)

#separate(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
 #        convert = FALSE, extra = "warn", fill = "warn", ...)

# checkins <- checkin %>%
#   #select(categories) %>%
#   separate(date, into = paste0("v", 1:20000), sep = ",") %>%
#   pivot_longer(cols = v1:v10000,
#                names_to = "pos",
#                values_to = "token",
#                values_drop_na = TRUE)
# #  filter(str_length(token) > 0) %>%
# # group_by(token) %>%
# #  count(sort = T)

glimpse(checkin)
checkin$date <- as.character(checkin$date)

checkin2 <- checkin %>% 
  mutate(checkincount = str_count(checkin$date, ',')+1)



checkin2 %>% 
  filter(is_open == 1) %>% 
  ggplot() +
  geom_jitter(aes(x = stars, y = checkincount, col = as.character(stars)), alpha = .5)+
  labs(title = "Check-Ins by Star Status (Open Restaurants)",
       x="Stars",
       y="Check-In Count",
       col = "Stars")+
  theme_minimal()

colnames(checkin2)
glimpse(checkin2)

checkin2 %>% 
  select(checkincount, stars) %>% 
  arrange(desc(checkincount)) %>% 
  head(10)

checkin2 %>% 
  select(checkincount, stars, city) %>% 
  group_by(city) %>% 
  summarise(meanCheckIns = mean(checkincount))
  


summarise(num_docs = length(unique(crmid)),
          n = n(),
          avg_pos = mean(pos)) %>% 
  mutate(doc_pct = num_docs / nreviews,
         n_per_doc = n /num_docs) %>% 
  arrange(desc(n))


  
glimpse(checkin2)

d <- b %>%
  group_by(depart_time, airline) %>%
  summarise(avg_arrive = mean(arrive_delay, na.rm = TRUE))
d

