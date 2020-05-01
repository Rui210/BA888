# Can Yelp Reviews Bridge the Gap Between Consumer Preferences and Industry Priorities? 

Finding restaurants through directory services apps and review forum apps have become a crucial part of business discovery. It has changed the way restaurants of different tiers attain brand loyalty and market themselves.   

Yelp’s terabytes of historical data on restaurants’ reviews, check-ins, and special attributes have led to the ability to uncover valuable trends to aid businesses in understanding their impact on customers and what makes customers tick.   

How can a restaurant leverage information on Yelp to increase their chances of attaining their business goals and preventing permanent closure? What customer characteristics are reflected in reviews and ratings?

***Please refer to the Project Summary for a detailed report of the project.***

## Prerequisites

### Original Dataset
1. Yelp business dataset about business types, attributes, location,  review count, and stars 
2. User dataset about reviews on each restaurant or service: text, rating, and reactions (useful, cool, funny)
3. Check-ins for business
4. 5.2 million (will be cleaned/filtered by restaurant) of 11 metropolitan areas in 4 countries

### Data Cleaning

This project focuses on 3 Canadian cities Calgary, Mississauga, and Toronto. In order to ensure rich, non-sparse data for the remainder of restaurants in the dataset, we further reduced the restaurant list to include only the restaurants with the top 50% of reviews. With these criteria, only restaurants with 14 or more reviews were included in the study. More information on the data cleaning can be found in the project summary. 

## Ways to solve our problem
### Descriptive + Diagnostic Analytics
Study the most essential aspects of a restaurant based on price level to understand what most customers value.
e.g. Do customers at low-price restaurants value the same “things” as mid-price and high-price restaurants?

### Predictive Analytics
Predict whether a restaurant will go out of business in the near future based on the importance of variables. <br>
Create a recommendation system that predicts users’ preferences based on their reviews. 

## Exploratory Data Analysis

### Restaurant Overview
Toronto has the majority of restaurants 5,372 (70%), followed by Calgary with 1,365 (18%), and finally Mississauga with 794 (11%).
The dataset contains 5,699 (76%) open restaurants and 1,831 (24%) restaurants that have gone out of business

### Star Ratings and Review Length
The majority of restaurants in the dataset have a 3.5-star rating, closely followed by 4-star restaurants.
Reviewers who gave 1 and 2-star reviews write longer reviews than reviewers giving 3, 4, and 5-star reviews.
The average length of 2-star reviews, the shortest average reviews were 150 words, while the average length of 5-star reviews, the longest average reviews is 113 words.

### Yelp Users and Reviews
The number of new customers increased until 2015. Review posted on yelp increased since 2008 but growth went to slow in 2016 and 2017
In 2018, the review amount drops. However, it could due to no data on reviews in December.

### User Activity
**Active Users Definition:** users whose total review in one particular year is more than the average reviews posted by all Yelp users in that year. <br>
From users who at least posted one review, in 2018, 22% of users were active compared to 24% in 2017.
Inactive users wrote less, but were more extreme in their ratings, 'bad to worst and good to best'.
Active users wrote longer reviews and gave more 'mild' star ratings to restaurants. 

## Methods
### Do Yelpers Value the Same Aspects of $ vs. $$$$ Restaurants?
Goal: Use topic modeling to see if themes differ depending on if the restaurant is classified as inexpensive ($) vs. expensive ($$$$)

### Study User Behavior
Goal: Use cohort analysis to study trends in users posting reviews on Yelp.

### What features of a restaurant are the most significant in predicting that a restaurant will stay in business?
Goal: Use Random Forest and XGBoost models to understand variable importance. Use Logistic regression models (OLS, Lasso, Ridge) and Stepwise regression models to examine the relationship between attributes and chances of staying open.

## Conclusion
### Cohort Analysis:
The first quarter is the core period to encourage users to post reviews. 
### Topic Modeling:
$$$$ - restaurants should focus on creating an experience that is out of the ordinary  because many customers that visit are coming to celebrate a special occasion <br>
$ - restaurants should focus on training and hiring employees that are friendly as their interactions with customers are shorter and they have less time to make a good impression; they should also be aware that how employees are working behind the counter, when not interacting with customers, can have an impact on the customer’s perception of service. 
### Supervised Models:
High review count and star ratings have the strongest significance in predicting that a restaurant will stay in business. <br>
The Presence of a parking lot has a high positive correlation with staying open, while just having street parking is highly negatively correlated. <br>
Kid-friendly restaurants and a high volume of check-ins during the summer also appear to be positively correlated with staying in business. <br>

***Please refer to the Project Summary for a detailed report of the project.***
