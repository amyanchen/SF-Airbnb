# Load --------------------------------------------------------------------

# Load packages
library(tidyverse)
library(tidytext)

# Import datasets
sentiments <- read_csv("data/processed/sentiments.csv")
listings <- read_csv("data/processed/listings.csv")

# Explore -----------------------------------------------------------------

sentiments %>% head()

# sentiments per review
listing_sent <- sentiments %>% 
  # group by reviewer
  group_by(reviewer_id) %>% 
  # count positive and negatve sentiments
  count(listing_id, sentiment) %>% 
  # make new positive and negative count columns
  spread(key = sentiment, value = n, fill = 0) %>%  
  # calculate overall sentiment per review
  mutate(sent = positive - negative)

# average sentiments per listing
listing_sent <- listing_sent %>% 
  # group by listing
  group_by(listing_id) %>% 
  # average sentiment per listing
  summarize(avg_sent = mean(sent)) %>%
  # join with original dataset
  left_join(listings, by = c("listing_id" = "id"))

listing_sent %>% 
  arrange(desc(avg_sent)) %>% 
  select(listing_id:number_of_reviews, review_scores_rating) %>% 
  head(10)

# sentiment vs scores
listing_sent %>% 
  ggplot(aes(x = as.factor(review_scores_rating), y = avg_sent)) +
  geom_boxplot() +
  labs(title = "Average Sentiment vs Review Scores",
       x = "Review Scores", y = "Average Sentiment")

# top positive words
positive_10 <- sentiments %>% 
  filter(sentiment == "positive") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  select(-sentiment) %>% 
  head(10)

# top negative words
negative_10 <- sentiments %>% 
  filter(sentiment == "negative") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  select(-sentiment) %>% 
  head(10)

# avg sent vs price
listing_sent %>% 
  ggplot(aes(x = price, y = avg_sent)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm") + 
  labs(title = "Average Sentiment Score vs Price",
       x = "Price", y = "Average Sentiment Score")

# avg sent vs price remove high prices
listing_sent %>% 
  filter(price <= 250) %>% 
  ggplot(aes(x = price, y = avg_sent)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm") + 
  labs(title = "Average Sentiment Score vs Price",
       x = "Price", y = "Average Sentiment Score")
