# Load --------------------------------------------------------------------

# Load packages
library(tidyverse)
library(ggmap)
library(modelr)
library(caret)
library(broom)

# Import datasets
prices <- read_csv(file = ("data/processed/prices.csv"), 
                   col_types = cols("n", col_factor(NULL), 
                   col_factor(NULL), "n", "n", "n", "c", "n", "n"))

# Amenities
amen <- prices$amenities
# Output vector
amen_list <- vector("list", length(amen))
for (i in seq_along(amen)){
  # Remove extra punctuation
  amen_list[[i]] = str_remove_all(amen[[i]], pattern = '[^([A-Z][a-z][0-9]|,|\'|\\s)]') %>% 
    # Split by comma
    str_split(pattern = ",")
}
prices[["amenities"]] <- amen_list


# Explore -----------------------------------------------------------------

# Explore Dataset
dim(prices)
map_df(prices, class)
prices %>% head()
prices %>% glimpse()
summary(prices[-c(7)])

# Price Distributions

# Room Type
# Price vs Room Type
prices %>% 
  ggplot(aes(x = room_type, y = price)) + 
  geom_boxplot() + 
  labs(title = "Price vs Room Type", x = "Room Type", y = "Price")

# Facet by bedrooms
prices %>% 
  filter(bedrooms <= 5) %>% 
  ggplot(aes(x = room_type, y = price)) +
  geom_boxplot() +
  facet_wrap(~bedrooms) + 
  coord_flip() +
  labs(title = "Price vs Room Type by Number of Bedrooms",
       x = "Room Type", y = "Price")

# Color by bedrooms
prices %>% 
  filter(bedrooms <= 5) %>% 
  group_by(room_type) %>% 
  mutate(avg_price = mean(price)) %>% 
  ggplot(aes(x = room_type, color = as.factor(bedrooms))) +
  geom_boxplot(aes(y = price)) + 
  geom_point(aes(y = avg_price, size = 1), color = "red") +
  labs(title = "Price vs Room Type by Number of Bedrooms",
       x = "Room Type", y = "Price", color = "Number of Bedrooms",
       size = "Average Price")

# Neighborhood
# Find avg price per neighborhood
prices_neigh <- prices %>% 
  group_by(neighbourhood_cleansed) %>% 
  mutate(avg_price = round(mean(price), 2))

# Label neighborhoods by price range
prices_neigh[["neigh_cat"]] <- cut(prices_neigh$avg_price, 6,
                             labels = c("Cheapest", "Cheap", "Moderately Cheap",
                                        "Moderately Expensive", "Expensive", "Most Expensive"))

# Neighborhood groups
prices_neigh %>% 
  select(neighbourhood_cleansed, neigh_cat) %>% 
  group_by(neighbourhood_cleansed) %>% 
  unique() %>% 
  arrange(neigh_cat, neighbourhood_cleansed)

prices_neigh %>% 
  select(neighbourhood_cleansed, neigh_cat) %>% 
  group_by(neigh_cat) %>% 
  summarize(count = n())

prices_neigh %>% 
  ggplot(aes(x = neigh_cat)) +
    geom_bar()

# Price vs Neighborhood Group
prices_neigh %>% 
  ggplot(aes(x = as.factor(neigh_cat), y = price)) +
  geom_boxplot() +
  labs(x = "Neighborhood Group", y = "Price") +
  coord_flip()

# Price vs Neighborhood Group + Accommodates
prices_neigh %>% 
  ggplot(aes(y = price)) +
  geom_boxplot(aes(x = as.factor(neigh_cat), color = as.factor(accommodates))) +
  labs(x = "Neighborhood Group", y = "Price", color = "Bedrooms") +
  coord_flip()

# Price vs neighborhood group and room type
prices_neigh %>% 
  mutate(price = price/max(price)) %>% 
  ggplot(aes(x = room_type, y = neigh_cat)) + 
  geom_tile(aes(fill = price)) + 
  labs(title = "Price vs Neighborhood Group and Room Type", 
       x = "Room Type", y = "Neighborhood Group")

# Price vs neighborhood accommodates
prices %>% 
  filter(accommodates <= 6) %>% 
  ggplot(aes(x = as.factor(accommodates), y = neighbourhood_cleansed)) + 
  geom_tile(aes(fill = price)) + 
  scale_fill_gradientn(colors = c("#c1e8ff", "#002b42")) + 
  labs(title = "Price vs Neighborhood Group and Accommodates", 
       x = "Accommodates", y = "Neighborhood Group")

# Locations
sf <- get_stamenmap(bbox = c(left = -122.5164, bottom = 37.7066, 
        right = -122.3554, top = 37.8103), 
        maptype = c("toner-lines"), zoom = 13)

ggmap(sf) + 
  geom_point(data = prices, 
             aes(x = longitude, y = latitude, color = price),
             alpha = 0.7) +
  scale_colour_gradient(low = '#a4faff', high = '#00275b') +
  labs(title = "Prices by Location")
  
# Bedrooms
# Price vs bedrooms
prices %>% 
  filter(bedrooms <= 6) %>% 
  ggplot(aes(x = as.factor(bedrooms), y = price)) +
  geom_boxplot(aes(color = room_type)) + 
  geom_smooth(aes(x = bedrooms), se = FALSE, model="lm") + 
  labs(x = "Bedrooms", y = "Price", color = "Room Type")

# Price vs bedrooms by neighborhood
prices_neigh %>% 
  filter(bedrooms <= 4) %>% 
  ggplot(aes(x = as.factor(bedrooms), y = price)) +
  geom_boxplot(aes(color = as.factor(neigh_cat))) + 
  geom_smooth(aes(x = as.numeric(bedrooms)), se = FALSE, model="lm") + 
  labs(x = "Bedrooms", y = "Price", color = "Neighborhood Group")

# Accommodates
prices %>% 
  ggplot(aes(x = accommodates)) +
  geom_bar()

# Price vs accommodates by room type
prices %>% 
  ggplot(aes(x = as.factor(accommodates), y = price)) +
  geom_boxplot(aes(color = room_type)) +
  geom_smooth(aes(x = accommodates), se = FALSE, color = "purple") + 
  labs(title = "Price vs Number Accommodated",
       x = "Number Accommodated", y = "Price", color = "Room Type")

# Price vs accommodates by bedrooms
prices %>% 
  filter(bedrooms <= 5, accommodates <= 10) %>% 
  mutate(price = price/max(price)) %>% 
  ggplot(aes(x = as.factor(accommodates), y = as.factor(bedrooms))) +
  geom_raster(aes(fill = price))


# Bathrooms
# vs bedrooms
prices %>% 
  ggplot(aes(x = bathrooms, y = bedrooms)) +
  geom_tile(aes(fill = price))

# Price
prices %>% 
  filter(bedrooms <= 5, bathrooms <= 5) %>% 
  ggplot(aes(x = as.factor(bathrooms), y = price)) +
  geom_boxplot() + 
  facet_wrap(~bedrooms) +
  labs(title = "Price vs Number of Bathrooms per Number of Bedrooms", 
       x = "Number of Bathrooms", y = "Price")

# Amenities
# Count frequency
amen_freq <- prices$amenities %>% 
  unlist() %>% 
  table()

# Remove unwanted rows
amen_freq <- amen_freq[-c(1, 168, 169)]

# Top 15
amen_freq %>%
  sort(decreasing = TRUE) %>% 
  head(15)

# Split into two groups by price
amen_prices <- prices %>% 
  mutate(above_average = ifelse(price > median(price), TRUE, FALSE))

# Top amenities per group
expensive_amenities <- amen_prices %>% 
  filter(above_average == TRUE) %>% 
  select(amenities) %>% 
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE)

expensive_amenities %>% head(10)
expensive_amenities %>% tail(10)
  
cheap_amenities <- amen_prices %>% 
  filter(above_average == FALSE) %>% 
  select(amenities) %>% 
  unlist() %>% 
  table() %>% 
  sort(decreasing = TRUE)

cheap_amenities %>% head(10)
cheap_amenities %>% tail(10)

# Model -------------------------------------------------------------------

# Model dataset
dataset <- prices %>% 
  select(-amenities)

# Train-Test Split
set.seed(100)
pindex <- createDataPartition(dataset$price, p = 0.8, list = FALSE)
ptrain <- dataset[pindex,]
ptest <- dataset[-pindex,]

# Linear model
# all data
price_lm_1 <- ptrain %>% 
  lm(price ~ ., data = .)

# all except lat and lon
price_lm_2 <- ptrain %>% 
  lm(price ~ neighbourhood_cleansed + room_type + accommodates
     + bathrooms + bedrooms, data = .)

# all except neighborhood
price_lm_3 <- ptrain %>% 
  lm(price ~ room_type + accommodates + bathrooms + 
       bedrooms + latitude + longitude, data = .)

summary(price_lm_1)
summary(price_lm_2)
summary(price_lm_3)

# Add predictions and residuals
ptest <- ptest %>% 
  add_predictions(model = price_lm_1, var = "1_pred") %>% 
  add_predictions(model = price_lm_2, var = "2_pred") %>%
  add_predictions(model = price_lm_3, var = "3_pred") %>%
  add_residuals(model = price_lm_1, var = "1_resid") %>% 
  add_residuals(model = price_lm_2, var = "2_resid") %>% 
  add_residuals(model = price_lm_3, var = "3_resid")

c(sum(ptest$`1_resid`^2),
  sum(ptest$`2_resid`^2),
  sum(ptest$`3_resid`^2))