# Load --------------------------------------------------------------------

# Load packages
library(tidyverse)
library(tidytext)

# Load datasets
listings <- read_csv(file = ("data/unprocessed/listings_sf.csv"))
reviews <- read_csv(file = ("data/unprocessed/reviews.csv"))


# Listings ----------------------------------------------------------------


# Clean -------------------------------------------------------------------

# Examine variables
listings$host_location %>% unique()
listings$street %>% unique()
listings$neighbourhood %>% unique()
listings$neighbourhood_cleansed %>% unique()
sum(is.na(listings$square_feet))/length(listings$square_feet)

# Dollar character to numeric
dollar_num <- function(amt) {
  as.numeric(str_sub(amt, 2))
}

# Clean data
listings_cleaned <- listings %>% 
  # Drop uninformative columns
  select(-(listing_url:last_scraped),
         -experiences_offered,
         -(thumbnail_url:xl_picture_url),
         -(host_url:host_name),
         -(host_location:host_about),
         -host_verifications,
         -host_neighbourhood,
         -(host_thumbnail_url:host_picture_url),
         -street,
         -square_feet,
         -(neighbourhood_group_cleansed:country),
         -is_location_exact,
         -jurisdiction_names,
         -(name:house_rules)) %>% 
  # Convert to numbers
  mutate(
    price = dollar_num(price),
    weekly_price = dollar_num(weekly_price),
    monthly_price = dollar_num(monthly_price),
    security_deposit = dollar_num(security_deposit),
    cleaning_fee = dollar_num(cleaning_fee)
  )


# Prices ------------------------------------------------------------------

# Amenities
class(listings$amenities)
amen <- listings_cleaned$amenities
# Output vector
amen_list <- vector("list", length(amen))
for (i in seq_along(amen)){
  # Remove extra punctuation
  amen_list[[i]] = str_remove_all(amen[[i]], pattern = '[^([A-Z][a-z][0-9]|,|\'|\\s)]') %>% 
    # Split by comma
    str_split(pattern = ",")
}
# listings_cleaned[["amenities_list"]] <- amen_list

# Price Dataset
prices <- listings_cleaned %>% 
  # Remove missing prices
  filter(!is.na(price)) %>% 
  # Select useful columns
  select(price, neighbourhood_cleansed, room_type:bedrooms,
         amenities, latitude, longitude)

# Number of missing observations
prices %>% 
  map(., is.na) %>% 
  map_df(., sum)

# Remove missing values
prices <- prices %>% 
  filter(!is.na(bathrooms), !is.na(bedrooms))

# Save cleaned prices dataset
write_csv(prices, path = "data/processed/prices.csv")


# Reviews -----------------------------------------------------------------


# Clean -------------------------------------------------------------------

# Listings
listings_rev <- listings_cleaned %>% 
  # Select useful columns
  select(id, price, number_of_reviews, review_scores_rating)

# Save cleaned prices dataset
write_csv(listings_rev, path = "data/processed/listings.csv")

# Remove unneeded columns
reviews <- reviews %>% 
  select(-id, -reviewer_name)

# Extract tokens for a given id
extract_tokens <- function(id) {
  rev <- reviews %>% 
    filter(listing_id == id)
  unnest_tokens(rev, word, comments)
}

# Select listings
ids <- reviews %>% 
  select(listing_id) %>% 
  unique()

# Extract tokens for each listing
sentiments <- map_df(ids$listing_id, extract_tokens) %>% 
  # Match tokens to sentiments
  inner_join(get_sentiments("bing"))

# Save cleaned prices dataset
write_csv(sentiments, path = "data/processed/sentiments.csv")
