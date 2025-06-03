# scripts/load_data.R
# ------------------------------------------------------------
# Purpose:   Load the three CSV files (users, businesses, reviews),
#            inspect their structure, and do basic data type conversion.
# Author :  [Your Name]
# Date   :  [Date you’re working on it]
# ------------------------------------------------------------

# 1. Load packages
library(tidyverse)   # includes readr, dplyr, ggplot2, tidyr, etc.
library(lubridate)   # for parsing dates
library(knitr)       # for nicely printing tables (kable)
library(kableExtra)  # for styling kable tables

# 2. Define file paths (relative to project root)
users_filepath      <- "data/users.csv"
businesses_filepath <- "data/businesses.csv"
reviews_filepath    <- "data/reviews.csv"

# 3. Read in the CSVs

# 3a. USERS
users <- read_csv(
  users_filepath,
  col_types = cols(
    user_id       = col_character(),
    name          = col_character(),
    review_count  = col_integer(),
    average_stars = col_double(),
    member_since  = col_date(format = "")
  )
)

# 3b. BUSINESSES
# Note: the CSV header for average stars is “business avg stars” (with spaces).
# We read that exact name, then immediately rename it to business_avg_stars.
businesses <- read_csv(
  businesses_filepath,
  col_types = cols(
    business_id         = col_character(),
    name                = col_character(),
    city                = col_character(),
    state               = col_character(),
    `business avg stars` = col_double(),
    review_count        = col_integer(),
    categories          = col_character(),
    business_group      = col_character()
  )
) %>%
  rename(business_avg_stars = `business avg stars`)

# 3c. REVIEWS
reviews <- read_csv(
  reviews_filepath,
  col_types = cols(
    review_id   = col_character(),
    user_id     = col_character(),
    business_id = col_character(),
    stars       = col_double(),
    date        = col_date(format = ""),
    text        = col_character()
  )
)

# 4. Quick inspections

# 4a. Print number of rows and columns in each dataset
cat("Users:     ", nrow(users), "rows ×", ncol(users), "cols\n")
cat("Businesses:", nrow(businesses), "rows ×", ncol(businesses), "cols\n")
cat("Reviews:   ", nrow(reviews), "rows ×", ncol(reviews), "cols\n\n")

# 4b. Show the first few rows of each
message("Preview of users:")
print(head(users))

message("\nPreview of businesses:")
print(head(businesses))

message("\nPreview of reviews:")
print(head(reviews))

# 5. Check for missing values in key columns

na_summary_users <- users %>%
  summarise(
    missing_user_id       = sum(is.na(user_id)),
    missing_member_since  = sum(is.na(member_since)),
    missing_review_count  = sum(is.na(review_count)),
    missing_avg_stars     = sum(is.na(average_stars))
  )

na_summary_businesses <- businesses %>%
  summarise(
    missing_business_id   = sum(is.na(business_id)),
    missing_state         = sum(is.na(state)),
    missing_avg_stars     = sum(is.na(business_avg_stars)),
    missing_review_count  = sum(is.na(review_count))
  )

na_summary_reviews <- reviews %>%
  summarise(
    missing_review_id    = sum(is.na(review_id)),
    missing_user_id      = sum(is.na(user_id)),
    missing_business_id  = sum(is.na(business_id)),
    missing_stars        = sum(is.na(stars)),
    missing_date         = sum(is.na(date)),
    missing_text         = sum(is.na(text))
  )

cat("\nNA summary for users:\n")
print(na_summary_users)

cat("\nNA summary for businesses:\n")
print(na_summary_businesses)

cat("\nNA summary for reviews:\n")
print(na_summary_reviews)

# 6. Show structure of each dataframe (types and examples)
cat("\nStructure of 'users':\n")
glimpse(users)

cat("\nStructure of 'businesses':\n")
glimpse(businesses)

cat("\nStructure of 'reviews':\n")
glimpse(reviews)