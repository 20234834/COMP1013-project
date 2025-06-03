# scripts/q3_top_users.R
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)

# 1. Load reviews and users
reviews <- read_csv("data/reviews.csv", col_types = cols(
  review_id   = col_character(),
  user_id     = col_character(),
  business_id = col_character(),
  stars       = col_double(),
  date        = col_date(format = ""),
  text        = col_character()
))

users <- read_csv("data/users.csv", col_types = cols(
  user_id       = col_character(),
  name          = col_character(),
  review_count  = col_integer(),
  average_stars = col_double(),
  member_since  = col_date(format = "")
))

# 2. Drop reviews with missing user_id or missing stars
reviews_clean <- reviews %>%
  filter(!is.na(user_id) & !is.na(stars))

cat("Reviews after dropping NA user_id or stars:", nrow(reviews_clean), "\n\n")

# 3. Compute total reviews and avg star per user
user_counts <- reviews_clean %>%
  group_by(user_id) %>%
  summarise(
    total_reviews = n(),
    avg_star      = mean(stars, na.rm = TRUE)
  ) %>%
  ungroup()

# 4. Identify top 10 users by total_reviews
top10_users <- user_counts %>%
  arrange(desc(total_reviews)) %>%
  slice(1:10)

cat("Top 10 users (by user_id) and their total_reviews, avg_star:\n")
print(top10_users)

# 5. Join to users table to get name & member_since
top10_summary <- top10_users %>%
  left_join(select(users, user_id, name, member_since), by = "user_id") %>%
  mutate(avg_star = round(avg_star, 2))

# 6. Print summary table for report
top10_summary %>%
  kable(
    col.names = c("User ID", "Total Reviews", "Avg Star", "Name", "Member Since"),
    caption   = "Table 3: Top 10 Users by Review Count"
  ) %>%
  kable_styling(full_width = FALSE)

# 7. Extract all reviews by these top 10 users
top10_reviews <- reviews_clean %>%
  filter(user_id %in% top10_summary$user_id)

# 8. Create a boxplot of star distribution for these 10 users
#    Ensure users appear in descending order of review count on x‐axis
ordered_ids <- top10_summary$user_id

plot_top10_box <- ggplot(
  top10_reviews,
  aes(x = factor(user_id, levels = ordered_ids), y = stars)
) +
  geom_boxplot(fill = "orchid") +
  labs(
    x     = "User ID",
    y     = "Star Rating",
    title = "Star Rating Distribution for Top 10 Users"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "figures/q3_top10_star_boxplot.png",
  plot     = plot_top10_box,
  width    = 8,
  height   = 4,
  dpi      = 300
)
print(plot_top10_box)