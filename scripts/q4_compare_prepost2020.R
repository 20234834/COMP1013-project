# scripts/q4_compare_prepost2020.R
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggplot2)

# 1. Load users and reviews
users <- read_csv("data/users.csv", col_types = cols(
  user_id       = col_character(),
  name          = col_character(),
  review_count  = col_integer(),
  average_stars = col_double(),
  member_since  = col_date(format = "")
))

reviews <- read_csv("data/reviews.csv", col_types = cols(
  review_id   = col_character(),
  user_id     = col_character(),
  business_id = col_character(),
  stars       = col_double(),
  date        = col_date(format = ""),
  text        = col_character()
))

# 2. Drop users with missing member_since, then create Pre/Post group
users_prepost <- users %>%
  filter(!is.na(member_since)) %>%
  mutate(
    join_group = if_else(
      member_since < ymd("2020-01-01"),
      "Pre2020",
      "Post2020"
    )
  )

cat("Users after dropping NA member_since:", nrow(users_prepost), "\n")

# 3. Drop reviews with missing user_id OR missing stars OR missing text
reviews_clean <- reviews %>%
  filter(!is.na(user_id) & !is.na(stars) & !is.na(text))

cat("Reviews after dropping NA user_id, stars, or text:", nrow(reviews_clean), "\n\n")

# 4. Join reviews → users_prepost to bring join_group onto each review
reviews_with_group <- reviews_clean %>%
  inner_join(select(users_prepost, user_id, join_group), by = "user_id")

cat("Joined reviews_with_group rows:", nrow(reviews_with_group), "\n")
cat("Distinct join_group values:", unique(reviews_with_group$join_group), "\n\n")

# 5. Star rating summary by join_group
star_summary <- reviews_with_group %>%
  group_by(join_group) %>%
  summarise(
    avg_star      = mean(stars, na.rm = TRUE),
    sd_star       = sd(stars, na.rm = TRUE),
    total_reviews = n()
  ) %>%
  ungroup()

print(star_summary)

# 6. Boxplot: star distribution by Pre/Post 2020
plot_star_dist <- ggplot(
  reviews_with_group,
  aes(x = join_group, y = stars)
) +
  geom_boxplot(fill = "lightblue") +
  labs(
    x     = "User Join Group",
    y     = "Star Rating",
    title = "Star Rating Distribution: Pre-2020 vs. Post-2020"
  ) +
  theme_minimal(base_size = 12)

ggsave("figures/q4_star_distribution.png", plot_star_dist,
       width = 6, height = 4, dpi = 300)
print(plot_star_dist)

# 7. Compute review length (number of characters) then summarise by group
reviews_with_group <- reviews_with_group %>%
  mutate(review_length = str_length(text))

length_summary <- reviews_with_group %>%
  group_by(join_group) %>%
  summarise(
    avg_review_length = mean(review_length, na.rm = TRUE),
    sd_review_length  = sd(review_length, na.rm = TRUE),
    total_reviews     = n()
  ) %>%
  ungroup()

print(length_summary)

# 8. Bar chart: average review length by group
plot_review_length <- ggplot(
  length_summary,
  aes(x = join_group, y = avg_review_length)
) +
  geom_col(fill = "coral") +
  labs(
    x     = "User Join Group",
    y     = "Average Review Length (chars)",
    title = "Average Review Length: Pre-2020 vs. Post-2020"
  ) +
  theme_minimal(base_size = 14)

ggsave("figures/q4_avg_review_length.png", plot_review_length,
       width = 6, height = 4, dpi = 300)
print(plot_review_length)

# 9. Combined table for report
comparison_summary <- star_summary %>%
  select(join_group, avg_star) %>%
  left_join(
    select(length_summary, join_group, avg_review_length),
    by = "join_group"
  ) %>%
  mutate(
    avg_star          = round(avg_star, 2),
    avg_review_length = round(avg_review_length, 1)
  )

comparison_summary %>%
  kable(
    caption = "Table 4: Comparison—Pre-2020 vs. Post-2020 (Avg Star & Avg Review Length)"
  ) %>%
  kable_styling(full_width = FALSE)