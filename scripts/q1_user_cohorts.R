# scripts/q1_user_cohorts.R
# ------------------------------------------------------------
# Task 1: Group users into Veteran/Intermediate/New based on member_since
# and compute #users, avg review stars, avg reviews per user.
# ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)

# 1. Re-load users and reviews (paths are relative to project root)
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

# 2. Drop any user with missing member_since, then assign cohort
users_cohorted <- users %>%
  filter(!is.na(member_since)) %>%
  mutate(
    cohort = case_when(
      member_since < ymd("2017-01-01") ~ "Veteran",
      member_since < ymd("2023-01-01") ~ "Intermediate",
      TRUE                              ~ "New"
    )
  )

# 3. SHOW how many users remain after dropping NA member_since
cat("After dropping NA member_since, users_cohorted has:", nrow(users_cohorted), "rows\n")

# 4. Attempt to join reviews to users_cohorted
reviews_clean <- reviews %>%
  filter(!is.na(user_id) & !is.na(stars))

reviews_with_cohort <- reviews_clean %>%
  inner_join(select(users_cohorted, user_id, cohort), by = "user_id")

# 5. Check how many rows remain after the join
cat("After joining, reviews_with_cohort has:", nrow(reviews_with_cohort), "rows\n")
cat("Distinct users in cohort data:", n_distinct(reviews_with_cohort$user_id), "\n\n")

# 6. If reviews_with_cohort is empty, we cannot compute any cohort metrics
if (nrow(reviews_with_cohort) == 0) {
  message("==> No data available to form Veteran/Intermediate/New cohorts (all member_since were NA).")
} else {
  # 7. Compute summary by cohort
  cohort_summary <- reviews_with_cohort %>%
    group_by(cohort) %>%
    summarise(
      num_users_in_cohort     = n_distinct(user_id),
      total_reviews_in_cohort = n(),
      avg_review_stars        = mean(stars, na.rm = TRUE),
      avg_reviews_per_user    = total_reviews_in_cohort / num_users_in_cohort
    ) %>%
    ungroup()
  
  # 8. Print summary
  print(cohort_summary)
  
  # 9. Table for report
  cohort_summary %>%
    mutate(
      avg_review_stars     = round(avg_review_stars, 2),
      avg_reviews_per_user = round(avg_reviews_per_user, 2)
    ) %>%
    kable(
      caption = "Table 1: User Cohort Summary (Veteran / Intermediate / New)"
    ) %>%
    kable_styling(full_width = FALSE)
  
  # 10. Bar‐plot of avg_review_stars by cohort
  library(ggplot2)
  plot_cohort_stars <- ggplot(cohort_summary, aes(x = cohort, y = avg_review_stars)) +
    geom_col(fill = "steelblue") +
    labs(
      x     = "User Cohort",
      y     = "Average Review Stars",
      title = "Average Review Stars by User Cohort"
    ) +
    theme_minimal(base_size = 14)
  
  ggsave("figures/q1_avg_stars_by_cohort.png", plot_cohort_stars,
         width = 6, height = 4, dpi = 300)
  print(plot_cohort_stars)
}