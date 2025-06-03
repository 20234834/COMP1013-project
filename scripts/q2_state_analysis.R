# scripts/q2_state_analysis.R
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)

# 1. Load businesses & reviews
businesses <- read_csv("data/businesses.csv", col_types = cols(
  business_id         = col_character(),
  name                = col_character(),
  city                = col_character(),
  state               = col_character(),
  `business avg stars` = col_double(),
  review_count        = col_integer(),
  categories          = col_character(),
  business_group      = col_character()
)) %>% rename(business_avg_stars = `business avg stars`)

reviews <- read_csv("data/reviews.csv", col_types = cols(
  review_id   = col_character(),
  user_id     = col_character(),
  business_id = col_character(),
  stars       = col_double(),
  date        = col_date(format = ""),
  text        = col_character()
))

# 2. Drop businesses where state is NA
businesses_clean <- businesses %>%
  filter(!is.na(state))

# 3. Drop reviews where business_id or stars is NA
reviews_clean <- reviews %>%
  filter(!is.na(business_id) & !is.na(stars))

# 4. Join reviews → businesses to get state on each review
reviews_with_state <- reviews_clean %>%
  inner_join(select(businesses_clean, business_id, state), by = "business_id")

# 5. How many rows remain after filtering/join?
cat("After dropping NA and joining, reviews_with_state rows:", nrow(reviews_with_state), "\n")
cat("Distinct states represented:", n_distinct(reviews_with_state$state), "\n\n")

# 6. Summarise by state
state_summary <- reviews_with_state %>%
  group_by(state) %>%
  summarise(
    num_reviews = n(),
    num_users   = n_distinct(user_id),
    avg_star    = mean(stars, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(avg_star))

# 7. Print the summary table (console)
print(state_summary)

# 8. kable table (for report)
state_summary %>%
  mutate(avg_star = round(avg_star, 2)) %>%
  kable(
    caption = "Table 2: #Reviews, #Users, and Average Star by State"
  ) %>%
  kable_styling(full_width = FALSE)

# 9. Bar chart of avg_star by state (descending)
plot_state_avg <- ggplot(state_summary, aes(x = reorder(state, avg_star), y = avg_star)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    x     = "State",
    y     = "Average Review Star",
    title = "Average Review Star by State"
  ) +
  theme_minimal(base_size = 12)

ggsave("figures/q2_avg_star_by_state.png", plot_state_avg,
       width = 6, height = 8, dpi = 300)
print(plot_state_avg)