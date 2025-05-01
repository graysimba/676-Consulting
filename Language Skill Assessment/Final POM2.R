library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
set.seed(999)

# parameters
n_children <- 200
n_questions <- 3
n_age_groups <- 3  # age Group 1, 2, 3

# assume each question has 3 possible ordinal outcomes: 1 < 2 < 3

# For demonstration, define "difficulties" for each question & age group
# We'll make older age groups slightly "harder" or "easier" arbitrarily
difficulties <- matrix(
  c(0.0,  0.5,  1.0,   # Q1 difficulty across age groups
    -0.5, 0.0,  0.8,   # Q2
    0.2, 0.7,  1.2),  # Q3
  nrow = n_questions, ncol = n_age_groups, byrow = TRUE
)

# simulate Ordinal responses
# We'll create an array Y of dimensions (n_children, n_age_groups, n_questions)
# Y[i, k, q] = child's response for question q at age group k

Y <- array(0, dim = c(n_children, n_age_groups, n_questions))

for (i in 1:n_children) {
  for (k in 1:n_age_groups) {
    for (q in 1:n_questions) {
      # simple logic: "Latent score" = rnorm() - difficulty
      latent_score <- rnorm(1, mean = 1.0) - difficulties[q, k]
      
      # convert latent score to an ordinal outcome in {1, 2, 3}
      # define thresholds at -0.5 and +0.5
      if (latent_score < -0.5) {
        Y[i, k, q] <- 1
      } else if (latent_score < 0.5) {
        Y[i, k, q] <- 2
      } else {
        Y[i, k, q] <- 3
      }
    }
  }
}


# For each question q in 1 to 3, that gives us 3 models => 9 total.

age_pairs <- list(
  c(2, 1),
  c(3, 2)
)

# We'll reshape data into a long format, 
# so each row = 1 child, 1 question, 1 pair of age groups
df_list <- list()

for (q in 1:n_questions) {
  for (pair in age_pairs) {
    future_k <- pair[1]
    current_k <- pair[2]
    
    # extract ordinal responses for these two age groups
    current_responses <- Y[, current_k, q]
    future_responses  <- Y[, future_k, q]
    
    # build a data frame that has one row per child per question per age pair, 
    # with the responses stored as ordered factors.
    tmp_df <- data.frame(
      child_id   = 1:n_children,
      question   = paste0("Q", q),
      current_age_group = current_k,
      future_age_group  = future_k,
      current_response  = factor(current_responses, ordered = TRUE),
      future_response   = factor(future_responses,  ordered = TRUE)
    )
    
    df_list[[length(df_list) + 1]] <- tmp_df
  }
}

model_data <- bind_rows(df_list)

# fit independent Proportional Odds Models
# We'll do one model per question+age-pair combination:
# current_response ~ future_response

model_results <- model_data %>%
  group_by(question, current_age_group, future_age_group) %>%
  do({
    fit <- polr(current_response ~ future_response, data = ., 
                method = "logistic")
    
    data.frame(
      question          = unique(.$question),
      current_age_group = unique(.$current_age_group),
      future_age_group  = unique(.$future_age_group),
      AIC               = AIC(fit)
    )
  })

model_results
summary(model_results)

# Q1, the current age group is 1, the future age group is 2.
example_df <- model_data %>%
  filter(question == "Q1", current_age_group == 1, future_age_group == 2)
example_model <- polr(current_response ~ future_response, data = example_df, 
                      method = "logistic")
summary(example_model)


# This plot shows how current responses are distributed across questions and age groups.
ggplot(model_data, aes(x = current_response)) +
  geom_bar(fill = "skyblue", color = "black") +
  facet_grid(question ~ current_age_group) +
  labs(title = "Distribution of Current Responses by Question and Age Group",
       x = "Current Response (Ordinal)",
       y = "Count")

# Using Age Group 2 to predict Age Group 1
example_df <- model_data %>%
  filter(question == "Q1", current_age_group == 1, future_age_group == 2) %>%
  mutate(current_num = as.numeric(as.character(current_response)))

ggplot(example_df, aes(x = future_response, y = current_num)) +
  geom_jitter(width = 0.2, height = 0.2, color = "orange") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(title = "Relationship Between Future and Current Responses",
       x = "Future Response (Age Group 2) Ability Score",
       y = "Current Response (Age Group 1) Ability Score")
