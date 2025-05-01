library(pwr)
library(ggplot2)

# Define parameters
odds_ratio <- 0.8
alpha <- 0.05
power_values <- seq(0, 1, length.out = 1000)
n_healthy <- 41

# Calculate Cohen's h from odds ratio
prob_healthy <- 0.8 # assumed baseline success rate for healthy group
odds_healthy <- prob_healthy / (1 - prob_healthy)
odds_disorder <- odds_healthy * odds_ratio
prob_disorder <- odds_disorder / (1 + odds_disorder)

# Compute Cohen's h
cohen_h <- ES.h(prob_disorder, prob_healthy)
?pwr.2p2n.test
?pwr.t.test
?pwr.chisq.test
# Loop to calculate required sample size for varying power (with error handling)
sample_sizes <- numeric(length(power_values))
for(i in 1:length(power_values)) {
  p <- power_values[i]
  if(p <= alpha || p >= 1) {
    sample_sizes[i] <- NA
    next
  }
  
  result <- tryCatch({
    test <- pwr.2p2n.test(h = cohen_h,
                          n1 = n_healthy,
                          sig.level = alpha,
                          power = p,
                          alternative = "two.sided")
    ceiling(test$n2)
  }, error = function(e) {
    NA
  })
  
  sample_sizes[i] <- result
}

# Create data frame without NA values for plotting
power_df <- data.frame(Power = power_values, SampleSize = sample_sizes)
power_df_clean <- power_df[!is.na(power_df$SampleSize),]

# Plotting with the cleaned data
ggplot(power_df_clean, aes(x = Power, y = SampleSize)) +
  geom_line(color = "orange", linewidth = 1.2) +
  labs(title = "Sample Size Required (Disordered Group) vs. Power",
       subtitle = paste("Healthy group size fixed at", n_healthy, 
                        "| Odds ratio =", odds_ratio),
       x = "Power",
       y = "Required Sample Size (Disordered Group)") +
  theme_minimal()

