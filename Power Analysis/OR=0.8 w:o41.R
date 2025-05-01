library(pwr)
library(ggplot2)

# Define parameters
odds_ratio <- 0.8
alpha <- 0.05
power_values <- seq(0, 1, length.out = 1000)

# Calculate probabilities from odds ratio
prob_healthy <- 0.8 # assumed baseline success rate for healthy group
odds_healthy <- prob_healthy / (1 - prob_healthy)
odds_disorder <- odds_healthy * odds_ratio
prob_disorder <- odds_disorder / (1 + odds_disorder)

# Compute Cohen's h
g <- ES.h(prob_disorder, prob_healthy)

# Loop to calculate required sample size for varying power (balanced groups)
sample_sizes <- sapply(power_values, function(p) {
  if (p <= alpha || p >= 1) return(NA)
  n <- tryCatch({
    pwr.2p.test(h = g,
                sig.level = alpha,
                power = p,
                alternative = "two.sided")$n
  }, error = function(e) NA)
  if (is.na(n)) return(NA)
  ceiling(n)
})

# Prepare data for plotting
power_df <- data.frame(Power = power_values, SampleSize = sample_sizes)

# Plotting
ggplot(power_df, aes(x = Power, y = SampleSize)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(
    title = "Required Sample Size vs. Power (Balanced Groups)",
    x = "Power",
    y = "Sample Size per Group"
  ) +
  theme_minimal()

