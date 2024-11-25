# Load the pwr package
install.packages("pwr")  # Run this if you don't have the pwr package installed
library(pwr)

# Define your parameters
effect_size_f <- 0.10  # Small effect size (Cohen's f)
num_groups <- 2       # Number of groups
alpha <- 0.05          # Significance level (5%)
desired_power <- 0.80  # Desired power (80%)

# Perform power analysis to calculate sample size
pwr_anova_sample_size <- pwr.anova.test(f = effect_size_f, k = num_groups, power = desired_power, sig.level = alpha)

# Display the result
pwr_anova_sample_size
