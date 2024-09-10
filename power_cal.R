# Example parameters
library(pwr)
effect_size <- 0.1 # medium effect size
alpha <- 0.05
power <- 0.80
num_groups <- 6  # 3 groups

# Power analysis
result <- pwr.anova.test(k = 4, f = .1, sig.level = alpha, power = power)

# Display the result
result


 ### Small effect - 322 in each group, 966 total 
### Medium effect - 52 in each group, 156 total 