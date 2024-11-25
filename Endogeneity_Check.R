####### Assessing Endogeneity Visually ----------------------------------------
## Inclusivity - 1 is Inclusive 0 is Exclusive
latinos_2020 <- full_cmps_2020 %>% filter(Hispanic == 1)
high_str <- latinos_2020 %>% filter(inclusivity == 0)
low_str <- latinos_2020 %>% filter(inclusivity == 1)
## Linked Fate compared between High and Low Structural Stigma ---------------

lf_hi <- ggplot(high_str, aes(Linked_Fate)) + geom_histogram(binwidth = .5) +
  labs(title = "High Structural Stigma") + 
  xlab("Linked Fate") + ylab("Count") + theme_bw() + xlim(0, 6) +  # Set identical x limits
  ylim(0, 850)
lf_lo <- ggplot(low_str, aes(Linked_Fate)) + geom_histogram(binwidth = .5) + 
  labs(title = "Low Structural Stigma") + 
  xlab("Linked Fate") + ylab("Count") + theme_bw() + xlim(0, 6) +  # Set identical x limits
  ylim(0, 850)

both_lf <- lf_hi + lf_lo

ggsave("both_lf.pdf", width = 7, height = 4)

### Acculturation Distribution compared between High and Low Structural Stigma -=====

ggplot(high_str, aes(Accult)) + geom_histogram(bins = 11) + 
  labs(title = "Acculturation in High Structural Stigma Contexts") + 
  xlab("Acculturation") + ylab("Count") + theme_bw()
ggplot(low_str, aes(Accult)) + geom_histogram(bins = 11) + 
  labs(title = "Acculturation in Low Structural Stigma Contexts") + 
  xlab("Acculturation") + ylab("Count") + theme_bw()


## T-Test Comparisons ---- No Evidence to Suggest that They are Different 

result <- t.test(Linked_Fate ~ inclusivity, data = full_cmps_2020)


