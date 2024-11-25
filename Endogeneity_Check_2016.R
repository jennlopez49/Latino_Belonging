## 2016 Linked Fate Graphs
latinos_2016 <- full_cmps2016 %>% filter(Latino == 1)
high_str_16 <- latinos_2016 %>% filter(ICI_collapsed == 0 | ICI_collapsed == 0.5)
low_str_16 <- latinos_2016 %>% filter(ICI_collapsed == 1)
## Linked Fate compared between High and Low Structural Stigma ---------------

lf_hi_16 <- ggplot(high_str_16, aes(Linked_Fate)) + geom_histogram(binwidth = .5) +
  labs(title = "High Structural Stigma") + 
  xlab("Linked Fate") + ylab("Count") + theme_bw()  +  # Set identical x limits
  ylim(0, 800)
lf_lo_16 <- ggplot(low_str_16, aes(Linked_Fate)) + geom_histogram(binwidth = .5) + 
  labs(title = "Low Structural Stigma") + 
  xlab("Linked Fate") + ylab("Count") + theme_bw()  +  # Set identical x limits
  ylim(0, 800)

both_lf_16 <- lf_hi_16 + lf_lo_16

ggsave("both_lf_16.pdf", width = 7, height = 4)

## T-Test Comparisons ---- No Evidence to Suggest that They are Different 
shapiro.test(latinos_2016$Linked_Fate[latinos_2016$ICI_collapsed == 0])
shapiro.test(latinos_2016$Linked_Fate[latinos_2016$ICI_collapsed == 0.5])
shapiro.test(latinos_2016$Linked_Fate[latinos_2016$ICI_collapsed == 1])

chisq_data <- table(latinos_2016$Linked_Fate, latinos_2016$ICI_collapsed)
chisq.test(chisq_data)