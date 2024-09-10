####### Assessing Endogeneity Visually ----------------------------------------
## Inclusivity - 1 is Inclusive 0 is Exclusive
latinos_2020 <- full_cmps_2020 %>% filter(Hispanic == 1)
high_str <- latinos_2020 %>% filter(inclusivity == 0)
low_str <- latinos_2020 %>% filter(inclusivity == 1)
## Linked Fate compared between High and Low Structural Stigma ---------------

ggplot(high_str, aes(Linked_Fate)) + geom_histogram(bins = 9) + 
  labs(title = "Linked Fate in High Structural Stigma Contexts") + 
  xlab("Linked Fate") + ylab("Count") + theme_bw()
ggplot(low_str, aes(Linked_Fate)) + geom_histogram(bins = 9) + 
  labs(title = "Linked Fate in Low Structural Stigma Contexts") + 
  xlab("Linked Fate") + ylab("Count") + theme_bw()

### Acculturation Distribution compared between High and Low Structural Stigma -=====

ggplot(high_str, aes(Accult)) + geom_histogram(bins = 11) + 
  labs(title = "Acculturation in High Structural Stigma Contexts") + 
  xlab("Acculturation") + ylab("Count") + theme_bw()
ggplot(low_str, aes(Accult)) + geom_histogram(bins = 11) + 
  labs(title = "Acculturation in Low Structural Stigma Contexts") + 
  xlab("Acculturation") + ylab("Count") + theme_bw()


