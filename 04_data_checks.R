##### Variables of Interest -------

vars_interest <- latinos_2016 %>% select(Age, Gender, Education, Income, 
                                         Generation, Parents, Grandparents,
                                         Mexican, Puerto_Rican,
                                         Cuban, Economy, Discrimination, ICI_Score_2016,
                                         ICI_collapsed, percent.latino.2016,
                                         vote_margin, NativeBorn, More_Than_SecondGen,
                                         Battleground)
na_counts <- map_int(vars_interest, ~ sum(is.na(.)))


# Double checking generation ----
na_immigrants <- latinos_2016 %>%
  filter(is.na(Parents) & NativeBorn == 0) %>%
  count()
na_immigrants

na_count_born_in_us <- latinos_2016 %>%
  filter(is.na(Parents) & NativeBorn == 1) %>%
  count()

na_count_born_in_us