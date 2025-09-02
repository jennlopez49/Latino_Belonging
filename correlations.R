cors <- cmps_lat_16$variables %>% dplyr::select(Linked_Fate, Age, Gender, Education, 
                                         Income,
                                         More_Than_SecondGen, Mexican, 
                                     Discrimination_Scale, 
                                     Discrimination_National_Perc, 
                                     Spanish_Media,
                                     Worry_Deport,
                                     Pol_Interest, ICI_Reverse,
                                     Fear_Election, Angry_Election, Pride_Election,
                                     Hope_Election, Sad_Election, Latino_Disc) %>% 
  na.omit()
cor(cors)
