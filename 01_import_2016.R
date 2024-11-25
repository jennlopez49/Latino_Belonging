### 2016 Import ##### 
load("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/cmps_2016.rda")
### Selecting vars ########
cmps2016 <- da38040.0001 %>% select(S2_2, S4, S6, S7, S10, C12, C25, C26, L29,
                                    C31,C33, C35_1, C35_2, C35_3, C35_4, C35_5,
                                    C35_OTHER,
                                    C38, C39, C41, L46, C108, C109,
                                    C110, C111, C112, C113, C114, C115, A116, A117,
                                    C118, C119, C120, C121, C129, BLA190,
                                    BLA191, L192, LA193, C194,L195_1, L195_2,
                                    L195_3, L197, L198, C246, C247, LA250, C251,
                                    C252, C256, L268, L270, L271, L300, L301, 
                                    LA303, L364, L365, L366, C374, C375, C375_6_OTHER,
                                    C377, C379, C381, C383, C384, C150, C151, C393, C394,)

cmps2016 <- cmps2016 %>% mutate(
  Latino = S2_2, 
  State = S4,
  Birthyear = paste0("19", S6),
  Age = (2016 - as.numeric(Birthyear)),
  NativeBorn = ifelse(cmps2016$S7 == 1, 1, 0),
  Origin = S10,
  Voted = case_when(S10 == "(1) Yes, I voted" ~ 1,
                    S10 == "(2) No, I did NOT vote"  ~ 0),
  Party = case_when(C25 == "(1) Republican" ~ 1, 
                    C25 == "(2) Democrat" ~ 2,
                    C25 == "(3) Independent" ~ 3,
                    C25 == "(4) Other party" ~ 4),
  #PtyStrength = case_when(C)
  #Party_5pt = 
  Linked_Fate = case_when(C150 == "(2) No" ~ 0,
                          C151 == "(3) Not very much" ~ 1,
                          C151 == "(2) Some" ~ 2,
                          C151 ==  "(1) A lot"  ~ 3), ## C150 1 - Yes, 2 - No -- Recoded to expand 0 - NO, 1 - Not very Much, 2 - Some, 3 - A lot
  Resp_Born = case_when(S7 == "(2) Another country" ~ 0, 
                        S7 == "(1) United States" ~ 1,
                        S7 == "(3) Puerto Rico " ~ 2), ### Reverse coding so acculturation will be from least (0) to most (1)
  Parents_Born = case_when(C377 == "(02) Both parents born in another country" ~ 0,
                           C377 == "(04) 1 parent born in U.S. / 1 parent born elsewhere" ~ 1,
                           C377 == "(01) Both parents born in the U.S." ~ 2,
                           C377 == "(03) Both parents born in Puerto Rico" ~ 3,
                           C377 == "(88) Don't know" ~ NA),
  Grandparents_Born = case_when (C379 == "(05) All 4 grandparents born outside U.S." ~ 0,
                                 C379 == "(04) 1 grandparents born in U.S. / 3 elsewhere" ~ 1,
                                 C379 == "(03) 2 grandparents born in U.S. / 2 elsewhere" ~ 2,
                                 C379 == "(02) 3 grandparents born in U.S. / 1 elsewhere " ~ 3,
                                 C379 == "(01) All 4 grandparents born in U.S." ~ 4,
                                 C379 == "(88) Don't know" ~ NA)
  )

inclusivity_2016 <- readxl::read_xlsx("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/inclusivity_2016.xlsx")
full_cmps2016<- left_join(cmps2016, inclusivity_2016, by = "State")

full_cmps2016 <- full_cmps2016 %>% mutate(
  ICI_Score_2016 = as.numeric(full_cmps2016$ICI_Score_2016),
  ICI_collapsed = ifelse(full_cmps2016$ICI_Score_2016 < -86, 0, 
                         ifelse(full_cmps2016$ICI_Score_2016 > -86 & full_cmps2016$ICI_Score_2016 <= -1, .5, 
                                ifelse(full_cmps2016$ICI_Score_2016 > -1, 1, NA))) 
)


