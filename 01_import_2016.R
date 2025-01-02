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
                                    C377, C379, C381, C383, C384, C150, C151, C393, C394,
                                    S3, C390, C23, A134)

cmps2016 <- cmps2016 %>% mutate(
  Latino = S2_2, 
  State = S4,
  Birthyear = paste0("19", S6),
  Age = (2016 - as.numeric(Birthyear)),
  Native = as.character(S7),
  NativeBorn = case_when(S7 == "(1) United States" ~ 1,
                      S7 == "(3) Puerto Rico" ~ .5,
                      S7 == "(2) Another country" ~ 0),
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
                        S7 == "(3) Puerto Rico " ~ 2)
)
  
  cmps2016 <- cmps2016 %>% mutate( ### Reverse coding so acculturation will be from least (0) to most (1)
    C377 = str_trim(C377),
    C379 = str_trim(C379),
  Parents = case_when(C377 == "(02) Both parents born in another country" ~ 0,
                      C377 == "(03) Both parents born in Puerto Rico" ~ 1,
                           C377 == "(04) 1 parent born in U.S. / 1 parent born elsewhere" ~ 2,
                           C377 == "(01) Both parents born in the U.S." ~ 3,
                           C377 == "(88) Don't know" ~ 9),
  Grandparents = case_when(C379 == "(05) All 4 grandparents born outside U.S." ~ 0,
                                 C379 == "(04) 1 grandparents born in U.S. / 3 elsewhere" ~ 1,
                                 C379 == "(03) 2 grandparents born in U.S. / 2 elsewhere" ~ 2,
                                 C379 == "(02) 3 grandparents born in U.S. / 1 elsewhere" ~ 3,
                                 C379 == "(01) All 4 grandparents born in U.S." ~ 4,
                                 C379 == "(88) Don't know" ~ 9),
  Generation = Parents + Grandparents, 
  More_Than_SecondGen = case_when(NativeBorn == 0 ~ 0,                          # Immigrant respondent, first gen 
                                  NativeBorn == 1 & Parents < 3 ~ 1,            # At least one immigrant parent, second gen
                                  NativeBorn == 1 & Parents == 3 ~ 2,           # Both are born in the US, at least third gen
                                  Parents == 9 ~ NA_real_, #####collapsing so any US-born with US-born Parents is marked as "above 2nd gen) and 1 is 2nd gen
                                  is.na(Parents) & NativeBorn == 1 ~ NA_real_),
  Gender = case_when(S3 == "(1) Female" ~ 1,
                     S3 == "(2) Male" ~ 2), ## excluded "other" only 18 respondents  
  Income = case_when(C383 == "(01) Less than $20,000" ~ 1,
                     C383 == "(02) $20,000 to $29,999" ~ 2,
                     C383 == "(03) $30,000 to $39,999" ~ 3,
                     C383 == "(04) $40,000 to $49,999" ~ 4,
                     C383 == "(05) $50,000 to $59,999" ~ 5,
                     C383 == "(06) $60,000 to $69,999" ~ 6,
                     C383 == "(07) $70,000 to $79,999" ~ 7,
                     C383 == "(08) $80,000 to $89,999" ~ 8,
                     C383 == "(09) $90,000 to $99,999" ~ 9,
                     C383 == "(10) $100,000 to $149,999" ~ 10,
                     C383 == "(11) $150,000 to $199,999" ~ 11,
                     C383 == "(12) $200,000 or more" ~ 12,
                     C383 == "(99) Refused" ~ NA_real_),
  Education = case_when(C381 == "(1) Grades 1 - 8" ~ 1,
                        C381 == "(2) Some High School" ~ 2,
                        C381 == "(3) High School graduate or GED" ~ 3,
                        C381 == "(4) Some college, 2-year degree" ~ 4,
                        C381 == "(5) 4-year college graduate" ~ 5,
                        C381 == "(6) Post-graduate education" ~ 6),
  Discrim = as.character(L268),
  Discrimination = case_when(Discrim == "(1) The primary problem" ~ 5,
                             Discrim == "(2) A major problem" ~ 4,
                             Discrim == "(3) A moderate problem" ~ 3,
                             Discrim == "(4) A minor problem" ~ 2,
                             Discrim == "(5) Not a problem at all" ~ 1,
                             TRUE ~ NA_real_), ### reverse coded -- increasing - bigger problem
  Economy = case_when(C23 == "(1) Getting a lot better" ~ 5,
                      C23 == "(2) Getting a little better" ~ 4,
                      C23 == "(5) Staying about the same" ~ 3,
                      C23 == "(3) Getting a little worse" ~ 2,
                      C23 == "(4) Getting a lot worse" ~ 1), ### changed so worse is negative, about the same is middle
  Mexican = ifelse(cmps2016$S10 == "(12) Mexico", 1, 0),
  Cuban = ifelse(cmps2016$S10 == "(06) Cuba", 1, 0),
  Puerto_Rican = ifelse(cmps2016$S10 == "(17) Puerto Rico", 1, 0),
  Imm_Church = A134,
  Angry_Election = case_when(C112 == "(1) All the time" ~ 4, 
                             C112 == "(2) Often" ~ 3,
                             C112 == "(3) Sometimes" ~ 2,
                             C112 == "(4) Never" ~ 1),
  Fear_Election = case_when(C111 == "(1) All the time" ~ 4, 
                            C111 == "(2) Often" ~ 3,
                            C111 == "(3) Sometimes" ~ 2,
                            C111 == "(4) Never" ~ 1),
  Hope_Election = case_when(C113 == "(1) All the time" ~ 4, 
                            C113 == "(2) Often" ~ 3,
                            C113 == "(3) Sometimes" ~ 2,
                            C113 == "(4) Never" ~ 1),
  Pride_Election = case_when(C114 == "(1) All the time" ~ 4, 
                             C114 == "(2) Often" ~ 3,
                             C114 == "(3) Sometimes" ~ 2,
                             C114 == "(4) Never" ~ 1),
  Sad_Election = case_when(C115 == "(1) All the time" ~ 4, 
                           C115 == "(2) Often" ~ 3,
                           C115 == "(3) Sometimes" ~ 2,
                           C115 == "(4) Never" ~ 1)
  )
  
#### CHECKING NAs IN GENERATION VAR ------
  na_immigrants <- cmps2016 %>%
    filter(is.na(Parents) & NativeBorn == 0) %>%
    count()
  na_immigrants
  

######### Adding ICI Index ---------
inclusivity_2016 <- readxl::read_xlsx("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/inclusivity_2016.xlsx")
full_cmps2016<- left_join(cmps2016, inclusivity_2016, by = "State")

full_cmps2016 <- full_cmps2016 %>% mutate(
  ICI_Score_2016 = as.numeric(full_cmps2016$ICI_Score_2016),
  ICI_collapsed = ifelse(full_cmps2016$ICI_Score_2016 < -86, 0, 
                         ifelse(full_cmps2016$ICI_Score_2016 > -86 & full_cmps2016$ICI_Score_2016 <= -1, .5, 
                                ifelse(full_cmps2016$ICI_Score_2016 > -1, 1, NA))) 
)

full_cmps2016 <- full_cmps2016 %>% mutate(
  ICI_collapsed_alt = case_when(
   ICI_Score_2016 >= -355 & ICI_Score_2016 < -100 ~ -1,
   ICI_Score_2016 >= -100 & ICI_Score_2016 < -60 ~ -.5,
   ICI_Score_2016 >= -60 & ICI_Score_2016 < 0 ~ 0,
   ICI_Score_2016 >= 0 & ICI_Score_2016 < 50 ~ .5,
   ICI_Score_2016 >= 50 & ICI_Score_2016 <= 164 ~ 1,
    TRUE ~ NA_real_ # Handle unexpected values
  )
)

#### Adding in Latino Pop &&& Election Vote Margin Results ------

data_president <- read_csv("dataverse_files/1976-2020-president.csv")
data_votes <- data_president %>% filter(year == 2020 | year == 2016)

data_2016_votes <- data_votes %>% filter(party_detailed == "REPUBLICAN" | 
                                                party_detailed == "DEMOCRAT") %>%
  filter(year == 2016) %>% filter(writein == FALSE)
data_2020_votes <- data_votes %>% filter(party_detailed == "REPUBLICAN" | 
                                                party_detailed == "DEMOCRAT") %>% 
  filter(year == 2020) %>% filter(writein == FALSE)

## switching to wide format 
data_2016_votes <- data_2016_votes %>%
  select(year, state, party_detailed, candidatevotes, totalvotes) %>%  # Keep relevant columns
  pivot_wider(names_from = party_detailed, values_from = candidatevotes)

data_2016_votes <- data_2016_votes %>% mutate(
  vote_margin = ((REPUBLICAN - DEMOCRAT)/totalvotes)*100,
  vote_pp_d = (DEMOCRAT/totalvotes)*100,
  vote_pp_r = (REPUBLICAN/totalvotes)*100,
  vote_diff_pp = vote_pp_d - vote_pp_r 
  
)

data_2020_votes <- data_2020_votes %>%
  select(year, state, party_detailed, candidatevotes, totalvotes) %>%  # Keep relevant columns
  pivot_wider(names_from = party_detailed, values_from = candidatevotes)

data_2020_votes <- data_2020_votes %>% mutate(
  vote_margin = ((REPUBLICAN - DEMOCRAT)/totalvotes)*100,
  vote_pp_d = (DEMOCRAT/totalvotes)*100,
  vote_pp_r = (REPUBLICAN/totalvotes)*100,
  vote_diff_pp = vote_pp_d - vote_pp_r 
)

votemargin_16 <- data_2016_votes %>% mutate(state = str_to_title(state),
                                            State = state.abb[match(state, state.name)]) %>%
  select(State, vote_margin, REPUBLICAN, DEMOCRAT, totalvotes)

# adding in latino pop data 
latino.pop.data_16 <- read.csv("latino_pop.csv") %>% mutate(State = NAME,
                    State = state.abb[match(State, state.name)]) %>% 
  select(State, percent.latino.2016)

### adding in the 2016 pieces to the CMPS data ----------
full_cmps2016 <- left_join(full_cmps2016, latino.pop.data_16, by = "State")
### adding vote margins ------
full_cmps2016 <- left_join(full_cmps2016, votemargin_16, by = "State")

full_cmps2016 <- full_cmps2016 %>% mutate(vote_margin = -vote_margin)

full_cmps2016$Battleground <- ifelse(full_cmps2016$vote_margin > -6 & full_cmps2016$vote_margin < 6, 1, 0)

#### subsetting to just latinos --------
latinos_2016 <- full_cmps2016 %>% filter(Latino == 1)


