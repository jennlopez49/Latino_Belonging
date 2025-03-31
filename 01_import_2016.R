### 2016 Import ##### 
load("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/cmps_2016.rda")
### Selecting vars ########
cmps.sub.2016 <- da38040.0001 %>% dplyr::select(S2_1, S2_2, S2_3, S2_4,S2_5, S2_6, S2_7, S4,
                                    S6, S7, S10, C12, C25, C26, L29,
                                    C31,C33, C35_1, C35_2, C35_3, C35_4, C35_5,
                                    C35_OTHER,
                                    C38, C39, C41, L46,C107, C108, C109,
                                    C110, C111, C112, C113, C114, C115, A116, A117,
                                    C118, C119, C120, C121, C129, BLA190,
                                    BLA191, L192, LA193, C194,L195_1, L195_2,
                                    L195_3, L197, L198, C246, C247, LA250, C251,
                                    C252, C256, L268, L270, L271, L300, L301, 
                                    LA303, L364, L365, L366, C374, C375, C375_6_OTHER,
                                    C377, C379, C381, C383, C384, C150, C151, C393, C394,
                                    S3, C390, C23, A134, NAT_WEIGHT,ETHNIC_QUOTA,
                                    C253)

cmps.clean.2016 <- cmps.sub.2016 %>% mutate(
  Latino = S2_2, 
  State = S4,
  Birthyear = paste0("19", S6),
  Age = (2016 - as.numeric(Birthyear)),
  age_sqd = Age^2,
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
  
  cmps.add.2016 <- cmps.clean.2016 %>% mutate( ### Reverse coding so acculturation will be from least (0) to most (1)
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
  Discrimination_National_Perc = case_when(Discrim == "(1) The primary problem" ~ 5,
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
  Mexican = ifelse(cmps.clean.2016$S10 == "(12) Mexico", 1, 0),
  Cuban = ifelse(cmps.clean.2016$S10 == "(06) Cuba", 1, 0),
  Puerto_Rican = ifelse(cmps.clean.2016$S10 == "(17) Puerto Rico", 1, 0),
  Dominican = ifelse(cmps.clean.2016$S10 == "(07) Dominican Republic", 1, 0),
  Central_American = ifelse(cmps.clean.2016$S10 == "(09) El Salvador", 1, 
                            ifelse(cmps.clean.2016$S10 == "(10) Guatemala", 1, 
                                   ifelse(cmps.clean.2016$S10 == "(11) Honduras", 1, 
                                          ifelse(cmps.clean.2016$S10 == "(05) Costa Rica", 1,  
                                                 ifelse(cmps.clean.2016$S10 == "(13) Nicaragua", 1, 
                                                        ifelse(cmps.clean.2016$S10 == "(14) Panama", 1, 0)))))),
  South_American = ifelse(cmps.clean.2016$S10 == "(01) Argentina", 1, 
                          ifelse(cmps.clean.2016$S10 == "(02) Bolivia", 1, 
                                 ifelse(cmps.clean.2016$S10 == "(03) Chile", 1, 
                                        ifelse(cmps.clean.2016$S10 == "(04) Colombia", 1,
                                               ifelse(cmps.clean.2016$S10 == "(08) Ecuador", 1, 
                                                      ifelse(cmps.clean.2016$S10 == "(15) Paraguay", 1, 
                                                             ifelse(cmps.clean.2016$S10 == "(16) Peru", 1, 
                                                                    ifelse(cmps.clean.2016$S10 == "(18) Uruguay", 1,
                                                                           ifelse(cmps.clean.2016$S10 == "(19) Venezuela", 1, 0))))))))),
  Spanish = ifelse(cmps.clean.2016$S10 == "(20) Spain / Spanish", 1, 0),
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
                           C115 == "(4) Never" ~ 1),
  WhiteNonHisp = S2_1,
  Black = S2_3,
  Asian = S2_4, 
  MiddleEast = S2_5,
  NativeAm = S2_6,
  OtherRace = S2_7,
  #Race_Combined = case_when(WhiteNonHisp == 1 ~ 1, 
                            #)
  Weight = NAT_WEIGHT,
  Race_Prime = ETHNIC_QUOTA,
  #Race_num = case_when(cmps.clean.2016$Race_Prime == "(1) White Non Hispanic" ~ 1,
                      # cmps.clean.2016$Race_Prime == "(2) Hispanic or Latino" ~ 2,
                      # cmps.clean.2016$Race_Prime == "(3) Black or African American" ~ 3,
                      # cmps.clean.2016$Race_Prime == "(4) Asian American" ~ 4),
  Belong_US = case_when(C107 == "(1) Strongly belong" ~ 4,                      # recoded - 1 is not at all belong, 4 is strongly belong now
                        C107 == "(2) Moderately belong" ~ 3,
                        C107 == "(3) Slightly belong" ~ 2,
                        C107 == "(4) Not at all belong" ~ 1
                        ),
  Valued_Respected_US = case_when(C108 == "(4) Strongly disagree" ~ 1,        # Statement is "Most Americans value and respect your presence in the US." Agree or disagree
                        C108 == "(3) Somewhat disagree" ~ 2,                  # Coded so higher numbers --> more perceived external cues for belonging
                        C108 == "(2) Somewhat agree" ~ 3,
                        C108 == "(1) Strongly agree" ~ 4
                        ),
  Outsider_US = case_when(C109 == "(1) Strongly as an outsider" ~ 1,        # Question is - "How much do you feel you are an outsider in the US?"
                                  C109 == "(2) Moderately as an outsider" ~ 2,                  # Coded so higher numbers --> more belonging
                                  C109 == "(3) Slightly as an outsider" ~ 3,
                                  C109 == "(4) Not at all as an outsider" ~ 4
                          ),
  Excluded_US_Soc = case_when(C109 == "(1) Always" ~ 1,        # Question is - "How often do ppl try to exclude you in the US?"
                              C109 == "(2) Very often" ~ 2,                  # Coded so higher numbers --> less exclusion
                              C109 == "(3) Rarely" ~ 3,
                              C109 == "(4) Never" ~ 4
                              ),
  Personal_Discrimination = case_when(C251 == "(2) No" ~ 0,                      # Have you ever been treated unfairly or personally experienced discrimination 
                                      C251 == "(1) Yes" ~ 1                      #because of your race, ethnicity, gender, sexuality,
                                      ),                                           # being an immigrant, religious heritage or having an accent?
  Place_of_Disc = case_when(C252 == "(2) In my country of origin" ~ 0,
                            C252 == "(1) In the United States" ~ 1,
                            C252 == "(3) In both places" ~ 1),
  Race_Ethnicity_Disc = case_when(C253 == "(2) No" ~ 0,
                                  C253 == "(1) Yes" ~ 1),
  Immigration_Status_Disc = case_when(C256 == "(2) No" ~ 0,
                                      C256 == "(1) Yes" ~ 1),
  Discrimination_Scale = case_when(
    Personal_Discrimination == 0 | Place_of_Disc == 0 ~ 0, # Did not experience discrimination, not in the US
    Race_Ethnicity_Disc == 1 & Place_of_Disc == 1 ~ 1, # Experienced discrimination in US due to race/ethnicity 
    Immigration_Status_Disc == 1 & Place_of_Disc == 1 ~ 1 # or immigration status
  ),
  Pol_Interest = case_when(C33 == "(1) Very interested in politics" ~ 4,
                           C33 == "(2) Somewhat interested" ~ 3,
                           C33 == "(3) Not that interested in politics" ~ 2,
                           C33 == "(4) Not at all interested in politics" ~ 1), ### reverse-coded 1 - not at all, 4 - very interested
  National_Origin = case_when(
    S10 == "(12) Mexico" ~ "Mexican",
    S10 == "(06) Cuba" ~ "Cuban",
    S10 == "(17) Puerto Rico" ~ "Puerto Rican",
    S10 == "(07) Dominican Republic" ~ "Dominican",
    S10 %in% c("(09) El Salvador", "(10) Guatemala", "(11) Honduras", "(05) Costa Rica", 
               "(13) Nicaragua", "(14) Panama") ~ "Central American",
    S10 %in% c("(01) Argentina", "(02) Bolivia", "(03) Chile", "(04) Colombia", 
               "(08) Ecuador", "(15) Paraguay", "(16) Peru", "(18) Uruguay", "(19) Venezuela") ~ "South American",
    S10 == "(20) Spain / Spanish" ~ "Spanish",
    TRUE ~ "Other"  # Default value for any other cases
  )
  )
  
######## Belonging Index ------------------------------------------------------
  # Standardize the variables (mean=0, sd=1) to combine them
  cmps.add.2016$Belong_US_z <- scale(cmps.add.2016$Belong_US)
  cmps.add.2016$Valued_Respected_US_z <- scale(cmps.add.2016$Valued_Respected_US)
  cmps.add.2016$Outsider_US_z <- scale(cmps.add.2016$Outsider_US)
  cmps.add.2016$Excluded_US_Soc_z <- scale(cmps.add.2016$Excluded_US_Soc)
  
  # Construct the inclusion index by averaging the standardized variables
  cmps.add.2016$Inclusion_Index <- rowMeans(cmps.add.2016[, c("Belong_US_z", "Valued_Respected_US_z", "Outsider_US_z", "Excluded_US_Soc_z")], na.rm = TRUE)
  cmps.add.2016$Inclusion_Internal <- rowMeans(cmps.add.2016[, c("Belong_US_z", "Outsider_US_z")], na.rm = TRUE)
  cmps.add.2016$Inclusion_External <- rowMeans(cmps.add.2016[, c( "Valued_Respected_US_z", "Excluded_US_Soc_z")], na.rm = TRUE)
  
  # Check the index
  summary(cmps.add.2016$Inclusion_Index)
  
#### CHECKING NAs IN GENERATION VAR ------
  # na_immigrants <- cmps.add.2016 %>%
  #   filter(is.na(Parents) & NativeBorn == 0) %>%
  #   count()
  # na_immigrants
  # 

######### Adding ICI Index ---------
inclusivity_2016 <- readxl::read_xlsx("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/inclusivity_2016.xlsx")
# full_cmps2016<- left_join(cmps.add.2016, inclusivity_2016, by = "State")
# 
# full_cmps2016 <- full_cmps2016 %>% mutate(
#   ICI_Score_2016 = as.numeric(full_cmps2016$ICI_Score_2016),
#   ICI_collapsed = ifelse(full_cmps2016$ICI_Score_2016 < -86, 0, 
#                          ifelse(full_cmps2016$ICI_Score_2016 > -86 & full_cmps2016$ICI_Score_2016 <= -1, .5, 
#                                 ifelse(full_cmps2016$ICI_Score_2016 > -1, 1, NA))) 
# )
# 
# full_cmps2016 <- full_cmps2016 %>% mutate(
#   ICI_collapsed_alt = case_when(
#    ICI_Score_2016 >= -355 & ICI_Score_2016 < -100 ~ 1,
#    ICI_Score_2016 >= -100 & ICI_Score_2016 < -60 ~ -.5,
#    ICI_Score_2016 >= -60 & ICI_Score_2016 < 0 ~ 0,
#    ICI_Score_2016 >= 0 & ICI_Score_2016 < 50 ~ -.5,
#    ICI_Score_2016 >= 50 & ICI_Score_2016 <= 164 ~ -1,
#     TRUE ~ NA_real_ # Handle unexpected values
#   )
# )
  
  ## Importing original Indicator 
  
full_indicators <- read.csv("/Users/jenniferlopez/Desktop/COIi work/State_Laws/full_indicators_2016.csv")
  
full_cmps2016 <- left_join(cmps.add.2016, full_indicators, by = "State")

full_cmps2016 <- full_cmps2016 %>% mutate(
    Imm_Con_Ind = (Imm_Class_Concrete_2016 - min(Imm_Class_Concrete_2016)) / (max(Imm_Class_Concrete_2016) - min(Imm_Class_Concrete_2016)),
    Imm_Full_Ind = (Imm_Class_Full_2016 - min(Imm_Class_Full_2016)) / (max(Imm_Class_Full_2016) - min(Imm_Class_Full_2016)),
    Imm_Con_Index = case_when(
      Imm_Class_Concrete_2016 <= -5  ~ -1,   # High Anti
      Imm_Class_Concrete_2016 < 0    ~ -0.5, # Low Anti
      Imm_Class_Concrete_2016 < 10   ~ 0.5,  # Low Pro
      Imm_Class_Concrete_2016 >= 10   ~ 1,    # High Pro
    ),
    Imm_Sym_Index = case_when(
      Imm_Class_Full_2016 <= -10 ~ -1,     # High Anti: score <= -10
      Imm_Class_Full_2016 > -10 & Imm_Class_Full_2016 <= 0 ~ -0.5,  # Low Anti: score between -10 and 0
      Imm_Class_Full_2016 > 0 & Imm_Class_Full_2016 < 20 ~ 0.5,   # Low Pro: score between 0 and 20
      Imm_Class_Full_2016 >= 20 ~ 1,        # High Pro: score > 20
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
  dplyr::select(year, state, party_detailed, candidatevotes, totalvotes) %>%  # Keep relevant columns
  pivot_wider(names_from = party_detailed, values_from = candidatevotes)

data_2016_votes <- data_2016_votes %>% mutate(
  vote_margin = ((REPUBLICAN - DEMOCRAT)/totalvotes)*100,
  vote_pp_d = (DEMOCRAT/totalvotes)*100,
  vote_pp_r = (REPUBLICAN/totalvotes)*100,
  vote_diff_pp = vote_pp_d - vote_pp_r 
  
)
# 
# data_2020_votes <- data_2020_votes %>%
#   select(year, state, party_detailed, candidatevotes, totalvotes) %>%  # Keep relevant columns
#   pivot_wider(names_from = party_detailed, values_from = candidatevotes)
# 
# data_2020_votes <- data_2020_votes %>% mutate(
#   vote_margin = ((REPUBLICAN - DEMOCRAT)/totalvotes)*100,
#   vote_pp_d = (DEMOCRAT/totalvotes)*100,
#   vote_pp_r = (REPUBLICAN/totalvotes)*100,
#   vote_diff_pp = vote_pp_d - vote_pp_r 
# )

votemargin_16 <- data_2016_votes %>% mutate(state = str_to_title(state),
                                            State = state.abb[match(state, state.name)]) %>%
  dplyr::select(State, vote_margin, REPUBLICAN, DEMOCRAT, totalvotes)

# adding in latino pop data 
latino.pop.data_16 <- read.csv("latino_pop.csv") %>% mutate(State = NAME,
                    State = state.abb[match(State, state.name)]) %>% 
  dplyr::select(State, percent.latino.2016)

### adding in the 2016 pieces to the CMPS data ----------
full_cmps2016 <- left_join(full_cmps2016, latino.pop.data_16, by = "State")
### adding vote margins ------
full_cmps2016 <- left_join(full_cmps2016, votemargin_16, by = "State")

full_cmps2016 <- full_cmps2016 %>% mutate(vote_margin = -vote_margin)

full_cmps2016$Battleground <- ifelse(full_cmps2016$vote_margin > -6 & full_cmps2016$vote_margin < 6, 1, 0)

#### subsetting to just latinos --------
#latinos_2016 <- full_cmps2016 %>% filter(Latino == 1)

#### Making the survey design ----------------------------

### To make the Survey Weights nationally Representative -- checking proportions
# 
# svydes <- svydesign(id = ~ 1, weights = ~NAT_WEIGHT, data = full_cmps2016)
# # # checking 
# prop.table(svytable(~full_cmps2016$Race_Prime, svydes))
# 
# 
# acs_margins <- list(
#   Race_Prime = c("(1) White Non Hispanic" = 0.62, 
#                  "(2) Hispanic or Latino" = 0.173, 
#                  "(3) Black or African American" = 0.123, 
#                  "(4) Asian American" = 0.052, 
#                  "(6) American Indian/Native American" = 0.007)
# )
# 
# # Apply Post-stratification & raking
# cmps_design <- svydesign(ids = ~1, data = full_cmps2016, weights = ~Weight)
# 
# pop_table <- data.frame(
#   Race_Prime = c("(1) White Non Hispanic", "(2) Hispanic or Latino", "(3) Black or African American", 
#                  "(4) Asian American", "(6) American Indian/Native American"),
#   Freq = c(0.62, 0.173, 0.123, 0.052, 0.007)  # ACS proportions
# )
# 
# acs_margins <- list(
#   data.frame(Race_Prime = c("(1) White Non Hispanic", "(2) Hispanic or Latino", 
#                             "(3) Black or African American", "(4) Asian American", 
#                             "(6) American Indian/Native American"),
#              Freq = c(0.62, 0.173, 0.123, 0.052, 0.007)) # Proportions
# )
# 
# # Post-stratify the weights
# cmps_design_adj <- postStratify(cmps_design, ~Race_Prime, pop_table, partial = TRUE)
# svytable(~Race_Prime, cmps_design_adj) / sum(weights(cmps_design_adj))
# prop.table(svytable(~Race_Prime, cmps_design_adj))
# 
# ### Subsetting to just Latinos
# cmps_lat_16 <- subset(cmps_design_adj, subset = cmps_design_adj$variables$Latino == 1)
# 
# ## alternate -- 
# cmps_lat_16_alt <- subset(cmps_design_adj, Latino == 1)

### Changing ICI to Factor 
full_cmps2016$ICI_Reverse <- (full_cmps2016$Imm_Con_Index * -1)
full_cmps2016$ICI_Index <- as.factor(full_cmps2016$ICI_Reverse)
full_cmps2016$ICI_Index <- relevel(full_cmps2016$ICI_Index, ref = "1")



### For Latinos ONLY checking Weights and Sample Representativeness among Nat. Origin Groups ---- 

latinos_data <- full_cmps2016 %>% filter(Race_Prime == "(2) Hispanic or Latino")

### Creating Survey Design ---
cmps_lat_16 <- svydesign(
  ids = ~1, 
  data = latinos_data, 
  weights = ~Weight
)


### Double Checking - Sample Prop Table - 
prop.table(svytable(~National_Origin, cmps_lat_16))

##Checking against 2016 ACS by Specific Origin
total_pop_2016 <- 55199107

national_origin_acs_2016 <- data.frame(Origins = c("Central American", "Cuban", "Dominican", "Mexican",
                         "Other", "Puerto Rican", "South American", "Spanish"),
             Pop = c(5002699, 2077828, 1788697, 35110480, NA, 5275008,3344238, 2600157)
    )
national_origin_acs_2016$Pop_Percent <- (national_origin_acs_2016$Pop/total_pop_2016)*100

## Overall - very small discrepancy, generally representative of Latinos 





