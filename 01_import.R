### Data from CH.2 
cmps2020 <- read_dta("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/CMPS 2020 full adult sample weighted STATA.dta",
                     encoding = "UTF-8")
### Subsetting to specific vars ########
cmps_sub <- cmps2020 %>% select(uuid, S2_Racer2, S2_Race_Prime, S2_Hispanicr2,
                                S2_Hispanicr3, S2_Hispanicr4, S3b,
                                S4, S5_Age, S7, S10, S10_Mex, S13, S15,
                                Q21, Q22, Q22, Q29, Q145, Q146, Q147, Q151, Q152,
                                Q153, Q183,
                                Q184r1, Q184r2, Q184r3, Q352, Q467_Q469r1, 
                                Q467_Q469r2, Q467_Q469r3, 
                                Q470, Q471r1, Q471r2, Q471r3, Q471r4, Q471r5, 
                                Q471r6, Q471r7, Q471r8, Q471r9, Q490r1, Q490r2, 
                                Q490r3,
                                Q490r4, Q490r5, Q490r6, Q490r7, Q490r8,
                                Q491A, Q491r1, Q491r2, Q491r3, Q491r4,
                                Q491r5, Q491r6, Q491r7, Q492, Q493,
                                Q543, Q551_Q559r2, Q560r1,Q560r5, Q639, 
                                Q714, Q713, Q715_718r1, Q715_718r2, Q715_718r3,
                                Q715_718r4, Q794r1, Q794r2, Q794r3, Q794r4,
                                Q794r5, Q794r6,Q807, Q807, Q807r8oe, 
                                Q808, Q809, Q812, Q813, Q814, Q816, 
                                Q560r1, Q560r2, Q560r3, Q560r4, 
                                Q560r5, Q560r6, Q560r7, Q560r8, weight, Q271, 
                                Q478_Q483r4, Q197A1, Q197B1,
                                Q12, Q31, Q504r8, Q505_Q508r1, Q505_Q508r2,
                                Q505_Q508r4, Q509_Q511r1, Q509_Q511r2, 
                                Q509_Q511r3, Q630_Q632r3, Q633, Q627, Q628,
                                Q619_Q626r5, Q619_Q626r6,
                                Q411_Q416r4,Q411_Q416r3, 
                                #Q412, Q413, Q414, Q415, Q416, ### emotions
                                Q308r1, Q411_Q416r1, Q411_Q416r2, Q154r1,
                                Q154r2, Q154r3, Q154r4, Q154r5, Q159r1,
                                Q159r2, Q159r3, Q411_Q416r6, Q411_Q416r5)   ############## 197 A1 and B1 are if election was today, 31 is internal efficacy (no external on CMPS)
### 308 is feeling thermometer on undoc immigrants
## excluding MENA, AI/NA, NH, PI

cmps_sub <- cmps_sub %>% filter(!c(S2_Race_Prime == 5 | S2_Race_Prime == 6 | S2_Race_Prime == 7 | S2_Race_Prime == 8))
# table(cmps_sub$S2_Race_Prime)
# 
# ### Making the Survey Weights Representative -- checking proportions
# 
# svydes <- svydesign(id = ~ 1, weights = ~weight, data = cmps_sub)
# # checking 
# prop.table(svytable(~cmps_sub$S2_Race_Prime, svydes))

# adjusting weight based on 2021 ACS estimates
# 60.6% White non-Hispanic
# 11.6% Black non-Hispanic
# 6.02% Asian non-Hispanic
# 0.511% AI/AN
# 17.2% Hispanic or Latino
# 4.09% Other or multiracial


#### Cleaning ###### 
cmps_clean <- cmps_sub %>% mutate(Hispanic = ifelse(cmps_sub$S2_Racer2 == 1, 1, 
                                                    ifelse(cmps_sub$S2_Race_Prime == 2, 1,
                                                           ifelse(cmps_sub$S2_Hispanicr2 == 2,1, 
                                                                  ifelse(cmps_sub$S2_Hispanicr3 == 3, 1, 
                                                                         ifelse(cmps_sub$S2_Hispanicr4 == 4, 1, 0))))),
                                  race_weight = case_when(S2_Race_Prime == 1 ~ weight*(0.64/0.2345043),
                                                          S2_Race_Prime == 2 ~ weight*(0.17/0.2455291),
                                                          S2_Race_Prime == 3 ~ weight*(0.13/0.2817263),
                                                          S2_Race_Prime == 4 ~ weight*(0.06/0.2382403)),
                                  Gender = case_when(S3b == 1 ~ 1,              # man
                                                     S3b == 2 ~ 2,              # woman
                                                     S3b == 3 ~ 3,              # other 
                                                     S3b == 4 ~ 3),             # other 
                                  State = S4,
                                  Age = S5_Age, 
                                  Mexican = ifelse(cmps_sub$S10 == 12 | 
                                                     cmps_sub$S10_Mex == 1, 1, 
                                                   0),
                                  Immigrant = ifelse(cmps_sub$S7 == 2, 1, 0),
                                  Education = S13,                              # 1 - Grade school, 7 - post-grad
                                  Zip = S15,
                                  Party = case_when(Q21 == 1 ~ 1,               # Rep
                                                    Q21 == 2 ~ 2,               # Dem
                                                    Q21 == 3 ~ 3,               # Other
                                                    Q21 == 4 ~ 3),              # Other 
                                  Party_5pt = ifelse(Q21 == 1 & Q22 == 1, 1, 
                                                     ifelse(Q21 == 1 & Q22 == 2, 2,
                                                            ifelse(Q21 == 2 & Q22 == 1, 3,
                                                                   ifelse(Q21 == 2 & Q22 == 2, 4,
                                                                          0)))),
                                  Republican = ifelse(cmps_sub$Q21 == 1, 1, 0), 
                                  Interest_Pol = Q29,                           # 1 - Very, 4 - Not at all
                                  Econ_Hope = Q145,                             # 1 - more hope, 7 - much less
                                  Econ_Angry = Q146,                            # 1 - more angry, 7 - much less
                                  Econ_Fear = Q147,                             # 1 - more afraid, 7 - much less
                                  ice_opinion = Q183,                           # 1 - very unfavorable, 4 - very favorable  
                                  border_sec_first = Q184r1,
                                  border_sec_sec = Q184r2,
                                  border_sec_third = Q184r3,
                                  border_security_combined = case_when(Q184r1 == 1 ~ 1, 
                                                                       Q184r2 == 1 ~ 1,
                                                                       Q184r3 == 1 ~ 1,
                                                                       Q184r1 == 2 ~ 2, 
                                                                       Q184r2 == 2 ~ 2, 
                                                                       Q184r3 == 2 ~ 2,
                                                                       Q184r1 == 3 ~ 3,
                                                                       Q184r2 == 3 ~ 3,
                                                                       Q184r3 == 3 ~ 3,
                                                                       Q184r1 == 4 ~ 4,
                                                                       Q184r2 == 4 ~ 4,
                                                                       Q184r3 == 4 ~ 4,
                                                                       Q184r1 == 5 ~ 5,
                                                                       Q184r2 == 5 ~ 5,
                                                                       Q184r3 == 5 ~ 5),  # 1 - Strongly agree (increase even if migrants die), strongly disagree
                                  collab_gov_deport = Q352,                      # 1 - no collab, 2 - some, 3 - extensive, 88 - IDK
                                  imm_helpinfo = Q467_Q469r1,
                                  imm_crimereport = Q467_Q469r2,
                                  imm_intharrassimm = Q467_Q469r3,              # 467 - 469, 1 - yes, 2 - no 
                                  DACA_use = Q470,                              # 1 - yes (you, family or close friend), 2 - no 
                                  DACA_who = case_when(Q471r1 == 1 ~ "Me",
                                                       Q471r2 == 1 ~ "My child",
                                                       Q471r3 == 1 ~ "My parent",
                                                       Q471r4 == 1 ~ "My partner",
                                                       Q471r5 == 1 ~ "My sibling",
                                                       Q471r6 == 1 ~ "Aunt/Uncle",
                                                       Q471r7 == 1 ~ "Cousin/Other Relative",
                                                       Q471r8 == 1 ~ "Grandparent",
                                                       Q471r9 == 1 ~ "Close friend"),
                                  ImmEnf_Stopped_Who = case_when(Q490r1 == 1 ~ "Me",
                                                                 Q490r2 == 1 ~ "Family member, stopped or questioned",
                                                                 Q490r3 == 1 ~ "Family member, detained or deported",
                                                                 Q490r4 == 1 ~ "Some other relative, stopped or questioned",
                                                                 Q490r5 == 1 ~ "Some other relative, detained or deported",
                                                                 Q490r6 == 1 ~ "Friend/Coworker, stopped or questioned",
                                                                 Q490r7 == 1 ~ "Friend/Coworker, detained or deported",
                                                                 Q490r8 == 1 ~ "No"),
                                  ImmEnf_Enc_PosNeg = Q491A,                    # 1 - mostly pos, 2 - neutral, 3 - mostly neg
                                  Worried_Detained_Deported = Q492,
                                  Worried_Personally_DetainedDeported = Q493,
                                  Increase_BorderSpend_Wall = Q543,             # 1 - support, 2 - oppose
                                  Linked_Fate = Q551_Q559r2,                           # 1 - nothing to do with my life, 5 - a huge amt to do with my life
                                  Own_Home = Q639,                              # 1 - own, 2 - live with homeowners, 3 - rent, 4 - live w renters, 5 - other
                                  Imm_Comm = Q713,                              # 1 - all the time, 4 - never (do you think of yourself as an imm or part of the imm comm)
                                  Family_Origin_Questioned = Q714,              # 1 - Frequently, 4 - Never
                                  Belong_USSociety = Q715_718r1,                # 1 - A lot, 3 - Not much, 4 (technically, though zero respondents responded with 4...) - not at all
                                  Accepted_Included_USSoc = Q715_718r4,
                                  Value_Respect_inUSSoc = Q715_718r3,
                                  Remit = ifelse(cmps_sub$Q794r1 == 1, 0, 1),
                                  Remit_Parents = ifelse(cmps_sub$Q794r2 == 1, 1, 0),
                                  Remit_Children = ifelse(cmps_sub$Q794r3 == 1, 1, 0),
                                  Remit_Grandparents = ifelse(cmps_sub$Q794r4 == 1, 1, 0),
                                  Remit_OtherFam = ifelse(cmps_sub$Q794r5 == 1, 1, 0),
                                  Remit_Friends = ifelse(cmps_sub$Q794r6 == 1, 1, 0),
                                  Legal_Status = Q807,
                                  Imm_Naturalized = ifelse(cmps_sub$Q807 == 1, 1, 0),
                                  Imm_Resident = ifelse(cmps_sub$Q807 == 3, 1, 0),
                                  Legal_Status_Other = Q807r8oe,
                                  Year_Naturalized = Q808,
                                  Parents_Born = ifelse(Q809 == 1, 1,           # originally - 1 - both in the US, 2 - both in another, 3 - PR, 4 - 1 in US, 1 Out, 88 -IDK
                                                        ifelse(Q809 == 2, 4,
                                                               ifelse(Q809 == 4, 2,
                                                                      ifelse(Q809 == 3, 3, NA)))),    # Recoded - 1 - Both in the US, 2 - 1 in US/1 Outside, 3 - PR, 4 - Both outside US
                                  Grandparents_Born = Q812,                     # 1 - All in US, 5 - All 4 outside of US 
                                  Income = Q813,                                # 1 - > 20k, 12 - < 200k, 99 - IDK
                                  Employed = Q814,                              # 1 - full time, 5 - unemployed, 6 - homemaker
                                  Spanish = Q816,                               # 1 - Very often, 5 - almost never
                                  Race_Imp = Q560r1,
                                  CulturalID_Imp = Q560r5,
                                  American_Imp = case_when(Q560r8 == 8 ~ 1,     # Recoded so 1 is least imp, 8 is most 
                                                           Q560r8 == 7 ~ 2,
                                                           Q560r8 == 6 ~ 3,
                                                           Q560r8 == 5 ~ 4,
                                                           Q560r8 == 4 ~ 5,
                                                           Q560r8 == 3 ~ 6,
                                                           Q560r8 == 2 ~ 7,
                                                           Q560r8 == 1 ~ 8),
                                  identity_strength = Q271, # (All) 1 - most important, 5 - least important, recoded to flip 
                                  identity_strength_recoded = case_when(Q271 == 1 ~ 5,
                                                                        Q271 == 2 ~ 4,
                                                                        Q271 == 3 ~ 3,
                                                                        Q271 == 4 ~ 2,
                                                                        Q271 == 5 ~ 1),
                                  Full_Citizen = Q478_Q483r4,
                                  # Parents_Born = ifelse(cmps_sub$Parents_Born == 88, NA, 
                                  #                       cmps_sub$Parents_Born),
                                  # Grandparents_Born = ifelse(cmps_sub$Grandparents_Born == 88, NA,
                                                             # cmps_sub$Grandparents_Born),
                                  Parents_Born_Recoded = case_when(Parents_Born == 1 ~ 3,                       # Recoded so 3 - All in US, 2 - 1 in US, 1 - None in US
                                                                   Parents_Born == 2 ~ 2,
                                                                   Parents_Born == 4 ~ 1),
                                  Grandparents_Born_Recoded = case_when(Grandparents_Born == 1 ~ 5,
                                                                        Grandparents_Born == 2 ~ 4,
                                                                        Grandparents_Born == 3 ~ 3,
                                                                        Grandparents_Born == 4 ~ 2,
                                                                        Grandparents_Born == 5 ~ 1),
                                  Accult = Parents_Born_Recoded + Grandparents_Born_Recoded,
                                  Increase_Border_Spending = case_when(Increase_BorderSpend_Wall == 1 ~ 1,      # Recoded - 1 is Support, 0 is Oppose 
                                                                       Increase_BorderSpend_Wall == 2 ~ 0),
                                  # Under_200_Miles = ifelse(distance_km < 321.869, 1, 0),
                                  # Under_100_Miles = ifelse(distance_km < 160.934, 1, 0),
                                  linked_simp = case_when(Linked_Fate == 1 ~ 1,
                                                          Linked_Fate == 2 ~ 1,
                                                          Linked_Fate == 3 ~ 1,
                                                          Linked_Fate == 4 ~ 2,
                                                          Linked_Fate == 5 ~ 2),
                                  id_simp = case_when(identity_strength == 1 ~ 1,
                                                      identity_strength == 2 ~ 1,
                                                      identity_strength == 3 ~ 0,
                                                      identity_strength == 4 ~ 0,
                                                      identity_strength == 5 ~ 0),
                                  psych_dist_imm = Accult + Imm_Comm,
                                  # dist_sqd = distance_km^2,
                                  California = ifelse(State == 5, 1 ,0),
                                  Texas = ifelse(State == 44, 1 ,0),
                                  Arizona = ifelse(State == 3, 1, 0),
                                  New_Mexico = ifelse(State == 32, 1, 0),
                                  border_state = ifelse(State == 5, 1, 
                                                        ifelse(State == 44, 1 ,
                                                               ifelse(State == 3, 1,
                                                                      ifelse(State == 32, 1, 0)))),
                                  border_security_recoded = case_when(border_sec_first == 1 ~ 5,
                                                                      border_sec_first == 2 ~ 4,
                                                                      border_sec_first == 3 ~ 3,
                                                                      border_sec_first == 4 ~ 2,
                                                                      border_sec_first == 5 ~ 1),
                                  psych_dist_lang = psych_dist_imm + Spanish,
                                  Belong_US = case_when(Belong_USSociety == 1 ~ 3,  ### 1 - Not at all, 3 - A lot 
                                                        Belong_USSociety == 2 ~ 2,
                                                        Belong_USSociety == 3 ~ 1),
                                  Accepted_US = case_when(Accepted_Included_USSoc == 1 ~ 3,
                                                          Accepted_Included_USSoc == 2 ~ 2,
                                                          Accepted_Included_USSoc == 3 ~ 1),
                                  Value_US = case_when(Value_Respect_inUSSoc == 1 ~ 3,
                                                       Value_Respect_inUSSoc == 2 ~ 2,
                                                       Value_Respect_inUSSoc == 3 ~ 1),
                                  Full_Cit = case_when(Full_Citizen == 1 ~ 3,
                                                       Full_Citizen == 2 ~ 3,
                                                       Full_Citizen == 3 ~ 3,
                                                       Full_Citizen == 4 ~ 2,
                                                       Full_Citizen == 5 ~ 1,
                                                       Full_Citizen == 6 ~ 1,
                                                       Full_Citizen == 7 ~ 1),
                                  citizenship_exp = Belong_US + Accepted_US + Value_US,
                                  citizenship_ext = citizenship_exp + Full_Cit, 
                                  Type_Border = case_when(State == 3 ~ 1, 
                                                          Texas == 1 ~ 1,
                                                          State == 32 ~ 2,                                      
                                                          California == 1 ~ 2, 
                                                          border_state == 0 ~ 3),                                 #Least Inclusive -- 1, More Inclusive --  2, Non-border -- 3
                                  Inclusive = case_when(State == 3 ~ 0,                                         #Least Inclusive -- 0, More Inclusive --  1, Non-border -- 
                                                        Texas == 1 ~ 0,
                                                        State == 32 ~ 1,                                      
                                                        California == 1 ~ 1),
                                  # Remittances_Index <- ifelse(cmps_sub$Remit_Children == 1 |
                                  #                               cmps_sub$Remit_Friends == 1 |
                                  #                               cmps_sub$Remit_Grandparents == 1 |
                                  #                               cmps_sub$Remit_OtherFam == 1 | 
                                  #                               cmps_sub$Remit_Parents == 1, 1,
                                  #                                            0),
                                  # Remittances_Scale = (Remit_Children + Remit_Friends + Remit_Grandparents +
                                  #                          Remit_OtherFam + Remit_Parents),
                                  Anxiety_Imm = Q504r8,
                                  Susp_You = Q505_Q508r1, 
                                  Insult_You = Q505_Q508r2,
                                  Graffiti_Target = Q505_Q508r4, 
                                  Govt_Disc = Q509_Q511r1, 
                                  Govt_Off = Q509_Q511r2, 
                                  Neg_Exec = Q509_Q511r3,                       ## all 505-508 go from never (1) to very often (4) 
                                  Assume_NotAmerican = case_when(Q630_Q632r3 == 1 ~ 1,
                                                                 Q630_Q632r3 == 2 ~ 0), ### recoded yes 1 no 0 
                                  Disc_Affect = case_when(Q633 == 1 ~ 6,
                                                          Q633 == 2 ~ 5,
                                                          Q633 == 3 ~ 4,
                                                          Q633 == 4 ~ 3,
                                                          Q633 == 5 ~ 2,
                                                          Q633 == 6 ~ 1),       # recoded, 1 is refused, 2 is IDK, 3 is not at all - 6 a lot 
                                  Pers_Disc = case_when(Q627 == 1 ~ 1,
                                                        Q627 == 2 ~ 0),                             ####Experienced Disc yes 1 no 0, recoded
                                  Disc_Where = case_when(Q628 == 1 ~ 1, 
                                                         Q628 == 3 ~ .5,
                                                         Q628 == 2 ~ 0),         ##### Experienced Disc In the US = 1 & both in and out of US = .5, neither 0
                                  Imm_Disc = case_when(Q619_Q626r5 == 1 ~ 5,
                                                       Q619_Q626r5 == 2 ~ 4,
                                                       Q619_Q626r5 == 3 ~ 3,
                                                       Q619_Q626r5 == 4 ~ 2,
                                                       Q619_Q626r5 == 5 ~ 1),    ### 1 is don't know, 2 is none at all, 5 is a lot  
                                  Latino_Disc = case_when(Q619_Q626r6 == 1 ~ 5,
                                                          Q619_Q626r6 == 2 ~ 4,
                                                          Q619_Q626r6 == 3 ~ 3,
                                                          Q619_Q626r6 == 4 ~ 2,
                                                          Q619_Q626r6 == 5 ~ 1), ## 1 is IDK, 2 is none, 5 is a lot
                                  Ext_Pol_Efficacy = ifelse(cmps_sub$Q31 == 1, 1, 0),
                                  Int_Pol_Efficacy = ifelse(cmps_sub$Q31 == 2, 1, 0),
                                  Low_Pol_Efficacy = ifelse(cmps_sub$Q31 == 1 | cmps_sub$Q31 == 1, 1, 0),
                                  Voted = case_when(Q12 == 1 ~ 1,
                                                    Q12 == 2 ~ 1,
                                                    Q12 == 3 ~ 0,
                                                    Q12 == 4 ~ 0),              ### Voted 1 did not 0, 
                                  Experienced_Disc = ifelse(cmps_sub$Q627 == 2 & cmps_sub$Q628 == 2, 0,
                                                            ifelse(cmps_sub$Q627 == 2, 0, 1)), ### reverse coding - marking those that did not experience disc or only experienced disc in their home countries as 0 all others as 1
                                  ) 

party_maj <- read.csv("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/party_majority.csv")

# merging 

full_cmps <- left_join(cmps_clean, party_maj, by = "State")
state_abb <- full_cmps$State_abb
## Inclusivity 
# write.csv(unique(state_abb), "state_abb.csv")
inclusivity <- readxl::read_xlsx("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/inclusive.xlsx")
colnames(inclusivity) <- c("State_abb","inclusivity", "inclusivity_varied")
full_cmps_2020 <- left_join(full_cmps, inclusivity, by = "State_abb")
full_cmps_2020$inclusivity_varied <- as.numeric(full_cmps_2020$inclusivity_varied)


#### Making Indexes 

full_cmps_2020 <- full_cmps_2020 %>% mutate( 
  ### Perceived Index 
  Full_Perceived_Index = (Disc_Affect + Latino_Disc + Imm_Disc),
  Perceived_Index_Groups = Latino_Disc + Imm_Disc,
  age_sqd = Age^2,
  #Parents_Born_NO_NA = ifelse(is.na(full_cmps_2020$Parents_Born) == TRUE, 
                                             #99, full_cmps_2020$Parents_Born),
  #Grandparents_Born_NO_NA = ifelse(is.na(full_cmps_2020$Grandparents_Born) == TRUE, 
                                                  #99, full_cmps_2020$Grandparents_Born),
  missing_birth = ifelse(is.na(full_cmps_2020$Parents_Born) == TRUE | is.na(full_cmps_2020$Grandparents_Born) == TRUE, 1, 0),
  ## mental health indicators
  little_interest = Q411_Q416r1,
  depressed = Q411_Q416r2,
  anxious = Q411_Q416r3,
  foreboding = Q411_Q416r4, ### like something might happen suddenly
  anxious_no_plan = Q411_Q416r5,
  uncertainty_anxious = Q411_Q416r6,
  NativeBorn = case_when(S7 == 1 ~ 1, 
                         S7 == 2 ~ 0,
                         S7 == 3 ~ .5), ### PR is .5
  MoreThanSecondGen = case_when(NativeBorn == 0 ~ 0,
                                NativeBorn == 1 & Parents_Born_Recoded < 3 ~ 1,
                                NativeBorn == 1 & Parents_Born_Recoded == 3 ~ 2, 
                                is.na(Parents_Born_Recoded) & NativeBorn == 1 ~ NA_real_,
                                TRUE ~ NA_real_),
  state_relations_race_hope = Q151, 
  state_relations_race_angry = Q152,
  state_relations_race_afraid = Q153, 
  pride_amflag = Q154r1,
  pride_participation = Q154r2,
  pride_protests_race = Q154r3,
  pride_milestone_race = Q154r4,
  pride_electedoff_race = Q154r5,
  pub_off_respond_race = Q159r1,
  say_in_govt_race = Q159r2
  )

#### Adding in 2020 pop and votes --------------


votemargin_20<- data_2020_votes %>% mutate(state = str_to_title(state),
                                            State = state.abb[match(state, state.name)]) %>%
  select(State, vote_margin, REPUBLICAN, DEMOCRAT, totalvotes)
votemargin_20$state_abb <- votemargin_20$State
# adding in latino pop data 
latino.pop.data_20 <- read.csv("latino_pop.csv") %>% mutate(State = NAME,
                                                            State = state.abb[match(State, state.name)]) %>% 
  select(State, percent.latino.2020)
latino.pop.data_20$state_abb <- latino.pop.data_20$State
## matching state to # in CMPS ----------
states <- cbind(c(state.abb[c(1:8)], "DC", state.abb[c(9:50)]), c(1:51)) ### CMPS includes DC 
colnames(states) <- c("state_abb", "State")
states <- as.data.frame(states) %>% mutate(State = as.numeric(State))
full_cmps_2020 <- left_join(full_cmps_2020, states, by = "State")
### adding in 
full_cmps_2020 <- left_join(full_cmps_2020, latino.pop.data_20, by = "state_abb")

### adding vote margins ------
full_cmps_2020 <- left_join(full_cmps_2020, votemargin_20, by = "state_abb")

### Restricting to Latinos -------
latinos_cmps_2020 <- full_cmps_2020 %>% filter(Hispanic == 1)

latinos_cmps_2020 <- latinos_cmps_2020 %>% mutate(
  fear 
)

                