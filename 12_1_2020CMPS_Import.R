### Importing 2020 --- 
cmps2020 <- read_dta("/Users/jenniferlopez/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/CMPS 2020 full adult sample weighted STATA.dta",
                     encoding = "UTF-8")

### Subsetting to only relevant variables ######
cmps.sub.2020  <- cmps2020 %>% dplyr::select(S1, S2_Racer2,S2_Race_Prime, 
                                             S2_Hispanicr2,  
                                             S2_Hispanicr3,  S2_Hispanicr4, S2_Hispanicr5, 
                                             S3b, S4, S5, S5_Age, S7, S10, S10_Mex, S13,
                                             Q12, Q21, Q22, Q23, Q43, Q44, Q45, Q46, Q77r2,
                                             Q77r3, Q77r4, Q77r5, Q77r6, Q77r7, Q77r8, Q77r9,
                                             Q99r3, Q117r11, Q131r3, Q131r4, Q131r5,
                                             Q159r1, Q159r2, Q159r3, Q182, Q183, Q184r1, 
                                             Q184r2, Q187r1, Q208r1, Q208r2, Q208r3, Q208r4,
                                             Q208r5, Q213r1, Q217r1, Q217r2, Q217r3, Q217r4,
                                             Q261, Q411_Q416r1, Q411_Q416r2, Q411_Q416r3,
                                             Q411_Q416r4, Q411_Q416r5, Q411_Q416r6, 
                                             Q467_Q469r1, Q478_Q483r4, Q490r1, Q490r2,
                                             Q490r3, Q490r4, Q490r5, Q490r6, Q490r7, Q490r8,
                                             Q491r1, Q491r2, Q491r3, Q491r4, Q491r5, Q491r6,
                                             Q491r7, Q493, Q492, Q504r8, Q550r1, Q550r2, 
                                             Q550r3, Q550r4, Q550r5, Q550r6, Q551_Q559r2, 
                                             Q619_Q626r5, Q619_Q626r6, Q630_Q632r1, Q630_Q632r3,
                                             Q629r1, Q627, Q628, Q629r5, Q629r7, Q709_712r1,
                                             Q709_712r2, Q709_712r3, Q713, Q715_718r1, 
                                             Q715_718r2, Q715_718r3, Q715_718r4, Q809, Q812,
                                             Q816, weight, splitQ716, Q29, Q813
)


### Cleaning --------------------------------------------------------------------
cmps2020.clean <- cmps.sub.2020 %>% mutate(
  Survey.Lang = as.factor(S1),                                                   ## Language of Survey
  Hispanic = as.numeric(S2_Racer2),                                              ## Self identify as Hisp for sample 
  Race = as.factor(S2_Race_Prime),                                               ## Primary race/ethnicity 
  Self_Hisp = case_when(S2_Hispanicr2 == 2 ~ 1,                                  ## Consider themselves Hisp 
                        S2_Hispanicr2 == 0 ~ 0,
                        TRUE ~ NA),
  Parents_Hisp = case_when(S2_Hispanicr2 == 3 ~ 1,                               ## Consider parents Hisp 
                           S2_Hispanicr2 == 0 ~ 0,
                           TRUE ~ NA),
  GrParents_Hisp = case_when(S2_Hispanicr2 == 4 ~ 1,                             ## Consider grandparents Hisp 
                             S2_Hispanicr2 == 0 ~ 0,
                             TRUE ~ NA),
  DisRel_Hisp = case_when(S2_Hispanicr2 == 5 ~ 1,                                ## Consider distant relatives Hisp 
                          S2_Hispanicr2 == 0 ~ 0,
                          TRUE ~ NA),
  Gender = S3b,                                                                  ## Man - 1, Woman - 2, Nonbin - 3, Other - 4 
  State = S4,
  Birthyear = S5,
  Age_Category = as.factor(S5_Age),
  Age = 2025 - S5,
  Native = case_when(S7 == 1 ~ 1, 
                     S7 ==  2 ~ 0,
                     S7 == 3 ~ 0.5), 
  Country_Origin = S10,
  National_Origin = case_when(
    S10 == 12 ~ "Mexican",
    S10 == 6 ~ "Cuban",
    S10 == 17 ~ "Puerto Rican",
    S10 == 7 ~ "Dominican",
    S10 %in% c(9, 10, 11, 5, 13, 14) ~ "Central American",
    S10 %in% c(1, 2, 3, 4, 8, 15, 16, 18, 19, 20) ~ "South American",            # Brazil is included in 2020 CMPS
    S10 == 21 ~ "Spanish",
    TRUE ~ "Other"
  ),
  Cuban = ifelse(cmps.sub.2020$S10 == 6, 1, 0),                                  # Cuban
  Mexican = ifelse(cmps.sub.2020$S10 == 12, 1, 0),                               # Mexican --- other S10_Mex doesn't 
  Education = S13,                                                               # Numeric - 1-7, 1 - Grade school (up to 8th) and 7 - Post-grad
  Voted = case_when(Q12 == 1 ~ 1,                                                # Voted - recoded so ---> higher numbers, higher confidence of having voted
                    Q12 == 2 ~ .5, 
                    Q12 == 3 ~ -.5,
                    Q12 == 4 ~ 0), 
  PartyID_3 = case_when(Q21 == 1 ~ 0,                                            ## 0 - Rep, 1 - Independent/Other, 2 - Dem
                        Q21 == 3 ~ 1,
                        Q21 == 4 ~ 1, 
                        Q21 == 2 ~ 2), 
  PartyStrength = Q22,                                                           ## 1 - Strong, 2 - Not so strong
  Ind_Closer = Q23,                                                              ## 1 - Rep, 2 - Dem, 3 - Ind, 4 - Other party 
  Ideology = ifelse(cmps.sub.2020$Q43 == 6, NA_real_,                                         ## 1 - Very Lib, 5 - Very cons, 6 - none of these - recoded to NA
                    cmps.sub.2020$Q43),                                                               
  Trust_Government = case_when(Q44 == 4 ~ 0,                                      ## Trust in Govt in Washington to do right thing - Recoded so 0 - never, 3 - always
                               Q44 == 3 ~ 1,
                               Q44 == 2 ~ 2,
                               Q44 == 1 ~ 3), 
  Pol_System_Needs = case_when(Q45 == 4 ~ 0,                                      ## Pol System Helps Ppl with their needs - Recoded so 0 - strongly disagree, 3 - strongly agree
                               Q45 == 3 ~ 1,
                               Q45 == 2 ~ 2,
                               Q45 == 1 ~ 3), 
  Ext_Pol_Eff_1 = Q46,                                                           ## "The Pol System does not listen to ppl like me." -- left as-is, 1 - strongly agree, 4 - strongly disagree
  Contributed_CampMoney = case_when(Q77r2 == 1 ~ 5,                              ## Recoded - 5 - I did that, 1 - No, and don't want to. 
                                    Q77r2 == 2 ~ 4,                              ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                                    Q77r2 == 3 ~ 1,
                                    Q77r2 == 4 ~ 2,
                                    Q77r2 == 5 ~ 3),
  Wore_CampaignButton = case_when(Q77r3 == 1 ~ 5,                                ## Recoded - 5 - I did that, 1 - No, and don't want to.
                                  Q77r3 == 2 ~ 4,                                ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                                  Q77r3 == 3 ~ 1,
                                  Q77r3 == 4 ~ 2,
                                  Q77r3 == 5 ~ 3),
  Contacted_Rep =  case_when(Q77r4 == 1 ~ 5,                                     ## Recoded - 5 - I did that, 1 - No, and don't want to.
                             Q77r4 == 2 ~ 4,                                      ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                             Q77r4 == 3 ~ 1,
                             Q77r4 == 4 ~ 2,
                             Q77r4 == 5 ~ 3),
  Contacted_Means =  case_when(Q77r5 == 1 ~ 5,                                   ## Recoded - 5 - I did that, 1 - No, and don't want to.
                               Q77r5 == 2 ~ 4,                                   ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                               Q77r5 == 3 ~ 1,                                   # unclear what this is asking ---> just says "Contacted in any way, such as by letter, telephone, internet, or in perso" on all docs
                               Q77r5 == 4 ~ 2,
                               Q77r5 == 5 ~ 3),
  Cooperated_Others = case_when(Q77r6 == 1 ~ 5,                                  ## Recoded - 5 - I did that, 1 - No, and don't want to.
                                Q77r6 == 2 ~ 4,                                  ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                                Q77r6 == 3 ~ 1,                                   
                                Q77r6 == 4 ~ 2,
                                Q77r6 == 5 ~ 3),
  Discussed_Pol_Online = case_when(Q77r7 == 1 ~ 5,                               ## Recoded - 5 - I did that, 1 - No, and don't want to.
                                   Q77r7 == 2 ~ 4,                               ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                                   Q77r7 == 3 ~ 1,                                 
                                   Q77r7 == 4 ~ 2,
                                   Q77r7 == 5 ~ 3),, 
  Signed_Petition = case_when(Q77r8 == 1 ~ 5,                                    ## Recoded - 5 - I did that, 1 - No, and don't want to.
                              Q77r8 == 2 ~ 4,                                    ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                              Q77r8 == 3 ~ 1,                                   
                              Q77r8 == 4 ~ 2,
                              Q77r8 == 5 ~ 3), 
  Boycott_Pol = case_when(Q77r9 == 1 ~ 5,                                        ## Recoded - 5 - I did that, 1 - No, and don't want to.
                          Q77r9 == 2 ~ 4,                                        ## 3 and 1 were switched --  3 was no & don't want to, 1 was did not but will
                          Q77r9 == 3 ~ 1,                                      
                          Q77r9 == 4 ~ 2,
                          Q77r9 == 5 ~ 3),
  Effective_Protests_Imm = case_when(Q77r5 == 1 ~ 5,                             ## Recoded - 5 - Very effective, 1 - very ineffective.
                                     Q77r5 == 2 ~ 4,                             
                                     Q77r5 == 3 ~ 3,                                   
                                     Q77r5 == 4 ~ 2,
                                     Q77r5 == 5 ~ 1),
  Priority_ImmRights = Q117r11, 
  Family_Separation = case_when(Q131r3 == 1 ~ 5,                                 ## Recoded - 5 - str support, 1 - str oppose
                                Q131r3 == 2 ~ 4,                                  
                                Q131r3 == 3 ~ 3,                                   
                                Q131r3 == 4 ~ 2,
                                Q131r3 == 5 ~ 1), 
  Pathway_Citizenship = case_when(Q131r4 == 1 ~ 5,                               ## Recoded - 5 - str support, 1 - str oppose
                                  Q131r4 == 2 ~ 4,                                  
                                  Q131r4 == 3 ~ 3,                                   
                                  Q131r4 == 4 ~ 2,
                                  Q131r4 == 5 ~ 1), 
  EssentialWorkers_Citizenship = case_when(Q131r5 == 1 ~ 5,                      ## Recoded - 5 - str support, 1 - str oppose
                                           Q131r5 == 2 ~ 4,                                  
                                           Q131r5 == 3 ~ 3,                                   
                                           Q131r5 == 4 ~ 2,
                                           Q131r5 == 5 ~ 1),
  Government_Helps_Latinos = Q159r1,                                             # Never - All the time (1-5)
  Latinos_Say = Q159r2, 
  ICE_Accountable = case_when(Q182 == 1 ~ 1,                                     ## Who should be held accountable for hiring of undoc workers?
                              Q182 == 3 ~ 2,                                     ## Switched order so it goes from employer --> both --> only undocumented immigrants
                              Q182 == 2 ~ 3), 
  ICE_Opinion = Q183, 
  BorderSecurity = case_when(Q184r1 == 5 ~ 1,                                    ## BorderSec as a Nat'l Priority - recoded, 1 - strongly disagree - 5 - strongly agree
                             Q184r1 == 4 ~ 2,
                             Q184r1 == 3 ~ 3,
                             Q184r1 == 2 ~ 4,
                             Q184r1 == 1 ~ 5), 
  Immigration_Easier = case_when(Q184r1 == 5 ~ 1,                                ## Easier to get visa + citizenship - recoded, 1 - strongly disagree - 5 - strongly agree
                                 Q184r1 == 4 ~ 2,
                                 Q184r1 == 3 ~ 3,
                                 Q184r1 == 2 ~ 4,
                                 Q184r1 == 1 ~ 5),  
  Birthright_Undocumented = Q187r1,                                              # Kept as is - 1 - Strongly disagree about removing automatic birthright, 5 - strongly agree
  Individualism1 = Q208r1, 
  Individualism2 = Q208r2, 
  Individualism3 = Q208r3,
  Individualism4 = Q208r4,
  Individualism5 = Q208r5, 
  Resentment1 = case_when(Q213r1 == 5 ~ 1,                                       ## Irish & Italian, others should do the same --> 1) agree, 5) disagree
                          Q213r1 == 4 ~ 2, 
                          Q213r1 == 3 ~ 3,
                          Q213r1 == 2 ~ 4,
                          Q213r1 == 1 ~ 5),
  RaceDenial1 = case_when(Q217r1 == 5 ~ 1,                                       ## recoded, 1-strongly disagree, 5-strongly agree - White ppl don't have advantages
                          Q217r1 == 4 ~ 2, 
                          Q217r1 == 3 ~ 3,
                          Q217r1 == 2 ~ 4,
                          Q217r1 == 1 ~ 5),  
  RaceDenial2 = case_when(Q217r2 == 5 ~ 1,                                       ## recoded, 1-strongly disagree, 5-strongly agree - Race problems are rare isolated events
                          Q217r2 == 4 ~ 2, 
                          Q217r2 == 3 ~ 3,
                          Q217r2 == 2 ~ 4,
                          Q217r2 == 1 ~ 5), 
  RaceDenial3 = Q217r3,                                                        ## kept asis, 1-strongly agree, 5-strongly disagree - I am angry that racism exists
  RaceDenial4 = case_when(Q217r4 == 5 ~ 1,                                       ## recoded, 1-strongly disagree, 5-strongly agree - I am sometimes fearful of people of other races
                          Q217r4 == 4 ~ 2, 
                          Q217r4 == 3 ~ 3,
                          Q217r4 == 2 ~ 4,
                          Q217r4 == 1 ~ 5), 
  Skintone = Q261, 
  Little_Interest = Q411_Q416r1,                                                 ## Mental Health 1 - little interest / pleasure in doing things over past 2 weeks
  Down_Depressed = Q411_Q416r2,                                                 ## Mental Health 2 - Feeling down, depressed, or hopeless over past 2 weeks
  Depressed =  case_when(Q411_Q416r2 == 1 ~ 0,                                  ### Binned to avoid pathologizing.... 
                            Q411_Q416r2 > 1 ~ 1,
                            TRUE ~ NA_real_),
  Nervous_OnEdge = Q411_Q416r3,                                                  ## Mental Health 3 - Feeling nervous, anxious, or on edge over past 2 weeks
  Anxiety = case_when(Q411_Q416r3 == 1 ~ 0,                                     ### Binned to avoid pathologizing.... 
                      Q411_Q416r3 > 1 ~ 1,
                      TRUE ~ NA_real_),
  Afraid = Q411_Q416r4,                                                          ## Mental Health 4 - Feeling afraid as if something awful might happen over past 2 weeks
  Dread = case_when(Q411_Q416r4 == 1 ~ 0,                                       ### Binned to avoid pathologizing.... 
                    Q411_Q416r4 > 1 ~ 1,
                    TRUE ~ NA_real_),
  Cannot_Relax = Q411_Q416r5,                                                    ## Mental Health 5 - Can't relax unless they know what will happen the next day
  Uncertainty_Stressed = Q411_Q416r6,                                            ## Mental Health 6 - Uncertainty makes them stressed / anxious / uneasy 
  Helped_Someone_SeekImm = case_when(Q467_Q469r1 == 1 ~ 1,                       ## Helped someone seek immigration-related information or services (1 - Yes, 0 - No [recoded])
                                     Q467_Q469r1 == 2 ~ 0),  
  Feel_FullCitizen =  case_when(Q478_Q483r4 == 7 ~ 1,                            ## recoded, 1-strongly disagree, 7-strongly agree - I feel like a full and equal citizen in this country with all the rights & protections others have
                                Q478_Q483r4 == 6 ~ 2, 
                                Q478_Q483r4 == 5 ~ 3,
                                Q478_Q483r4 == 4 ~ 4,
                                Q478_Q483r4 == 3 ~ 5,
                                Q478_Q483r4 == 2 ~ 6,
                                Q478_Q483r4 == 1 ~ 7), 
  BeenStopped_Questioned = Q490r1,
  BeenStopped_Questioned_Family = Q490r2,
  BeenDetained_Deported_Family = Q490r3,
  BeenStopped_Questioned_OtherRel = Q490r4,
  BeenDetained_Deported_OtherRel = Q490r5, 
  BeenStopped_Questioned_FriendCoworker = Q490r6, 
  BeenDetained_Deported_FriendCoworker = Q490r7, 
  No_DoNotKnowAnyone = Q490r8,
  DoNotKnowUndoc = Q491r1,                                                       # Questions over whether they know someone undocumented & their relation (0 - no, 1 - yes)
  Parent_Undoc = Q491r2, 
  Sibling_Undoc = Q491r3, 
  Children_Undoc = Q491r4,
  OtherFam_Undoc = Q491r5, 
  Friend_Undoc = Q491r6,
  Coworker_Undoc = Q491r7, 
  Worried_DetainedDeported = case_when(Q493 == 1 ~ 4,                           ### about themselves -- worry over being detained /deported 
                                       Q493 == 2 ~ 2,
                                       Q493 == 3 ~ 3,
                                       Q493 == 4 ~ 1
                                      ),                                      
  Worried_DetainedDeported_SomeoneElse = case_when(Q492 == 1 ~ 4,                ### about ppl they know -- worry over being detained /deported 
                                                   Q492 == 2 ~ 2,
                                                   Q492 == 3 ~ 3,
                                                   Q492 == 4 ~ 1
  ),                               
  Anxiety_Stress_ImmPolicy = Q504r8,                                             ## HOw much anxiety / stress has Imm Policy caused you - 1 - least 10 - most
  KnowLatinoImm_Partner = Q550r1, 
  KnowLatinoImm_GPParents = Q550r2, 
  KnowLatinoImm_Children = Q550r3, 
  KnowLatinoImm_CloseFriend = Q550r4, 
  KnowLatinoImm_DoNot = Q550r5, 
  KnowLatinoImm_Unsure = Q550r6, 
  Linked_Fate = Q551_Q559r2,                                                     # 1 - nothing, 5 - a huge amt 
  Immigrant_Disc = case_when(Q619_Q626r5 == 1 ~ 4,                               # How much disc against immigrants (1 - none at all, 4 - a lot [recoded 5 "don't know" as NA])
                             Q619_Q626r5 == 2 ~ 3,
                             Q619_Q626r5 == 3 ~ 2,
                             Q619_Q626r5 == 4 ~ 1,
                             Q619_Q626r5 == 5 ~ NA_real_),
  Latinos_Disc = case_when(Q619_Q626r6 == 1 ~ 4,                                 # How much disc against Latinos (1 - none at all, 4 - a lot [recoded 5 "don't know" as NA])
                           Q619_Q626r6 == 2 ~ 3,
                           Q619_Q626r6 == 3 ~ 2,
                           Q619_Q626r6 == 4 ~ 1,
                           Q619_Q626r6 == 5 ~ NA_real_),
  Assume_DontSpeakEnglish = case_when(Q630_Q632r1 == 1 ~ 1,                      # Ppl act as if you don't speak English (recoded, 1 - yes, 0 - no)
                                      Q630_Q632r1 == 2 ~ 0), 
  Assume_NotAmerican = case_when(Q630_Q632r3 == 1 ~ 1,                           # Ppl assume as if you are not American (recoded, 1 - yes, 0 - no)
                                 Q630_Q632r3 == 2 ~ 0), 
  Disc_Racial_Ethnic = Q629r1, 
  Discriminated = case_when(Q627 == 1 ~ 1,                                       # Recoded 1 - Yes, 0 - No
                            Q627 == 2 ~ 0), 
  Disc_Where = Q628,                                                             # Where 1 - US, 2 -Country of Origin, 3 - In both
  Disc_Imm_Status = Q629r5, 
  Disc_Imm_Accent = Q629r7, 
  Race_Denial5 = Q709_712r1,                                                     # White privilege is a major problem in the US today (1 - strongly agree, 4 - Strongly disagree)
  Race_Denial6 = case_when(Q709_712r2 == 1 ~ 4,                                  # Discrimination against Whites has been a big a problem as against others (recoded, 1- strongly disagree, 4 - strongly agree)
                           Q709_712r2 == 2 ~ 3,
                           Q709_712r2 == 3 ~ 2,
                           Q709_712r2 == 4 ~ 1),                                                     
  Race_Denial7 = case_when(Q709_712r3 == 1 ~ 4,                                  # Am. Way of Life needs to be protected from foreign influence (recoded, 1- strongly disagree, 4 - strongly agree)
                           Q709_712r3 == 2 ~ 3,
                           Q709_712r3 == 3 ~ 2,
                           Q709_712r3 == 4 ~ 1),    
  Part_ImmComm = case_when(Q713 == 1 ~ 4,                                        # Part of Imm Community (recoded, 1- never, 4 - all the time)
                           Q713 == 2 ~ 3,
                           Q713 == 3 ~ 2,
                           Q713 == 4 ~ 1),    , 
  Belong_USSociety = case_when(Q715_718r1 == 1 ~ 1,                              # Believe they belong in US Society (1 - a lot, 3 - not at all)
                               Q715_718r1 == 2 ~ 2,
                               Q715_718r1 == 3 ~ 3), 
  InsOutsider_USSociety = Q715_718r2,
  Split_Terms_Ins = splitQ716,
  Outsider_USSociety = ifelse(cmps.sub.2020$splitQ716 == 1,                      # Outsider in US Society (recoded, 1 (not at all) - 3 (a lot)), flipped the scale for those shown "outsider" (---> higher numbers, higher feelings of being excluded) 
                              cmps.sub.2020$Q715_718r2,
                              4 - cmps.sub.2020$Q715_718r2),
  Value_RespectYou = Q715_718r3,                                                 # how much do those in the US value and respect you? 1 - a lot, 3 - not at all.    
  Excluded_US = Q715_718r4,                                                      # how much do those in the US accept and include you? 1 - a lot, 3 - not at all.    
  Parents_Born = case_when(Q809 == 2 ~ 1,                                        # Recoded - 1 - both outside the US, 2 - 1/1, 3 - Both in PR, 4 - both in US, 88 - IDK
                           Q809 == 4 ~ 2,
                           Q809 == 3 ~ 3,
                           Q809 == 1 ~ 4, 
                           Q809 == 88 ~ NA_real_),
  Grandparents_Born = case_when(Q812 == 5 ~ 1,                                   # Recoded - 1 - 4 outside the US, 2 - 3/1, 3 - 2/2, 4 - 1/3, 5 - all 4 in the US, 88 - NA
                                Q812 == 4 ~ 2,
                                Q812 == 3 ~ 3,
                                Q812 == 2 ~ 4, 
                                Q812 == 1 ~ 5,
                                Q812 == 88 ~ NA_real_),
  Spanish_Use = case_when(Q816 == 5 ~ 1,                                         # Recoded - 1 - never, 5 - very often 
                          Q816 == 4 ~ 2,
                          Q816 == 3 ~ 3,
                          Q816 == 2 ~ 4, 
                          Q816 == 1 ~ 5),
  More_Than_SecondGen = case_when(Native == 0 ~ 0,                               # Immigrant respondent, first gen 
                                  Native == 1 & Parents_Born < 3 ~ 1,            # At least one immigrant parent, second gen
                                  Native == 1 & Parents_Born == 3 ~ 2,           # Both are born in the US, at least third gen
                                  Parents_Born == 9 ~ NA_real_,                  ##### collapsing so any US-born with US-born Parents is marked as "above 2nd gen" and 1 is 2nd gen
                                  is.na(Parents_Born) & Native == 1 ~ NA_real_),    
  Pol_Interest = case_when(Q29 == 1 ~ 4,                                         # Reversed --> 1 - not at all 4 - very interested
                           Q29 == 2 ~ 3, 
                           Q29 == 3 ~ 2, 
                           Q29 == 4 ~ 1),
  Income = ifelse(Q813 == 99, NA_real_, Q813)
)
##### Discrimination Measure + Int/External Belong Measures ====================
cmps2020.clean <- cmps2020.clean %>% mutate(
  Belong_USSociety_rev = 4 - Belong_USSociety,
  Outsider_USSociety_rev = 4 - Outsider_USSociety,
  Value_RespectYou_rev = 4 - Value_RespectYou,
  Excluded_US_rev = 4 - Excluded_US,
  Internal_Belonging = Belong_USSociety_rev + Outsider_USSociety_rev,
  External_Belonging = Value_RespectYou_rev + Excluded_US_rev,
  Discriminated_US = ifelse(Discriminated == 1 & Disc_Where == 1, 1, 0)
)

cmps2020.clean <- cmps2020.clean %>%
  mutate(state_labels = as_factor(State), 
         States = state.abb[match(state_labels, state.name)])
cmps2020.clean$States <- ifelse(cmps2020.clean$state_labels == "District of Columbia", "DC", cmps2020.clean$States)
#### Adding Pop + State Pol Characteristics ====================================
data_president <- read_csv("/Users/jenniferlopez/Desktop/COIi work/Latino_Belonging/dataverse_files/1976-2020-president.csv")
data_votes <- data_president %>% filter(year == 2020)

data_2020_votes <- data_votes %>% filter(party_detailed == "REPUBLICAN" | 
                                           party_detailed == "DEMOCRAT") %>% 
  filter(year == 2020) %>% filter(writein == FALSE)

data_2020_votes <- data_2020_votes %>%
  dplyr::select(year, state, party_detailed, candidatevotes, totalvotes) %>%  # Keep relevant columns
  pivot_wider(names_from = party_detailed, values_from = candidatevotes)

data_2020_votes <- data_2020_votes %>% mutate(
  vote_margin = ((REPUBLICAN - DEMOCRAT)/totalvotes)*100,
  vote_pp_d = (DEMOCRAT/totalvotes)*100,
  vote_pp_r = (REPUBLICAN/totalvotes)*100,
  vote_diff_pp = vote_pp_d - vote_pp_r
)

votemargin_20 <- data_2020_votes %>% mutate(state = str_to_title(state),
                                            State = state.abb[match(state, state.name)]) %>%
  dplyr::select(State, vote_margin, REPUBLICAN, DEMOCRAT, totalvotes)

# adding in latino pop data 
latino.pop.data_20 <- read.csv("/Users/jenniferlopez/Desktop/COIi work/Latino_Belonging/latino_pop.csv")  %>% 
  filter(NAME != "Puerto Rico") %>% 
  mutate(State = NAME, State = state.abb[match(State, state.name)]) %>% 
  dplyr::select(State, percent.latino.2020)

latino.pop.data_20$State[is.na(latino.pop.data_20$State)] <- "DC"
### adding in the 2016 pieces to the CMPS data ----------
full_cmps2020 <- left_join(cmps2020.clean, latino.pop.data_20, by = c("States" = "State"))
### adding vote margins ------
full_cmps2020 <- left_join(full_cmps2020, votemargin_20, by = c("States" = "State"))

full_cmps2020 <- full_cmps2020 %>% mutate(vote_margin = -vote_margin)

full_cmps2020$Battleground <- ifelse(full_cmps2020$vote_margin > -6 & full_cmps2020$vote_margin < 6, 1, 0)

##### Imm Climate Index ========================================================
final_scores <- read.csv("scores_final.csv")

fullcmps2020 <- left_join(full_cmps2020, final_scores, by = c("States" = "State"))

#### Create Survey Design ======================================================

latinos_data_20 <- fullcmps2020 %>% filter(S2_Racer2 == 1) %>% mutate(
  sym_lat_index_16 = case_when(
    latino_sym_16 < -10 ~ -1,
    latino_sym_16 >= -10 & latino_sym_16 < -5 ~ -0.5,
    latino_sym_16 >= -5 & latino_sym_16 <= 5 ~ 0,
    latino_sym_16 > 5 & latino_sym_16 <= 15 ~ 0.5,
    latino_sym_16 > 15 ~ 1),
  conc_lat_index_16 = case_when(
    latino_conc_16 < -10 ~ -1,
    latino_conc_16 >= -10 & latino_conc_16 < -5 ~ -0.5,
    latino_conc_16 >= -5 & latino_conc_16 <= 5 ~ 0,
    latino_conc_16 > 5 & latino_conc_16 <= 15 ~ 0.5,
    latino_conc_16 > 15 ~ 1),
  sym_lat_index_20 = case_when(
    latino_sym_20 < -10 ~ -1,
    latino_sym_20 >= -10 & latino_sym_20 < -5 ~ -0.5,
    latino_sym_20 >= -5 & latino_sym_20 <= 5 ~ 0,
    latino_sym_20 > 5 & latino_sym_20 <= 15 ~ 0.5,
    latino_sym_20 > 15 ~ 1),
  conc_lat_index_20 = case_when(
    latino_conc_20 < -10 ~ -1,
    latino_conc_20 >= -10 & latino_conc_20 < -5 ~ -0.5,
    latino_conc_20 >= -5 & latino_conc_20 <= 5 ~ 0,
    latino_conc_20 > 5 & latino_conc_20 <= 15 ~ 0.5,
    latino_conc_20 > 15 ~ 1)
)


cmps_lat_20 <- svydesign(
  ids = ~1, 
  data = latinos_data_20, 
  weights = ~weight
)

# Checking proporitions 
prop.table(svytable(~National_Origin, cmps_lat_20))

##Checking against 2020 ACS by Specific Origin -- total pop is total Latino pop -- 59,361,020
total_pop_2020 <- 59361020

national_origin_acs_2020 <- data.frame(National_Origin = c("Central American", "Cuban", "Dominican", "Mexican",
                                                           "Other", "Puerto Rican", "South American", "Spanish"),
                                       Pop = c(5497573, 2332584, 2042360, 36537028, 1942951, 5699150,3819508, 1489866)
)
national_origin_acs_2020$ACS_Prop <- national_origin_acs_2020$Pop / total_pop_2020

# Get CMPS sample proportions
cmps_props <- as.data.frame(prop.table(svytable(~National_Origin, cmps_lat_20)))
colnames(cmps_props) <- c("National_Origin", "Sample_Prop")

# Merge ACS and CMPS proportions
post_strat_weights <- merge(national_origin_acs_2020, cmps_props, by = "National_Origin")

# Compute post-stratification weight (ACS proportion divided by sample proportion)
post_strat_weights$Post_Weight <- post_strat_weights$ACS_Prop / post_strat_weights$Sample_Prop

# Joining post-stratification weights back to original data
latinos_data_20 <- latinos_data_20 %>%
  left_join(post_strat_weights %>% dplyr::select(National_Origin, Post_Weight), by = "National_Origin") %>%
  mutate(weight_adj = weight * Post_Weight)

# New survey design object with adjusted weights
cmps_lat_20_adj <- svydesign(
  ids = ~1,
  data = latinos_data_20,
  weights = ~weight_adj
)