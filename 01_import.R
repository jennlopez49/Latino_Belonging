### Data from CH.2 
cmps2020 <- read_dta("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/CMPS 2020 full adult sample weighted STATA.dta",
                     encoding = "UTF-8")
### Subsetting to specific vars ########
cmps_sub <- cmps2020 %>% select(uuid, S2_Racer2, S2_Race_Prime, S2_Hispanicr2,
                                S2_Hispanicr3, S2_Hispanicr4, S3b,
                                S4, S5_Age, S7, S10, S10_Mex, S13, S15,
                                Q21, Q22, Q22, Q29, Q145, Q146, Q147, Q183,
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
                                Q478_Q483r4, Q197A1, Q197B1, Q31).   ############## 197 A1 and B1 are if election was today, 31 is internal efficacy (no external on CMPS)

## excluding MENA, AI/NA, NH, PI

cmps_sub <- cmps_sub %>% filter(!c(S2_Race_Prime == 5 | S2_Race_Prime == 6 | S2_Race_Prime == 7 | S2_Race_Prime == 8))
# table(cmps_sub$S2_Race_Prime)

### Making the Survey Weights Representative -- checking proportions

svydes <- svydesign(id = ~ 1, weights = ~weight, data = cmps_sub)
# checking 
prop.table(svytable(~cmps_sub$S2_Race_Prime, svydes))

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
                                  Full_Citizen = Q478_Q483r4
) 
