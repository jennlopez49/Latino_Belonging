### IMPORTING PILOT #1 ---------------------------------------------------------
library(GGally)
library(Hmisc)
pilot_open <- read.csv("Latino Political Attitudes and Behavior - Prolific_May 14, 2025_23.58.csv")
pilot_open <- pilot_open %>% mutate(across(everything(), ~na_if(., "")))        ### Setting so any blanks are treated as NA   

### Keeping labels as metadata --- but getting rid of the rows
labels <- pilot_open[1,]
pilot_op <- pilot_open[-c(1,2),]
for (col in names(pilot_op)) {
  label(pilot_op[[col]]) <- labels[[col]]
}


### Cleaning 
pilot_clean <- pilot_op %>% mutate(
  Latino_Num = case_when(Latino == "Yes, Mexican" ~ 3, 
                         Latino == "Yes, Puerto Rican" ~ 2, 
                         Latino == "Yes, other (Please specify.)" ~ 1,
                         Latino == "Yes, Cuban" ~ 0,
                         Latino == "No" ~ 99),                                   ## setting it so higher --> mex
  Other_Latino_Origins = Latino_5_TEXT,
  Birth.Year = as.numeric(BirthYear),
  Sex_Gender = case_when(Sex == "Male" ~ 0,
                         Sex == "Female" ~ 1,
                         Sex == "" ~ NA),
  Skintone_num = as.numeric(SkinTone),
  Income = as.factor(Income),
  Race = as.factor(Race),
  State = as.factor(State),
  Education = as.factor(Education),
  LatinoTerm = as.factor(TermLatinoHispanic),
  PolKnowl1_PartyPower = case_when(PolKnow1 == "the Republican Party" ~ 1,
                        PolKnow1 == "-99" ~ -99, 
                        PolKnow1 == "" ~ -99), ### Setting don't know, refused
  PolKnow2_Speaker = case_when(PolKnowledgeMatrix._1 == "Mike Johnson" ~ 1,
                               PolKnowledgeMatrix._1 == "Mitch McConnell" ~ 0, 
                               PolKnowledgeMatrix._1 == "" ~ -99),
  PolKnow3_Spending = case_when(PolKnowledgeMatrix._2 == "Chuck Schumer" ~ 1,
                                PolKnowledgeMatrix._2 == "Mike Johnson" ~ 0,
                                PolKnowledgeMatrix._2 == "Mitch McConnell" ~ 0,
                                PolKnowledgeMatrix._2 == "Joe Manchin" ~ 0,
                                PolKnowledgeMatrix._2 == "" ~ -99),
  PolKnowIndex = PolKnowl1_PartyPower + PolKnow2_Speaker + PolKnow3_Spending,
  SocDomMatrix1_Equal = case_when(SocDomMatrix_1 == "Strongly disagree" ~ 5,
                                  SocDomMatrix_1 == "Somewhat disagree" ~ 4,
                                  SocDomMatrix_1 == "Neither agree nor disagree" ~ 3,
                                  SocDomMatrix_1 == "Somewhat agree" ~ 2,
                                  SocDomMatrix_1 == "Strongly agree" ~ 1,
                                  SocDomMatrix_1 == "" ~ -99),
  SocDomMatrix2_Chance = case_when(SocDomMatrix_2 == "Strongly disagree" ~ 1,
                                   SocDomMatrix_2 == "Somewhat disagree" ~ 2,
                                   SocDomMatrix_2 == "Neither agree nor disagree" ~ 3,
                                   SocDomMatrix_2 == "Somewhat agree" ~ 4,
                                   SocDomMatrix_2 == "Strongly agree" ~ 5,
                                   SocDomMatrix_2 == "" ~ -99),                                    # Reverse coded --> higher numbers more social dominant
  SocDomMatrix3_Dom = case_when(SocDomMatrix_3 == "Strongly disagree" ~ 5,
                                SocDomMatrix_3 == "Somewhat disagree" ~ 4,
                                SocDomMatrix_3 == "Neither agree nor disagree" ~ 3,
                                SocDomMatrix_3 == "Somewhat agree" ~ 2,
                                SocDomMatrix_3 == "Strongly agree" ~ 1,
                                SocDomMatrix_3 == "" ~ -99),
  SocDom_Index = SocDomMatrix1_Equal + SocDomMatrix2_Chance + SocDomMatrix3_Dom,
  Native = case_when(Birthplace == "Yes" ~ 1,
                         Birthplace == "No" ~ 1,
                         Birthplace == "" ~ -99),
  ParentsBirth = case_when(ParentsBirthplace == "Both were born outside the US" ~ 3, 
                           ParentsBirthplace == "One in the US, one elsewhere" ~ 2,
                           ParentsBirthplace == "Both were born outside the US" ~ 1,
                           ParentsBirthplace == "" ~ -99),
  Spanish = case_when(Language == "English" ~ 3,
                      Language == "English and Spanish are equally used" ~ 2,
                      Language == "Spanish" ~ 1,
                      Language == "" ~ -99),
  Acc = Native + ParentsBirth + Spanish,
  Linked_Fate = as.factor(LinkedFate),
  ImmResent_1 = case_when(ImmResentment_Matrix_1 == "Strongly disagree" ~ 1,     ### ALL ITEMS ARE AGREE/DISAGREE W STATEMENTS "Undoc imms make other [Latinos/Hispanics/etc] look bad"
                          ImmResentment_Matrix_1 == "Somewhat disagree" ~ 2,
                          ImmResentment_Matrix_1 == "Neither agree or agree" ~ 3,
                          ImmResentment_Matrix_1 == "Somewhat agree" ~ 4,
                          ImmResentment_Matrix_1 == "Strongly agree" ~ 5,
                          ImmResentment_Matrix_1 == "" ~ -99),
  ImmResent_2 = case_when(ImmResentment_Matrix_2 == "Strongly disagree" ~ 1,    ### Previous generations of immigrants were able 
                          ImmResentment_Matrix_2 == "Somewhat disagree" ~ 2,    ## to become successful without relying on government welfare benefits - new immigrants should do the same.
                          ImmResentment_Matrix_2 == "Neither agree or agree" ~ 3,
                          ImmResentment_Matrix_2 == "Somewhat agree" ~ 4,
                          ImmResentment_Matrix_2 == "Strongly agree" ~ 5,
                          ImmResentment_Matrix_2 == "" ~ -99),
  ImmResent_3 = case_when(ImmResentment_Matrix_3 == "Strongly disagree" ~ 1,    ### There is no excuse for breaking the law and entering the US illegally.
                          ImmResentment_Matrix_3 == "Somewhat disagree" ~ 2,    
                          ImmResentment_Matrix_3 == "Neither agree or agree" ~ 3,
                          ImmResentment_Matrix_3 == "Somewhat agree" ~ 4,
                          ImmResentment_Matrix_3 == "Strongly agree" ~ 5,
                          ImmResentment_Matrix_3 == "" ~ -99),
  ImmResent_4 = case_when(ImmResentment_Matrix_4 == "Strongly disagree" ~ 1,    ### Lats/Hisps would be treated better in the US if immigrants would try harder to learn
                          ImmResentment_Matrix_4 == "Somewhat disagree" ~ 2,    ##  English and adopt US customs like other ethnic groups have done.
                          ImmResentment_Matrix_4 == "Neither agree or agree" ~ 3,
                          ImmResentment_Matrix_4 == "Somewhat agree" ~ 4,
                          ImmResentment_Matrix_4 == "Strongly agree" ~ 5,
                          ImmResentment_Matrix_4 == "" ~ -99),
  ImmResent_Index = ImmResent_1 + ImmResent_2 + ImmResent_3 + ImmResent_4,
  Voted = as.factor(Voted),
  VotedFor = as.factor(VotedFor),
  Party = case_when(Party3 == "Democrat" & Party7pt == "Strong" ~ "Strong Democrat",
                    Party3 == "Democrat" & Party7pt == "Not so strong" ~ "Not so strong Democrat",
                    Party3 == "Republican" & Party7pt == "Strong" ~ "Strong Republican",
                    Party3 == "Republican" & Party7pt == "Not so strong" ~ "Not so strong Republican",
                    Party3 == "Something else" & !is.na(Party3_3_TEXT) &
                      (Party3_3_TEXT == "Independent" | grepl("Independent", Party3_3_TEXT)) & 
                      IndParty == "Closer to the Democratic Party" ~ "Lean Democrat",
                    Party3 == "Something else" & !is.na(Party3_3_TEXT) & 
                      (Party3_3_TEXT == "Independent" | grepl("Independent", Party3_3_TEXT)) & 
                      IndParty == "Closer to the Republican Party" ~ "Lean Republican",
                    Party3 == "Something else" & !is.na(Party3_3_TEXT) &
                      (Party3_3_TEXT == "Independent" | grepl("Independent", Party3_3_TEXT)) & 
                      IndParty == "Neither" ~ "Independent",
                    Party3 == "Something else" ~ "Other",
                    TRUE ~ NA_character_),
  IntragroupDistinct = as.factor(IntragroupDistinct),
  Relief = as.numeric(Emotions_1),                                              ## Emotions -- 0:10, none to a lot
  Anger = as.numeric(Emotions_2),
  Pride = as.numeric(Emotions_9),
  Fear = as.numeric(Emotions_10),
  Joy = as.numeric(Emotions_11),
  Shame = as.numeric(Emotions_12),
  ManipCheck1 = as.factor(ManipCheck1),
  TreatmentCondition = as.factor(FL_12_DO),
  ManipCheck_Verif = ifelse(TreatmentCondition == "Control_Policy" & ManipCheck1 == "State symbols", 1,
                            ifelse((TreatmentCondition %in% c("Treatment-Hypothetical_Anti", "Treatment-Hypothetical_Pro")) & ManipCheck1 == "Immigration", 2,
                                   ifelse((TreatmentCondition %in% c("Treatment-Hypothetical_Anti", "Treatment-Hypothetical_Pro")) & ManipCheck1 == "Healthcare", 3,
                                          0))),
  HypPolicy_Attitudes = case_when(ImmAttitudeHyp == "Strongly oppose" ~ 1,
                                  ImmAttitudeHyp == "Somewhat oppose" ~ 2,
                                  ImmAttitudeHyp == "Neither support nor oppose" ~ 3,
                                  ImmAttitudeHyp == "Somewhat support" ~ 4,
                                  ImmAttitudeHyp == "Strongly support" ~ 5),
  HypPolicy_Likely = case_when(LikelyToHappen == "Not at all likely" ~ 1,
                               LikelyToHappen == "Somewhat unlikely" ~ 2,
                               LikelyToHappen == "Somewhat likely" ~ 3,
                               LikelyToHappen == "Very likely" ~ 4),
  HypAffectLatino_Community = case_when(AffectLatinoComm == "It would negatively affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 5,
                                     AffectLatinoComm == "It would somewhat negatively affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 4,
                                     AffectLatinoComm == "It would not affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 3,
                                     AffectLatinoComm == "It would somewhat positively affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 2,
                                     AffectLatinoComm == "It would positively affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 1),
  HypAffectImm_Community = case_when(AffectImmComm == "It would negatively affect immigrants" ~ 5,
                                     AffectImmComm == "It would somewhat negatively affect immigrants" ~ 4,
                                     AffectImmComm == "It would not affect immigrants" ~ 3,
                                     AffectImmComm == "It would somewhat positively affect immigrants" ~ 2,
                                     AffectImmComm == "It would positively affect immigrants" ~ 1),
  HypPersonallyAffect = case_when(PersonallyAffect == "No, the same amount" ~ 2, 
                                  PersonallyAffect == "No, a little less" ~ 1, 
                                  PersonallyAffect == "Yes, a little more" ~ 3,
                                  PersonallyAffect == "Yes, much more" ~ 4,
                                  PersonallyAffect == "Don't know" ~ 0),  ### Don't know ---> 0, could be informative a true IDK
  GroupDiscImms_Perc = case_when(GroupDiscImms == "None at all" ~ 1,            ### How much disc is there against imms? none said none at all --> might mess up the code here 
                                 GroupDiscImms == "A little" ~ 2,
                                 GroupDiscImms == "Some" ~ 3, 
                                 GroupDiscImms == "A lot" ~ 4),
  GroupDiscLatinos_Perc = case_when(GroupDiscLatinos == "None at all" ~ 1,         # also none said none at all
                                    GroupDiscLatinos == "A little" ~ 2,
                                    GroupDiscLatinos == "Some" ~ 3, 
                                    GroupDiscLatinos == "A lot" ~ 4), 
  Disc_Ethnicity = ifelse(str_detect(PersDisc, "Yes, for my ethnicity"), 1, 0),
  Disc_Immigrant = ifelse(str_detect(PersDisc, "Yes, for being an immigrant"), 1, 0),
  Disc_Accent = ifelse(str_detect(PersDisc, "Yes, for having an accent"), 1, 0),
  Disc_Total = Disc_Ethnicity + Disc_Immigrant + Disc_Accent,
  Discriminated = ifelse(is.na(PersDisc), NA, Disc_Total),                       ## distinguishing between 0s and NAs
  ImmsMajSourceCrime = case_when(StigmaImmMatrix_1 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                 StigmaImmMatrix_1 == "Somewhat disagree" ~ 2,
                                 StigmaImmMatrix_1 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                 StigmaImmMatrix_1 == "Somewhat agree" ~ 4,
                                 StigmaImmMatrix_1 == "Strongly agree" ~ 5),
  PoliticiansTalkNegImm = case_when(StigmaImmMatrix_2 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                 StigmaImmMatrix_2 == "Somewhat disagree" ~ 2,
                                 StigmaImmMatrix_2 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                 StigmaImmMatrix_2 == "Somewhat agree" ~ 4,
                                 StigmaImmMatrix_2 == "Strongly agree" ~ 5),
  PoliciesImmUnfair = case_when(StigmaImmMatrix_3 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                StigmaImmMatrix_3 == "Somewhat disagree" ~ 2,
                                StigmaImmMatrix_3 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                StigmaImmMatrix_3 == "Somewhat agree" ~ 4,
                                StigmaImmMatrix_3 == "Strongly agree" ~ 5),
  LatinosMajSourceCrime = case_when(StigmaLatinoMatrix_1 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                 StigmaLatinoMatrix_1 == "Somewhat disagree" ~ 2,
                                 StigmaLatinoMatrix_1 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                 StigmaLatinoMatrix_1 == "Somewhat agree" ~ 4,
                                 StigmaLatinoMatrix_1 == "Strongly agree" ~ 5),
  PoliticiansTalkNegLatinos = case_when(StigmaLatinoMatrix_2 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                 StigmaLatinoMatrix_2 == "Somewhat disagree" ~ 2,
                                 StigmaLatinoMatrix_2 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                 StigmaLatinoMatrix_2 == "Somewhat agree" ~ 4,
                                 StigmaLatinoMatrix_2 == "Strongly agree" ~ 5),
  PoliciesLatinosUnfair = case_when(StigmaLatinoMatrix_3 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                    StigmaLatinoMatrix_3 == "Somewhat disagree" ~ 2,
                                    StigmaLatinoMatrix_3 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                    StigmaLatinoMatrix_3 == "Somewhat agree" ~ 4,
                                    StigmaLatinoMatrix_3 == "Strongly agree" ~ 5),
  ExternalPolEfficacy1 = case_when(IntExtEffPost._1 == "Strongly agree" ~ 1, ### "Public officials don't care about what people like me think." --> higher #s --> higher efficacy
                                   IntExtEffPost._1 == "Somewhat agree" ~ 2,
                                   IntExtEffPost._1 == "Neither agree or agree" ~ 3, ## --> FIX!
                                   IntExtEffPost._1 == "Somewhat disagree" ~ 4,
                                   IntExtEffPost._1 == "Strongly disagree" ~ 5),
  InternalPolEfficacy1 = case_when(IntExtEffPost._2 == "Strongly agree" ~ 1, ### "Sometimes, politics seem so complicated that a person like me can’t really understand what’s going on in government." --> higher #s --> higher efficacy
                                   IntExtEffPost._2 == "Somewhat agree" ~ 2,
                                   IntExtEffPost._2 == "Neither agree or agree" ~ 3, ## --> FIX!
                                   IntExtEffPost._2 == "Somewhat disagree" ~ 4,
                                   IntExtEffPost._2 == "Strongly disagree" ~ 5),
  GovernmentResponsive = case_when(IntExtEffPost._3 == "Strongly disagree" ~ 1, ### "My government is responsive to what the people in my country want." --> higher #s --> higher efficacy
                                   IntExtEffPost._3 == "Somewhat disagree" ~ 2,
                                   IntExtEffPost._3 == "Neither agree or agree" ~ 3, ## --> FIX!
                                   IntExtEffPost._3 == "Somewhat agree" ~ 4,
                                   IntExtEffPost._3 == "Strongly agree" ~ 5),
  InternalPolEfficacy2 = case_when(IntExtEffPost._3 == "Strongly disagree" ~ 1, ### "I feel that I have a pretty good understanding of the important political issues debated by the government." --> higher #s --> higher efficacy
                                   IntExtEffPost._3 == "Somewhat disagree" ~ 2,
                                   IntExtEffPost._3 == "Neither agree or agree" ~ 3, ## --> FIX!
                                   IntExtEffPost._3 == "Somewhat agree" ~ 4,
                                   IntExtEffPost._3 == "Strongly agree" ~ 5),
  Belonging_State = as.numeric(BelongingPost_1),                                # 1 - 10 (not at all - a lot)
  Belonging_US = as.numeric(BelongingPost_2),                                   # 1 - 10 (not at all - a lot)
  Belong_State_Open = BelongText1,                                              # Free text answers to why they feel they do / do not belong in their state
  Belong_US_Open = BelongText,                                                  # Free text answers to why they feel they do / do not belong in the US 
  BorderWall = case_when(BorderPoliciesMatrix_1 == "Strongly oppose" ~ 1,       ## higher numbers --> higher support for a border wall
                         BorderPoliciesMatrix_1 == "Somewhat oppose" ~ 2,
                         BorderPoliciesMatrix_1 == "Neither favor or oppose" ~ 3, 
                         BorderPoliciesMatrix_1 == "Somewhat favor" ~ 4,
                         BorderPoliciesMatrix_1 == "Strongly favor" ~ 5),
  DeportAllUndocu = case_when(BorderPoliciesMatrix_2 == "Strongly oppose" ~ 1,       ## higher numbers --> higher support for a border wall
                              BorderPoliciesMatrix_2 == "Somewhat oppose" ~ 2,
                              BorderPoliciesMatrix_2 == "Neither favor or oppose" ~ 3, 
                              BorderPoliciesMatrix_2 == "Somewhat favor" ~ 4,
                              BorderPoliciesMatrix_2 == "Strongly favor" ~ 5),
  TroopsBorder = case_when(BorderPoliciesMatrix_3 == "Strongly oppose" ~ 1,       ## higher numbers --> higher support for a troops at border
                           BorderPoliciesMatrix_3 == "Somewhat oppose" ~ 2,
                           BorderPoliciesMatrix_3 == "Neither favor or oppose" ~ 3,
                           BorderPoliciesMatrix_3 == "Somewhat favor" ~ 4,
                           BorderPoliciesMatrix_3 == "Strongly favor" ~ 5),
  DecreaseBorderSecSpending = case_when(BorderPoliciesMatrix_4 == "Strongly favor" ~ 1,       ## lower #s --> more support for less border security spending
                                        BorderPoliciesMatrix_4 == "Somewhat favor" ~ 2,
                                        BorderPoliciesMatrix_4 == "Neither favor or oppose" ~ 3, 
                                        BorderPoliciesMatrix_4 == "Somewhat oppose" ~ 4,
                                        BorderPoliciesMatrix_4 == "Strongly oppose" ~ 5),
  ProhibitRaidsPlaces = case_when(BorderPoliciesMatrix_5 == "Strongly favor" ~ 1,       ## Places: schools, hospitals, places of worship --> higher #s --> more anti-imm
                                  BorderPoliciesMatrix_5 == "Somewhat favor" ~ 2,
                                  BorderPoliciesMatrix_5 == "Neither favor or oppose" ~ 3, 
                                  BorderPoliciesMatrix_5 == "Somewhat oppose" ~ 4,
                                  BorderPoliciesMatrix_5 == "Strongly oppose" ~ 5),
  BirthrightCitizenship = case_when(BorderPoliciesMatrix_6 == "Strongly favor" ~ 1,   ### higher #s --> more anti-imm
                                    BorderPoliciesMatrix_6 == "Somewhat favor" ~ 2,
                                    BorderPoliciesMatrix_6 == "Neither favor or oppose" ~ 3, 
                                    BorderPoliciesMatrix_6 == "Somewhat oppose" ~ 4,
                                    BorderPoliciesMatrix_6 == "Strongly oppose" ~ 5),
  Participation_Information_NoCue = case_when(Participation1 == "No" ~ 0, 
                                                 Participation1 == "Yes" ~ 1),
  Participation_Information_LatinoCue = case_when(Participation2 == "No" ~ 0, 
                                                  Participation2 == "Yes" ~ 1),
  Part_NoCue_Text = OpenEnd,
  Part_LatinoCue_Text = OpenEndVers2,
  ParticipationVersion = as.factor(FL_51_DO),
  Order_Imm_Lat_Stigma = as.factor(FL_72_DO),
  Participation_Response = case_when(                                           # Combined Responses for Both Paricipation Qs --> Include Var on which version they saw in model
    !is.na(Participation1) ~ ifelse(Participation1 == "Yes", 1, 0),
    !is.na(Participation2) ~ ifelse(Participation2 == "Yes", 1, 0),
    TRUE ~ NA_real_
  ),
  StartDateTime = as.POSIXct(StartDate, format = "%m/%d/%y %H:%M"),
  EndDateTime = as.POSIXct(EndDate, format = "%m/%d/%y %H:%M"),
  Duration_Minutes = as.numeric(difftime(EndDateTime, StartDateTime, units = "mins"))
)


### Saving Cleaned Version -----------------------------------------------------

write.csv(pilot_clean, "pilot_1_clean.csv")

### Importing + Cleaning Pilot 2 -----------------------------------------------

pilot2 <- read.csv("Latino Political Attitudes and Behavior - Prolific - Shortened Open-Ended_June 8, 2025_14.03.csv")
pilot2 <- pilot2 %>% mutate(across(everything(), ~na_if(., "")))    

### Keeping labels as metadata --- but getting rid of the rows
labels <- pilot2[1,]
pilot_op2 <- pilot2[-c(1,2),]
for (col in names(pilot_op2)) {
  label(pilot_op2[[col]]) <- labels[[col]]
}

## making sure the column names are the same ---> 

setdiff(names(pilot_op), names(pilot_op2))
setdiff(names(pilot_op2), names(pilot_op))

pilot_2_clean <- pilot_op2 %>% mutate(
  Latino_Num = case_when(Latino == "Yes, Mexican" ~ 3, 
                         Latino == "Yes, Puerto Rican" ~ 2, 
                         Latino == "Yes, other (Please specify.)" ~ 1,
                         Latino == "Yes, Cuban" ~ 0,
                         Latino == "No" ~ 99),                                   ## setting it so higher --> mex
  Other_Latino_Origins = Latino_5_TEXT,
  Birth.Year = as.numeric(BirthYear),
  Sex_Gender = case_when(Sex == "Male" ~ 0,
                         Sex == "Female" ~ 1,
                         Sex == "" ~ NA),
  Skintone_num = as.numeric(SkinTone),
  Income = as.factor(Income),
  Race = as.factor(Race),
  State = as.factor(State),
  Education = as.factor(Education),
  LatinoTerm = as.factor(TermLatinoHispanic),
  PolKnowl1_PartyPower = case_when(PolKnow1 == "the Republican Party" ~ 1,
                                   PolKnow1 == "-99" ~ -99, 
                                   PolKnow1 == "" ~ -99), ### Setting don't know, refused
  PolKnow2_Speaker = case_when(PolKnowledgeMatrix._1 == "Mike Johnson" ~ 1,
                               PolKnowledgeMatrix._1 == "Mitch McConnell" ~ 0, 
                               PolKnowledgeMatrix._1 == "" ~ -99),
  PolKnow3_Spending = case_when(PolKnowledgeMatrix._2 == "Chuck Schumer" ~ 1,
                                PolKnowledgeMatrix._2 == "Mike Johnson" ~ 0,
                                PolKnowledgeMatrix._2 == "Mitch McConnell" ~ 0,
                                PolKnowledgeMatrix._2 == "Joe Manchin" ~ 0,
                                PolKnowledgeMatrix._2 == "" ~ -99),
  PolKnowIndex = PolKnowl1_PartyPower + PolKnow2_Speaker + PolKnow3_Spending,
  SocDomMatrix1_Equal = case_when(SocDomMatrix_1 == "Strongly disagree" ~ 5,
                                  SocDomMatrix_1 == "Somewhat disagree" ~ 4,
                                  SocDomMatrix_1 == "Neither agree nor disagree" ~ 3,
                                  SocDomMatrix_1 == "Somewhat agree" ~ 2,
                                  SocDomMatrix_1 == "Strongly agree" ~ 1,
                                  SocDomMatrix_1 == "" ~ -99),
  SocDomMatrix2_Chance = case_when(SocDomMatrix_2 == "Strongly disagree" ~ 1,
                                   SocDomMatrix_2 == "Somewhat disagree" ~ 2,
                                   SocDomMatrix_2 == "Neither agree nor disagree" ~ 3,
                                   SocDomMatrix_2 == "Somewhat agree" ~ 4,
                                   SocDomMatrix_2 == "Strongly agree" ~ 5,
                                   SocDomMatrix_2 == "" ~ -99),                                    # Reverse coded --> higher numbers more social dominant
  SocDomMatrix3_Dom = case_when(SocDomMatrix_3 == "Strongly disagree" ~ 5,
                                SocDomMatrix_3 == "Somewhat disagree" ~ 4,
                                SocDomMatrix_3 == "Neither agree nor disagree" ~ 3,
                                SocDomMatrix_3 == "Somewhat agree" ~ 2,
                                SocDomMatrix_3 == "Strongly agree" ~ 1,
                                SocDomMatrix_3 == "" ~ -99),
  SocDom_Index = SocDomMatrix1_Equal + SocDomMatrix2_Chance + SocDomMatrix3_Dom,
  Native = case_when(Birthplace == "Yes" ~ 1,
                     Birthplace == "No" ~ 1,
                     Birthplace == "" ~ -99),
  ParentsBirth = case_when(ParentsBirthplace == "Both were born outside the US" ~ 3, 
                           ParentsBirthplace == "One in the US, one elsewhere" ~ 2,
                           ParentsBirthplace == "Both were born outside the US" ~ 1,
                           ParentsBirthplace == "" ~ -99),
  Spanish = case_when(Language == "English" ~ 3,
                      Language == "English and Spanish are equally used" ~ 2,
                      Language == "Spanish" ~ 1,
                      Language == "" ~ -99),
  Acc = Native + ParentsBirth + Spanish,
  Linked_Fate = as.factor(LinkedFate),
  ImmResent_1 = case_when(ImmResentment_Matrix_1 == "Strongly disagree" ~ 1,     ### ALL ITEMS ARE AGREE/DISAGREE W STATEMENTS "Undoc imms make other [Latinos/Hispanics/etc] look bad"
                          ImmResentment_Matrix_1 == "Somewhat disagree" ~ 2,
                          ImmResentment_Matrix_1 == "Neither agree or agree" ~ 3,
                          ImmResentment_Matrix_1 == "Somewhat agree" ~ 4,
                          ImmResentment_Matrix_1 == "Strongly agree" ~ 5,
                          ImmResentment_Matrix_1 == "" ~ -99),
  ImmResent_2 = case_when(ImmResentment_Matrix_2 == "Strongly disagree" ~ 1,    ### Previous generations of immigrants were able 
                          ImmResentment_Matrix_2 == "Somewhat disagree" ~ 2,    ## to become successful without relying on government welfare benefits - new immigrants should do the same.
                          ImmResentment_Matrix_2 == "Neither agree or agree" ~ 3,
                          ImmResentment_Matrix_2 == "Somewhat agree" ~ 4,
                          ImmResentment_Matrix_2 == "Strongly agree" ~ 5,
                          ImmResentment_Matrix_2 == "" ~ -99),
  ImmResent_3 = case_when(ImmResentment_Matrix_3 == "Strongly disagree" ~ 1,    ### There is no excuse for breaking the law and entering the US illegally.
                          ImmResentment_Matrix_3 == "Somewhat disagree" ~ 2,    
                          ImmResentment_Matrix_3 == "Neither agree or agree" ~ 3,
                          ImmResentment_Matrix_3 == "Somewhat agree" ~ 4,
                          ImmResentment_Matrix_3 == "Strongly agree" ~ 5,
                          ImmResentment_Matrix_3 == "" ~ -99),
  ImmResent_4 = case_when(ImmResentment_Matrix_4 == "Strongly disagree" ~ 1,    ### Lats/Hisps would be treated better in the US if immigrants would try harder to learn
                          ImmResentment_Matrix_4 == "Somewhat disagree" ~ 2,    ##  English and adopt US customs like other ethnic groups have done.
                          ImmResentment_Matrix_4 == "Neither agree or agree" ~ 3,
                          ImmResentment_Matrix_4 == "Somewhat agree" ~ 4,
                          ImmResentment_Matrix_4 == "Strongly agree" ~ 5,
                          ImmResentment_Matrix_4 == "" ~ -99),
  ImmResent_Index = ImmResent_1 + ImmResent_2 + ImmResent_3 + ImmResent_4,
  Voted = as.factor(Voted),
  VotedFor = as.factor(VotedFor),
  Party = case_when(Party3 == "Democrat" & Party7pt == "Strong" ~ "Strong Democrat",
                    Party3 == "Democrat" & Party7pt == "Not so strong" ~ "Not so strong Democrat",
                    Party3 == "Republican" & Party7pt == "Strong" ~ "Strong Republican",
                    Party3 == "Republican" & Party7pt == "Not so strong" ~ "Not so strong Republican",
                    Party3 == "Something else" & !is.na(Party3_3_TEXT) &
                      (Party3_3_TEXT == "Independent" | grepl("Independent", Party3_3_TEXT)) & 
                      IndParty == "Closer to the Democratic Party" ~ "Lean Democrat",
                    Party3 == "Something else" & !is.na(Party3_3_TEXT) & 
                      (Party3_3_TEXT == "Independent" | grepl("Independent", Party3_3_TEXT)) & 
                      IndParty == "Closer to the Republican Party" ~ "Lean Republican",
                    Party3 == "Something else" & !is.na(Party3_3_TEXT) &
                      (Party3_3_TEXT == "Independent" | grepl("Independent", Party3_3_TEXT)) & 
                      IndParty == "Neither" ~ "Independent",
                    Party3 == "Something else" ~ "Other",
                    TRUE ~ NA_character_),
  IntragroupDistinct = as.factor(IntragroupDistinct),
  Relief = as.numeric(Emotions_1),                                              ## Emotions -- 0:10, none to a lot
  Anger = as.numeric(Emotions_2),
  Pride = as.numeric(Emotions_9),
  Fear = as.numeric(Emotions_10),
  Joy = as.numeric(Emotions_11),
  Shame = as.numeric(Emotions_12),
  ManipCheck1 = as.factor(ManipCheck1),
  TreatmentCondition = as.factor(FL_12_DO),
  ManipCheck_Verif = ifelse(TreatmentCondition == "Control_Policy" & ManipCheck1 == "State symbols", 1,
                            ifelse((TreatmentCondition %in% c("Treatment-Hypothetical_Anti", "Treatment-Hypothetical_Pro")) & ManipCheck1 == "Immigration", 2,
                                   ifelse((TreatmentCondition %in% c("Treatment-Hypothetical_Anti", "Treatment-Hypothetical_Pro")) & ManipCheck1 == "Healthcare", 3,
                                          0))),
  HypPolicy_Attitudes = case_when(ImmAttitudeHyp == "Strongly oppose" ~ 1,
                                  ImmAttitudeHyp == "Somewhat oppose" ~ 2,
                                  ImmAttitudeHyp == "Neither support nor oppose" ~ 3,
                                  ImmAttitudeHyp == "Somewhat support" ~ 4,
                                  ImmAttitudeHyp == "Strongly support" ~ 5),
  HypPolicy_Likely = case_when(LikelyToHappen == "Not at all likely" ~ 1,
                               LikelyToHappen == "Somewhat unlikely" ~ 2,
                               LikelyToHappen == "Somewhat likely" ~ 3,
                               LikelyToHappen == "Very likely" ~ 4),
  HypAffectLatino_Community = case_when(AffectLatinoComm == "It would negatively affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 5,
                                        AffectLatinoComm == "It would somewhat negatively affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 4,
                                        AffectLatinoComm == "It would not affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 3,
                                        AffectLatinoComm == "It would somewhat positively affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 2,
                                        AffectLatinoComm == "It would positively affect ${q://QID116/ChoiceGroup/SelectedChoices}s" ~ 1),
  HypAffectImm_Community = case_when(AffectImmComm == "It would negatively affect immigrants" ~ 5,
                                     AffectImmComm == "It would somewhat negatively affect immigrants" ~ 4,
                                     AffectImmComm == "It would not affect immigrants" ~ 3,
                                     AffectImmComm == "It would somewhat positively affect immigrants" ~ 2,
                                     AffectImmComm == "It would positively affect immigrants" ~ 1),
  HypPersonallyAffect = case_when(PersonallyAffect == "No, the same amount" ~ 2, 
                                  PersonallyAffect == "No, a little less" ~ 1, 
                                  PersonallyAffect == "Yes, a little more" ~ 3,
                                  PersonallyAffect == "Yes, much more" ~ 4,
                                  PersonallyAffect == "Don't know" ~ 0),  ### Don't know ---> 0, could be informative a true IDK
  GroupDiscImms_Perc = case_when(GroupDiscImms == "None at all" ~ 1,            ### How much disc is there against imms? none said none at all --> might mess up the code here 
                                 GroupDiscImms == "A little" ~ 2,
                                 GroupDiscImms == "Some" ~ 3, 
                                 GroupDiscImms == "A lot" ~ 4),
  GroupDiscLatinos_Perc = case_when(GroupDiscLatinos == "None at all" ~ 1,         # also none said none at all
                                    GroupDiscLatinos == "A little" ~ 2,
                                    GroupDiscLatinos == "Some" ~ 3, 
                                    GroupDiscLatinos == "A lot" ~ 4), 
  Disc_Ethnicity = ifelse(str_detect(PersDisc, "Yes, for my ethnicity"), 1, 0),
  Disc_Immigrant = ifelse(str_detect(PersDisc, "Yes, for being an immigrant"), 1, 0),
  Disc_Accent = ifelse(str_detect(PersDisc, "Yes, for having an accent"), 1, 0),
  Disc_Total = Disc_Ethnicity + Disc_Immigrant + Disc_Accent,
  Discriminated = ifelse(is.na(PersDisc), NA, Disc_Total),                       ## distinguishing between 0s and NAs
  ImmsMajSourceCrime = case_when(StigmaImmMatrix_1 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                 StigmaImmMatrix_1 == "Somewhat disagree" ~ 2,
                                 StigmaImmMatrix_1 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                 StigmaImmMatrix_1 == "Somewhat agree" ~ 4,
                                 StigmaImmMatrix_1 == "Strongly agree" ~ 5),
  PoliticiansTalkNegImm = case_when(StigmaImmMatrix_2 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                    StigmaImmMatrix_2 == "Somewhat disagree" ~ 2,
                                    StigmaImmMatrix_2 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                    StigmaImmMatrix_2 == "Somewhat agree" ~ 4,
                                    StigmaImmMatrix_2 == "Strongly agree" ~ 5),
  PoliciesImmUnfair = case_when(StigmaImmMatrix_3 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                StigmaImmMatrix_3 == "Somewhat disagree" ~ 2,
                                StigmaImmMatrix_3 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                StigmaImmMatrix_3 == "Somewhat agree" ~ 4,
                                StigmaImmMatrix_3 == "Strongly agree" ~ 5),
  LatinosMajSourceCrime = case_when(StigmaLatinoMatrix_1 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                    StigmaLatinoMatrix_1 == "Somewhat disagree" ~ 2,
                                    StigmaLatinoMatrix_1 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                    StigmaLatinoMatrix_1 == "Somewhat agree" ~ 4,
                                    StigmaLatinoMatrix_1 == "Strongly agree" ~ 5),
  PoliticiansTalkNegLatinos = case_when(StigmaLatinoMatrix_2 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                        StigmaLatinoMatrix_2 == "Somewhat disagree" ~ 2,
                                        StigmaLatinoMatrix_2 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                        StigmaLatinoMatrix_2 == "Somewhat agree" ~ 4,
                                        StigmaLatinoMatrix_2 == "Strongly agree" ~ 5),
  PoliciesLatinosUnfair = case_when(StigmaLatinoMatrix_3 == "Strongly disagree" ~ 1,  ### higher #s --> higher stigma
                                    StigmaLatinoMatrix_3 == "Somewhat disagree" ~ 2,
                                    StigmaLatinoMatrix_3 == "Neither agree or agree" ~ 3, ## --> fixed in qualtrics
                                    StigmaLatinoMatrix_3 == "Somewhat agree" ~ 4,
                                    StigmaLatinoMatrix_3 == "Strongly agree" ~ 5),
  ExternalPolEfficacy1 = case_when(IntExtEffPost._1 == "Strongly agree" ~ 1, ### "Public officials don't care about what people like me think." --> higher #s --> higher efficacy
                                   IntExtEffPost._1 == "Somewhat agree" ~ 2,
                                   IntExtEffPost._1 == "Neither agree or agree" ~ 3, ## --> FIX!
                                   IntExtEffPost._1 == "Somewhat disagree" ~ 4,
                                   IntExtEffPost._1 == "Strongly disagree" ~ 5),
  InternalPolEfficacy1 = case_when(IntExtEffPost._2 == "Strongly agree" ~ 1, ### "Sometimes, politics seem so complicated that a person like me can’t really understand what’s going on in government." --> higher #s --> higher efficacy
                                   IntExtEffPost._2 == "Somewhat agree" ~ 2,
                                   IntExtEffPost._2 == "Neither agree or agree" ~ 3, ## --> FIX!
                                   IntExtEffPost._2 == "Somewhat disagree" ~ 4,
                                   IntExtEffPost._2 == "Strongly disagree" ~ 5),
  GovernmentResponsive = case_when(IntExtEffPost._3 == "Strongly disagree" ~ 1, ### "My government is responsive to what the people in my country want." --> higher #s --> higher efficacy
                                   IntExtEffPost._3 == "Somewhat disagree" ~ 2,
                                   IntExtEffPost._3 == "Neither agree or agree" ~ 3, ## --> FIX!
                                   IntExtEffPost._3 == "Somewhat agree" ~ 4,
                                   IntExtEffPost._3 == "Strongly agree" ~ 5),
  InternalPolEfficacy2 = case_when(IntExtEffPost._3 == "Strongly disagree" ~ 1, ### "I feel that I have a pretty good understanding of the important political issues debated by the government." --> higher #s --> higher efficacy
                                   IntExtEffPost._3 == "Somewhat disagree" ~ 2,
                                   IntExtEffPost._3 == "Neither agree or agree" ~ 3, ## --> FIX!
                                   IntExtEffPost._3 == "Somewhat agree" ~ 4,
                                   IntExtEffPost._3 == "Strongly agree" ~ 5),
  Belonging_State = as.numeric(BelongingPost_1),                                # 1 - 10 (not at all - a lot)
  Belonging_US = as.numeric(BelongingPost_2),                                   # 1 - 10 (not at all - a lot)
  Belong_State_Open = BelongText1,                                              # Free text answers to why they feel they do / do not belong in their state
  BorderWall = case_when(BorderPoliciesMatrix_1 == "Strongly oppose" ~ 1,       ## higher numbers --> higher support for a border wall
                         BorderPoliciesMatrix_1 == "Somewhat oppose" ~ 2,
                         BorderPoliciesMatrix_1 == "Neither favor or oppose" ~ 3, 
                         BorderPoliciesMatrix_1 == "Somewhat favor" ~ 4,
                         BorderPoliciesMatrix_1 == "Strongly favor" ~ 5),
  DeportAllUndocu = case_when(BorderPoliciesMatrix_2 == "Strongly oppose" ~ 1,       ## higher numbers --> higher support for a border wall
                              BorderPoliciesMatrix_2 == "Somewhat oppose" ~ 2,
                              BorderPoliciesMatrix_2 == "Neither favor or oppose" ~ 3, 
                              BorderPoliciesMatrix_2 == "Somewhat favor" ~ 4,
                              BorderPoliciesMatrix_2 == "Strongly favor" ~ 5),
  TroopsBorder = case_when(BorderPoliciesMatrix_3 == "Strongly oppose" ~ 1,       ## higher numbers --> higher support for a troops at border
                           BorderPoliciesMatrix_3 == "Somewhat oppose" ~ 2,
                           BorderPoliciesMatrix_3 == "Neither favor or oppose" ~ 3,
                           BorderPoliciesMatrix_3 == "Somewhat favor" ~ 4,
                           BorderPoliciesMatrix_3 == "Strongly favor" ~ 5),
  DecreaseBorderSecSpending = case_when(BorderPoliciesMatrix_4 == "Strongly favor" ~ 1,       ## lower #s --> more support for less border security spending
                                        BorderPoliciesMatrix_4 == "Somewhat favor" ~ 2,
                                        BorderPoliciesMatrix_4 == "Neither favor or oppose" ~ 3, 
                                        BorderPoliciesMatrix_4 == "Somewhat oppose" ~ 4,
                                        BorderPoliciesMatrix_4 == "Strongly oppose" ~ 5),
  ProhibitRaidsPlaces = case_when(BorderPoliciesMatrix_5 == "Strongly favor" ~ 1,       ## Places: schools, hospitals, places of worship --> higher #s --> more anti-imm
                                  BorderPoliciesMatrix_5 == "Somewhat favor" ~ 2,
                                  BorderPoliciesMatrix_5 == "Neither favor or oppose" ~ 3, 
                                  BorderPoliciesMatrix_5 == "Somewhat oppose" ~ 4,
                                  BorderPoliciesMatrix_5 == "Strongly oppose" ~ 5),
  BirthrightCitizenship = case_when(BorderPoliciesMatrix_6 == "Strongly favor" ~ 1,   ### higher #s --> more anti-imm
                                    BorderPoliciesMatrix_6 == "Somewhat favor" ~ 2,
                                    BorderPoliciesMatrix_6 == "Neither favor or oppose" ~ 3, 
                                    BorderPoliciesMatrix_6 == "Somewhat oppose" ~ 4,
                                    BorderPoliciesMatrix_6 == "Strongly oppose" ~ 5),
  Participation_Information_NoCue = case_when(Participation1 == "No" ~ 0, 
                                              Participation1 == "Yes" ~ 1),
  Participation_Information_LatinoCue = case_when(Participation2 == "No" ~ 0, 
                                                  Participation2 == "Yes" ~ 1),
  Participation_Response = case_when(
    !is.na(Participation1) ~ ifelse(Participation1 == "Yes", 1, 0),
    !is.na(Participation2) ~ ifelse(Participation2 == "Yes", 1, 0),
    TRUE ~ NA_real_
  ),
  Part_NoCue_Text = OpenEnd,
  Part_LatinoCue_Text = OpenEndVers2,
  ParticipationVersion = as.factor(FL_51_DO),
  Order_Imm_Lat_Stigma = as.factor(FL_72_DO),
  StartDateTime = as.POSIXct(StartDate, format = "%m/%d/%y %H:%M"),
  EndDateTime = as.POSIXct(EndDate, format = "%m/%d/%y %H:%M"),
  Duration_Minutes = as.numeric(difftime(EndDateTime, StartDateTime, units = "mins"))
)

### Writing CSV  ---> SAVING AS CLEAN DATASET

write.csv(pilot_2_clean, "pilot_2_clean.csv")

