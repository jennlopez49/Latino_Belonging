# ============================================================
# Latino Political Attitudes and Behavior - Data Cleaning
# ============================================================

INSERT_PATH <- "Latino Political Attitudes and Behavior - Cleaned_March 21, 2026_23.02.csv"

library(tidyverse)

df <- read_csv(INSERT_PATH)

# ============================================================
# REMOVE NON-CONSENT & METADATA ROWS
# ============================================================
# Qualtrics exports 3 header rows; keep only real responses
df_short <- df %>%
  slice(-(1:2)) %>%                          # drop Qualtrics label rows
  rename_with(~ str_replace_all(., " ", "_")) %>%
  filter(IRBConsent == 4)  %>%                  # keep only those who consented (1=consent, else drop)
  filter(AC1Fail == 0, AC2Fail == 0) %>% 
  filter(Latino != 1) ### those who said no to being Latino

# ============================================================
# RECODE VARIABLES
# ============================================================

df_short <- df_short %>% mutate(
  
  # --- ID ---
  Q134 = as.character(Q134),
  
  # --- Latino Origin ---
  # 1=No, 2=Mexican, 3=Cuban, 4=Puerto Rican, 5=Other
  Latino = as.numeric(Latino),
  
  # --- State (numeric 1-52, keep as-is or as factor) ---
  State = as.integer(State),
  BorderState = as.integer(State %in% c(3, 5, 32, 44)),            ### AZ, CA, NM, TX
  
  # --- Birth Year (numeric) ---
  BirthYear = as.integer(BirthYear),
  
  # --- Sex ---
  # 1=Male, 2=Female
  Sex = as.numeric(Sex),
  
  # --- Race (multi-select: one column per choice in Qualtrics export) ---
  # Qualtrics splits checkboxes into Race_1 ... Race_7
  # 1=White, 2=Black, 3=AmIndian, 4=Asian, 5=NativeHawaiian, 6=Other, 7=PrefNotSay
  across(starts_with("Race_"), ~ as.integer(!is.na(.) & . != 0)),
  
  # --- Skin Tone (1-10) ---
  SkinTone = as.integer(SkinTone),
  
  # --- Attention Check 1 (should select 1 AND 4) ---
  # Qualtrics stores multi-select as comma-separated or split columns
  # Flag as passed if both "1" and "4" are selected
  AttentionCheck1_pass = case_when(
    str_detect(as.character(AttentionCheck1), "1") &
      str_detect(as.character(AttentionCheck1), "4") ~ 1,
    TRUE ~ 0L),
  
  # --- Education ---
  # 1=<HS, 2=HS/GED, 3=Some college, 4=Associates, 5=Bachelor's, 6=Grad, 7=PrefNotSay
  Education = as.numeric(Education),
  
  # --- Preferred Latino/Hispanic Term ---
  # 1=Latino/a, 2=Hispanic, 3=Latinx
  TermLatinoHispanic = as.numeric(TermLatinoHispanic),
  
  # --- Political Knowledge 1 (Senate majority) ---
  # Correct answer: 2 = Republican Party
  PolKnow1 = as.integer(PolKnow1),
  PolKnow1_correct = ifelse(PolKnow1 == 2, 1, 0),
  
  # --- Political Knowledge Matrix ---
  # Rows: 1=Speaker of House (correct=3 Mike Johnson),
  #       2=Chief Justice (correct=4 John Roberts),
  #       3=Senate minority leader (correct=2 Chuck Schumer)
  PolKnowledgeMatrix_1 = as.integer(PolKnowledgeMatrix__1),
  PolKnowledgeMatrix_2 = as.integer(PolKnowledgeMatrix__2),
  PolKnowledgeMatrix_3 = as.integer(PolKnowledgeMatrix__3),
  PolKnow2_correct = if_else(PolKnowledgeMatrix_1 == 3, 1, 0),
  PolKnow3_correct = if_else(PolKnowledgeMatrix_2 == 4, 1, 0),
  PolKnow4_correct = if_else(PolKnowledgeMatrix_3 == 2, 1, 0),
  
  # Additive full political knowledge index (0-4)
  PolKnow_index_full = PolKnow1_correct + PolKnow2_correct + PolKnow3_correct + PolKnow4_correct,
  # Additive Pol Know Index -- excludes PolKnow1 (155 NAs)
  PolKnow_index = PolKnow2_correct + PolKnow3_correct + PolKnow4_correct,
  # --- Attention Check 2 (should select Fox News=1 AND NBC=2) ---
  AttentionCheck2_pass = case_when(
    str_detect(as.character(AttentionCheck2), "1") &
      str_detect(as.character(AttentionCheck2), "2") ~ 1,
    TRUE ~ 0),
  
  # --- Social Dominance Orientation Matrix (6-10 scale) ---
  # Rows: 1=equality (reverse-coded), 2=unequal chances, 3=dominance
  # 6=Strongly disagree, 7=Somewhat disagree, 8=Neither, 9=Somewhat agree, 10=Strongly agree
  SocDomMatrix_1 = as.integer(SocDomMatrix_1),  # equality - reverse code
  SocDomMatrix_2 = as.integer(SocDomMatrix_2),
  SocDomMatrix_3 = as.integer(SocDomMatrix_3),
  # Reverse code item 1 (pro-equality): 6->10, 10->6
  SocDomMatrix_1_r = (16 - SocDomMatrix_1),
  # SDO scale mean (higher = more social dominance)
  SDO_mean = rowMeans(cbind(SocDomMatrix_1_r, SocDomMatrix_2, SocDomMatrix_3), na.rm = TRUE),
  
  # --- Birthplace ---
  # 1=No (not born in US), 2=Yes
  Birthplace = factor(Birthplace, levels = 1:2, labels = c("Born outside US", "Born in US")),
  
  # --- Parents' Birthplace ---
  # 3=Both outside US, 4=One in US one elsewhere, 5=Both in US, 6=Don't know
  ParentsBirthplace = as.numeric(ParentsBirthplace),
  
  # --- Grandparents' Birthplace ---
  # 1=All 4 in US, 2 = 3 in US / 1 out, 3 = 2 in US / 2 Out, 4 =1 in US / 3 Out, 5=All 4 outside US, 6=Don't know
  Grandparents_Born = as.numeric(Grandparents_Born),
  
  # --- Acculturation Measure ---
  # Lower Numbers ---> Less Acculturation
  Parents_Birth = case_when(ParentsBirthplace == "3" ~ 1,
                            ParentsBirthplace == "4" ~ 2,
                            ParentsBirthplace == "5" ~ 3,
                            TRUE   ~ NA_real_
  ),
  Grandparents_Birth = case_when(Grandparents_Born == "1" ~ 5, 
                                 Grandparents_Born == "2" ~ 4,
                                 Grandparents_Born == "3" ~ 3,
                                 Grandparents_Born == "4" ~ 2,
                                 Grandparents_Born == "5" ~ 1,
                                 TRUE ~ NA_real_),
  Acculturation = Parents_Birth + Grandparents_Birth,
  # --- Language at home ---
  # 1=English, 2=Spanish, 3=Both equally, 4=Other
  Language = as.numeric(Language),
  
  # Expanisve Acculturation Measure ----- No Spanish 
  Span_Acc = case_when(Language == 1 ~ 3,
                       Language == 3 ~ 2, 
                       Language == 2 ~ 1,
                       TRUE ~ NA_real_),
  Full_Accult = Acculturation + Span_Acc,
  # --- Linked Fate (9-12 scale) ---
  # 9=A lot, 10=Some, 11=Not very much, 12=Not at all
  LinkedFate = as.integer(LinkedFate),
  # Recode to 1-4 (higher = stronger linked fate)
  LinkedFate_r = case_when(
    LinkedFate == 9  ~ 4,
    LinkedFate == 10 ~ 3,
    LinkedFate == 11 ~ 2,
    LinkedFate == 12 ~ 1),
  
  # --- Immigrant Resentment Matrix (1-5 scale) ---
  # 1=Strongly disagree ... 5=Strongly agree (higher = more resentment)
  ImmResentment_Matrix_1 = as.integer(ImmResentment_Matrix_1),
  ImmResentment_Matrix_2 = as.integer(ImmResentment_Matrix_2),
  ImmResentment_Matrix_3 = as.integer(ImmResentment_Matrix_3),
  ImmResentment_Matrix_4 = as.integer(ImmResentment_Matrix_4),
  ImmResentment_mean = rowMeans(cbind(
    ImmResentment_Matrix_1, ImmResentment_Matrix_2,
    ImmResentment_Matrix_3, ImmResentment_Matrix_4), na.rm = TRUE),
  
  # --- Voted in 2024 ---
  # 1=Did not vote, 2=Thought about it, 3=Usually vote but didn't, 4=Definitely voted
  Voted = as.numeric(Voted),
  Voted_Bin = case_when(Voted == 1 ~ 0,
                        Voted == 2 ~ 0,
                        Voted == 3 ~ 0,
                        Voted == 4 ~ 1),
  
  # --- Voted For (conditional on Voted==4) ---
  # 1=Kamala Harris, 2=Donald Trump, 3=Other
  VotedFor = as.numeric(VotedFor),
  
  # --- Party ID (3-category) ---
  # 1=Republican, 2=Democrat, 3=Something else
  Party3 = as.numeric(Party3),
  
  # --- Party Strength (conditional on Rep or Dem) ---
  # 1=Strong, 2=Not so strong
  Party7pt = as.numeric(Party7pt),
  
  # --- Independent Party Lean (conditional on "Something else") ---
  # 1=Closer to Dem, 2=Closer to Rep, 3=Neither
  IndParty = as.numeric(IndParty),
  
  # 7-point party ID scale  --- --- --- --- --- ---
  # Strong Dem (1), Lean Dem (2), Neither (3), Lean Rep (4), Strong Rep (5)
  PartyID_5pt = case_when(
    # Strong identifiers
    Party3 == 2 & Party7pt == 1 ~ 1,  # Strong Democrat
    # Weak identifiers
    Party3 == 2 & Party7pt == 2 ~ 2,  # Not so strong Democrat
    # Independents who lean
    Party3 == 3 & IndParty == 1 ~ 2,  # Closer to Democrat
    # True independents
    Party3 == 3 & IndParty == 3 ~ 3,  # Neither
    # Independents who lean Republican
    Party3 == 3 & IndParty == 2 ~ 4,  # Closer to Republican
    # Weak Republican
    Party3 == 1 & Party7pt == 2 ~ 4,  # Not so strong Republican
    # Strong Republican
    Party3 == 1 & Party7pt == 1 ~ 5,  # Strong Republican
    TRUE ~ NA_integer_
  ),
  
  PartyID_5pt_label = factor(PartyID_5pt,
                             levels = 1:5,
                             labels = c("Strong Dem", "Lean Dem", "Neither",
                                        "Lean Rep", "Strong Rep")),
  # --- Intragroup Distinction (13-17 scale) ---
  # 13=Never, 14=Sometimes, 15=About half, 16=Most of time, 17=Always
  IntragroupDistinct = as.integer(IntragroupDistinct),
  # Recode to 1-5
  IntragroupDistinct_r = IntragroupDistinct - 12L,
  
  # --- Treatment Assignment ---
  # Derive from which block was filled: Control / Anti / Pro
  # Qualtrics typically creates a variable or you can infer from non-NA
  # Adjust column names below to match your export
  Treatment = case_when(FL_12_DO == "Control_Policy" ~ "Control",
                        FL_12_DO == "Treatment-Hypothetical_Pro" ~ "Pro",
                        FL_12_DO == "Treatment-Hypothetical_Anti" ~ "Anti"),
  
  # --- Emotions (0-10 sliders) ---
  Emotions_Relief = as.numeric(Emotions_1),
  Emotions_Anger  = as.numeric(Emotions_2),
  Emotions_Pride  = as.numeric(Emotions_9),
  Emotions_Fear   = as.numeric(Emotions_10),
  Emotions_Joy    = as.numeric(Emotions_11),
  Emotions_Shame  = as.numeric(Emotions_12),
  
  # --- Manipulation Check ---
  # 1=Immigration, 2=State symbols, 3=Gun control, 4=Healthcare, 5=None
  ManipCheck1 = as.integer(ManipCheck1),
  ManipCheck1_result = case_when(
    # Pass as intended - correct answer for their condition
    Treatment == "Control" & ManipCheck1 == 2 ~ 1,  # State symbols
    Treatment %in% c("Anti", "Pro") & ManipCheck1 == 1 ~ 1,  # Immigration
    
    # Misinterpret - plausible alternative frame
    # Anti/Pro respondents who categorized it as healthcare
    # (hospital framing activated healthcare schema instead of immigration)
    Treatment %in% c("Anti", "Pro") & ManipCheck1 == 4 ~ 2,  # Healthcare
    # Control respondents who thought it was about state symbols but picked wrong
    # (no real equivalent misinterpretation for Control)
    
    # Fail completely - selected something unrelated
    TRUE ~ 3L  # Gun control, None of these, or wrong category entirely
  ),
  
  # --- Immigration Attitude (outcome; 6-10 scale) ---
  # 6=Strongly oppose ... 10=Strongly support
  ImmAttitudeHyp = as.integer(ImmAttitudeHyp),
  # Recode to 1-5
  ImmAttitudeHyp_r = ImmAttitudeHyp - 5L,
  
  # --- Likelihood of policy passing ---
  # 1=Very likely, 2=Somewhat likely, 3=Somewhat unlikely, 4=Not at all likely
  LikelyToHappen = as.numeric(LikelyToHappen),
  
  # --- Affect on Latino Community (16-20 scale) ---
  # 16=Negatively ... 20=Positively
  AffectLatinoComm = as.integer(AffectLatinoComm),
  AffectLatinoComm_r = AffectLatinoComm - 15L,  # 1-5
  
  # --- Affect on Immigrant Community (16-20 scale) ---
  AffectImmComm = as.integer(AffectImmComm),
  AffectImmComm_r = AffectImmComm - 15L,
  
  # --- Personally Affected, More discrimination? ---
  # 1=Yes much more ... 5=No a lot less, 6=Don't know
  PersonallyAffect = as.numeric(PersonallyAffect),
  
  # --- Group Discrimination: Immigrants (1-5) ---
  # 1=A lot, 2=Some, 3=A little, 4=None at all, 5=Don't know
  GroupDiscImms = as.numeric(GroupDiscImms),
  
  # --- Group Discrimination: Latinos (1-5) ---
  GroupDiscLatinos = as.numeric(GroupDiscLatinos),
  
  # --- Personal Discrimination (multi-select checkboxes) ---
  # PersDisc_1=ethnicity, _2=immigrant, _3=accent, _4=none
  across(starts_with("PersDisc_"), ~ as.integer(!is.na(.) & . != 0)),
  
  # --- Stigma: Immigrants Matrix (1-5) ---
  StigmaImmMatrix_1 = as.integer(StigmaImmMatrix_1),
  StigmaImmMatrix_2 = as.integer(StigmaImmMatrix_2),
  StigmaImmMatrix_5 = as.integer(StigmaImmMatrix_3),
  StigmaImm_mean = rowMeans(cbind(StigmaImmMatrix_1, StigmaImmMatrix_2, StigmaImmMatrix_5), na.rm = TRUE),
  
  # --- Stigma: Latinos Matrix (1-5) ---
  StigmaLatinoMatrix_1 = as.integer(StigmaLatinoMatrix_1),
  StigmaLatinoMatrix_2 = as.integer(StigmaLatinoMatrix_2),
  StigmaLatinoMatrix_5 = as.integer(StigmaLatinoMatrix_3),
  StigmaLatino_mean = rowMeans(cbind(StigmaLatinoMatrix_1, StigmaLatinoMatrix_2, StigmaLatinoMatrix_5), na.rm = TRUE),
  
  # --- Internal/External Efficacy Matrix (1-5) ---
  # Items: 1=officials don't care (external, reverse), 5=politics complicated (internal, reverse)
  #        2=govt responsive (external), 6=good understanding (internal)
  IntExtEffPost_1 = as.integer(IntExtEffPost__1),
  IntExtEffPost_2 = as.integer(IntExtEffPost__2),
  IntExtEffPost_5 = as.integer(IntExtEffPost__3),
  IntExtEffPost_6 = as.integer(IntExtEffPost__4),
  # Reverse code negative items (1 and 5): 1->5, 5->1
  IntExtEffPost_1_r = (6L - as.integer(IntExtEffPost_1)),
  IntExtEffPost_5_r = (6L - as.integer(IntExtEffPost_5)),
  ExtEfficacy_mean = rowMeans(cbind(IntExtEffPost_1_r, IntExtEffPost_2), na.rm = TRUE),
  IntEfficacy_mean = rowMeans(cbind(IntExtEffPost_5_r, IntExtEffPost_6), na.rm = TRUE),
  
  # --- Belonging: Internal (0-10 sliders) ---
  BelongingPost_state = as.numeric(BelongingPost_1),
  BelongingPost_US    = as.numeric(BelongingPost_2),
  
  # --- Belonging: External (0-10 sliders) ---
  BelongExternal_state = as.numeric(BelongExternal_1),
  BelongExternal_US    = as.numeric(BelongExternal_2),
  
  # --- Border Policy Matrix (1-5; 1=Strongly favor, 5=Strongly oppose) ---
  # Items: 1=border wall, 2=decrease border spending, 3=prohibit raids in schools/hospitals,
  #        4=legal status for undocumented, 6=prolonged detention, 9=restrict force,
  #        10=prohibit deportation to 3rd countries, 12=detain citizens opposing raids
  across(starts_with("BorderPoliciesMatrix_"), as.integer),
  BorderWall = case_when(BorderPoliciesMatrix_1 == 1 ~ 5,                        ## Recoding --- 5 --> more restrictive
                        BorderPoliciesMatrix_1 == 2 ~ 4,
                        BorderPoliciesMatrix_1 == 3 ~ 3,
                        BorderPoliciesMatrix_1 == 4 ~ 2,
                        BorderPoliciesMatrix_1 == 5 ~ 1),
  BorderSecurity = BorderPoliciesMatrix_2,                                     ## Already oppose --> restrictive
  Prohibit_Raids = BorderPoliciesMatrix_3,                                     ## Already oppose --> restrictive
  Pathway_Citizenship = case_when(BorderPoliciesMatrix_4 == 1 ~ 5,            ## Recoding so higher numbers more support 
                                  BorderPoliciesMatrix_4 == 2 ~ 4,
                                  BorderPoliciesMatrix_4 == 3 ~ 3,
                                  BorderPoliciesMatrix_4 == 4 ~ 2,
                                  BorderPoliciesMatrix_4 == 5 ~ 1),
  ProlongedDetention = case_when(BorderPoliciesMatrix_5 == 1 ~ 5,               ## Recoding --- 5 --> more restrictive
                                 BorderPoliciesMatrix_5 == 2 ~ 4,
                                 BorderPoliciesMatrix_5 == 3 ~ 3,
                                 BorderPoliciesMatrix_5 == 4 ~ 2,
                                 BorderPoliciesMatrix_5 == 5 ~ 1),
  UseForceICE = case_when(BorderPoliciesMatrix_6 == 1 ~ 5,               ## Recoding --- 5 --> more restrictive
                          BorderPoliciesMatrix_6 == 2 ~ 4,
                          BorderPoliciesMatrix_6 == 3 ~ 3,
                          BorderPoliciesMatrix_6 == 4 ~ 2,
                          BorderPoliciesMatrix_6 == 5 ~ 1),
  ThirdCountryDept = case_when(BorderPoliciesMatrix_7 == 1 ~ 5,               ## Recoding --- 5 --> more restrictive
                               BorderPoliciesMatrix_7 == 2 ~ 4,
                               BorderPoliciesMatrix_7 == 3 ~ 3,
                               BorderPoliciesMatrix_7 == 4 ~ 2,
                               BorderPoliciesMatrix_7 == 5 ~ 1),
  DetainUSCits = case_when(BorderPoliciesMatrix_8 == 1 ~ 5,               ## Recoding --- 5 --> more restrictive
                           BorderPoliciesMatrix_8 == 2 ~ 4,
                           BorderPoliciesMatrix_8 == 3 ~ 3,
                           BorderPoliciesMatrix_8 == 4 ~ 2,
                           BorderPoliciesMatrix_8 == 5 ~ 1),
  # Border Policy Index - Border Wall, Security, 3rd Country 
  BorderPolicyIndex = BorderWall + BorderSecurity + ThirdCountryDept,
  # Interior Enf Index - Raids, Prolonged Detention, Use of Force, Detain US Cits 
  InteriorPolicyIndex = Prohibit_Raids + ProlongedDetention + UseForceICE + DetainUSCits,
  # --- Participation: CHIRLA (multi-select) ---
  # _1=donate, _2=volunteer, _3=protest, _4=all, _5=none
  across(starts_with("ParticipationAlt1_"), ~ as.integer(!is.na(.) & . != 0)),
  
  # --- Income ---
  # 1=<$25k, 2=$25-49k, 3=$50-74k, 4=$75-99k, 5=$100-149k, 6=$150k+, 7=PrefNotSay
  Income = as.numeric(Income)
)

# ============================================================
# COMPUTE AGE FROM BIRTH YEAR
# ============================================================
df_short <- df_short %>%
  mutate(Age = 2024L - BirthYear,
         Treatment = relevel(factor(Treatment), ref = "Control"),
         Anti = as.integer(Treatment == "Anti"),
         Pro  = as.integer(Treatment == "Pro"),
         Treatment_cont = case_when(
           Treatment == "Anti"    ~ -1,
           Treatment == "Control" ~  0,
           Treatment == "Pro"     ~  1,
           TRUE                   ~ NA_integer_
         )
         )

# ============================================================
# FILTER OUT FAILED ATTENTION CHECKS (optional - comment out to retain)
# # ============================================================
# df_clean <- df %>%
#   filter(AttentionCheck1_pass == 1,
#          AttentionCheck2_pass == 1)

# ============================================================
# DROP QUALTRICS METADATA COLUMNS (optional)
# ============================================================
qualtrics_meta <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress",
                    "Duration (in seconds)", "Finished", "RecordedDate",
                    "ResponseId", "RecipientLastName", "RecipientFirstName",
                    "RecipientEmail", "ExternalReference", "LocationLatitude",
                    "LocationLongitude", "DistributionChannel", "UserLanguage")

df_clean <- df_short %>%
  dplyr::select(-any_of(qualtrics_meta))

# ============================================================
# PREVIEW
# ============================================================
glimpse(df_clean)
cat("N after consent filter and attention checks:", nrow(df_clean), "\n")

# ============================================================
# saving
# ============================================================
 write.csv(df_clean, "latino_survey.3.21.clean.csv")

# ============================================================
# Manipulation check pass rates by treatment
# ============================================================
# 
df_clean %>%
  group_by(Treatment) %>%
  summarise(
    n         = n(),
    passed    = sum(ManipCheck1_result == 1),
    pass_rate = round(mean(ManipCheck1_result == 1) * 100, 1)
  )

# Chi-square test
chisq.test(table(df_clean$Treatment, df_clean$ManipCheck1_result))

# Summary table for methods section
df_clean %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    pass_immigration = sum(ManipCheck1_result == 1),
    misinterpret_healthcare = sum(ManipCheck1 == 4 & 
                                    Treatment %in% c("Anti","Pro")),
    fail_other = sum(ManipCheck1_result == 3),
    pass_rate = round(mean(ManipCheck1_result == 1) * 100, 1)
  )

# what did the control group choose? 

df_clean %>%
  filter(Treatment == "Control") %>%
  count(ManipCheck1) %>%
  mutate(answer = case_when(
    ManipCheck1 == 1 ~ "Immigration",
    ManipCheck1 == 2 ~ "State symbols",
    ManipCheck1 == 3 ~ "Gun control",
    ManipCheck1 == 4 ~ "Healthcare",
    ManipCheck1 == 5 ~ "None of these")
    )

##### Quick Desc. Table to Check Balance ----------
df_clean %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    age = mean(Age, na.rm = TRUE),
    female = mean(Sex == 2, na.rm = TRUE),
    education = mean(Education, na.rm = TRUE),
    acculturation = mean(Acculturation, na.rm = TRUE),
    democrat = mean(Party3 == 2, na.rm = TRUE)
  )

### Total N by Treatment Group ----------
table(df_clean$Treatment, useNA = "ifany")

#### Checking the DVs ------
df_clean %>%
  summarise(
    across(c(BelongingPost_state, BelongingPost_US,
             BelongExternal_state, BelongExternal_US,
             Emotions_Anger, Emotions_Fear, Emotions_Relief,
             Emotions_Pride, Emotions_Joy, Emotions_Shame,
             ImmAttitudeHyp_r),
           list(mean = ~mean(., na.rm = TRUE),
                sd   = ~sd(., na.rm = TRUE),
                min  = ~min(., na.rm = TRUE),
                max  = ~max(., na.rm = TRUE)))
  ) %>% pivot_longer(everything())


df_clean %>%
  filter(!is.na(Treatment)) %>%
  group_by(Treatment) %>%
  summarise(across(c(Emotions_Relief, Emotions_Anger, Emotions_Pride,
                     Emotions_Fear, Emotions_Joy, Emotions_Shame),
                   ~round(mean(., na.rm = TRUE), 2)))
### missingness
df_clean %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  filter(value > 0) %>%
  arrange(desc(value))
# Check if PolKnow missingness is random across treatment
df_clean %>%
  group_by(Treatment) %>%
  summarise(polknow_missing = sum(is.na(PolKnow1)),
            pct_missing = round(mean(is.na(PolKnow1)) * 100, 1))

df_clean %>%
  group_by(Treatment) %>%
  summarise(mean_acc = mean(Acculturation, na.rm = TRUE),
            sd_acc   = sd(Acculturation, na.rm = TRUE),
            mean_full_acc = mean(Full_Accult, na.rm = TRUE),
            sd_full_acc = sd(Full_Accult, na.rm = TRUE))
summary(aov(Full_Accult ~ Treatment, data = df_clean %>% filter(!is.na(Treatment))))
# correlation
df_clean %>%
dplyr::select(Acculturation, Emotions_Anger, Emotions_Fear,
       Emotions_Relief, Emotions_Pride, Emotions_Joy,
       Emotions_Shame, BelongingPost_state, BelongingPost_US, BelongExternal_state,
       BelongExternal_US,
       ImmAttitudeHyp_r, StigmaImm_mean, StigmaLatino_mean, PolKnow_index_full,
       PolKnow_index) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)


## political knowledge 

df_clean <- df_clean %>%
  mutate(
    PolKnow_index = rowMeans(cbind(PolKnow1_correct, 
                                   PolKnow2_correct,
                                   PolKnow3_correct, 
                                   PolKnow4_correct), 
                             na.rm = TRUE)
  )

# Check distribution
summary(df_clean$PolKnow_index)
hist(df_clean$PolKnow_index)
