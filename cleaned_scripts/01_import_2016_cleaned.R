# =============================================================================
# Import and clean CMPS 2016
# Primary sample: Latino respondents only
# Secondary sample: Latino + Asian respondents (for immigrant comparison)
# Survey year: 2016
# =============================================================================

source("00_utilities.R")

# --- Load raw CMPS 2016 data --------------------------------------------------
load("~/Desktop/COIi work/Latino_Imm_Enf/Latino_Proj/cmps_2016.rda")

# --- Select relevant variables ------------------------------------------------
cmps_sub_2016 <- da38040.0001 %>%
  dplyr::select(
    # Race / ethnicity
    S2_1, S2_2, S2_3, S2_4, S2_5, S2_6, S2_7, ETHNIC_QUOTA,
    # Geography + nativity
    S4, S6, S7, S10,
    # Voting + party
    S10, C25, C26,
    # Belonging / inclusion
    C107, C108, C109, C110, C111, C112, C113, C114, C115,
    # Discrimination
    A116, A117, C118, C119, C120, C121,
    # Acculturation / generation
    C377, C379,
    # Sociodemographics
    C381, C383, S3,
    # Political variables
    L29, C31, C33, L46, C246, C247, LA250, C251, C252, C256,
    # Linked fate
    C150, C151,
    # Emotions
    C111, C112, C113, C114, C115,
    # Immigration attitudes
    C337, SPLITC337, C141, SPLITC141, SPLITC38,
    # National origin
    C374, C375, C375_6_OTHER,
    # Misc
    C38, C39, C41, BLA190, BLA191, L192, LA193, C194,
    L195_1, L195_2, L195_3, L197, L198,
    L268, L270, L271, L300, L301, LA303, L364, L365, L366,
    C384, C150, C151, C393, C394,
    C390, C23, A134, NAT_WEIGHT,
    C253, LA202_6, S1, L24, C262, C245,
    BLA205, C38
  )

# --- Core variable recoding ---------------------------------------------------
cmps_clean_2016 <- cmps_sub_2016 %>%
  mutate(
    # Nativity (born in US = 1, Puerto Rico = 0.5, another country = 0)
    NativeBorn = case_when(
      S7 == "(1) United States"  ~ 1,
      S7 == "(3) Puerto Rico"    ~ 0.5,
      S7 == "(2) Another country" ~ 0
    ),
    
    # Age (birth year pre-formatted as "19__" in survey instrument)
    BirthYear = as.numeric(paste0("19", str_trim(S6))),
    Age       = 2016 - BirthYear,
    age_sqd   = Age^2,
    
    # State
    State = as.character(S4),
    
    # Voting in 2016
    Voted = case_when(
      S10 == "(1) Yes, I voted"      ~ 1,
      S10 == "(2) No, I did NOT vote" ~ 0
    ),
    
    # Party ID (1 = Republican, 2 = Democrat, 3 = Independent, 4 = Other)
    Party = case_when(
      C25 == "(1) Republican"  ~ 1,
      C25 == "(2) Democrat"    ~ 2,
      C25 == "(3) Independent" ~ 3,
      C25 == "(4) Other party" ~ 4
    ),
    
    # Linked fate (0 = No, 1-3 = strength of yes)
    Linked_Fate = case_when(
      C150 == "(2) No"               ~ 0,
      C151 == "(3) Not very much"    ~ 1,
      C151 == "(2) Some"             ~ 2,
      C151 == "(1) A lot"            ~ 3
    ),
    
    # Gender (1 = Female, 2 = Male; "Other" excluded, n = 18)
    Gender = case_when(
      S3 == "(1) Female" ~ 1,
      S3 == "(2) Male"   ~ 2
    ),
    
    # Education (1 = Grades 1-8, 6 = Post-graduate)
    Education = case_when(
      C381 == "(1) Grades 1 - 8"              ~ 1,
      C381 == "(2) Some High School"           ~ 2,
      C381 == "(3) High School graduate or GED" ~ 3,
      C381 == "(4) Some college, 2-year degree" ~ 4,
      C381 == "(5) 4-year college graduate"    ~ 5,
      C381 == "(6) Post-graduate education"    ~ 6
    ),
    
    # Income (1 = <$20k, 12 = $200k+; 99 = Refused → NA)
    Income = case_when(
      C383 == "(01) Less than $20,000"      ~ 1,
      C383 == "(02) $20,000 to $29,999"     ~ 2,
      C383 == "(03) $30,000 to $39,999"     ~ 3,
      C383 == "(04) $40,000 to $49,999"     ~ 4,
      C383 == "(05) $50,000 to $59,999"     ~ 5,
      C383 == "(06) $60,000 to $69,999"     ~ 6,
      C383 == "(07) $70,000 to $79,999"     ~ 7,
      C383 == "(08) $80,000 to $89,999"     ~ 8,
      C383 == "(09) $90,000 to $99,999"     ~ 9,
      C383 == "(10) $100,000 to $149,999"   ~ 10,
      C383 == "(11) $150,000 to $199,999"   ~ 11,
      C383 == "(12) $200,000 or more"       ~ 12,
      C383 == "(99) Refused"                ~ NA_real_
    ),
    
    # Parental birthplace (0 = both outside US, 3 = both in US, 9 = don't know)
    Parents = case_when(
      str_trim(C377) == "(02) Both parents born in another country"           ~ 0,
      str_trim(C377) == "(03) Both parents born in Puerto Rico"               ~ 1,
      str_trim(C377) == "(04) 1 parent born in U.S. / 1 parent born elsewhere" ~ 2,
      str_trim(C377) == "(01) Both parents born in the U.S."                  ~ 3,
      str_trim(C377) == "(88) Don't know"                                     ~ 9
    ),
    
    # Grandparental birthplace (0 = all 4 outside US, 4 = all 4 in US, 9 = don't know)
    Grandparents = case_when(
      str_trim(C379) == "(05) All 4 grandparents born outside U.S."               ~ 0,
      str_trim(C379) == "(04) 1 grandparents born in U.S. / 3 elsewhere"          ~ 1,
      str_trim(C379) == "(03) 2 grandparents born in U.S. / 2 elsewhere"          ~ 2,
      str_trim(C379) == "(02) 3 grandparents born in U.S. / 1 elsewhere"          ~ 3,
      str_trim(C379) == "(01) All 4 grandparents born in U.S."                    ~ 4,
      str_trim(C379) == "(88) Don't know"                                         ~ 9
    ),
    
    # Generation index: sum of Parents + Grandparents (higher = more US-born ancestry)
    Generation = Parents + Grandparents,
    
    # Generational status (0 = immigrant, 1 = second gen, 2 = third gen+)
    More_Than_SecondGen = case_when(
      NativeBorn == 0              ~ 0,
      NativeBorn == 1 & Parents < 3 ~ 1,
      NativeBorn == 1 & Parents == 3 ~ 2,
      Parents == 9                 ~ NA_real_,
      is.na(Parents) & NativeBorn == 1 ~ NA_real_
    ),
    
    # National origin dummies (reference = Other Latino)
    Mexican = as.integer(S10 == "(12) Mexico"),
    Cuban   = as.integer(S10 == "(06) Cuba"),
    Puerto_Rican = as.integer(S10 == "(17) Puerto Rico"),
    Dominican    = as.integer(S10 == "(07) Dominican Republic"),
    
    # Central American (El Salvador, Guatemala, Honduras, Costa Rica, Nicaragua, Panama)
    Central_American = as.integer(S10 %in% c(
      "(09) El Salvador", "(10) Guatemala", "(11) Honduras",
      "(05) Costa Rica", "(13) Nicaragua", "(14) Panama"
    )),
    
    # South American
    South_American = as.integer(S10 %in% c(
      "(01) Argentina", "(02) Bolivia", "(03) Chile", "(04) Colombia",
      "(08) Ecuador", "(15) Paraguay", "(16) Peru",
      "(18) Uruguay", "(19) Venezuela"
    )),
    
    # Emotions during 2016 election (recoded: 1 = Never, 4 = All the time)
    Angry_Election = case_when(
      C112 == "(1) All the time" ~ 4,
      C112 == "(2) Often"        ~ 3,
      C112 == "(3) Sometimes"    ~ 2,
      C112 == "(4) Never"        ~ 1
    ),
    Fear_Election = case_when(
      C111 == "(1) All the time" ~ 4,
      C111 == "(2) Often"        ~ 3,
      C111 == "(3) Sometimes"    ~ 2,
      C111 == "(4) Never"        ~ 1
    ),
    Hope_Election = case_when(
      C113 == "(1) All the time" ~ 4,
      C113 == "(2) Often"        ~ 3,
      C113 == "(3) Sometimes"    ~ 2,
      C113 == "(4) Never"        ~ 1
    ),
    Pride_Election = case_when(
      C114 == "(1) All the time" ~ 4,
      C114 == "(2) Often"        ~ 3,
      C114 == "(3) Sometimes"    ~ 2,
      C114 == "(4) Never"        ~ 1
    ),
    Sad_Election = case_when(
      C115 == "(1) All the time" ~ 4,
      C115 == "(2) Often"        ~ 3,
      C115 == "(3) Sometimes"    ~ 2,
      C115 == "(4) Never"        ~ 1
    ),
    
    # --- Sense of belonging items ---------------------------------------------
    # Four items split into internal (felt) and external (perceived from others)
    
    # Internal item 1: "How much do you feel you belong in the US?"
    # Recoded: 1 = not at all, 4 = strongly belong
    Belong_US = case_when(
      C107 == "(1) Strongly belong"   ~ 4,
      C107 == "(2) Moderately belong" ~ 3,
      C107 == "(3) Slightly belong"   ~ 2,
      C107 == "(4) Not at all belong" ~ 1
    ),
    
    # External item 1: "Most Americans value and respect your presence in the US."
    # Recoded: 1 = strongly disagree, 4 = strongly agree (higher = more perceived belonging)
    Valued_Respected_US = case_when(
      C108 == "(4) Strongly disagree" ~ 1,
      C108 == "(3) Somewhat disagree" ~ 2,
      C108 == "(2) Somewhat agree"    ~ 3,
      C108 == "(1) Strongly agree"    ~ 4
    ),
    
    # Internal item 2: "How much do you feel you are an outsider in the US?"
    # Recoded: 1 = strongly outsider, 4 = not at all outsider (higher = more belonging)
    Outsider_US = case_when(
      C109 == "(1) Strongly as an outsider"   ~ 1,
      C109 == "(2) Moderately as an outsider" ~ 2,
      C109 == "(3) Slightly as an outsider"   ~ 3,
      C109 == "(4) Not at all as an outsider" ~ 4
    ),
    
    # External item 2: "How often do people try to exclude you in the US?"
    # Recoded: 1 = always excluded, 4 = never excluded (higher = more belonging)
    Excluded_US_Soc = case_when(
      C110 == "(1) Always"     ~ 1,
      C110 == "(2) Very often" ~ 2,
      C110 == "(3) Rarely"     ~ 3,
      C110 == "(4) Never"      ~ 4
    ),
    
    # Discrimination (personal experience)
    Personal_Discrimination = case_when(
      A116 == "(1) Yes" ~ 1,
      A116 == "(2) No"  ~ 0
    ),
    Race_Ethnicity_Disc = case_when(
      A117 == "(1) Yes" ~ 1,
      A117 == "(2) No"  ~ 0
    ),
    
    # Group-level discrimination perception (Latinos; reverse coded: higher = more discrimination)
    Latino_Disc = case_when(
      as.character(C247) == "(1) The primary problem" ~ 5,
      as.character(C247) == "(2) A major problem"     ~ 4,
      as.character(C247) == "(3) A moderate problem"  ~ 3,
      as.character(C247) == "(4) A minor problem"     ~ 2,
      as.character(C247) == "(5) Not a problem at all" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # National discrimination perception (reverse coded: higher = bigger problem)
    Discrimination_National_Perc = case_when(
      as.character(L268) == "(1) The primary problem" ~ 5,
      as.character(L268) == "(2) A major problem"     ~ 4,
      as.character(L268) == "(3) A moderate problem"  ~ 3,
      as.character(L268) == "(4) A minor problem"     ~ 2,
      as.character(L268) == "(5) Not a problem at all" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Economy perception (1 = getting a lot worse, 5 = getting a lot better)
    Economy = case_when(
      C23 == "(1) Getting a lot better"    ~ 5,
      C23 == "(2) Getting a little better" ~ 4,
      C23 == "(5) Staying about the same"  ~ 3,
      C23 == "(3) Getting a little worse"  ~ 2,
      C23 == "(4) Getting a lot worse"     ~ 1
    ),
    
    # Immigration church attendance
    Imm_Church = A134,
    
    # Policy attitude outcomes
    # BorderSecurity: -1 = decrease, 0 = stay same, 1 = increase border security
    # Coded directionally so positive values = more restrictive
    BorderSecurity = case_when(
      C337 == "(1) Decrease"        ~ -1,
      C337 == "(3) Stay the same"   ~  0,
      C337 == "(2) Increase"        ~  1
    ),
    
    # Pathway_Citizenship: 1 = strongly disagree, 5 = strongly agree
    # Higher values = more support for pathway to citizenship
    Pathway_Citizenship = case_when(
      C39 == "(5) Strongly disagree"          ~ 1,
      C39 == "(4) Disagree"                   ~ 2,
      C39 == "(3) Neither agree nor disagree" ~ 3,
      C39 == "(2) Agree"                      ~ 4,
      C39 == "(1) Strongly agree"             ~ 5
    ),
    
    # Survey weight and race identifier
    Weight     = NAT_WEIGHT,
    Race_Prime = ETHNIC_QUOTA
  )

# --- Belonging indices --------------------------------------------------------
# Internal: felt sense of belonging (Belong_US + Outsider_US), range 2-8
# External: perceived belonging from others (Valued_Respected_US + Excluded_US_Soc), range 2-8
# Both items within each index are on the same 1-4 scale so simple summation is appropriate

cmps_clean_2016 <- cmps_clean_2016 %>%
  mutate(
    Internal_Belonging = Belong_US + Outsider_US,
    External_Belonging = Valued_Respected_US + Excluded_US_Soc
  )

summary(cmps_clean_2016$Internal_Belonging)
summary(cmps_clean_2016$External_Belonging)

# --- Reliability checks (Cronbach's alpha) ------------------------------------
# With only two items per index, alpha is a function of the inter-item correlation
# Alpha >= 0.6 is generally acceptable; >= 0.7 is preferred
# Report these values in the methods/measures section of the paper

library(psych)

alpha_internal <- psych::alpha(
  cmps_clean_2016 %>%
    filter(!is.na(Belong_US), !is.na(Outsider_US)) %>%
    dplyr::select(Belong_US, Outsider_US)
)

alpha_external <- psych::alpha(
  cmps_clean_2016 %>%
    filter(!is.na(Valued_Respected_US), !is.na(Excluded_US_Soc)) %>%
    dplyr::select(Valued_Respected_US, Excluded_US_Soc)
)

cat("Internal Belonging -- Cronbach's alpha:",
    round(alpha_internal$total$raw_alpha, 3), "\n")
cat("External Belonging -- Cronbach's alpha:",
    round(alpha_external$total$raw_alpha, 3), "\n")

# Inter-item correlations (useful to report alongside alpha for 2-item scales)
cat("Internal Belonging -- inter-item correlation:",
    round(alpha_internal$item.stats$r.drop[1], 3), "\n")
cat("External Belonging -- inter-item correlation:",
    round(alpha_external$item.stats$r.drop[1], 3), "\n")

# --- Discrimination scale (composite) -----------------------------------------
# 0 = no personal discrimination; 1 = experienced discrimination by race/ethnicity
# or immigration status; NA = experienced discrimination but type not specified
cmps_clean_2016 <- cmps_clean_2016 %>%
  mutate(
    Discrimination_Scale = case_when(
      Personal_Discrimination == 0 ~ 0,
      Race_Ethnicity_Disc == 1 | Personal_Discrimination == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

# --- Filter to Latino respondents only ----------------------------------------
latinos_data <- cmps_clean_2016 %>%
  filter(Race_Prime == "(2) Hispanic or Latino")

# --- Merge state-level stigma indices -----------------------------------------
# NOTE: class.conc_lat_14_16 is the primary IV
# This is the 2014-2016 change in the concrete immigration policy index
# Chosen over longer windows (conc_lat_index_16, latino_conc_16) for
# defensibility -- captures the policy climate directly preceding the survey
# See workshop feedback [date] for justification

scores <- read.csv("scores_final_subperiods.csv")
latinos_data <- latinos_data %>%
  drop_na(class.conc_lat_14_16)          # drops Alaska (no index value)

# --- State-level stigma indices (categorized) ---------------------------------
# Collapses continuous index into 5 ordered categories
# Direction: higher values = more pro-immigrant (consistent with raw index)
#   -1   = most anti-immigrant  (raw < -10;  e.g. AZ, NC, VA)
#   -0.5 = lean anti-immigrant  (raw -10 to -5)
#    0   = neutral              (raw -5 to 5)
#    0.5 = lean pro-immigrant   (raw 5 to 10)
#    1   = most pro-immigrant   (raw > 10;   e.g. CA, IL, NY)
# Used as robustness check alongside raw continuous index in final models
categorize_index <- function(x) {
  case_when(
    x < -10                    ~ -1,
    x >= -10 & x < -5         ~ -0.5,
    x >= -5  & x <= 5         ~  0,
    x >  5   & x <= 10        ~  0.5,
    x > 10                    ~  1
  )
}

latinos_data <- latinos_data %>%
  mutate(
    sym_lat_index_16     = categorize_index(latino_sym_16),
    conc_lat_index_16    = categorize_index(latino_conc_16),
    sym_lat_index_20     = categorize_index(latino_sym_20),
    conc_lat_index_20    = categorize_index(latino_conc_20),
    # NOTE ON INDEX DIRECTION:
    # Higher values = more pro-immigrant policy climate (CA, NY, IL score highest)
    # Lower/more negative values = more anti-immigrant policy climate (AZ, TX score lowest)
    # This direction is maintained for consistency with previous chapters where
    # results are already written and interpreted in this direction.
    # Coefficients should therefore be read as:
    #   positive coefficient = more pro-immigrant context predicts more X
    #   negative coefficient = more pro-immigrant context predicts less X
    conc_lat_index_14_16 = categorize_index(class.conc_lat_14_16),
    sym_lat_index_14_16  = categorize_index(class.sym_lat_14_16)
  )

# --- Save cleaned Latino dataset ----------------------------------------------
#write.csv(latinos_data, "latinos_cmps_2016.csv", row.names = FALSE)

# --- Survey design objects ----------------------------------------------------
# Primary: all Latino respondents, weighted
cmps_lat_16 <- svydesign(
  ids     = ~1,
  data    = latinos_data,
  weights = ~Weight
)

# State-clustered: for robustness checks with clustered SEs
cmps_lat_16_state <- svydesign(
  ids     = ~State,
  data    = latinos_data,
  weights = ~Weight
)

# Subsamples by nativity
native_born  <- latinos_data %>% filter(NativeBorn == 1)
foreign_born <- latinos_data %>% filter(NativeBorn != 1)

native_cmps <- svydesign(ids = ~1, data = native_born,  weights = ~Weight)
foreign_cmps <- svydesign(ids = ~1, data = foreign_born, weights = ~Weight)

# --- Sample representativeness check ------------------------------------------
# Compare national origin distribution against 2016 ACS
prop.table(svytable(~National_Origin, cmps_lat_16))

total_pop_2016 <- 55199107
national_origin_acs_2016 <- data.frame(
  Origins = c("Central American", "Cuban", "Dominican", "Mexican",
              "Puerto Rican", "South American"),
  Pop     = c(5002699, 2077828, 1788697, 35110480, 5275008, 3344238)
)
national_origin_acs_2016 <- national_origin_acs_2016 %>%
  mutate(Pop_Percent = (Pop / total_pop_2016) * 100)

# Overall distribution is very close to ACS benchmarks

# --- Latino + Asian subsample (for immigrant comparison analyses) -------------
# NOTE: Uses foreign-group stigma indices rather than Latino-specific ones
# Indices are sign-flipped so higher = more anti-immigrant (consistent direction)
immigrants_data <- cmps_clean_2016 %>%
  filter(Race_Prime %in% c("(2) Hispanic or Latino", "(4) Asian American")) %>%
  drop_na(foreign_sym_20, foreign_conc_20) %>%
  mutate(
    sym_for_index_16  = categorize_index(foreign_sym_16)  * -1,
    conc_for_index_16 = categorize_index(foreign_conc_16) * -1,
    sym_for_index_20  = categorize_index(foreign_sym_20)  * -1,
    conc_for_index_20 = categorize_index(foreign_conc_20) * -1,
    foreign_sym_16    = foreign_sym_16  * -1,
    foreign_conc_16   = foreign_conc_16 * -1,
    foreign_sym_20    = foreign_sym_20  * -1,
    foreign_conc_20   = foreign_conc_20 * -1,
    # Group discrimination perception varies by race
    Group_Disc = case_when(
      Race_Prime == "(2) Hispanic or Latino" ~ as.character(C247),
      Race_Prime == "(4) Asian American"     ~ as.character(C245),
      TRUE ~ NA_character_
    )
  )

cmps_imm_16 <- svydesign(
  ids     = ~1,
  data    = immigrants_data,
  weights = ~Weight
)