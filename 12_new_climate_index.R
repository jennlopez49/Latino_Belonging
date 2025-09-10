### Importing Data for Topic Modeling (To Add Differing Weights) #####
coded_laws_12_16 <- read.csv('~/Desktop/COIi work/State_Laws/coding_2012_2016_full.csv')

coded_laws_16_20 <- read.csv('~/Desktop/COIi work/State_Laws/coding_2016_2020.csv')

### Excluded Irrelevant Bills from Both ----

codedlaws_2012 <- coded_laws_12_16 %>% filter(!Class_Cleaned == "I")
codedlaws_2016 <- coded_laws_16_20 %>% filter(!Class == "I")

### Doing Topic Modelling ---- 

library(dplyr)
library(tidytext)
library(tm)
library(topicmodels)
library(stringr)

codedlaws_2012 <- codedlaws_2012 %>% select(-c(Class, X))
                                            
codedlaws_2012 <- codedlaws_2012 %>% mutate(
  Class = Class_Cleaned) %>% dplyr::select(-Class_Cleaned)

codedlaws_2016 <- codedlaws_2016 %>% dplyr::select(-Column1)

### Combine into one dataframe ####
coded_laws <- bind_rows(
  codedlaws_2012 %>% mutate(period = "2012-2016"),
  codedlaws_2016 %>% mutate(period = "2016-2020")
)

### Text Preprocessing ####
# Keep only the ID (or create one), summary, and label for now
coded_laws <- coded_laws %>%
  mutate(id = Bill_Number,
         text = Summary) %>%
  dplyr::select(State, id, text, Class, period, Year)

################## Tried Text Methods ------------------------------------------
# Remove NAs and short entries
coded_laws <- coded_laws %>%
  filter(!is.na(text), nchar(text) > 20)


# Unnest tokens
tidy_bills <- coded_laws %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%       # Remove stop words
  filter(str_detect(word, "[a-z]"))            # Remove numbers and pure punctuation

immigration_terms <- c("immigration", "immigrant", "immigrants", "alien", "noncitizen",
                       "citizenship", "asylum", "border", "undocumented", "status", "visa",
                       "naturalization", "deportation", "migrant", "refugee", "law",
                       "resolution", "united", "u.s.")

# Then filter them out before DTM:
tidy_bills <- tidy_bills %>%
  filter(!word %in% immigration_terms)

# Optional: add custom stopwords or stemming
# tidy_bills <- tidy_bills %>% mutate(word = SnowballC::wordStem(word))

# Create Document-Term Matrix
dtm <- tidy_bills %>%
  count(id, word) %>%
  cast_dtm(document = id, term = word, value = n)

### Topic Modeling: Fit LDA model ####
k <- 5  # Number of topics — change this depending on your needs

lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

### Extract Topics and Terms ####
# Top terms per topic
topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot top terms
library(ggplot2)
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms in Each Topic", x = NULL, y = "Beta")

### Optional: Add topic probabilities to each bill ####
bill_topics <- tidy(lda_model, matrix = "gamma")  # gamma = document-topic prob
coded_laws_with_topics <- left_join(coded_laws, bill_topics, by = c("id" = "document"))

# After extracting top terms
top_terms %>%
  group_by(topic) %>%
  summarize(words = paste(term, collapse = ", ")) %>%
  arrange(topic)


##### USING STM ----- 
# Step 2: Preprocessing the text
processed <- textProcessor(
  documents = coded_laws$text,
  metadata = coded_laws,
  removestopwords = TRUE,
  wordLengths = c(3, Inf),
  customstopwords = c("immigrant", "immigration", "alien", "status", "noncitizen"),  # optional
  verbose = TRUE
)

# Step 3: Prepare documents
prep <- prepDocuments(
  documents = processed$documents,
  vocab = processed$vocab,
  meta = processed$meta
)

model <- stm(
  documents = prep$documents,
  vocab = prep$vocab,
  data = prep$meta,
  K = 5,  # number of topics
  seed = 1234
)
labelTopics(model, n = 10)
plot(model,
     type = "summary",
     labeltype = "frex",  # FREX = words that are frequent and exclusive
     n = 7,               # Top words to show
     main = "Topic Prevalence")

topicCorr <- topicCorr(model)
plot(topicCorr)

#### Using Top Terms -----------
# Load required packages

library(tidytext)
library(tidyr)
library(ggplot2)
library(tm)

# Tokenize into unigrams and bigrams
unigrams <- codedlaws_2012 %>%
  select(Summary) %>%
  unnest_tokens(word, Summary, token = "words") %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE)

# Bigrams
bigrams <- codedlaws_2012 %>%
  select(Summary) %>%
  unnest_tokens(word, Summary, token = "ngrams", n = 2) %>%
  count(word, sort = TRUE)

# View top unigrams and bigrams
head(unigrams, 10)
head(bigrams, 10)

# Separate bigrams into two words
bigrams_separated <- bigrams %>%
  separate(word, into = c("word1", "word2"), sep = " ")

# Remove if either word is a stopword
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word)

# Recombine and sort
bigrams_cleaned <- bigrams_filtered %>%
  unite(word, word1, word2, sep = " ") %>%
  arrange(desc(n))

# View top cleaned bigrams
head(bigrams_cleaned, 10)

custom_legal_stopwords <- c("law", "resolution", "act", "united", "states", "including", "section", "provide", "state")

# Apply to unigrams
unigrams_cleaned <- unigrams %>%
  filter(!word %in% custom_legal_stopwords)

# Apply to bigrams too (on full phrase)
bigrams_cleaned <- bigrams_cleaned %>%
  filter(!str_detect(word, str_c(custom_legal_stopwords, collapse = "|")))



### Doing it manually --- using Keywords ---------------------------------------
keyword_dict <- list(
  licensure = c("license", "licensure", "driver's license", "professional license", "real id", "apply to become"),
  education = c("tuition", "scholarship", "university", "college", "in-state", "grant", "undocumented student", "financial aid"),
  services = c("medicaid", "snap", "tanf", "social services", "welfare", "public assistance", "English language", "ESL", "civic"),
  enforcement = c("ICE", "detainer", "sheriff", "287\\(g\\)", "deportation", "alien assistance", "immigration status", "sanctuary",
                  "cooperation"),
  refugees_only = c("refugee"),
  migrants_only = c("migrant")
)

licensure_keywords <- c(
  "license", "driver's license", "professional license", "occupational license",
  "driver license", "commercial license", "identification card", "real id",
  "permit", "licensure", "state id", "id card", "driving privilege", "commercial driver's",
  "commercial learner's", "learner's", "identification"
)
education_access_keywords <- c(
  "tuition", "in-state tuition", "college", "university", "scholarship",
  "financial aid", "student", "education", "higher education",
  "residency for tuition", "undocumented student", "dream act", "nonresident tuition",
  "student"
)
services_keywords <- c(
  "medicaid", "snap", "tanf", "social services", "public assistance",
  "child care", "healthcare", "health", "healthcare", "medical", "welfare",
  "benefits eligibility", "mental health", "medical services",
  "food stamps", "subsidy", "public benefits", "limit english", "english language",
  "ESL", "civic", "english proficiency", "language learners", "victim", "naturalization assistance",
  "naturalization services"
)
status_keywords <- c("criminal alien", "qualified alien", "qualified aliens", "legal resident", "citizenship status",
                     "immigration status", "permanent residence", "permanent residents",
                     "authorization document", "eligibility requirements", "illegal alien",
                     "identity document", "legal immigrant", "legal permanent", "alien")
enforcement_imm_keywords <- c(
  "ice", "detainer", "287(g)", "immigration enforcement", "cooperation with ice",
  "law enforcement", "sheriff", "police", "deportation", "hold request",
  "jail", "immigration status check", "custody", "immigration detainer", "felony",
  "border protection", "notary public", "alien registration", "border security", 
  "enforcement agencies"
)

criminal_keywords <- c("felony", "misdemeanor", "conviction", "crime", "criminal offense",
  "sentencing", "plea", "detention", "smuggling", "harboring", "fraud", 
  "lawful presence")

refugee_keywords <- c("refugee", "asylee", "resettlement",
                      "refugee program", "resettlement agency")
migrant_keywords <- c("migrant", "migrants")
employment_keywords <- c("employment", "employer", "e-verify", "right to work", "hiring",
                         "labor rights", "workforce", "employee", "unauthorized employment")
emergency_keywords <- c("emergency services", "911", "police access", "fire department",
                        "ambulance")

coded_laws_full <- coded_laws %>%
  mutate(
    licensure = str_detect(tolower(text), str_c(licensure_keywords, collapse = "|")),
    education_access = str_detect(tolower(text), str_c(education_access_keywords, collapse = "|")),
    services = str_detect(tolower(text), str_c(services_keywords, collapse = "|")),
    status = str_detect(tolower(text), str_c(status_keywords, collapse = "|")),
    enforcement = str_detect(tolower(text), str_c(enforcement_imm_keywords, collapse = "|")),
    criminal = str_detect(tolower(text), str_c(criminal_keywords, collapse = "|")),
    refugee = str_detect(tolower(text), str_c(refugee_keywords, collapse = "|")),
    migrant = str_detect(tolower(text), str_c(migrant_keywords, collapse = "|")),
    employment = str_detect(tolower(text), str_c(employment_keywords, collapse = "|")),
    emergency = str_detect(tolower(text), str_c(emergency_keywords, collapse = "|"))
  ) %>%
  mutate(across(c(licensure, education_access, services, status, enforcement,
                  criminal, refugee, migrant, employment, emergency), as.integer))

### SCALE USED ------- 
# df_cleaned<-  df_cleaned %>% mutate(Imm_Class_Code = case_when(
#   Class_Cleaned == "A" ~ -1,
#   Class_Cleaned == "SA" ~ -0.25,
#   Class_Cleaned == "N" ~ 0,
#   Class_Cleaned == "SP" ~ 0.25,
#   Class_Cleaned == "P" ~ 1,
#   TRUE ~ NA_real_)
# )

coded_laws_full <- coded_laws_full %>% mutate(
  Class = str_trim(Class),
  Class_Int = case_when(Class == "A" ~ -1,
                        Class == "SA" ~ -0.5,
                        Class == "N" ~ 0,
                        Class == "SP" ~ 0.5,
                        Class == "P" ~ 1,
      TRUE ~ NA_real_),
  impact_score = licensure + education_access + services + status + enforcement + migrant + employment + emergency, ## EXCLUDING REFUGEES
  weighted_class = if_else(impact_score == 0, Class_Int, Class_Int * impact_score)
)


### FULL INDICATOR (WITH SYMBOLIC LEG) ------- 

coded_sym <- coded_laws_full %>%
  group_by(State, period, Year) %>%
  summarise(Class_Pts = sum(weighted_class)) %>%
  ungroup()


### FULL INDICATOR (WITHOUT SYMBOLIC)-----

coded_laws_concrete <- coded_laws_full %>%
  filter(!(Class_Int %in% c(-0.5, 0.5)))

coded_concrete <- coded_laws_concrete %>%
  group_by(State, period, Year) %>%
  summarise(Class_Pts = sum(weighted_class)) %>%
  ungroup()


### Adding in 287(g) agreements ------------

state_exp_287g <- read.csv("~/Desktop/COIi work/State_Laws/state_exp_full.csv")

### 2012-2016 period -----------------------------------------------------------

coded_sym_16 <- coded_sym %>% filter(period == "2012-2016")
exp_lat_16 <- state_exp_287g %>% dplyr::select(State, total_exp_lat_2016, total_exp_for_2016)

combined_df_16 <- coded_sym_16 %>%
  left_join(exp_lat_16, by = "State") %>%
  mutate(exp_lat_score = replace_na(total_exp_lat_2016, 0),
         exp_for_score = replace_na(total_exp_for_2016, 0))

combined_df_16 <- combined_df_16 %>% dplyr::select(-c(total_exp_lat_2016,total_exp_for_2016)) %>%
  mutate(class_lat_2016 = Class_Pts + exp_lat_score,
         class_for_2016 = Class_Pts + exp_for_score)

### Concrete policies only 
coded_conc_16 <- coded_concrete %>% filter(period == "2012-2016")

conc_df_16 <- coded_conc_16 %>%
  left_join(exp_lat_16, by = "State") %>%
  mutate(exp_lat_score = replace_na(total_exp_lat_2016, 0),
         exp_for_score = replace_na(total_exp_for_2016, 0))

conc_df_16 <- conc_df_16 %>% dplyr::select(-c(total_exp_lat_2016,total_exp_for_2016)) %>%
  mutate(class_lat_2016 = Class_Pts + exp_lat_score,
         class_for_2016 = Class_Pts + exp_for_score)

### 2016-2020 period -----------------------------------------------------------
coded_sym_20 <- coded_sym %>% filter(period == "2016-2020")
coded_conc_20 <- coded_concrete %>% filter(period == "2016-2020")
exp_lat_20 <- state_exp_287g %>% dplyr::select(State, total_exp_lat_2020, total_exp_for_2020)

combined_df_20 <- coded_sym_20 %>%
  left_join(exp_lat_20, by = "State") %>%
  mutate(exp_lat_score = replace_na(total_exp_lat_2020, 0),
         exp_for_score = replace_na(total_exp_for_2020, 0))

combined_df_20 <- combined_df_20 %>% dplyr::select(-c(total_exp_lat_2020,total_exp_for_2020)) %>%
  mutate(class_lat_2020 = Class_Pts + exp_lat_score,
         class_for_2020 = Class_Pts + exp_for_score)


conc_df_20 <- coded_conc_20 %>%
  left_join(exp_lat_20, by = "State") %>%
  mutate(exp_lat_score = replace_na(total_exp_lat_2020, 0),
         exp_for_score = replace_na(total_exp_for_2020, 0))

conc_df_20 <- conc_df_20 %>% dplyr::select(-c(total_exp_lat_2020,total_exp_for_2020)) %>%
  mutate(class_lat_2020 = Class_Pts + exp_lat_score,
         class_for_2020 = Class_Pts + exp_for_score)

### combining into one dataset ------------------------- Keeping only final score ------
symb_df_16 <- combined_df_16 %>%
  mutate(latino_sym_16 = class_lat_2016,
         foreign_sym_16 = class_for_2016)  %>% dplyr::select(State, latino_sym_16, foreign_sym_16)
symb_df_20 <- combined_df_20 %>%  
  mutate(latino_sym_20 = class_lat_2020,
         foreign_sym_20 = class_for_2020)  %>% 
  dplyr::select(State, latino_sym_20, foreign_sym_20)

conc_df_16 <- conc_df_16 %>% mutate(latino_conc_16 = class_lat_2016,
                                    foreign_conc_16 = class_for_2016)  %>% 
  dplyr::select(State, latino_conc_16, foreign_conc_16)
conc_df_20 <- conc_df_20 %>% mutate(latino_conc_20 = class_lat_2020,
                                    foreign_conc_20 = class_for_2020)  %>% 
  dplyr::select(State, latino_conc_20, foreign_conc_20)

## merging back into one single year-period 

scores_2016 <- merge(symb_df_16, conc_df_16, by = c("State"), all = TRUE)
scores_2020 <- merge(symb_df_20, conc_df_20, by = c("State"), all = TRUE)

scores_final <- merge(scores_2016, scores_2020, by = "State", all = TRUE)

## Alabama only had symbolic leg during this period -----> adding in the zero manually
scores_final$latino_conc_20[scores_final$State == "AL"] <- 0
scores_final$foreign_conc_20[scores_final$State == "AL"] <- 0

# saving 
# write.csv(scores_final, "scores_final.csv")

### ------------------- ADD SUBPERIODS 2012-2014 AND 2014-2016 -------------------

# Symbolic
coded_sym_12_14 <- coded_sym %>% filter(period == "2012-2016" & Year < 2014)
coded_sym_14_16 <- coded_sym %>% filter(period == "2012-2016" & Year > 2013)

# Concrete
coded_conc_12_14 <- coded_concrete %>% filter(period == "2012-2016" & Year < 2014)
coded_conc_14_16 <- coded_concrete %>% filter(period == "2012-2016" & Year > 2013)

# Join with 287(g) scores (assuming your state_exp_287g has columns for subperiods)
# exp_lat_12_14 <- state_exp_287g %>% dplyr::select(State, total_exp_lat_2016, total_exp_for_2016) ## only did relevant period (14-16)
exp_lat_14_16 <- state_exp_287g %>% dplyr::select(state, total_exp_lat_2014, total_exp_for_2014,
                                                  total_exp_lat_2016, total_exp_for_2016)

# Symbolic subperiods
# combined_sym_12_14 <- coded_sym_12_14 %>%
#   left_join(exp_lat_12_14, by = "State") %>%
#   mutate(exp_lat_score = replace_na(total_exp_lat_2014, 0),
#          exp_for_score = replace_na(total_exp_for_2014, 0),
#          class_lat_12_14 = Class_Pts + exp_lat_score,
#          class_for_12_14 = Class_Pts + exp_for_score) %>%
#   dplyr::select(State, class_lat_12_14, class_for_12_14)

combined_sym_14_16 <- coded_sym_14_16 %>%
  left_join(exp_lat_14_16, by = c("State" = "state")) %>%
  mutate(exp_lat_score_14 = replace_na(total_exp_lat_2014, 0),
         exp_for_score_14 = replace_na(total_exp_for_2014, 0),
         exp_lat_score_16 = replace_na(total_exp_lat_2016, 0),
         exp_for_score_16 = replace_na(total_exp_for_2016, 0),
         class_lat_14 = Class_Pts + exp_lat_score_14,
         class_for_14 = Class_Pts + exp_for_score_14,
         class_lat_16 = Class_Pts + exp_lat_score_16,
         class_for_16 = Class_Pts + exp_for_score_16,
         class_lat_14_16_avg = (class_lat_14 + class_lat_16)/2,
         class_for_14_16_avg = (class_for_14 + class_for_16)/2) %>%
  dplyr::select(State, class_lat_14, class_for_14, class_lat_16, class_for_16,
                class_lat_14_16_avg,
                class_for_14_16_avg)

# Concrete subperiods
# combined_conc_12_14 <- coded_conc_12_14 %>%
#   left_join(exp_lat_14, by = "State") %>%
#   mutate(exp_lat_score = replace_na(total_exp_lat_2014, 0),
#          exp_for_score = replace_na(total_exp_for_2014, 0),
#          class_lat_12_14 = Class_Pts + exp_lat_score,
#          class_for_12_14 = Class_Pts + exp_for_score) %>%
#   dplyr::select(State, class_lat_12_14, class_for_12_14)

combined_conc_14_16 <- coded_conc_14_16 %>%
  left_join(exp_lat_14_16, by = c("State" = "state")) %>%
  mutate(exp_lat_score_14 = replace_na(total_exp_lat_2014, 0),
         exp_for_score_14 = replace_na(total_exp_for_2014, 0),
         exp_lat_score_16 = replace_na(total_exp_lat_2016, 0),
         exp_for_score_16 = replace_na(total_exp_for_2016, 0),
         class_lat_14 = Class_Pts + exp_lat_score_14,
         class_for_14 = Class_Pts + exp_for_score_14,
         class_lat_16 = Class_Pts + exp_lat_score_16,
         class_for_16 = Class_Pts + exp_for_score_16,
         class_lat_14_16_avg = (class_lat_14 + class_lat_16)/2,
         class_for_14_16_avg = (class_for_14 + class_for_16)/2) %>%
  dplyr::select(State, class_lat_14, class_for_14, class_lat_16, class_for_16,
                class_lat_14_16_avg,
                class_for_14_16_avg)

combined_conc_14_16_final <- combined_conc_14_16 %>%
  group_by(State) %>%
  summarise(
    class_lat_14 = sum(class_lat_14, na.rm = TRUE),
    class_for_14 = sum(class_for_14, na.rm = TRUE),
    class_lat_16 = sum(class_lat_16, na.rm = TRUE),
    class_for_16 = sum(class_for_16, na.rm = TRUE)
  ) %>%
  mutate(
    class_lat_14_16_avg = (class_lat_14 + class_lat_16) / 2,
    class_for_14_16_avg = (class_for_14 + class_for_16) / 2
  )

# symbolic 


combined_sym_14_16_final <- combined_sym_14_16 %>%
  group_by(State) %>%
  summarise(
    class_lat_sym14 = sum(class_lat_14, na.rm = TRUE),
    class_for_sym14 = sum(class_for_14, na.rm = TRUE),
    class_lat_sym16 = sum(class_lat_16, na.rm = TRUE),
    class_for_sym16 = sum(class_for_16, na.rm = TRUE)
  ) %>%
  mutate(
    class_lat_14_16_sym.avg = (class_lat_sym14 + class_lat_sym16) / 2,
    class_for_14_16_sym.avg = (class_for_sym14 + class_for_sym16) / 2
  )

### ------------------- MERGE SUBPERIOD SCORES -------------------
# Symbolic
# symb_subperiods <- combined_sym_12_14 %>%
#   left_join(combined_sym_14_16, by = "State")
# 
# # Concrete
# conc_subperiods <- combined_conc_12_14 %>%
#   left_join(combined_conc_14_16, by = "State")

# Merge symbolic + concrete into one final dataset
scores_final_subperiods <- combined_conc_14_16_final %>%
  left_join(combined_sym_14_16_final, by = "State")

# Save
write.csv(scores_final_subperiods, "scores_final_subperiods.csv")
