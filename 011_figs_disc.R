##### Figures for the Poster ---------------------------------------------------
## DV histograms --------------------------------------------------------------
## Discrimination
disc_figure <- cmps_lat_16$variables %>% 
  ggplot(aes(x = Latino_Disc)) + geom_histogram(fill = "blue") + 
  xlab("Perception of Group Discrimination") + theme_bw()

ggsave("disc_plot.png", width = 12, height = 8, units = "in", dpi = 200)

## Internal Belonging

int_fig <- cmps_lat_16$variables %>% 
  ggplot(aes(x = Internal_Belonging)) + geom_histogram(fill = "gray") + 
  xlab("Internal Sense of Belonging") + theme_bw()

ggsave("internal_plot.png", int_fig, width = 12, height = 8, units = "in", dpi = 200)

ext_fig <- cmps_lat_16$variables %>% 
  ggplot(aes(x = External_Belonging)) + geom_histogram(fill = "gray") + 
  xlab("External Sense of Belonging") + theme_bw()

ggsave("external_plot.png", ext_fig, width = 12, height = 8, units = "in", dpi = 200)


######### Figures for the models  ----------------------------------------------


######### Figures for the basic models  ----------------------------------------
lapply(med_basic_ols$mediator_models, coefplot)

library(broom)
library(ggplot2)
library(dplyr)
library(purrr)

# Suppose your list is named models
### 
names(med_basic_ols$mediator_models) <- c("Fear", "Anger", "Pride", "Hope", "Sad")
names(med_basic_ols$outcome_models) <- c("Fear & Internal Belonging", 
                                         "Anger & Internal Belonging", 
                                         "Pride & Internal Belonging", 
                                         "Hope & Internal Belonging", 
                                         "Sad & Internal Belonging",
                                         "Fear & External Belonging", 
                                         "Anger & External Belonging", 
                                         "Pride & External Belonging", 
                                         "Hope & External Belonging", 
                                         "Sad & External Belonging")

tidy_models_emos <- map_dfr(med_basic_ols$mediator_models, tidy, conf.int = TRUE, .id = "model")

ggplot(tidy_models_emos, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ model) + 
  theme_bw() + labs(title = "Predictors of Emotion Response")

int_bel_mods <- list(
    "Fear & Internal Belonging"  = med_basic_ols$outcome_models$`Fear & Internal Belonging`,
    "Anger & Internal Belonging" = med_basic_ols$outcome_models$`Anger & Internal Belonging`,
    "Pride & Internal Belonging" = med_basic_ols$outcome_models$`Pride & Internal Belonging`,
    "Hope & Internal Belonging"  = med_basic_ols$outcome_models$`Hope & Internal Belonging`,
    "Sad & Internal Belonging"   = med_basic_ols$outcome_models$`Sad & Internal Belonging`
  )
  
ext_bel_mods <- list(
    "Fear & External Belonging"  = med_basic_ols$outcome_models$`Fear & External Belonging`,
    "Anger & External Belonging" = med_basic_ols$outcome_models$`Anger & External Belonging`,
    "Pride & External Belonging" = med_basic_ols$outcome_models$`Pride & External Belonging`,
    "Hope & External Belonging"  = med_basic_ols$outcome_models$`Hope & External Belonging`,
    "Sad & External Belonging"   = med_basic_ols$outcome_models$`Sad & External Belonging`
  )
  

tidy_models_bel <- map_dfr(int_bel_mods, tidy, conf.int = TRUE, .id = "model")
tidy_models_bel_ext <- map_dfr(ext_bel_mods, tidy, conf.int = TRUE, .id = "model")

tidy_models_bel <- tidy_models_bel %>%
  mutate(term_category = case_when(
    term %in% c("Age", "Gender") ~ "Demographics",
    term %in% c("Mexican", "Cuban", "More_Than_SecondGen") ~ "Ethnicity/Generation",
    term %in% c("class.conc_lat_14_16", "Fear_Election", "Sad_Election", "Angry_Election",
                "Pride_Election", "Hope_Election") ~ "Key Predictors",
    term %in% c("Party", "Intercept") ~ "Other",
    TRUE ~ "Other"
  ))


tidy_models_bel_ext <- tidy_models_bel_ext %>%
  mutate(term_category = case_when(
    term %in% c("Age", "Gender") ~ "Demographics",
    term %in% c("Mexican", "Cuban", "More_Than_SecondGen") ~ "Ethnicity/Generation",
    term %in% c("class.conc_lat_14_16", "Fear_Election", "Sad_Election", "Angry_Election",
                "Pride_Election", "Hope_Election") ~ "Key Predictors",
    term %in% c("Party", "Intercept") ~ "Other",
    TRUE ~ "Other"
  ))

ggplot(tidy_models_bel, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = .5,  # thicker error bars
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ term_category) +
  theme_bw() + labs(title = "Basic Models for Internal Belonging")

ggplot(tidy_models_bel_ext, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = .3,  # thicker error bars
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ term_category) +
  theme_bw() + labs(title = "Basic Models for External Belonging")

## more visible plots 
keep_terms <- c("class.conc_lat_14_16", "Gender", "More_Than_SecondGen", "Mexican",
                "Fear_Election", "Sad_Election", "Angry_Election",
                "Pride_Election", "Hope_Election" )

tidy_models_bel_filt <- tidy_models_bel %>%
  filter(term %in% keep_terms)

tidy_models_bel_ext_filt <- tidy_models_bel_ext %>%
  filter(term %in% keep_terms)
basic_filt_int <- ggplot(tidy_models_bel_filt, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = .5,
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ term_category, drop = TRUE) + theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set1") +
  theme_bw() + labs(title = "Basic Models for Internal Belonging")
ggsave("basic_int_plot.png",  basic_filt_int, width = 10, height = 6, dpi = 300)


basic_filt_ext <- ggplot(tidy_models_bel_ext_filt, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = .3,
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ term_category, drop = TRUE) + theme(legend.position = "none") + 
  theme_bw() + labs(title = "Basic Models for External Belonging")
ggsave("basic_ext_plot.png",  basic_filt_ext,width = 10, height = 6, dpi = 300)

library(patchwork)

bothplots <- basic_filt_int + basic_filt_ext + 
  plot_layout(guides = "collect") & theme(legend.position = "right") 

ggsave("plots_side_by_side.png", bothplots,
       width = 14, height = 7, dpi = 300)


######### Figures for the full models  -----------------------------------------

names(med_full_ols$mediator_models)

names(med_full_ols$mediator_models) <- c("Fear", "Anger", "Pride", "Hope", "Sad")
names(med_full_ols$outcome_models) <- c("Fear & Internal Belonging", 
                                         "Anger & Internal Belonging", 
                                         "Pride & Internal Belonging", 
                                         "Hope & Internal Belonging", 
                                         "Sad & Internal Belonging",
                                         "Fear & External Belonging", 
                                         "Anger & External Belonging", 
                                         "Pride & External Belonging", 
                                         "Hope & External Belonging", 
                                         "Sad & External Belonging")

full_models_emos <- map_dfr(med_full_ols$mediator_models, tidy, conf.int = TRUE, .id = "model")

ggplot(full_models_emos, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ model) + 
  theme_bw() + labs(title = "Predictors of Emotion Response")

int_bel_full <- list(
  "Fear & Internal Belonging"  = med_full_ols$outcome_models$`Fear & Internal Belonging`,
  "Anger & Internal Belonging" = med_full_ols$outcome_models$`Anger & Internal Belonging`,
  "Pride & Internal Belonging" = med_full_ols$outcome_models$`Pride & Internal Belonging`,
  "Hope & Internal Belonging"  = med_full_ols$outcome_models$`Hope & Internal Belonging`,
  "Sad & Internal Belonging"   = med_full_ols$outcome_models$`Sad & Internal Belonging`
)

ext_bel_full <- list(
  "Fear & External Belonging"  = med_full_ols$outcome_models$`Fear & External Belonging`,
  "Anger & External Belonging" = med_full_ols$outcome_models$`Anger & External Belonging`,
  "Pride & External Belonging" = med_full_ols$outcome_models$`Pride & External Belonging`,
  "Hope & External Belonging"  = med_full_ols$outcome_models$`Hope & External Belonging`,
  "Sad & External Belonging"   = med_full_ols$outcome_models$`Sad & External Belonging`
)

full_models_bel <- map_dfr(int_bel_full, tidy, conf.int = TRUE, .id = "model")
full_models_bel_ext <- map_dfr(ext_bel_full, tidy, conf.int = TRUE, .id = "model")

full_models_bel <- full_models_bel %>%
  mutate(term_category = case_when(
    term %in% c("Age", "Gender", "Education", "Income") ~ "Demographics",
    term %in% c("Mexican", "Cuban", "More_Than_SecondGen", "Linked_Fate") ~ "Ethnicity/Generation",
    term %in% c("Discrimination_Scale", "Latino_Disc") ~ "Discrimination",
    term %in% c("class.conc_lat_14_16", "Fear_Election", "Sad_Election", "Angry_Election",
                "Pride_Election", "Hope_Election") ~ "Key Predictors",
    term %in% c("Pol_Interest","Party", "Intercept") ~ "Other",
    TRUE ~ "Other"
  ))


full_models_bel_ext <- full_models_bel_ext %>%
  mutate(term_category = case_when(
    term %in% c("Age", "Gender", "Education", "Income") ~ "Demographics",
    term %in% c("Mexican", "Cuban", "More_Than_SecondGen", "Linked_Fate") ~ "Ethnicity/Generation",
    term %in% c("Discrimination_Scale", "Latino_Disc") ~ "Discrimination",
    term %in% c("class.conc_lat_14_16", "Fear_Election", "Sad_Election", "Angry_Election",
                "Pride_Election", "Hope_Election") ~ "Key Predictors",
    term %in% c("Pol_Interest","Party", "Intercept") ~ "Other",
    TRUE ~ "Other"
  ))

ggplot(full_models_bel, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = .5,  # thicker error bars
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ term_category) +
  theme_bw() + labs(title = "Full Models for Internal Belonging")

ggplot(full_models_bel_ext, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = .3,  # thicker error bars
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ term_category) +
  theme_bw() + labs(title = "Full Models for External Belonging")

### Clustered SEs MODELS 



sb_ses_int <- map_dfr(listmods_sb_int, tidy, conf.int = TRUE, .id = "model")
sb_ses_ext <- map_dfr(listmods_sb_ext, tidy, conf.int = TRUE, .id = "model")

se_intbel <- sb_ses_int %>%
  mutate(term_category = case_when(
    term %in% c("Age", "Gender", "Education", "Income") ~ "Demographics",
    term %in% c("Mexican", "Cuban", "More_Than_SecondGen", "Linked_Fate") ~ "Ethnicity/Generation",
    term %in% c("class.conc_lat_14_16", "Fear_Election", "Sad_Election", "Angry_Election",
                "Pride_Election", "Hope_Election") ~ "Key Predictors",
    term %in% c("Pol_Interest","Party", "Intercept") ~ "Other",
    TRUE ~ "Other"
  ))


se_extbel <- sb_ses_ext %>%
  mutate(term_category = case_when(
    term %in% c("Age", "Gender", "Education", "Income") ~ "Demographics",
    term %in% c("Mexican", "Cuban", "More_Than_SecondGen", "Linked_Fate") ~ "Ethnicity/Generation",
    term %in% c("class.conc_lat_14_16", "Fear_Election", "Sad_Election", "Angry_Election",
                "Pride_Election", "Hope_Election") ~ "Key Predictors",
    term %in% c("Pol_Interest","Party", "Intercept") ~ "Other",
    TRUE ~ "Other"
  ))


ggplot(se_intbel, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = .5,
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ term_category, drop = TRUE) + theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set1") +
  theme_bw() + labs(title = "Basic Models for Internal Belonging, Clustered SEs at the State Level")

ggplot(full_models_bel_ext, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = .3,  # thicker error bars
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ term_category) +
  theme_bw() + labs(title = "Full Models for External Belonging")
