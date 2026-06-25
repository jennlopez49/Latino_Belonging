################### CHAPTER 3 ANALYSES #########################################
survey_controls_pol <- c("latino_conc_24","Age", "Sex", "Education", "Income",
                         "PartyID_5pt", "Acculturation", "Span_Acc", "Birthplace","GroupDiscImms_clean",  
                         "BorderState")
emotions <- c("Emotions_Anger", "Emotions_Fear", "Emotions_Shame",
              "Emotions_Relief", "Emotions_Pride", "Emotions_Joy")
emotions_all <- c("Emotions_Anger + Emotions_Fear + Emotions_Shame +
               Emotions_Relief + Emotions_Pride + Emotions_Joy")
survey_emotions   <- c("Emotions_Anger", "Emotions_Fear", "Emotions_Shame",
                       "Emotions_Relief", "Emotions_Pride", "Emotions_Joy")

survey_stigma_full     <- c("StigmaImm_mean", "StigmaLatino_mean")
survey_stigma     <- c("StigmaImm_mean")
policy_dvs        <- c("BorderPolicyIndex",
                       "InteriorPolicyIndex")
policy_dvs_indiv <- c(
  # Interior enforcement
  "ProlongedDetention",
  "DetainUSCits", 
  "UseForceICE",
  "ThirdCountryDept",
  # Border enforcement
  "BorderSecurity",
  "BorderWall",
  # Expansive
  "Pathway_Citizenship"
)
df_pass <- df_clean %>% filter(ManipCheck1_result == 1)
mediation_function_survey(
  dvs             = policy_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls_pol,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "ch3_results"
)
mediation_function_survey(
  dvs             = policy_dvs_indiv,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = emotions_all,
  controls        = survey_controls_pol,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "ch3_results_indiv"
)


mediation_function_survey(
  dvs             = policy_dvs_indiv,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = emotions_all,
  controls        = survey_controls_pol,
  dat             = df_pass %>% filter(!is.na(Treatment)),
  out             = "ch3_results_pass"
)

# 
# ### SEM FUNCTION
# sem_survey <- function(dvs, emotions, stigma_measures, controls,
#                        data, estimator = "MLR", out = NULL) {
#   
#   results <- list()
#   
#   for (dv in dvs) {
#     for (em in emotions) {
#       for (stigma in stigma_measures) {
#         
#         ctrl <- paste(controls, collapse = " + ")
#         
#         # Build model string
#         model_str <- paste0("
#         # Treatment --> Emotion (direct path)
#         ", em, " ~ e1*Treatment_cont
#           # Treatment --> Stigma perception
#           ", stigma, " ~ a1*Treatment_cont + ", ctrl, "
# 
#           # Stigma --> Emotion
#           ", em, " ~ b1*", stigma, " + ", ctrl, "
#            # # Treatment --> Emotion (direct path)
#            # ", em, " ~ e1*Anti + e2*Pro
#           # Emotion --> Outcome (with direct stigma path)
#           ", dv, " ~ c1*", em, " + d1*", stigma, " +
#                      Treatment_cont+ ", ctrl, "
# 
#           # Indirect effects
#           #  Treatment --> Stigma --> Emotion --> Outcome
#           indirect_anti := a1 * b1 * c1
# 
#           # Total effects
#           total := a1 * b1 * c1 + (a1 * d1) + e1
#         ")
#         
#         key <- paste0(dv, "_", em, "_", stigma)
#         
#         fit <- tryCatch({
#           sem(model_str, data = data, estimator = estimator, fixed.x = FALSE)
#         }, error = function(e) {
#           message("Model failed for: ", key, " -- ", e$message)
#           NULL
#         })
#         
#         results[[key]] <- fit
#         
#         if (!is.null(fit)) {
#           cat("\n====", key, "====\n")
#           print(summary(fit, standardized = TRUE, fit.measures = TRUE))
#         }
#       }
#     }
#   }
#   
#   if (!is.null(out)) {
#     assign(out, results, envir = .GlobalEnv)
#   } else {
#     return(results)
#   }
# }
# 
# 
# sem_survey(
#   dvs            = policy_dvs,
#   emotions       = survey_emotions,
#   stigma_measures = survey_stigma,
#   controls       = survey_controls_pol,
#   data           = df_clean %>% filter(!is.na(Treatment)),
#   out            = "sem_ch3_results"
# )
# 
# sem_survey_nostigma <- function(dvs, emotions, controls,
#                                 data, estimator = "MLR", out = NULL) {
#   results <- list()
#   
#   for (dv in dvs) {
#     for (em in emotions) {
#       
#       ctrl <- paste(controls, collapse = " + ")
#       
#       model_str <- paste0("
#         # Treatment --> Emotion (direct path)
#         ", em, " ~ a1*Treatment_cont + ", ctrl, "
#         # Emotion --> Outcome
#         ", dv, " ~ b1*", em, " + Treatment_cont + ", ctrl, "
#         # Indirect effect
#         indirect := a1 * b1
#       ")
#       
#       key <- paste0(dv, "_", em)
#       
#       fit <- tryCatch({
#         sem(model_str, data = data, estimator = estimator, fixed.x = FALSE)
#       }, error = function(e) {
#         message("Model failed for: ", key, " -- ", e$message)
#         NULL
#       })
#       
#       results[[key]] <- fit
#       
#       if (!is.null(fit)) {
#         cat("\n====", key, "====\n")
#         print(summary(fit, standardized = TRUE, fit.measures = TRUE))
#       }
#     }
#   }
#   
#   if (!is.null(out)) {
#     assign(out, results, envir = .GlobalEnv)
#   } else {
#     return(results)
#   }
# }
# 
# # Run it
# sem_survey_nostigma(
#   dvs      = belonging_dvs,
#   emotions = survey_emotions,
#   controls = survey_controls,
#   data     = df_clean %>% filter(!is.na(Treatment)),
#   out      = "sem_ch2_nostigma"
# )
# 
# sem_survey_nostigma(
#   dvs      = policy_dvs,
#   emotions = survey_emotions,
#   controls = survey_controls_pol,
#   data     = df_clean %>% filter(!is.na(Treatment)),
#   out      = "sem_ch3_nostigma"
# )


#### trying out interactions 

ch3_int_imm <- lapply(policy_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment * StigmaImm_mean +",
                           "latino_conc_24 + Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Acculturation + Birthplace + GroupDiscImms_clean + Span_Acc + BorderState"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})

ch3_int_imm_indv <- lapply(policy_dvs_indiv, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment * StigmaImm_mean +",
                           "latino_conc_24 + Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Birthplace + Acculturation + Span_Acc + Birthplace + 
                           GroupDiscImms_clean + BorderState"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})

ch3_int_sanity <- lapply(policy_dvs_indiv, function(dv) {
  form <- as.formula(paste(dv, "~ GroupDiscImms_clean * PartyID_5pt +",
                           "Treatment + latino_conc_24 +StigmaImm_mean +  Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           Birthplace + Acculturation + Span_Acc + Birthplace + 
                           BorderState"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})


library(marginaleffects)

# For ProlongedDetention model
# plot_predictions(ch3_int_imm_indv,
#                  condition = c("StigmaImm_mean", "Treatment"))

### appendix --- no stigma
ch3_nostig_imm_indv <- lapply(policy_dvs_indiv, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment +",
                           "Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Acculturation + Span_Acc + BorderState"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})

### appendix --- no border state res
df_borderstates <- df_clean %>% filter(BorderState == 0)

mediation_function_survey(
  dvs             = policy_dvs_indiv,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = emotions_all,
  controls        = survey_controls_pol[-8],
  dat             = df_borderstates %>% filter(!is.na(Treatment)),
  out             = "ch3_results_no.b.s"
)

joy_test <- lm(Emotions_Joy ~ Treatment * BorderState + Age + Sex + Education + Income + 
                 PartyID_5pt + Acculturation + Span_Acc,
   data = df_clean %>% filter(!is.na(Treatment)))
########## Models with Nativity ------------------------------------------
survey_controls_nat <- c("Age", "Sex", "Education", "Income",
                         "PartyID_5pt", "Birthplace", "Span_Acc",
                         "BorderState")
mediation_function_survey(
  dvs             = policy_dvs_indiv,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = emotions_all,
  controls        = survey_controls_nat,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "ch3_results_nat"
)


##### Formatting tables ---------------------------------------------------------
# main tab 1 
stargazer(ch3_results_indiv$outcome_models, type = "latex", 
          dep.var.labels = c("Third Country", "Prolonged Detention",
                             "Detain US Cits.", "ICE Use of Force",
                             "Border Security", "Border Wall", "Pathway for Citizenship"),
          covariate.labels = c("Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Imm. Stigma", "Treatment - Anti", "Treatment - Pro",
                               "Age", "Sex", "Education", "Income", "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "English Dom.", "Border State",
                               "Constant"), out = "main_tab1_ch3.tex"
          )


### main tab 2 

stargazer(ch3_int_imm_indv, type = "latex", 
          dep.var.labels = c("Third Country", "Prolonged Detention",
                             "Detain US Cits.", "ICE Use of Force",
                             "Border Security", "Border Wall", "Pathway for Citizenship"),
          covariate.labels = c("Treatment - Anti", "Treatment - Pro", 
                               "Imm. Stigma", "Age", "Sex", "Education", "Income", 
                               "Anger", 
                               "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "English Dom.", "Border State",
                               "Anti x Imm. Stigma", "Pro x Imm. Stigma",
                               "Constant"), out = "main_tab2_int.tex"
)

# plot_predictions(ch3_int_imm_indv,
                 # condition = c("StigmaImm_mean", "Treatment"))

#### appendix tabs --- 

stargazer(ch3_nostig_imm_indv, type = "latex",
          dep.var.labels = c("Third Country", "Prolonged Detention",
                             "Detain US Cits.", "ICE Use of Force",
                             "Border Security", "Border Wall", "Pathway for Citizenship"),
          covariate.labels = c("Treatment - Anti", "Treatment - Pro", 
                              "Age", "Sex", "Education", "Income", 
                               "Anger", 
                               "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "English Dom.", "Border State",
                               "Constant"), out = "app_nostig.tex"
)

stargazer(ch3_results_no.b.s$outcome_models, type = "latex",
          dep.var.labels = c("Third Country", "Prolonged Detention",
                             "Detain US Cits.", "ICE Use of Force",
                             "Border Security", "Border Wall", "Pathway for Citizenship"),
          covariate.labels = c("Anger", 
                               "Fear", "Shame", "Relief", "Pride", "Joy", "Imm. Stigma",
                               "Treatment - Anti", "Treatment - Pro", 
                               "Age", "Sex", "Education", "Income", 
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "English Dom.",
                               "Constant"), out = "app_nobs.tex"
)



############## Chapter 2 revised tabs ----------------------------------------

mediation_function_survey(
  dvs             = belonging_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls_pol,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "ch2_results"
)

bel <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment + StigmaImm_mean +",
                           "latino_conc_24 + Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Acculturation + Span_Acc + Birthplace + GroupDiscImms_clean + BorderState"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})

bel_int <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment*StigmaImm_mean +",
                           "latino_conc_24 + Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Acculturation + Span_Acc + Birthplace + GroupDiscImms_clean + BorderState"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})

stargazer(ch3_results$stigma_models, type = "latex",
          dep.var.labels = "Imm. Stigma",
          covariate.labels = c("Treatment - Anti",
                               "Treatment - Pro", "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income", "Party",
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Constant"),
          out = "stigma.org.tex"
          )

stargazer(ch3_results$emotion_models, type = "latex", 
          dep.var.labels = c("Anger", "Fear", "Shame", "Relief", "Pride", "Joy"),
          covariate.labels = c("Imm. Stigma", "Treatment - Anti", "Treatment - Pro",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income", "Party",
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Constant"),
          out = "emotions.org.tex")

## internal 
names(ch2_results$outcome_models)
# ── Internal Belonging (BelongingPost) ────────────────────────────────────────

# Internal State - Negative emotions
int_state_neg <- list(
  ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
  ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
  ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame
)

# Internal State - Positive emotions
int_state_pos <- list(
  ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
  ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
  ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy
)

# Internal US - Negative emotions
int_us_neg <- list(
  ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
  ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
  ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame
)

# Internal US - Positive emotions
int_us_pos <- list(
  ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
  ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
  ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy
)

# ── External Belonging (BelongExternal) ──────────────────────────────────────

# External State - Negative emotions
ext_state_neg <- list(
  ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
  ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
  ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame
)

# External State - Positive emotions
ext_state_pos <- list(
  ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
  ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
  ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy
)

# External US - Negative emotions
ext_us_neg <- list(
  ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
  ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
  ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame
)

# External US - Positive emotions
ext_us_pos <- list(
  ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
  ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
  ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy
)

# Quick checks
stargazer(int_state_neg,int_state_pos, type = "latex",
          dep.var.labels = c("Internal Belonging - State"),
          covariate.labels = c("Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Imm. Stigma", "Treatment - Anti", "Treatment - Pro",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income", "Party",
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Constant"),
          out = "int.state.tex")

stargazer(int_us_neg, int_us_pos, type = "latex",
          dep.var.labels = c("Internal Belonging - US"),
          covariate.labels = c("Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Imm. Stigma", "Treatment - Anti", "Treatment - Pro",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income", "Party",
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Constant"),
          out = "int.us.tex"
          )
stargazer(ext_state_neg,ext_state_pos, type = "latex",
          dep.var.labels = c("External Belonging - State"),
          covariate.labels = c("Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Imm. Stigma", "Treatment - Anti", "Treatment - Pro",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income", "Party",
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Constant"),
          out = "ext.state.tex"
          )
stargazer(ext_us_neg, ext_us_pos, type = "latex",
          dep.var.labels = c("External Belonging - US"),
          covariate.labels = c("Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Imm. Stigma", "Treatment - Anti", "Treatment - Pro",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income", "Party",
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Constant"),
          out = "ext.us.tex"
)


### all emotions

stargazer(bel, type = "latex",
          covariate.labels = c("Treatment - Anti", "Treatment - Pro", "Imm. Stigma",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income",  
                               "Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Party",
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Constant"),
          out = "all_bel.tex"
          )

stargazer(bel_int, type = "latex",
          covariate.labels = c("Treatment - Anti", "Treatment - Pro", "Imm. Stigma",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income",  
                               "Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Party",
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Anti x Imm. Stigma",
                               "Pro x Imm. Stigma",
                               "Constant"),
          out = "bel_int.tex"
)


########## Chapter 3 tables ----------------------------------------------------
stargazer(ch3_results_indiv$outcome_models, type = "latex",
          dep.var.labels = c("Prolonged Det.", "Detain US Cits", "Use of Force (ICE)",
                             "Third Country Deport.", "Border Sec.", "Border Wall",
                             "Pathway for Citizenship"),
          covariate.labels = c("Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Imm. Stigma", "Treatment - Anti", "Treatment - Pro",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income", "Party", 
                               "Generation", "English Dom.", "Born in the US",
                               "Imm. Disc.", "Border State", "Constant"),
          out = "ch3_policy.tex"
          )

stargazer(ch3_int_imm_indv, type = "latex",
          dep.var.labels = c("Prolonged Det.", "Detain US Cits", "Use of Force (ICE)",
                   "Third Country Deport.", "Border Sec.", "Border Wall",
                   "Pathway for Citizenship"),
          covariate.labels = c("Treatment - Anti", "Treatment - Pro",
                     "Imm. Stigma", "Str. Stigma Index",
                     "Age", "Sex", "Education", "Income", "Anger", "Fear", 
                     "Shame", "Relief", "Pride", "Joy",
                     "Party",  "Born in the US",
                     "Generation", "English Dom.",
                     "Imm. Disc.", "Border State", "Anti x Imm. Stigma",
                     "Pro x Imm. Stigma", "Constant"),
          out = "ch3_policy.int.tex")

### Figures -----

library(ggeffects)
library(ggplot2)
library(dplyr)
library(marginaleffects)

#-------------------------------------------------
#margins
#-------------------------------------------------
mfx_border <- slopes(
  ch3_int_imm_indv[[5]],
  variables = "Treatment",
  condition = "StigmaImm_mean",
  newdata = datagrid(StigmaImm_mean = seq(1, 2, by = 0.1))
) %>% 
  filter(contrast == "Anti - Control") # Focus on the specific effect

# 2. Plot it
ggplot(mfx_border, aes(x = StigmaImm_mean, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # The "No Effect" line
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 1) +
  labs(
    title = "Strength of 'Anti' Treatment Effect",
    subtitle = "Difference from Control as Immigration Stigma increases",
    x = "Immigration Stigma",
    y = "Marginal Effect (Treatment vs. Control)"
  ) +
  theme_minimal()


mfx_path <- slopes(
  ch3_int_imm_indv[[7]],
  variables = "Treatment",
  condition = "StigmaImm_mean",
  newdata = datagrid(StigmaImm_mean = seq(1, 2, by = 0.1))
) %>% 
  filter(contrast == "Anti - Control") # Focus on the specific effect

# 2. Plot it
ggplot(mfx_path, aes(x = StigmaImm_mean, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # The "No Effect" line
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 1) +
  labs(
    title = "Strength of 'Anti' Treatment Effect",
    subtitle = "Difference from Control as Immigration Stigma increases",
    x = "Immigration Stigma",
    y = "Marginal Effect (Treatment vs. Control)"
  ) +
  theme_minimal()


# 1. Load the library
library(margins)

# 2. Calculate the marginal effects of 'Treatment' at specific levels of Stigma
# We use 'at' to tell R exactly which points to calculate
m_border <- margins(
  ch3_int_imm_indv[[5]], 
  at = list(StigmaImm_mean = seq(1, 5, 0.5)),
  variables = "Treatment"
)

# 3. Create the summary for plotting
# This turns the complex margins object into a simple data frame
m_summary <- summary(m_border) %>%
  filter(factor == "TreatmentPro") # Focus on the Anti vs Control difference

# 4. Plot it using ggplot (this gives you more control than the base plot)
profig_border <- ggplot(m_summary, aes(x = StigmaImm_mean, y = AME)) +
  # The "No Effect" baseline
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  
  # The confidence interval ribbon
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "gray50") +
  
  # The line representing the strength of the effect
  geom_line(color = "black", linewidth = 1) +
  
  # Add points for the specific 'at' values we calculated
  geom_point() +
  
  labs(
    title = "Marginal Effect of Pro-Immigrant Treatment on Support for Border Sec.",
    subtitle = "Difference from Control across levels of Stigma",
    x = "Immigration Stigma",
    y = "Average Marginal Effect (AME)"
  ) +
  theme_bw()



# 1. Calculate Margins for Model 5 (Border Security)
m5 <- margins(ch3_int_imm_indv[[5]], 
              at = list(StigmaImm_mean = seq(1, 5, 0.5)),
              variables = "Treatment")
m5_df <- summary(m5) %>% mutate(Model = "Border Security")

# 2. Calculate Margins for Model 7 (Pathway to Citizenship)
m7 <- margins(ch3_int_imm_indv[[7]], 
              at = list(StigmaImm_mean = seq(1, 5, 0.5)),
              variables = "Treatment")
m7_df <- summary(m7) %>% mutate(Model = "Pathway to Citizenship")

# 3. Combine and Clean
# We filter for the two treatment effects (vs. Control)
plot_df <- bind_rows(m5_df, m7_df) %>%
  filter(factor %in% c("TreatmentAnti", "TreatmentPro")) %>%
  mutate(Treatment_Label = ifelse(factor == "TreatmentAnti", "Anti vs. Control", "Pro vs. Control"))

# 4. The Four-Panel Plot
ggplot(plot_df, aes(x = StigmaImm_mean, y = AME, color = factor, fill = factor)) +
  # The Zero Baseline
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  
  # The Strength of Effect
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  
  # This creates the 2x2 grid (Models on rows, Treatments on columns)
  facet_grid(Model ~ Treatment_Label, scales = "free_y") +
  
  # Aesthetics
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Comparative Strength of Treatment Effects",
    subtitle = "Marginal effects of experimental conditions across reported levels of structural stigma",
    x = "Immigration Stigma",
    y = "Average Marginal Effect (Relative to Control)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none", # Facet labels handle the identification
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


ggsave("figure_int_ch3.pdf", width = 9,
       height = 7)


## chapter 2 fig
# Model labels matching your stargazer output order
bel_labels <- c("Internal Belonging (State)", 
                "Internal Belonging (US)", 
                "External Belonging (State)", 
                "External Belonging (US)")

# Calculate margins for all four models
bel_margins <- lapply(seq_along(bel), function(i) {
  m <- margins(bel[[i]], 
               at = list(latino_conc_24 = seq(
                 min(df_clean$latino_conc_24, na.rm = TRUE),
                 max(df_clean$latino_conc_24, na.rm = TRUE),
                 length.out = 9)),
               variables = "Treatment")
  summary(m) %>% mutate(Model = bel_labels[i])
})

# Combine and filter
plot_df_bel <- bind_rows(bel_margins) %>%
  filter(factor %in% c("TreatmentAnti", "TreatmentPro")) %>%
  mutate(Treatment_Label = ifelse(factor == "TreatmentAnti",
                                  "Anti vs. Control",
                                  "Pro vs. Control"),
         # Order models for cleaner facet display
         Model = factor(Model, levels = bel_labels))

# Plot - now 4 rows x 2 columns
ggplot(plot_df_bel, aes(x = latino_conc_24, y = AME,
                        color = factor, fill = factor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_grid(Model ~ Treatment_Label, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Treatment Effects on Belonging by Stigma Environment",
    subtitle = "Marginal effects of experimental conditions across levels of structural stigma",
    x = "Structural Stigma Index",
    y = "Average Marginal Effect (Relative to Control)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    strip.text.y = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

library(margins)

# 1. Calculate Marginal Effects for the specific model
# We use the 'at' argument to see how Treatment effects change across Stigma levels
m_single <- margins(
  ch3_int_imm_indv[[1]],
  at = list(
    StigmaImm_mean = seq(
      min(df_short$StigmaImm_mean, na.rm = TRUE),
      max(df_short$StigmaImm_mean, na.rm = TRUE),
      length.out = 10 # Smoother line than the original 9
    )
  ),
  variables = "Treatment"
)

# 2. Prepare the data for plotting
plot_df_single <- summary(m_single) %>%
  filter(factor %in% c("TreatmentAnti", "TreatmentPro")) %>%
  mutate(
    Treatment_Label = ifelse(factor == "TreatmentAnti", 
                             "Anti vs. Control", 
                             "Pro vs. Control")
  )

# 3. Create the Visualization
int.plot.ch3 <- ggplot(plot_df_single, 
       aes(x = StigmaImm_mean, 
           y = AME, 
           color = factor, 
           fill = factor)) +
  
  # Reference line at zero (null effect)
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  
  # Confidence Intervals
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.15, 
              color = NA) +
  
  # Main trend lines and points
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  # Separate panels for Anti vs Control and Pro vs Control
  facet_wrap(~ Treatment_Label) +
  
  # Styling
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Effect of Treatment on Prolonged Detention Attitudes",
    subtitle = "Marginal effects moderated by Immigrant Structural Stigma",
    x = "Immigrant Structural Stigma Index",
    y = "Average Marginal Effect (AME)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("figure_int_ch3.pdf", width = 9,
       height = 7)
### df-pass
stargazer(ch3_results_pass$outcome_models, type = "latex",
          dep.var.labels = c("Prolonged Det.", "Detain Cits", "Force",
                             "3rd Country", "Border Sec.", "Border Wall",
                             "Pathway"),
          covariate.labels = c("Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Imm. Stigma","Treatment - Anti", "Treatment - Pro", 
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income",
                               "Party (D $\\longrightarrow$ R)",
                               "Generation","English Dom.", "Born in US", "Imm. Disc.",
                               "Border State",
                               "Constant"), out = "ch3_policy_pass.tex"
)
          

ch3_int_indv_pass <- lapply(policy_dvs_indiv, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment * StigmaImm_mean +",
                           "latino_conc_24 + Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Birthplace + Acculturation + Span_Acc + Birthplace + 
                           GroupDiscImms_clean + BorderState"))
  lm(form, data = df_pass %>% filter(!is.na(Treatment)))
})
stargazer(ch3_int_indv_pass, type = "latex",
          dep.var.labels = c("Prolonged Det.", "Detain Cits", "Force",
                             "3rd Country", "Border Sec.", "Border Wall",
                             "Pathway"),
          covariate.labels = c("Treatment - Anti", "Treatment - Pro", "Imm. Stigma",
                               "Str. Stigma Index",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)", 
                               "Born in US",
                               "Generation", "English Dom.", "Imm. Disc.",
                               "Border State",
                               "Anti x Imm. Stigma",
                               "Pro x Imm. Stigma",
                               "Constant"), out = "ch3_policy_int_pass.tex")

