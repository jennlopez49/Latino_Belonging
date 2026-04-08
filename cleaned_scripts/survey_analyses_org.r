#### Models ----------
# 
# # Belonging
# lm(BelongingPost_state ~ Treatment, data = df_clean 
#    %>% filter(!is.na(Treatment))) %>% summary()
# lm(BelongingPost_US ~ Treatment, data = df_clean
#    %>% filter(!is.na(Treatment))) %>% summary()
# lm(BelongExternal_state ~ Treatment, data = df_clean 
#    %>% filter(!is.na(Treatment))) %>% summary()
# lm(BelongExternal_US ~ Treatment, data = df_clean 
#    %>% filter(!is.na(Treatment))) %>% summary()
# 
# # Policy
# lm(ImmAttitudeHyp_r ~ Treatment, data = df_clean %>% filter(!is.na(Treatment))) %>% summary()
# lm(BorderPoliciesMatrix_1 ~ Treatment, data = df_clean %>% filter(!is.na(Treatment))) %>% summary()

emotions <- c("Emotions_Anger", "Emotions_Fear", "Emotions_Shame",
              "Emotions_Relief", "Emotions_Pride", "Emotions_Joy")
# 
# lapply(emotions, function(e) {
#   form <- as.formula(paste(e, "~ Treatment"))
#   lm(form, data = df_clean %>% filter(!is.na(Treatment))) %>% 
#     broom::tidy() %>%
#     mutate(DV = e)
# }) %>% bind_rows() %>%
#   filter(term != "(Intercept)") %>%
#   dplyr::select(DV, term, estimate, std.error, p.value) %>%
#   mutate(across(where(is.numeric), ~round(., 3)))
# 
# lapply(emotions, function(e) {
#   form <- as.formula(paste("BelongingPost_state ~", e))
#   lm(form, data = df_clean %>% filter(!is.na(Treatment))) %>%
#     broom::tidy() %>%
#     mutate(DV = "BelongingPost_state", Mediator = e)
# }) %>% bind_rows() %>%
#   filter(term != "(Intercept)") %>%
#   dplyr::select(DV, Mediator, estimate, std.error, p.value) %>%
#   mutate(across(where(is.numeric), ~round(., 3)))
# ### STRICT PASS ONLY ---------
# 
# 
# lm(BelongingPost_state ~ Treatment, data = df_pass 
#    %>% filter(!is.na(Treatment))) %>% summary()
# lm(BelongingPost_US ~ Treatment, data = df_pass
#    %>% filter(!is.na(Treatment))) %>% summary()
# lm(BelongExternal_state ~ Treatment, data = df_pass 
#    %>% filter(!is.na(Treatment))) %>% summary()
# lm(BelongExternal_US ~ Treatment, data = df_pass 
#    %>% filter(!is.na(Treatment))) %>% summary()
# 
# # Policy
# lm(ImmAttitudeHyp_r ~ Treatment, data = df_pass %>% filter(!is.na(Treatment))) %>% summary()
# lm(BorderPoliciesMatrix_1 ~ Treatment, data = df_pass %>% filter(!is.na(Treatment))) %>% summary()
# 
# 
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
#           total := a1 * b1 * c1 + (a1 * d1)
#         ")
#         
#         key <- paste0(dv, "_", em, "_", stigma)
#         
#         fit <- tryCatch({
#           sem(model_str, data = data, estimator = estimator)
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

# Define variable sets
survey_emotions   <- c("Emotions_Anger", "Emotions_Fear", "Emotions_Shame",
                       "Emotions_Relief", "Emotions_Pride", "Emotions_Joy")

survey_stigma     <- c("StigmaImm_mean", "StigmaLatino_mean")

survey_controls   <- c("Age", "Sex", "Education", "Income",
                       "PartyID_5pt", "Acculturation")

belonging_dvs     <- c("BelongingPost_state", "BelongingPost_US",
                       "BelongExternal_state", "BelongExternal_US")

policy_dvs        <- c("ImmAttitudeHyp_r", 
                       "BorderSecurity",
                       "Pathway_Citizenship",
                       "BorderPolicyIndex",
                       "InteriorPolicyIndex")
df_pass <- df_clean %>% filter(ManipCheck1_result == 1)
# Chapter 2: Belonging
# sem_survey(
#   dvs            = belonging_dvs,
#   emotions       = survey_emotions,
#   stigma_measures = survey_stigma,
#   controls       = survey_controls,
#   data           = df_clean,
#   out            = "sem_ch2_results"
# )

# Chapter 3: Policy
# sem_survey(
#   dvs            = policy_dvs,
#   emotions       = survey_emotions,
#   stigma_measures = survey_stigma,
#   controls       = survey_controls,
#   data           = df_clean %>% filter(!is.na(Treatment)),
#   out            = "sem_ch3_results"
# )

### trying with mean centered DVs --- 
df_clean <- df_clean %>%
  mutate(
    # Center the stigma measures
    StigmaImm_mean_c = as.numeric(scale(StigmaImm_mean, center = TRUE, scale = FALSE)),
    StigmaLatino_mean_c = as.numeric(scale(StigmaLatino_mean, center = TRUE, scale = FALSE)),
    
    # Create Interaction terms (Treatment * Centered Stigma)
    # Treatment_cont is used as is since it's likely your 0/1/2 or dummy
    int_imm = Treatment_cont * StigmaImm_mean_c,
    int_latino = Treatment_cont * StigmaLatino_mean_c
  )

# Update your stigma_measures list to use the centered ones
survey_stigma_c <- c("StigmaImm_mean_c", "StigmaLatino_mean_c")
# sem_survey(
#   dvs            = belonging_dvs,
#   emotions       = survey_emotions,
#   stigma_measures = survey_stigma_c,
#   controls       = survey_controls,
#   data           = df_clean,
#   out            = "sem_ch2_results"
# )
# 
# sem_survey_interaction <- function(dvs, emotions, stigma_measures, controls, 
#                                    data, estimator = "MLR", out = NULL) {
#   results <- list()
#   
#   for (dv in dvs) {
#     for (em in emotions) {
#       for (stigma in stigma_measures) {
#         
#         # Identify which interaction term to use based on the stigma variable name
#         interaction_term <- ifelse(grepl("Imm", stigma), "int_imm", "int_latino")
#         
#         ctrl <- paste(controls, collapse = " + ")
#         
#         model_str <- paste0("
#           # Path 1: Treatment --> Stigma
#           ", stigma, " ~ a1*Treatment_cont + ", ctrl, "
#           
#           # Path 2: Stigma --> Emotion
#           ", em, " ~ b1*", stigma, " + ", ctrl, "
#           
#           # Path 3: Emotion & Interaction --> Outcome
#           # Here we add the interaction between Treatment and Stigma
#           ", dv, " ~ c1*", em, " + d1*", stigma, " + 
#                      mod1*", interaction_term, " + 
#                      Treatment_cont + ", ctrl, "
#           
#           # Defined Parameters
#           # Indirect effect of treatment via stigma and emotion
#           indirect_path := a1 * b1 * c1
#           
#           # Interaction effect (Moderation)
#           interaction_effect := mod1
#         ")
#         
#         key <- paste0(dv, "_", em, "_", stigma)
#         
#         fit <- tryCatch({
#           # Added cluster argument for your State-level clustering
#           sem(model_str, data = data, estimator = estimator, cluster = "State")
#         }, error = function(e) {
#           message("Model failed for: ", key, " -- ", e$message)
#           NULL
#         })
#         
#         results[[key]] <- fit
#         if (!is.null(fit)) {
#           cat("\n====", key, "====\n")
#           # summarize with fit measures to check if centering improved things
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
# sem_survey_interaction(
#   dvs             = belonging_dvs,
#   emotions        = survey_emotions,
#   stigma_measures = survey_stigma_c, # Use the centered list
#   controls        = survey_controls,
#   data            = df_clean,
#   out             = "sem_ch2_interaction_results"
# )

### Switching to OLS 

mediation_function_survey <- function(dvs, ivs, stigma_measures, 
                                      emotions, controls, dat, out = NULL) {
  
  mediation_results <- list()
  
  ## Step 1: Treatment --> Stigma (IV --> Stigma)
  stigma_models <- list()
  for (S in stigma_measures) {
    for (X in ivs) {
      form_stigma <- as.formula(paste(S, "~", X, "+", 
                                      paste(controls, collapse = " + ")))
      stigma_models[[paste0("IV_", X, "_Stigma_", S)]] <- 
        lm(form_stigma, data = dat)
    }
  }
  
  ## Step 2: Stigma --> Emotion (Stigma + IV --> Emotion)
  emotion_models <- list()
  for (M in emotions) {
    for (S in stigma_measures) {
      for (X in ivs) {
        form_emotion <- as.formula(paste(M, "~", S, "+", X, "+",
                                         paste(controls, collapse = " + ")))
        emotion_models[[paste0("IV_", X, "_Stigma_", S, "_Em_", M)]] <- 
          lm(form_emotion, data = dat)
      }
    }
  }
  
  ## Step 3: Emotion --> DV (Emotion + Stigma + IV --> DV)
  outcome_models <- list()
  for (Y in dvs) {
    for (M in emotions) {
      for (S in stigma_measures) {
        for (X in ivs) {
          form_outcome <- as.formula(paste(Y, "~", M, "+", S, "+", X, "+",
                                           paste(controls, collapse = " + ")))
          outcome_models[[paste0("DV_", Y, "_IV_", X, 
                                 "_Stigma_", S, "_Em_", M)]] <- 
            lm(form_outcome, data = dat)
        }
      }
    }
  }
  
  mediation_results$stigma_models  <- stigma_models
  mediation_results$emotion_models <- emotion_models
  mediation_results$outcome_models <- outcome_models
  
  if (!is.null(out)) {
    if (!is.character(out)) stop("Argument 'out' must be a character string")
    assign(out, mediation_results, envir = .GlobalEnv)
  } else {
    return(mediation_results)
  }
}

mediation_function_survey(
  dvs             = belonging_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "med_ch2_results"
)

imm_med_mods <- list(med_ch2_results$stigma_models$IV_Treatment_Stigma_StigmaImm_mean,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)


stigma_imm_mods <- list(med_ch2_results$stigma_models$IV_Treatment_Stigma_StigmaImm_mean,
                     med_ch2_results$stigma_models$IV_Treatment_Stigma_StigmaLatino_mean)
bel_em_imm_mods <- list(med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)
bel_em_lat_mods <- list(med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                     med_ch2_results$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)
belstate_imm_out <- list(med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)
belstate_lat_out <- list(med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                 med_ch2_results$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)

belUS_imm_out <- list(med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)
belUS_lat_out <- list(med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                         med_ch2_results$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)
belext_state_imm <- list(med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                         med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                         med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                         med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                         med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                         med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)

belext_state_lat <- list(med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                        med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                        med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                        med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                        med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                        med_ch2_results$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)
belext_US_imm <- list(med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)
belext_US_lat <- list(med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                      med_ch2_results$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)


### Models only on Strict Pass Sample 
df_pass$Treatment <- relevel(factor(df_pass$Treatment), ref = "Control")
mediation_function_survey(
  dvs             = belonging_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls,
  dat             = df_pass,
  out             = "ch2_pass"
)

stigma_pass_mods <- list(ch2_pass$stigma_models$IV_Treatment_Stigma_StigmaImm_mean,
                        ch2_pass$stigma_models$IV_Treatment_Stigma_StigmaLatino_mean)
pass_em_imm_mods <- list(ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)
pass_em_lat_mods <- list(ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                        ch2_pass$emotion_models$IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)
passstate_imm_out <- list(ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)
passstate_lat_out <- list(ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                         ch2_pass$outcome_models$DV_BelongingPost_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)

passUS_imm_out <- list(ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)
passUS_lat_out <- list(ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                      ch2_pass$outcome_models$DV_BelongingPost_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)
passext_state_imm <- list(ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)

passext_state_lat <- list(ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                         ch2_pass$outcome_models$DV_BelongExternal_state_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)
passbelext_US_imm <- list(ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy)
passbelext_US_lat <- list(ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Anger,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Fear,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Shame,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Relief,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Pride,
                      ch2_pass$outcome_models$DV_BelongExternal_US_IV_Treatment_Stigma_StigmaLatino_mean_Em_Emotions_Joy)


## OUTPUT TABLES FOR CH. 2 --- BREAKDOWN FULL SAMPLE AS MAIN TABLES, ONLY IMMIGRANT STIGMA MEASURES ======
## FULL SAMPLE LATINO STIGMA TABLES IN APPENDIX ----
## STRICT PASS SAMPLE (ONLY RESPONDENTS WHO IDENTIFIED IMMIGRANT FRAME) FULL TABLE IN APPENDIX =======

## main table --- mediators: perception of stigmatization & emotions

stargazer(imm_med_mods, type = "latex", dep.var.labels = c("Imm. Stigma Perception",
          "Anger", "Fear", "Shame", "Relief", "Pride", "Joy"), 
          covariate.labels = c("Imm. Stigma Perception", "Treatment - Anti", 
                               "Treatment - Pro", "Age", "Sex", "Education",
                               "Income",
                               "Party (D $\\longrightarrow$ R)", 
                               "Acculturation", 
                               "Constant"), 
          out = "main_med_ch2.tex")

## main table internal belonging --- state

stargazer(belstate_imm_out, belUS_imm_out, type = "latex", dep.var.caption = "Internal Belonging",
          dep.var.labels = c("State", "United States"),
          covariate.labels = c( "Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                                "Imm. Stigma Perception", "Treatment - Anti", 
                               "Treatment - Pro", "Age", "Sex", "Education",
                               "Income",
                               "Party (D $\\longrightarrow$ R)", 
                               "Acculturation", 
                               "Constant"), 
          out = "main_int_ch2.tex")

stargazer(belstate_lat_out, belUS_lat_out, type = "text", 
          dep.var.caption = "Internal Belonging",
          dep.var.labels = c("State", "United States"),
          covariate.labels = c( "Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                                "Latino Stigma Perception", "Treatment - Anti", 
                                "Treatment - Pro", "Age", "Sex", "Education",
                                "Income",
                                "Party (D $\\longrightarrow$ R)", 
                                "Acculturation", 
                                "Constant"), 
          out = "app_int_ch2.tex")

stargazer(belext_state_imm, belext_US_imm, type = "latex", 
          dep.var.caption = "External Belonging",
          dep.var.labels = c("State", "United States"),
          covariate.labels = c( "Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                                "Imm. Stigma Perception", "Treatment - Anti", 
                                "Treatment - Pro", "Age", "Sex", "Education",
                                "Income",
                                "Party (D $\\longrightarrow$ R)", 
                                "Acculturation", 
                                "Constant"), 
          out = "main_ext_ch2.tex")

stargazer(belext_state_lat, belext_US_lat, type = "latex", 
          dep.var.caption = "External Belonging",
          dep.var.labels = c("State", "United States"),
          covariate.labels = c( "Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                                "Latino Stigma Perception", "Treatment - Anti", 
                                "Treatment - Pro", "Age", "Sex", "Education",
                                "Income",
                                "Party (D $\\longrightarrow$ R)", 
                                "Acculturation", 
                                "Constant"), 
          out = "app_ext_lat_ch2.tex")


### interactions -------------------------- stigma x treatment --> outcome?
## all emotions in one model
belonging_dvs <- c("BelongingPost_state", "BelongingPost_US",
                   "BelongExternal_state", "BelongExternal_US")

## basic models first ----
belong_em <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment + StigmaImm_mean +",
                           "Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Acculturation"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})
names(belong_em) <- belonging_dvs
state_belong <- list(belong_em$BelongingPost_state, belong_em$BelongExternal_state)
US_belong <- list(belong_em$BelongingPost_US, belong_em$BelongExternal_US)
re_belong_em <- c(state_belong, US_belong)

belong_em_app <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment + StigmaLatino_mean +",
                           "Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Acculturation"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})
names(belong_em_app) <- belonging_dvs
state_belong_app <- list(belong_em_app$BelongingPost_state, belong_em_app$BelongExternal_state)
state_belong_app <- list(belong_em_app$BelongingPost_US, belong_em_app$BelongExternal_US)
re_belong_em_app <- c(state_belong_app, state_belong_app)

## main table -----------------------------------------------------------------
# will have to manually edit in latex
stargazer(re_belong_em,
          type = "latex",
          dep.var.caption = "Belonging", dep.var.labels = c("Internal", "External", "Internal", "External"),
          column.labels = c("State", "US"),
          column.separate = c(2, 2), covariate.labels = c("Anti", "Pro",
          "Imm. Stigma",
          "Age", "Sex", "Education", "Income",
          "Anger", "Fear", "Shame",
          "Relief", "Pride", "Joy",
          "Party (D $\\longrightarrow$ R)", "Acculturation", "Constant"),
          out = "main_em.tex")

stargazer(re_belong_em_app,
          type = "latex",
          dep.var.caption = "Belonging", dep.var.labels = c("Internal", "External", "Internal", "External"),
          column.labels = c("State", "US"),
          column.separate = c(2, 2), covariate.labels = c("Anti", "Pro",
                                                          "Latino Stigma",
                                                          "Age", "Sex", "Education", "Income",
                                                          "Anger", "Fear", "Shame",
                                                          "Relief", "Pride", "Joy",
                                                          "Party (D $\\longrightarrow$ R)", "Acculturation",
                                                          "Constant"),
          out = "app_em.tex"
          )
# Treatment x StigmaImm interaction
belong_int_imm <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment * StigmaImm_mean +",
                           "Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Acculturation"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})
names(belong_int_imm) <- belonging_dvs

# Treatment x StigmaLatino interaction
belong_int_lat <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment * StigmaLatino_mean +",
                           "Age + Sex + Education + Income + Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                       Emotions_Relief + Emotions_Pride + Emotions_Joy + 
                           PartyID_5pt + Acculturation"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
})
names(belong_int_lat) <- belonging_dvs

# Quick results check
lapply(belong_int_imm, function(m) {
  broom::tidy(m) %>%
    filter(str_detect(term, "Treatment|Stigma")) %>%
    dplyr::select(term, estimate, std.error, p.value) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
})

# Tables
stargazer(belong_int_imm,
          type = "latex",
          dep.var.labels = c("State (Internal)", "US (Internal)",
                             "State (External)", "US (External)"),
          dep.var.caption = "Dependent Variable: Belonging",
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)", "Acculturation",
                               "Anti x Stigma",
                               "Pro x Stigma",
                               "Constant"),
          label = "belong.int.imm",
          out = "belong_inter_imm.tex")

stargazer(belong_int_lat,
          type = "latex",
          dep.var.labels = c("State (Internal)", "US (Internal)",
                             "State (External)", "US (External)"),
          dep.var.caption = "Dependent Variable: Belonging",
          covariate.labels = c("Anti", "Pro",
                               "Latino Stigma",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)", "Acculturation",
                               "Anti x Stigma",
                               "Pro x Stigma",
                               "Constant"),
          label = "belong.int.lat",
          out = "belong_inter_lat.tex")

### one emotion at a time
# Treatment x StigmaImm x Emotion interaction
belong_int_imm_em <- lapply(belonging_dvs, function(dv) {
  lapply(survey_emotions, function(em) {
    form <- as.formula(paste(dv, "~ Treatment * StigmaImm_mean +",
                             em, "+",
                             "Age + Sex + Education + Income +",
                             "PartyID_5pt + Acculturation"))
    lm(form, data = df_clean %>% filter(!is.na(Treatment)))
  }) %>% setNames(survey_emotions)
}) %>% setNames(belonging_dvs)

# Treatment x StigmaLatino x Emotion interaction
belong_int_lat_em <- lapply(belonging_dvs, function(dv) {
  lapply(survey_emotions, function(em) {
    form <- as.formula(paste(dv, "~ Treatment * StigmaLatino_mean +",
                             em, "+",
                             "Age + Sex + Education + Income +",
                             "PartyID_5pt + Acculturation"))
    lm(form, data = df_clean %>% filter(!is.na(Treatment)))
  }) %>% setNames(survey_emotions)
}) %>% setNames(belonging_dvs)

# Quick check -- interaction and emotion terms only
lapply(belonging_dvs, function(dv) {
  lapply(survey_emotions, function(em) {
    broom::tidy(belong_int_imm_em[[dv]][[em]]) %>%
      filter(str_detect(term, "Treatment|Stigma|Emotions")) %>%
      dplyr::select(term, estimate, std.error, p.value) %>%
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      mutate(DV = dv, Emotion = em)
  }) %>% bind_rows()
}) %>% bind_rows() %>%
  filter(p.value < 0.05) %>%
  arrange(DV, Emotion, term)

bel_state_int_em <- belong_int_imm_em$BelongingPost_state
bel_US_int_em <- belong_int_imm_em$BelongingPost_US
belext_state_int_em <- belong_int_imm_em$BelongExternal_state
belext_US_int_em <- belong_int_imm_em$BelongExternal_state

stargazer(bel_state_int_em, bel_US_int_em, type = "latex",
          dep.var.labels = c("State", "United States"),
          dep.var.caption = "Internal Belonging",
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma",
                               "Anger", "Fear", "Shame","Pride",
                               "Relief", "Pride","Joy", "Age", "Sex", "Education", "Income",
                               "Party (D $\\longrightarrow$ R)", "Acculturation",
                               "Anti x Stigma",
                               "Pro x Stigma",
                               "Constant"), out = "inter_em_int_imm.tex")
stargazer(belext_state_int_em, belext_US_int_em, type = "latex",
          dep.var.labels = c("State", "United States"),
          dep.var.caption = "External Belonging",
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride","Joy", "Age", "Sex", 
                               "Education", "Income",
                               "Party (D $\\longrightarrow$ R)", "Acculturation",
                               "Anti x Stigma",
                               "Pro x Stigma",
                               "Constant"), out = "exter_em_int_imm.tex")

################### CHAPTER 3 ANALYSES #########################################
survey_controls_pol <- c("Age", "Sex", "Education", "Income",
                        "PartyID_5pt", "Acculturation", 
                        "BorderState")
mediation_function_survey(
  dvs             = policy_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls_pol,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "ch3_results"
)


## all emotions in one model 

intbel_allem_state <- lm(BelongingPost_state ~ Treatment + StigmaImm_mean + 
                           Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame +
                         Emotions_Relief + Emotions_Pride + Emotions_Joy +
                           Age + Sex + Education + Income + 
                         PartyID_5pt + Acculturation +
                         BorderState, data = df_clean)


intbel_allem_US <- lm(BelongingPost_US ~ Treatment + StigmaImm_mean + 
                           Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame + 
                         Emotions_Relief + Emotions_Pride + Emotions_Joy +
                           Age + Sex + Education + Income + 
                         PartyID_5pt + Acculturation +
                           BorderState, data = df_clean)

extbel_allem_state <- lm(BelongExternal_state ~ Treatment + StigmaImm_mean + 
                           Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame +
                           Emotions_Relief + Emotions_Pride + Emotions_Joy +
                           Age + Sex + Education + Income + 
                           PartyID_5pt + Acculturation +
                           BorderState, data = df_clean)


extbel_allem_US <- lm(BelongExternal_US ~ Treatment + StigmaImm_mean + 
                        Emotions_Anger + 
                        Emotions_Fear + Emotions_Shame + 
                        Emotions_Relief + Emotions_Pride + Emotions_Joy +
                        Age + Sex + Education + Income +
                        PartyID_5pt + Acculturation +
                        BorderState, data = df_clean)
## interactions ----

intbel_allem_state_int <- lm(BelongingPost_state ~ Treatment*StigmaImm_mean + 
                           Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame +
                           Emotions_Relief + Emotions_Pride + Emotions_Joy +
                           Age + Sex + Education + Income +
                           PartyID_5pt + Acculturation +
                           BorderState, data = df_clean)


intbel_allem_US_int <- lm(BelongingPost_US ~ Treatment*StigmaImm_mean + 
                        Emotions_Anger + 
                        Emotions_Fear + Emotions_Shame + 
                        Emotions_Relief + Emotions_Pride + Emotions_Joy +
                        Age + Sex + Education + Income +
                        PartyID_5pt + Acculturation +
                        BorderState, data = df_clean)

extbel_allem_state_int <- lm(BelongExternal_state ~ Treatment*StigmaImm_mean + 
                           Emotions_Anger + 
                           Emotions_Fear + Emotions_Shame +
                           Emotions_Relief + Emotions_Pride + Emotions_Joy +
                           Age + Sex + Education + Income +
                           PartyID_5pt + Acculturation +
                           BorderState, data = df_clean)


extbel_allem_US_int <- lm(BelongExternal_US ~ Treatment*StigmaImm_mean + 
                        Emotions_Anger + 
                        Emotions_Fear + Emotions_Shame + 
                        Emotions_Relief + Emotions_Pride + Emotions_Joy +
                        Age + Sex + Education + Income +
                        PartyID_5pt + Acculturation +
                        BorderState, data = df_clean)

## appendix / (maybe main) table with all belonging models with all emotions 
bel_full <- list(intbel_allem_state, intbel_allem_US,extbel_allem_state, 
                 extbel_allem_US, intbel_allem_state_int, intbel_allem_US_int,
                 extbel_allem_state_int, 
                 extbel_allem_US_int)

stargazer(bel_full, type = "latex", dep.var.labels = c("Internal - State",
                                                           "Internal - US",
                                                           "External - State",
                                                           "External - US",
                                                           "Internal - State",
                                                           "Internal - US",
                                                           "External - State",
                                                           "External - US"),
          dep.var.caption = "Internal and External Belonging",
          covariate.labels = c("Treatment - Anti", "Treatment - Pro",
                               "Imm. Stigma",
                               "Anger", "Fear", "Shame", "Relief", "Pride", "Joy",
                               "Age", "Sex", "Education", "Income",
          "Party (D $\\longrightarrow$ R)", 
          "Acculturation", 
          "Border State", "Treatment (Anti) x Imm. Stigma",
          "Treatment (Pro) x Imm. Stigma",
          "Constant"), out = "ch2_all_em_org.tex")


###### RE-RUNNING MODELS WITH STIGMA CONTEXT 
## ============================================================

## --- 1. MEDIATOR MODELS (for main_med_ch2) ---
stigma_c <-med_ch2_results$stigma_models$IV_Treatment_Stigma_StigmaImm_mean
# Stigma model
stigma_cc <- lm(StigmaImm_mean ~ Treatment + latino_conc_24 +
                  Age + Sex + Education + Income +
                  PartyID_5pt + Acculturation,
                data = df_clean %>% filter(!is.na(Treatment)))

mlm_stigma <- lmer(StigmaImm_mean ~ Treatment +
                     latino_conc_24 +  # start linear, see if it holds
                     Age + Sex + Education + Income +
                     PartyID_5pt + Acculturation +
                     (1 | State_char),
                   data = df_clean %>% filter(!is.na(Treatment)))

summary(mlm_stigma)

library(clubSandwich)
model_data <- model.frame(mlm_stigma)
# CR2 is the recommended type for small numbers of clusters
mlm <- coef_test(mlm_stigma, vcov = "CR2", cluster = model_data$State_char)

# state_level <- df_clean %>%
#   group_by(State_char) %>%
#   summarize(
#     mean_stigma = mean(StigmaImm_mean, na.rm = TRUE),
#     exposure = mean(latino_conc_24, na.rm = TRUE),
#     n = n()
#   )

lm_state <- lm(mean_stigma ~ exposure, data = state_level)
summary(lm_state)

library(modelsummary)
robust_vcov <- vcovCR(mlm_stigma, cluster = model_data$State_char, type = "CR2")
modelsummary(
  list("Basic" = stigma_c,
    "OLS" = stigma_cc,
       "MLM" = mlm_stigma,
       "MLM (Robust SE)" = mlm_stigma,
       "State OLS" = lm_state),
  vcov = list("classical",   # Basic
              "classical", "classical",mlm$SE,
              "classical"),
  stars = c("*" = .05, "**" = .01, "***" = .001),
  gof_map = c("nobs", "icc", "r.squared", "adj.r.squared"),
  coef_rename = c(
    "latino_conc_24" = "Structural Exposure",
    "TreatmentAnti" = "Anti Treatment",
    "TreatmentPro" = "Pro Treatment",
    "PartyID_5pt" = "Party ID",
    "Acculturation" = "Acculturation",
    "(Intercept)" = "Intercept"
  ),
  coef_omit = NULL, # or list specific ones to drop
  notes = list("Robust SEs clustered at state level (CR2).",
               "State OLS uses state-level means as unit of analysis."),
  output = "latex_tabular"
)

# Emotion models
emotions_cc <- lapply(survey_emotions, function(em) {
  form <- as.formula(paste(em, "~ StigmaImm_mean + Treatment + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "PartyID_5pt + Acculturation"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
}) %>% setNames(survey_emotions)

imm_med_mods_cc <- c(
  list(stigma_cc),
  list(emotions_cc$Emotions_Anger,
       emotions_cc$Emotions_Fear,
       emotions_cc$Emotions_Shame,
       emotions_cc$Emotions_Relief,
       emotions_cc$Emotions_Pride,
       emotions_cc$Emotions_Joy)
)

imm_med_mods_interleaved <- c(rbind(imm_med_mods, imm_med_mods_cc))

stargazer(imm_med_mods_interleaved[3:8], type = "latex",
          dep.var.labels = c("Anger", "Fear",
                             "Shame"),
          covariate.labels = c("Imm. Stigma Perception", "Treatment - Anti",
                               "Treatment - Pro", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Constant"),
          longtable = TRUE, float = FALSE,
          out = "main_med_neg_cc.tex")

# Table 2: Positive Emotions (Relief, Pride, Joy)
# cols: relief(9), relief_cc(10), pride(11), pride_cc(12), joy(13), joy_cc(14)
stargazer(imm_med_mods_interleaved[9:14], type = "latex",
          dep.var.labels = c("Relief", "Relief",
                             "Pride", "Pride",
                             "Joy", "Joy"),
          covariate.labels = c("Imm. Stigma Perception", "Treatment - Anti",
                               "Treatment - Pro", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Constant"),
          longtable = TRUE, float = FALSE,
          out = "main_med_pos_cc.tex")


## --- 2. ALL-EMOTIONS MODELS (for main_em) ---
## These mirror belong_em but add class.conc_lat_14_16

belong_em_cc <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment + StigmaImm_mean + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "Emotions_Anger + Emotions_Fear + Emotions_Shame +",
                           "Emotions_Relief + Emotions_Pride + Emotions_Joy +",
                           "PartyID_5pt + Acculturation"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
}) %>% setNames(belonging_dvs)

state_belong_cc <- list(belong_em_cc$BelongingPost_state, 
                        belong_em_cc$BelongExternal_state)
US_belong_cc    <- list(belong_em_cc$BelongingPost_US,    
                        belong_em_cc$BelongExternal_US)
re_belong_em_cc <- c(state_belong_cc, US_belong_cc)

re_belong_em_interleaved <- list(
  belong_em$BelongingPost_state,     belong_em_cc$BelongingPost_state,
  belong_em$BelongExternal_state,    belong_em_cc$BelongExternal_state,  # <-- swapped
  belong_em$BelongingPost_US,        belong_em_cc$BelongingPost_US,
  belong_em$BelongExternal_US,       belong_em_cc$BelongExternal_US
)

stargazer(re_belong_em_interleaved,
          type = "latex",
          dep.var.caption = "Belonging", 
          dep.var.labels = c("Internal", "External", "Internal", "External"),
          column.labels = c("State", "US"),
          column.separate = c(4, 4),
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)", "Acculturation",
                               "Constant"),
          out = "main_em_cc.tex")


## --- 3. INTERACTION MODELS (for belong_inter_imm) ---
## These mirror belong_int_imm but add class.conc_lat_14_16

belong_int_imm_cc <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment * StigmaImm_mean + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "Emotions_Anger + Emotions_Fear + Emotions_Shame +",
                           "Emotions_Relief + Emotions_Pride + Emotions_Joy +",
                           "PartyID_5pt + Acculturation"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
}) %>% setNames(belonging_dvs)

belong_int_imm_interleaved <- list(
  belong_int_imm$BelongingPost_state,    belong_int_imm_cc$BelongingPost_state,
  belong_int_imm$BelongingPost_US,       belong_int_imm_cc$BelongingPost_US,
  belong_int_imm$BelongExternal_state,   belong_int_imm_cc$BelongExternal_state,
  belong_int_imm$BelongExternal_US,      belong_int_imm_cc$BelongExternal_US
)

stargazer(belong_int_imm_interleaved,
          type = "latex",
          dep.var.caption = "Belonging", 
          dep.var.labels = c("Internal", "External", "Internal", "External"),
          column.labels = c("State", "US"),
          column.separate = c(4, 4),
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)", "Acculturation",
                               "Anti x Stigma",
                               "Pro x Stigma",
                               "Constant"),
          label = "belong.int.imm.cc",
          out = "belong_inter_imm_cc.tex")


# ============================================================
# APPENDIX TABLES — MODELS WITH LINKED FATE
# All models identical to main text but include LinkedFate_r
# ============================================================

survey_controls_lf <- c("Age", "Sex", "Education", "Income",
                        "PartyID_5pt", "Acculturation", "LinkedFate_r")

# --- 1. STIGMA MODELS WITH LINKED FATE ---

stigma_c_app <- lm(StigmaImm_mean ~ Treatment +
                     Age + Sex + Education + Income +
                     PartyID_5pt + Acculturation + LinkedFate_r,
                   data = df_clean %>% filter(!is.na(Treatment)))

stigma_cc_app <- lm(StigmaImm_mean ~ Treatment + latino_conc_24 +
                      Age + Sex + Education + Income +
                      PartyID_5pt + Acculturation + LinkedFate_r,
                    data = df_clean %>% filter(!is.na(Treatment)))

mlm_stigma_app <- lmer(StigmaImm_mean ~ Treatment +
                         latino_conc_24 +
                         Age + Sex + Education + Income +
                         PartyID_5pt + Acculturation + LinkedFate_r +
                         (1 | State_char),
                       data = df_clean %>% filter(!is.na(Treatment)))

# State OLS with linked fate
state_level_app <- df_clean %>%
  group_by(State_char) %>%
  summarize(
    mean_stigma = mean(StigmaImm_mean, na.rm = TRUE),
    exposure = mean(latino_conc_24, na.rm = TRUE),
    mean_lf = mean(LinkedFate_r, na.rm = TRUE),
    n = n()
  )

lm_state_app <- lm(mean_stigma ~ exposure + mean_lf, data = state_level_app)

# Robust vcov for appendix MLM
model_data_app <- model.frame(mlm_stigma_app)
robust_vcov_app <- vcovCR(mlm_stigma_app, 
                          cluster = model_data_app$State_char, 
                          type = "CR2")

modelsummary(
  list("Basic" = stigma_c_app,
       "OLS" = stigma_cc_app,
       "MLM" = mlm_stigma_app,
       "MLM (Robust SE)" = mlm_stigma_app,
       "State OLS" = lm_state_app),
  vcov = list("classical",
              "classical",
              "classical",
              robust_vcov_app,
              "classical"),
  stars = c("*" = .05, "**" = .01, "***" = .001),
  gof_map = c("nobs", "icc", "r.squared", "adj.r.squared"),
  coef_rename = c(
    "latino_conc_24" = "Structural Exposure",
    "TreatmentAnti" = "Anti Treatment",
    "TreatmentPro" = "Pro Treatment",
    "LinkedFate_r" = "Linked Fate",
    "mean_lf" = "Linked Fate",
    "PartyID_5pt" = "Party ID",
    "Acculturation" = "Acculturation",
    "(Intercept)" = "Intercept"
  ),
  notes = list("Robust SEs clustered at state level (CR2).",
               "State OLS uses state-level means as unit of analysis.",
               "Models include linked fate as covariate; see main text for discussion."),
  output = "mlm_stigma_app.tex"
)

# --- 2. EMOTION MODELS WITH LINKED FATE ---

mediation_function_survey(
  dvs             = belonging_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls_lf,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "med_ch2_results_app"
)

imm_med_mods_app <- list(
  stigma_c_app,
  med_ch2_results_app$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
  med_ch2_results_app$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
  med_ch2_results_app$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
  med_ch2_results_app$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
  med_ch2_results_app$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
  med_ch2_results_app$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy
)

# Emotion models with concrete index — negative emotions
emotions_cc_app <- lapply(survey_emotions, function(em) {
  form <- as.formula(paste(em, "~ StigmaImm_mean + Treatment + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "PartyID_5pt + Acculturation + LinkedFate_r"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
}) %>% setNames(survey_emotions)

imm_med_mods_cc_app <- c(
  list(stigma_cc_app),
  list(emotions_cc_app$Emotions_Anger,
       emotions_cc_app$Emotions_Fear,
       emotions_cc_app$Emotions_Shame,
       emotions_cc_app$Emotions_Relief,
       emotions_cc_app$Emotions_Pride,
       emotions_cc_app$Emotions_Joy)
)

imm_med_mods_interleaved_app <- c(rbind(imm_med_mods_app, imm_med_mods_cc_app))

# Negative emotions table
stargazer(imm_med_mods_interleaved_app[3:8],
          type = "latex",
          dep.var.labels = c("Anger", "Fear", "Shame"),
          covariate.labels = c("Imm. Stigma Perception", "Treatment - Anti",
                               "Treatment - Pro", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Linked Fate", "Constant"),
          longtable = TRUE, float = FALSE,
          out = "main_med_neg_cc_app.tex")

# Positive emotions table
stargazer(imm_med_mods_interleaved_app[9:14],
          type = "latex",
          dep.var.labels = c("Relief", "Relief", "Pride", "Pride", "Joy", "Joy"),
          covariate.labels = c("Imm. Stigma Perception", "Treatment - Anti",
                               "Treatment - Pro", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Linked Fate", "Constant"),
          longtable = TRUE, float = FALSE,
          out = "main_med_pos_cc_app.tex")

# --- 3. BELONGING MODELS WITH LINKED FATE ---

belong_em_app_lf <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment + StigmaImm_mean +",
                           "Age + Sex + Education + Income +",
                           "Emotions_Anger + Emotions_Fear + Emotions_Shame +",
                           "Emotions_Relief + Emotions_Pride + Emotions_Joy +",
                           "PartyID_5pt + Acculturation + LinkedFate_r"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
}) %>% setNames(belonging_dvs)

belong_em_cc_app_lf <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment + StigmaImm_mean + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "Emotions_Anger + Emotions_Fear + Emotions_Shame +",
                           "Emotions_Relief + Emotions_Pride + Emotions_Joy +",
                           "PartyID_5pt + Acculturation + LinkedFate_r"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
}) %>% setNames(belonging_dvs)

re_belong_em_interleaved_app <- list(
  belong_em_app_lf$BelongingPost_state,
  belong_em_cc_app_lf$BelongingPost_state,
  belong_em_app_lf$BelongExternal_state,
  belong_em_cc_app_lf$BelongExternal_state,
  belong_em_app_lf$BelongingPost_US,
  belong_em_cc_app_lf$BelongingPost_US,
  belong_em_app_lf$BelongExternal_US,
  belong_em_cc_app_lf$BelongExternal_US
)

stargazer(re_belong_em_interleaved_app,
          type = "latex",
          dep.var.caption = "Belonging",
          dep.var.labels = c("Internal", "External", "Internal", "External"),
          column.labels = c("State", "US"),
          column.separate = c(4, 4),
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Linked Fate", "Constant"),
          out = "main_em_cc_app.tex")

# --- 4. INTERACTION MODELS WITH LINKED FATE ---

belong_int_imm_app_lf <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment * StigmaImm_mean + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "Emotions_Anger + Emotions_Fear + Emotions_Shame +",
                           "Emotions_Relief + Emotions_Pride + Emotions_Joy +",
                           "PartyID_5pt + Acculturation + LinkedFate_r"))
  lm(form, data = df_clean %>% filter(!is.na(Treatment)))
}) %>% setNames(belonging_dvs)

belong_int_imm_interleaved_app <- list(
  belong_int_imm$BelongingPost_state,
  belong_int_imm_app_lf$BelongingPost_state,
  belong_int_imm$BelongingPost_US,
  belong_int_imm_app_lf$BelongingPost_US,
  belong_int_imm$BelongExternal_state,
  belong_int_imm_app_lf$BelongExternal_state,
  belong_int_imm$BelongExternal_US,
  belong_int_imm_app_lf$BelongExternal_US
)

stargazer(belong_int_imm_interleaved_app,
          type = "latex",
          dep.var.caption = "Belonging",
          dep.var.labels = c("Internal", "External", "Internal", "External"),
          column.labels = c("State", "US"),
          column.separate = c(4, 4),
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Linked Fate",
                               "Anti x Stigma", "Pro x Stigma",
                               "Constant"),
          label = "belong.int.imm.cc.app",
          out = "belong_inter_imm_cc_app.tex")

################### APPENDIX TABLES ---- STRICT PASS SAMPLE ONLY ---------------
# ============================================================
# STRICT PASS SAMPLE MODELS — WITHOUT LINKED FATE
# Only respondents who identified immigration as primary topic
# ============================================================

# Make sure Treatment is releveled
df_pass$Treatment <- relevel(factor(df_pass$Treatment), ref = "Control")

# --- 1. STIGMA MODELS --- 

stigma_c_pass <- lm(StigmaImm_mean ~ Treatment +
                      Age + Sex + Education + Income +
                      PartyID_5pt + Acculturation,
                    data = df_pass)

stigma_cc_pass <- lm(StigmaImm_mean ~ Treatment + latino_conc_24 +
                       Age + Sex + Education + Income +
                       PartyID_5pt + Acculturation,
                     data = df_pass)

mlm_stigma_pass <- lmer(StigmaImm_mean ~ Treatment +
                          latino_conc_24 +
                          Age + Sex + Education + Income +
                          PartyID_5pt + Acculturation +
                          (1 | State_char),
                        data = df_pass)

# State OLS pass sample
state_level_pass <- df_pass %>%
  group_by(State_char) %>%
  summarize(
    mean_stigma = mean(StigmaImm_mean, na.rm = TRUE),
    exposure    = mean(latino_conc_24, na.rm = TRUE),
    n           = n()
  )

lm_state_pass <- lm(mean_stigma ~ exposure, data = state_level_pass)

# Robust SEs for pass sample MLM
model_data_pass <- model.frame(mlm_stigma_pass)
robust_vcov_pass <- vcovCR(mlm_stigma_pass,
                           cluster = model_data_pass$State_char,
                           type = "CR2")

modelsummary(
  list("Basic"          = stigma_c_pass,
       "OLS"            = stigma_cc_pass,
       "MLM"            = mlm_stigma_pass,
       "MLM (Robust SE)" = mlm_stigma_pass,
       "State OLS"      = lm_state_pass),
  vcov = list("classical",
              "classical",
              "classical",
              robust_vcov_pass,
              "classical"),
  stars = c("*" = .05, "**" = .01, "***" = .001),
  gof_map = c("nobs", "icc", "r.squared", "adj.r.squared"),
  coef_rename = c(
    "latino_conc_24" = "Structural Exposure",
    "TreatmentAnti"  = "Anti Treatment",
    "TreatmentPro"   = "Pro Treatment",
    "PartyID_5pt"    = "Party ID",
    "Acculturation"  = "Acculturation",
    "(Intercept)"    = "Intercept"
  ),
  notes = list("Robust SEs clustered at state level (CR2).",
               "State OLS uses state-level means as unit of analysis.",
               "Sample restricted to respondents who identified immigration as primary policy topic."),
  output = "mlm_stigma_pass.tex"
)

# --- 2. EMOTION MODELS ---

mediation_function_survey(
  dvs             = belonging_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls, # no linked fate
  dat             = df_pass,
  out             = "med_ch2_pass_nolf"
)

imm_med_mods_pass <- list(
  stigma_c_pass,
  med_ch2_pass_nolf$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Anger,
  med_ch2_pass_nolf$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Fear,
  med_ch2_pass_nolf$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Shame,
  med_ch2_pass_nolf$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Relief,
  med_ch2_pass_nolf$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Pride,
  med_ch2_pass_nolf$emotion_models$IV_Treatment_Stigma_StigmaImm_mean_Em_Emotions_Joy
)

emotions_cc_pass <- lapply(survey_emotions, function(em) {
  form <- as.formula(paste(em, "~ StigmaImm_mean + Treatment + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "PartyID_5pt + Acculturation"))
  lm(form, data = df_pass)
}) %>% setNames(survey_emotions)

imm_med_mods_cc_pass <- c(
  list(stigma_cc_pass),
  list(emotions_cc_pass$Emotions_Anger,
       emotions_cc_pass$Emotions_Fear,
       emotions_cc_pass$Emotions_Shame,
       emotions_cc_pass$Emotions_Relief,
       emotions_cc_pass$Emotions_Pride,
       emotions_cc_pass$Emotions_Joy)
)

imm_med_mods_interleaved_pass <- c(rbind(imm_med_mods_pass, 
                                         imm_med_mods_cc_pass))

# Negative emotions
stargazer(imm_med_mods_interleaved_pass[3:8],
          type = "latex",
          dep.var.labels = c("Anger", "Fear", "Shame"),
          covariate.labels = c("Imm. Stigma Perception", "Treatment - Anti",
                               "Treatment - Pro", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Constant"),
          longtable = TRUE, float = FALSE,
          out = "main_med_neg_cc_pass.tex")

# Positive emotions
stargazer(imm_med_mods_interleaved_pass[9:14],
          type = "latex",
          dep.var.labels = c("Relief", "Relief", "Pride", "Pride", "Joy", "Joy"),
          covariate.labels = c("Imm. Stigma Perception", "Treatment - Anti",
                               "Treatment - Pro", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Constant"),
          longtable = TRUE, float = FALSE,
          out = "main_med_pos_cc_pass.tex")

# --- 3. BELONGING MODELS ---

belong_em_pass <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment + StigmaImm_mean +",
                           "Age + Sex + Education + Income +",
                           "Emotions_Anger + Emotions_Fear + Emotions_Shame +",
                           "Emotions_Relief + Emotions_Pride + Emotions_Joy +",
                           "PartyID_5pt + Acculturation"))
  lm(form, data = df_pass)
}) %>% setNames(belonging_dvs)

belong_em_cc_pass <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment + StigmaImm_mean + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "Emotions_Anger + Emotions_Fear + Emotions_Shame +",
                           "Emotions_Relief + Emotions_Pride + Emotions_Joy +",
                           "PartyID_5pt + Acculturation"))
  lm(form, data = df_pass)
}) %>% setNames(belonging_dvs)

re_belong_em_interleaved_pass <- list(
  belong_em_pass$BelongingPost_state,
  belong_em_cc_pass$BelongingPost_state,
  belong_em_pass$BelongExternal_state,
  belong_em_cc_pass$BelongExternal_state,
  belong_em_pass$BelongingPost_US,
  belong_em_cc_pass$BelongingPost_US,
  belong_em_pass$BelongExternal_US,
  belong_em_cc_pass$BelongExternal_US
)

stargazer(re_belong_em_interleaved_pass,
          type = "latex",
          dep.var.caption = "Belonging",
          dep.var.labels = c("Internal", "External", "Internal", "External"),
          column.labels = c("State", "US"),
          column.separate = c(4, 4),
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation", "Constant"),
          out = "main_em_cc_pass.tex")

# --- 4. INTERACTION MODELS ---

belong_int_imm_pass <- lapply(belonging_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ Treatment * StigmaImm_mean + latino_conc_24 +",
                           "Age + Sex + Education + Income +",
                           "Emotions_Anger + Emotions_Fear + Emotions_Shame +",
                           "Emotions_Relief + Emotions_Pride + Emotions_Joy +",
                           "PartyID_5pt + Acculturation"))
  lm(form, data = df_pass)
}) %>% setNames(belonging_dvs)

belong_int_imm_interleaved_pass <- list(
  belong_int_imm_pass$BelongingPost_state,
  belong_int_imm_pass$BelongingPost_US,
  belong_int_imm_pass$BelongExternal_state,
  belong_int_imm_pass$BelongExternal_US
)

stargazer(belong_int_imm_interleaved_pass,
          type = "latex",
          dep.var.caption = "Belonging",
          dep.var.labels = c("Internal - State", "Internal - US",
                             "External - State", "External - US"),
          covariate.labels = c("Anti", "Pro",
                               "Imm. Stigma", "Concrete Imm. Index",
                               "Age", "Sex", "Education", "Income",
                               "Anger", "Fear", "Shame",
                               "Relief", "Pride", "Joy",
                               "Party (D $\\longrightarrow$ R)",
                               "Acculturation",
                               "Anti x Stigma", "Pro x Stigma",
                               "Constant"),
          label = "belong.int.imm.pass",
          out = "belong_inter_imm_pass.tex")

