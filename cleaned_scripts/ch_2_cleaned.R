# =============================================================================
# Primary analysis: Stigma context --> Emotions --> Sense of Belonging
# Primary IV: class.conc_lat_14_16 (2014-2016 concrete immigration index change)
# =============================================================================

source("00_utilities.R")
source("01_import_2016_cleaned.R")

# =============================================================================
# SECTION 1: STIGMA CONTEXT --> EMOTIONS (Mediator models)
# Tests whether state-level immigration policy climate predicts emotional response
# Three specifications: basic controls, full controls, clustered SEs
# =============================================================================

# --- 1a. Basic controls -------------------------------------------------------
mediation_function_standard(
  dvs       = belonging.dvs,
  iv        = "class.conc_lat_14_16",
  mediators = emotions,
  controls  = basic.controls,
  des       = cmps_lat_16,
  dat       = latinos_data,
  out       = "med_basic"
)

# --- 1b. Full controls --------------------------------------------------------
mediation_function_standard(
  dvs       = belonging.dvs,
  iv        = "class.conc_lat_14_16",
  mediators = emotions,
  controls  = full.controls,
  des       = cmps_lat_16,
  dat       = latinos_data,
  out       = "med_full"
)

# --- 1c. Clustered SEs at state level (robustness) ----------------------------
res_emotions_clustered <- cluster_svyglm(
  dvs         = emotions,
  iv          = "class.conc_lat_14_16",
  controls    = basic.controls,
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

# --- 1d. Tables: Stigma --> Emotions ------------------------------------------
# Negative emotions
neg_basic    <- med_basic$mediator_models[c("IV_class.conc_lat_14_16_Med_Fear_Election",
                                            "IV_class.conc_lat_14_16_Med_Angry_Election",
                                            "IV_class.conc_lat_14_16_Med_Sad_Election")]
neg_full     <- med_full$mediator_models[c("IV_class.conc_lat_14_16_Med_Fear_Election",
                                           "IV_class.conc_lat_14_16_Med_Angry_Election",
                                           "IV_class.conc_lat_14_16_Med_Sad_Election")]
neg_clustered <- res_emotions_clustered[c("DV_Fear_Election_IV_class.conc_lat_14_16",
                                          "DV_Angry_Election_IV_class.conc_lat_14_16",
                                          "DV_Sad_Election_IV_class.conc_lat_14_16")]

stargazer(neg_basic, neg_full, neg_clustered,
          type   = "latex",
          dep.var.labels   = rep(c("Fear", "Anger", "Sadness"), 3),
          dep.var.caption  = "Dependent variable: Negative Emotions",
          covariate.labels = c("Concrete Imm. Index (2014-2016)",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Linked Fate",
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Constant"),
          add.lines = list(
            c("Clustered SEs", rep("No", 6), rep("Yes", 3))
          ),
          label = "neg.med",
          out   = "tables/neg_emotions.tex")

# Positive emotions
pos_basic    <- med_basic$mediator_models[c("IV_class.conc_lat_14_16_Med_Pride_Election",
                                            "IV_class.conc_lat_14_16_Med_Hope_Election")]
pos_full     <- med_full$mediator_models[c("IV_class.conc_lat_14_16_Med_Pride_Election",
                                           "IV_class.conc_lat_14_16_Med_Hope_Election")]
pos_clustered <- res_emotions_clustered[c("DV_Pride_Election_IV_class.conc_lat_14_16",
                                          "DV_Hope_Election_IV_class.conc_lat_14_16")]

stargazer(pos_basic, pos_full, pos_clustered,
          type   = "latex",
          dep.var.labels   = rep(c("Pride", "Hope"), 3),
          dep.var.caption  = "Dependent variable: Positive Emotions",
          covariate.labels = c("Concrete Imm. Index (2014-2016)",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Linked Fate",
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Constant"),
          add.lines = list(
            c("Clustered SEs", rep("No", 4), rep("Yes", 2))
          ),
          label = "pos.med",
          out   = "tables/pos_emotions.tex")

# =============================================================================
# SECTION 2: PRIMARY SEM MODELS (lavaan + lavaan.survey)
# Full mediation chain: Stigma --> Emotion --> Outcome
# Each emotion is modeled separately, then all emotions together
# Survey design uses state-level clustering
# =============================================================================

# State-clustered design for lavaan.survey
state_ses <- svydesign(
  ids     = ~State,
  data    = latinos_data,
  weights = ~Weight
)

# --- Helper: build and fit one SEM model -------------------------------------
# Args:
#   emotion  - character string, emotion variable name
#   outcome  - character string, outcome variable name
#   controls - character vector of control variable names
fit_sem <- function(emotion, outcome, controls) {
  ctrl <- paste(controls, collapse = " + ")
  model_str <- paste0('
    # Step 1: Stigma context --> Emotion
    ', emotion, ' ~ a*class.conc_lat_14_16 + ', ctrl, '

    # Step 2: Emotion --> Outcome (with stigma context direct effect)
    ', outcome, ' ~ b*', emotion, ' + cprime*class.conc_lat_14_16 + ', ctrl, '

    # Indirect and total effects
    indirect := a * b
    total    := cprime + (a * b)
  ')
  
  fit_unweighted <- sem(model_str, data = latinos_data, estimator = "MLR")
  fit_weighted   <- lavaan.survey(fit_unweighted, survey.design = state_ses)
  return(fit_weighted)
}

# --- 2a. Individual emotion models: Belonging outcomes -----------------------
# Each emotion modeled separately to preserve interpretability
# Allows comparison of emotion-specific indirect effects

sem_belonging <- list()
for (em in emotions) {
  for (dv in belonging.dvs) {
    key <- paste0(em, "_", dv)
    sem_belonging[[key]] <- fit_sem(em, dv, basic.controls)
  }
}

# --- 2b. Individual emotion models: Policy outcomes --------------------------
sem_policy <- list()
for (em in emotions) {
  for (dv in policy.dvs) {
    key <- paste0(em, "_", dv)
    sem_policy[[key]] <- fit_sem(em, dv, basic.controls)
  }
}

# --- 2c. All emotions simultaneously (kitchen sink model) --------------------
# Tests which emotions dominate when all are included
# NOTE: interpret with caution -- multicollinearity among emotions is possible

fit_all_emotions_sem <- function(outcome, controls) {
  ctrl <- paste(controls, collapse = " + ")
  em_str <- paste(emotions, collapse = " + ")
  model_str <- paste0('
    # All emotions predicted by stigma context
    ', paste(sapply(emotions, function(e)
      paste0(e, ' ~ class.conc_lat_14_16 + ', ctrl)), collapse = '\n    '), '

    # Outcome predicted by all emotions + stigma context
    ', outcome, ' ~ ', em_str, ' + class.conc_lat_14_16 + ', ctrl, '
  ')
  fit_unweighted <- sem(model_str, data = latinos_data, estimator = "MLR")
  lavaan.survey(fit_unweighted, survey.design = state_ses)
}

sem_all_belonging <- lapply(setNames(belonging.dvs, belonging.dvs),
                            fit_all_emotions_sem, controls = basic.controls)
sem_all_policy    <- lapply(setNames(policy.dvs, policy.dvs),
                            fit_all_emotions_sem, controls = basic.controls)

# --- 2d. Print SEM summaries --------------------------------------------------
for (key in names(sem_belonging)) {
  cat("\n====", key, "====\n")
  summary(sem_belonging[[key]], standardized = TRUE, fit.measures = TRUE)
}
for (key in names(sem_policy)) {
  cat("\n====", key, "====\n")
  summary(sem_policy[[key]], standardized = TRUE, fit.measures = TRUE)
}

# =============================================================================
# SECTION 3: EMOTIONS --> OUTCOMES (Outcome models with individual emotions)
# OLS specifications mirroring SEM Step 2 for table presentation
# Three specifications: basic, clustered SEs, full controls
# =============================================================================

# Build clustered outcome models for each emotion x DV combination
res_belonging_clustered <- list()
for (em in emotions) {
  res <- cluster_svyglm(
    dvs         = belonging.dvs,
    iv          = "class.conc_lat_14_16",
    controls    = c(basic.controls, em),
    dat         = latinos_data,
    cluster_var = "State",
    weight_var  = "Weight"
  )
  res_belonging_clustered[[em]] <- res
}

res_policy_clustered <- list()
for (em in emotions) {
  res <- cluster_svyglm(
    dvs         = policy.dvs,
    iv          = "class.conc_lat_14_16",
    controls    = c(basic.controls, em),
    dat         = latinos_data,
    cluster_var = "State",
    weight_var  = "Weight"
  )
  res_policy_clustered[[em]] <- res
}

# --- Tables: Emotions --> Internal Belonging ----------------------------------
int_basic_list <- med_basic$outcome_models[
  paste0("DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_", emotions)]
int_full_list  <- med_full$outcome_models[
  paste0("DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_", emotions)]
int_clust_list <- lapply(res_belonging_clustered,
                         function(x) x$DV_Internal_Belonging_IV_class.conc_lat_14_16)

stargazer(int_basic_list, int_clust_list, int_full_list,
          type   = "latex",
          dep.var.labels   = rep(c("Fear", "Anger", "Sadness", "Pride", "Hope"), 3),
          dep.var.caption  = "Dependent Variable: Internal Sense of Belonging",
          covariate.labels = c("Concrete Imm. Index (2014-2016)",
                               "Fear", "Anger", "Sadness", "Pride", "Hope",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Linked Fate",
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Constant"),
          add.lines = list(
            c("Clustered SEs", rep("No", 5), rep("Yes", 5), rep("No", 5))
          ),
          label = "int.belong",
          out   = "tables/internal_belonging.tex")

# --- Tables: Emotions --> External Belonging ----------------------------------
ext_basic_list <- med_basic$outcome_models[
  paste0("DV_External_Belonging_IV_class.conc_lat_14_16_Med_", emotions)]
ext_full_list  <- med_full$outcome_models[
  paste0("DV_External_Belonging_IV_class.conc_lat_14_16_Med_", emotions)]
ext_clust_list <- lapply(res_belonging_clustered,
                         function(x) x$DV_External_Belonging_IV_class.conc_lat_14_16)

stargazer(ext_basic_list, ext_clust_list, ext_full_list,
          type   = "latex",
          dep.var.labels   = rep(c("Fear", "Anger", "Sadness", "Pride", "Hope"), 3),
          dep.var.caption  = "Dependent Variable: External Sense of Belonging",
          covariate.labels = c("Concrete Imm. Index (2014-2016)",
                               "Fear", "Anger", "Sadness", "Pride", "Hope",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Linked Fate",
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Constant"),
          add.lines = list(
            c("Clustered SEs", rep("No", 5), rep("Yes", 5), rep("No", 5))
          ),
          label = "ext.belong",
          out   = "tables/external_belonging.tex")

# =============================================================================
# SECTION 4: INTERACTION MODELS
# Tests whether discrimination experience moderates stigma --> outcome path
# =============================================================================

# Remove discrimination vars from controls to avoid collinearity
controls_no_disc <- setdiff(full.controls, c("Discrimination_Scale", "Latino_Disc"))

# Personal discrimination as moderator
res_int_pers <- interaction_function(
  dvs        = all.dvs,
  iv         = "class.conc_lat_14_16",
  moderators = c("Discrimination_Scale"),
  controls   = c(controls_no_disc, emotions),
  des        = cmps_lat_16,
  dat        = latinos_data
)

# Group discrimination as moderator
res_int_group <- interaction_function(
  dvs        = all.dvs,
  iv         = "class.conc_lat_14_16",
  moderators = c("Latino_Disc"),
  controls   = c(controls_no_disc, emotions),
  des        = cmps_lat_16,
  dat        = latinos_data
)

# Clustered SE versions for robustness
res_int_pers_cl <- cluster_svyglm(
  dvs         = all.dvs,
  iv          = "class.conc_lat_14_16*Discrimination_Scale",
  controls    = c(controls_no_disc, emotions),
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

# --- Marginal effects plot for significant interaction -----------------------
# NOTE: Update model name below to whichever interaction is significant
int_model <- res_int_pers[[paste0("DV_BorderSecurity_IV_class.conc_lat_14_16*Discrimination_Scale")]]

int_plot <- plot_model(int_model, type = "int") +
  labs(x     = "Concrete Imm. Index (2014-2016)",
       color = "Personal Discrimination",
       title = "Interaction: Stigma Context x Personal Discrimination")

ggsave(filename = "figures/interaction_plot.png",
       plot     = int_plot,
       width    = 10, height = 8, dpi = 300)

# =============================================================================
# SECTION 5: ROBUSTNESS - BAYESIAN HIERARCHICAL MODELS
# Runs cs_hier for all primary DVs to properly account for state nesting
# Recommended: run for both belonging and policy DVs
# Note: computationally intensive -- run overnight if needed
# =============================================================================

res_bayes <- cs_hier(
  dvs         = all.dvs,
  iv          = "class.conc_lat_14_16",
  controls    = basic.controls,
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight",
  random_slopes = FALSE,
  family      = gaussian(),
  chains      = 4,
  iter        = 2000,
  cores       = 4,
  out         = "res_bayes"
)

# =============================================================================
# SECTION 6: NATIVITY SUBGROUP ANALYSES
# Compares native-born vs. foreign-born Latinos
# Tests whether stigma context effects differ by immigration experience
# =============================================================================

med_native <- mediation_function_standard(
  dvs       = belonging.dvs,
  iv        = "class.conc_lat_14_16",
  mediators = emotions,
  controls  = basic.controls,
  des       = native_cmps,
  dat       = native_born
)

med_foreign <- mediation_function_standard(
  dvs       = belonging.dvs,
  iv        = "class.conc_lat_14_16",
  mediators = emotions,
  controls  = basic.controls,
  des       = foreign_cmps,
  dat       = foreign_born
)