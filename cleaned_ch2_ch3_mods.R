library(survey)
library(mediation)
library(dplyr)

#--------------------------------------------------
# Survey design (state clustered SEs)
#--------------------------------------------------
state_ses <- svydesign(
  ids = ~State,
  weights = ~Weight,
  data = latinos_data
)

#--------------------------------------------------
# Core mediation function
#--------------------------------------------------
run_mediation <- function(design, data,
                          outcome, mediator, treatment,
                          controls) {
  
  f_m <- as.formula(
    paste(mediator, "~", treatment, "+", paste(controls, collapse = " + "))
  )
  
  f_y <- as.formula(
    paste(outcome, "~", treatment, "+", mediator, "+",
          paste(controls, collapse = " + "))
  )
  
  model_m <- svyglm(f_m, design = design, family = gaussian())
  model_y <- svyglm(f_y, design = design, family = gaussian())
  
  mediate(
    model.m = model_m,
    model.y = model_y,
    treat = treatment,
    mediator = mediator,
    sims = 5000
  )
}


controls <- c(
  "Age", "Gender", "Education", "Party",
  "Income", "Parents"
)

dvs <- c(
  "Internal_Belonging",
  "External_Belonging"
)

ivs <- c(
  "class.conc_lat_14_16"
)

mediators <- c(
  "Discrimination_Scale",
  "Imm_Disc",
  "Fear_Election",
  "Angry_Election",
  "Sad_Election",
  "Hope_Election",
  "Pride_Election"
)
control_alt <- c("Age", "Gender", "Education", "Party",
                   "Income", "Parents", "Imm_Disc", "BorderState")

all_em <- "Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election"
med_results <- list()

for (y in dvs) {
  for (x in ivs) {
    for (m in mediators) {
      
      key <- paste(y, x, m, sep = "__")
      
      med_results[[key]] <- run_mediation(
        design = state_ses,
        data = latinos_data,
        outcome = y,
        mediator = m,
        treatment = x,
        controls = controls
      )
    }
  }
}

# Example:
summary(med_results[["Internal_Belonging__class.conc_lat_14_16__Fear_Election"]])

############################################################
# 6. SEM PIPELINE (SURVEY-ADJUSTED LAVAAN)
############################################################

run_sem <- function(model_syntax, design) {
  
  dat <- design$variables
  
  # Use do.call to ensure the syntax is evaluated before being passed to sem
  fit <- do.call(lavaan::sem, list(
    model = model_syntax,
    data = dat,
    estimator = "MLR"
  ))
  
  # Now pass the fit object to lavaan.survey
  res <- lavaan.survey::lavaan.survey(
    lavaan.fit = fit,
    survey.design = design
  )
  
  return(res)
}

############################################################
# 7. EXAMPLE SEM MODELS
############################################################

sem_fear_internal <- '
  Fear_Election ~ a*class.conc_lat_14_16 + Age + Gender + Education + Income + Parents
  Internal_Belonging ~ b*Fear_Election + cprime*class.conc_lat_14_16 + Age + Gender + Education + Party + Income + Parents

  indirect := a*b
  total := cprime + (a*b)
'

fit_fear_internal <- run_sem(
  sem_fear_internal,
  state_ses
)

summary(fit_fear_internal,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)
############################################################
# 7. SVYGLM with CLUSTERED SEs & REGULAR
############################################################

mediation_function_standard(dvs, ivs, mediators, simp_controls,  state_ses, 
                            state_ses, out ="med_results_cl")

mediation_function_standard(dvs, ivs, mediators, control_alt,  state_ses, 
                            state_ses, out ="med_results_cl.full")

mediation_function_standard(dvs, ivs, mediators, simp_controls,  cmps_lat_16, 
                            cmps_lat_16, out ="med_ols_ch2")

mediation_function_standard(dvs, ivs, mediators, control_alt,  cmps_lat_16, 
                            cmps_lat_16, out ="med_ols_ch2.full")

mediation_function_standard(dvs, ivs, all_em, control_alt,  cmps_lat_16, 
                            cmps_lat_16, out ="med_all_ch2")

mediation_function_standard(dvs, ivs, all_em, control_alt,  state_ses, 
                            state_ses, out ="med_all.cl_ch2")

cmps_int <- lapply(dvs, function(dv) {
  form <- as.formula(paste(dv, "~ class.conc_lat_14_16 * Imm_Disc +",
                           "Age + Gender + Education + Income + Parents +",
                           "Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election +",
                           "Party + BorderState"))
  svyglm(form, design = cmps_lat_16)
})


cmps_int <- lapply(dvs, function(dv) {
  form <- as.formula(paste(dv, "~ class.conc_lat_14_16 * Imm_Disc +",
                           "Age + Gender + Education + Income + Parents +",
                           "Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election +",
                           "Party + BorderState"))
  svyglm(form, design = cmps_lat_16)
})
pol_dvs <- c("BorderSecurity", "Pathway_Citizenship")

ch3_cmps_int <- lapply(pol_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ class.conc_lat_14_16 * Imm_Disc +",
                           "Age + Gender + Education + Income + Parents +",
                           "Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election +",
                           "Party + BorderState"))
  svyglm(form, design = cmps_lat_16)
})

ch3_cmps_int_cl <- lapply(pol_dvs, function(dv) {
  form <- as.formula(paste(dv, "~ class.conc_lat_14_16 * Imm_Disc +",
                           "Age + Gender + Education + Income + Parents +",
                           "Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election +",
                           "Party + BorderState"))
  svyglm(form, design = state_ses)
})

path_plot <- plot_model(ols_all_dvs[[2]], type = "pred", terms = "class.conc_lat_14_16") +
  labs(x     = "Str. Imm. Index",
       title = "Support for Pathway for Citizenship by Str. Stigma Context")


int_plot <- plot_model(ch3_cmps_int_cl[[2]], type = "int") +
  labs(x     = "Str. Imm. Index",
       title = "Support for Pathway for Citizenship by Str. Stigma Context")

ggsave(filename = "path_plot.png",
       plot     = path_plot,
       width    = 10,
       height   = 8,
       dpi      = 300)
# ── Chapter 2 DV lists ───────────────────────────────────────────────────────
# DVs for ch2 are belonging measures: 
# External_Belonging, Internal_Belonging

# OLS: Ch2 DVs - all emotions
ols_dvs_ch2 <- list(
  med_ols_ch2.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Fear_Election,
  med_ols_ch2.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Angry_Election,
  med_ols_ch2.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Sad_Election,
  med_ols_ch2.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Hope_Election,
  med_ols_ch2.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Pride_Election,
  med_ols_ch2.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Fear_Election,
  med_ols_ch2.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Angry_Election,
  med_ols_ch2.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Sad_Election,
  med_ols_ch2.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Hope_Election,
  med_ols_ch2.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Pride_Election
)

# CL: Ch2 DVs - all emotions
cl_dvs_ch2 <- list(
  med_results_cl.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Fear_Election,
  med_results_cl.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Angry_Election,
  med_results_cl.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Sad_Election,
  med_results_cl.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Hope_Election,
  med_results_cl.full$outcome_models$DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Pride_Election,
  med_results_cl.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Fear_Election,
  med_results_cl.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Angry_Election,
  med_results_cl.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Sad_Election,
  med_results_cl.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Hope_Election,
  med_results_cl.full$outcome_models$DV_External_Belonging_IV_class.conc_lat_14_16_Med_Pride_Election
)

# all emotions --- 
allem_ch2 <- list(
  med_all_ch2$outcome_models$`DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election`,
  med_all_ch2$outcome_models$`DV_External_Belonging_IV_class.conc_lat_14_16_Med_Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election`
)

allem_cl_ch2 <- list(
  med_all.cl_ch2$outcome_models$`DV_Internal_Belonging_IV_class.conc_lat_14_16_Med_Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election`,
  med_all.cl_ch2$outcome_models$`DV_External_Belonging_IV_class.conc_lat_14_16_Med_Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election`
)


################
# CHAPTER 3 # 
#################
ch3_med <- c(
  "Fear_Election",
  "Angry_Election",
  "Sad_Election",
  "Hope_Election",
  "Pride_Election"
)
ch3_dvs <- c("BorderSecurity", "Pathway_Citizenship")

# Chapter 3 controls (adds BorderState to basic controls)
ch3_controls <- c(controls, "BorderState")
ch3_controls.alt <- c(controls,"Imm_Disc", "BorderState")

med_results_ch3 <- list()

for (y in ch3_dvs) {
  for (x in ivs) {
    for (m in mediators) {
      
      key <- paste(y, x, m, sep = "__")
      
      med_results_ch3[[key]] <- run_mediation(
        design = state_ses,
        data = latinos_data,
        outcome = y,
        mediator = m,
        treatment = x,
        controls = ch3_controls
      )
    }
  }
}

mediation_function_standard(ch3_dvs, ivs, ch3_med, ch3_controls.alt,  state_ses, 
                            state_ses, out ="med_results_cl_ch3")

mediation_function_standard(ch3_dvs, ivs, ch3_med, ch3_controls.alt,  cmps_lat_16, 
                            cmps_lat_16, out ="med_ols_ch3")


mediation_function_standard(ch3_dvs, ivs, all_em, ch3_controls.alt,  state_ses, 
                            state_ses, out ="all_results_cl_ch3")

mediation_function_standard(ch3_dvs, ivs, all_em, ch3_controls.alt,  cmps_lat_16, 
                            cmps_lat_16, out ="all_ols_ch3")

# ── OLS lists ────────────────────────────────────────────────────────────────

# OLS: Positive emotions
ols_pos <- list(
  med_ols_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Hope_Election,
  med_ols_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Pride_Election
)

# OLS: Negative emotions
ols_neg <- list(
  med_ols_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Fear_Election,
  med_ols_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Angry_Election,
  med_ols_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Sad_Election
)

# OLS: DVs (BorderSecurity and Pathway, all emotions)
ols_dvs <- list(
  med_ols_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Fear_Election,
  med_ols_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Angry_Election,
  med_ols_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Sad_Election,
  med_ols_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Hope_Election,
  med_ols_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Pride_Election,
  med_ols_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Fear_Election,
  med_ols_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Angry_Election,
  med_ols_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Sad_Election,
  med_ols_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Hope_Election,
  med_ols_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Pride_Election
)

# ── Clustered SE lists ────────────────────────────────────────────────────────

# CL: Positive emotions
cl_pos <- list(
  med_results_cl_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Hope_Election,
  med_results_cl_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Pride_Election
)

# CL: Negative emotions
cl_neg <- list(
  med_results_cl_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Fear_Election,
  med_results_cl_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Angry_Election,
  med_results_cl_ch3$mediator_models$IV_class.conc_lat_14_16_Med_Sad_Election
)

# CL: DVs (BorderSecurity and Pathway, all emotions)
cl_dvs <- list(
  med_results_cl_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Fear_Election,
  med_results_cl_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Angry_Election,
  med_results_cl_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Sad_Election,
  med_results_cl_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Hope_Election,
  med_results_cl_ch3$outcome_models$DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Pride_Election,
  med_results_cl_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Fear_Election,
  med_results_cl_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Angry_Election,
  med_results_cl_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Sad_Election,
  med_results_cl_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Hope_Election,
  med_results_cl_ch3$outcome_models$DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Pride_Election
)

# ── All emotions combined (from all_ols and all_results_cl) ──────────────────

ols_all_dvs <- list(
  all_ols_ch3$outcome_models$`DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election`,
  all_ols_ch3$outcome_models$`DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election`
)

cl_all_dvs <- list(
  all_results_cl_ch3$outcome_models$`DV_BorderSecurity_IV_class.conc_lat_14_16_Med_Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election`,
  all_results_cl_ch3$outcome_models$`DV_Pathway_Citizenship_IV_class.conc_lat_14_16_Med_Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election`
)

## stigma --- 
stigma.ch2 <- list(
  med_ols_ch2$mediator_models$IV_class.conc_lat_14_16_Med_Imm_Disc,
  med_results_cl$mediator_models$IV_class.conc_lat_14_16_Med_Imm_Disc
  
) 

# ── Quick checks ─────────────────────────────────────────────────────────────
stargazer(ols_neg, ols_pos, type = "text")
stargazer(ols_dvs, type = "text")
stargazer(ols_all_dvs, type = "text")
stargazer(cl_neg, cl_pos, type = "text")
stargazer(cl_dvs, type = "text")
stargazer(cl_all_dvs, type = "text")
stargazer(stigma.ch2, type = "text")

#### Tables -----------------------------------
stargazer(ols_neg, ols_pos, type = "latex",
          dep.var.labels = c("Fear", "Anger", "Sad", "Hope", "Pride"),
          covariate.labels = c("Str. Stigma Index", "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", "Imm. Disc.",
                               "Border State", "Constant"), out = "emotions_ols_5.28.tex")

stargazer(ols_dvs, type = "latex",
          dep.var.labels = c("Border Security", "Pathway for Citizenship"),
          covariate.labels = c("Str. Stigma Index", "Fear", "Anger", "Sad","Hope", "Pride",
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", "Imm. Disc.",
                               "Border State", "Constant"), out = "dvs_ols_5.28.tex")


stargazer(ols_all_dvs, type = "latex",
          dep.var.labels = c("Border Security", "Pathway for Citizenship"),
          covariate.labels = c("Str. Stigma Index", "Fear", "Anger", "Pride", "Hope", "Sad",
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", "Imm. Disc.",
                               "Border State", "Constant"), out = "dvs_ols.all5.28.tex")


stargazer(cl_neg, cl_pos, type = "latex",
          dep.var.labels = c("Fear", "Anger", "Sad", "Hope", "Pride"),
          covariate.labels = c("Str. Stigma Index", "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace","Imm. Disc.",
                               "Border State", "Constant"), out = "emotions_cl_5.28.tex")

stargazer(cl_dvs, type = "latex",
          dep.var.labels = c("Border Security", "Pathway for Citizenship"),
          covariate.labels = c("Str. Stigma Index", "Fear", "Anger", "Sad","Hope", "Pride",
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", 
                               "Border State", "Constant"), out = "dvs_cl_5.28.tex")

stargazer(cl_all_dvs, type = "latex",
          dep.var.labels = c("Border Security", "Pathway for Citizenship"),
          covariate.labels = c("Str. Stigma Index", "Fear", "Anger", "Pride", "Hope", "Sad",
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", "Imm. Disc.",
                               "Border State", "Constant"), out = "dvs_cl.all5.28.tex")


### chapter 2 tabs --- 
# Quick check
stargazer(ols_dvs_ch2, type = "text")
stargazer(cl_dvs_ch2, type = "text")
stargazer(allem_ch2, type = "text")
stargazer(allem_cl_ch2, type = "text")

stargazer(stigma.ch2, type = "latex",
          dep.var.labels = "Imm. Disc.",
          covariate.labels = c("Str. Stigma Index", 
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace",
                               "Border State", "Constant"), out = "stigma.5.28.tex")
stargazer(ols_dvs_ch2, type = "latex",
          dep.var.labels = c("Internal Belonging", "External Belonging"),
          covariate.labels = c("Str. Stigma Index", "Fear", "Anger", "Sad","Hope", "Pride",
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", "Imm. Disc.",
                               "Border State", "Constant"), out = "ch2_ols_5.28.tex")

stargazer(cl_dvs_ch2, type = "latex",
          dep.var.labels = c("Internal Belonging", "External Belonging"),
          covariate.labels = c("Str. Stigma Index", "Fear", "Anger", "Sad","Hope", "Pride",
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", "Imm. Disc.",
                               "Border State", "Constant"), out = "ch2_cl_5.28.tex")

stargazer(allem_ch2, type = "latex",
          dep.var.labels = c("Internal Belonging", "External Belonging"),
          covariate.labels = c("Str. Stigma Index", "Fear", "Anger", "Pride","Hope","Sad",
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", "Imm. Disc.",
                               "Border State", "Constant"), out = "ch2_all_5.28.tex")

stargazer(allem_cl_ch2, type = "latex",
          dep.var.labels = c("Internal Belonging", "External Belonging"),
          covariate.labels = c("Str. Stigma Index", "Fear", "Anger", "Pride","Hope","Sad",
                               "Age", "Gender", "Education", 
                               "Party", "Income", "Parent's Birthplace", "Imm. Disc.",
                               "Border State", "Constant"), out = "ch2_all.cl.5.28.tex")

stargazer(cmps_int,  type = "latex",
          dep.var.labels = c("Internal Belonging", "External Belonging"),
          covariate.labels = c("Str. Stigma Index",  "Imm. Disc.",
                               "Age", "Gender", "Education", 
                               "Income", "Parent's Birthplace",
                               "Fear", "Anger", "Pride","Hope","Sad", "Party", 
                               "Border State", "Str. Stigma Index x Imm. Disc.","Constant"), out = "cmps_int.tex")

stargazer(ch3_cmps_int,  type = "latex",
          dep.var.labels = c("Border Security", "Pathway for Citizenship"),
          covariate.labels = c("Str. Stigma Index",  "Imm. Disc.",
                               "Age", "Gender", "Education", 
                               "Income", "Parent's Birthplace",
                               "Fear", "Anger", "Pride","Hope","Sad", "Party", 
                               "Border State", "Str. Stigma Index x Imm. Disc.","Constant"), out = "ch3_cmps_int.tex")
