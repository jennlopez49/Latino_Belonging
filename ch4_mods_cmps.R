###### Ch. 4 -------------------- CMPS -----------------------------------------

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
  
  # Identify complete cases across ALL variables used in either model
  all_vars <- c(outcome, mediator, treatment, controls)
  complete_rows <- complete.cases(data[, all_vars])
  data_clean <- data[complete_rows, ]
  data_clean <- droplevels(data_clean)
  
  # Rebuild the survey design on the cleaned data
  design_clean <- svydesign(
    ids = ~State,
    weights = ~Weight,
    data = data_clean
  )
  
  f_m <- as.formula(
    paste(mediator, "~", treatment, "+", paste(controls, collapse = " + "))
  )
  
  f_y <- as.formula(
    paste(outcome, "~", treatment, "+", mediator, "+",
          paste(controls, collapse = " + "))
  )
  
  model_m <- svyglm(f_m, design = design_clean, family = gaussian())
  model_y <- svyglm(f_y, design = design_clean, family = gaussian())
  
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
  "Income", "Parents", "Pol_Interest"
)

dvs <- c(
  "Voted", "DonatedMoney", "ContactedGovOff", "Protested"
)

ivs <- c(
  "class.conc_lat_14_16"
)

mediators <- c(
  "Imm_Disc",
  "ExternalEfficacy",
  "InternalEfficacy",
  "Fear_Election",
  "Angry_Election",
  "Sad_Election",
  "Hope_Election",
  "Pride_Election"
)
control_alt <- c("Age", "Gender", "Education", "Party",
                 "Income", "Parents", "Pol_Interest","Imm_Disc", "ExternalEfficacy")

all_em <- "Fear_Election + Angry_Election + Pride_Election + Hope_Election + Sad_Election"
med_results_ch4 <- list()

for (y in dvs) {
  for (x in ivs) {
    for (m in mediators) {
      
      key <- paste(y, x, m, sep = "__")
      
      med_results_ch4[[key]] <- run_mediation(
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

#### 

# Example:
extract_mediation <- function(results_list) {
  do.call(rbind, lapply(names(results_list), function(key) {
    s <- summary(results_list[[key]])
    parts <- strsplit(key, "__")[[1]]
    data.frame(
      outcome   = parts[1],
      treatment = parts[2],
      mediator  = parts[3],
      ACME      = s$d.avg,
      ACME_lo   = s$d.avg.ci[1],
      ACME_hi   = s$d.avg.ci[2],
      ACME_p    = s$d.avg.p,
      ADE       = s$z.avg,
      ADE_p     = s$z.avg.p,
      Total     = s$tau.coef,
      Total_p   = s$tau.p,
      PropMed   = s$n.avg,
      PropMed_p = s$n.avg.p,
      N         = s$nobs,
      stringsAsFactors = FALSE
    )
  }))
}

results_table <- extract_mediation(med_results_ch4)

# Quick view of significant ACMEs only
results_table[results_table$ACME_p < 0.05, ]

# Or print everything by outcome
print(results_table[results_table$outcome == "Voted", ], digits = 3)

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

mediation_function_standard(dvs, ivs, mediators, controls,  state_ses, 
                            state_ses, out ="med_ols_ch4")
mediation_function_standard(dvs, ivs, all_em, control_alt,  state_ses, 
                            state_ses, out ="med_all_ch4")

############################################################
# 8. Hierarchical 
############################################################

res_cs_ch4 <- cs_hier(
  dvs = dvs,
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Education","Income","Party", "Parents", "Pol_Interest",
               "Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
               "Sad_Election","Imm_Disc", "ExternalEfficacy"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",
  weight_var = "Weight",
  random_slopes = FALSE,
  #family = gaussian(),
  chains = 4,
  iter = 2000,
  cores = 4
)
