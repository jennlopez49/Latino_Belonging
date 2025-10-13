### Necessary Pkgs --- install if needed
#install.packages(c("haven", "survey", "sjPlot", "stargazer", "patchwork",
#                 "tidycensus", "tigris", "sf", "mediation"))
library(tidyverse)
library(haven)
library(survey)
library(sjPlot)
library(stargazer)
library(patchwork)
library(tidycensus)
library(tigris)
library(sf)
library(mediation)
library(dplyr)
library(broom)
library(lavaan)
library(lavaan.survey)
library(lme4)
library(brms)
library(csSampling)
library(coefplot)

###### Custom Function run Multiple Mediation Analyses  -----------------
mediation_function <- function(dvs, ivs, mediators, controls, des, dat, out = NULL) {
  
  mediation_results <- list()
  
  for (Y in dvs) {  # Loop through each dependent variable
    results_Y <- list()  # Store results for this dependent variable
    
    for (X in ivs) {  # Loop through each IV
      for (M in mediators) {  # Loop through each mediator
        
        # Fit Mediator Model (M ~ X + controls)
        form_mediator <- as.formula(paste(M, " ~ ", X, " + ", paste(controls, collapse = " + ")))
        mediator_model <- svyglm(form_mediator, design = des, family = gaussian(), data = dat)
        
        # Fit Outcome Model (Y ~ X + M + controls)
        form_outcome <- as.formula(paste(Y, " ~ ", X, " + ", M, " + ", paste(controls, collapse = " + ")))
        outcome_model <- svyglm(form_outcome, design = des, family = gaussian(), data = dat)
        
        # Run Mediation Analysis
        mediation_result <- mediate(mediator_model, outcome_model, treat = X, mediator = M)
        
        # Store results
        results_Y[[paste0("IV_", X, "_Med_", M)]] <- list(
          mediator_model = mediator_model,
          outcome_model = outcome_model,
          mediation_result = mediation_result
        )
      }
    }
    
    mediation_results[[Y]] <- results_Y  # Store results
  }
  
  # Assign to global environment for out
  if (!is.null(out)) {
    if (!is.character(out)) stop("Argument 'out' must be a character string")
    assign(out, mediation_results, envir = .GlobalEnv)
  } else {
    return(mediation_results)
  }
}

###### Custom Function Interactions  -----------------
interaction_function <- function(dvs, ivs, mediators, controls, des, dat, out = NULL) {
  interaction_results <- list()
  
  for (Y in dvs) {  # Loop through each dependent variable
    results_Y <- list()  # Store results for this dependent variable
    
    for (X in ivs) {  # Loop through each IV
      for (M in mediators) {  # Loop through each mediator
        
        # Fit survey-weighted OLS model with interaction (Y ~ X * M + controls)
        form_interaction <- as.formula(paste(Y, " ~ ", X, " * ", M, " + ", paste(controls, collapse = " + ")))
        interaction_model <- svyglm(form_interaction, design = des, family = gaussian(), data = dat)
        
        # Store results
        results_Y[[paste0("IV_", X, "_Med_", M)]] <- interaction_model
      }
    }
    
    interaction_results[[Y]] <- results_Y  # Store results 
  }
  
  # Assign to global environment 
  if (!is.null(out)) {
    if (!is.character(out)) stop("Argument 'out' must be a character string")
    assign(out, interaction_results, envir = .GlobalEnv)
  } else {
    return(interaction_results)
  }
}
######### Custom Function for Regular OLS ---------------------------------------

ols_function <- function(dvs, vars, des, dat, out){
  lm_mods1 <- list()
  
  for (Y in dvs) {
    mod1 <- list()
    for (i in 1:length(vars)) {
      form <- as.formula(paste(Y, " ~ ", paste(vars[[i]], collapse = " + ")))
      mod1[[i]] <- svyglm(form, design = des, family = gaussian(), data = dat) 
    }
    lm_mods1[[Y]] <- mod1
  }
  assign(out, lm_mods1, envir = .GlobalEnv)
}


######## Alternate Custom Function with Standardization ------------------------
ols_function_standard <- function(dvs, vars_list, des,
                                  weight_var, dat, out = NULL) {
  standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)  # Standardization function
  
  # Standardize all predictors except the DV
  dat_std <- dat  # Copy of dataset
  predictors <- unique(unlist(vars_list))  # Get all predictors
  predictors <- setdiff(predictors, dvs)  # Exclude dependent variables
  
  for (var in predictors) {
    if (var %in% names(dat_std)) {
      dat_std[[var]] <- standardize(dat_std[[var]])
    }
  }
  
  ols_models <- list()
  
  for (Y in dvs) {  # Loop through each dependent variable
    model_list <- list()
    
    for (i in 1:length(vars_list)) {
      formula <- as.formula(paste(Y, " ~ ", paste(vars_list[[i]], collapse = " + ")))
      model_list[[i]] <- svyglm(formula, design = des, family = gaussian(), 
                                weights = dat[[weight_var]], data = dat_std) 
    }
    
    ols_models[[Y]] <- model_list
  }
  
  if (!is.null(out)) {
    assign(out, ols_models, envir = .GlobalEnv)
  } else {
    return(ols_models)
  }
}

####### Interaction with Standardized Data -----------
interaction_function_standard <- function(dvs, ivs, mediators, controls, 
                                          weight_var, des, 
                                          dat, out = NULL) {
  standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)  # Standardization function
  
  # Standardize IVs, mediators, and controls (but not DVs)
  dat_std <- dat  # Copy of dataset
  vars_to_standardize <- unique(c(ivs, mediators, controls))
  
  for (var in vars_to_standardize) {
    if (var %in% names(dat_std)) {
      dat_std[[var]] <- standardize(dat_std[[var]])
    }
  }
  
  interaction_results <- list()
  
  for (Y in dvs) {  
    results_Y <- list() 
    for (X in ivs) {  
      for (M in mediators) {  
        
        # Model with interaction between Stigma and Linked Fate
        form_interaction <- as.formula(paste(Y, " ~ ", X, "* Linked_Fate +", M, "+", paste(controls, collapse = " + ")))
        
        interaction_model <- svyglm(form_interaction, design = des, family = gaussian(), 
                                    weights = dat[[weight_var]], data = dat_std)
        
        # Store results
        interaction_results[[paste0("DV_", Y, "_IV_", X, "_LF_Interaction_Med_", M)]] <- interaction_model
      }
    }
    interaction_results[[Y]] <- results_Y  # Store results 
  }
  
  # Assign to global environment 
  if (!is.null(out)) {
    if (!is.character(out)) stop("Argument 'out' must be a character string")
    assign(out, interaction_results, envir = .GlobalEnv)
  } else {
    return(interaction_results)
  }
}

###### mediation 
mediation_function_standard <- function(dvs, ivs, mediators, controls, des,
                                        dat, out = NULL) {
  standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)  # Standardization function
  
  # Standardize IVs, mediators, and controls (but not DVs)
  dat_std <- dat  # Copy dataset
  vars_to_standardize <- unique(c(ivs, mediators, controls))
  
  for (var in vars_to_standardize) {
    if (var %in% names(dat_std)) {
      dat_std[[var]] <- standardize(dat_std[[var]])
    }
  }
  
  mediation_results <- list()
  
  ## Step 1: Predict mediators (M ~ IV + Linked Fate + Controls)
  mediator_models <- list()
  for (M in mediators) {  
    for (X in ivs) {  
      form_mediator <- as.formula(paste(M, " ~ ", X, "+", paste(controls, collapse = " + ")))
      mediator_model <- svyglm(form_mediator, design = des, family = gaussian(),
                               data = dat_std)
      
      mediator_models[[paste0("IV_", X, "_Med_", M)]] <- mediator_model
    }
  }
  
  ## Step 2: Predict DV (Y ~ IV + M + Linked Fate + Controls)
  outcome_models <- list()
  for (Y in dvs) {  
    for (M in mediators) {  
      for (X in ivs) {  
        form_outcome <- as.formula(paste(Y, " ~ ", X, "+", M, "+", paste(controls, collapse = " + ")))
        outcome_model <- svyglm(form_outcome, design = des, family = gaussian(),
                                data = dat_std)
        
        outcome_models[[paste0("DV_", Y, "_IV_", X, "_Med_", M)]] <- outcome_model
      }
    }
  }
  
  mediation_results$mediator_models <- mediator_models
  mediation_results$outcome_models <- outcome_models
  
  # Assign to global environment
  if (!is.null(out)) {
    if (!is.character(out)) stop("Argument 'out' must be a character string")
    assign(out, mediation_results, envir = .GlobalEnv)
  } else {
    return(mediation_results)
  }
}

########## Function to cluster SEs by State ------------------------------------


cluster_svyglm <- function(dvs, ivs, controls, dat, cluster_var, weight_var, out = NULL) {
  
  results <- list()
  
  # define survey design: clustering + weights
  des <- svydesign(
    ids = ~ get(cluster_var),   # cluster IDs (e.g., state)
    weights = ~ get(weight_var),
    data = dat
  )
  
  for (Y in dvs) {
    for (X in ivs) {
      # build formula
      form <- as.formula(
        paste(Y, "~", X, "+", paste(controls, collapse = " + "))
      )
      
      # fit weighted, clustered model
      mod <- svyglm(form, design = des, family = gaussian())
      
      results[[paste0("DV_", Y, "_IV_", X)]] <- mod
    }
  }
  
  if (!is.null(out)) {
    assign(out, results, envir = .GlobalEnv)
  } else {
    return(results)
  }
}


############# Hierarchical Models to nest respondents --------------------------

hier_lmer <- function(dvs, ivs, controls, dat, cluster_var, out = NULL, random_slopes = FALSE) {
  
  results <- list()
  
  for (Y in dvs) {
    for (X in ivs) {
      # Random intercepts
      rand <- paste0("(1 | ", cluster_var, ")")
      
      # Random slopes if requested
      if (random_slopes) {
        rand <- paste0("(", X, " | ", cluster_var, ")")
      }
      
      # Build formula
      form <- as.formula(
        paste(Y, "~", X, "+", paste(controls, collapse = " + "), "+", rand)
      )
      
      # Fit model
      mod <- lmer(form, data = dat)
      
      results[[paste0("DV_", Y, "_IV_", X)]] <- mod
    }
  }
  
  if (!is.null(out)) {
    assign(out, results, envir = .GlobalEnv)
  } else {
    return(results)
  }
}

############ Hierarchical using Bayesian --------------------------------------

cs_hier <- function(dvs, ivs, controls, dat, cluster_var, weight_var = NULL,
                    random_slopes = FALSE, family = gaussian(),
                    chains = 4, iter = 2000, cores = 4, out = NULL) {
  
  library(brms)
  library(survey)
  library(csSampling)
  
  results <- list()
  
  # create survey design object
  if (is.null(weight_var)) {
    survey_design <- svydesign(ids = ~1, data = dat)
  } else {
    survey_design <- svydesign(ids = ~1, data = dat, weights = as.formula(paste0("~", weight_var)))
  }
  
  for (Y in dvs) {
    for (X in ivs) {
      
      # random effects formula
      rand <- if(random_slopes) paste0("(", X, " | ", cluster_var, ")") else paste0("(1 | ", cluster_var, ")")
      
      # build model formula
      formula <- bf(
        as.formula(
          paste(Y, "~", X, "+", paste(controls, collapse = " + "), "+", rand)
        )
      )
      
      # Step 1: Fit brms model
      mod_brms <- brm(
        formula = formula,
        data = dat,
        family = family,
        chains = chains,
        iter = iter,
        cores = cores,
        seed = 123
      )
      
      mod_cs <-cs_sampling_brms(
        svydes = survey_design,
        brmsmod = mod_brms,   # <--- changed from mod_stan
        data = dat,           # <--- changed from data_stan
        family = family,
        ctrl_stan = list(chains = chains, iter = iter)
      )
      
      # save results
      results[[paste0("DV_", Y, "_IV_", X)]] <- mod_cs
    }
  }
  
  # return or assign globally
  if (!is.null(out)) {
    assign(out, results, envir = .GlobalEnv)
  } else {
    return(results)
  }
}

