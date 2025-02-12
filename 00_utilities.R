### Necessary Pkgs 
install.packages(c("haven", "survey", "sjPlot", "stargazer", "patchwork",
                 "tidycensus", "tigris", "sf"))
library(tidyverse)
library(haven)
library(survey)
library(sjPlot)
library(stargazer)
library(patchwork)
library(tidycensus)
library(tigris)
library(sf)
#install.packages("mediation")
library(mediation)
#install.packages("tidyverse", repos = "https://cloud.r-project.org/")
library(dplyr)


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
        
        # Store results in a structured way
        results_Y[[paste0("IV_", X, "_Med_", M)]] <- list(
          mediator_model = mediator_model,
          outcome_model = outcome_model,
          mediation_result = mediation_result
        )
      }
    }
    
    mediation_results[[Y]] <- results_Y  # Store results for this dependent variable
  }
  
  # Assign to global environment if 'out' is provided, otherwise return results
  if (!is.null(out)) {
    if (!is.character(out)) stop("Argument 'out' must be a character string")
    assign(out, mediation_results, envir = .GlobalEnv)
  } else {
    return(mediation_results)
  }
}
