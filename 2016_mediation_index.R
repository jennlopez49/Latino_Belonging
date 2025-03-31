### Mediation analysis ---------------------------------------------------------
############ Fear --------------------------------------------------------------
# Fit the mediator model (Model 1: M ~ X + controls)
fear_mediator <- svyglm(Fear_Election ~ ICI_Reverse + age_sqd + Gender + 
                           Education + Income + Linked_Fate + percent.latino.2016, 
                         design = cmps_lat_16, family = "gaussian")

# Fit the outcome model (Model 2: Y ~ X + M + controls)
model_outcome.fear <- svyglm(Inclusion_Index ~ ICI_Reverse + Fear_Election + 
                          age_sqd + Gender + Education + Income + Linked_Fate +
                          percent.latino.2016, 
                        design = cmps_lat_16, family = "gaussian")

# Run the mediation analysis
mediation_result.fear <- mediate(fear_mediator, model_outcome.fear, treat = "ICI_Reverse", mediator = "Fear_Election")


# Summarize the results
summary(mediation_result.fear)

#plot(mediation_result.fear)
############ Anger -------------------------------------------------------------
# Fit the mediator model (Model 1: M ~ X + controls)
anger_med <- svyglm(Angry_Election ~ ICI_collapsed_alt + age_sqd + Gender + 
                          Education + Income + Linked_Fate + percent.latino.2016, 
                        design = cmps_lat_16, family = "gaussian")

# Fit the outcome model (Model 2: Y ~ X + M + controls)
mod_anger <- svyglm(Inclusion_Index ~ ICI_collapsed_alt + Angry_Election + 
                               age_sqd + Gender + Education + Income + Linked_Fate +
                               percent.latino.2016, 
                             design = cmps_lat_16, family = "gaussian")

# Run the mediation analysis
med_anger_res <- mediate(anger_med, mod_anger, treat = "ICI_collapsed_alt", mediator = "Angry_Election")


# Summarize the results
summary(med_anger_res)
#plot(med_anger_res)

############ Sad ---------------------------------------------------------------
# Fit the mediator model (Model 1: M ~ X + controls)
sad_med <- svyglm(Sad_Election ~ ICI_collapsed_alt + age_sqd + Gender + 
                      Education + Income + Linked_Fate + percent.latino.2016, 
                    design = cmps_lat_16, family = "gaussian")

# Fit the outcome model (Model 2: Y ~ X + M + controls)
sad_mod <- svyglm(Inclusion_Index ~ ICI_collapsed_alt + Sad_Election + 
                      age_sqd + Gender + Education + Income + Linked_Fate +
                      percent.latino.2016, 
                    design = cmps_lat_16, family = "gaussian")

# Run the mediation analysis
sad_med_res <- mediate(sad_med, sad_mod, treat = "ICI_collapsed_alt", mediator = "Sad_Election")


# Summarize the results
summary(sad_med_res)
#plot(med_anger_res)

############ Pride -------------------------------------------------------------
# Fit the mediator model (Model 1: M ~ X + controls)
pride_med <- svyglm(Pride_Election ~ ICI_collapsed_alt + age_sqd + Gender + 
                    Education + Income + Linked_Fate + percent.latino.2016, 
                  design = cmps_lat_16, family = "gaussian")

# Fit the outcome model (Model 2: Y ~ X + M + controls)
pride_mod<- svyglm(Inclusion_Index ~ ICI_collapsed_alt + Pride_Election + 
                    age_sqd + Gender + Education + Income + Linked_Fate +
                    percent.latino.2016, 
                  design = cmps_lat_16, family = "gaussian")

# Run the mediation analysis
pride_med_res <- mediate(pride_med, pride_mod, treat = "ICI_collapsed_alt", mediator = "Pride_Election")


# Summarize the results
summary(pride_med_res)
#plot(med_anger_res)

############# Hopeful ---------------------------------------------------------
# Fit the mediator model (Model 1: M ~ X + controls)
hope_med <- svyglm(Hope_Election ~ ICI_collapsed_alt + age_sqd + Gender + 
                      Education + Income + Linked_Fate + percent.latino.2016, 
                    design = cmps_lat_16, family = "gaussian")

# Fit the outcome model (Model 2: Y ~ X + M + controls)
hope_mod<- svyglm(Inclusion_Index ~ ICI_collapsed_alt + Hope_Election + 
                     age_sqd + Gender + Education + Income + Linked_Fate +
                     percent.latino.2016, 
                   design = cmps_lat_16, family = "gaussian")

# Run the mediation analysis
hope_med_res <- mediate(hope_med, hope_mod, treat = "ICI_collapsed_alt", mediator = "Hope_Election")


# Summarize the results
summary(hope_med_res)
#plot(med_anger_res)

########## Separating out Index into Internal and External -----------------------
# Define Variables
dvs <- c("Inclusion_External", "Inclusion_Internal")  # List of dependent variables (Y)
ivs <- list("Imm_Con_Index", "ICI_Reverse")  # List of IVs (X)
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("age_sqd", "Gender", "Education", "Income", "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination_Scale", "Discrimination_National_Perc") 

# Run Mediation Analysis
med_results <- mediation_function(dvs, ivs, mediators, controls, des = cmps_lat_16, dat = cmps_lat_16)

# View Summary of a Mediation Result Example
summary(med_results$Inclusion_Internal$IV_Imm_Con_Index_Med_Fear_Election$mediation_result)
summary(med_results$Inclusion_Internal$IV_ICI_Reverse_Med_Fear_Election$mediation_result)

#### Pulling out Results --- 
create_single_mediation_table <- function(med_results, model_name, printone = TRUE, clp = 95) {
  
  # Split the model_name to determine which sublist to access (e.g., Inclusion_External or Inclusion_Internal)
  split_name <- strsplit(model_name, "\\$")[[1]]
  category <- split_name[1]  # Either "Inclusion_External" or "Inclusion_Internal"
  sub_model <- split_name[2] # The specific model (e.g., "IV_Imm_Con_Index_Med_Fear_Election")
  
  # Extract the specific model
  x <- med_results[[category]][[sub_model]]
  
  # Check if the necessary components exist, otherwise set them to NA
  d1 <- ifelse(!is.null(x$d1), x$d1, NA)
  d1_c <- ifelse(!is.null(x$d1.ci), x$d1.ci, NA)
  d1_p <- ifelse(!is.null(x$d1.p), x$d1.p, NA)
  
  z0 <- ifelse(!is.null(x$z0), x$z0, NA)
  z0_c <- ifelse(!is.null(x$z0.ci), x$z0.ci, NA)
  z0_p <- ifelse(!is.null(x$z0.p), x$z0.p, NA)
  
  tau_coef <- ifelse(!is.null(x$tau.coef), x$tau.coef, NA)
  tau_c <- ifelse(!is.null(x$tau.ci), x$tau.ci, NA)
  tau_p <- ifelse(!is.null(x$tau.p), x$tau.p, NA)
  
  n0 <- ifelse(!is.null(x$n0), x$n0, NA)
  n0_c <- ifelse(!is.null(x$n0.ci), x$n0.ci, NA)
  n0_p <- ifelse(!is.null(x$n0.p), x$n0.p, NA)
  
  # Create matrix with the components
  if (printone) {
    smat_model <- matrix(c(d1, d1_c, d1_p), nrow = 1)
    smat_model <- rbind(smat_model, c(z0, z0_c, z0_p))
    smat_model <- rbind(smat_model, c(tau_coef, tau_c, tau_p))
    smat_model <- rbind(smat_model, c(n0, n0_c, n0_p))
    
    rownames(smat_model) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
  } else {
    d0 <- ifelse(!is.null(x$d0), x$d0, NA)
    d0_c <- ifelse(!is.null(x$d0.ci), x$d0.ci, NA)
    d0_p <- ifelse(!is.null(x$d0.p), x$d0.p, NA)
    
    d1 <- ifelse(!is.null(x$d1), x$d1, NA)
    d1_c <- ifelse(!is.null(x$d1.ci), x$d1.ci, NA)
    d1_p <- ifelse(!is.null(x$d1.p), x$d1.p, NA)
    
    z1 <- ifelse(!is.null(x$z1), x$z1, NA)
    z1_c <- ifelse(!is.null(x$z1.ci), x$z1.ci, NA)
    z1_p <- ifelse(!is.null(x$z1.p), x$z1.p, NA)
    
    smat_model <- matrix(c(d0, d0_c, d0_p), nrow = 1)
    smat_model <- rbind(smat_model, c(d1, d1_c, d1_p))
    smat_model <- rbind(smat_model, c(z0, z0_c, z0_p))
    smat_model <- rbind(smat_model, c(z1, z1_c, z1_p))
    smat_model <- rbind(smat_model, c(tau_coef, tau_c, tau_p))
    smat_model <- rbind(smat_model, c(n0, n0_c, n0_p))
    smat_model <- rbind(smat_model, c(x$n1, x$n1.ci, x$n1.p))
    smat_model <- rbind(smat_model, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
    smat_model <- rbind(smat_model, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
    smat_model <- rbind(smat_model, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
    
    rownames(smat_model) <- c("ACME (control)", "ACME (treated)", 
                              "ADE (control)", "ADE (treated)", "Total Effect", 
                              "Prop. Mediated (control)", "Prop. Mediated (treated)", 
                              "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
  }
  
  # Assign appropriate column names
  colnames(smat_model) <- c("Estimate", paste(clp, "% CI Lower", sep = ""), 
                            paste(clp, "% CI Upper", sep = ""), "p-value")
  
  return(smat_model)
}

# Example of usage:
model_name <- "Inclusion_External$IV_Imm_Con_Index_Med_Fear_Election"
mediation_table <- create_single_mediation_table(med_results, model_name)

# Display the table
print(mediation_table)

extract_mediation_summary <- function (x) { 
  
  # Ensure that x is a model object with the expected structure
  clp <- 100 * x$conf.level
  isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) || 
                   (inherits(x$model.y, "glm") && x$model.y$family$family == 
                      "gaussian" && x$model.y$family$link == "identity") || 
                   (inherits(x$model.y, "survreg") && x$model.y$dist == 
                      "gaussian"))
  
  # Check if INT exists before using it, if not, set it to FALSE
  printone <- ifelse(!is.null(x$INT), !x$INT && isLinear.y, FALSE)
  
  # Construct the mediation summary table
  if (printone) {
    smat <- c(x$d1, x$d1.ci, x$d1.p)
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    
    rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
    
  } else {
    smat <- c(x$d0, x$d0.ci, x$d0.p)
    smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
    smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
    smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
    smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
    
    rownames(smat) <- c("ACME (control)", "ACME (treated)", 
                        "ADE (control)", "ADE (treated)", "Total Effect", 
                        "Prop. Mediated (control)", "Prop. Mediated (treated)", 
                        "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
  }
  
  # Add column names
  colnames(smat) <- c("Estimate", paste(clp, "% CI Lower", sep = ""), 
                      paste(clp, "% CI Upper", sep = ""), "p-value")
  
  return(smat)
}

# Example usage:
mediation_model <- med_results$Inclusion_External$IV_Imm_Con_Index_Med_Fear_Election
mediation_table <- extract_mediation_summary(mediation_model)

# Display the table
print(mediation_table)

mod1 <- med_results$Inclusion_External$IV_Imm_Con_Index_Med_Fear_Election$mediation_result
