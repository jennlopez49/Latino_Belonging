### Mediation analysis ---------------------------------------------------------
############ Fear --------------------------------------------------------------
# Fit the mediator model (Model 1: M ~ X + controls)
fear_mediator <- svyglm(Fear_Election ~ ICI_collapsed_alt + age_sqd + Gender + 
                           Education + Income + Linked_Fate + percent.latino.2016, 
                         design = cmps_lat_16, family = "gaussian")

# Fit the outcome model (Model 2: Y ~ X + M + controls)
model_outcome.fear <- svyglm(Inclusion_Index ~ ICI_collapsed_alt + Fear_Election + 
                          age_sqd + Gender + Education + Income + Linked_Fate +
                          percent.latino.2016, 
                        design = cmps_lat_16, family = "gaussian")

# Run the mediation analysis
mediation_result.fear <- mediate(fear_mediator, model_outcome.fear, treat = "ICI_collapsed_alt", mediator = "Fear_Election")


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
ivs <- list("ICI_Score_2016", "ICI_collapsed_alt")  # List of IVs (X)
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("age_sqd", "Gender", "Education", "Income", "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination") 

# Run Mediation Analysis
med_results <- mediation_function(dvs, ivs, mediators, controls, des = cmps_lat_16, dat = cmps_lat_16)

# View Summary of a Mediation Result
summary(med_results$Inclusion_External$IV_ICI_collapsed_alt_Med_Angry_Election$mediation_result)

