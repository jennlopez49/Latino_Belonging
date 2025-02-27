#### General Vars For Int & Med Models ---------
dvs <- c("Inclusion_External", "Inclusion_Internal")  # List of DVs (Y)
ivs <- list("ICI_Score_2016", "ICI_collapsed_alt", "ICI_Index")  # List of IVs (X)
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
              "Mexican", "Cuban",
              "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination_Scale", 
              "Discrimination_National_Perc") 

################ No interaction/ Basic Models -------------------------------------
## List of Models
no_int_list <- list()

no_int_list[[1]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican","Cuban", "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale")
no_int_list[[2]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican","Cuban","Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_Score_2016")
no_int_list[[3]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican","Cuban","Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale", 
                      "Discrimination_National_Perc", "ICI_collapsed_alt")
no_int_list[[4]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican","Cuban","Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt",
                      "Fear_Election")
no_int_list[[5]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican","Cuban","Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt",
                      "Angry_Election")
no_int_list[[6]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican","Cuban","Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt",
                      "Sad_Election")
no_int_list[[7]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican","Cuban","Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt", 
                      "Pride_Election")
no_int_list[[8]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
                      "Mexican","Cuban","Linked_Fate", "Party",
  "More_Than_SecondGen", "Discrimination_Scale", "Discrimination_National_Perc",
  "ICI_collapsed_alt", "Hope_Election")

## Run the models

ols_function(dvs, no_int_list, cmps_lat_16, cmps_lat_16, "no_int_models")

# Produce Stargazer Tables - 

stargazer(no_int_models$Inclusion_Internal[1:3], type = "latex", out = "prelim_general_models.tex")

# Running with Standardization of Vars to ease interpretation - 

ols_function_standard(dvs, no_int_list, cmps_lat_16, cmps_lat_16, "no_int_models_standard")

### Producing Tables ----

stargazer(no_int_models_standard$Inclusion_Internal[c(1,4:8)], type = "text")
stargazer(no_int_models_standard$Inclusion_Internal[1:3], 
          no_int_models_standard$Inclusion_External[1:3], type = "latex", 
          out = "prelim_general_models.tex")


#### Run function for Mediation models------------------------------------------


mediation_function_standard(dvs, ivs, mediators, controls, cmps_lat_16, cmps_lat_16, "med_results_ols")

#### PRODUCING TABLES OF STIGMA --> EMOTION ------------------------------------

stargazer(med_results_ols$mediator_models[c(2,3,5,6, 14,15)], type = "latex",
          covariate.labels = c("ICI Index", "ICI Index (-1)", "ICI Index (0)",
                               "ICI Index (0.5)", "Linked Fate", "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party ($\\longright arrow$ D)",
                               "Second Generation +", "Discrimination Exp.",
                               "Nat. Discrimination Percep.", "Constant"),
          dep.var.labels = c("Fear", "Anger", "Sad"), 
          dep.var.caption = "Dependent variable: Negative Emotions", 
          out = "neg.em.gen.tex")

stargazer(med_results_ols$mediator_models[c(8,9,11,12)], type = "latex",
          covariate.labels = c("ICI Index", "ICI Index (-1)", "ICI Index (0)",
                               "ICI Index (0.5)", "Linked Fate", "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party ($\\longright arrow$ D)",
                               "Second Generation +", "Discrimination Exp.",
                               "Nat. Discrimination Percep.", "Constant"),
          dep.var.labels = c("Pride", "Hope"), 
          dep.var.caption = "Dependent variable: Positive Emotions",
          out = "pos.em.gen.tex")

#### Producing tables testing stigma + lf + emotions --> social ----------------

# Internal -----------

# Select specific models manually by name (instead of index)
selected_models <- med_results_ols$outcome_models[c(16:30)]  # Start with all

# Rearrange the last two models to be in the middle
reordered_models <- c(
  selected_models[c(2,3,5,6)],  # First set (e.g., negative emotions)
  selected_models[c(14,15)], # Move last two models here
  selected_models[c(8,9,11,12)]   # Remaining models
)

# Display in Stargazer
stargazer(reordered_models, type = "latex", dep.var.labels = "Internal Inclusion",
          covariate.labels = c("ICI Index", "ICI Index (-1)", "ICI Index (0)",
                               "ICI Index (0.5)", 
                               "Fear", "Anger", "Sad", "Pride", "Hope",
                               "Linked Fate", "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party ($\\longright arrow$ D)",
                               "Second Generation +", "Discrimination Exp.",
                               "Nat. Discrimination Percep.", "Constant"),
          out = "fullmodel_internal.tex")
# External  -----------

# Rearrange the last two models to be in the middle --- did not index / separate the models because external models were 1 - 15 
reordered_models_ext <- c(
  med_results_ols$outcome_models[c(2,3,5,6)],  # First set (e.g., negative emotions)
  med_results_ols$outcome_models[c(14,15)], # Move last two models here
  med_results_ols$outcome_models[c(8,9,11,12)]   # Remaining models
)

stargazer(reordered_models_ext, type = "latex", dep.var.labels = "External Inclusion",
          covariate.labels = c("ICI Index", "ICI Index (-1)", "ICI Index (0)",
                               "ICI Index (0.5)", 
                               "Fear", "Anger", "Sad", "Pride", "Hope",
                               "Linked Fate", "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party ($\\longright arrow$ D)",
                               "Second Generation +", "Discrimination Exp.",
                               "Nat. Discrimination Percep.", "Constant"),
          out = "fullmodel_external.tex")
#### Testing out interactions between stigma x lf + emotions --> social? -----
interaction_function_standard(dvs, ivs, mediators, controls, des = cmps_lat_16, 
                              dat = cmps_lat_16, "int_models")


# WRONG SET UP FOR THE THEORY ---------
# #### Models with Fear ------------------------------
# fear_models <- list(
#   m1 = no_int_models_standard$Inclusion_Internal[4], 
#   m2 = int_models_standard$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Fear_Election,
#   m3 = no_int_models_standard$Inclusion_External[4],
#   m4 = int_models_standard$Inclusion_External$IV_ICI_collapsed_alt_Med_Fear_Election
# )
# 
# stargazer(fear_models, type = "latex", out = "prelim_models_fear.tex")
# 
# #### Models with Anger ------------------------------
# anger_models <- list(
#   m1 = no_int_models_standard$Inclusion_Internal[5], 
#   m2 = int_models_standard$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Angry_Election,
#   m3 = no_int_models_standard$Inclusion_External[5],
#   m4 = int_models_standard$Inclusion_External$IV_ICI_collapsed_alt_Med_Angry_Election
# )
# 
# stargazer(anger_models, type = "latex", out = "prelim_models_anger.tex")
# 
# #### Models with Sadness ------------------------------
# sad_models <- list(
#   m1 = no_int_models_standard$Inclusion_Internal[6], 
#   m2 = int_models_standard$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Sad_Election,
#   m3 = no_int_models_standard$Inclusion_External[6],
#   m4 = int_models_standard$Inclusion_External$IV_ICI_collapsed_alt_Med_Sad_Election
# )
# 
# stargazer(sad_models, type ="latex", out = "prelim_models_sad.tex")
# 
# #### Models with Pride ------------------------------
# pride_models <- list(
#   m1 = no_int_models_standard$Inclusion_Internal[7], 
#   m2 = int_models_standard$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Pride_Election,
#   m3 = no_int_models_standard$Inclusion_External[7],
#   m4 = int_models_standard$Inclusion_External$IV_ICI_collapsed_alt_Med_Pride_Election
# )
# 
# stargazer(pride_models, type ="latex", out = "prelim_models_pride.tex")
# 
# #### Models with Hope ------------------------------
# hope_models <- list(
#   m1 = no_int_models_standard$Inclusion_Internal[8], 
#   m2 = int_models_standard$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Hope_Election,
#   m3 = no_int_models_standard$Inclusion_External[8],
#   m4 = int_models_standard$Inclusion_External$IV_ICI_collapsed_alt_Med_Hope_Election
# )
# 
# stargazer(hope_models, type ="latex", out = "prelim_models_hope.tex")
# 
