#### General Vars For Int & Med Models ---------
dvs <- c("Inclusion_External", "Inclusion_Internal")  # List of DVs (Y)
ivs <- list("ICI_Score_2016", "ICI_collapsed_alt")  # List of IVs (X)
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination_Scale", 
              "Discrimination_National_Perc") 

################ No interaction/ Basic Models -------------------------------------
## List of Models
no_int_list <- list()

no_int_list[[1]] <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", 
                      "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale")
no_int_list[[2]] <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", 
                      "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt")
no_int_list[[3]] <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", 
                      "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale", 
                      "Discrimination_National_Perc", "ICI_collapsed_alt")
no_int_list[[4]] <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", 
                      "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt",
                      "Fear_Election")
no_int_list[[5]] <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", 
                      "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt",
                      "Angry_Election")
no_int_list[[6]] <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", 
                      "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt",
                      "Sad_Election")
no_int_list[[7]] <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", 
                      "Linked_Fate", "Party",
                      "More_Than_SecondGen", "Discrimination_Scale",
                      "Discrimination_National_Perc", "ICI_collapsed_alt", 
                      "Pride_Election")
no_int_list[[8]] <- c("age_sqd", "Gender", "Education", "Income", "Pol_Interest", 
                      "Linked_Fate", "Party",
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


#### Run function for Interaction models--------------------------------------------
int_results <- interaction_function(dvs, ivs, mediators, controls, 
                                    des = cmps_lat_16, dat = cmps_lat_16)

interaction_function_standard(dvs, ivs, mediators, controls, des = cmps_lat_16, 
                              dat = cmps_lat_16, "int_models_standard")

#### Models with Fear ------------------------------
fear_models <- list(
  m1 = no_int_models$Inclusion_Internal[4], 
  m2 = int_results$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Fear_Election,
  m3 = no_int_models$Inclusion_External[4],
  m4 = int_results$Inclusion_External$IV_ICI_collapsed_alt_Med_Fear_Election
)

stargazer(fear_models, type = "latex", out = "prelim_models_fear.tex")

#### Models with Anger ------------------------------
anger_models <- list(
  m1 = no_int_models$Inclusion_Internal[5], 
  m2 = int_results$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Angry_Election,
  m3 = no_int_models$Inclusion_External[5],
  m4 = int_results$Inclusion_External$IV_ICI_collapsed_alt_Med_Angry_Election
)

stargazer(anger_models, type = "latex", out = "prelim_models_anger.tex")

#### Models with Sadness ------------------------------
sad_models <- list(
  m1 = no_int_models$Inclusion_Internal[6], 
  m2 = int_results$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Sad_Election,
  m3 = no_int_models$Inclusion_External[6],
  m4 = int_results$Inclusion_External$IV_ICI_collapsed_alt_Med_Sad_Election
)

stargazer(sad_models, type ="latex", out = "prelim_models_sad.tex")

#### Models with Pride ------------------------------
pride_models <- list(
  m1 = no_int_models$Inclusion_Internal[7], 
  m2 = int_results$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Pride_Election,
  m3 = no_int_models$Inclusion_External[7],
  m4 = int_results$Inclusion_External$IV_ICI_collapsed_alt_Med_Pride_Election
)

stargazer(pride_models, type ="latex", out = "prelim_models_pride.tex")

#### Models with Hope ------------------------------
hope_models <- list(
  m1 = no_int_models$Inclusion_Internal[8], 
  m2 = int_results$Inclusion_Internal$IV_ICI_collapsed_alt_Med_Hope_Election,
  m3 = no_int_models$Inclusion_External[8],
  m4 = int_results$Inclusion_External$IV_ICI_collapsed_alt_Med_Hope_Election
)

stargazer(hope_models, type ="latex", out = "prelim_models_hope.tex")

