############### DISCRIMINATION MODELS -----------------------------------------

# Vars
dvs <- c("Inclusion_External", "Inclusion_Internal")  # List of DVs (Y)
# ivs <- list("ICI_Reverse", "Imm_Con_Index", "ICI_Reverse_Fac")  # List of IVs (X) ### OLD INDICATOR
ivs <- list("conc_lat_index_16","latino_conc_16")
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election","Discrimination_Scale",
                  "Latino_Disc")  # List of Mediators (M)
simp_controls <- c("Age", "Gender", "Party",
              "More_Than_SecondGen")


###
mediation_function_standard(dvs, ivs, mediators, simp_controls, cmps_lat_16, 
                            cmps_lat_16, out ="med_results_ols")

### Clustering SEs at the state level
res <- cluster_svyglm(
  dvs = c("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
          "Sad_Election","Discrimination_Scale",
          "Latino_Disc"),
  ivs = c("conc_lat_index_16","latino_conc_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

##################### Running Bayesian Mods ------------------------------------
res_cs <- cs_hier(
  dvs = c("Discrimination_Scale", "Latino_Disc"),
  ivs = c("conc_lat_index_16","latino_conc_16"),
  controls = c("Age", "Gender", "Party", "More_Than_SecondGen"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",
  weight_var = "Weight",
  random_slopes = FALSE,
  family = gaussian(),
  chains = 4,
  iter = 2000,
  cores = 4
)



