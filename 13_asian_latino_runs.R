### Asian & Latino Models (No Latino-Specific Vars) -----------------------------

# Vars
dvs <- c("Inclusion_External", "Inclusion_Internal")  # List of DVs (Y)
# ivs <- list("ICI_Reverse", "Imm_Con_Index", "ICI_Reverse_Fac")  # List of IVs (X) ### OLD INDICATOR
ivs <- list("conc_for_index_16","foreign_conc_16")
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
              "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination_Scale",
              "Group_Disc", 
              # "Worry_Deport", 
              "Latino")


###
mediation_function_standard(dvs, ivs, mediators, controls, cmps_imm_16, 
                            cmps_imm_16, out ="results_ols_imm")


#### Interaction to see emotions based on stigma * immigrant group 
controls_int <- controls[controls != "Latino"]
fear_int <- paste("Fear_Election ~ conc_for_index_16*Latino +", 
                  paste(c(controls_int), collapse = " + "))
anger_int <- paste("Angry_Election ~ conc_for_index_16*Latino +", 
                      paste(c(controls_int), collapse = " + "))
sad_int <- paste("Sad_Election ~ conc_for_index_16*Latino +", 
                  paste(c(controls_int), collapse = " + "))
pride_int <- paste("Pride_Election ~ conc_for_index_16*Latino +", 
                      paste(c(controls_int), collapse = " + "))
hope_int <- paste("Hope_Election ~ conc_for_index_16*Latino +", 
                   paste(c(controls_int), collapse = " + "))

fint <- svyglm(fear_int,
                  design = cmps_imm_16)
aint <- svyglm(anger_int,
                  design = cmps_imm_16)
sint <- svyglm(sad_int,
                  design = cmps_imm_16)
pint <- svyglm(pride_int,
                  design = cmps_imm_16)
hint <- svyglm(hope_int,
               design = cmps_imm_16)
mods_ints <- c(fint, aint, sint, pint, hint)

stargazer(fint, aint, sint, pint, hint, type = "text")
