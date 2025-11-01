############### DISCRIMINATION MODELS -----------------------------------------

# Vars
dvs <- c("External_Belonging", "Internal_Belonging")  # List of DVs (Y)
# ivs <- list("ICI_Reverse", "Imm_Con_Index", "ICI_Reverse_Fac")  # List of IVs (X) ### OLD INDICATOR
ivs <- list("conc_lat_index_16","latino_conc_16", "class.conc_lat_14_16")
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
          "Sad_Election"
          # ,"Discrimination_Scale",
          # "Latino_Disc"
          ),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

res_sb_fear <- cluster_svyglm(
  dvs = c("External_Belonging", "Internal_Belonging"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Fear_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

res_sb_angry <- cluster_svyglm(
  dvs = c("External_Belonging", "Internal_Belonging"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Angry_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

res_sb_sad <- cluster_svyglm(
  dvs = c("External_Belonging", "Internal_Belonging"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Sad_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)
res_sb_pride<- cluster_svyglm(
  dvs = c("External_Belonging", "Internal_Belonging"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Pride_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

res_sb_hope <- cluster_svyglm(
  dvs = c("External_Belonging", "Internal_Belonging"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Hope_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

listmods_sb_ext <- setNames(
  list(
    res_sb_fear$DV_External_Belonging_IV_class.conc_lat_14_16, 
    res_sb_angry$DV_External_Belonging_IV_class.conc_lat_14_16, 
    res_sb_sad$DV_External_Belonging_IV_class.conc_lat_14_16, 
    res_sb_hope$DV_External_Belonging_IV_class.conc_lat_14_16, 
    res_sb_pride$DV_External_Belonging_IV_class.conc_lat_14_16
  ),
  c("Fear & External Belonging", 
    "Anger & External Belonging", 
    "Sadness & External Belonging", 
    "Hope & External Belonging", 
    "Pride & External Belonging")
)

listmods_sb_int <- setNames(
  list(
    res_sb_fear$DV_Internal_Belonging_IV_class.conc_lat_14_16, 
    res_sb_angry$DV_Internal_Belonging_IV_class.conc_lat_14_16, 
    res_sb_sad$DV_Internal_Belonging_IV_class.conc_lat_14_16, 
    res_sb_hope$DV_Internal_Belonging_IV_class.conc_lat_14_16, 
    res_sb_pride$DV_Internal_Belonging_IV_class.conc_lat_14_16
  ),
  c("Fear & Internal Belonging", 
    "Anger & Internal Belonging", 
    "Sadness & Internal Belonging", 
    "Hope & Internal Belonging", 
    "Pride & Internal Belonging")
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

# find_csv_creator <- function(csv_name) {
#   for (f in list.files(".", pattern = "\\.R$", recursive = TRUE)) {
#     lines <- readLines(f, warn = FALSE)
#     if (any(grepl(csv_name, lines, fixed = TRUE))) {
#       cat("Found in:", f, "\n")
#     }
#   }
# }

# Example: search for where final_dataset.csv is written
# find_csv_creator("scores_final.csv")


#### Table ---> basic, basic with clustered SEs, full 
# stargazer(med_basic_ols$mediator_models, type = "text")
neg_basic <- med_basic_ols$mediator_models[c(1,2,5)]
pos_basic <- med_basic_ols$mediator_models[c(3,4)]
neg_full <- med_full_ols$mediator_models[c(1,2,5)]
pos_full <- med_full_ols$mediator_models[c(3,4)]
neg_cl <- res[c(1,2,5)]
pos_cl <- res[c(3,4)]
# stargazer(res, type = "text")
# stargazer(med_full_ols$mediator_models, type = "text")

stargazer(med_basic_ols$mediator_models,res, med_full_ols$mediator_models,
          type = "text", font.size = "footnotesize")


stargazer(neg_basic, neg_full, neg_cl,
          type = "latex",
          dep.var.labels = c("Fear", "Anger", "Sadness",
                             "Fear", "Anger", "Sadness", 
                             "Fear", "Anger", "Sadness"),
          covariate.labels = c("Concrete Imm. Index",
                                 "Age", "Gender",
                                 "Education", "Income", "Political Interest",
                                 "Mexican", "Cuban", "Linked Fate",
                               "Party (R $\\longrightarrow$ D)",
                                 "Generation", "Discrimination Exp.",
                                 "Group Discrimination Percep.",
                                 "Constant"),
          # column.labels = c("Basic", "Full", "Basic + Clustered SEs"),
          add.lines = list(
            c("Clustered SEs", "No", "No", "No",
              "No", "No", "No", "Yes", "Yes", "Yes")
          ),
          label = "neg.med", out = "neg.med.tex")

stargazer(pos_basic, pos_full, pos_cl,
          type = "latex",
          dep.var.labels = c("Pride", "Hope",
                             "Pride", "Hope",
                             "Pride", "Hope"),
          # column.labels = c("Basic", "Full", "Basic + Clustered SEs"),
          covariate.labels = c("Concrete Imm. Index",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Linked Fate",
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.",
                               "Constant"),
          add.lines = list(
            c("Clustered SEs", "No", "No", "No",
              "No", "Yes", "Yes")
          ),
          label = "pos.med", out = "pos.med.tex")


int_basic <- med_basic_ols$outcome_models[c(1:5)]
# ext_basic <- med_basic_ols$outcome_models[c(6,7,10)]
ext_basic <- med_basic_ols$outcome_models[c(6:10)]
# ext_basic <- med_basic_ols$outcome_models[c()]

int_full <- med_full_ols$outcome_models[c(1:5)]
ext_full <- med_full_ols$outcome_models[c(6:10)]

stargazer(int_basic, listmods_sb_int, int_full, type = "latex",
          covariate.labels = c("Concrete Imm. Index",
                               "Fear", "Angry", "Pride", "Hope",
                               "Sad",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Linked Fate",
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.",
                               "Constant"),
          add.lines = list(
            c("Clustered SEs", "No", "No", "No",
              "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes",
              "No", "No", "No", "No", "No")
          ),
          label = "int.cl", out = "int.cl.tex"
          )
stargazer(ext_basic, listmods_sb_ext, ext_full, type = "latex",
          covariate.labels = c("Concrete Imm. Index",
                               "Fear", "Angry", "Pride", "Hope",
                               "Sad",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Linked Fate",
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.",
                               "Constant"),
          add.lines = list(
            c("Clustered SEs", "No", "No", "No",
              "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes",
              "No", "No", "No", "No", "No")
          ),
          label = "ext.cl", out = "ext.cl.tex"
)


######### Ch. 3 Models ---------------

# DVS -- BorderSecurity, Pathway_Citizenship

res_ch3_fear <- cluster_svyglm(
  dvs = c("BorderSecurity", "Pathway_Citizenship", "Pathway_Deport"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Fear_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

res_ch3_anger <- cluster_svyglm(
  dvs = c("BorderSecurity", "Pathway_Citizenship", "Pathway_Deport"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Angry_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

res_ch3_sad <- cluster_svyglm(
  dvs = c("BorderSecurity", "Pathway_Citizenship", "Pathway_Deport"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Sad_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

res_ch3_pride <- cluster_svyglm(
  dvs = c("BorderSecurity", "Pathway_Citizenship", "Pathway_Deport"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Pride_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

res_ch3_hope <- cluster_svyglm(
  dvs = c("BorderSecurity", "Pathway_Citizenship", "Pathway_Deport"),
  ivs = c("class.conc_lat_14_16"),
  controls = c("Age", "Gender", "Party",
               "More_Than_SecondGen", "Hope_Election"),
  dat = cmps_lat_16$variables,
  cluster_var = "State",       # your clustering variable
  weight_var = "Weight" # your survey weight variable
)

# 
listmods_ch3 <- setNames(
  list(
    res_ch3_fear$DV_BorderSecurity_IV_class.conc_lat_14_16, 
    res_ch3_fear$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_fear$DV_Pathway_Deport_IV_class.conc_lat_14_16, 
    res_ch3_anger$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_anger$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_anger$DV_Pathway_Deport_IV_class.conc_lat_14_16, 
    res_ch3_sad$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_sad$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_sad$DV_Pathway_Deport_IV_class.conc_lat_14_16, 
    res_ch3_hope$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_hope$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_hope$DV_Pathway_Deport_IV_class.conc_lat_14_16, 
    res_ch3_pride$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_pride$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_pride$DV_Pathway_Deport_IV_class.conc_lat_14_16
  ),
  c("Fear & Border Security", 
    "Fear & Pathway-Deportations", 
    "Fear & Pathway for Citizenship", 
    "Anger & Border Security",
    "Anger & Pathway-Deportations", 
    "Anger & Pathway for Citizenship", 
    "Sad & Border Security",
    "Sad & Pathway-Deportations", 
    "Sad & Pathway for Citizenship", 
    "Hope & Border Security",
    "Hope & Pathway-Deportations", 
    "Hope & Pathway for Citizenship", 
    "Pride & Border Security",
    "Pride & Pathway-Deportations", 
    "Pride & Pathway for Citizenship")
)

listmods_bs <- setNames(
  list(
    res_ch3_fear$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_anger$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_sad$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_hope$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_pride$DV_BorderSecurity_IV_class.conc_lat_14_16
  ),
  c("Fear & Border Security",
    "Anger & Border Security",
    "Sad & Border Security",
    "Hope & Border Security",
    "Pride & Border Security")
)

listmods_pathway <- setNames(
  list(
    res_ch3_fear$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_fear$DV_Pathway_Deport_IV_class.conc_lat_14_16, 
    res_ch3_anger$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_anger$DV_Pathway_Deport_IV_class.conc_lat_14_16, 
    res_ch3_sad$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_sad$DV_Pathway_Deport_IV_class.conc_lat_14_16, 
    res_ch3_hope$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_hope$DV_Pathway_Deport_IV_class.conc_lat_14_16, 
    res_ch3_pride$DV_Pathway_Citizenship_IV_class.conc_lat_14_16, 
    res_ch3_pride$DV_Pathway_Deport_IV_class.conc_lat_14_16
  ),
  c(
    "Fear & Pathway-Deportations", 
    "Fear & Pathway for Citizenship", 
    "Anger & Pathway-Deportations", 
    "Anger & Pathway for Citizenship", 
    "Sad & Pathway-Deportations", 
    "Sad & Pathway for Citizenship", 
    "Hope & Pathway-Deportations", 
    "Hope & Pathway for Citizenship", 
    "Pride & Pathway-Deportations", 
    "Pride & Pathway for Citizenship")
)

stargazer(listmods_bs, type = "latex",
          covariate.labels = c("Concrete Imm. Index", "Age",
                               "Gender", "Party (R $\\longrightarrow$ D)", 
                               "Second Gen +",
                               "Fear", "Anger", "Sad", "Hope", "Pride", 
                               "Constant"),
          dep.var.labels = "Tighten Border Security",
          out = "bs_mods.tex")  
stargazer(listmods_pathway[c(1,3,5,7,9)], type = "latex",
          covariate.labels = c("Concrete Imm. Index", "Age",
                               "Gender", "Party (R $\\longrightarrow$ D)", 
                               "Second Gen +",
                               "Fear", "Anger", "Sad", "Hope", "Pride", 
                               "Constant"),
          dep.var.labels = c("Pathway for Citizenship"),
          out = "cit_mods.tex")
