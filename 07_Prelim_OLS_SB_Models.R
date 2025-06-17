#### General Vars For Int & Med Models ---------
cmps_lat_16$variables$ICI_Reverse_Fac <- as.factor(cmps_lat_16$variables$ICI_Reverse)
dvs <- c("Inclusion_External", "Inclusion_Internal")  # List of DVs (Y)
ivs <- list("ICI_Reverse", "Imm_Con_Index", "ICI_Reverse_Fac")  # List of IVs (X)
mediators <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
              "Mexican", "Cuban",
              "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination_Scale", 
              "Latino_Disc", "Spanish_Media", "Worry_Deport")

# ################ No interaction/ Basic Models -------------------------------------
# ## List of Models
# no_int_list <- list()
# 
# no_int_list[[1]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban", "Linked_Fate", "Party",
#                       "More_Than_SecondGen", "Discrimination_Scale")
# no_int_list[[2]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban","Linked_Fate", "Party",
#                       "More_Than_SecondGen", "Discrimination_Scale",
#                       "Discrimination_National_Perc", "Imm_Con_Index")
# no_int_list[[3]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban","Linked_Fate", "Party",
#                       "More_Than_SecondGen", "Discrimination_Scale", 
#                       "Discrimination_National_Perc", "ICI_Reverse")
# no_int_list[[4]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban","Linked_Fate", "Party",
#                       "More_Than_SecondGen", "Discrimination_Scale", 
#                       "Discrimination_National_Perc", "ICI_Reverse_Fac")
# no_int_list[[5]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban","Linked_Fate", "Party",
#                       "More_Than_SecondGen", "Discrimination_Scale",
#                       "Discrimination_National_Perc", "ICI_Reverse",
#                       "Fear_Election")
# no_int_list[[6]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban","Linked_Fate", "Party",
#                       "More_Than_SecondGen", "Discrimination_Scale",
#                       "Discrimination_National_Perc", "ICI_Reverse",
#                       "Angry_Election")
# no_int_list[[7]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban","Linked_Fate", "Party",
#                       "More_Than_SecondGen", "Discrimination_Scale",
#                       "Discrimination_National_Perc", "ICI_Reverse",
#                       "Sad_Election")
# no_int_list[[8]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban","Linked_Fate", "Party",
#                       "More_Than_SecondGen", "Discrimination_Scale",
#                       "Discrimination_National_Perc", "ICI_Reverse", 
#                       "Pride_Election")
# no_int_list[[9]] <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
#                       "Mexican","Cuban","Linked_Fate", "Party",
#   "More_Than_SecondGen", "Discrimination_Scale", "Discrimination_National_Perc",
#   "ICI_Reverse", "Hope_Election")
# 
# ## Run the models
# 
# ols_function(dvs, no_int_list, cmps_lat_16, cmps_lat_16, "no_int_models")
# 
# # Produce Stargazer Tables - 
# 
# stargazer(no_int_models$Inclusion_Internal[1:3], type = "text", out = "prelim_general_models.tex")
# 
# # Running with Standardization of Vars to ease interpretation - 
# 
# ols_function_standard(dvs, no_int_list, cmps_lat_16, weight_var, cmps_lat_16, "no_int_models_standard")
# 
# ### Producing Tables ----
# 
# stargazer(no_int_models_standard$Inclusion_Internal[c(1,2,3, 4:8)], type = "text")
# stargazer(no_int_models_standard$Inclusion_External[c(1,2,3,4:8)], type = "text")
# stargazer(no_int_models_standard$Inclusion_Internal[1:4], 
#           no_int_models_standard$Inclusion_External[1:4], type = "latex", 
#           out = "prelim_general_models.tex")
# 

#### Run function for Mediation models------------------------------------------


mediation_function_standard(dvs, ivs, mediators, controls, cmps_lat_16, cmps_lat_16, out ="med_results_ols")

#### PRODUCING TABLES OF STIGMA --> EMOTION ------------------------------------
# list_mods <- med_results_ols$mediator_models[c(2,3,5,6, 14,15)]

# stargazer(med_results_ols$mediator_models, type = "text",
#           # covariate.labels = c("Imm. Stigma", "Imm. Stigma (-0.5)",
#           #                      "Imm. Stigma (0.5)", "Imm. Stigma (1)", "Linked Fate", 
#           #                      "Age", "Gender",
#           #                      "Education", "Income", "Political Interest",
#           #                      "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
#           #                      "Generation", "Discrimination Exp.",
#           #                      "Group Discrimination Percep.", 
#           #                      "Spanish Media", "Deportation Worry",
#           #                      "Constant"),
#           dep.var.labels = c("Fear", "Anger", "Sad"), 
#           dep.var.caption = "Dependent variable: Negative Emotions"
#           #,
#           #out = "neg.em.gen.full.tex"
#           )
listmods <- med_results_ols$mediator_models[c(1,3,4,6,13,15)]
stargazer(listmods, type = "text",
          # covariate.labels = c("Imm. Stigma", "Imm. Stigma (-0.5)",
          # "Imm. Stigma (0.5)", "Imm. Stigma (1)", "Linked Fate",
          # "Age", "Gender",
          # "Education", "Income", "Political Interest",
          # "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
          # "Generation", "Discrimination Exp.",
          # "Group Discrimination Percep.","Deportation Worry",
          # "Constant"),
          dep.var.labels = c("Fear", "Anger", "Sadness"), 
          dep.var.caption = "Dependent variable: Negative Emotions"
          # ,
          # out = "neg.em.gen.short.tex"
)

### Figures ----
negp <- plot_model(listmods$IV_ICI_Reverse_Fac_Med_Fear_Election, type = "est", 
                   title = "Fear")
angp <- plot_model(listmods$IV_ICI_Reverse_Fac_Med_Angry_Election, type = "est",
                   title = "Anger")
                   
### positive ------

posem <- med_results_ols$mediator_models[c(7,9,10,12)]

stargazer(posem, type = "latex",
          covariate.labels = c("Imm. Stigma", "Imm. Stigma (-0.5)",
                               "Imm. Stigma (0.5)", "Imm. Stigma (1)", "Linked Fate",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Media", "Deportation Worry",
                               "Constant"),
          dep.var.caption = "Dependent variable: Positive Emotions"
          ,
          out = "pos.em.gen.tex"
          )

## Figures -- pos ------
proudp <- plot_model(posem$IV_ICI_Reverse_Fac_Med_Pride_Election, type = "est",
                     title = "Pride")
hopep <- plot_model(posem$IV_ICI_Reverse_Fac_Med_Hope_Election, type = "est",
                    title = "Hope")

### Putting figures together 
negem.fig <- negp + angp + plot_annotation(title = "Predictors of Negative Emotions") & 
  theme(plot.title = element_text(hjust = 0.5))
posem.fig <- proudp + hopep + plot_annotation(title = "Predictors of Positive Emotions") & 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "negem_fig.png",  
       plot = negem.fig,                             
       width = 10,
       height = 8,                        
       dpi = 300) 

ggsave(filename = "posem_fig.png",  
       plot = posem.fig,                             
       width = 10,
       height = 8,                        
       dpi = 300) 
#### Producing tables testing stigma + lf + emotions --> social ----------------

# Internal -----------

# Select specific models manually by name (instead of index)
selected_models <- med_results_ols$outcome_models[c(16:30)]  # Start with all

# Rearrange the last two models to be in the middle
reordered_models <- c(
  selected_models[c(1,3,4,6)],  # First set (e.g., negative emotions)
  selected_models[c(13,15)], # Move last two models here
  selected_models[c(7,9,10,12)]   # Remaining models
)

# Display in Stargazer
stargazer(reordered_models, type = "latex", dep.var.labels = "Internal Inclusion",
          covariate.labels = c("Imm Stigma",  "Imm Stigma (-0.5)",
                               "Imm Stigma (0.5)", "Imm Stigma (1)",
                               "Fear", "Anger", "Sad", "Pride", "Hope",
                               "Linked Fate", "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.",
                               "Spanish Media", "Deportation Worry","Constant")
          ,
          out = "fullmodel_internal.tex"
          )

fearp1 <- plot_model(reordered_models$DV_Inclusion_Internal_IV_ICI_Reverse_Fac_Med_Fear_Election,
                     type = "est", title = "Fear")
angerp1 <- plot_model(reordered_models$DV_Inclusion_Internal_IV_ICI_Reverse_Fac_Med_Angry_Election,
                     type = "est", title = "Anger")
proudp1 <- plot_model(reordered_models$DV_Inclusion_Internal_IV_ICI_Reverse_Fac_Med_Pride_Election,
                      type = "est", title = "Pride")
hopep1 <- plot_model(reordered_models$DV_Inclusion_Internal_IV_ICI_Reverse_Fac_Med_Hope_Election,
                      type = "est", title = "Hope")

neg.int <- fearp1 + angerp1 + plot_annotation(title = "Sense of Belonging, Negative Emotions Individually") &
  theme(plot.title = element_text(hjust = 0.5))
pos.int <- proudp1 + hopep1 + plot_annotation(title = "Sense of Belonging, Positive Emotions Individually") &
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "int_indv_neg.png",  
       plot = neg.int,                             
       width = 10,
       height = 8,                        
       dpi = 300) 
ggsave(filename = "int_indv_pos.png",  
       plot = pos.int,                             
       width = 10,
       height = 8,                        
       dpi = 300) 
# External  -----------

# Rearrange the last two models to be in the middle --- did not index / separate the models because external models were 1 - 15 
reordered_models_ext <- c(
  med_results_ols$outcome_models[c(1,3,4,6)],  # First set (e.g., negative emotions)
  med_results_ols$outcome_models[c(13,15)], # Move last two models here
  med_results_ols$outcome_models[c(7,9,10,12)]   # Remaining models
)

stargazer(reordered_models_ext, type = "latex", dep.var.labels = "External Inclusion",
          covariate.labels = c("Imm Stigma",  "Imm Stigma (-0.5)",
                               "Imm Stigma (0.5)", "Imm Stigam (1)",
                               "Fear", "Anger", "Sad", "Pride", "Hope",
                               "Linked Fate", "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longright arrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Media", 
                               "Deportation Worry",
                               "Constant")
          ,
          out = "fullmodel_external.tex"
          )
### figures 
fearext <- plot_model(reordered_models_ext$DV_Inclusion_External_IV_ICI_Reverse_Fac_Med_Fear_Election,
                      type = "est", title = "Fear")
angerext <- plot_model(reordered_models_ext$DV_Inclusion_External_IV_ICI_Reverse_Fac_Med_Angry_Election,
                      type = "est", title = "Anger")
prideext <- plot_model(reordered_models_ext$DV_Inclusion_External_IV_ICI_Reverse_Fac_Med_Pride_Election,
                       type = "est", title = "Pride")
hopeext <- plot_model(reordered_models_ext$DV_Inclusion_External_IV_ICI_Reverse_Fac_Med_Hope_Election,
                      type = "est", title = "Hope")

neg.ext <- fearext + angerext + plot_annotation(title = "Sense of Belonging, Negative Emotions Individually") &
  theme(plot.title = element_text(hjust = 0.5))

pos.ext <- prideext + hopeext + plot_annotation(title = "Sense of Belonging, Positive Emotions Individually") &
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "ext_indv_pos.png",  
       plot = pos.ext,                             
       width = 10,
       height = 8,                        
       dpi = 300) 
ggsave(filename = "ext_indv_neg.png",  
       plot = neg.ext,                             
       width = 10,
       height = 8,                        
       dpi = 300) 

#### Testing out interactions between stigma x lf + emotions --> social? -----
interaction_function_standard(dvs, ivs, mediators, controls, des = cmps_lat_16, 
                              dat = cmps_lat_16, "int_models")
listofint_ext <- int_models[c(1:15)]
listofint_int <- int_models[c(17:31)]
chosenmodels <- c(listofint_int[c(11:15)], listofint_ext[c(11:15)])

stargazer(chosenmodels[c(1:5)], type = "latex", dep.var.labels = c("Internal Inclusion"),
          covariate.labels = c("Imm Index (-0.5)",
                               "Imm Index (0.5)", "Imm Index (1)","Linked Fate", 
                               "Fear", "Anger", "Pride", "Hope", "Sad",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longright arrow$ D)",
                               "Second Generation +", "Discrimination Exp.",
                               "Nat. Discrimination Percep.", "Imm Index (-0.5) x Linked Fate",
                               "Imm Index (0.5) x Linked Fate",  "Imm Index (1) x Linked Fate",
                               "Constant"),
          out = "int_mods_inclusion_internal.tex")
stargazer(chosenmodels[c(6:10)], type = "latex", dep.var.labels = c("External Inclusion"),
          covariate.labels = c("Imm Index (-0.5)",
                               "Imm Index (0.5)", "Imm Index (1)","Linked Fate", 
                               "Fear", "Anger", "Pride", "Hope", "Sad",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longright arrow$ D)",
                               "Second Generation +", "Discrimination Exp.",
                               "Nat. Discrimination Percep.", "Imm Index (-0.5) x Linked Fate",
                               "Imm Index (0.5) x Linked Fate",  "Imm Index (1) x Linked Fate",
                               "Constant"),
          out = "int_mods_inclusion_external.tex")

## visualizations of interactions 

p1 <- plot_model(chosenmodels[[1]], type = "est", title = "Fear")
p2 <- plot_model(chosenmodels[[2]], type = "est", title = "Anger")
p3 <- plot_model(chosenmodels[[6]], type = "est", title = "Fear")
p4 <- plot_model(chosenmodels[[7]], type = "est",
           title = "Anger")
int_sense_int <- p1 + p2 + plot_annotation(title = "Internal Sense of Belonging") & 
  theme(plot.title = element_text(hjust = 0.5))

ext_sense_int <- p3 + p4 + plot_annotation(title = "Internal Sense of Belonging") & 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "int_sense_int.png",  
       plot = int_sense_int,                             
       width = 10,
       height = 8,                        
       dpi = 300) 
ggsave(filename = "ext_sense_int.png", 
       plot = ext_sense_int,                   
       width = 10,                           
       height = 8,                           
       dpi = 300) 


### testing all emotions at once 
formula_str <- paste("Inclusion_Internal ~ ICI_Reverse_Fac +", 
                     paste(c(controls, mediators), collapse = " + "))
formula_str_ext <- paste("Inclusion_External ~ ICI_Reverse_Fac +", 
                     paste(c(controls, mediators), collapse = " + "))

all_em <- svyglm(formula_str,
                 design = cmps_lat_16)
all_em_nospan <- svyglm(formula_str,
                        design = cmps_lat_16)
all_em_ext <- svyglm(formula_str_ext,
                 design = cmps_lat_16)
all_em_ext_nospan <- svyglm(formula_str_ext,
                     design = cmps_lat_16)
all_em_p <- plot_model(all_em, type = "est", title = "Internal")
all_em_ext_p <- plot_model(all_em_ext, type = "est", title = "External")

all_emotion <- all_em_p + all_em_ext_p + plot_annotation(title = "Predictors of Sense of Belonging") & 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "all_sense.png", 
       plot = all_emotion,                   
       width = 15,                           
       height = 10,                           
       dpi = 300) 

### table ---
soc_models <- c(all_em, all_em_ext)
stargazer(all_em, all_em_ext, type = "latex",
          covariate.labels = c("Imm. Stigma (-.5)", "Imm. Stigma (.5)",
                               "Imm. Stigma (1)", "Age", "Gender", "Education",
                               "Income", "Pol. Interest", "Mexican", "Cuban",
                               "Linked Fate", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Media",
                               "Deportation Worry",
                               "Fear",
                               "Anger", "Pride", "Hope", "Sad",
                               "Constant"),
          dep.var.labels = c("Internal", "External"),
          dep.var.caption = "Dependent Variable: Sense of Belonging"
          ,
          out = "all_social.tex"
          )

#### Interactions ---- 
controls_int <- controls[controls != "Linked_Fate"]
form_int <- paste("Inclusion_Internal ~ ICI_Reverse_Fac*Linked_Fate +", 
      paste(c(controls_int, mediators), collapse = " + "))
form_int_alt <- paste("Inclusion_Internal ~ ICI_Reverse*Linked_Fate +", 
                  paste(c(controls_int, mediators), collapse = " + "))
form_ext <- paste("Inclusion_External ~ ICI_Reverse_Fac*Linked_Fate +", 
                  paste(c(controls_int, mediators), collapse = " + "))
internal_int_all <- svyglm(form_int,
                 design = cmps_lat_16)
internal_int_all_alt <- svyglm(form_int_alt,
                           design = cmps_lat_16)
external_int_all <- svyglm(form_ext,
                           design = cmps_lat_16)

int_p1 <- plot_model(internal_int_all, type = "est", title = "Internal")
int_p2 <- plot_model(external_int_all, type = "est", title = "External")

both_int <- int_p1 + int_p2 + 
  plot_annotation(title = "Interaction Effects Between Stigma Context and Linked Fate") & 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "both_int_all.png",
       plot = both_int,
       width = 10,                           
       height = 8,                           
       dpi = 300) 
### table 
stargazer(internal_int_all, external_int_all, type = "latex", 
          dep.var.caption = "Dependent Variable: Sense of Belonging",
          dep.var.labels = c("Internal", "External"), 
          covariate.labels = c("Imm. Stigma (-0.5)", "Imm. Stigma (0.5)",
                               "Imm. Stigma (1)", "Linked Fate",
                               "Age", "Gender", "Education", "Income",
                               "Pol. Interest", "Mexican", "Cuban", 
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percp.", "Spanish Media",
                               "Deportation Worry","Fear", "Anger",
                               "Pride", "Hope", "Sad", 
                               "Imm. Stigma (-0.5) x Linked Fate",
                               "Imm. Stigma (0.5) x Linked Fate",
                               "Imm. Stigma (1) x Linked Fate",
                               "Constant"
                               )
          , out = "all_int.tex"
          )

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
