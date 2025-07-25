#### 2020 Runs Set up ------ 
## DO AS FACTOR 
dvs_20 <- c("External_Belonging", "Internal_Belonging")  # List of DVs (Y)
# ivs <- list("ICI_Reverse", "Imm_Con_Index", "ICI_Reverse_Fac")  # List of IVs (X) ### OLD INDICATOR
ivs_20 <- list("conc_lat_index_20","latino_conc_20")
mediators_20 <- list("Depressed", "Anxiety", "Dread", "Anxiety_Stress_ImmPolicy")  # List of Mediators (M)
controls_20 <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
              "Mexican", "Cuban",
              "Linked_Fate", "PartyID_3",
              "More_Than_SecondGen", "Discriminated_US",
              "Latinos_Disc", "Spanish_Use", 
              "Worried_DetainedDeported_SomeoneElse"
              # , 
              # "Worried_DetainedDeported"
              # , "Skintone"
              )

###### Runs ========== 
mediation_function_standard(dvs_20, ivs_20, mediators_20, controls_20, 
                            cmps_lat_20_adj, cmps_lat_20_adj, out ="med_ols_20")

stargazer(med_ols_20$mediator_models, type = "latex",
          covariate.labels = c("Concrete Imm. Index","Concrete Imm. Stigma","Linked Fate",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Use", 
                               "Deportation Worry", 
                               "Constant"),
          dep.var.labels = c("Depression", "Anxiety", "Dread", "Anxiety due to Imm."), 
          dep.var.caption = "Dependent variable: Negative Emotions in 2020"
          ,
          out = "neg.em.2020.tex"
)
ext.mods <- med_ols_20$outcome_models[1:8]
stargazer(ext.mods, type = "latex",
          covariate.labels = c("Concrete Imm. Index", "Concrete Imm. Stigma",
                               "Depression", "Anxiety", "Dread", "Anxiety due to Imm.",
                               "Linked Fate",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Use", 
                               "Deportation Worry", 
                               "Constant"),
          dep.var.labels = c("External Belonging"), 
          dep.var.caption = "Dependent variable: External Belonging in 2020"
          ,
          out = "ext.belonging.2020.tex"
)
int.mods <- med_ols_20$outcome_models[9:16]
stargazer(int.mods, type = "latex",
          covariate.labels = c("Concrete Imm. Index", "Concrete Imm. Stigma",
                               "Depression", "Anxiety", "Dread", "Anxiety due to Imm.",
                               "Linked Fate",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Use", 
                               "Deportation Worry", 
                               "Constant"),
          dep.var.labels = c("Internal Belonging"), 
          dep.var.caption = "Dependent variable: Internal Belonging in 2020"
          ,
          out = "int.belonging.2020.tex"
)

## Interaction ----- 
weight_var_20 <- "weight_adj"
interaction_function_standard(dvs_20, ivs_20, mediators_20, controls_20, 
                              weight_var_20, des = cmps_lat_20_adj, 
                              dat = cmps_lat_20_adj$variables, "int_models_20")
inter.20.int <- int_models_20[10:17]

stargazer(inter.20.int, type = "latex",
          covariate.labels = c("Concrete Imm. Index", "Concrete Imm. Stigma",
                               "Depression", "Anxiety", "Dread", "Anxiety due to Imm.",
                               "Linked Fate",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Use", 
                               "Deportation Worry", 
                               "Concrete Imm. Index x Linked Fate",
                               "Concrete Imm. Stigma x Linked Fate",
                               "Constant"),
          dep.var.labels = c("Internal Belonging"), 
          dep.var.caption = "Dependent variable: Internal Belonging in 2020"
          ,
          out = "inter.intbelonging.2020.tex"
)

inter.20.ext <- int_models_20[1:8]

stargazer(inter.20.ext, type = "latex",
          covariate.labels = c("Concrete Imm. Index", "Concrete Imm. Stigma",
                               "Depression", "Anxiety", "Dread", "Anxiety due to Imm.",
                               "Linked Fate",
                               "Age", "Gender",
                               "Education", "Income", "Political Interest",
                               "Mexican", "Cuban", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Use", 
                               "Deportation Worry", 
                               "Concrete Imm. Index x Linked Fate",
                               "Concrete Imm. Stigma x Linked Fate",
                               "Constant"),
          dep.var.labels = c("External Belonging"), 
          dep.var.caption = "Dependent variable: Internal Belonging in 2020"
          ,
          out = "inter.extbelonging.2020.tex"
)

### Doing all emotions at once ---- 

formula_int20 <- paste("Internal_Belonging ~ latino_conc_20 +", 
                     paste(c(controls_20, mediators_20), collapse = " + "))
formula_ext20 <- paste("External_Belonging ~ latino_conc_20 +", 
                         paste(c(controls_20, mediators_20), collapse = " + "))
formula_int20_alt <- paste("Internal_Belonging ~ conc_lat_index_20 +", 
                         paste(c(controls_20, mediators_20), collapse = " + "))
formula_ext20_alt <- paste("External_Belonging ~ conc_lat_index_20 +", 
                             paste(c(controls_20, mediators_20), collapse = " + "))
nonlin20_int <- paste("Internal_Belonging ~ factor(conc_lat_index_20) +", 
                           paste(c(controls_20, mediators_20), collapse = " + "))
nonlin20_ext <- paste("External_Belonging ~ factor(conc_lat_index_20)+", 
                           paste(c(controls_20, mediators_20), collapse = " + "))

all_int20_1 <- svyglm(formula_int20,
                  design = cmps_lat_20_adj)
all_int20_2 <- svyglm(formula_int20_alt,
                  design = cmps_lat_20_adj)
all_ext20_1 <- svyglm(formula_ext20,
                      design = cmps_lat_20_adj)
all_ext20_2 <- svyglm(formula_ext20_alt,
                      design = cmps_lat_20_adj)

nonlin20_int1 <- svyglm(nonlin20_int,
                      design = cmps_lat_20_adj)
nonlin20_ext1 <- svyglm(nonlin20_ext,
                        design = cmps_lat_20_adj)


stargazer(all_int20_1, all_int20_2, all_ext20_1, all_ext20_2, type = "latex",
          covariate.labels = c("Concrete Imm. Stigma", "Concrete Imm. Index", 
                               "Age", "Gender", "Education",
                               "Income", "Pol. Interest", "Mexican", "Cuban",
                               "Linked Fate", "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Use",
                               "Deportation Worry",
                               "Depression",
                               "Anxiety", "Dread", "Anxiety due to Imm.",
                               "Constant"),
          dep.var.labels = c("Internal", "External"),
          dep.var.caption = "Dependent Variable: Sense of Belonging"
          ,
          out = "all_social_20.tex"
)

#### All together for interaction ----- 
controls_int_20 <- controls_20[controls_20 != "Linked_Fate"]
fint20_1 <- paste("Internal_Belonging ~ latino_conc_20*Linked_Fate +", 
                  paste(c(controls_int_20, mediators_20), collapse = " + "))
fint20_2 <- paste("Internal_Belonging ~ conc_lat_index_20*Linked_Fate +", 
                      paste(c(controls_int_20, mediators_20), collapse = " + "))
fext20_1 <- paste("External_Belonging ~ latino_conc_20*Linked_Fate +", 
                  paste(c(controls_int_20, mediators_20), collapse = " + "))
fext20_2 <- paste("External_Belonging ~ conc_lat_index_20*Linked_Fate +", 
                      paste(c(controls_int_20, mediators_20), collapse = " + "))
intint20_1 <- svyglm(fint20_1,
                  design = cmps_lat_20_adj)
intint20_2 <- svyglm(fint20_2,
                  design = cmps_lat_20_adj)
extint20_1 <- svyglm(fext20_1,
                  design = cmps_lat_20_adj)
extint20_2 <- svyglm(fext20_2,
                  design = cmps_lat_20_adj)

stargazer(intint20_1, intint20_2, extint20_1, extint20_2,type = "latex",
          covariate.labels = c("Concrete Imm. Stigma", "Concrete Imm. Index", 
                               "Linked Fate",
                               "Age", "Gender", "Education",
                               "Income", "Pol. Interest", "Mexican", "Cuban",
                               "Party (R $\\longrightarrow$ D)",
                               "Generation", "Discrimination Exp.",
                               "Group Discrimination Percep.", "Spanish Use",
                               "Deportation Worry",
                               "Depression",
                               "Anxiety", "Dread", "Anxiety due to Imm.",
                               "Concrete Imm. Stigma x Linked Fate", 
                               "Concrete Imm. Index x Linked Fate", 
                               "Constant"),
          dep.var.labels = c("Internal", "External"),
          dep.var.caption = "Dependent Variable: Sense of Belonging"
          ,
          out = "all_int_20.tex"
)
