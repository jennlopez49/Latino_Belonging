### Setting Up Survey Design -------
#full_cmps_lat <- full_cmps_2020 %>% filter(Grandparents_Born != 3 | Parents_Born != 3)
cmps_des <- svydesign(id = ~ 1, weights = ~race_weight, data = full_cmps_2020)


### Filtering to Latinos ------
cmps_lat <- subset(cmps_des, subset = cmps_des$variables$Hispanic == 1)


### Full Sample Models -----

full_sample_belong <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                        inclusivity + Linked_Fate + MoreThanSecondGen + foreboding,
                      design = cmps_lat, 
                      family = "gaussian")
full_sample_belong_int <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                               inclusivity + Linked_Fate + MoreThanSecondGen + inclusivity*foreboding,
                             design = cmps_lat, 
                             family = "gaussian")
full_sample_belong_sp <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                               inclusivity + Linked_Fate + Spanish + foreboding,
                             design = cmps_lat, 
                             family = "gaussian")
full_sample_fullcit <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                               inclusivity + Linked_Fate + MoreThanSecondGen +  foreboding,
                             design = cmps_lat, 
                             family = "gaussian")
full_sample_fullcit_int <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + MoreThanSecondGen +  inclusivity*foreboding,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_fullcit_sp <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Spanish +  foreboding,
                              design = cmps_lat, 
                              family = "gaussian")
### Tables ---- 
stargazer(full_sample_belong, full_sample_belong_int, 
          full_sample_fullcit, full_sample_fullcit_int, type = "latex", 
          dep.var.labels = c("Belong in the US", "Same Rights and Protections as Others"),
          covariate.labels = c("Party", "Age", "Gender", "Education",
                               "Income", "Inclusivity", "Linked Fate", "Generation",
                               "Foreboding", "Interaction",
                               "Constant")
          )
#### Alternate Measures ------
full_sample_value <- svyglm(Value_US ~ Party_5pt + Gender + Education + Income + 
                        inclusivity + Linked_Fate + Accult + Fear_Imm,
                      design = cmps_lat, 
                      family = "gaussian")
full_sample_accept <- svyglm(Accepted_US ~ Party_5pt + Gender + Education + Income + 
                              inclusivity + Linked_Fate + Accult + Fear_Imm,
                            design = cmps_lat, 
                            family = "gaussian")
full_sample_index <- svyglm(citizenship_exp ~ Party_5pt + Gender + Education + Income + 
                               inclusivity + Linked_Fate + Accult + Fear_Imm,
                             design = cmps_lat, 
                             family = "gaussian")

### Alt Models --------

full_sample_expdisc <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Experienced_Disc,
                             design = cmps_lat, 
                             family = "gaussian")
full_sample_perdisc <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Perceived_Index_Groups,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_perdisc_belong <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                       inclusivity + Linked_Fate + Accult + Perceived_Index_Groups,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_latdisc <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Latino_Disc,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_latdisc_fcit <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Latino_Disc,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_immdisc <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Imm_Disc,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_immdisc_cit <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Imm_Disc,
                              design = cmps_lat, 
                              family = "gaussian")

##### Tables ----- 
stargazer(full_sample_perdisc, full_sample_latdisc_fcit, full_sample_immdisc_cit, type = "latex", 
          dep.var.labels = c("Same Rights and Protections"),
          covariate.labels = c("Party", "Age", "Gender", "Education",
                               "Income", "Inclusivity", "Linked Fate", "Acculturation",
                               "Discrimination against Immigrants and Latinos",
                               "Discrimination against Latinos",
                               "Discrimination against Immigrants",
                               "Constant")
)

stargazer(full_sample_perdisc_belong, full_sample_latdisc, full_sample_immdisc, type = "latex", 
          dep.var.labels = c("Belong in the US"),
          covariate.labels = c("Party", "Age", "Gender", "Education",
                               "Income", "Inclusivity", "Linked Fate", "Acculturation",
                               "Discrimination against Immigrants and Latinos",
                               "Discrimination against Latinos",
                               "Discrimination against Immigrants",
                               "Constant")
)

###### By Exclusive/Inclusive -------

incl <- subset(cmps_des, subset = cmps_des$variables$inclusivity == 1)
excl <- subset(cmps_des, subset = cmps_des$variables$inclusivity == 0)

### Models Belonging -----------
belong_incl <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income
                                + Linked_Fate + Accult + Fear_Imm,
                             design = incl, 
                             family = "gaussian")
belong_sp_incl <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                   Linked_Fate + Spanish + Fear_Imm,
                                design = incl, 
                                family = "gaussian")
fullcit_incl <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                Linked_Fate + Accult +  Fear_Imm,
                              design = incl, 
                              family = "gaussian")
fullcit_sp_incl <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                  Linked_Fate + Spanish +  Fear_Imm,
                                 design = incl, 
                                 family = "gaussian")


belong_excl <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                       Linked_Fate + Accult + Fear_Imm,
                      design = excl, 
                      family = "gaussian")
belong_sp_excl <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                      Linked_Fate + Spanish + Fear_Imm,
                         design = excl, 
                         family = "gaussian")
fullcit_excl <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                         Linked_Fate + Accult +  Fear_Imm,
                       design = excl, 
                       family = "gaussian")
fullcit_sp_excl <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                      Linked_Fate + Spanish +  Fear_Imm,
                          design = excl, 
                          family = "gaussian")

#### Tables -------
stargazer(belong_incl, belong_sp_incl, belong_excl, belong_sp_excl, type = "latex", 
          dep.var.labels = c("Belong in the US"),
          covariate.labels = c("Party", "Age", "Gender", "Education",
                               "Income","Linked Fate", "Acculturation",
                               "Spanish",
                               "Fear - Immigration",
                               "Constant"),
          column.separate = c(2,2), column.labels = c("Welcoming", "Hostile")
)

stargazer(fullcit_incl, fullcit_sp_incl, fullcit_excl, fullcit_sp_excl, type = "latex", 
          dep.var.labels = c("Same Rights and Protections"),
          covariate.labels = c("Party", "Age", "Gender", "Education",
                               "Income","Linked Fate", "Acculturation",
                               "Spanish",
                               "Fear - Immigration",
                               "Constant"),
          column.separate = c(2,2), column.labels = c("Welcoming", "Hostile")
)
