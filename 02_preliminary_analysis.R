### Setting Up Survey Design -------
cmps_des <- svydesign(id = ~ 1, weights = ~race_weight, data = full_cmps_2020)


### Filtering to Latinos ------
cmps_lat <- subset(cmps_des, subset = cmps_des$variables$Hispanic == 1)

### Full Sample Models -----

full_sample_belong <- svyglm(Belong_US ~ Party_5pt + Gender + Education + Income + 
                        inclusivity + Linked_Fate + Accult + Fear_Imm,
                      design = cmps_lat, 
                      family = "gaussian")
full_sample_fullcit <- svyglm(Full_Cit ~ Party_5pt + Gender + Education + Income + 
                               inclusivity + Linked_Fate + Accult + Fear_Imm,
                             design = cmps_lat, 
                             family = "gaussian")

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

full_sample_expdisc <- svyglm(Belong_US ~ Party_5pt + Gender + Education + Income + 
                               inclusivity + Linked_Fate + Accult + Experienced_Disc,
                             design = cmps_lat, 
                             family = "gaussian")
full_sample_perdisc <- svyglm(Full_Cit ~ Party_5pt + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Perceived_Index_Groups,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_perdisc_belong <- svyglm(Belong_US ~ Party_5pt + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Perceived_Index_Groups,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_latdisc <- svyglm(Belong_US ~ Party_5pt + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Latino_Disc,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_immdisc <- svyglm(Belong_US ~ Party_5pt + Gender + Education + Income + 
                                inclusivity + Linked_Fate + Accult + Imm_Disc,
                              design = cmps_lat, 
                              family = "gaussian")
