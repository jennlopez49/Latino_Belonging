### Setting Up Survey Design -------
cmps_des <- svydesign(id = ~ 1, weights = ~race_weight, data = full_cmps_2020)


### Filtering to Latinos ------
cmps_lat <- subset(cmps_des, subset = cmps_des$variables$Hispanic == 1)

### Full Sample Models -----

full_sample <- svyglm(citizenship_exp ~ Party_5pt + Gender + Education + Income + 
                        inclusivity*Mexican + Linked_Fate + Psych_Distance + border_state,
                      design = cmps_lat, 
                      family = "gaussian")
