### prelim emotions and sense of belonging ### 
##
cmps_des_16 <- svydesign(id = ~ 1, weights = ~race_weight, data = cmps2016)


### Filtering to Latinos ------
cmps_lat_16 <- subset(cmps_des, subset = cmps_des$variables$Hispanic == 1)
##

full_sample_belong <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                               inclusivity + Linked_Fate + MoreThanSecondGen + foreboding,
                             design = cmps_lat, 
                             family = "gaussian")
full_sample_belong_int <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                   inclusivity + Linked_Fate + MoreThanSecondGen + inclusivity*foreboding,
                                 design = cmps_lat, 
                                 family = "gaussian")
# #full_sample_belong_sp <- svyglm(Belong_US ~ Party_5pt + age_sqd + Gender + Education + Income + 
#                                inclusivity + Linked_Fate + Spanish + foreboding,
#                              design = cmps_lat, 
#                              family = "gaussian")
full_sample_fullcit <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                inclusivity + Linked_Fate + MoreThanSecondGen +  foreboding,
                              design = cmps_lat, 
                              family = "gaussian")
full_sample_fullcit_int <- svyglm(Full_Cit ~ Party_5pt + age_sqd + Gender + Education + Income + 
                                    inclusivity + Linked_Fate + MoreThanSecondGen +  inclusivity*foreboding,
                                  design = cmps_lat, 
                                  family = "gaussian")