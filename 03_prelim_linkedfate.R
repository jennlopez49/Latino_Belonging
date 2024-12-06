###### 2016 LINKED FATE REGRESSIONS ###############
## making ICI collapsed reference as 1 instead of 0 
latinos_2016$ICI_collapsed_fac <- relevel(as.factor(latinos_2016$ICI_collapsed), ref = "0")
basemodel <- lm(Linked_Fate ~ Age + Gender + Education + Income + Generation + 
                  Mexican + Puerto_Rican + Cuban + Economy + Discrimination, 
                data = latinos_2016)
stargazer(basemodel, type = "text")

pop.vote.model <- lm(Linked_Fate ~ Age + Gender + Education + Income + Generation + 
                       Mexican + Puerto_Rican + Cuban + Economy + Discrimination + 
                       vote_margin.x + percent.latino.2016.x, 
                     data = latinos_2016)
full.model <- lm(Linked_Fate ~ Age + Gender + Education + Income + Generation + 
                       Mexican + Puerto_Rican + Cuban + Economy + Discrimination + 
                       vote_margin.x + percent.latino.2016.x + ICI_Score_2016*Discrimination, 
                     data = latinos_2016)
full.model.coll <- lm(Linked_Fate ~ Age + Gender + Education + Income + Generation + 
                   Mexican + Puerto_Rican + Cuban + Economy + Discrimination + 
                   vote_margin.x + percent.latino.2016.x + ICI_collapsed_fac*Discrimination, 
                 data = latinos_2016)

stargazer(basemodel, pop.vote.model, full.model, full.model.coll, type = "text",
          out="initialmodels.html")
