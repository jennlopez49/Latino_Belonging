###### 2016 LINKED FATE REGRESSIONS ############### -----
## making ICI collapsed reference as 1 instead of 0 
latinos_2016$ICI_collapsed_fac <- relevel(as.factor(latinos_2016$ICI_collapsed_alt), ref = "1")
basemodel <- lm(Linked_Fate ~ Age + Gender + Education + Income + More_Than_SecondGen + 
                  Mexican + Puerto_Rican + Cuban + Economy + Discrimination, 
                data = latinos_2016)
stargazer(basemodel, type = "text")

pop.vote.model <- lm(Linked_Fate ~ Age + Gender + Education + Income + More_Than_SecondGen + 
                       Mexican + Puerto_Rican + Cuban + Economy + Discrimination + 
                       vote_margin + percent.latino.2016, 
                     data = latinos_2016)
full.model <- lm(Linked_Fate ~ Age + Gender + Education + Income + More_Than_SecondGen + 
                       Mexican + Puerto_Rican + Cuban + Economy + Discrimination + 
                   vote_margin + percent.latino.2016 + ICI_Score_2016, 
                     data = latinos_2016)
full.model.coll <- lm(Linked_Fate ~ Age + Gender + Education + Income + More_Than_SecondGen + 
                   Mexican + Puerto_Rican + Cuban + Economy + Discrimination + 
                     vote_margin + percent.latino.2016 + ICI_collapsed_fac, 
                 data = latinos_2016)

stargazer(basemodel, pop.vote.model, full.model, full.model.coll, type = "text",
          out="initialmodels_new.html")


#### Excluding PR 

latinos_2016_sub <- latinos_2016 %>% filter(Puerto_Rican == 0)

base <- lm(Linked_Fate ~ Age + Gender + Education + Income + More_Than_SecondGen + 
             Mexican + Cuban + Economy + Discrimination, 
           data = latinos_2016_sub)

pop.vote <- lm(Linked_Fate ~ Age + Gender + Education + Income + More_Than_SecondGen + 
                       Mexican + Cuban + Economy + Discrimination + 
                       Battleground*percent.latino.2016, 
                     data = latinos_2016_sub)
full_cont <- lm(Linked_Fate ~ Age + Gender + Education + Income + More_Than_SecondGen + 
                   Mexican + Cuban + Economy + Discrimination + 
                  vote_margin*percent.latino.2016 + ICI_Score_2016, 
                 data = latinos_2016_sub)
full_ind <- lm(Linked_Fate ~ Age + Gender + Education + Income + More_Than_SecondGen + 
                        Mexican + Cuban + Economy + Discrimination + 
                 vote_margin*percent.latino.2016 + ICI_collapsed_fac, 
                      data = latinos_2016_sub)


stargazer(base, pop.vote, full_cont, full_ind, type = "text",
          out="initialmodels_noPR.html")

stargazer(full_ind, type = "text", out = "main.html")

### standardizing 

latinos_2016_clean <- latinos_2016_sub %>% select(Age, Gender, Education, Income, 
                                                         Mexican,
                                                         Cuban, Economy, Discrimination, ICI_Score_2016,
                                                         ICI_collapsed, percent.latino.2016,
                                                         vote_margin, NativeBorn, More_Than_SecondGen,
                                                         Battleground, Linked_Fate, State) %>% na.omit() %>%  mutate(
    age_s = scale(Age),
    educ_s = scale(Education),
    income_s = scale(Income),
    mex_s = scale(Mexican),
    cub_s = scale(Cuban),
    econ_s = scale(Economy),
    disc_s = scale(Discrimination),
    ici_s = scale(ICI_Score_2016),
    ici_ind_s = scale(ICI_collapsed),
    pop_s = scale(percent.latino.2016),
    vote_s = scale(vote_margin),
    gen_s = scale(More_Than_SecondGen),
    lf_s = scale(Linked_Fate),
    gender_s = scale(Gender)
  )

latinos_2016_clean <- left_join(latinos_2016_clean, votemargin_16, by = "State")

latinos_2016_clean <- latinos_2016_clean %>% mutate(
  GOP_vote = (REPUBLICAN/totalvotes)*100,
  GOP_vote_s = scale(GOP_vote),
  DEM_vote = (DEMOCRAT/totalvotes)*100,
  DEM_vote_s = scale(DEM_vote)
)

s_full_ind<- lm(lf_s ~ age_s + gender_s + educ_s + income_s + gen_s + 
                    mex_s + cub_s + econ_s + disc_s + 
                  vote_s*pop_s + ici_ind_s, 
                data = latinos_2016_clean)
s_full_cont<- lm(lf_s ~ age_s + gender_s + educ_s + income_s + gen_s + 
                  mex_s + cub_s + econ_s + disc_s + 
                  vote_s*pop_s + ici_s, 
                data = latinos_2016_clean)
s_full_vote <- lm(lf_s ~ age_s + gender_s + educ_s + income_s + gen_s + 
                    mex_s + cub_s + econ_s + disc_s + 
                    vote_s*pop_s + GOP_vote_s, 
                  data = latinos_2016_clean)
s_full_voted <- lm(lf_s ~ age_s + gender_s + educ_s + income_s + gen_s + 
                    mex_s + cub_s + econ_s + disc_s + 
                    vote_s*pop_s + DEM_vote_s, 
                  data = latinos_2016_clean)
s_full_noint <- lm(lf_s ~ age_s + gender_s + educ_s + income_s + gen_s + 
                     mex_s + cub_s + econ_s + disc_s + 
                     vote_s + pop_s + GOP_vote_s, 
                   data = latinos_2016_clean)
stargazer(s_full_ind, type = "text", out = "scaled.html")
stargazer(s_full_noint, s_full_vote, s_full_voted, type = "text", out = "votes.html")

############ 2020 LF Regressions -----------------------------------------------

base <- lm(Linked_Fate ~ Age + Gender + Education + Income + MoreThanSecondGen + 
             Mexican  + Econ_Hope + Disc_Affect, 
           data = latinos_cmps_2020)
base_incl <- lm(Linked_Fate ~ Age + Gender + Education + Income + MoreThanSecondGen + 
             Mexican  + Econ_Hope + Disc_Affect + inclusivity_varied, 
           data = latinos_cmps_2020)
pop_vote_incl <- lm(Linked_Fate ~ Age + Gender + Education + Income + MoreThanSecondGen + 
                  Mexican  + Econ_Hope + Disc_Affect + vote_margin + percent.latino.2020 +
                    inclusivity_varied, 
                data = latinos_cmps_2020)
pop.vote <- lm(Linked_Fate ~ Age + Gender + Education + Income + MoreThanSecondGen + 
                 Mexican + Econ_Hope + Disc_Affect + 
                 vote_margin + percent.latino.2020, 
               data = latinos_cmps_2020)
full_cont <- lm(Linked_Fate ~ Age + Gender + Education + Income + MoreThanSecondGen + 
                  Mexican + Econ_Hope + Disc_Affect + 
                  vote_margin*percent.latino.2020 + inclusivity_varied, 
                data = latinos_cmps_2020)
stargazer(base, base_incl, pop.vote, pop_vote_incl, full_cont, type = "text")

