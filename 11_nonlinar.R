#### NON-LINEAR MODELS ---- EMOTION ---------------------
 # List of IVs (X)
dvs_non <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
              "Mexican", "Cuban",
              "Linked_Fate", "Party",
              "More_Than_SecondGen", "Discrimination_Scale", 
              "Discrimination_National_Perc") 
form_em <- paste("I(ICI_Reverse^2)", controls, collapse = "+")

# lapply
models_non <- lapply(dvs_non, function(dv) {
  # Construct the formula dynamically
  formula_str <- paste(dv, "~ ICI_Reverse +", paste(controls, collapse = " + "))
  formula_final <- as.formula(formula_str)
  
  # Run svyglm
  svyglm(formula_final, design = cmps_lat_16)
})

# Naming the list of models for easier access
names(models_non) <- dvs_non

# Example: Access the summary of a specific model
summary(models_non[["Fear_Election"]])

#### NON-LINEAR MODELS ---- INCLUSION ----------


### squared -- parabolic 

form_sq_int <- paste("Inclusion_Internal ~ I(ICI_Reverse^2) +", 
                     paste(c(controls, mediators), collapse = " + "))
form_sq_ext <- paste("Inclusion_External ~ I(ICI_Reverse^2) +", 
                         paste(c(controls, mediators), collapse = " + "))

sq_int <- svyglm(form_sq_int,
                 design = cmps_lat_16)
sq_ext <- svyglm(form_sq_ext,
                 design = cmps_lat_16)
summary(sq_int)
summary(sq_ext)

### cubic --- 

form_cu_int <- paste("Inclusion_Internal ~ I(ICI_Reverse^3) +", 
                     paste(c(controls, mediators), collapse = " + "))
form_cu_ext <- paste("Inclusion_External ~ I(ICI_Reverse^3) +", 
                     paste(c(controls, mediators), collapse = " + "))

cu_int <- svyglm(form_cu_int,
                 design = cmps_lat_16)
cu_ext <- svyglm(form_cu_ext,
                 design = cmps_lat_16)
summary(cu_int)
summary(cu_ext)

### 
library(segmented)

lm_base <- svyglm(formula_str, 
                  design = cmps_lat_16)

seg_model <- segmented(lm_base, seg.Z = ~ICI_Reverse, psi = 0)

summary(seg_model)

high_stigma <- subset(cmps_lat_16, cmps_lat_16$variables$ICI_Reverse >= 0)
low_stigma <- subset(cmps_lat_16, cmps_lat_16$variables$ICI_Reverse < 0)

############## Mediation with Discrimination ------------------------------
disc_mediator <- svyglm(Latino_Disc ~ ICI_Reverse_Fac + Age + Gender + 
                        Education + Income + Discrimination_Scale + Linked_Fate +
                      Mexican + Cuban + More_Than_SecondGen + Skin_Tone + 
                        Spanish_Interview, 
                      design = high_stigma, family = "gaussian")
# fear_mediator <- svyglm(Fear_Election ~ I(ICI_Reverse^2) + age_sqd + Gender + 
#                           Education + Income + Linked_Fate + 
#                           Discrimination_Scale + Latino_Disc +
#                         Mexican + Cuban + More_Than_SecondGen, 
#                         design = cmps_lat_16, family = "gaussian")

# Fit the outcome model (Model 2: Y ~ X + M + controls)
model_outcome.fear <- svyglm(Fear_Election ~ ICI_Reverse_Fac + 
                               Age + Gender + Education + Income + Linked_Fate +
                               Discrimination_Scale + Latino_Disc
                             + Mexican + Cuban + More_Than_SecondGen + Skin_Tone + 
                               Spanish_Interview, 
                             design = high_stigma, family = "gaussian")

# Run the mediation analysis
# mediation_result.fear <- mediate(fear_mediator, model_outcome.fear, treat = "I(ICI_Reverse^2)", mediator = "Fear_Election")
mediation_result.disc <- mediate(disc_mediator, model_outcome.fear, 
                               treat = "ICI_Reverse_Fac", mediator = "Latino_Disc")
summary(mediation_result.disc)


disc_mediator_low <- svyglm(Latino_Disc ~ ICI_Reverse_Fac + Age + Gender + 
                          Education + Income + Discrimination_Scale + Linked_Fate +
                          Mexican + Cuban + More_Than_SecondGen + Skin_Tone + 
                          Spanish_Interview, 
                        design = low_stigma, family = "gaussian")
# fear_mediator <- svyglm(Fear_Election ~ I(ICI_Reverse^2) + age_sqd + Gender + 
#                           Education + Income + Linked_Fate + 
#                           Discrimination_Scale + Latino_Disc +
#                         Mexican + Cuban + More_Than_SecondGen, 
#                         design = cmps_lat_16, family = "gaussian")

# Fit the outcome model (Model 2: Y ~ X + M + controls)
model_outcome.fear_l <- svyglm(Fear_Election ~ ICI_Reverse_Fac + 
                               Age + Gender + Education + Income + Linked_Fate +
                               Discrimination_Scale + Latino_Disc
                             + Mexican + Cuban + More_Than_SecondGen + Skin_Tone + 
                               Spanish_Interview, 
                             design = low_stigma, family = "gaussian")

# Run the mediation analysis
# mediation_result.fear <- mediate(fear_mediator, model_outcome.fear, treat = "I(ICI_Reverse^2)", mediator = "Fear_Election")
mediation_result.disc_l <- mediate(disc_mediator_low, model_outcome.fear_l, 
                                 treat = "ICI_Reverse_Fac", mediator = "Latino_Disc")
summary(mediation_result.disc_l)

