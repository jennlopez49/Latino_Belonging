library(pwr)

emotion_effect_analysis <- function(data, emotion_vars, predictors, treatment_var = "TreatmentCondition", control_label = "Control_Policy",
                                    anti_label = "Treatment-Hypothetical_Anti", pro_label = "Treatment-Hypothetical_Pro",
                                    sig.level = 0.05, target_power = 0.80) {
  
  results <- data.frame(
    Emotion = character(),
    Mean_Control = numeric(),
    Mean_Anti = numeric(),
    Mean_Pro = numeric(),
    D_Anti = numeric(),
    D_Pro = numeric(),
    R2 = numeric(),
    f2 = numeric(),
    Power = numeric(),
    Needed_N_for_80 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (emotion in emotion_vars) {
    # Subset data by condition
    control <- subset(data, data[[treatment_var]] == control_label)
    anti <- subset(data, data[[treatment_var]] == anti_label)
    pro <- subset(data, data[[treatment_var]] == pro_label)
    
    # Means and SDs
    m_ctrl <- mean(control[[emotion]], na.rm = TRUE)
    m_anti <- mean(anti[[emotion]], na.rm = TRUE)
    m_pro  <- mean(pro[[emotion]], na.rm = TRUE)
    
    sd_ctrl <- sd(control[[emotion]], na.rm = TRUE)
    sd_anti <- sd(anti[[emotion]], na.rm = TRUE)
    sd_pro  <- sd(pro[[emotion]], na.rm = TRUE)
    
    # Pooled SDs
    sd_pooled_anti <- sqrt((sd_ctrl^2 + sd_anti^2) / 2)
    sd_pooled_pro  <- sqrt((sd_ctrl^2 + sd_pro^2) / 2)
    
    # Cohen's d
    d_anti <- (m_anti - m_ctrl) / sd_pooled_anti
    d_pro  <- (m_pro - m_ctrl) / sd_pooled_pro
    
    # Full regression model
    formula <- as.formula(paste(emotion, "~", paste(predictors, collapse = " + ")))
    model <- lm(formula, data = data)
    
    r2 <- summary(model)$r.squared
    f2 <- ifelse(r2 < 1, r2 / (1 - r2), NA)  # Avoid divide by zero
    
    # Power
    u <- length(coef(model)) - 1
    v <- df.residual(model)
    n <- u + v + 1
    
    power_val <- tryCatch({
      pwr.f2.test(u = u, v = v, f2 = f2, sig.level = sig.level)$power
    }, error = function(e) NA)
    
    needed_n <- tryCatch({
      needed_v <- pwr.f2.test(u = u, f2 = f2, sig.level = sig.level, power = target_power)$v
      u + needed_v + 1
    }, error = function(e) NA)
    
    # Store results
    results <- rbind(results, data.frame(
      Emotion = emotion,
      Mean_Control = round(m_ctrl, 2),
      Mean_Anti = round(m_anti, 2),
      Mean_Pro = round(m_pro, 2),
      D_Anti = round(d_anti, 3),
      D_Pro = round(d_pro, 3),
      R2 = round(r2, 3),
      f2 = round(f2, 3),
      Power = round(power_val, 3),
      Needed_N_for_80 = round(needed_n, 0)
    ))
  }
  
  return(results)
}


# Your emotion outcome variables
emotions <- c("Anger", "Fear", "Shame", "Pride", "Relief", "Joy")

# Your predictor variables (from your model)
predictors <- c("TreatmentCondition", "Party", "SkinTone", "Sex_Gender", "Age", "Education")

# Run the analysis
effect_table <- emotion_effect_analysis(data = both_pilots,
                                        emotion_vars = emotions,
                                        predictors = predictors)

print(effect_table)


## Stigma Vars -----> 
stigma_vars <- c("ImmsMajSourceCrime", "PoliticiansTalkNegImm", "PoliciesImmUnfair", 
                 "LatinosMajSourceCrime", "PoliticiansTalkNegLatinos",
                 "PoliciesLatinosUnfair", "Stigma_Imm_Index", "Stigma_Latinos_Index",
                 "GroupDiscImms_Perc", "GroupDiscLatinos_Perc")

effect_table_stigma <- emotion_effect_analysis(data = both_pilots,
                                        emotion_vars = stigma_vars,
                                        predictors = predictors)
print(effect_table_stigma)