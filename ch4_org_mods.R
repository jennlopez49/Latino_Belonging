### org survey 

################### CHAPTER 4 ANALYSES #########################################
survey_controls_part <- c("latino_conc_24","Age", "Sex", "Education", "Income",
                         "PartyID_5pt", "PolKnow_index",
                         "Acculturation", "Span_Acc","GroupDiscImms_clean",
                         "IntEff", "ExtEff")
emotions <- c("Emotions_Anger", "Emotions_Fear", "Emotions_Shame",
              "Emotions_Relief", "Emotions_Pride", "Emotions_Joy")
emotions_all <- c("Emotions_Anger + Emotions_Fear + Emotions_Shame +
               Emotions_Relief + Emotions_Pride + Emotions_Joy")
survey_emotions   <- c("Emotions_Anger", "Emotions_Fear", "Emotions_Shame",
                       "Emotions_Relief", "Emotions_Pride", "Emotions_Joy")

survey_stigma_full     <- c("StigmaImm_mean", "StigmaLatino_mean")
survey_stigma     <- c("StigmaImm_mean")
part_dvs        <- c("ParticipationAlt1_donate", "ParticipationAlt1_volunteer",
                     "ParticipationAlt1_protest")

df_pass <- df_clean %>% filter(ManipCheck1_result == 1)
mediation_function_survey(
  dvs             = part_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls_part,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "ch4_results"
)

mediation_function_survey(
  dvs             = part_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = emotions_all,
  controls        = survey_controls_part,
  dat             = df_clean %>% filter(!is.na(Treatment)),
  out             = "ch4_results_all"
)


mediation_function_survey(
  dvs             = part_dvs,
  ivs             = "Treatment",
  stigma_measures = survey_stigma,
  emotions        = survey_emotions,
  controls        = survey_controls_part,
  dat             = df_pass %>% filter(!is.na(Treatment)),
  out             = "ch4_results_pass"
)

### Mediation models 
# For each emotion × each participation outcome
# Treatment → Emotion → Participation
run_mediation_survey <- function(outcome, mediator, treatment = "Treatment",
                                 controls, data, sims = 5000) {
  
  f_m <- as.formula(
    paste(mediator, "~", treatment, "+", paste(controls, collapse = " + "))
  )
  
  f_y <- as.formula(
    paste(outcome, "~", treatment, "+", mediator, "+", paste(controls, collapse = " + "))
  )
  
  model_m <- lm(f_m, data = data)
  model_y <- lm(f_y, data = data)
  
  mediate(
    model.m = model_m,
    model.y = model_y,
    treat = treatment,
    mediator = mediator,
    sims = sims
  )
}

# Define your vectors
survey_emotions <- c("Emotions_Anger", "Emotions_Fear", "Emotions_Shame",
                     "Emotions_Relief", "Emotions_Pride", "Emotions_Joy")

part_dvs <- c("ParticipationAlt1_donate", 
              "ParticipationAlt1_volunteer", 
              "ParticipationAlt1_protest")

# Loop
med_results_survey <- list()

for (y in part_dvs) {
  for (m in survey_emotions) {
    key <- paste(y, m, sep = "__")
    cat("Running:", key, "\n")
    
    med_results_survey[[key]] <- tryCatch(
      run_mediation_survey(
        outcome   = y,
        mediator  = m,
        controls  = survey_controls_part,
        data      = df_clean %>% filter(!is.na(Treatment)),
        sims      = 500  # start low, bump to 5000 for final run
      ),
      error = function(e) {
        message("Failed: ", key, " — ", e$message)
        NULL
      }
    )
  }
}

extract_mediation_survey <- function(results_list) {
  do.call(rbind, lapply(names(results_list), function(key) {
    
    # Skip failed models
    if (is.null(results_list[[key]])) return(NULL)
    
    s <- summary(results_list[[key]])
    parts <- strsplit(key, "__")[[1]]
    
    data.frame(
      outcome  = parts[1],
      mediator = parts[2],
      ACME     = s$d.avg,
      ACME_lo  = s$d.avg.ci[1],
      ACME_hi  = s$d.avg.ci[2],
      ACME_p   = s$d.avg.p,
      ADE      = s$z.avg,
      ADE_p    = s$z.avg.p,
      Total    = s$tau.coef,
      Total_p  = s$tau.p,
      PropMed  = s$n.avg,
      PropMed_p = s$n.avg.p,
      N        = s$nobs,
      stringsAsFactors = FALSE
    )
  }))
}

survey_results_table <- extract_mediation_survey(med_results_survey)

# Significant ACMEs only
survey_results_table[survey_results_table$ACME_p < 0.05, ]

# By outcome
print(survey_results_table[survey_results_table$outcome == "ParticipationAlt1_protest", ], digits = 3)
print(survey_results_table[survey_results_table$outcome == "ParticipationAlt1_donate", ], digits = 3)
print(survey_results_table[survey_results_table$outcome == "ParticipationAlt1_volunteer", ], digits = 3)

med_results_anti <- list()
med_results_pro <- list()

df_anti <- df_clean %>% 
  filter(Treatment %in% c("Control", "Anti")) %>%
  mutate(Treatment_bin = as.integer(Treatment == "Anti"))

df_pro <- df_clean %>% 
  filter(Treatment %in% c("Control", "Pro")) %>%
  mutate(Treatment_bin = as.integer(Treatment == "Pro"))

for (y in part_dvs) {
  for (m in survey_emotions) {
    key <- paste(y, m, sep = "__")
    cat("Running:", key, "\n")
    
    med_results_anti[[key]] <- tryCatch(
      run_mediation_survey(
        outcome  = y, mediator = m,
        treatment = "Treatment_bin",
        controls  = survey_controls_part,
        data      = df_anti, sims = 5000
      ), error = function(e) { message("Anti failed: ", key); NULL }
    )
    
    med_results_pro[[key]] <- tryCatch(
      run_mediation_survey(
        outcome  = y, mediator = m,
        treatment = "Treatment_bin",
        controls  = survey_controls_part,
        data      = df_pro, sims = 5000
      ), error = function(e) { message("Pro failed: ", key); NULL }
    )
  }
}

anti_table <- extract_mediation_survey(med_results_anti)
pro_table  <- extract_mediation_survey(med_results_pro)

# Significant ACMEs
anti_table[anti_table$ACME_p < 0.05, ]
pro_table[pro_table$ACME_p < 0.05, ]


### making table for mediation runs --- 
make_mediation_latex <- function(dv, anti_table, pro_table, 
                                 dv_label = NULL,
                                 caption = NULL) {
  
  if(is.null(dv_label)) dv_label <- dv
  if(is.null(caption)) caption <- paste("Causal Mediation Analysis:", dv_label)
  
  stars <- function(p) {
    ifelse(p < 0.001, "^{***}",
           ifelse(p < 0.01,  "^{**}",
                  ifelse(p < 0.05,  "^{*}",
                         ifelse(p < 0.1,   "^{\\dagger}", ""))))
  }
  
  fmt <- function(x, p, digits = 3) {
    paste0(formatC(x, digits = digits, format = "f"), stars(p))
  }
  
  fmt_ci <- function(lo, hi, digits = 3) {
    paste0("[", formatC(lo, digits = digits, format = "f"),
           ", ", formatC(hi, digits = digits, format = "f"), "]")
  }
  
  emotion_labels <- c(
    "Emotions_Anger"   = "Anger",
    "Emotions_Fear"    = "Fear",
    "Emotions_Shame"   = "Shame",
    "Emotions_Relief"  = "Relief",
    "Emotions_Pride"   = "Pride",
    "Emotions_Joy"     = "Joy"
  )
  
  anti <- anti_table[anti_table$outcome == dv, ]
  pro  <- pro_table[pro_table$outcome == dv, ]
  emotions <- names(emotion_labels)
  
  build_rows <- function(dat) {
    rows <- c()
    for (em in emotions) {
      r <- dat[dat$mediator == em, ]
      if (nrow(r) == 0) next
      label <- emotion_labels[em]
      rows <- c(rows,
                paste0("\\quad ", label, " & ",
                       fmt(r$ACME, r$ACME_p), " & ",
                       fmt(r$ADE,  r$ADE_p),  " & ",
                       fmt(r$Total, r$Total_p), " \\\\"),
                paste0(" & ",
                       fmt_ci(r$ACME_lo, r$ACME_hi), " & & \\\\")
      )
    }
    rows
  }
  
  anti_rows <- build_rows(anti)
  pro_rows  <- build_rows(pro)
  
  n_anti <- if(nrow(anti) > 0) anti$N[1] else "---"
  n_pro  <- if(nrow(pro) > 0)  pro$N[1]  else "---"
  
  cat(paste0(
    "\\begin{table}[htbp]
\\centering
\\caption{", caption, "}
\\label{tab:med_", gsub("ParticipationAlt1_", "", dv), "}
\\begin{tabular}{lccc}
\\hline\\hline
 & ACME & ADE & Total Effect \\\\
\\hline
\\multicolumn{4}{l}{\\textit{Panel A: Anti-Immigrant Treatment vs. Control}} \\\\
\\hline
", paste(anti_rows, collapse = "\n"), "
\\hline
\\textit{N} & \\multicolumn{3}{l}{", n_anti, "} \\\\
\\hline
\\multicolumn{4}{l}{\\textit{Panel B: Pro-Immigrant Treatment vs. Control}} \\\\
\\hline
", paste(pro_rows, collapse = "\n"), "
\\hline
\\textit{N} & \\multicolumn{3}{l}{", n_pro, "} \\\\
\\hline\\hline
\\multicolumn{4}{l}{\\textit{Note:} $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.1$} \\\\
\\multicolumn{4}{l}{95\\% confidence intervals in brackets below ACME estimates.} \\\\
\\multicolumn{4}{l}{Estimates based on quasi-Bayesian simulation (5,000 draws).} \\\\
\\end{tabular}
\\end{table}
"
  ))
}

# Regenerate all three
make_mediation_latex(
  dv        = "ParticipationAlt1_donate",
  anti_table = anti_table,
  pro_table  = pro_table,
  caption   = "Causal Mediation Analysis: Willingness to Donate to CHIRLA"
)

make_mediation_latex(
  dv        = "ParticipationAlt1_volunteer",
  anti_table = anti_table,
  pro_table  = pro_table,
  caption   = "Causal Mediation Analysis: Willingness to Volunteer with CHIRLA"
)

make_mediation_latex(
  dv        = "ParticipationAlt1_protest",
  anti_table = anti_table,
  pro_table  = pro_table,
  caption   = "Causal Mediation Analysis: Willingness to Join a Protest with CHIRLA"
)

sink("mediation_tables_ch4.tex")
make_mediation_latex("ParticipationAlt1_donate", anti_table, pro_table,
                     caption = "Causal Mediation Analysis: Willingness to Donate to CHIRLA")
make_mediation_latex("ParticipationAlt1_volunteer", anti_table, pro_table,
                     caption = "Causal Mediation Analysis: Willingness to Volunteer with CHIRLA")
make_mediation_latex("ParticipationAlt1_protest", anti_table, pro_table,
                     caption = "Causal Mediation Analysis: Willingness to Protest with CHIRLA")
sink()


### ITT Pre-Treatment Models 

# Clean ITT models - pre-treatment controls only
pre_controls <- c("Age", "Sex", "Education", "Income", "PartyID_5pt", 
                  "PolKnow_index", "Acculturation", "Span_Acc", "latino_conc_24")

itt_donate   <- lm(ParticipationAlt1_donate ~ Treatment + Age + Sex + Education + 
                     Income + PartyID_5pt + PolKnow_index + Acculturation + 
                     Span_Acc + latino_conc_24,
                   data = df_clean %>% filter(!is.na(Treatment)))

itt_volunteer <- lm(ParticipationAlt1_volunteer ~ Treatment + Age + Sex + Education + 
                      Income + PartyID_5pt + PolKnow_index + Acculturation + 
                      Span_Acc + latino_conc_24,
                    data = df_clean %>% filter(!is.na(Treatment)))

itt_protest  <- lm(ParticipationAlt1_protest ~ Treatment + Age + Sex + Education + 
                     Income + PartyID_5pt + PolKnow_index + Acculturation + 
                     Span_Acc + latino_conc_24,
                   data = df_clean %>% filter(!is.na(Treatment)))

stargazer(itt_donate, itt_volunteer, itt_protest, type = "text")


library(sandwich)
library(lmtest)

# Robust SEs (HC2 is standard for experiments)
coeftest(itt_donate, vcov = vcovHC(itt_donate, type = "HC2"))
coeftest(itt_volunteer, vcov = vcovHC(itt_donate, type = "HC2"))
coeftest(itt_protest, vcov = vcovHC(itt_donate, type = "HC2"))


