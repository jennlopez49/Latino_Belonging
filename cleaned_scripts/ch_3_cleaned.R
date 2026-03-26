# =============================================================================
# Chapter 3: Stigma context --> Emotions --> Immigration Policy Attitudes
# DVs: BorderSecurity (-1/0/1), Pathway_Citizenship (1-5)
# Primary IV: class.conc_lat_14_16 (higher = more pro-immigrant)
# =============================================================================

source("00_utilities.R")
source("01_import_2016.R")

# Chapter 3 DVs
ch3_dvs <- c("BorderSecurity", "Pathway_Citizenship")

# Chapter 3 controls (adds BorderState to basic controls)
ch3_controls <- c(basic.controls, "BorderState")

# State-clustered survey design (used for lavaan.survey)
state_ses <- svydesign(
  ids     = ~State,
  data    = latinos_data,
  weights = ~Weight
)

# =============================================================================
# SECTION 1: STIGMA CONTEXT --> EMOTIONS (Mediator models)
# Same approach as chapter 2 but with BorderState added as control
# =============================================================================

# --- 1a. Basic controls + BorderState ----------------------------------------
mediation_function_standard(
  dvs       = ch3_dvs,
  iv        = "class.conc_lat_14_16",
  mediators = emotions,
  controls  = ch3_controls,
  des       = cmps_lat_16,
  dat       = latinos_data,
  out       = "ch3_basic_ols"
)

# --- 1b. Clustered SEs --------------------------------------------------------
res_ch3_fear <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16",
  controls    = c(ch3_controls, "Fear_Election"),
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

res_ch3_anger <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16",
  controls    = c(ch3_controls, "Angry_Election"),
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

res_ch3_sad <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16",
  controls    = c(ch3_controls, "Sad_Election"),
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

res_ch3_pride <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16",
  controls    = c(ch3_controls, "Pride_Election"),
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

res_ch3_hope <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16",
  controls    = c(ch3_controls, "Hope_Election"),
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

# --- 1c. Discrimination as control (no interaction) ---------------------------
res_ch3_disc_group <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16",
  controls    = c(ch3_controls, "Latino_Disc"),
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

res_ch3_disc_pers <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16",
  controls    = c(ch3_controls, "Discrimination_Scale"),
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

# =============================================================================
# SECTION 2: INTERACTION MODELS
# Tests whether discrimination experience moderates stigma --> policy attitude
# Two moderators: group discrimination perception (Latino_Disc),
#                 personal discrimination experience (Discrimination_Scale)
# =============================================================================

res_ch3_int_group <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16*Latino_Disc",
  controls    = ch3_controls,
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

res_ch3_int_pers <- cluster_svyglm(
  dvs         = ch3_dvs,
  iv          = "class.conc_lat_14_16*Discrimination_Scale",
  controls    = ch3_controls,
  dat         = latinos_data,
  cluster_var = "State",
  weight_var  = "Weight"
)

# Non-clustered interaction models (for comparison in tables)
control_ch3_bs <- svyglm(
  BorderSecurity ~ class.conc_lat_14_16 + Age + Gender +
    Party + More_Than_SecondGen + BorderState,
  design = cmps_lat_16, family = gaussian()
)

control_ch3_path <- svyglm(
  Pathway_Citizenship ~ class.conc_lat_14_16 + Age + Gender +
    Party + More_Than_SecondGen + BorderState,
  design = cmps_lat_16, family = gaussian()
)

int_ch3_bs_pers <- svyglm(
  BorderSecurity ~ class.conc_lat_14_16 * Discrimination_Scale + Age + Gender +
    Party + More_Than_SecondGen + BorderState,
  design = cmps_lat_16, family = gaussian()
)

int_ch3_bs_group <- svyglm(
  BorderSecurity ~ class.conc_lat_14_16 * Latino_Disc + Age + Gender +
    Party + More_Than_SecondGen + BorderState,
  design = cmps_lat_16, family = gaussian()
)

int_ch3_path_pers <- svyglm(
  Pathway_Citizenship ~ class.conc_lat_14_16 * Discrimination_Scale + Age + Gender +
    Party + More_Than_SecondGen + BorderState,
  design = cmps_lat_16, family = gaussian()
)

int_ch3_path_group <- svyglm(
  Pathway_Citizenship ~ class.conc_lat_14_16 * Latino_Disc + Age + Gender +
    Party + More_Than_SecondGen + BorderState,
  design = cmps_lat_16, family = gaussian()
)

# =============================================================================
# SECTION 3: MODEL LISTS FOR TABLES
# Organized by DV for clean stargazer output
# =============================================================================

# --- Border Security: all emotion + discrimination models --------------------
listmods_bs <- setNames(
  list(
    res_ch3_fear$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_anger$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_sad$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_hope$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_pride$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_disc_group$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_disc_pers$DV_BorderSecurity_IV_class.conc_lat_14_16,
    res_ch3_int_group$`DV_BorderSecurity_IV_class.conc_lat_14_16*Latino_Disc`,
    res_ch3_int_pers$`DV_BorderSecurity_IV_class.conc_lat_14_16*Discrimination_Scale`
  ),
  c("Fear & Border Security",
    "Anger & Border Security",
    "Sad & Border Security",
    "Hope & Border Security",
    "Pride & Border Security",
    "Border Security & Group Discrimination",
    "Border Security & Personal Discrimination",
    "BS Int Group",
    "BS Int Pers")
)

# --- Pathway for Citizenship: all emotion + discrimination models -------------
listmods_pathway <- setNames(
  list(
    res_ch3_fear$DV_Pathway_Citizenship_IV_class.conc_lat_14_16,
    res_ch3_anger$DV_Pathway_Citizenship_IV_class.conc_lat_14_16,
    res_ch3_sad$DV_Pathway_Citizenship_IV_class.conc_lat_14_16,
    res_ch3_hope$DV_Pathway_Citizenship_IV_class.conc_lat_14_16,
    res_ch3_pride$DV_Pathway_Citizenship_IV_class.conc_lat_14_16,
    res_ch3_disc_group$DV_Pathway_Citizenship_IV_class.conc_lat_14_16,
    res_ch3_disc_pers$DV_Pathway_Citizenship_IV_class.conc_lat_14_16,
    res_ch3_int_group$`DV_Pathway_Citizenship_IV_class.conc_lat_14_16*Latino_Disc`,
    res_ch3_int_pers$`DV_Pathway_Citizenship_IV_class.conc_lat_14_16*Discrimination_Scale`
  ),
  c("Fear & Pathway for Citizenship",
    "Anger & Pathway for Citizenship",
    "Sad & Pathway for Citizenship",
    "Hope & Pathway for Citizenship",
    "Pride & Pathway for Citizenship",
    "Pathway for Citizenship & Group Discrimination",
    "Pathway for Citizenship & Personal Discrimination",
    "PC Int Group",
    "PC Int Pers")
)

# --- Basic models (no clustered SEs) for comparison --------------------------
# Combines control-only, emotion mediation, and interaction models
basic_ch3_bs <- list(
  control_ch3_bs,
  ch3_basic_ols$outcome_models[1:5],   # emotion models: Fear, Anger, Sad, Pride, Hope
  int_ch3_bs_pers,
  int_ch3_bs_group
)

basic_ch3_path <- list(
  control_ch3_path,
  ch3_basic_ols$outcome_models[6:10],  # emotion models: Fear, Anger, Sad, Pride, Hope
  int_ch3_path_pers,
  int_ch3_path_group
)

# =============================================================================
# SECTION 4: TABLES
# =============================================================================

# --- Table: Border Security (clustered SEs) ----> bs_mods.tex ----------------
stargazer(listmods_bs,
          type   = "latex",
          dep.var.labels = "Tighten Border Security",
          covariate.labels = c("Concrete Imm. Index",
                               "Age", "Gender",
                               "Party (R $\\longrightarrow$ D)",
                               "Second Gen +", "Border State",
                               "Fear", "Anger", "Sadness", "Hope", "Pride",
                               "Group Discrimination",
                               "Personal Discrimination",
                               "Concrete Imm. Index $\\times$ Group Disc.",
                               "Concrete Imm. Index $\\times$ Pers. Disc.",
                               "Constant"),
          add.lines = list(c("Clustered SEs", rep("Yes", 9))),
          label = "bs.mods",
          out   = "tables/bs_mods.tex")

# --- Table: Pathway for Citizenship (clustered SEs) ----> cit_mods.tex -------
stargazer(listmods_pathway,
          type   = "latex",
          dep.var.labels = "Pathway for Citizenship",
          covariate.labels = c("Concrete Imm. Index",
                               "Age", "Gender",
                               "Party (R $\\longrightarrow$ D)",
                               "Second Gen +", "Border State",
                               "Fear", "Anger", "Sadness", "Hope", "Pride",
                               "Group Discrimination",
                               "Personal Discrimination",
                               "Concrete Imm. Index $\\times$ Group Disc.",
                               "Concrete Imm. Index $\\times$ Pers. Disc.",
                               "Constant"),
          add.lines = list(c("Clustered SEs", rep("Yes", 9))),
          label = "cit.mods",
          out   = "tables/cit_mods.tex")

# --- Table: Border Security basic (no clustered SEs) ----> basic_bs.tex ------
stargazer(basic_ch3_bs,
          type   = "latex",
          dep.var.labels = "Tighten Border Security",
          covariate.labels = c("Concrete Imm. Index",
                               "Age", "Gender",
                               "Party (R $\\longrightarrow$ D)",
                               "Second Gen +", "Border State",
                               "Fear", "Anger", "Pride", "Hope", "Sadness",
                               "Personal Discrimination",
                               "Group Discrimination",
                               "Concrete Imm. Index $\\times$ Pers. Disc.",
                               "Concrete Imm. Index $\\times$ Group Disc.",
                               "Constant"),
          add.lines = list(c("Clustered SEs", rep("No", length(basic_ch3_bs)))),
          label = "basic.bs",
          out   = "tables/basic_bs.tex")

# --- Table: Pathway for Citizenship basic ----> basic_path.tex ---------------
stargazer(basic_ch3_path,
          type   = "latex",
          dep.var.labels = "Pathway for Citizenship",
          covariate.labels = c("Concrete Imm. Index",
                               "Age", "Gender",
                               "Party (R $\\longrightarrow$ D)",
                               "Second Gen +", "Border State",
                               "Fear", "Anger", "Pride", "Hope", "Sadness",
                               "Personal Discrimination",
                               "Group Discrimination",
                               "Concrete Imm. Index $\\times$ Pers. Disc.",
                               "Concrete Imm. Index $\\times$ Group Disc.",
                               "Constant"),
          add.lines = list(c("Clustered SEs", rep("No", length(basic_ch3_path)))),
          label = "basic.path",
          out   = "tables/basic_path.tex")

# =============================================================================
# SECTION 5: PRIMARY SEM MODELS (lavaan + lavaan.survey)
# Stigma --> Discrimination --> Policy Attitude
# Discrimination (personal and group) as mediator, not moderator
# =============================================================================

# --- Border Security: personal discrimination as mediator --------------------
bs_med_pers <- '
  Discrimination_Scale ~ a*class.conc_lat_14_16 + Age + Gender +
                         Party + More_Than_SecondGen + BorderState
  BorderSecurity ~ b*Discrimination_Scale + c*class.conc_lat_14_16 +
                   Age + Gender + Party + More_Than_SecondGen + BorderState
  indirect := a * b
  total    := c + (a * b)
'
fit_bs_pers    <- sem(bs_med_pers, data = latinos_data, meanstructure = TRUE)
fit_bs1_svy    <- lavaan.survey(fit_bs_pers, survey.design = state_ses)
summary(fit_bs1_svy, standardized = TRUE, fit.measures = TRUE)

# --- Border Security: group discrimination as mediator -----------------------
bs_med_group <- '
  Latino_Disc ~ a*class.conc_lat_14_16 + Age + Gender +
                Party + More_Than_SecondGen + BorderState
  BorderSecurity ~ b*Latino_Disc + c*class.conc_lat_14_16 +
                   Age + Gender + Party + More_Than_SecondGen + BorderState
  indirect := a * b
  total    := c + (a * b)
'
fit_bs_group   <- sem(bs_med_group, data = latinos_data, meanstructure = TRUE)
fit_bs2_svy    <- lavaan.survey(fit_bs_group, survey.design = state_ses)
summary(fit_bs2_svy, standardized = TRUE, fit.measures = TRUE)

# --- Pathway for Citizenship: personal discrimination as mediator ------------
path_med_pers <- '
  Discrimination_Scale ~ a*class.conc_lat_14_16 + Age + Gender +
                         Party + More_Than_SecondGen + BorderState
  Pathway_Citizenship ~ b*Discrimination_Scale + c*class.conc_lat_14_16 +
                        Age + Gender + Party + More_Than_SecondGen + BorderState
  indirect := a * b
  total    := c + (a * b)
'
fit_path_pers  <- sem(path_med_pers, data = latinos_data, meanstructure = TRUE)
fit_path1_svy  <- lavaan.survey(fit_path_pers, survey.design = state_ses)
summary(fit_path1_svy, standardized = TRUE, fit.measures = TRUE)

# --- Pathway for Citizenship: group discrimination as mediator ---------------
path_med_group <- '
  Latino_Disc ~ a*class.conc_lat_14_16 + Age + Gender +
                Party + More_Than_SecondGen + BorderState
  Pathway_Citizenship ~ b*Latino_Disc + c*class.conc_lat_14_16 +
                        Age + Gender + Party + More_Than_SecondGen + BorderState
  indirect := a * b
  total    := c + (a * b)
'
fit_path_group <- sem(path_med_group, data = latinos_data, meanstructure = TRUE)
fit_path2_svy  <- lavaan.survey(fit_path_group, survey.design = state_ses)
summary(fit_path2_svy, standardized = TRUE, fit.measures = TRUE)

# =============================================================================
# SECTION 6: MARGINAL EFFECTS FOR SIGNIFICANT INTERACTIONS
# Plot interaction between stigma context and discrimination experience
# Update model name if a different interaction is significant
# =============================================================================

# Clustered SE model (preferred for inference)
int_path_cl <- res_ch3_int_pers$`DV_Pathway_Citizenship_IV_class.conc_lat_14_16*Discrimination_Scale`

# Non-clustered model (for comparison)
int_path_basic <- int_ch3_path_pers

# Estimated marginal means
emm_cl    <- emmeans(int_path_cl,    ~ Discrimination_Scale | class.conc_lat_14_16)
emm_basic <- emmeans(int_path_basic, ~ Discrimination_Scale | class.conc_lat_14_16)
contrast(emm_cl,    method = "pairwise")
contrast(emm_basic, method = "pairwise")

# Interaction plot
int_plot <- plot_model(int_path_cl, type = "int") +
  labs(x     = "Concrete Imm. Index",
       color = "Personal Discrimination",
       title = "Interaction: Stigma Context x Personal Discrimination")

ggsave(filename = "figures/int_plot_ch3.png",
       plot     = int_plot,
       width    = 10,
       height   = 8,
       dpi      = 300)