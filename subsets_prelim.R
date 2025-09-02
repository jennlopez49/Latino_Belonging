### interactions of emotions 

high_stigma <- subset(cmps_lat_16, cmps_lat_16$variables$ICI_Reverse >= 0)
low_stigma <- subset(cmps_lat_16, cmps_lat_16$variables$ICI_Reverse < 0)

### Setting up the models ---- 

int_terms <- ("Linked_Fate*More_Than_SecondGen")
dvs <- list("Fear_Election", "Angry_Election", "Pride_Election", "Hope_Election",
                  "Sad_Election")  # List of Mediators (M)
controls <- c("Age", "Gender", "Education", "Income", "Pol_Interest", 
              "Mexican", "Cuban",
              "Linked_Fate", "Party", "Discrimination_Scale", 
              "Discrimination_National_Perc") 

interaction_function_emotions <- function(dvs, controls, int_term, dat, out = NULL) {
  # Standardization function
  standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  
  # Copy dataset to avoid modifying the original
  dat_std <- dat
  vars_to_standardize <- unique(c(controls))  # Select variables to standardize
  
  # Standardize the variables
  for (var in vars_to_standardize) {
    if (var %in% names(dat_std)) {
      dat_std[[var]] <- standardize(dat_std[[var]])
    }
  }
  
  interaction_results <- list()  # Initialize an empty list to store the models
  
  # Loop over dependent variables (DVs)
  for (Y in dvs) {
    # Create the model formula with the interaction term for each dependent variable
    form_interaction <- as.formula(paste(Y, " ~ ", int_term, "+", paste(controls, collapse = " + ")))
    
    # Ensure that 'dat' is a survey object, check its class
    if ("survey.design" %in% class(dat)) {
      interaction_model <- svyglm(form_interaction, design = dat, family = gaussian())
    } else {
      # If it's not a survey object, fit a regular linear model
      interaction_model <- lm(form_interaction, data = dat_std)
    }
    
    # Store the model result in the list
    interaction_results[[Y]] <- interaction_model
  }
  
  # If 'out' is provided, assign the list to the 'out' object
  if (!is.null(out)) {
    if (!is.character(out)) stop("Argument 'out' must be a character string")
    assign(out, interaction_results, envir = .GlobalEnv)  # Assign list to global env
  } else {
    return(interaction_results)  # Return the list if no 'out' argument
  }
}


interaction_function_emotions(dvs, controls = controls, int_term = int_terms, dat = high_stigma, out = "highstigma.mods")
interaction_function_emotions(dvs, controls = controls, int_term = int_terms, dat = low_stigma, out = "lowstigma.mods")

stargazer(highstigma.mods, type = "text", dep.var.labels = c("Fear", "Angry", 
                                                             "Pride", "Hope", "Sadness"))
          ,
          covariate.labels = c("Linked Fate","More than Second Gen +", "Age", 
                               "Gender", "Education", "Income", "Pol_Interest",
                               "Mexican", "Cuban", "Party", "Discrimination Exp.",
                               "Nat. Perc. Discrimination", "Linked Fate x More than Second Gen +",
                               "Constant"))
          # ,
          # out = "highstigma_mods.tex")
