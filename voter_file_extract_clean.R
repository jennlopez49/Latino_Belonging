# Multi-State Voter File Cleaning Pipeline
# For New York and Florida Voter Files

# Load required packages
library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Base configuration
config <- list(
  # UPDATE THIS PATH to your external drive location
  # On Mac, external drives are typically at /Volumes/[DriveName]
  base_path = "/Volumes/easystore",  # Path to external drive containing FL_VoterFiles and NY_VoterFiles
  
  states = c("FL", "NY"),  # States to process
  clean_folder_name = "cleaned_data",  # Name of subfolder for cleaned files
  unzip_temp = "temp_unzipped"  # Temporary folder for unzipped files
)

# Automatically detect state folders
state_folders <- list.files(config$base_path, pattern = "^(FL|NY)_VoterFiles$", full.names = TRUE)
cat("Detected state folders:", paste(state_folders, collapse = ", "), "\n")

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Function to clean names
clean_name <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z\\s\\-']", "") %>%
    str_squish()
}

# Function to standardize addresses
clean_address <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace_all("\\s+", " ") %>%
    # Standardize common abbreviations
    str_replace_all("\\bSTREET\\b", "ST") %>%
    str_replace_all("\\bAVENUE\\b", "AVE") %>%
    str_replace_all("\\bROAD\\b", "RD") %>%
    str_replace_all("\\bBOULEVARD\\b", "BLVD") %>%
    str_replace_all("\\bDRIVE\\b", "DR") %>%
    str_replace_all("\\bLANE\\b", "LN") %>%
    str_replace_all("\\bCOURT\\b", "CT") %>%
    str_replace_all("\\bAPARTMENT\\b", "APT") %>%
    str_replace_all("\\bSUITE\\b", "STE")
}

# Function to validate dates
parse_date_safe <- function(x, format = "%m/%d/%Y") {
  parsed <- parse_date_time(x, orders = c("mdy", "ymd", "dmy"), quiet = TRUE)
  as.Date(parsed)
}

# Function to standardize party codes
standardize_party <- function(x, state) {
  x <- str_trim(toupper(x))
  
  if (state == "FL") {
    # Florida party standardization
    case_when(
      x %in% c("DEM", "DEMOCRATIC") ~ "DEM",
      x %in% c("REP", "REPUBLICAN") ~ "REP",
      x == "NPA" ~ "NPA",
      x == "LPF" ~ "LIB",
      x == "GRE" ~ "GRN",
      TRUE ~ "OTH"
    )
  } else if (state == "NY") {
    # New York party standardization
    case_when(
      x %in% c("DEM", "DEMOCRATIC") ~ "DEM",
      x %in% c("REP", "REPUBLICAN") ~ "REP",
      x %in% c("IND", "INDEPENDENCE") ~ "IND",
      x %in% c("CON", "CONSERVATIVE") ~ "CON",
      x %in% c("WOR", "WORKING FAMILIES") ~ "WFP",
      x %in% c("GRE", "GREEN") ~ "GRN",
      x %in% c("LIB", "LIBERTARIAN") ~ "LIB",
      x == "BLANK" | is.na(x) ~ "UNENROLLED",
      TRUE ~ "OTH"
    )
  }
}

# =============================================================================
# FLORIDA DATA CLEANING
# =============================================================================

clean_florida_registration <- function(file_path) {
  cat("Processing Florida registration file:", basename(file_path), "\n")
  
  # Read tab-delimited file
  df <- fread(file_path, sep = "\t", quote = "", na.strings = c("", "NA"))
  
  # Expected column structure (based on FL layout)
  expected_cols <- c(
    "CountyCode", "VoterID", "NameLast", "NameSuffix", "NameFirst", 
    "NameMiddle", "Exemption", "ResidenceAddressLine1", "ResidenceAddressLine2",
    "ResidenceCity", "ResidenceState", "ResidenceZipcode",
    "MailingAddressLine1", "MailingAddressLine2", "MailingAddressLine3",
    "MailingCity", "MailingState", "MailingZipcode", "MailingCountry",
    "Gender", "Race", "BirthDate", "RegistrationDate", "PartyAffiliation",
    "Precinct", "PrecinctGroup", "PrecinctSplit", "PrecinctSuffix",
    "VoterStatus", "CongressionalDistrict", "HouseDistrict", "SenateDistrict",
    "CountyCommissionDistrict", "SchoolBoardDistrict",
    "DaytimeAreaCode", "DaytimePhoneNumber", "DaytimePhoneExtension", "EmailAddress"
  )
  
  # Set column names if not present
  if (ncol(df) == length(expected_cols)) {
    setnames(df, expected_cols)
  }
  
  # Clean and standardize
  cleaned <- df %>%
    mutate(
      # Clean names
      NameLast = clean_name(NameLast),
      NameFirst = clean_name(NameFirst),
      NameMiddle = clean_name(NameMiddle),
      NameSuffix = clean_name(NameSuffix),
      
      # Full name for matching
      FullName = paste(NameFirst, NameMiddle, NameLast, NameSuffix) %>% 
        str_squish(),
      
      # Clean addresses
      ResidenceAddressLine1 = clean_address(ResidenceAddressLine1),
      ResidenceCity = str_to_upper(str_trim(ResidenceCity)),
      MailingAddressLine1 = clean_address(MailingAddressLine1),
      MailingCity = str_to_upper(str_trim(MailingCity)),
      
      # Standardize zip codes (keep first 5 digits)
      ResidenceZipcode = str_sub(str_trim(ResidenceZipcode), 1, 5),
      MailingZipcode = str_sub(str_trim(MailingZipcode), 1, 5),
      
      # Parse dates
      BirthDate = parse_date_safe(BirthDate),
      RegistrationDate = parse_date_safe(RegistrationDate),
      
      # Calculate age as of today
      Age = as.numeric(difftime(Sys.Date(), BirthDate, units = "days")) / 365.25,
      
      # Standardize categorical variables
      Gender = toupper(str_trim(Gender)),
      Gender = case_when(
        Gender %in% c("F", "FEMALE") ~ "F",
        Gender %in% c("M", "MALE") ~ "M",
        TRUE ~ "U"
      ),
      
      VoterStatus = toupper(str_trim(VoterStatus)),
      PartyAffiliation = standardize_party(PartyAffiliation, "FL"),
      
      # Create active voter flag
      IsActive = VoterStatus == "ACT",
      
      # State identifier
      State = "FL"
    ) %>%
    # Remove records with missing critical data
    filter(
      !is.na(VoterID),
      !is.na(NameLast),
      !is.na(NameFirst)
    ) %>%
    # Flag potential duplicates
    group_by(FullName, BirthDate, ResidenceZipcode) %>%
    mutate(
      PotentialDuplicate = n() > 1,
      DuplicateCount = n()
    ) %>%
    ungroup()
  
  return(cleaned)
}

clean_florida_history <- function(file_path) {
  cat("Processing Florida history file:", basename(file_path), "\n")
  
  df <- fread(file_path, sep = "\t", quote = "", na.strings = c("", "NA"))
  
  # Expected columns for history file
  expected_cols <- c("CountyCode", "VoterID", "ElectionDate", "ElectionType", "HistoryCode")
  
  if (ncol(df) == length(expected_cols)) {
    setnames(df, expected_cols)
  }
  
  cleaned <- df %>%
    mutate(
      ElectionDate = parse_date_safe(ElectionDate),
      ElectionYear = year(ElectionDate),
      ElectionType = toupper(str_trim(ElectionType)),
      HistoryCode = toupper(str_trim(HistoryCode)),
      
      # Decode voting method
      VotingMethod = case_when(
        HistoryCode == "A" ~ "Absentee/Mail",
        HistoryCode == "E" ~ "Early Voting",
        HistoryCode == "Y" ~ "Polling Place",
        HistoryCode == "P" ~ "Provisional (Not Counted)",
        HistoryCode == "B" ~ "Mail Ballot (Not Counted)",
        HistoryCode == "L" ~ "Mail Late (Not Counted)",
        HistoryCode == "N" ~ "Did Not Vote",
        TRUE ~ "Unknown"
      ),
      
      # Flag whether vote was counted
      VoteCounted = !HistoryCode %in% c("B", "L", "P", "N"),
      
      State = "FL"
    ) %>%
    filter(!is.na(VoterID), !is.na(ElectionDate))
  
  return(cleaned)
}

# =============================================================================
# NEW YORK DATA CLEANING
# =============================================================================

clean_newyork_voters <- function(file_path) {
  cat("Processing New York voter file:", basename(file_path), "\n")
  
  # Read comma-delimited file
  df <- fread(file_path, sep = ",", quote = "\"", na.strings = c("", "NA"))
  
  # Common NY fields (adjust based on actual structure)
  # Note: Actual field names may vary - check your specific file
  
  cleaned <- df %>%
    rename_with(~str_to_upper(str_trim(.))) %>%
    mutate(
      # Clean names (adjust column names as needed)
      across(contains(c("LAST", "FIRSTNAME", "FIRST_NAME")), clean_name),
      
      # Standardize addresses
      across(contains(c("ADDRESS", "STREET")), clean_address),
      across(contains("CITY"), ~str_to_upper(str_trim(.))),
      
      # Standardize zip codes
      across(contains("ZIP"), ~str_sub(str_trim(.), 1, 5)),
      
      # Parse dates
      across(contains(c("DOB", "BIRTH", "REGDATE", "REGISTRATION")), 
             ~parse_date_safe(.)),
      
      State = "NY"
    )
  
  # If party enrollment exists, standardize it
  if (any(str_detect(names(cleaned), "PARTY|ENROLL"))) {
    party_col <- names(cleaned)[str_detect(names(cleaned), "PARTY|ENROLL")][1]
    cleaned <- cleaned %>%
      mutate(!!party_col := standardize_party(!!sym(party_col), "NY"))
  }
  
  # Parse voting history if it exists (NY concatenates history in one field)
  if (any(str_detect(names(cleaned), "HISTORY|VOTEHISTORY"))) {
    hist_col <- names(cleaned)[str_detect(names(cleaned), "HISTORY")][1]
    
    cleaned <- cleaned %>%
      mutate(
        VotingHistoryRaw = !!sym(hist_col),
        # Count number of elections voted in
        ElectionsVoted = str_count(VotingHistoryRaw, ";") + 1,
        # Extract recent elections (last 10 years)
        RecentElections = str_extract_all(
          VotingHistoryRaw, 
          "\\d{2}/\\d{2}/\\d{4}"
        )
      )
  }
  
  return(cleaned)
}

# =============================================================================
# FILE ORGANIZATION AND UNZIPPING FUNCTIONS
# =============================================================================

# Function to get directory structure
get_directory_structure <- function(state_folder) {
  state <- str_extract(basename(state_folder), "^[A-Z]{2}")
  
  # Look for folders like FL_VoterFiles_2021, FL_VoterFiles_2022, etc.
  year_pattern <- paste0("^", state, "_VoterFiles_\\d{4}$")
  years <- list.dirs(state_folder, full.names = TRUE, recursive = FALSE) %>%
    .[str_detect(basename(.), year_pattern)]
  
  structure <- lapply(years, function(year_path) {
    year_name <- basename(year_path)
    # Extract just the year (e.g., "2021" from "FL_VoterFiles_2021")
    year_num <- str_extract(year_name, "\\d{4}$")
    
    zip_files <- list.files(year_path, pattern = "\\.zip$", full.names = TRUE)
    
    list(
      year = year_num,
      year_folder_name = year_name,
      year_path = year_path,
      zip_files = zip_files,
      clean_path = file.path(year_path, config$clean_folder_name)
    )
  })
  
  names(structure) <- sapply(structure, function(x) x$year)
  return(structure)
}

# Function to unzip and process a single zip file
unzip_and_extract <- function(zip_path, temp_dir) {
  cat("  Unzipping:", basename(zip_path), "\n")
  
  # Create temp directory for this zip
  zip_temp <- file.path(temp_dir, tools::file_path_sans_ext(basename(zip_path)))
  dir.create(zip_temp, showWarnings = FALSE, recursive = TRUE)
  
  # Unzip
  tryCatch({
    unzip(zip_path, exdir = zip_temp)
    
    # Return paths to extracted files
    extracted_files <- list.files(zip_temp, pattern = "\\.(txt|csv)$", 
                                  full.names = TRUE, recursive = TRUE)
    return(list(
      zip_file = zip_path,
      extracted_files = extracted_files,
      temp_dir = zip_temp
    ))
  }, error = function(e) {
    cat("    Error unzipping:", e$message, "\n")
    return(NULL)
  })
}

# Function to determine file type (registration vs history for FL)
classify_florida_file <- function(filename) {
  base_name <- basename(filename)
  
  # Florida naming: YYYY.MM.DD_VoterDetail.zip and YYYY.MM.DD_VoterHistory.zip
  if (str_detect(base_name, "VoterHistory|_VoterHistory|History")) {
    return("history")
  } else if (str_detect(base_name, "VoterDetail|_VoterDetail|Detail|Registration")) {
    return("registration")
  } else {
    # If unclear, try to determine from content after unzipping
    return("unknown")
  }
}

classify_by_content <- function(filepath) {
  # Read first few lines to check column structure
  tryCatch({
    first_lines <- readLines(filepath, n = 2)
    header <- first_lines[1]
    
    # Florida history has fewer columns (5): CountyCode, VoterID, ElectionDate, ElectionType, HistoryCode
    # Florida registration has many columns (38)
    
    col_count <- length(strsplit(header, "\t")[[1]])
    
    if (col_count <= 10) {
      return("history")
    } else {
      return("registration")
    }
  }, error = function(e) {
    return("unknown")
  })
}

# Function to get output filename
get_output_filename <- function(original_zip_path, file_type = NULL) {
  base_name <- tools::file_path_sans_ext(basename(original_zip_path))
  
  # Remove "VoterDetail" or "VoterHistory" from the name if present
  # e.g., "2022.02.14_VoterDetail" -> "2022.02.14"
  base_name <- str_replace(base_name, "_VoterDetail$|_VoterHistory$|_VoterRegistration$", "")
  
  if (!is.null(file_type) && file_type == "history") {
    output_name <- paste0(base_name, "_history_clean.csv")
  } else if (!is.null(file_type) && file_type == "registration") {
    output_name <- paste0(base_name, "_registration_clean.csv")
  } else {
    output_name <- paste0(base_name, "_clean.csv")
  }
  
  return(output_name)
}

# =============================================================================
# AUTOMATED PROCESSING FUNCTIONS
# =============================================================================

process_single_zip <- function(zip_info, state, year_clean_path) {
  if (is.null(zip_info)) return(NULL)
  
  cat("\n  Processing zip:", basename(zip_info$zip_file), "\n")
  
  # Create cleaned data folder if it doesn't exist
  dir.create(year_clean_path, showWarnings = FALSE, recursive = TRUE)
  
  results <- list()
  
  if (state == "FL") {
    # Classify the zip file by name first
    zip_type <- classify_florida_file(zip_info$zip_file)
    
    # Process files based on type
    for (file in zip_info$extracted_files) {
      # If zip type is unknown, check file content
      if (zip_type == "unknown") {
        file_type <- classify_by_content(file)
      } else {
        file_type <- zip_type
      }
      
      tryCatch({
        if (file_type == "history") {
          cleaned_data <- clean_florida_history(file)
          output_name <- get_output_filename(zip_info$zip_file, "history")
          output_path <- file.path(year_clean_path, output_name)
          fwrite(cleaned_data, output_path)
          cat("    Saved history to:", basename(output_path), "\n")
          results$history <- cleaned_data
          
        } else if (file_type == "registration") {
          cleaned_data <- clean_florida_registration(file)
          output_name <- get_output_filename(zip_info$zip_file, "registration")
          output_path <- file.path(year_clean_path, output_name)
          fwrite(cleaned_data, output_path)
          cat("    Saved registration to:", basename(output_path), "\n")
          results$registration <- cleaned_data
          
        } else {
          cat("    Warning: Could not determine file type for:", basename(file), "\n")
        }
      }, error = function(e) {
        cat("    Error processing file:", e$message, "\n")
      })
    }
    
  } else if (state == "NY") {
    # Process NY voter files
    for (voter_file in zip_info$extracted_files) {
      tryCatch({
        cleaned_voters <- clean_newyork_voters(voter_file)
        output_name <- get_output_filename(zip_info$zip_file)
        output_path <- file.path(year_clean_path, output_name)
        fwrite(cleaned_voters, output_path)
        cat("    Saved to:", basename(output_path), "\n")
        results$voters <- cleaned_voters
      }, error = function(e) {
        cat("    Error processing NY file:", e$message, "\n")
      })
    }
  }
  
  return(results)
}

process_year_folder <- function(year_info, state, temp_base) {
  cat("\nProcessing year folder:", year_info$year_folder_name, "\n")
  cat("Found", length(year_info$zip_files), "zip files\n")
  
  if (length(year_info$zip_files) == 0) {
    cat("  No zip files found, skipping...\n")
    return(NULL)
  }
  
  # Show what files were found
  cat("  Files:\n")
  for (zf in basename(year_info$zip_files)) {
    cat("    -", zf, "\n")
  }
  
  # Create year-specific temp directory
  year_temp <- file.path(temp_base, year_info$year)
  dir.create(year_temp, showWarnings = FALSE, recursive = TRUE)
  
  # Process each zip file
  all_results <- list()
  
  for (zip_file in year_info$zip_files) {
    # Unzip
    zip_info <- unzip_and_extract(zip_file, year_temp)
    
    # Process extracted files
    if (!is.null(zip_info) && length(zip_info$extracted_files) > 0) {
      result <- process_single_zip(zip_info, state, year_info$clean_path)
      all_results[[basename(zip_file)]] <- result
    }
  }
  
  # Clean up temp directory for this year
  unlink(year_temp, recursive = TRUE)
  
  return(all_results)
}

process_state_folder <- function(state_folder) {
  state <- str_extract(basename(state_folder), "^[A-Z]{2}")
  cat("\n", strrep("=", 70), "\n")
  cat("PROCESSING STATE:", state, "\n")
  cat(strrep("=", 70), "\n")
  
  # Get directory structure
  dir_structure <- get_directory_structure(state_folder)
  
  if (length(dir_structure) == 0) {
    cat("No year folders found in", state_folder, "\n")
    return(NULL)
  }
  
  # Create temp directory for this state
  state_temp <- file.path(config$base_path, config$unzip_temp, state)
  dir.create(state_temp, showWarnings = FALSE, recursive = TRUE)
  
  # Process each year
  state_results <- list()
  for (year_name in names(dir_structure)) {
    year_results <- process_year_folder(dir_structure[[year_name]], state, state_temp)
    state_results[[year_name]] <- year_results
  }
  
  # Clean up state temp directory
  unlink(state_temp, recursive = TRUE)
  
  cat("\n", strrep("=", 70), "\n")
  cat("COMPLETED STATE:", state, "\n")
  cat(strrep("=", 70), "\n\n")
  
  return(state_results)
}

# =============================================================================
# TEST MODE FUNCTIONS
# =============================================================================

# Function to run test on one file per state
run_test_mode <- function() {
  cat("\n", strrep("=", 70), "\n")
  cat("TEST MODE - Processing one file per state\n")
  cat(strrep("=", 70), "\n\n")
  
  # Find all state folders
  state_folders <- list.files(config$base_path, 
                              pattern = "^(FL|NY)_VoterFiles$", 
                              full.names = TRUE)
  
  if (length(state_folders) == 0) {
    cat("ERROR: No state folders found!\n")
    return(NULL)
  }
  
  test_results <- list()
  
  for (state_folder in state_folders) {
    state <- str_extract(basename(state_folder), "^[A-Z]{2}")
    cat("\n", strrep("-", 70), "\n")
    cat("Testing", state, "\n")
    cat(strrep("-", 70), "\n")
    
    # Get directory structure
    dir_struct <- get_directory_structure(state_folder)
    
    if (length(dir_struct) == 0) {
      cat("No year folders found for", state, "\n")
      next
    }
    
    # Find first year with zip files
    test_year_info <- NULL
    for (year_info in dir_struct) {
      if (length(year_info$zip_files) > 0) {
        test_year_info <- year_info
        break
      }
    }
    
    if (is.null(test_year_info)) {
      cat("No zip files found for", state, "\n")
      next
    }
    
    cat("Selected test file from:", test_year_info$year_folder_name, "\n")
    
    # For Florida, try to get one registration and one history file
    if (state == "FL") {
      detail_file <- NULL
      history_file <- NULL
      
      for (zip_file in test_year_info$zip_files) {
        if (is.null(detail_file) && str_detect(basename(zip_file), "VoterRegistration|Registration|Detail")) {
          detail_file <- zip_file
        }
        if (is.null(history_file) && str_detect(basename(zip_file), "VoterHistory|History")) {
          history_file <- zip_file
        }
        if (!is.null(detail_file) && !is.null(history_file)) break
      }
      
      # Use first file if we couldn't find specific types
      if (is.null(detail_file)) detail_file <- test_year_info$zip_files[1]
      
      test_files <- c(detail_file, history_file)
      test_files <- test_files[!is.na(test_files) & !is.null(test_files)]
      
    } else {
      # For NY, just get first file
      test_files <- test_year_info$zip_files[1]
    }
    
    cat("Test file(s):\n")
    for (tf in test_files) {
      cat("  -", basename(tf), "\n")
    }
    
    # Create test temp directory
    test_temp <- file.path(config$base_path, config$unzip_temp, "test", state)
    dir.create(test_temp, showWarnings = FALSE, recursive = TRUE)
    
    # Process test files
    state_test_results <- list()
    
    for (test_file in test_files) {
      cat("\nProcessing:", basename(test_file), "\n")
      
      # Unzip
      zip_info <- unzip_and_extract(test_file, test_temp)
      
      if (!is.null(zip_info) && length(zip_info$extracted_files) > 0) {
        # Process
        result <- process_single_zip(zip_info, state, test_year_info$clean_path)
        state_test_results[[basename(test_file)]] <- result
        
        # Show preview of cleaned data
        if (!is.null(result)) {
          for (data_type in names(result)) {
            if (is.data.frame(result[[data_type]])) {
              cat("\n--- Preview of", data_type, "data ---\n")
              cat("Dimensions:", nrow(result[[data_type]]), "rows x", 
                  ncol(result[[data_type]]), "columns\n")
              cat("\nFirst few rows:\n")
              print(head(result[[data_type]], 3))
              cat("\nColumn names:\n")
              print(names(result[[data_type]]))
              cat("\n")
            }
          }
        }
      }
    }
    
    test_results[[state]] <- state_test_results
    
    # Clean up test temp
    unlink(test_temp, recursive = TRUE)
  }
  
  cat("\n", strrep("=", 70), "\n")
  cat("TEST MODE COMPLETE\n")
  cat(strrep("=", 70), "\n\n")
  
  cat("Review the output above to verify the data is being cleaned correctly.\n")
  cat("Check the cleaned_data folders in each year directory to see the output files.\n\n")
  
  cat("If everything looks good, run: results <- main()\n")
  cat("To process all files for all states.\n\n")
  
  return(test_results)
}

# =============================================================================
# DATA QUALITY CHECKS
# =============================================================================

run_quality_checks <- function(df, state_name = "Unknown") {
  cat("\n=== Quality Checks for", state_name, "===\n")
  
  # Total records
  cat("Total records:", nrow(df), "\n")
  
  # Check for missing critical fields
  cat("\nMissing data summary:\n")
  missing_summary <- df %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Field", values_to = "MissingCount") %>%
    filter(MissingCount > 0) %>%
    arrange(desc(MissingCount))
  
  if (nrow(missing_summary) > 0) {
    print(missing_summary)
  } else {
    cat("No missing data found!\n")
  }
  
  # Check for duplicates
  if ("PotentialDuplicate" %in% names(df)) {
    cat("\nPotential duplicates:", sum(df$PotentialDuplicate, na.rm = TRUE), "\n")
  }
  
  # Age distribution
  if ("Age" %in% names(df)) {
    cat("\nAge distribution:\n")
    print(summary(df$Age))
    cat("Voters under 18:", sum(df$Age < 18, na.rm = TRUE), "\n")
    cat("Voters over 100:", sum(df$Age > 100, na.rm = TRUE), "\n")
  }
  
  # Party distribution
  party_col <- names(df)[str_detect(names(df), "Party")][1]
  if (length(party_col) > 0 && !is.na(party_col)) {
    cat("\nParty distribution:\n")
    print(table(df[[party_col]], useNA = "ifany"))
  }
  
  # Voter status (if available)
  if ("VoterStatus" %in% names(df) || "IsActive" %in% names(df)) {
    status_col <- ifelse("IsActive" %in% names(df), "IsActive", "VoterStatus")
    cat("\nVoter status:\n")
    print(table(df[[status_col]], useNA = "ifany"))
  }
}

# =============================================================================
# SUMMARY REPORTING
# =============================================================================

generate_processing_summary <- function(all_results) {
  cat("\n", strrep("=", 70), "\n")
  cat("PROCESSING SUMMARY\n")
  cat(strrep("=", 70), "\n\n")
  
  for (state in names(all_results)) {
    cat("State:", state, "\n")
    cat(strrep("-", 50), "\n")
    
    state_data <- all_results[[state]]
    if (is.null(state_data)) {
      cat("  No data processed\n\n")
      next
    }
    
    for (year in names(state_data)) {
      cat("  Year:", year, "\n")
      year_data <- state_data[[year]]
      
      if (is.null(year_data)) {
        cat("    No files processed\n")
      } else {
        cat("    Files processed:", length(year_data), "\n")
        
        # Count records
        total_records <- 0
        for (zip_name in names(year_data)) {
          zip_data <- year_data[[zip_name]]
          if (!is.null(zip_data)) {
            for (file_type in names(zip_data)) {
              if (is.data.frame(zip_data[[file_type]])) {
                total_records <- total_records + nrow(zip_data[[file_type]])
              }
            }
          }
        }
        cat("    Total records:", format(total_records, big.mark = ","), "\n")
      }
    }
    cat("\n")
  }
  
  cat(strrep("=", 70), "\n\n")
}

# Function to find all cleaned files in the directory structure
find_cleaned_files <- function() {
  cat("\n", strrep("=", 70), "\n")
  cat("CLEANED FILES LOCATION\n")
  cat(strrep("=", 70), "\n\n")
  
  for (state in config$states) {
    state_folder <- file.path(config$base_path, paste0(state, "_VoterFiles"))
    
    if (!dir.exists(state_folder)) next
    
    cat("State:", state, "\n")
    cat(strrep("-", 50), "\n")
    
    # Find all cleaned_data folders
    clean_folders <- list.dirs(state_folder, recursive = TRUE) %>%
      .[str_detect(., config$clean_folder_name)]
    
    for (clean_folder in clean_folders) {
      year <- basename(dirname(clean_folder))
      files <- list.files(clean_folder, pattern = "_clean\\.csv$")
      
      if (length(files) > 0) {
        cat("  ", year, "/", config$clean_folder_name, "/\n", sep = "")
        for (file in files) {
          cat("    -", file, "\n")
        }
      }
    }
    cat("\n")
  }
  
  cat(strrep("=", 70), "\n\n")
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main <- function() {
  cat("\n", strrep("=", 70), "\n")
  cat("VOTER FILE CLEANING PIPELINE\n")
  cat(strrep("=", 70), "\n")
  cat("Started at:", format(Sys.time()), "\n\n")
  
  cat("Directory structure:\n")
  cat("  Base path:", config$base_path, "\n")
  cat("  Looking for: [State]_VoterFiles/[State]_VoterFiles_[Year]/[Date]_Voter[Detail|History].zip\n")
  cat("  Output: [State]_VoterFiles/[State]_VoterFiles_[Year]/", config$clean_folder_name, 
      "/[Date]_[registration|history]_clean.csv\n\n")
  
  # Find all state folders
  state_folders <- list.files(config$base_path, 
                              pattern = "^(FL|NY)_VoterFiles$", 
                              full.names = TRUE)
  
  if (length(state_folders) == 0) {
    cat("ERROR: No state folders found!\n")
    cat("Please ensure you have folders named 'FL_VoterFiles' and/or 'NY_VoterFiles'\n")
    cat("in your working directory:", getwd(), "\n")
    return(NULL)
  }
  
  cat("Found state folders:\n")
  for (folder in state_folders) {
    state <- str_extract(basename(folder), "^[A-Z]{2}")
    cat("  -", basename(folder), "\n")
    
    # Show year subfolders
    dir_struct <- get_directory_structure(folder)
    if (length(dir_struct) > 0) {
      for (year_info in dir_struct) {
        cat("    └── ", year_info$year_folder_name, " (", 
            length(year_info$zip_files), " zip files)\n", sep = "")
      }
    } else {
      cat("    └── No year folders found\n")
    }
  }
  cat("\n")
  
  # Process each state
  all_results <- list()
  for (state_folder in state_folders) {
    state_name <- str_extract(basename(state_folder), "^[A-Z]{2}")
    all_results[[state_name]] <- process_state_folder(state_folder)
  }
  
  # Generate summary
  generate_processing_summary(all_results)
  
  # Show where cleaned files are located
  find_cleaned_files()
  
  # Clean up any remaining temp files
  temp_dir <- file.path(config$base_path, config$unzip_temp)
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
    cat("Cleaned up temporary files\n\n")
  }
  
  cat("Pipeline completed at:", format(Sys.time()), "\n")
  cat(strrep("=", 70), "\n\n")
  
  return(all_results)
}

# =============================================================================
# HELPER FUNCTION FOR SELECTIVE PROCESSING
# =============================================================================

# Process only specific state and year
process_selective <- function(state = NULL, year = NULL) {
  cat("Processing selective files...\n")
  
  if (!is.null(state)) {
    state_folder <- file.path(config$base_path, paste0(state, "_VoterFiles"))
    
    if (!dir.exists(state_folder)) {
      cat("Error: State folder not found:", state_folder, "\n")
      return(NULL)
    }
    
    if (!is.null(year)) {
      # Process specific year only
      # Year folder is named like "FL_VoterFiles_2022"
      year_folder_name <- paste0(state, "_VoterFiles_", year)
      year_path <- file.path(state_folder, year_folder_name)
      
      if (!dir.exists(year_path)) {
        cat("Error: Year folder not found:", year_path, "\n")
        cat("Expected folder name:", year_folder_name, "\n")
        return(NULL)
      }
      
      zip_files <- list.files(year_path, pattern = "\\.zip$", full.names = TRUE)
      clean_path <- file.path(year_path, config$clean_folder_name)
      
      year_info <- list(
        year = year,
        year_folder_name = year_folder_name,
        year_path = year_path,
        zip_files = zip_files,
        clean_path = clean_path
      )
      
      state_temp <- file.path(config$base_path, config$unzip_temp, state)
      result <- process_year_folder(year_info, state, state_temp)
      unlink(state_temp, recursive = TRUE)
      
      return(result)
    } else {
      # Process all years for this state
      return(process_state_folder(state_folder))
    }
  } else {
    # Process all states
    return(main())
  }
}

# =============================================================================
# QUICK START EXAMPLES
# =============================================================================

# STEP 1: Run test mode first to verify cleaning on one file per state
# test_results <- run_test_mode()

# STEP 2: Review the output above and check the cleaned files

# STEP 3: If test looks good, process everything
# results <- main()

# Alternative: Process only specific state or year
# results_fl <- process_selective(state = "FL")
# results_fl_2022 <- process_selective(state = "FL", year = "2022")

# Check directory structure without processing
# dir_structure_fl <- get_directory_structure("/Volumes/easystore/FL_VoterFiles")
# str(dir_structure_fl)

# Multi-State Voter File Cleaning Pipeline
# For New York and Florida Voter Files
# Multi-State Voter File Cleaning Pipeline
# For New York and Florida Voter Files

# Load required packages
library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)

# Multi-State Voter File Cleaning Pipeline
# For New York and Florida Voter Files
clean_newyork_voters <- function(file_path) {
  cat("Processing New York voter file:", basename(file_path), "\n")
  
  # Read comma-delimited file with header
  df <- fread(file_path, sep = ",", quote = "\"", na.strings = c("", "NA", "BLK"), 
              header = TRUE, stringsAsFactors = FALSE)
  
  # Clean up column names (remove spaces, standardize)
  original_names <- names(df)
  names(df) <- str_replace_all(names(df), " ", "")  # Remove spaces
  names(df) <- toupper(names(df))
  
  cat("  Original columns found:", ncol(df), "\n")
  cat("  First few column names:", paste(names(df)[1:min(5, ncol(df))], collapse = ", "), "\n")
  
  # Determine which layout based on number of columns
  # 2018-2021: 42 columns (older format)
  # Pre-2022: 45 columns 
  # 2022+: 47 columns (new layout)
  
  if (ncol(df) == 47) {
    # NEW LAYOUT (2022+) - 47 fields
    cat("  Using NEW layout (2022+, 47 fields)\n")
    expected_cols <- c(
      "LASTNAME", "FIRSTNAME", "MIDDLENAME", "NAMESUFFIX",
      "RADDNUMBER", "RHALFCODE", "RPREDIRECTION", "RSTREETNAME", "RPOSTDIRECTION",
      "RAPARTMENTTYPE", "RAPARTMENT", "RADDRNONSTD", "RCITY", "RZIP5", "RZIP4",
      "MAILADD1", "MAILADD2", "MAILADD3", "MAILADD4",
      "DOB", "GENDER", "ENROLLMENT", "OTHERPARTY",
      "COUNTYCODE", "ED", "LD", "TOWNCITY", "WARD",
      "CD", "SD", "AD",
      "LASTVOTEDDATE", "PREVYEARVOTED", "PREVCOUNTY", "PREVADDRESS", "PREVNAME",
      "COUNTYVRNUMBER", "REGDATE", "VRSOURCE", "IDREQUIRED", "IDMET",
      "STATUS", "REASONCODE", "INACT_DATE", "PURGE_DATE",
      "SBOEID", "VOTERHISTORY"
    )
    setnames(df, expected_cols)
  } else if (ncol(df) == 45) {
    # MIDDLE LAYOUT (2019-2021) - 45 fields
    cat("  Using MIDDLE layout (2019-2021, 45 fields)\n")
    expected_cols <- c(
      "LASTNAME", "FIRSTNAME", "MIDDLENAME", "NAMESUFFIX",
      "RADDNUMBER", "RHALFCODE", "RAPARTMENT", "RPREDIRECTION",
      "RSTREETNAME", "RPOSTDIRECTION",# Multi-State Voter File Cleaning Pipeline
      # For New York and Florida Voter Files
  }
}

      # Load required packages
      library(tidyverse)
      library(data.table)
      library(lubridate)
      library(stringr)
      
      # =============================================================================
      # CONFIGURATION
      # =============================================================================
      
      # Base configuration
      config <- list(
        # UPDATE THIS PATH to your external drive location
        # On Mac, external drives are typically at /Volumes/[DriveName]
        base_path = "/Volumes/easystore",  # Path to external drive containing FL_VoterFiles and NY_VoterFiles
        
        states = c("FL", "NY"),  # States to process
        clean_folder_name = "cleaned_data",  # Name of subfolder for cleaned files
        unzip_temp = "temp_unzipped"  # Temporary folder for unzipped files
      )
      
      # Automatically detect state folders
      state_folders <- list.files(config$base_path, pattern = "^(FL|NY)_VoterFiles$", full.names = TRUE)
      cat("Detected state folders:", paste(state_folders, collapse = ", "), "\n")
      
      # =============================================================================
      # UTILITY FUNCTIONS
      # =============================================================================
      
      # Function to clean names
      clean_name <- function(x) {
        # Handle NA and empty values
        if (length(x) == 0) return(character(0))
        
        result <- x %>%
          as.character() %>%
          str_trim() %>%
          str_to_upper() %>%
          str_replace_all("[^A-Z\\s\\-']", "") %>%
          str_squish()
        
        # Replace empty strings with NA
        result[result == ""] <- NA_character_
        
        return(result)
      }
      
      # Function to standardize addresses
      clean_address <- function(x) {
        # Handle NA and empty values
        if (length(x) == 0) return(character(0))
        
        result <- x %>%
          as.character() %>%
          str_trim() %>%
          str_to_upper() %>%
          str_replace_all("\\s+", " ") %>%
          # Standardize common abbreviations
          str_replace_all("\\bSTREET\\b", "ST") %>%
          str_replace_all("\\bAVENUE\\b", "AVE") %>%
          str_replace_all("\\bROAD\\b", "RD") %>%
          str_replace_all("\\bBOULEVARD\\b", "BLVD") %>%
          str_replace_all("\\bDRIVE\\b", "DR") %>%
          str_replace_all("\\bLANE\\b", "LN") %>%
          str_replace_all("\\bCOURT\\b", "CT") %>%
          str_replace_all("\\bAPARTMENT\\b", "APT") %>%
          str_replace_all("\\bSUITE\\b", "STE")
        
        # Replace empty strings with NA
        result[result == ""] <- NA_character_
        
        return(result)
      }
      
      # Function to validate dates
      parse_date_safe <- function(x, format = "%m/%d/%Y") {
        parsed <- parse_date_time(x, orders = c("mdy", "ymd", "dmy"), quiet = TRUE)
        as.Date(parsed)
      }
      
      # Function to standardize party codes
      standardize_party <- function(x, state) {
        x <- str_trim(toupper(x))
        
        if (state == "FL") {
          # Florida party standardization
          case_when(
            x %in% c("DEM", "DEMOCRATIC") ~ "DEM",
            x %in% c("REP", "REPUBLICAN") ~ "REP",
            x == "NPA" ~ "NPA",
            x == "LPF" ~ "LIB",
            x == "GRE" ~ "GRN",
            TRUE ~ "OTH"
          )
        } else if (state == "NY") {
          # New York party standardization
          case_when(
            x %in% c("DEM", "DEMOCRATIC") ~ "DEM",
            x %in% c("REP", "REPUBLICAN") ~ "REP",
            x %in% c("IND", "INDEPENDENCE") ~ "IND",
            x %in% c("CON", "CONSERVATIVE") ~ "CON",
            x %in% c("WOR", "WORKING FAMILIES") ~ "WFP",
            x %in% c("GRE", "GREEN") ~ "GRN",
            x %in% c("LIB", "LIBERTARIAN") ~ "LIB",
            x == "BLANK" | is.na(x) ~ "UNENROLLED",
            TRUE ~ "OTH"
          )
        }
      }
      
      # =============================================================================
      # FLORIDA DATA CLEANING
      # =============================================================================
      
      clean_florida_registration <- function(file_path) {
        cat("Processing Florida registration file:", basename(file_path), "\n")
        
        # Read tab-delimited file
        df <- fread(file_path, sep = "\t", quote = "", na.strings = c("", "NA"))
        
        # Expected column structure (based on FL layout)
        expected_cols <- c(
          "CountyCode", "VoterID", "NameLast", "NameSuffix", "NameFirst", 
          "NameMiddle", "Exemption", "ResidenceAddressLine1", "ResidenceAddressLine2",
          "ResidenceCity", "ResidenceState", "ResidenceZipcode",
          "MailingAddressLine1", "MailingAddressLine2", "MailingAddressLine3",
          "MailingCity", "MailingState", "MailingZipcode", "MailingCountry",
          "Gender", "Race", "BirthDate", "RegistrationDate", "PartyAffiliation",
          "Precinct", "PrecinctGroup", "PrecinctSplit", "PrecinctSuffix",
          "VoterStatus", "CongressionalDistrict", "HouseDistrict", "SenateDistrict",
          "CountyCommissionDistrict", "SchoolBoardDistrict",
          "DaytimeAreaCode", "DaytimePhoneNumber", "DaytimePhoneExtension", "EmailAddress"
        )
        
        # Set column names if not present
        if (ncol(df) == length(expected_cols)) {
          setnames(df, expected_cols)
        }
        
        # Clean and standardize
        cleaned <- df %>%
          mutate(
            # Clean names
            NameLast = clean_name(NameLast),
            NameFirst = clean_name(NameFirst),
            NameMiddle = clean_name(NameMiddle),
            NameSuffix = clean_name(NameSuffix),
            
            # Full name for matching
            FullName = paste(NameFirst, NameMiddle, NameLast, NameSuffix) %>% 
              str_squish(),
            
            # Clean addresses
            ResidenceAddressLine1 = clean_address(ResidenceAddressLine1),
            ResidenceCity = str_to_upper(str_trim(ResidenceCity)),
            MailingAddressLine1 = clean_address(MailingAddressLine1),
            MailingCity = str_to_upper(str_trim(MailingCity)),
            
            # Standardize zip codes (keep first 5 digits)
            ResidenceZipcode = str_sub(str_trim(ResidenceZipcode), 1, 5),
            MailingZipcode = str_sub(str_trim(MailingZipcode), 1, 5),
            
            # Parse dates
            BirthDate = parse_date_safe(BirthDate),
            RegistrationDate = parse_date_safe(RegistrationDate),
            
            # Calculate age as of today
            Age = as.numeric(difftime(Sys.Date(), BirthDate, units = "days")) / 365.25,
            
            # Standardize categorical variables
            Gender = toupper(str_trim(Gender)),
            Gender = case_when(
              Gender %in% c("F", "FEMALE") ~ "F",
              Gender %in% c("M", "MALE") ~ "M",
              TRUE ~ "U"
            ),
            
            VoterStatus = toupper(str_trim(VoterStatus)),
            PartyAffiliation = standardize_party(PartyAffiliation, "FL"),
            
            # Create active voter flag
            IsActive = VoterStatus == "ACT",
            
            # State identifier
            State = "FL"
          ) %>%
          # Remove records with missing critical data
          filter(
            !is.na(VoterID),
            !is.na(NameLast),
            !is.na(NameFirst)
          ) %>%
          # Flag potential duplicates
          group_by(FullName, BirthDate, ResidenceZipcode) %>%
          mutate(
            PotentialDuplicate = n() > 1,
            DuplicateCount = n()
          ) %>%
          ungroup()
        
        return(cleaned)
      }
      
      clean_florida_history <- function(file_path) {
        cat("Processing Florida history file:", basename(file_path), "\n")
        
        df <- fread(file_path, sep = "\t", quote = "", na.strings = c("", "NA"))
        
        # Expected columns for history file
        expected_cols <- c("CountyCode", "VoterID", "ElectionDate", "ElectionType", "HistoryCode")
        
        if (ncol(df) == length(expected_cols)) {
          setnames(df, expected_cols)
        }
        
        cleaned <- df %>%
          mutate(
            ElectionDate = parse_date_safe(ElectionDate),
            ElectionYear = year(ElectionDate),
            ElectionType = toupper(str_trim(ElectionType)),
            HistoryCode = toupper(str_trim(HistoryCode)),
            
            # Decode voting method
            VotingMethod = case_when(
              HistoryCode == "A" ~ "Absentee/Mail",
              HistoryCode == "E" ~ "Early Voting",
              HistoryCode == "Y" ~ "Polling Place",
              HistoryCode == "P" ~ "Provisional (Not Counted)",
              HistoryCode == "B" ~ "Mail Ballot (Not Counted)",
              HistoryCode == "L" ~ "Mail Late (Not Counted)",
              HistoryCode == "N" ~ "Did Not Vote",
              TRUE ~ "Unknown"
            ),
            
            # Flag whether vote was counted
            VoteCounted = !HistoryCode %in% c("B", "L", "P", "N"),
            
            State = "FL"
          ) %>%
          filter(!is.na(VoterID), !is.na(ElectionDate))
        
        return(cleaned)
      }
      
      # =============================================================================
      # NEW YORK DATA CLEANING (Updated for actual NY structure)
      # =============================================================================
      
      clean_newyork_voters <- function(file_path) {
        cat("Processing New York voter file:", basename(file_path), "\n")
        
        # Read comma-delimited file
        df <- fread(file_path, sep = ",", quote = "\"", na.strings = c("", "NA", "BLK"), 
                    header = TRUE, stringsAsFactors = FALSE)
        
        cat("  Original columns found:", ncol(df), "\n")
        
        # Determine which layout based on number of columns
        # Old layout (pre-2022): 45 columns
        # New layout (2022+): 47 columns
        
        if (ncol(df) == 47) {
          # NEW LAYOUT (2022+) - 47 fields
          cat("  Using NEW layout (2022+, 47 fields)\n")
          expected_cols <- c(
            "LASTNAME", "FIRSTNAME", "MIDDLENAME", "NAMESUFFIX",
            "RADDNUMBER", "RHALFCODE", "RPREDIRECTION", "RSTREETNAME", "RPOSTDIRECTION",
            "RAPARTMENTTYPE", "RAPARTMENT", "RADDRNONSTD", "RCITY", "RZIP5", "RZIP4",
            "MAILADD1", "MAILADD2", "MAILADD3", "MAILADD4",
            "DOB", "GENDER", "ENROLLMENT", "OTHERPARTY",
            "COUNTYCODE", "ED", "LD", "TOWNCITY", "WARD",
            "CD", "SD", "AD",
            "LASTVOTEDDATE", "PREVYEARVOTED", "PREVCOUNTY", "PREVADDRESS", "PREVNAME",
            "COUNTYVRNUMBER", "REGDATE", "VRSOURCE", "IDREQUIRED", "IDMET",
            "STATUS", "REASONCODE", "INACT_DATE", "PURGE_DATE",
            "SBOEID", "VoterHistory"
          )
          setnames(df, expected_cols)
        } else if (ncol(df) == 45) {
          # OLD LAYOUT (pre-2022) - 45 fields
          cat("  Using OLD layout (pre-2022, 45 fields)\n")
          expected_cols <- c(
            "LASTNAME", "FIRSTNAME", "MIDDLENAME", "NAMESUFFIX",
            "RADDNUMBER", "RHALFCODE", "RAPARTMENT", "RPREDIRECTION",
            "RSTREETNAME", "RPOSTDIRECTION", "RCITY", "RZIP5", "RZIP4",
            "MAILADD1", "MAILADD2", "MAILADD3", "MAILADD4",
            "DOB", "GENDER", "ENROLLMENT", "OTHERPARTY",
            "COUNTYCODE", "ED", "LD", "TOWNCITY", "WARD",
            "CD", "SD", "AD",
            "LASTVOTEDDATE", "PREVYEARVOTED", "PREVCOUNTY", "PREVADDRESS", "PREVNAME",
            "COUNTYVRNUMBER", "REGDATE", "VRSOURCE", "IDREQUIRED", "IDMET",
            "STATUS", "REASONCODE", "INACT_DATE", "PURGE_DATE",
            "SBOEID", "VoterHistory"
          )
          setnames(df, expected_cols)
        } else {
          error_msg <- paste0("ERROR: Unexpected number of columns: ", ncol(df), 
                              ". Expected 45 (old layout) or 47 (new layout).",
                              "\nFirst few original column names: ", 
                              paste(names(df)[1:min(5, ncol(df))], collapse = ", "))
          cat(error_msg, "\n")
          stop(error_msg)
        }
        
        # Verify critical columns exist before processing
        critical_cols <- c("LASTNAME", "FIRSTNAME", "SBOEID", "DOB")
        missing_cols <- setdiff(critical_cols, names(df))
        if (length(missing_cols) > 0) {
          error_msg <- paste0("ERROR: Missing critical columns after setting names: ",
                              paste(missing_cols, collapse = ", "))
          cat(error_msg, "\n")
          stop(error_msg)
        }
        
        # Build address based on layout
        if (ncol(df) == 47) {
          # New layout has RAPARTMENTTYPE and RADDRNONSTD
          cleaned <- df %>%
            mutate(
              # Clean names - handle NA values
              LASTNAME = ifelse(is.na(LASTNAME), "", clean_name(LASTNAME)),
              FIRSTNAME = ifelse(is.na(FIRSTNAME), "", clean_name(FIRSTNAME)),
              MIDDLENAME = ifelse(is.na(MIDDLENAME), "", clean_name(MIDDLENAME)),
              NAMESUFFIX = ifelse(is.na(NAMESUFFIX), "", clean_name(NAMESUFFIX)),
              
              # Full name for matching
              FullName = paste(FIRSTNAME, MIDDLENAME, LASTNAME, NAMESUFFIX) %>% str_squish(),
              
              # Build full residence address (use non-standard if present)
              ResidenceAddress = ifelse(
                !is.na(RADDRNONSTD) & RADDRNONSTD != "",
                as.character(RADDRNONSTD),
                paste(RADDNUMBER, RHALFCODE, RPREDIRECTION, RSTREETNAME, RPOSTDIRECTION,
                      RAPARTMENTTYPE, RAPARTMENT) %>% str_squish()
              ),
              ResidenceAddress = clean_address(ResidenceAddress),
              
              RCITY = str_to_upper(str_trim(as.character(RCITY))),
              
              # Standardize zip codes
              RZIP5 = str_sub(str_trim(as.character(RZIP5)), 1, 5),
              
              # Parse dates (format: YYYYMMDD)
              DOB = parse_date_safe(as.character(DOB), format = "%Y%m%d"),
              REGDATE = parse_date_safe(as.character(REGDATE), format = "%Y%m%d"),
              LASTVOTEDDATE = parse_date_safe(as.character(LASTVOTEDDATE), format = "%Y%m%d"),
              INACT_DATE = parse_date_safe(as.character(INACT_DATE), format = "%Y%m%d"),
              PURGE_DATE = parse_date_safe(as.character(PURGE_DATE), format = "%Y%m%d"),
              
              # Calculate age
              Age = as.numeric(difftime(Sys.Date(), DOB, units = "days")) / 365.25,
              
              # Standardize gender
              GENDER = toupper(str_trim(as.character(GENDER))),
              GENDER = case_when(
                GENDER == "M" ~ "M",
                GENDER == "F" ~ "F",
                TRUE ~ "U"
              ),
              
              # Standardize party enrollment
              PartyEnrollment = standardize_party(as.character(ENROLLMENT), "NY"),
              
              # Voter status
              STATUS = toupper(str_trim(as.character(STATUS))),
              IsActive = STATUS %in% c("A", "AM", "AF", "AP", "AU"),
              
              # Parse voting history (semicolon-separated)
              VotingHistoryRaw = as.character(VoterHistory),
              ElectionsVoted = ifelse(is.na(VoterHistory) | VoterHistory == "", 
                                      0, 
                                      str_count(as.character(VoterHistory), ";") + 1),
              
              # State identifier
              State = "NY"
            )
        } else {
          # Old layout (45 columns)
          cleaned <- df %>%
            mutate(
              # Clean names - now clean_name handles vectors properly
              LASTNAME = clean_name(LASTNAME),
              FIRSTNAME = clean_name(FIRSTNAME),
              MIDDLENAME = clean_name(MIDDLENAME),
              NAMESUFFIX = clean_name(NAMESUFFIX),
              
              # Full name for matching
              FullName = paste(
                ifelse(is.na(FIRSTNAME), "", FIRSTNAME),
                ifelse(is.na(MIDDLENAME), "", MIDDLENAME),
                ifelse(is.na(LASTNAME), "", LASTNAME),
                ifelse(is.na(NAMESUFFIX), "", NAMESUFFIX)
              ) %>% str_squish(),
              
              # Build full residence address
              ResidenceAddress = paste(
                ifelse(is.na(RADDNUMBER), "", RADDNUMBER),
                ifelse(is.na(RHALFCODE), "", RHALFCODE),
                ifelse(is.na(RAPARTMENT), "", RAPARTMENT),
                ifelse(is.na(RPREDIRECTION), "", RPREDIRECTION),
                ifelse(is.na(RSTREETNAME), "", RSTREETNAME),
                ifelse(is.na(RPOSTDIRECTION), "", RPOSTDIRECTION)
              ) %>% str_squish(),
              ResidenceAddress = clean_address(ResidenceAddress),
              
              RCITY = str_to_upper(str_trim(as.character(RCITY))),
              
              # Standardize zip codes
              RZIP5 = str_sub(str_trim(as.character(RZIP5)), 1, 5),
              
              # Parse dates (format: YYYYMMDD)
              DOB = parse_date_safe(as.character(DOB), format = "%Y%m%d"),
              REGDATE = parse_date_safe(as.character(REGDATE), format = "%Y%m%d"),
              LASTVOTEDDATE = parse_date_safe(as.character(LASTVOTEDDATE), format = "%Y%m%d"),
              INACT_DATE = parse_date_safe(as.character(INACT_DATE), format = "%Y%m%d"),
              PURGE_DATE = parse_date_safe(as.character(PURGE_DATE), format = "%Y%m%d"),
              
              # Calculate age
              Age = as.numeric(difftime(Sys.Date(), DOB, units = "days")) / 365.25,
              
              # Standardize gender
              GENDER = toupper(str_trim(as.character(GENDER))),
              GENDER = case_when(
                GENDER == "M" ~ "M",
                GENDER == "F" ~ "F",
                TRUE ~ "U"
              ),
              
              # Standardize party enrollment
              PartyEnrollment = standardize_party(as.character(ENROLLMENT), "NY"),
              
              # Voter status
              STATUS = toupper(str_trim(as.character(STATUS))),
              IsActive = STATUS %in% c("A", "AM", "AF", "AP", "AU"),
              
              # Parse voting history (semicolon-separated)
              VotingHistoryRaw = as.character(VoterHistory),
              ElectionsVoted = ifelse(is.na(VoterHistory) | VoterHistory == "", 
                                      0, 
                                      str_count(as.character(VoterHistory), ";") + 1),
              
              # State identifier
              State = "NY"
            )
        }
        
        # Common filtering and duplicate detection
        cleaned <- cleaned %>%
          # Remove records with missing critical data
          filter(
            !is.na(SBOEID) & SBOEID != "",
            !is.na(LASTNAME) & LASTNAME != "",
            !is.na(FIRSTNAME) & FIRSTNAME != ""
          ) %>%
          # Flag potential duplicates
          group_by(FullName, DOB, RZIP5) %>%
          mutate(
            PotentialDuplicate = n() > 1,
            DuplicateCount = n()
          ) %>%
          ungroup()
        
        return(cleaned)
      }
      
      # =============================================================================
      # FILE ORGANIZATION AND UNZIPPING FUNCTIONS
      # =============================================================================
      
      # Function to get directory structure
      get_directory_structure <- function(state_folder) {
        state <- str_extract(basename(state_folder), "^[A-Z]{2}")
        
        # Look for folders like FL_VoterFiles_2021, FL_VoterFiles_2022, etc.
        year_pattern <- paste0("^", state, "_VoterFiles_\\d{4}$")
        years <- list.dirs(state_folder, full.names = TRUE, recursive = FALSE) %>%
          .[str_detect(basename(.), year_pattern)]
        
        structure <- lapply(years, function(year_path) {
          year_name <- basename(year_path)
          # Extract just the year (e.g., "2021" from "FL_VoterFiles_2021")
          year_num <- str_extract(year_name, "\\d{4}$")
          
          zip_files <- list.files(year_path, pattern = "\\.zip$", full.names = TRUE)
          
          list(
            year = year_num,
            year_folder_name = year_name,
            year_path = year_path,
            zip_files = zip_files,
            clean_path = file.path(year_path, config$clean_folder_name)
          )
        })
        
        names(structure) <- sapply(structure, function(x) x$year)
        return(structure)
      }
      
      # Function to unzip and process a single zip file
      unzip_and_extract <- function(zip_path, temp_dir) {
        cat("  Unzipping:", basename(zip_path), "\n")
        
        # Create temp directory for this zip
        zip_temp <- file.path(temp_dir, tools::file_path_sans_ext(basename(zip_path)))
        dir.create(zip_temp, showWarnings = FALSE, recursive = TRUE)
        
        # Unzip with better error handling
        tryCatch({
          # Suppress warnings and capture them
          withCallingHandlers(
            {
              unzip(zip_path, exdir = zip_temp)
            },
            warning = function(w) {
              if (grepl("corrupt", w$message, ignore.case = TRUE)) {
                cat("    WARNING: Zip file may be corrupt:", basename(zip_path), "\n")
                cat("    Attempting to continue with partial extraction...\n")
              }
              invokeRestart("muffleWarning")
            }
          )
          
          # Return paths to extracted files
          extracted_files <- list.files(zip_temp, pattern = "\\.(txt|csv)$", 
                                        full.names = TRUE, recursive = TRUE)
          
          if (length(extracted_files) == 0) {
            cat("    ERROR: No files extracted from:", basename(zip_path), "\n")
            return(NULL)
          }
          
          return(list(
            zip_file = zip_path,
            extracted_files = extracted_files,
            temp_dir = zip_temp
          ))
        }, error = function(e) {
          cat("    Error unzipping:", e$message, "\n")
          cat("    File:", basename(zip_path), "\n")
          return(NULL)
        })
      }
      
      # Function to determine file type (registration vs history for FL)
      classify_florida_file <- function(filename) {
        base_name <- basename(filename)
        
        # Florida naming: YYYY.MM.DD_VoterDetail.zip and YYYY.MM.DD_VoterHistory.zip
        if (str_detect(base_name, "VoterHistory|_VoterHistory|History")) {
          return("history")
        } else if (str_detect(base_name, "VoterDetail|_VoterDetail|Detail|Registration")) {
          return("registration")
        } else {
          # If unclear, try to determine from content after unzipping
          return("unknown")
        }
      }
      
      classify_by_content <- function(filepath) {
        # Read first few lines to check column structure
        tryCatch({
          first_lines <- readLines(filepath, n = 2)
          header <- first_lines[1]
          
          # Florida history has fewer columns (5): CountyCode, VoterID, ElectionDate, ElectionType, HistoryCode
          # Florida registration has many columns (38)
          
          col_count <- length(strsplit(header, "\t")[[1]])
          
          if (col_count <= 10) {
            return("history")
          } else {
            return("registration")
          }
        }, error = function(e) {
          return("unknown")
        })
      }
      
      # Function to get output filename
      get_output_filename <- function(original_zip_path, file_type = NULL) {
        base_name <- tools::file_path_sans_ext(basename(original_zip_path))
        
        # Remove "VoterDetail" or "VoterHistory" from the name if present
        # e.g., "2022.02.14_VoterDetail" -> "2022.02.14"
        base_name <- str_replace(base_name, "_VoterDetail$|_VoterHistory$", "")
        
        if (!is.null(file_type) && file_type == "history") {
          output_name <- paste0(base_name, "_history_clean.csv")
        } else if (!is.null(file_type) && file_type == "registration") {
          output_name <- paste0(base_name, "_registration_clean.csv")
        } else {
          output_name <- paste0(base_name, "_clean.csv")
        }
        
        return(output_name)
      }
      
      # =============================================================================
      # AUTOMATED PROCESSING FUNCTIONS
      # =============================================================================
      
      process_single_zip <- function(zip_info, state, year_clean_path) {
        if (is.null(zip_info)) return(NULL)
        
        cat("\n  Processing zip:", basename(zip_info$zip_file), "\n")
        
        # Create cleaned data folder if it doesn't exist
        dir.create(year_clean_path, showWarnings = FALSE, recursive = TRUE)
        
        results <- list()
        
        if (state == "FL") {
          # Classify the zip file by name first
          zip_type <- classify_florida_file(zip_info$zip_file)
          
          # Process files based on type
          for (file in zip_info$extracted_files) {
            # If zip type is unknown, check file content
            if (zip_type == "unknown") {
              file_type <- classify_by_content(file)
            } else {
              file_type <- zip_type
            }
            
            tryCatch({
              if (file_type == "history") {
                cleaned_data <- clean_florida_history(file)
                output_name <- get_output_filename(zip_info$zip_file, "history")
                output_path <- file.path(year_clean_path, output_name)
                fwrite(cleaned_data, output_path)
                cat("    Saved history to:", basename(output_path), "\n")
                results$history <- cleaned_data
                
              } else if (file_type == "registration") {
                cleaned_data <- clean_florida_registration(file)
                output_name <- get_output_filename(zip_info$zip_file, "registration")
                output_path <- file.path(year_clean_path, output_name)
                fwrite(cleaned_data, output_path)
                cat("    Saved registration to:", basename(output_path), "\n")
                results$registration <- cleaned_data
                
              } else {
                cat("    Warning: Could not determine file type for:", basename(file), "\n")
              }
            }, error = function(e) {
              cat("    Error processing file:", e$message, "\n")
            })
          }
          
        } else if (state == "NY") {
          # Process NY voter files
          for (voter_file in zip_info$extracted_files) {
            tryCatch({
              cleaned_voters <- clean_newyork_voters(voter_file)
              output_name <- get_output_filename(zip_info$zip_file)
              output_path <- file.path(year_clean_path, output_name)
              fwrite(cleaned_voters, output_path)
              cat("    Saved to:", basename(output_path), "\n")
              results$voters <- cleaned_voters
            }, error = function(e) {
              cat("    Error processing NY file:", e$message, "\n")
            })
          }
        }
        
        return(results)
      }
      
      process_year_folder <- function(year_info, state, temp_base) {
        cat("\nProcessing year folder:", year_info$year_folder_name, "\n")
        cat("Found", length(year_info$zip_files), "zip files\n")
        
        if (length(year_info$zip_files) == 0) {
          cat("  No zip files found, skipping...\n")
          return(NULL)
        }
        
        # Show what files were found
        cat("  Files:\n")
        for (zf in basename(year_info$zip_files)) {
          cat("    -", zf, "\n")
        }
        
        # Create year-specific temp directory
        year_temp <- file.path(temp_base, year_info$year)
        dir.create(year_temp, showWarnings = FALSE, recursive = TRUE)
        
        # Process each zip file
        all_results <- list()
        
        for (zip_file in year_info$zip_files) {
          # Unzip
          zip_info <- unzip_and_extract(zip_file, year_temp)
          
          # Process extracted files
          if (!is.null(zip_info) && length(zip_info$extracted_files) > 0) {
            result <- process_single_zip(zip_info, state, year_info$clean_path)
            all_results[[basename(zip_file)]] <- result
          }
        }
        
        # Clean up temp directory for this year
        unlink(year_temp, recursive = TRUE)
        
        return(all_results)
      }
      
      process_state_folder <- function(state_folder) {
        state <- str_extract(basename(state_folder), "^[A-Z]{2}")
        cat("\n", strrep("=", 70), "\n")
        cat("PROCESSING STATE:", state, "\n")
        cat(strrep("=", 70), "\n")
        
        # Get directory structure
        dir_structure <- get_directory_structure(state_folder)
        
        if (length(dir_structure) == 0) {
          cat("No year folders found in", state_folder, "\n")
          return(NULL)
        }
        
        # Create temp directory for this state
        state_temp <- file.path(config$base_path, config$unzip_temp, state)
        dir.create(state_temp, showWarnings = FALSE, recursive = TRUE)
        
        # Process each year
        state_results <- list()
        for (year_name in names(dir_structure)) {
          year_results <- process_year_folder(dir_structure[[year_name]], state, state_temp)
          state_results[[year_name]] <- year_results
        }
        
        # Clean up state temp directory
        unlink(state_temp, recursive = TRUE)
        
        cat("\n", strrep("=", 70), "\n")
        cat("COMPLETED STATE:", state, "\n")
        cat(strrep("=", 70), "\n\n")
        
        return(state_results)
      }
      
      # =============================================================================
      # NEW YORK SPECIFIC PROCESSING (separate from FL)
      # =============================================================================
      
      # Function to get NY directory structure (different naming pattern)
      get_ny_directory_structure <- function(state_folder) {
        # Look for folders like NY_VoterFiles_2018, NY_VoterFiles_2019, etc.
        year_pattern <- "^NY_VoterFiles_\\d{4}$"
        years <- list.dirs(state_folder, full.names = TRUE, recursive = FALSE) %>%
          .[str_detect(basename(.), year_pattern)]
        
        structure <- lapply(years, function(year_path) {
          year_name <- basename(year_path)
          year_num <- str_extract(year_name, "\\d{4}$")
          
          # Look for AllNYSVoters_*.zip files (note: no underscore between NYS and Voters)
          zip_files <- list.files(year_path, pattern = "^AllNYSVoters_.*\\.zip$", full.names = TRUE)
          
          list(
            year = year_num,
            year_folder_name = year_name,
            year_path = year_path,
            zip_files = zip_files,
            clean_path = file.path(year_path, config$clean_folder_name)
          )
        })
        
        names(structure) <- sapply(structure, function(x) x$year)
        return(structure)
      }
      
      # Process a single NY zip file
      process_ny_zip <- function(zip_info, year_clean_path) {
        if (is.null(zip_info)) return(NULL)
        
        cat("\n  Processing zip:", basename(zip_info$zip_file), "\n")
        
        # Create cleaned data folder if it doesn't exist
        dir.create(year_clean_path, showWarnings = FALSE, recursive = TRUE)
        
        results <- list()
        
        # Process NY voter files (should be .txt files inside)
        for (voter_file in zip_info$extracted_files) {
          tryCatch({
            cleaned_voters <- clean_newyork_voters(voter_file)
            
            # Get output filename based on zip name
            # AllNYSVoters_2019.01.09.zip -> AllNYSVoters_2019.01.09_clean.csv
            output_name <- get_output_filename(zip_info$zip_file)
            output_path <- file.path(year_clean_path, output_name)
            
            fwrite(cleaned_voters, output_path)
            cat("    Saved to:", basename(output_path), "\n")
            cat("    Records:", format(nrow(cleaned_voters), big.mark = ","), "\n")
            
            results$voters <- cleaned_voters
          }, error = function(e) {
            error_msg <- paste0("CRITICAL ERROR processing NY file: ", e$message, 
                                "\nFile: ", voter_file)
            cat("\n", strrep("!", 70), "\n")
            cat(error_msg, "\n")
            cat(strrep("!", 70), "\n\n")
            stop(error_msg)
          })
        }
        
        return(results)
      }
      
      # Process NY year folder
      process_ny_year_folder <- function(year_info, temp_base) {
        cat("\nProcessing NY year folder:", year_info$year_folder_name, "\n")
        cat("Found", length(year_info$zip_files), "zip files\n")
        
        # Check if cleaned_data folder already exists and has files
        if (dir.exists(year_info$clean_path)) {
          existing_files <- list.files(year_info$clean_path, pattern = "_clean\\.csv$")
          if (length(existing_files) > 0) {
            cat("  ** Cleaned data already exists (", length(existing_files), 
                " files). Skipping this year. **\n", sep = "")
            return(NULL)
          }
        }
        
        if (length(year_info$zip_files) == 0) {
          cat("  No zip files found, skipping...\n")
          return(NULL)
        }
        
        # Show what files were found
        cat("  Files:\n")
        for (zf in basename(year_info$zip_files)) {
          cat("    -", zf, "\n")
        }
        
        # Create year-specific temp directory
        year_temp <- file.path(temp_base, year_info$year)
        dir.create(year_temp, showWarnings = FALSE, recursive = TRUE)
        
        # Process each zip file
        all_results <- list()
        
        for (zip_file in year_info$zip_files) {
          # Unzip
          zip_info <- unzip_and_extract(zip_file, year_temp)
          
          # Process extracted files
          if (!is.null(zip_info) && length(zip_info$extracted_files) > 0) {
            result <- process_ny_zip(zip_info, year_info$clean_path)
            all_results[[basename(zip_file)]] <- result
          }
        }
        
        # Clean up temp directory for this year
        unlink(year_temp, recursive = TRUE)
        
        return(all_results)
      }
      
      # Main NY processing function
      process_ny_only <- function() {
        cat("\n", strrep("=", 70), "\n")
        cat("PROCESSING NEW YORK ONLY\n")
        cat(strrep("=", 70), "\n\n")
        
        ny_folder <- file.path(config$base_path, "NY_VoterFiles")
        
        if (!dir.exists(ny_folder)) {
          cat("ERROR: NY_VoterFiles folder not found at:", ny_folder, "\n")
          return(NULL)
        }
        
        # Get directory structure
        dir_struct <- get_ny_directory_structure(ny_folder)
        
        if (length(dir_struct) == 0) {
          cat("No NY year folders found\n")
          return(NULL)
        }
        
        cat("Found NY year folders:\n")
        for (year_info in dir_struct) {
          cat("  -", year_info$year_folder_name, "(", 
              length(year_info$zip_files), "zip files)\n")
        }
        cat("\n")
        
        # Create temp directory
        ny_temp <- file.path(config$base_path, config$unzip_temp, "NY")
        dir.create(ny_temp, showWarnings = FALSE, recursive = TRUE)
        
        # Process each year
        ny_results <- list()
        for (year_name in names(dir_struct)) {
          year_results <- process_ny_year_folder(dir_struct[[year_name]], ny_temp)
          ny_results[[year_name]] <- year_results
        }
        
        # Clean up temp directory
        unlink(ny_temp, recursive = TRUE)
        
        # Generate summary
        cat("\n", strrep("=", 70), "\n")
        cat("NY PROCESSING SUMMARY\n")
        cat(strrep("=", 70), "\n\n")
        
        for (year in names(ny_results)) {
          cat("  Year:", year, "\n")
          year_data <- ny_results[[year]]
          
          if (is.null(year_data)) {
            cat("    Skipped or no files\n")
          } else {
            cat("    Files processed:", length(year_data), "\n")
            
            total_records <- 0
            for (zip_name in names(year_data)) {
              zip_data <- year_data[[zip_name]]
              if (!is.null(zip_data$voters) && is.data.frame(zip_data$voters)) {
                total_records <- total_records + nrow(zip_data$voters)
              }
            }
            cat("    Total records:", format(total_records, big.mark = ","), "\n")
          }
        }
        
        cat("\n", strrep("=", 70), "\n")
        cat("NY PROCESSING COMPLETE\n")
        cat(strrep("=", 70), "\n\n")
        
        return(ny_results)
      }
      
      # Function to run test on one file per state
      run_test_mode <- function() {
        cat("\n", strrep("=", 70), "\n")
        cat("TEST MODE - Processing one file per state\n")
        cat(strrep("=", 70), "\n\n")
        
        # Find all state folders
        state_folders <- list.files(config$base_path, 
                                    pattern = "^(FL|NY)_VoterFiles$", 
                                    full.names = TRUE)
        
        if (length(state_folders) == 0) {
          cat("ERROR: No state folders found!\n")
          return(NULL)
        }
        
        test_results <- list()
        
        for (state_folder in state_folders) {
          state <- str_extract(basename(state_folder), "^[A-Z]{2}")
          cat("\n", strrep("-", 70), "\n")
          cat("Testing", state, "\n")
          cat(strrep("-", 70), "\n")
          
          # Get directory structure
          dir_struct <- get_directory_structure(state_folder)
          
          if (length(dir_struct) == 0) {
            cat("No year folders found for", state, "\n")
            next
          }
          
          # Find first year with zip files
          test_year_info <- NULL
          for (year_info in dir_struct) {
            if (length(year_info$zip_files) > 0) {
              test_year_info <- year_info
              break
            }
          }
          
          if (is.null(test_year_info)) {
            cat("No zip files found for", state, "\n")
            next
          }
          
          cat("Selected test file from:", test_year_info$year_folder_name, "\n")
          
          # For Florida, try to get one registration and one history file
          if (state == "FL") {
            detail_file <- NULL
            history_file <- NULL
            
            for (zip_file in test_year_info$zip_files) {
              if (is.null(detail_file) && str_detect(basename(zip_file), "VoterDetail|Detail")) {
                detail_file <- zip_file
              }
              if (is.null(history_file) && str_detect(basename(zip_file), "VoterHistory|History")) {
                history_file <- zip_file
              }
              if (!is.null(detail_file) && !is.null(history_file)) break
            }
            
            # Use first file if we couldn't find specific types
            if (is.null(detail_file)) detail_file <- test_year_info$zip_files[1]
            
            test_files <- c(detail_file, history_file)
            test_files <- test_files[!is.na(test_files) & !is.null(test_files)]
            
          } else {
            # For NY, just get first file
            test_files <- test_year_info$zip_files[1]
          }
          
          cat("Test file(s):\n")
          for (tf in test_files) {
            cat("  -", basename(tf), "\n")
          }
          
          # Create test temp directory
          test_temp <- file.path(config$base_path, config$unzip_temp, "test", state)
          dir.create(test_temp, showWarnings = FALSE, recursive = TRUE)
          
          # Process test files
          state_test_results <- list()
          
          for (test_file in test_files) {
            cat("\nProcessing:", basename(test_file), "\n")
            
            # Unzip
            zip_info <- unzip_and_extract(test_file, test_temp)
            
            if (!is.null(zip_info) && length(zip_info$extracted_files) > 0) {
              # Process
              result <- process_single_zip(zip_info, state, test_year_info$clean_path)
              state_test_results[[basename(test_file)]] <- result
              
              # Show preview of cleaned data
              if (!is.null(result)) {
                for (data_type in names(result)) {
                  if (is.data.frame(result[[data_type]])) {
                    cat("\n--- Preview of", data_type, "data ---\n")
                    cat("Dimensions:", nrow(result[[data_type]]), "rows x", 
                        ncol(result[[data_type]]), "columns\n")
                    cat("\nFirst few rows:\n")
                    print(head(result[[data_type]], 3))
                    cat("\nColumn names:\n")
                    print(names(result[[data_type]]))
                    cat("\n")
                  }
                }
              }
            }
          }
          
          test_results[[state]] <- state_test_results
          
          # Clean up test temp
          unlink(test_temp, recursive = TRUE)
        }
        
        cat("\n", strrep("=", 70), "\n")
        cat("TEST MODE COMPLETE\n")
        cat(strrep("=", 70), "\n\n")
        
        cat("Review the output above to verify the data is being cleaned correctly.\n")
        cat("Check the cleaned_data folders in each year directory to see the output files.\n\n")
        
        cat("If everything looks good, run: results <- main()\n")
        cat("To process all files for all states.\n\n")
        
        return(test_results)
      }
      
      # =============================================================================
      # DATA QUALITY CHECKS
      # =============================================================================
      
      run_quality_checks <- function(df, state_name = "Unknown") {
        cat("\n=== Quality Checks for", state_name, "===\n")
        
        # Total records
        cat("Total records:", nrow(df), "\n")
        
        # Check for missing critical fields
        cat("\nMissing data summary:\n")
        missing_summary <- df %>%
          summarise(across(everything(), ~sum(is.na(.)))) %>%
          pivot_longer(everything(), names_to = "Field", values_to = "MissingCount") %>%
          filter(MissingCount > 0) %>%
          arrange(desc(MissingCount))
        
        if (nrow(missing_summary) > 0) {
          print(missing_summary)
        } else {
          cat("No missing data found!\n")
        }
        
        # Check for duplicates
        if ("PotentialDuplicate" %in% names(df)) {
          cat("\nPotential duplicates:", sum(df$PotentialDuplicate, na.rm = TRUE), "\n")
        }
        
        # Age distribution
        if ("Age" %in% names(df)) {
          cat("\nAge distribution:\n")
          print(summary(df$Age))
          cat("Voters under 18:", sum(df$Age < 18, na.rm = TRUE), "\n")
          cat("Voters over 100:", sum(df$Age > 100, na.rm = TRUE), "\n")
        }
        
        # Party distribution
        party_col <- names(df)[str_detect(names(df), "Party")][1]
        if (length(party_col) > 0 && !is.na(party_col)) {
          cat("\nParty distribution:\n")
          print(table(df[[party_col]], useNA = "ifany"))
        }
        
        # Voter status (if available)
        if ("VoterStatus" %in% names(df) || "IsActive" %in% names(df)) {
          status_col <- ifelse("IsActive" %in% names(df), "IsActive", "VoterStatus")
          cat("\nVoter status:\n")
          print(table(df[[status_col]], useNA = "ifany"))
        }
      }
      
      # =============================================================================
      # SUMMARY REPORTING
      # =============================================================================
      
      generate_processing_summary <- function(all_results) {
        cat("\n", strrep("=", 70), "\n")
        cat("PROCESSING SUMMARY\n")
        cat(strrep("=", 70), "\n\n")
        
        for (state in names(all_results)) {
          cat("State:", state, "\n")
          cat(strrep("-", 50), "\n")
          
          state_data <- all_results[[state]]
          if (is.null(state_data)) {
            cat("  No data processed\n\n")
            next
          }
          
          for (year in names(state_data)) {
            cat("  Year:", year, "\n")
            year_data <- state_data[[year]]
            
            if (is.null(year_data)) {
              cat("    No files processed\n")
            } else {
              cat("    Files processed:", length(year_data), "\n")
              
              # Count records
              total_records <- 0
              for (zip_name in names(year_data)) {
                zip_data <- year_data[[zip_name]]
                if (!is.null(zip_data)) {
                  for (file_type in names(zip_data)) {
                    if (is.data.frame(zip_data[[file_type]])) {
                      total_records <- total_records + nrow(zip_data[[file_type]])
                    }
                  }
                }
              }
              cat("    Total records:", format(total_records, big.mark = ","), "\n")
            }
          }
          cat("\n")
        }
        
        cat(strrep("=", 70), "\n\n")
      }
      
      # Function to find all cleaned files in the directory structure
      find_cleaned_files <- function() {
        cat("\n", strrep("=", 70), "\n")
        cat("CLEANED FILES LOCATION\n")
        cat(strrep("=", 70), "\n\n")
        
        for (state in config$states) {
          state_folder <- file.path(config$base_path, paste0(state, "_VoterFiles"))
          
          if (!dir.exists(state_folder)) next
          
          cat("State:", state, "\n")
          cat(strrep("-", 50), "\n")
          
          # Find all cleaned_data folders
          clean_folders <- list.dirs(state_folder, recursive = TRUE) %>%
            .[str_detect(., config$clean_folder_name)]
          
          for (clean_folder in clean_folders) {
            year <- basename(dirname(clean_folder))
            files <- list.files(clean_folder, pattern = "_clean\\.csv$")
            
            if (length(files) > 0) {
              cat("  ", year, "/", config$clean_folder_name, "/\n", sep = "")
              for (file in files) {
                cat("    -", file, "\n")
              }
            }
          }
          cat("\n")
        }
        
        cat(strrep("=", 70), "\n\n")
      }
      
      # =============================================================================
      # MAIN EXECUTION
      # =============================================================================
      
      main <- function() {
        cat("\n", strrep("=", 70), "\n")
        cat("VOTER FILE CLEANING PIPELINE\n")
        cat(strrep("=", 70), "\n")
        cat("Started at:", format(Sys.time()), "\n\n")
        
        cat("Directory structure:\n")
        cat("  Base path:", config$base_path, "\n")
        cat("  Looking for: [State]_VoterFiles/[State]_VoterFiles_[Year]/[Date]_Voter[Detail|History].zip\n")
        cat("  Output: [State]_VoterFiles/[State]_VoterFiles_[Year]/", config$clean_folder_name, 
            "/[Date]_[registration|history]_clean.csv\n\n")
        
        # Find all state folders
        state_folders <- list.files(config$base_path, 
                                    pattern = "^(FL|NY)_VoterFiles$", 
                                    full.names = TRUE)
        
        if (length(state_folders) == 0) {
          cat("ERROR: No state folders found!\n")
          cat("Please ensure you have folders named 'FL_VoterFiles' and/or 'NY_VoterFiles'\n")
          cat("in your working directory:", getwd(), "\n")
          return(NULL)
        }
        
        cat("Found state folders:\n")
        for (folder in state_folders) {
          state <- str_extract(basename(folder), "^[A-Z]{2}")
          cat("  -", basename(folder), "\n")
          
          # Show year subfolders
          dir_struct <- get_directory_structure(folder)
          if (length(dir_struct) > 0) {
            for (year_info in dir_struct) {
              cat("    └── ", year_info$year_folder_name, " (", 
                  length(year_info$zip_files), " zip files)\n", sep = "")
            }
          } else {
            cat("    └── No year folders found\n")
          }
        }
        cat("\n")
        
        # Process each state
        all_results <- list()
        for (state_folder in state_folders) {
          state_name <- str_extract(basename(state_folder), "^[A-Z]{2}")
          all_results[[state_name]] <- process_state_folder(state_folder)
        }
        
        # Generate summary
        generate_processing_summary(all_results)
        
        # Show where cleaned files are located
        find_cleaned_files()
        
        # Clean up any remaining temp files
        temp_dir <- file.path(config$base_path, config$unzip_temp)
        if (dir.exists(temp_dir)) {
          unlink(temp_dir, recursive = TRUE)
          cat("Cleaned up temporary files\n\n")
        }
        
        cat("Pipeline completed at:", format(Sys.time()), "\n")
        cat(strrep("=", 70), "\n\n")
        
        return(all_results)
      }
      
      # =============================================================================
      # HELPER FUNCTION FOR SELECTIVE PROCESSING
      # =============================================================================
      
      # Process only specific state and year
      process_selective <- function(state = NULL, year = NULL) {
        cat("Processing selective files...\n")
        
        if (!is.null(state)) {
          state_folder <- file.path(config$base_path, paste0(state, "_VoterFiles"))
          
          if (!dir.exists(state_folder)) {
            cat("Error: State folder not found:", state_folder, "\n")
            return(NULL)
          }
          
          if (!is.null(year)) {
            # Process specific year only
            # Year folder is named like "FL_VoterFiles_2022"
            year_folder_name <- paste0(state, "_VoterFiles_", year)
            year_path <- file.path(state_folder, year_folder_name)
            
            if (!dir.exists(year_path)) {
              cat("Error: Year folder not found:", year_path, "\n")
              cat("Expected folder name:", year_folder_name, "\n")
              return(NULL)
            }
            
            zip_files <- list.files(year_path, pattern = "\\.zip$", full.names = TRUE)
            clean_path <- file.path(year_path, config$clean_folder_name)
            
            year_info <- list(
              year = year,
              year_folder_name = year_folder_name,
              year_path = year_path,
              zip_files = zip_files,
              clean_path = clean_path
            )
            
            state_temp <- file.path(config$base_path, config$unzip_temp, state)
            result <- process_year_folder(year_info, state, state_temp)
            unlink(state_temp, recursive = TRUE)
            
            return(result)
          } else {
            # Process all years for this state
            return(process_state_folder(state_folder))
          }
        } else {
          # Process all states
          return(main())
        }
      }
      
      # =============================================================================
      # QUICK START EXAMPLES
      # =============================================================================
      
      # STEP 1: Run test mode first to verify cleaning on one file per state
      # test_results <- run_test_mode()
      
      # STEP 2: Review the output above and check the cleaned files
      
      # STEP 3A: If test looks good, process everything
      # results <- main()
      
      # STEP 3B: OR process just New York (if FL is already done)
      # ny_results <- process_ny_only()
      
      # Alternative: Process only specific state or year
      # results_fl <- process_selective(state = "FL")
      # results_fl_2022 <- process_selective(state = "FL", year = "2022")
      
      # Check directory structure without processing
      # dir_structure_ny <- get_ny_directory_structure("/Volumes/easystore/NY_VoterFiles")
      # str(dir_structure_ny)
      
      ### updated ----------
      # Load the fixed script
      source("/path/to/voter_file_cleaner_fixed.R")
      
      # Then run NY processing
      ny_results <- process_ny_only()
      