#' @title Analyze Metabolite Concentrations Against Healthy Reference Ranges
#'
#' @description
#' Compares a user-supplied metabolite concentration dataset against the
#' standardized healthy reference ranges provided by \code{GetRefRanges()} based
#' on Human Metabolome Database(HMDB).
#' It returns a data frame showing the classification of each metabolite's
#' concentration in each sample as "Normal," "High," or "Low." If reference
#' range not available, it will show "Missing Reference" instead.
#'
#' @param data_input A data frame or a character string representing the file path
#'   to a CSV file containing metabolite concentrations. 1st row must represent
#'   individual samples, and \strong{1st column must be the metabolite identifiers
#'   (HMDB IDs or Metabolite Names)}. If names were used instead of IDS,
#'   please make sure they match the reference data's name
#'   (obtained by GetRefRanges()). Columns represent samples.
#' @param age A numeric vector where each element represents the age (in years)
#'   for the corresponding sample column in \code{data_input}. The length of this
#'   vector must match the number of sample columns.
#' @param sample_type A character vector specifying the biospecimen type for
#'   each sample. The length of this vector must match the number of samples.
#'   (e.g., \code{"Blood/Serum/Plasma"}, \code{"Urine"}, \code{"CSF"}).
#' @param match_by A character string indicating whether the \strong{row names} in
#'   \code{data_input} represent "HMDB_ID" (default) or "Metabolite_Name". Default
#'   set to be "HMDB_ID".
#' @param ref_data_override An optional data frame to use as the reference,
#'   bypassing the \code{GetRefRanges()} call.
#'
#' @details
#' This function assumes the user data has been formatted so that metabolite
#' identifiers are used as \strong{row names} and all column values are numeric
#' concentrations for different samples. The function uses the \code{match_by}
#' parameter to determine which column in the reference table to match the row
#' names against.
#'
#' **Required Structure of \code{data_input}:**
#' \itemize{
#'   \item **Row Names:** Must be HMDB IDs (e.g., "HMDB0000001") or
#'    Metabolite Names (e.g., "Alanine").
#'   \item **Columns:** Must be numeric concentration values for each sample.
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1
#' # Make a mock reference data
#' mock_reference_data <- data.frame(
#'   'HMDB_ID' = c("HMDB0000001", "HMDB0000001", "HMDB0000002", "HMDB0000003", "HMDB0000005"),
#'   'Metabolite_Name' = c("Alanine", "Alanine", "Glucose", "Creatinine", "Missing_Metab"),
#'   'Sample_Type' = c("Urine", "Plasma", "Plasma", "Urine", "Plasma"),
#'   'Min_Age(year)' = c(0, 11, 0, 0, 0),
#'   'Max_Age(year)' = c(99, 99, 99, 99, 99),
#'   'Min_Concentration(Healthy)' = c(500, 150, 4.0, 50, 200),
#'   'Max_Concentration(Healthy)' = c(1000, 300, 7.0, 100, 400),
#'   check.names = FALSE
#' )
#'
#' # User's Input
#' user_data <- data.frame(
#'   Sample_A = c(250, 6.0, 120, 50),
#'   Sample_B = c(160, 12.0, 60, 50),
#'   row.names = c("HMDB0000001", "HMDB0000002", "HMDB0000003", "HMDB0000004")
#' )
#'
#' # Age and Sample Type
#' age_vector <- c(5, 20)
#' sample_type_vector <- c("Urine", "Plasma") # Sample A is Urine, Sample B is Plasma
#'
#' # Test Case: Matching by HMDB_ID
#' results_id <- MetabAnalysis(
#'   data_input = user_data,
#'   age = age_vector,
#'   sample_type = sample_type_vector,
#'   match_by = "HMDB_ID",
#'   ref_data_override = mock_reference_data
#' )
#' print(results_id)
#'
#' # Test Case : Matching by Metabolite_Name
#' # Rename row names to test the 'Metabolite_Name' matching path
#' user_data_name <- user_data
#' row.names(user_data_name) <- c("Alanine", "Glucose", "Creatinine", "Unknown_Metab")
#'
#' results_name <- MetabAnalysis(
#'   data_input = user_data_name,
#'   age = age_vector,
#'   sample_type = sample_type_vector,
#'   match_by = "Metabolite_Name",
#'   ref_data_override = mock_reference_data
#' )
#' print(results_name)
#' }
#'
#' @return A data frame with the same structure as \code{data_input} (rows as
#'   metabolites, columns as samples), where the numeric concentration values are
#'   replaced by character classifications: "Normal", "High", or "Low". If a
#'   metabolite cannot be matched or no age-appropriate reference is found,
#'   the classification will be "Missing Reference".
#'
#' @author Yunze Du, \email{yunze.du@mail.utoronto.ca}
#'
#' @references
#' \strong{HMDB Metabolite Reference Data}:
#' Wishart, D. S., et al. (2022). HMDB 5.0: The Human Metabolome Database for 2022.
#' Nucleic Acids Research, 50(D1), D1-D10. Retrieved from
#' \href{https://hmdb.ca/}{HMDB}.
#'
#' \strong{Debugging Assistance:}
#' Google. (2025). Gemini (v 2.0 Flash) [Large language model].
#' \href{https://gemini.google.com}{Gemini}
#'
#' @importFrom utils read.csv
#' @export
MetabAnalysis <- function(data_input = NULL,
                          age = NULL,
                          sample_type = NULL,
                          match_by = c("HMDB_ID", "Metabolite_Name"),
                          ref_data_override = NULL) {

  match_by <- match.arg(match_by)

  # 1. Load Reference Data and check if there's a override
  if (!is.null(ref_data_override)) {
    ref_df <- ref_data_override
  } else {
    ref_df <- GetRefRanges()
  }

  # Confirm ref_df was loaded successfully
  if (is.null(ref_df) || nrow(ref_df) == 0) {
    stop(
      "Failed to retrieve or load reference ranges. Cannot perform MetabAnalysis.")
  } else {

  }

  # 2. Load User Data
  user_df <- NULL
  # Load from CSV
  if (is.character(data_input) && file.exists(data_input)) {
    user_df <- utils::read.csv(data_input,
                               stringsAsFactors = FALSE,
                               check.names = FALSE)

    # Convert first row to column names and first column to row names ----
    if (!is.null(user_df) && nrow(user_df) > 1 && ncol(user_df) > 1) {
      # Save original column names temporarily
      original_colnames <- colnames(user_df)

      # Set column names from the first row
      colnames(user_df) <- as.character(user_df[1, ])

      # Remove the first row
      user_df <- user_df[-1, ]

      # Set row names from the first column
      rownames(user_df) <- as.character(user_df[[1]])

      # Remove the first column
      user_df <- user_df[, -1, drop = FALSE]
    } else {

    }

  } else if (is.data.frame(data_input)) {
    # Load from data frame
    user_df <- data_input
  } else {
    # Input not valid
    stop("Invalid data_input. Must be a valid CSV path or a data frame.")
  }

  # 3. Validate and Prepare User Data

  # Ensure all columns are numeric (concentration data)
  conc_data <- user_df
  if (!all(sapply(conc_data, is.numeric))) {
    warning("Non-numeric columns detected in concentration data.
            Attempting to convert to numeric.")
    conc_data <- as.data.frame(lapply(conc_data, function(x) {
      if (!is.numeric(x)) as.numeric(as.character(x)) else x
    }))
  } else {

  }

  # Extract metabolite identifiers (row names)
  metab_ids <- row.names(conc_data)
  if (is.null(metab_ids) || length(metab_ids) == 0) {
    stop("Metabolite ID/names are missing. Data frame must have row names set
         (or the first column must be identifiers if loading from CSV with row.names = 1).")
  } else {

  }

  # Define which column in ref_df to match against (HMDB_ID or Metabolite_Name)
  match_ref_col <- match_by

  # Make sure ages are numbers. Also the length of age and sample_type matches sample size.
  num_samples <- ncol(conc_data)
  if (!is.numeric(age) || length(age) != num_samples) {
    stop(paste0("The 'age' parameter must be a numeric vector whose length (",
                length(age),
                ") matches the number of sample columns (",
                num_samples,
                ")."))
  } else {

  }

  # If sample_type is a single string, recycle it to the needed length
  if (is.character(sample_type) && length(sample_type) == 1) {
    sample_type <- rep(sample_type, num_samples)
  } else if (!is.character(sample_type) || length(sample_type) != num_samples) {
    # If sample_type is a character vector
    stop(paste0("The 'sample_type' parameter must be a character vector whose length (",
                length(sample_type),
                ") matches the number of sample columns (",
                num_samples,
                ")."))
  } else {

  }

  # Load corresponding ref column and convert to string
  all_ref_ids <- as.character(ref_df[[match_ref_col]])

  # Find which user metabolites are missing from the reference
  missing_ids <- setdiff(metab_ids, all_ref_ids)

  # If NO user metabolites are found in the reference (i.e., all are missing), stop immediately
  if (length(metab_ids) == length(missing_ids)) {
    stop(paste("None of the user's metabolite identifiers (row names) were found in the",
               match_ref_col, "column of the reference data. Analysis cannot proceed."))
  } else {

  }

  if (length(missing_ids) > 0) {
    # If some are missing, print a warning with the list
    warning(paste(length(missing_ids), "of", length(metab_ids),
                  "metabolites could not be matched in the reference database
                  and will be classified as 'Missing Reference':\n",
                  paste(missing_ids, collapse = ", ")))
  } else {

  }

  # Initialize the results data frame with all Missing Reference
  results_df <- conc_data
  results_df[] <- "Missing Reference"

  # 4. Analyze Concentrations (Per Sample)

  sample_names <- colnames(conc_data)

  for (j in seq_along(sample_names)) {

    sample_name <- sample_names[j] # Sample name
    current_age <- age[j] # Age for the current sample
    current_type <- sample_type[j] # Sample type for the current sample

    # Age Filtering: Filter reference data by the current sample's age, and sample type
    age_filtered_ref <- ref_df[
      ref_df$"Min_Age(year)" <= current_age & ref_df$"Max_Age(year)" >= current_age, ]

    filtered_ref <- age_filtered_ref[
      age_filtered_ref$"Sample_Type" == current_type, ]

    # Check if HMDB has the reference data corresponding to the age.
    if (nrow(filtered_ref) == 0) {
      warning(paste("No reference ranges found for age", current_age,
                    "for sample", sample_name, ". Classifying as 'Missing Reference'."))
      next
    } else {

    }

    for (i in seq_len(nrow(conc_data))) {

      metab_id <- metab_ids[i] # Current row name (ID or Name)
      conc_value <- conc_data[i, sample_name] # Current metabolite concentration for sample j

      # Match the metabolite's reference range using the age-filtered data
      metab_ref <- filtered_ref[filtered_ref[[match_ref_col]] == metab_id, ]

      classification <- "Missing Reference" # Default classification
      metab_ref <- NULL

      # If match by name
      if (match_ref_col == "Metabolite_Name") {
        # Substring matching (user's name is contained in ref_df's name)
        match_indices <- grepl(metab_id, filtered_ref[[match_ref_col]], ignore.case = TRUE)
        metab_ref <- filtered_ref[match_indices, ]

        # If multiple matches are found, take the first one
        if (nrow(metab_ref) > 1) {
          warning(paste("Multiple reference ranges found for metabolite name containing '",
                        metab_id, "'. Using the first match for sample ", sample_name, "."))
          metab_ref <- metab_ref[1, ]
        } else {

        }

      } else {
        # Exact matching (used for HMDB_ID)
        metab_ref <- filtered_ref[filtered_ref[[match_ref_col]] == metab_id, ]
      }

      if (!is.null(metab_ref) && nrow(metab_ref) > 0 && !is.na(conc_value)) {

        # Take the first matched range
        min_conc <- metab_ref[1, "Min_Concentration(Healthy)"]
        max_conc <- metab_ref[1, "Max_Concentration(Healthy)"]

        # Comparison logic
        if (is.na(min_conc) || is.na(max_conc)) {
          classification <- "Missing Reference"
        } else if (conc_value < min_conc) {
          classification <- "Low"
        } else if (conc_value > max_conc) {
          classification <- "High"
        } else {
          classification <- "Normal"
        }
      } else {

      }

      # Assign classification to the results data frame
      results_df[i, sample_name] <- classification
    }
  }

  return(results_df)
}

# [END]
