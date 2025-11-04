utils::globalVariables(c(
  "Score", "Metabolites_Affected", "Activity_Status"
))

#' @title Map Metabolites to Corresponding Pathways
#'
#' @description
#' Maps the classification results from MetabAnalysis to metabolic pathways and
#' determines the pathway's overall activity level (Hypoactive, Normal, or Hyperactive).
#'
#' @details This function enforces the use of the default metabolite-to-pathway
#' map ('metab_to_pwys') loaded internally. It assumes that metabolite identifiers
#' (HMDB_ID or Metabolite_Name) are stored in the **row names** of the 'metab_results'
#'  data frame, not in an explicit column.
#'
#' @param metab_results A data frame output from MetabAnalysis.
#'        Metabolite identifiers must be in the **row names**.
#'        Sample columns contain classifications ("Low", "Normal", "High", "Missing Reference").
#' @param match_by A character string specifying the identifier type used in the
#'        row names (and the pathway map). Can be "HMDB_ID" (default) or "Metabolite_Name".
#'
#' @return A data frame summarizing the activity for each pathway and sample,
#'        with columns: Sample_ID, Pathway_Name, Net_Score, Metabolites_Affected, and Activity_Status.
#'        "Low" from metab_results will be considered as -1 Net score, and "High"
#'        will be +1. Missing and Normal will simply be +0. The more metabolites
#'        expressed in that pathway, the more active it is.
#'
#' @examples
#' \dontrun{
#' # Mock data
#' metab_to_pwys <- data.frame(
#' Metabolite_Name = c("Glucose", "Fructose", "Lactate", "Pyruvate",
#'                    "Cholesterol", "Leucine", "Valine", "Isoleucine",
#'                    "Tryptophan", "Serotonin"),
#' HMDB_ID = c("HMDB00001", "HMDB00002", "HMDB00003", "HMDB00004",
#'            "HMDB00005", "HMDB00006", "HMDB00007", "HMDB00008",
#'            "HMDB00009", "HMDB00010"),
#' Pathway_Name = c("Glycolysis", "Glycolysis", "Energy Metabolism",
#'                 "Energy Metabolism", "Lipid Metabolism", "BCAA Metabolism",
#'                 "BCAA Metabolism", "BCAA Metabolism", "Serotonin Synthesis",
#'                 "Serotonin Synthesis"), stringsAsFactors = FALSE)
#'
#' # Assign the object to the global environment, so the internal helper function
#' # can find it (simulating loaded package data).
#' assign("metab_to_pwys", metab_to_pwys, envir = .GlobalEnv)
#'
#' # Create input data where metabolite names are in the row names, simulating
#' # the typical output from metab_results.
#' metabolites <- c("Glucose", "Fructose", "Lactate", "Pyruvate",
#'                 "Cholesterol", "Leucine", "Valine", "Isoleucine",
#'                 "Tryptophan", "Serotonin", "Unmapped Metabolite")
#'
#' metab_results <- data.frame(
#'   Sample1 = c("Low", "Low", "High", "High", "Normal", "High", "Normal",
#'     "High", "Low", "Normal", "High"),
#'   Sample2 = c("High", "Low", "Normal", "High", "Low", "Normal", "High",
#'     "High", "Normal", "Low", "Low"),
#'   Sample3 = c("Normal", "Low", "High", "Normal", "Low", "Normal", "Normal",
#'     "High", "High", "High", "Normal"),
#'   stringsAsFactors = FALSE)
#'
#' # Assign metabolite names as row names directly, as required by the function
#' rownames(metab_results) <- metabolites
#'
#' # Run the function
#' result <- MapToPathway(metab_results, match_by = "Metabolite_Name")
#'
#' }
#'
#' @references
#' \strong{HMDB Metabolite Reference Data}:
#' Wishart, D. S., et al. (2022). HMDB 5.0: The Human Metabolome Database for 2022.
#' Nucleic Acids Research, 50(D1), D1-D10. Retrieved from \href{https://hmdb.ca/}{HMDB}.
#'
#' \strong{Debugging Assistance:}
#' Google. (2025). Gemini (v 2.0 Flash) [Large language model]. \href{https://gemini.google.com}{Gemini}
#'
#' @import dplyr tidyr stringr tibble
#' @export
MapToPathway <- function(metab_results, match_by = "HMDB_ID") {

  # 1. Load Pathway Map and Input Validation ---
  if (!match_by %in% c("HMDB_ID", "Metabolite_Name")) {
    stop("The 'match_by' argument must be 'HMDB_ID' or 'Metabolite_Name'.")
  }

  # Load the metabolite-to-pathway map using the internal helper
  pathway_map_df <- GetPathwayMap()

  if (is.null(pathway_map_df) || nrow(pathway_map_df) == 0) {
    stop("Pathway mapping data ('metab_to_pwys') could not be loaded or is empty. Ensure the data is correctly loaded in the package.")
  }

  # Ensure the necessary columns exist in the pathway map
  if (!all(c(match_by, "Pathway_Name") %in% names(pathway_map_df))) {
    stop(paste0("Pathway map ('metab_to_pwys') must contain the matching column ('", match_by, "') and 'Pathway_Name' columns."))
  }

  # 2. Prepare Scoring Data ---
  metab_data <- metab_results %>%
    tibble::rownames_to_column(var = match_by)

  # Identify sample columns (all columns except the newly created identifier column)
  sample_cols <- names(metab_data)[!names(metab_data) %in% c(match_by)]

  if (length(sample_cols) == 0) {
    stop("No sample columns (concentration scores) found in metab_results.")
  }

  # Convert the wide result to long format for easier joining
  long_metab <- metab_data %>%
    tidyr::pivot_longer(
      cols = all_of(sample_cols),
      names_to = "Sample_ID",
      values_to = "Concentration_Status"
    ) %>%
    # Assign numerical scores based on MetabAnalysis output
    dplyr::mutate(
      Score = dplyr::case_when(
        Concentration_Status == "High" ~ 1,
        Concentration_Status == "Low" ~ -1,
        TRUE ~ 0
      )
    )

  # 3. Join metabolite scores with pathway map ---

  pathway_scores_raw <- long_metab %>%
    dplyr::left_join(
      pathway_map_df,
      by = match_by # Use the dynamically set column name for the join
    ) %>%
    # Remove any metabolites that were scored but failed to map to a pathway in the provided map
    tidyr::drop_na(Pathway_Name)

  # 4. Calculate the Net Score for each Sample and Pathway ---

  pathway_activity <- pathway_scores_raw %>%
    dplyr::group_by(Sample_ID, Pathway_Name) %>%
    dplyr::summarise(
      Net_Score = sum(Score, na.rm = TRUE),
      Metabolites_Affected = n(),
      .groups = "drop"
    ) %>%
    # 5. Determine the final Pathway Activity Status ---
    dplyr::mutate(
      Activity_Status = dplyr::case_when(
        Net_Score > 1 ~ "Hyperactive",
        Net_Score < -1 ~ "Hypoactive",
        Net_Score == 1 ~ "Mild Activation",
        Net_Score == -1 ~ "Mild Inhibition",
        Net_Score == 0 ~ "Normal Activity"
      )
    ) %>%
    dplyr::select(Sample_ID, Pathway_Name, Net_Score, Metabolites_Affected, Activity_Status)

  return(pathway_activity)
}

# [END]
