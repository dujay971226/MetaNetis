#' @title Retrieve Reference Ranges(HMDB)
#'
#' @description
#' Loads the final, cleaned, and aggregated human reference range data frame
#' (which includes standardized age categories, biofluid types, and
#' descriptive column names) directly from the package's 'data' directory if it
#' has not already been loaded into the user's environment.
#'
#' @details
#' This function first checks for the existence of the 'reference_range_df' object.
#' If found, it uses the existing object. If not found, it attempts to load it
#' using \code{data()}. The function will halt execution if the data file cannot
#' be located or loaded.
#'
#' @return Prints the full 'reference_range_df' data frame to the console.
#'
GetRefRanges <- function() {

  data_object_name <- "reference_ranges_df"
  ref_df <- NULL

  # Check if the object is already loaded in the environment.
  if (exists(data_object_name, envir = .GlobalEnv)) {
    ref_df <- get(data_object_name, envir = .GlobalEnv)

  } else {
    # Attempt to load the data
    load_success <- tryCatch({
      data(data_object_name)
      TRUE
    }, error = function(e) {
      stop("Reference data missing, please reinstall the package or load altertative reference range with SetAltBaseline().")
    })

    if (load_success) {
      ref_df <- get(data_object_name)
    }
  }

  return(ref_df)
}
