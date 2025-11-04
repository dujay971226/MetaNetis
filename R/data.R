#' Standardized Human Metabolite Reference Ranges (MetaNetis Baseline)
#'
#' A pre-processed and aggregated dataset containing healthy human reference
#' ranges for metabolites derived from the Human Metabolome Database (HMDB).
#' This table forms the standardized baseline for comparison within the \code{MetaNetis} package.
#'
#' The original HMDB data was extensively cleaned to produce a robust dataset
#' suitable for comparing user metabolomic profiles against healthy controls.
#' The key cleaning and standardization steps performed include:
#' \itemize{
#'   \item \strong{Filtering:} Removed all non-specific entries such as "Unknown,"
#'         "Not Specified," and ranges containing only one reference (e.g., mean only).
#'   \item \strong{Merging & Ranging:} Metabolite entries were grouped by Name,
#'         Biofluid, and Subject Age Description. The final \code{Min_Concentration}
#'         and \code{Max_Concentration} ranges were determined by taking the
#'         minimum of all minimums and the maximum of all maximums for the
#'         metabolite group with same Sample_Type and Subject_Age_Description.
#'   \item \strong{Age Standardization:} Age ranges were parsed from free-text
#'         descriptions and converted into standardized \code{Min_Age(year)} and
#'         \code{Max_Age(year)} columns, handling units (days, months, years)
#'         and symbols (>, <, single values).
#' }
#'
#' @source Human Metabolome Database (HMDB 5.0), \url{https://hmdb.ca/downloads}.
#'
#' @format A data frame with rows representing unique metabolite, biofluid, and age group combinations, and 10 columns:
#' \describe{
#'  \item{HMDB_ID}{Unique identifier of the metabolite.}
#'  \item{Metabolite_Name}{Common name of the metabolite.}
#'  \item{Sample_Type}{Standardized biological fluid type (e.g., Blood/Serum/Plasma, CSF, Urine).}
#'  \item{Subject_Age_Description}{The original age group description from the HMDB dataset.}
#'  \item{Min_Age(year)}{Minimum age (in years) for the applicable age group.}
#'  \item{Max_Age(year)}{Maximum age (in years) for the applicable age group (\code{Inf} for general adult groups).}
#'  \item{Min_Concentration(Healthy)}{Minimum concentration observed in healthy individuals within the age group.}
#'  \item{Mean_Concentration(Healthy)}{Aggregated average concentration in healthy individuals within the age group.}
#'  \item{Max_Concentration(Healthy)}{Maximum concentration observed in healthy individuals within the age group.}
#'  \item{Unit}{The unit of measurement for concentration (e.g., uM, umol/mmol creatinine).}
#' }
#'
#' @examples
#' # The preferred way to obtain this data is using the helper function:
#' # ref_df <- GetRefRanges()
#' # Or it can be directly loaded using data(reference_ranges_df)
#' \dontrun{
#'   data(reference_ranges_df)
#'   head(reference_ranges_df)
#' }
"reference_ranges_df"

#' Metabolite-to-Pathway Mapping (MetaNetis Baseline)
#'
#' A standardized, cleaned, and aggregated mapping of metabolites to their
#' associated biochemical pathways, derived from the Human Metabolome Database (HMDB).
#' This table is used internally by functions like \code{MapToPathway} to score
#' pathway activity based on metabolite concentrations.
#'
#' @source Human Metabolome Database (HMDB 5.0), \url{https://hmdb.ca/downloads}.
#'
#' @format A data frame with rows representing unique metabolite-to-pathway associations, and 3 columns:
#' \describe{
#'  \item{Metabolite_Name}{Standardized common name of the metabolite.}
#'  \item{HMDB_ID}{Unique identifier of the metabolite.}
#'  \item{Pathway}{Standardized name of the biochemical pathway the metabolite belongs to.}
#' }
#'
#' @examples
#' # The preferred way to obtain this data is using the helper function:
#' # map_df <- GetPathwayMap()
#' # Or it can be directly loaded using data(metab_to_pwys)
#' \dontrun{
#'   data(metab_to_pwys)
#'   head(metab_to_pwys)
#' }
"metab_to_pwys"


# [END]
