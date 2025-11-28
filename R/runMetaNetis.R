#' Launch Shiny App for MetaNetis
#'
#' A function that launches the Shiny app for MetaNetis.
#' The purpose of this app is only to illustrate how a Shiny
#' app works. The code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#'
#' MetaNetis::runMetaNetis()
#'
#' }
#'
#' @author Yunze Du, \email{yunze.du@mail.utoronto.ca}
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials.
#' \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @import DT
#' @import ggplot2
#' @import dplyr
#' @importFrom shiny runApp
#' @importFrom shinyjs useShinyjs toggle runjs
#' @importFrom shinyWidgets switchInput

runMetaNetis <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "MetaNetis")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")

  return(actionShiny)
}
# [END]
