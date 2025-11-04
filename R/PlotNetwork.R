#' Plot pathway network for a given sample
#'
#' @description
#' Builds a pathway-level network where each node is a pathway, and an edge
#' connects two pathways if they share at least one metabolite.
#' Node color is determined by pathway Net_Score
#' (red = hyperactive, blue = hypoactive).
#'
#' @param result Tibble containing columns: Sample_ID, Pathway_Name, Net_Score
#' @param sample_id Sample ID to visualize (character)
#' @param metab_to_pwys Tibble containing: Metabolite_Name, HMDB_ID, Pathway_Name
#'
#' @return Returns a \code{ggplot2} object (via \code{ggraph}).
#'
#' @examples
#' \dontrun{
#'   # Assuming MapToPathway returns a data frame like this:
#'   mock_pathway_map <- data.frame(
#'     Metabolite = c("Glucose", "Fructose", "ATP", "ADP", "Citrate", "Isocitrate"),
#'     Pathway = c("Glycolysis", "Glycolysis", "Glycolysis", "TCA cycle", "TCA cycle", "TCA cycle")
#'   )
#'
#'   network_plot <- PlotNetwork(mock_pathway_map)
#'   print(network_plot)
#' }
#'
#' @import dplyr igraph ggraph ggplot2 scales
#' @export
PlotNetwork <- function(result, sample_id) {
  # Required packages
  library(dplyr)
  library(tidyr)
  library(igraph)
  library(ggraph)
  library(scales)

  # --- 1. Filter to the chosen sample
  df <- result %>%
    filter(Sample_ID == sample_id) %>%
    select(Pathway_Name, Net_Score)

  if (nrow(df) == 0) {
    stop(paste("No data found for Sample_ID:", sample_id))
  }

  # --- 2. Retrieve metabolite-pathway relationships dynamically
  metab_to_pwys <- GetPathwayMap()

  # --- 3. Keep only pathways present in this sample
  rel_map <- metab_to_pwys %>%
    filter(Pathway_Name %in% df$Pathway_Name)

  if (nrow(rel_map) == 0) {
    stop("No metabolite-pathway relationships found for selected pathways.")
  }

  # Vertices
  vertices <- df %>% rename(name = Pathway_Name)

  # Edges
  edges <- rel_map %>%
    inner_join(rel_map, by = "HMDB_ID") %>%
    filter(Pathway_Name.x != Pathway_Name.y) %>%
    count(Pathway_Name.x, Pathway_Name.y, name = "Shared_Metabolites") %>%
    distinct() %>%
    rename(from = Pathway_Name.x, to = Pathway_Name.y)

  # Graph
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = vertices)

  # Plot
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = Shared_Metabolites),
                   alpha = 0.3, color = "grey60") +
    geom_node_point(aes(fill = Net_Score),
                    shape = 21, color = "black", stroke = 1.2, size = 6) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      name = "Pathway Activity",
      breaks = c(min(df$Net_Score), 0, max(df$Net_Score)),
      labels = c("Hypoactive", "Normal", "Hyperactive")
    ) +
    ggtitle("Metabolic Pathway Network") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )

  return(p)
}
