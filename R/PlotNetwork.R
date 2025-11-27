utils::globalVariables(c(
  "Sample_ID", "Pathway_Name", "Net_Score",
  "Pathway_Name.x", "Pathway_Name.y",
  "Shared_Metabolites", "name", "aes"
))

#' @title Plot pathway network for a given sample
#'
#' @description
#' Builds a pathway-level network where each node is a pathway, and an edge
#' connects two pathways if they share at least one metabolite.
#' Node color is determined by pathway Net_Score
#' (red = hyperactive, blue = hypoactive).
#'
#' @param result Tibble containing columns: Sample_ID, Pathway_Name, Net_Score
#' @param sample_id Sample ID to visualize (character)
#'
#' @return Returns a \code{ggplot2} object (via \code{ggraph}).
#'
#' @examples
#' \dontrun{
#' # Define 12 pathways
#' pathways <- c("Glycolysis", "Energy Metabolism", "Lipid Metabolism",
#'   "BCAA Metabolism", "Serotonin Synthesis", "Histidine Metabolism",
#'   "TCA Cycle", "Purine Metabolism", "Pyrimidine Metabolism",
#'   "Fatty Acid Oxidation", "Amino Acid Metabolism", "Urea Cycle")
#'
#' # Create 50 metabolites
#' metabolites <- paste0("Metab", 1:50)
#' hmdb_ids <- paste0("HMDB", sprintf("%05d", 1:50))
#'
#' # Each metabolite participates in 2-4 random pathways to create dense connections
#' metab_to_pwys <- tibble()
#' for (i in 1:50) {
#'  n_paths <- sample(2:4, 1)
#'  temp <- tibble(
#'   Metabolite_Name = rep(metabolites[i], n_paths),
#'   HMDB_ID = rep(hmdb_ids[i], n_paths),
#'   Pathway_Name = sample(pathways, n_paths)
#'  )
#'  metab_to_pwys <- bind_rows(metab_to_pwys, temp)
#' }
#'
#' # Mock pathway results per sample
#' result <- expand.grid(
#'  Sample_ID = c("Sample1", "Sample2"),
#'  Pathway_Name = pathways) %>%
#'  as_tibble() %>%
#'  mutate(
#'   Net_Score = sample(-3:3, n(), replace = TRUE),
#'   Metabolites_Affected = sample(1:10, n(), replace = TRUE),
#'   Activity_Status = sample(c(
#'    "Hyperactive", "Normal Activity", "Hypoactive", "Mild Inhibition"),
#'    n(), replace = TRUE
#'   )
#'  )
#'
#'  PlotNetwork(result, "Sample1")
#'
#' }
#'
#' @author Yunze Du, \email{yunze.du@mail.utoronto.ca}
#'
#' @references
#' \strong{HMDB Metabolite Reference Data}:
#' Wishart, D. S., et al. (2022). HMDB 5.0: The Human Metabolome Database for 2022.
#' Nucleic Acids Research, 50(D1), D1-D10. Retrieved from \href{https://hmdb.ca/}{HMDB}.
#'
#' \strong{Debugging Assistance:}
#' Google. (2025). Gemini (v 2.0 Flash) [Large language model]. \href{https://gemini.google.com}{Gemini}
#'
#' @importFrom dplyr filter select rename count inner_join
#' @importFrom igraph graph_from_data_frame layout_with_kk
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text scale_edge_width
#' @importFrom ggplot2 scale_fill_gradient2 ggtitle theme element_text theme_void
#' @export
PlotNetwork <- function(result, sample_id) {

  # 1. Filter to the chosen sample
  df <- result %>%
    dplyr::filter(Sample_ID == sample_id) %>%
    dplyr::select(Pathway_Name, Net_Score)

  if (nrow(df) == 0) {
    stop(paste("No data found for Sample_ID:", sample_id))
  }

  # 2. Retrieve metabolite-pathway relationships dynamically
  metab_to_pwys <- GetPathwayMap()

  # 3. Keep only pathways present in this sample
  rel_map <- metab_to_pwys %>%
    dplyr::filter(Pathway_Name %in% df$Pathway_Name)

  if (nrow(rel_map) == 0) {
    stop("No metabolite-pathway relationships found for selected pathways.")
  }

  # 4. Get Vertices
  vertices <- df %>% rename(name = Pathway_Name)

  # 5. Get Edges
  edges <- rel_map %>%
    dplyr::inner_join(rel_map, by = "HMDB_ID") %>%
    dplyr::filter(Pathway_Name.x != Pathway_Name.y) %>%
    dplyr::count(Pathway_Name.x, Pathway_Name.y, name = "Shared_Metabolites") %>%
    distinct() %>%
    dplyr::rename(from = Pathway_Name.x, to = Pathway_Name.y)

  # 6. Graph
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = vertices)

  # Calculate symmetrical limits for the visual gradient
  max_abs_score <- max(abs(df$Net_Score), na.rm = TRUE)
  min_score <- min(df$Net_Score)
  max_score <- max(df$Net_Score)

  # Determine if data is Bipolar (contains both positive and negative values)
  is_bipolar <- min(df$Net_Score) < 0 && max(df$Net_Score) > 0

  if (min_score == max_score) {
    custom_breaks <- c(min_score)
    custom_labels <- c(paste("Activation Score:", round(min_score, 2)))

  } else if (is_bipolar) {
    custom_breaks <- c(min_score, 0, max_score)
    custom_labels <- c("Hypoactive", "Neutral", "Hyperactive")

  } else if (min_score > 0) {
    custom_breaks <- c(min_score, max_score)
    custom_labels <- c("Low Activation", "High Activation")

  } else { # max_score < 0
    custom_breaks <- c(min_score, max_score)
    custom_labels <- c("Strong Hypo.", "Weak Hypo.")
  }

  layout <- layout_with_kk(g)

  p <- ggraph::ggraph(g, layout = layout) +
    ggraph::geom_edge_link(ggplot2::aes(width = Shared_Metabolites),
                   alpha = 0.3, color = "grey60") +
    scale_edge_width(range = c(0.2, 2)) +
    ggraph::geom_node_point(ggplot2::aes(fill = Net_Score),
                    shape = 21, color = "black", stroke = 1.2, size = 6) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3,
                           max.overlaps = Inf) +
    ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0,
      limits = c(-max_abs_score, max_abs_score),
      name = "Pathway Activity",
      breaks = custom_breaks,
      labels = custom_labels
    ) +
    ggplot2::ggtitle("Metabolic Pathway Network") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10)
    )

  return(p)
}

# [END]
