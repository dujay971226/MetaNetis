# Mock reference data and samples
metab_to_pwys <- data.frame(
  Metabolite_Name = c("Glucose", "Fructose", "Lactate", "Pyruvate",
                      "Cholesterol", "Leucine", "Valine", "Isoleucine",
                      "Tryptophan", "Serotonin"),
  HMDB_ID = c("HMDB00001", "HMDB00002", "HMDB00003", "HMDB00004",
              "HMDB00005", "HMDB00006", "HMDB00007", "HMDB00008",
              "HMDB00009", "HMDB00010"),
  Pathway_Name = c("Glycolysis", "Glycolysis", "Energy Metabolism",
                   "Energy Metabolism", "Lipid Metabolism", "BCAA Metabolism",
                   "BCAA Metabolism", "BCAA Metabolism", "Serotonin Synthesis",
                   "Serotonin Synthesis"),
  stringsAsFactors = FALSE
)

# Create input data where metabolite names are in the row names, simulating
# the typical output from metab_results.
metabolites <- c("Glucose", "Fructose", "Lactate", "Pyruvate",
                 "Cholesterol", "Leucine", "Valine", "Isoleucine",
                 "Tryptophan", "Serotonin", "Unmapped Metabolite")

metab_results <- data.frame(
  Sample1 = c("Low", "Low", "High", "High", "Normal", "High", "Normal", "High", "Low", "Normal", "High"),
  Sample2 = c("High", "Low", "Normal", "High", "Low", "Normal", "High", "High", "Normal", "Low", "Low"),
  Sample3 = c("Normal", "Low", "High", "Normal", "Low", "Normal", "Normal", "High", "High", "High", "Normal"),
  stringsAsFactors = FALSE
)

# Assign metabolite names as row names directly, as required by the function
rownames(metab_results) <- metabolites


# -----------------Test Block----------------------
test_that("MapToPathway correctly aggregates scores using row names and internal data", {

  # Run the function using 'Metabolite_Name' as the row identifier
  result <- MapToPathway(metab_results = metab_results,
                         match_by = "Metabolite_Name",
                         alt_pwy_map = metab_to_pwys)

  # Check if the output structure is correct
  expect_true(is.data.frame(result))

  # 5 Total Pathways * 3 Samples = 15 possible results.
  # We expect 2 rows to be filtered out (Net_Score == 0):
  # 1. Glycolysis in Sample 2 (1 + (-1) = 0)
  # 2. Lipid Metabolism in Sample 1 (0 = 0)
  # Expected rows: 15 - 2 = 13
  expect_equal(nrow(result), 13)

  expect_true(all(c("Sample_ID", "Pathway_Name", "Net_Score", "Metabolites_Affected", "Activity_Status") %in% names(result)))

  # --- Test Specific Pathway Aggregations (Glycolysis: Glucose, Fructose) ---

  glycolysis_results <- result %>%
    dplyr::filter(Pathway_Name == "Glycolysis") %>%
    dplyr::arrange(Sample_ID)

  # Sample 1: Low (-1) + Low (-1) = -2 (Hypoactive) -> REMAINS
  expect_equal(glycolysis_results$Net_Score[glycolysis_results$Sample_ID == "Sample1"], -2)
  expect_equal(glycolysis_results$Activity_Status[glycolysis_results$Sample_ID == "Sample1"], "Hypoactive")

  # Sample 2: High (+1) + Low (-1) = 0 (Normal Activity) -> FILTERED OUT
  # We must skip the test for Sample2 Glycolysis as it is filtered out of 'result'.

  # Sample 3: Normal (0) + Low (-1) = -1 (Mild Inhibition) -> REMAINS
  expect_equal(glycolysis_results$Net_Score[glycolysis_results$Sample_ID == "Sample3"], -1)
  expect_equal(glycolysis_results$Activity_Status[glycolysis_results$Sample_ID == "Sample3"], "Mild Inhibition")

  # --- Test BCAA Metabolism (Leucine, Valine, Isoleucine) ---

  bcaa_results <- result %>%
    dplyr::filter(Pathway_Name == "BCAA Metabolism") %>%
    dplyr::arrange(Sample_ID)

  # Sample 1: High (+1) + Normal (0) + High (+1) = +2 (Hyperactive) -> REMAINS
  expect_equal(bcaa_results$Net_Score[bcaa_results$Sample_ID == "Sample1"], 2)
  expect_equal(bcaa_results$Activity_Status[bcaa_results$Sample_ID == "Sample1"], "Hyperactive")
  expect_equal(bcaa_results$Metabolites_Affected[bcaa_results$Sample_ID == "Sample1"], 3)

  # Sample 2: Normal (0) + High (+1) + High (+1) = +2 (Hyperactive) -> REMAINS
  expect_equal(bcaa_results$Net_Score[bcaa_results$Sample_ID == "Sample2"], 2)
  expect_equal(bcaa_results$Activity_Status[bcaa_results$Sample_ID == "Sample2"], "Hyperactive")

  # Sample 3: Normal (0) + Normal (0) + High (+1) = +1 (Mild Activation) -> REMAINS
  expect_equal(bcaa_results$Net_Score[bcaa_results$Sample_ID == "Sample3"], 1)
  expect_equal(bcaa_results$Activity_Status[bcaa_results$Sample_ID == "Sample3"], "Mild Activation")


  # --- Test Serotonin Synthesis (Tryptophan, Serotonin) ---

  serotonin_results <- result %>%
    dplyr::filter(Pathway_Name == "Serotonin Synthesis") %>%
    dplyr::arrange(Sample_ID)

  # Sample 1: Low (-1) + Normal (0) = -1 (Mild Inhibition) -> REMAINS
  expect_equal(serotonin_results$Net_Score[serotonin_results$Sample_ID == "Sample1"], -1)
  expect_equal(serotonin_results$Activity_Status[serotonin_results$Sample_ID == "Sample1"], "Mild Inhibition")

  # Sample 2: Normal (0) + Low (-1) = -1 (Mild Inhibition) -> REMAINS
  expect_equal(serotonin_results$Net_Score[serotonin_results$Sample_ID == "Sample2"], -1)
  expect_equal(serotonin_results$Activity_Status[serotonin_results$Sample_ID == "Sample2"], "Mild Inhibition")

  # Sample 3: High (+1) + High (+1) = +2 (Hyperactive) -> REMAINS
  expect_equal(serotonin_results$Net_Score[serotonin_results$Sample_ID == "Sample3"], 2)
  expect_equal(serotonin_results$Activity_Status[serotonin_results$Sample_ID == "Sample3"], "Hyperactive")
})

# Clean up the alt reference ranges
SetAltBaseline()

# [END]
