test_that("Intergrated Test of the Whole Package", {
  # Mock reference data
  mock_ref <- data.frame(
    'HMDB_ID' = c("HMDB0000001", "HMDB0000002", "HMDB0000003"),
    'Metabolite_Name' = c("Alanine", "Glucose", "Creatinine"),
    'Sample_Type' = c("Urine", "Plasma", "Urine"),
    'Min_Age(year)' = c(0, 0, 0),
    'Max_Age(year)' = c(99, 99, 99),
    'Min_Concentration(Healthy)' = c(500, 4.0, 50),
    'Max_Concentration(Healthy)' = c(1000, 7.0, 100),
    'Unit' = c("uM", "uM", "uM"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  MetaNetis::SetAltBaseline(mock_ref)

  # Check if get give the alt reference ranges
  ref_df <- MetaNetis::GetRefRanges()
  print(ref_df)
  print(mock_ref)
  expect_true(identical(ref_df, mock_ref))

  # User data with known concentrations
  user_data <- data.frame(
    Sample_A = c(250, 9.0, 120), # low, normal, high values
    Sample_B = c(600, 6.0, 50), # normal, high, normal
    row.names = c("HMDB0000001", "HMDB0000002", "HMDB0000003")
  )

  age_vector <- c(10, 20)
  sample_type_vector <- c("Urine", "Plasma")

  # Test HMDB_ID matching
  results_id <- MetaNetis::MetabAnalysis(
    data_input = user_data,
    age = age_vector,
    sample_type = sample_type_vector,
    match_by = "HMDB_ID"
  )

  expect_true(is.data.frame(results_id))
  expect_equal(dim(results_id), dim(user_data))

  # Expected classifications
  expected_id <- data.frame(
    Sample_A = c("Low", "Missing Reference", "High"),
    Sample_B = c("Missing Reference", "Normal", "Missing Reference"),
    row.names = row.names(user_data),
    stringsAsFactors = FALSE
  )

  expect_equal(results_id, expected_id)

  # Test Metabolite_Name matching
  user_data_name <- user_data
  row.names(user_data_name) <- c("Alanine", "Glucose", "Creatinine")

  results_name <- MetaNetis::MetabAnalysis(
    data_input = user_data_name,
    age = age_vector,
    sample_type = sample_type_vector,
    match_by = "Metabolite_Name"
  )

  # Expected classifications
  expected_name <- data.frame(
    Sample_A = c("Low", "Missing Reference", "High"),
    Sample_B = c("Missing Reference", "Normal", "Missing Reference"),
    row.names = row.names(user_data_name),
    stringsAsFactors = FALSE
  )

  expect_equal(results_name, expected_name)

  # Create a simplified pathway map that corresponds to the metabolites in results_id
  # HMDB0000001 (Alanine), HMDB0000002 (Glucose), HMDB0000003 (Creatinine)
  metab_to_pwys_integrated <- data.frame(
    HMDB_ID = c("HMDB0000001", "HMDB0000002", "HMDB0000003"),
    Metabolite_Name = c("Alanine", "Glucose", "Creatinine"),
    Pathway_Name = c("Amino Acid Metabolism", "Glycolysis", "Creatine Metabolism"),
    stringsAsFactors = FALSE
  )

  # Run MapToPathway using the result from MetabAnalysis (results_id)
  result_pwy <- MetaNetis::MapToPathway(metab_results = results_id,
                                        match_by = "HMDB_ID",
                                        alt_pwy_map = metab_to_pwys_integrated)

  # Check output structure and row count
  expect_true(is.data.frame(result_pwy))
  expect_equal(nrow(result_pwy), 3 * 2) # 3 pathways * 2 samples
  expect_true(all(c("Sample_ID", "Pathway_Name", "Net_Score",
                    "Metabolites_Affected", "Activity_Status") %in% names(result_pwy)))

  # Test Amino Acid Metabolism (Alanine: HMDB0000001)
  amino_acid_results <- result_pwy %>%
    dplyr::filter(Pathway_Name == "Amino Acid Metabolism") %>%
    dplyr::arrange(Sample_ID)

  # Sample A: Low (-1). Metabolites Affected: 1
  expect_equal(amino_acid_results$Net_Score[amino_acid_results$Sample_ID == "Sample_A"], -1)
  expect_equal(amino_acid_results$Activity_Status[amino_acid_results$Sample_ID == "Sample_A"], "Mild Inhibition")
  expect_equal(amino_acid_results$Metabolites_Affected[amino_acid_results$Sample_ID == "Sample_A"], 1)

  # Sample B: Missing Reference (0). Metabolites Affected: 0
  expect_equal(amino_acid_results$Net_Score[amino_acid_results$Sample_ID == "Sample_B"], 0)
  expect_equal(amino_acid_results$Activity_Status[amino_acid_results$Sample_ID == "Sample_B"], "Normal Activity")
  expect_equal(amino_acid_results$Metabolites_Affected[amino_acid_results$Sample_ID == "Sample_B"], 1)

  # --- Test Creatine Metabolism (Creatinine: HMDB0000003) ---
  creatine_results <- result_pwy %>%
    dplyr::filter(Pathway_Name == "Creatine Metabolism") %>%
    dplyr::arrange(Sample_ID)

  # Sample A: High (+1). Metabolites Affected: 1
  expect_equal(creatine_results$Net_Score[creatine_results$Sample_ID == "Sample_A"], 1)
  expect_equal(creatine_results$Activity_Status[creatine_results$Sample_ID == "Sample_A"], "Mild Activation")
  expect_equal(creatine_results$Metabolites_Affected[creatine_results$Sample_ID == "Sample_A"], 1)

  # Sample B: Missing Reference (0). Metabolites Affected: 0
  expect_equal(creatine_results$Net_Score[creatine_results$Sample_ID == "Sample_B"], 0)
  expect_equal(creatine_results$Activity_Status[creatine_results$Sample_ID == "Sample_B"], "Normal Activity")
  expect_equal(creatine_results$Metabolites_Affected[creatine_results$Sample_ID == "Sample_B"], 1)

  # Test Glycolysis (Glucose: HMDB0000002)
  glycolysis_results <- result_pwy %>%
    dplyr::filter(Pathway_Name == "Glycolysis") %>%
    dplyr::arrange(Sample_ID)

  # Sample A: Missing Reference (0). Metabolites Affected: 0
  expect_equal(glycolysis_results$Net_Score[glycolysis_results$Sample_ID == "Sample_A"], 0)
  expect_equal(glycolysis_results$Activity_Status[glycolysis_results$Sample_ID == "Sample_A"], "Normal Activity")

  # Sample B: Normal (0). Metabolites Affected: 0
  expect_equal(glycolysis_results$Net_Score[glycolysis_results$Sample_ID == "Sample_B"], 0)
  expect_equal(glycolysis_results$Activity_Status[glycolysis_results$Sample_ID == "Sample_B"], "Normal Activity")

  # Reset the baseline back to default
  MetaNetis::SetAltBaseline()

})
