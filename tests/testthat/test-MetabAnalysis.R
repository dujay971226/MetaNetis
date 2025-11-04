test_that("MetabAnalysis returns correct classifications for HMDB_ID and Metabolite_Name", {

  # Mock reference data
  mock_ref <- data.frame(
    'HMDB_ID' = c("HMDB0000001", "HMDB0000002", "HMDB0000003"),
    'Metabolite_Name' = c("Alanine", "Glucose", "Creatinine"),
    'Sample_Type' = c("Urine", "Plasma", "Urine"),
    'Min_Age(year)' = c(0, 0, 0),
    'Max_Age(year)' = c(99, 99, 99),
    'Min_Concentration(Healthy)' = c(500, 4.0, 50),
    'Mean_Concentration(Healthy)' = c(750, 5.5, 75),
    'Max_Concentration(Healthy)' = c(1000, 7.0, 100),
    'Unit' = c("uM", "uM", "uM"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # User data with known concentrations
  user_data <- data.frame(
    Sample_A = c(250, 9.0, 120), # low, normal, high values
    Sample_B = c(600, 6.0, 50), # normal, high, normal
    row.names = c("HMDB0000001", "HMDB0000002", "HMDB0000003")
  )

  age_vector <- c(10, 20)
  sample_type_vector <- c("Urine", "Plasma")

  # Test HMDB_ID matching
  results_id <- MetabAnalysis(
    data_input = user_data,
    age = age_vector,
    sample_type = sample_type_vector,
    match_by = "HMDB_ID",
    ref_data_override = mock_ref
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

  results_name <- MetabAnalysis(
    data_input = user_data_name,
    age = age_vector,
    sample_type = sample_type_vector,
    match_by = "Metabolite_Name",
    ref_data_override = mock_ref
  )

  # Expected classifications
  expected_name <- data.frame(
    Sample_A = c("Low", "Missing Reference", "High"),
    Sample_B = c("Missing Reference", "Normal", "Missing Reference"),
    row.names = row.names(user_data_name),
    stringsAsFactors = FALSE
  )

  expect_equal(results_name, expected_name)

})
