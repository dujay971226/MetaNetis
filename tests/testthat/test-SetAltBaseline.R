test_that("SetAltBaseline correctly updates baseline reference data", {
  # Mock reference dataframe
  ref_df <- data.frame(
    HMDB_ID = c("HMDB0000190", "HMDB0000064", "HMDB0000001", "HMDB0001918"),
    Metabolite_Name = c("L-Tryptophan", "Creatinine",
                        "1-Methylhistidine", "Glucose"),
    Sample_Type = c("Blood/Serum/Plasma", "Urine", "CSF", "Blood/Serum/Plasma"),
    Subject_Age_Description = c(
      "Adult (>18 years old)",
      "Newborn (0-30 days old)",
      "Adolescent (13-18 years old)",
      "Children (1-13 years old)"
    ),
    `Min_Age(year)` = c(18.0, 0.0, 13.0, 1.0),
    `Max_Age(year)` = c(Inf, 0.082, 18.0, 13.0),
    `Min_Concentration(Healthy)` = c(50.0, 0.15, 0.012, 3500.0),
    `Mean_Concentration(Healthy)` = c(75.5, 0.25, 0.021, 5200.0),
    `Max_Concentration(Healthy)` = c(100.0, 0.50, 0.030, 8000.0),
    Unit = c("uM", "umol/mmol creatinine", "uM", "uM"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Run SetAltBaseline
  SetAltBaseline(ref_df)

  # Retrieve it
  result <- GetRefRanges()

  # ---- Validation ----
  expect_s3_class(result, "data.frame")
  expect_true(all(c("HMDB_ID", "Metabolite_Name") %in% names(result)))

  # Check if custom data has been set
  expect_equal(nrow(result), nrow(ref_df))
  expect_equal(result$HMDB_ID, ref_df$HMDB_ID)
  expect_equal(result$Metabolite_Name, ref_df$Metabolite_Name)

  # Numeric values should match
  expect_equal(
    result$`Max_Concentration(Healthy)`, ref_df$`Max_Concentration(Healthy)`)
  expect_equal(
    result$`Min_Concentration(Healthy)`, ref_df$`Min_Concentration(Healthy)`)

  # Unit consistency check
  expect_true(all(result$Unit %in% c("uM", "umol/mmol creatinine")))

  # Set Original Back
  SetAltBaseline()
})

# [END]
