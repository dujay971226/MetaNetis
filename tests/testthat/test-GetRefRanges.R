test_that("GetRefRanges runs correctly and returns valid structure", {
  ref_df <- GetRefRanges()

  # Basic structure checks
  expect_s3_class(ref_df, "data.frame")
  expect_true(nrow(ref_df) > 1000)
  expect_true(all(c(
    "HMDB_ID", "Metabolite_Name", "Sample_Type",
    "Subject_Age_Description", "Min_Age(year)",
    "Max_Age(year)", "Min_Concentration(Healthy)",
    "Mean_Concentration(Healthy)", "Max_Concentration(Healthy)", "Unit"
  ) %in% colnames(ref_df)))

  # Data type checks
  expect_type(ref_df$HMDB_ID, "character")
  expect_type(ref_df$Metabolite_Name, "character")
  expect_type(ref_df$`Min_Concentration(Healthy)`, "double")
  expect_type(ref_df$`Mean_Concentration(Healthy)`, "double")
  expect_type(ref_df$`Max_Concentration(Healthy)`, "double")

  # Logical sanity checks
  expect_true(all(ref_df$`Min_Concentration(Healthy)` <=
                    ref_df$`Max_Concentration(Healthy)`))
  expect_true(all(
    is.finite(ref_df$`Mean_Concentration(Healthy)`) |
      is.na(ref_df$`Mean_Concentration(Healthy)`)
  ))

  # Units should be non-empty
  expect_true(all(nchar(ref_df$Unit) > 0))
})

# [END]
