test_that("GetPathwayMap runs correctly and returns valid structure", {

  # Fetch the data frame
  map_df <- GetPathwayMap()

  # 1. Basic structure checks
  expect_s3_class(map_df, "data.frame")
  # Expect a reasonable number of mappings, but check for > 1 to confirm content exists
  expect_true(nrow(map_df) > 1)

  # Check for all expected columns in the final output
  expected_cols <- c("Metabolite", "HMDB_ID", "Pathway")
  expect_true(all(expected_cols %in% colnames(map_df)))

  # 2. Data type checks
  # All three columns are expected to contain text/identifiers
  expect_type(map_df$Metabolite, "character")
  expect_type(map_df$HMDB_ID, "character")
  expect_type(map_df$Pathway, "character")

  # 3. Logical sanity checks
  # IDs should be non-empty strings
  expect_true(all(nchar(map_df$HMDB_ID) > 0))

  # Pathway names should be non-empty strings
  expect_true(all(nchar(map_df$Pathway) > 0))
})
