reference_ranges_df <- data.frame(
  # Identity Columns
  HMDB_ID = c("HMDB0000190", "HMDB0000064", "HMDB0000001", "HMDB0001918"),
  Metabolite_Name = c("L-Tryptophan", "Creatinine", "1-Methylhistidine", "Glucose"),
  Sample_Type = c("Blood/Serum/Plasma", "Urine", "CSF", "Blood/Serum/Plasma"),
  Subject_Age_Description = c("Adult (>18 years old)", "Newborn (0-30 days old)", "Adolescent (13-18 years old)", "Children (1-13 years old)"),

  # Age Columns (Numeric with units in the name)
  `Min_Age (year)` = c(18.0, 0.0, 13.0, 1.0),
  `Max_Age (year)` = c(Inf, 0.082, 18.0, 13.0), # 30 days is ~0.082 years

  # Concentration Columns (Numeric with context in the name) - STANDARDIZED UNITS
  # Row 1 (uM): No change
  # Row 2 (umol/mmol creatinine): No change
  # Row 3 (CSF): Converted from 12-30 nmol/L -> 0.012-0.030 uM (Divided by 1000)
  # Row 4 (Blood/Serum/Plasma): Converted from 3.5-8.0 mmol/L -> 3500-8000 uM (Multiplied by 1000)
  `Min Concentration (Healthy)` = c(50.0, 0.15, 0.012, 3500.0),
  `Mean (Healthy)` = c(75.5, 0.25, 0.021, 5200.0),
  `Max Concentration (Healthy)` = c(100.0, 0.50, 0.030, 8000.0),

  # Unit Column - NOW STANDARDIZED
  `Concentration Unit` = c("uM", "umol/mmol creatinine", "uM", "uM"),

  stringsAsFactors = FALSE
)
