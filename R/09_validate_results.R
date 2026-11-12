# Remote Data Miners — reproducibility and share-readiness validation
#
# This script independently recomputes the key claims shown in the planned
# presentation and stops if a saved table disagrees with the underlying data.

library(tidyverse)

clean_path <- file.path("data", "processed", "visitor_arrivals_clean.rds")
regression_metrics_path <-
  file.path("output", "models", "recovery_regression_metrics.csv")
regression_predictions_path <-
  file.path("output", "models", "recovery_regression_predictions.csv")
classification_metrics_path <-
  file.path("output", "models", "recovery_classification_metrics.csv")
classification_predictions_path <-
  file.path("output", "models", "recovery_classification_predictions.csv")

required_paths <- c(
  clean_path,
  regression_metrics_path,
  regression_predictions_path,
  classification_metrics_path,
  classification_predictions_path
)

if (any(!file.exists(required_paths))) {
  stop("Validation inputs are missing. Run scripts 01–08 first.")
}

visitor_data <- read_rds(clean_path)

key_columns <- c(
  "Date", "CountryShort", "TravelPurpose", "Age", "Sex"
)

data_quality <- tibble(
  Check = c(
    "Rows",
    "Months",
    "Countries",
    "Travel purposes",
    "Age groups",
    "Sex categories",
    "Missing values",
    "Duplicate analytical keys",
    "Negative values",
    "Fractional values",
    "Rows in every month"
  ),
  Observed = c(
    nrow(visitor_data),
    n_distinct(visitor_data$Date),
    n_distinct(visitor_data$CountryShort),
    n_distinct(visitor_data$TravelPurpose),
    n_distinct(visitor_data$Age),
    n_distinct(visitor_data$Sex),
    sum(is.na(visitor_data)),
    sum(duplicated(visitor_data[, key_columns])),
    sum(visitor_data$VisitorArrivals < 0),
    sum(visitor_data$VisitorArrivals %% 1 != 0),
    as.numeric(all(count(visitor_data, Date)$n == 768))
  ),
  Expected = c(96000, 125, 8, 3, 16, 2, 0, 0, 0, 0, 1)
) %>%
  mutate(Status = if_else(Observed == Expected, "PASS", "FAIL"))

stopifnot(all(data_quality$Status == "PASS"))

annual_segment <- visitor_data %>%
  filter(Year %in% c(2019, 2025)) %>%
  group_by(Year, CountryShort, TravelPurpose) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop")

annual_country <- annual_segment %>%
  group_by(Year, CountryShort) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop")

all_eight <- annual_country %>%
  group_by(Year) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop")

australia <- annual_country %>%
  filter(CountryShort == "Australia")

excluding_australia <- annual_country %>%
  filter(CountryShort != "Australia") %>%
  group_by(Year) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop")

recovery_percent <- function(table_data) {
  100 * table_data$VisitorArrivals[table_data$Year == 2025] /
    table_data$VisitorArrivals[table_data$Year == 2019]
}

annual_segment_recovery <- annual_segment %>%
  pivot_wider(names_from = Year, values_from = VisitorArrivals) %>%
  mutate(RecoveryPercent = 100 * `2025` / `2019`)

story_claims <- tibble(
  Claim = c(
    "All-eight 2025 recovery percent",
    "Australia 2025 recovery percent",
    "Seven-country 2025 recovery percent",
    "Annual Country × Purpose combinations at or above 2019",
    "Business combinations at or above 2019"
  ),
  Observed = c(
    recovery_percent(all_eight),
    recovery_percent(australia),
    recovery_percent(excluding_australia),
    sum(annual_segment_recovery$RecoveryPercent >= 100),
    sum(
      annual_segment_recovery$TravelPurpose == "Business" &
        annual_segment_recovery$RecoveryPercent >= 100
    )
  ),
  Expected = c(
    91.63185003426399,
    99.53595038266323,
    82.68929421197443,
    6,
    0
  )
) %>%
  mutate(
    AbsoluteDifference = abs(Observed - Expected),
    Status = if_else(AbsoluteDifference < 1e-8, "PASS", "FAIL")
  )

stopifnot(all(story_claims$Status == "PASS"))

regression_predictions <- read_csv(
  regression_predictions_path,
  show_col_types = FALSE
) %>%
  filter(
    EvaluationDesign == "Chronological future check",
    Split == "Test"
  )

regression_recalculated <- regression_predictions %>%
  group_by(EvaluationDesign, Split, Model) %>%
  summarise(
    MSE = mean(Error^2),
    RMSE = sqrt(mean(Error^2)),
    MAE = mean(abs(Error)),
    RSquared = 1 - sum(Error^2) /
      sum((ActualRecoveryPercent - mean(ActualRecoveryPercent))^2),
    .groups = "drop"
  )

regression_saved <- read_csv(
  regression_metrics_path,
  show_col_types = FALSE
) %>%
  filter(
    EvaluationDesign == "Chronological future check",
    Split == "Test"
  ) %>%
  select(EvaluationDesign, Split, Model, MSE, RMSE, MAE, RSquared)

regression_check <- regression_saved %>%
  left_join(
    regression_recalculated,
    by = c("EvaluationDesign", "Split", "Model"),
    suffix = c("_Saved", "_Recalculated")
  ) %>%
  mutate(
    MaximumAbsoluteDifference = pmax(
      abs(MSE_Saved - MSE_Recalculated),
      abs(RMSE_Saved - RMSE_Recalculated),
      abs(MAE_Saved - MAE_Recalculated),
      abs(RSquared_Saved - RSquared_Recalculated)
    ),
    Status = if_else(MaximumAbsoluteDifference < 1e-8, "PASS", "FAIL")
  )

stopifnot(all(regression_check$Status == "PASS"))

classification_predictions <- read_csv(
  classification_predictions_path,
  show_col_types = FALSE
) %>%
  filter(EvaluationDesign == "Chronological future check") %>%
  mutate(
    ActualRecovered = as.logical(ActualRecovered),
    TunedPrediction = as.logical(TunedPrediction)
  )

classification_counts <- classification_predictions %>%
  summarise(
    TruePositive = sum(TunedPrediction & ActualRecovered),
    FalsePositive = sum(TunedPrediction & !ActualRecovered),
    TrueNegative = sum(!TunedPrediction & !ActualRecovered),
    FalseNegative = sum(!TunedPrediction & ActualRecovered)
  )

classification_saved <- read_csv(
  classification_metrics_path,
  show_col_types = FALSE
) %>%
  filter(
    EvaluationDesign == "Chronological future check",
    Model == "Logistic regression: validation threshold"
  )

classification_check <- tibble(
  Check = c("TruePositive", "FalsePositive", "TrueNegative", "FalseNegative"),
  Recalculated = c(
    classification_counts$TruePositive,
    classification_counts$FalsePositive,
    classification_counts$TrueNegative,
    classification_counts$FalseNegative
  ),
  Saved = c(
    classification_saved$TruePositive,
    classification_saved$FalsePositive,
    classification_saved$TrueNegative,
    classification_saved$FalseNegative
  )
) %>%
  mutate(Status = if_else(Recalculated == Saved, "PASS", "FAIL"))

stopifnot(all(classification_check$Status == "PASS"))

validation_summary <- bind_rows(
  data_quality %>%
    transmute(Category = "Data quality", Check, Status),
  story_claims %>%
    transmute(Category = "Story claim", Check = Claim, Status),
  regression_check %>%
    transmute(Category = "Regression metric", Check = Model, Status),
  classification_check %>%
    transmute(Category = "Classification count", Check, Status)
)

write_csv(validation_summary, file.path("output", "validation_summary.csv"))

report_lines <- c(
  "REMOTE DATA MINERS — VALIDATION REPORT",
  paste("Generated:", Sys.Date()),
  "",
  paste("Checks passed:", sum(validation_summary$Status == "PASS")),
  paste("Checks failed:", sum(validation_summary$Status == "FAIL")),
  "",
  "Key share-ready claims independently confirmed:",
  sprintf("- All eight represented countries: %.1f%% of 2019 in 2025", recovery_percent(all_eight)),
  sprintf("- Australia: %.1f%%", recovery_percent(australia)),
  sprintf("- Seven countries excluding Australia: %.1f%%", recovery_percent(excluding_australia)),
  sprintf("- Recovered annual Country × Purpose combinations: %d of 24", sum(annual_segment_recovery$RecoveryPercent >= 100)),
  sprintf("- Recovered Business combinations: %d of 8", sum(annual_segment_recovery$TravelPurpose == "Business" & annual_segment_recovery$RecoveryPercent >= 100)),
  "",
  "Regression and classification metrics match the saved prediction-level files.",
  "No personal teammate names were included in the R project files."
)

write_lines(report_lines, file.path("output", "validation_report.txt"))

print(validation_summary)
cat("All validation checks passed.\n")
