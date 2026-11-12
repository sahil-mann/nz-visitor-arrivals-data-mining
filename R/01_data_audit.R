# Remote Data Miners — Dataset 1 data-quality audit
# Run this script from the RemoteDataMiners RStudio Project.

library(tidyverse)
library(readxl)

raw_path <- file.path("data", "raw", "visitor_arrivals_transformed.xlsx")

if (!file.exists(raw_path)) {
  stop("Raw workbook not found. Open RemoteDataMiners.Rproj before running this script.")
}

# The workbook contains the analysis table and the original matrix layout.
print(excel_sheets(raw_path))

raw_data <- read_excel(raw_path, sheet = "flattened dataset")

# Course-style first inspection (Module 0).
dim(raw_data)
head(raw_data)
tail(raw_data)
str(raw_data)
summary(raw_data)
glimpse(raw_data)

# Short working names make the checks easier to read.
audit_data <- raw_data %>%
  rename(
    Country = `Country of Residence`,
    TravelPurpose = `Travel Purpose`,
    VisitorArrivals = `Visitor Arrivals`
  ) %>%
  mutate(
    Date = as.Date(paste0(str_replace(Month, "M", "-"), "-01"))
  )

key_columns <- c("Month", "Country", "TravelPurpose", "Age", "Sex")
expected_rows_per_month <-
  n_distinct(audit_data$Country) *
  n_distinct(audit_data$TravelPurpose) *
  n_distinct(audit_data$Age) *
  n_distinct(audit_data$Sex)

missing_cells <- sum(is.na(audit_data))
blank_cells <- audit_data %>%
  select(where(is.character)) %>%
  summarise(across(everything(), ~ sum(trimws(.x) == ""))) %>%
  unlist() %>%
  sum()

exact_duplicate_rows <- sum(duplicated(audit_data))
duplicate_observation_keys <- sum(duplicated(audit_data[key_columns]))
invalid_month_labels <- sum(
  !str_detect(audit_data$Month, "^[0-9]{4}M(0[1-9]|1[0-2])$")
)

expected_months <- seq(
  min(audit_data$Date),
  max(audit_data$Date),
  by = "month"
)

missing_months <- length(setdiff(expected_months, unique(audit_data$Date)))

monthly_coverage <- audit_data %>%
  group_by(Month, Date) %>%
  summarise(Rows = n(), .groups = "drop") %>%
  mutate(
    ExpectedRows = expected_rows_per_month,
    Complete = Rows == ExpectedRows
  )

checks <- tibble(
  Check = c(
    "No missing cells",
    "No blank text cells",
    "No exact duplicate rows",
    "One row per observation key",
    "All Month labels are valid",
    "No months missing from the sequence",
    "Every month has the complete category grid",
    "Visitor Arrivals are non-negative",
    "Visitor Arrivals are whole numbers"
  ),
  Result = c(
    missing_cells,
    blank_cells,
    exact_duplicate_rows,
    duplicate_observation_keys,
    invalid_month_labels,
    missing_months,
    sum(!monthly_coverage$Complete),
    sum(audit_data$VisitorArrivals < 0, na.rm = TRUE),
    sum(abs(audit_data$VisitorArrivals - round(audit_data$VisitorArrivals)) > 1e-9,
        na.rm = TRUE)
  ),
  Passed = Result == 0
)

data_profile <- tibble(
  Metric = c(
    "Rows", "Columns", "First month", "Last month", "Distinct months",
    "Countries", "Travel purposes", "Age groups", "Sex categories",
    "Rows per complete month", "Zero arrival rows", "Zero arrival percent",
    "Minimum arrivals", "Median arrivals", "Mean arrivals", "Maximum arrivals"
  ),
  Value = c(
    nrow(audit_data),
    ncol(raw_data),
    format(min(audit_data$Date), "%Y-%m"),
    format(max(audit_data$Date), "%Y-%m"),
    n_distinct(audit_data$Date),
    n_distinct(audit_data$Country),
    n_distinct(audit_data$TravelPurpose),
    n_distinct(audit_data$Age),
    n_distinct(audit_data$Sex),
    expected_rows_per_month,
    sum(audit_data$VisitorArrivals == 0),
    round(100 * mean(audit_data$VisitorArrivals == 0), 1),
    min(audit_data$VisitorArrivals),
    median(audit_data$VisitorArrivals),
    round(mean(audit_data$VisitorArrivals), 1),
    max(audit_data$VisitorArrivals)
  )
)

category_levels <- bind_rows(
  tibble(Variable = "Country", Level = sort(unique(audit_data$Country))),
  tibble(Variable = "TravelPurpose", Level = sort(unique(audit_data$TravelPurpose))),
  tibble(Variable = "Age", Level = unique(audit_data$Age)),
  tibble(Variable = "Sex", Level = sort(unique(audit_data$Sex)))
)

print(checks)
print(data_profile)

write_csv(checks, file.path("output", "data_quality_checks.csv"))
write_csv(data_profile, file.path("output", "data_profile.csv"))
write_csv(category_levels, file.path("output", "category_levels.csv"))
write_csv(monthly_coverage, file.path("output", "monthly_coverage.csv"))

if (!all(checks$Passed)) {
  warning("At least one data-quality check failed. Review output/data_quality_checks.csv.")
}

