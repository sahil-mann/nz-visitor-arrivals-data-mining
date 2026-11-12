# Remote Data Miners — Dataset 1 data management
# This script preserves every observation and creates analysis-ready variables.

library(tidyverse)
library(readxl)

raw_path <- file.path("data", "raw", "visitor_arrivals_transformed.xlsx")

if (!file.exists(raw_path)) {
  stop("Raw workbook not found. Open RemoteDataMiners.Rproj before running this script.")
}

raw_data <- read_excel(raw_path, sheet = "flattened dataset")

age_levels <- c(
  "0-4 years", "5-9 years", "10-14 years", "15-19 years",
  "20-24 years", "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years", "50-54 years", "55-59 years",
  "60-64 years", "65-69 years", "70-74 years", "75 years and over"
)

clean_data <- raw_data %>%
  rename(
    YearMonth = Month,
    Country = `Country of Residence`,
    TravelPurpose = `Travel Purpose`,
    VisitorArrivals = `Visitor Arrivals`
  ) %>%
  mutate(
    Date = as.Date(paste0(str_replace(YearMonth, "M", "-"), "-01")),
    Year = as.integer(format(Date, "%Y")),
    MonthNumber = as.integer(format(Date, "%m")),
    MonthName = factor(month.abb[MonthNumber], levels = month.abb, ordered = TRUE),
    TimeIndex = match(Date, sort(unique(Date))),
    CountryShort = case_when(
      Country == "China, People's Republic of" ~ "China",
      Country == "Korea, Republic of" ~ "South Korea",
      Country == "United States of America" ~ "United States",
      TRUE ~ Country
    ),
    Country = factor(Country),
    CountryShort = factor(CountryShort),
    TravelPurpose = factor(TravelPurpose),
    Age = factor(Age, levels = age_levels, ordered = TRUE),
    Sex = factor(Sex)
  ) %>%
  select(
    YearMonth, Date, TimeIndex, Year, MonthNumber, MonthName,
    Country, CountryShort, TravelPurpose, Age, Sex, VisitorArrivals
  )

# Cleaning must not silently remove or alter the original observations.
stopifnot(
  nrow(clean_data) == nrow(raw_data),
  sum(is.na(clean_data)) == 0,
  all(clean_data$VisitorArrivals >= 0),
  n_distinct(clean_data$TimeIndex) == n_distinct(clean_data$Date)
)

glimpse(clean_data)
summary(clean_data)
table(clean_data$CountryShort)
table(clean_data$TravelPurpose)
table(clean_data$Age)
table(clean_data$Sex)

# RDS retains ordered factors; CSV is convenient for sharing and inspection.
write_rds(clean_data, file.path("data", "processed", "visitor_arrivals_clean.rds"))
write_csv(clean_data, file.path("data", "processed", "visitor_arrivals_clean.csv"))

message("Created analysis-ready data without deleting any rows.")

