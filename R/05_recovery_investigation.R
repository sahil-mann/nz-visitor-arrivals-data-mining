# Remote Data Miners — iterative investigation of recovery and composition
# Course methods: dplyr summaries and ggplot2 EDA from Module 0.
# This script does not fit a model. It creates the evidence that motivates
# the next regression/classification question.

library(tidyverse)

clean_path <- file.path("data", "processed", "visitor_arrivals_clean.rds")

if (!file.exists(clean_path)) {
  stop("Processed data not found. Run R/02_clean_data.R first.")
}

visitor_data <- read_rds(clean_path)

stopifnot(
  all(c(2019, 2025) %in% unique(visitor_data$Year)),
  n_distinct(filter(visitor_data, Year == 2019)$MonthNumber) == 12,
  n_distinct(filter(visitor_data, Year == 2025)$MonthNumber) == 12
)

# -----------------------------------------------------------------------------
# Profile the response in analytically meaningful periods.
# The official source identifies 1 August 2022 as reopening to all visitors.
# -----------------------------------------------------------------------------

period_profile <- visitor_data %>%
  mutate(
    Period = case_when(
      Date <= as.Date("2020-02-01") ~ "Before disruption",
      Date <= as.Date("2022-07-01") ~ "Restricted-border period",
      TRUE ~ "After full reopening"
    )
  ) %>%
  group_by(Period) %>%
  summarise(
    FirstMonth = min(Date),
    LastMonth = max(Date),
    Rows = n(),
    EstimatedPassengerMovements = sum(VisitorArrivals),
    MedianCellEstimate = median(VisitorArrivals),
    MeanCellEstimate = mean(VisitorArrivals),
    ZeroEstimatePercent = 100 * mean(VisitorArrivals == 0),
    MaximumCellEstimate = max(VisitorArrivals),
    .groups = "drop"
  )

# The full Age × Sex grid is useful for EDA, but Country × Purpose monthly
# aggregates reduce small-cell sampling noise for the recovery investigation.
monthly_segment <- visitor_data %>%
  group_by(
    Date, Year, MonthNumber, MonthName,
    CountryShort, TravelPurpose
  ) %>%
  summarise(
    VisitorArrivals = sum(VisitorArrivals),
    .groups = "drop"
  )

monthly_all_eight <- monthly_segment %>%
  group_by(Date, Year, MonthNumber, MonthName) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop")

# -----------------------------------------------------------------------------
# Question 1: Did the total recover, or did the composition change underneath?
# Compare the complete years 2019 and 2025 and preserve calendar-month matching.
# -----------------------------------------------------------------------------

annual_country <- monthly_segment %>%
  group_by(Year, CountryShort) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(SharePercent = 100 * VisitorArrivals / sum(VisitorArrivals)) %>%
  ungroup()

annual_purpose <- monthly_segment %>%
  group_by(Year, TravelPurpose) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(SharePercent = 100 * VisitorArrivals / sum(VisitorArrivals)) %>%
  ungroup()

annual_country_purpose <- monthly_segment %>%
  group_by(Year, CountryShort, TravelPurpose) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop")

country_recovery <- annual_country %>%
  filter(Year %in% c(2019, 2025)) %>%
  select(Year, CountryShort, VisitorArrivals, SharePercent) %>%
  pivot_wider(
    names_from = Year,
    values_from = c(VisitorArrivals, SharePercent),
    names_sep = "_"
  ) %>%
  mutate(
    RecoveryPercent = 100 * VisitorArrivals_2025 / VisitorArrivals_2019,
    ShareChangePoints = SharePercent_2025 - SharePercent_2019
  ) %>%
  arrange(desc(RecoveryPercent))

purpose_recovery <- annual_purpose %>%
  filter(Year %in% c(2019, 2025)) %>%
  select(Year, TravelPurpose, VisitorArrivals, SharePercent) %>%
  pivot_wider(
    names_from = Year,
    values_from = c(VisitorArrivals, SharePercent),
    names_sep = "_"
  ) %>%
  mutate(
    RecoveryPercent = 100 * VisitorArrivals_2025 / VisitorArrivals_2019,
    ShareChangePoints = SharePercent_2025 - SharePercent_2019
  ) %>%
  arrange(desc(RecoveryPercent))

country_purpose_recovery <- annual_country_purpose %>%
  filter(Year %in% c(2019, 2025)) %>%
  pivot_wider(
    names_from = Year,
    values_from = VisitorArrivals,
    names_prefix = "VisitorArrivals_"
  ) %>%
  mutate(
    RecoveryPercent = 100 * VisitorArrivals_2025 / VisitorArrivals_2019
  ) %>%
  arrange(desc(RecoveryPercent))

aggregate_recovery <- annual_country %>%
  filter(Year %in% c(2019, 2025)) %>%
  group_by(Year) %>%
  summarise(
    AllEight = sum(VisitorArrivals),
    ExcludingAustralia = sum(VisitorArrivals[CountryShort != "Australia"]),
    Australia = sum(VisitorArrivals[CountryShort == "Australia"]),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(AllEight, ExcludingAustralia, Australia),
    names_to = "MarketView",
    values_to = "VisitorArrivals"
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = VisitorArrivals,
    names_prefix = "VisitorArrivals_"
  ) %>%
  mutate(
    RecoveryPercent = 100 * VisitorArrivals_2025 / VisitorArrivals_2019
  )

# Concentration is an EDA description, not a separate modelling method.
# HHI increases when arrivals are concentrated in fewer represented countries.
country_concentration <- annual_country %>%
  group_by(Year) %>%
  summarise(
    AustraliaSharePercent = SharePercent[CountryShort == "Australia"],
    HHI = sum((SharePercent / 100)^2) * 10000,
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Question 2: Which country-purpose markets recovered relative to their own
# 2019 seasonal benchmark after the border reopened?
# -----------------------------------------------------------------------------

baseline_2019 <- monthly_segment %>%
  filter(Year == 2019) %>%
  select(
    CountryShort, TravelPurpose, MonthNumber,
    Baseline2019 = VisitorArrivals
  )

recovery_monthly <- monthly_segment %>%
  filter(Date >= as.Date("2022-08-01")) %>%
  left_join(
    baseline_2019,
    by = c("CountryShort", "TravelPurpose", "MonthNumber")
  ) %>%
  arrange(Date, CountryShort, TravelPurpose) %>%
  mutate(
    MonthsSinceReopening = match(Date, sort(unique(Date))) - 1L,
    RecoveryPercent = 100 * VisitorArrivals / Baseline2019,
    Recovered = VisitorArrivals >= Baseline2019
  )

stopifnot(
  sum(is.na(recovery_monthly$Baseline2019)) == 0,
  all(recovery_monthly$Baseline2019 > 0)
)

recovery_status_summary <- recovery_monthly %>%
  group_by(Year) %>%
  summarise(
    SegmentMonths = n(),
    RecoveredSegmentMonths = sum(Recovered),
    RecoveredPercent = 100 * mean(Recovered),
    MedianRecoveryPercent = median(RecoveryPercent),
    .groups = "drop"
  )

# Measure how much of each annual country-purpose seasonal profile moved to
# different calendar months between 2019 and 2025.
seasonal_profiles <- monthly_segment %>%
  filter(Year %in% c(2019, 2025)) %>%
  group_by(Year, CountryShort, TravelPurpose) %>%
  mutate(AnnualShare = VisitorArrivals / sum(VisitorArrivals)) %>%
  ungroup() %>%
  select(Year, CountryShort, TravelPurpose, MonthNumber, AnnualShare) %>%
  pivot_wider(
    names_from = Year,
    values_from = AnnualShare,
    names_prefix = "AnnualShare_"
  )

seasonal_profile_change <- seasonal_profiles %>%
  group_by(CountryShort, TravelPurpose) %>%
  summarise(
    SeasonalRedistributionPercent =
      100 * 0.5 * sum(abs(AnnualShare_2025 - AnnualShare_2019)),
    .groups = "drop"
  ) %>%
  arrange(desc(SeasonalRedistributionPercent))

# -----------------------------------------------------------------------------
# Save reproducible evidence tables.
# -----------------------------------------------------------------------------

write_csv(period_profile, file.path("output", "period_profile.csv"))
write_csv(annual_country, file.path("output", "annual_country.csv"))
write_csv(annual_purpose, file.path("output", "annual_purpose.csv"))
write_csv(country_recovery, file.path("output", "country_recovery_2019_2025.csv"))
write_csv(purpose_recovery, file.path("output", "purpose_recovery_2019_2025.csv"))
write_csv(
  country_purpose_recovery,
  file.path("output", "country_purpose_recovery_2019_2025.csv")
)
write_csv(aggregate_recovery, file.path("output", "aggregate_recovery_2019_2025.csv"))
write_csv(country_concentration, file.path("output", "country_concentration_by_year.csv"))
write_csv(recovery_monthly, file.path("output", "recovery_classification_data.csv"))
write_csv(recovery_status_summary, file.path("output", "recovery_status_by_year.csv"))
write_csv(seasonal_profile_change, file.path("output", "seasonal_profile_change.csv"))

print(country_recovery)
print(purpose_recovery)
print(country_purpose_recovery)
print(aggregate_recovery)
print(recovery_status_summary)
print(seasonal_profile_change)

# -----------------------------------------------------------------------------
# Chart contracts
# 1. Recovery trend: monthly aggregate, matched with the same month of 2019.
# 2. Country comparison: 8 sorted categories against a 100% recovery benchmark.
# 3. Country × Purpose matrix: 24 comparable recovery estimates.
# 4. Country composition: paired dots compare 2019 and 2025 shares.
# -----------------------------------------------------------------------------

blue <- "#1F6F8B"
orange <- "#D47A3A"
gold <- "#D6A43A"
ink <- "#1F2D35"
muted <- "#586A73"
grid_colour <- "#DDE4E7"

project_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(colour = ink, face = "bold", size = 17),
    plot.subtitle = element_text(colour = muted, size = 11),
    axis.title = element_text(colour = ink),
    axis.text = element_text(colour = ink),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = grid_colour, linewidth = 0.35),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

aggregate_baseline <- monthly_all_eight %>%
  filter(Year == 2019) %>%
  select(MonthNumber, Baseline2019 = VisitorArrivals)

monthly_recovery_index <- monthly_all_eight %>%
  filter(Date >= as.Date("2022-08-01")) %>%
  left_join(aggregate_baseline, by = "MonthNumber") %>%
  mutate(RecoveryPercent = 100 * VisitorArrivals / Baseline2019)

recovery_trend_plot <- ggplot(
  monthly_recovery_index,
  aes(x = Date, y = RecoveryPercent)
) +
  geom_hline(yintercept = 100, colour = muted, linetype = "dashed", linewidth = 0.7) +
  geom_line(colour = blue, linewidth = 1.05) +
  geom_point(
    data = filter(monthly_recovery_index, RecoveryPercent >= 100),
    colour = gold,
    size = 1.8
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Monthly recovery relative to the same month in 2019",
    subtitle = "All eight represented countries and three purposes; 100% means the matching 2019 month",
    x = NULL,
    y = "Recovery index"
  ) +
  project_theme

country_recovery_plot_data <- country_recovery %>%
  mutate(CountryShort = fct_reorder(CountryShort, RecoveryPercent))

country_recovery_plot <- ggplot(
  country_recovery_plot_data,
  aes(x = RecoveryPercent, y = CountryShort)
) +
  geom_vline(xintercept = 100, colour = muted, linetype = "dashed", linewidth = 0.7) +
  geom_segment(
    aes(x = 0, xend = RecoveryPercent, yend = CountryShort),
    colour = "#B8C7CD",
    linewidth = 1.2
  ) +
  geom_point(aes(colour = RecoveryPercent >= 100), size = 5) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, scale = 1)(RecoveryPercent)),
    hjust = -0.25,
    colour = ink,
    size = 3.7
  ) +
  scale_colour_manual(values = c("TRUE" = blue, "FALSE" = orange), guide = "none") +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = "Country totals in 2025 relative to 2019",
    subtitle = "Complete calendar years; 100% indicates the 2019 level was restored",
    x = "2025 arrivals as a percentage of 2019",
    y = NULL
  ) +
  project_theme +
  theme(panel.grid.major.y = element_blank())

heatmap_plot <- ggplot(
  country_purpose_recovery,
  aes(x = TravelPurpose, y = CountryShort, fill = RecoveryPercent)
) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 1, scale = 1)(RecoveryPercent)),
    colour = ink,
    size = 3.6,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#F2C6AE",
    mid = "#F4F1EA",
    high = "#A9D4E0",
    midpoint = 100,
    labels = scales::label_percent(scale = 1),
    name = "Recovery"
  ) +
  labs(
    title = "Recovery differs across country–purpose combinations",
    subtitle = "Estimated passenger movements in 2025 as a percentage of 2019; complete calendar years",
    x = NULL,
    y = NULL
  ) +
  project_theme +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.position = "top"
  )

country_share_plot_data <- annual_country %>%
  filter(Year %in% c(2019, 2025)) %>%
  mutate(
    CountryShort = fct_reorder(CountryShort, SharePercent, .fun = max)
  )

country_share_plot <- ggplot(
  country_share_plot_data,
  aes(x = SharePercent, y = CountryShort, group = CountryShort)
) +
  geom_line(colour = "#AEBCC2", linewidth = 1.1) +
  geom_point(aes(colour = factor(Year)), size = 4.4) +
  scale_colour_manual(values = c("2019" = orange, "2025" = blue), name = NULL) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Country composition in 2019 and 2025",
    subtitle = "Share of estimated arrivals across the eight represented countries",
    x = "Share of represented arrivals",
    y = NULL
  ) +
  project_theme +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

ggsave(
  file.path("output", "figures", "07_monthly_recovery_index.png"),
  recovery_trend_plot,
  width = 11,
  height = 6,
  dpi = 200,
  bg = "white"
)

ggsave(
  file.path("output", "figures", "08_country_recovery_2019_2025.png"),
  country_recovery_plot,
  width = 9.5,
  height = 6.2,
  dpi = 200,
  bg = "white"
)

ggsave(
  file.path("output", "figures", "09_country_purpose_recovery_heatmap.png"),
  heatmap_plot,
  width = 10.5,
  height = 6.3,
  dpi = 200,
  bg = "white"
)

ggsave(
  file.path("output", "figures", "10_country_share_shift.png"),
  country_share_plot,
  width = 9.5,
  height = 6.2,
  dpi = 200,
  bg = "white"
)

message("Recovery investigation tables and four figures created.")

