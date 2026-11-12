# Remote Data Miners — Dataset 1 exploratory analysis
# Course methods: group_by(), summarise(), arrange(), mutate(), and ggplot().

library(tidyverse)

clean_path <- file.path("data", "processed", "visitor_arrivals_clean.rds")

if (!file.exists(clean_path)) {
  stop("Processed data not found. Run R/02_clean_data.R first.")
}

visitor_data <- read_rds(clean_path)

# Confirm the analysis starts at the intended observation grain.
stopifnot(
  nrow(visitor_data) == 96000,
  sum(duplicated(visitor_data[c(
    "YearMonth", "Country", "TravelPurpose", "Age", "Sex"
  )])) == 0
)

# -----------------------------------------------------------------------------
# Summary tables
# -----------------------------------------------------------------------------

monthly_totals <- visitor_data %>%
  group_by(Date, Year, MonthNumber, MonthName) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  arrange(Date)

country_totals <- visitor_data %>%
  group_by(CountryShort) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  arrange(desc(VisitorArrivals)) %>%
  mutate(SharePercent = 100 * VisitorArrivals / sum(VisitorArrivals))

purpose_totals <- visitor_data %>%
  group_by(TravelPurpose) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  arrange(desc(VisitorArrivals)) %>%
  mutate(SharePercent = 100 * VisitorArrivals / sum(VisitorArrivals))

age_totals <- visitor_data %>%
  group_by(Age) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  mutate(SharePercent = 100 * VisitorArrivals / sum(VisitorArrivals))

sex_totals <- visitor_data %>%
  group_by(Sex) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  mutate(SharePercent = 100 * VisitorArrivals / sum(VisitorArrivals))

seasonal_summary <- monthly_totals %>%
  group_by(MonthName) %>%
  summarise(
    MeanMonthlyArrivals = mean(VisitorArrivals),
    MedianMonthlyArrivals = median(VisitorArrivals),
    MinimumMonthlyArrivals = min(VisitorArrivals),
    MaximumMonthlyArrivals = max(VisitorArrivals),
    NumberOfYears = n(),
    .groups = "drop"
  )

write_csv(monthly_totals, file.path("output", "monthly_totals.csv"))
write_csv(country_totals, file.path("output", "country_totals.csv"))
write_csv(purpose_totals, file.path("output", "purpose_totals.csv"))
write_csv(age_totals, file.path("output", "age_totals.csv"))
write_csv(sex_totals, file.path("output", "sex_totals.csv"))
write_csv(seasonal_summary, file.path("output", "seasonal_summary.csv"))

print(country_totals)
print(purpose_totals)
print(age_totals)
print(sex_totals)
print(seasonal_summary)

# -----------------------------------------------------------------------------
# Chart contracts
# 1. Trend: 125 monthly observations; line chart; one blue series.
# 2. Country comparison: 8 categories; sorted horizontal bars; one blue series.
# 3. Purpose composition: 3 exhaustive categories; sorted horizontal bars.
# 4. Seasonality: 12 calendar-month groups; boxplots show spread across years.
# All totals refer only to the 8 countries and 3 purposes in this dataset.
# -----------------------------------------------------------------------------

blue <- "#256A8A"
gold <- "#C28A28"
ink <- "#24313A"
grid_colour <- "#D9E1E5"

project_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(colour = ink, face = "bold", size = 15),
    plot.subtitle = element_text(colour = "#52616B", size = 10.5),
    axis.title = element_text(colour = ink),
    axis.text = element_text(colour = ink),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = grid_colour, linewidth = 0.35),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

monthly_trend_plot <- ggplot(monthly_totals, aes(x = Date, y = VisitorArrivals)) +
  geom_line(colour = blue, linewidth = 0.8) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  labs(
    title = "Monthly visitor arrivals in the study dataset",
    subtitle = "Sum across 8 countries, 3 purposes, 16 age groups and 2 sex categories; Jan 2016–May 2026",
    x = NULL,
    y = "Visitor arrivals"
  ) +
  project_theme

country_plot_data <- country_totals %>%
  mutate(CountryShort = fct_reorder(CountryShort, VisitorArrivals))

country_plot <- ggplot(
  country_plot_data,
  aes(x = VisitorArrivals, y = CountryShort)
) +
  geom_col(fill = blue, colour = "#19495F", linewidth = 0.25) +
  geom_text(
    aes(label = scales::label_number(scale_cut = scales::cut_short_scale())(VisitorArrivals)),
    hjust = -0.12,
    colour = ink,
    size = 3.4
  ) +
  scale_x_continuous(
    breaks = seq(0, 10000000, by = 2500000),
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "Visitor arrivals by country of residence",
    subtitle = "Total observations in the dataset, Jan 2016–May 2026",
    x = "Visitor arrivals",
    y = NULL
  ) +
  project_theme +
  theme(panel.grid.major.y = element_blank())

purpose_plot_data <- purpose_totals %>%
  mutate(TravelPurpose = fct_reorder(TravelPurpose, VisitorArrivals))

purpose_plot <- ggplot(
  purpose_plot_data,
  aes(x = VisitorArrivals, y = TravelPurpose)
) +
  geom_col(fill = gold, colour = "#7D5A1F", linewidth = 0.25) +
  geom_text(
    aes(label = scales::label_percent(accuracy = 0.1)(SharePercent / 100)),
    hjust = -0.12,
    colour = ink,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = "Visitor arrivals by travel purpose",
    subtitle = "Share of all arrivals represented in the dataset, Jan 2016–May 2026",
    x = "Visitor arrivals",
    y = NULL
  ) +
  project_theme +
  theme(panel.grid.major.y = element_blank())

seasonality_plot <- ggplot(
  monthly_totals,
  aes(x = MonthName, y = VisitorArrivals)
) +
  geom_boxplot(
    fill = "#DCEAF0",
    colour = blue,
    outlier.colour = gold,
    outlier.size = 1.8,
    linewidth = 0.55
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  labs(
    title = "Monthly visitor arrivals by calendar month",
    subtitle = "Distribution across available years; pandemic-period months remain included",
    x = NULL,
    y = "Visitor arrivals"
  ) +
  project_theme

ggsave(
  file.path("output", "figures", "01_monthly_trend.png"),
  monthly_trend_plot,
  width = 11,
  height = 5.8,
  dpi = 180,
  bg = "white"
)

ggsave(
  file.path("output", "figures", "02_country_totals.png"),
  country_plot,
  width = 9,
  height = 5.8,
  dpi = 180,
  bg = "white"
)

ggsave(
  file.path("output", "figures", "03_purpose_totals.png"),
  purpose_plot,
  width = 9,
  height = 4.5,
  dpi = 180,
  bg = "white"
)

ggsave(
  file.path("output", "figures", "04_monthly_seasonality.png"),
  seasonality_plot,
  width = 10,
  height = 5.8,
  dpi = 180,
  bg = "white"
)

message("Exploratory tables and four figures created in output/.")
