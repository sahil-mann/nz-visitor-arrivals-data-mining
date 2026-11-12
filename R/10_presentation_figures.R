# Remote Data Miners — presentation-only figures
#
# This script does not alter the dataset or either modelling workflow. It
# rebuilds presentation visuals from the validated processed data so that the
# final slides are editable, legible, and traceable to the same source values.

library(tidyverse)
library(scales)

visitor_data <- read_rds(
  file.path("data", "processed", "visitor_arrivals_clean.rds")
)

figure_dir <- file.path("output", "merged_presentation")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

ink <- "#153039"
muted <- "#66797A"
green <- "#078B6C"
green_dark <- "#05634F"
green_soft <- "#DDEFE8"
teal <- "#178CA3"
amber <- "#F2A93B"
coral <- "#E86F51"
cream <- "#FAF7F0"
grid <- "#DDE5E2"

presentation_theme <- theme_minimal(base_family = "Arial", base_size = 13) +
  theme(
    plot.background = element_rect(fill = cream, colour = NA),
    panel.background = element_rect(fill = cream, colour = NA),
    plot.title = element_text(colour = ink, face = "bold", size = 17),
    plot.subtitle = element_text(colour = muted, size = 11),
    axis.title = element_blank(),
    axis.text = element_text(colour = muted, size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = grid, linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(12, 16, 12, 16)
  )

country_totals <- visitor_data %>%
  group_by(CountryShort) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  arrange(VisitorArrivals) %>%
  mutate(
    CountryShort = factor(CountryShort, levels = CountryShort),
    Highlight = if_else(CountryShort == "Australia", "Australia", "Other")
  )

purpose_totals <- visitor_data %>%
  group_by(TravelPurpose) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  mutate(
    Share = VisitorArrivals / sum(VisitorArrivals),
    TravelPurpose = fct_reorder(TravelPurpose, Share)
  )

age_order <- c(
  "0-4 years", "5-9 years", "10-14 years", "15-19 years",
  "20-24 years", "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years", "50-54 years", "55-59 years",
  "60-64 years", "65-69 years", "70-74 years", "75 years and over"
)

age_totals <- visitor_data %>%
  group_by(Age) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  mutate(
    Age = factor(Age, levels = age_order),
    Highlight = if_else(Age %in% c("25-29 years", "30-34 years"), "Peak", "Other")
  ) %>%
  arrange(Age)

sex_totals <- visitor_data %>%
  group_by(Sex) %>%
  summarise(VisitorArrivals = sum(VisitorArrivals), .groups = "drop") %>%
  mutate(Share = VisitorArrivals / sum(VisitorArrivals))

p_country <- ggplot(country_totals, aes(VisitorArrivals, CountryShort, fill = Highlight)) +
  geom_col(width = 0.64) +
  geom_text(
    aes(label = label_number(scale = 1e-6, accuracy = 0.1, suffix = "M")(VisitorArrivals)),
    hjust = -0.12, colour = ink, size = 3.7, fontface = "bold"
  ) +
  scale_fill_manual(values = c(Australia = amber, Other = green)) +
  scale_x_continuous(
    labels = label_number(scale = 1e-6, accuracy = 1, suffix = "M"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Country",
    subtitle = "Australia is the scale-setter—not the whole story"
  ) +
  presentation_theme

purpose_colours <- c(
  "Business" = coral,
  "Holiday/Vacation" = green,
  "Visit Friends/Relatives" = amber
)

p_purpose <- ggplot(
  purpose_totals,
  aes(Share, TravelPurpose, fill = TravelPurpose)
) +
  geom_col(width = 0.64) +
  geom_text(
    aes(label = percent(Share, accuracy = 0.1)),
    hjust = -0.12, colour = ink, size = 3.8, fontface = "bold"
  ) +
  scale_fill_manual(values = purpose_colours) +
  scale_x_continuous(
    labels = percent_format(accuracy = 10),
    limits = c(0, 0.64),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Travel purpose",
    subtitle = "Holiday/Vacation accounts for more than half"
  ) +
  presentation_theme +
  theme(axis.text.y = element_text(size = 9.5))

p_age <- ggplot(age_totals, aes(Age, VisitorArrivals, fill = Highlight)) +
  geom_col(width = 0.76) +
  scale_fill_manual(values = c(Peak = amber, Other = green)) +
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.5, suffix = "M")) +
  scale_x_discrete(labels = c(
    "0-4 years" = "0–4", "5-9 years" = "5–9", "10-14 years" = "10–14",
    "15-19 years" = "15–19", "20-24 years" = "20–24", "25-29 years" = "25–29",
    "30-34 years" = "30–34", "35-39 years" = "35–39", "40-44 years" = "40–44",
    "45-49 years" = "45–49", "50-54 years" = "50–54", "55-59 years" = "55–59",
    "60-64 years" = "60–64", "65-69 years" = "65–69", "70-74 years" = "70–74",
    "75 years and over" = "75+"
  )) +
  labs(title = "Age", subtitle = "The distribution peaks at ages 25–34") +
  presentation_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8.5))

p_sex <- ggplot(sex_totals, aes(x = 1, y = Share, fill = Sex)) +
  geom_col(width = 0.68, colour = cream, linewidth = 2) +
  geom_text(
    aes(label = paste0(Sex, "  ", percent(Share, accuracy = 0.1))),
    position = position_stack(vjust = 0.5), colour = "white", size = 4.3,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = c(Female = teal, Male = green_dark)) +
  labs(title = "Sex", subtitle = "Near balance suggests limited predictive separation") +
  theme_void(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = cream, colour = NA),
    plot.title = element_text(colour = ink, face = "bold", size = 17),
    plot.subtitle = element_text(colour = muted, size = 11),
    plot.margin = margin(12, 16, 12, 16),
    legend.position = "none"
  )

ggsave(file.path(figure_dir, "q2_eda_country.png"), p_country, width = 6.0, height = 3.4, dpi = 200, bg = cream)
ggsave(file.path(figure_dir, "q2_eda_purpose.png"), p_purpose, width = 6.0, height = 3.4, dpi = 200, bg = cream)
ggsave(file.path(figure_dir, "q2_eda_age.png"), p_age, width = 6.0, height = 3.4, dpi = 200, bg = cream)
ggsave(file.path(figure_dir, "q2_eda_sex.png"), p_sex, width = 6.0, height = 3.4, dpi = 200, bg = cream)

importance_country <- tribble(
  ~Level, ~Coefficient,
  "USA", -844,
  "United Kingdom", -939,
  "Korea", -1054,
  "Japan", -1052,
  "Germany", -1044,
  "China", -868,
  "Canada", -1058
) %>%
  mutate(Level = fct_reorder(Level, Coefficient))

importance_age <- tribble(
  ~Level, ~Coefficient,
  "60–64", 231,
  "55–59", 236,
  "50–54", 220,
  "35–39", 211,
  "30–34", 285,
  "25–29", 288
) %>%
  mutate(Level = fct_reorder(Level, Coefficient))

importance_month <- tribble(
  ~Level, ~Coefficient,
  "Feb", 4, "Mar", -58, "Apr", -118, "May", -187,
  "Jun", -175, "Jul", -139, "Aug", -144, "Sep", -128,
  "Oct", -126, "Nov", -52, "Dec", 87
) %>%
  mutate(Level = factor(Level, levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

importance_purpose <- tribble(
  ~Level, ~Coefficient,
  "Visit Friends/Relatives", 196,
  "Holiday/Vacation", 356
) %>%
  mutate(Level = fct_reorder(Level, Coefficient))

importance_theme <- presentation_theme +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 9),
    axis.text = element_text(size = 9)
  )

p_ic <- importance_country %>%
  mutate(
    Difference = abs(Coefficient),
    Level = fct_reorder(Level, Difference)
  ) %>%
  ggplot(aes(Difference, Level)) +
  geom_col(fill = green, width = 0.65) +
  geom_text(
    aes(label = paste0("−", comma(Difference))),
    hjust = -0.12, colour = ink, size = 3.35, fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(0, 1225), breaks = seq(0, 1200, 300),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "Country differences",
    subtitle = "Same profile, compared with Australia"
  ) + importance_theme

p_ia <- ggplot(importance_age, aes(Coefficient, Level)) +
  geom_col(fill = amber, width = 0.65) +
  geom_text(aes(label = paste0("+", Coefficient)), hjust = -0.12, colour = ink, size = 3.35, fontface = "bold") +
  scale_x_continuous(limits = c(0, 335), breaks = seq(0, 300, 100), expand = expansion(mult = c(0, 0.01))) +
  labs(title = "Age differences", subtitle = "Compared with age 0–4") + importance_theme

p_im <- ggplot(importance_month, aes(Level, Coefficient, fill = Coefficient > 0)) +
  geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_col(width = 0.7) +
  geom_text(aes(label = Coefficient), vjust = if_else(importance_month$Coefficient > 0, -0.5, 1.25), size = 3.0, colour = ink) +
  scale_fill_manual(values = c(`TRUE` = green, `FALSE` = coral)) +
  scale_y_continuous(limits = c(-225, 115), breaks = c(-200, -100, 0, 100)) +
  labs(title = "Month differences", subtitle = "Green is above January; coral is below") + importance_theme +
  theme(axis.text.x = element_text(size = 8))

p_ip <- ggplot(importance_purpose, aes(Coefficient, Level)) +
  geom_col(fill = teal, width = 0.58) +
  geom_text(aes(label = paste0("+", Coefficient)), hjust = -0.12, colour = ink, size = 3.35, fontface = "bold") +
  scale_x_continuous(limits = c(0, 420), breaks = seq(0, 400, 100), expand = expansion(mult = c(0, 0.01))) +
  labs(title = "Purpose differences", subtitle = "Compared with Business") + importance_theme

ggsave(file.path(figure_dir, "q2_importance_country.png"), p_ic, width = 6.0, height = 3.2, dpi = 200, bg = cream)
ggsave(file.path(figure_dir, "q2_importance_age.png"), p_ia, width = 6.0, height = 3.2, dpi = 200, bg = cream)
ggsave(file.path(figure_dir, "q2_importance_month.png"), p_im, width = 6.0, height = 3.2, dpi = 200, bg = cream)
ggsave(file.path(figure_dir, "q2_importance_purpose.png"), p_ip, width = 6.0, height = 3.2, dpi = 200, bg = cream)

china_june_profile <- visitor_data %>%
  filter(
    CountryShort == "China",
    TravelPurpose == "Holiday/Vacation",
    Age == "30-34 years",
    Sex == "Male",
    MonthNumber == 6,
    Year <= 2025
  ) %>%
  select(Year, VisitorArrivals) %>%
  arrange(Year)

australia_business_june_profile <- visitor_data %>%
  filter(
    CountryShort == "Australia",
    TravelPurpose == "Business",
    Age == "30-34 years",
    Sex == "Male",
    MonthNumber == 6,
    Year <= 2025
  ) %>%
  select(Year, VisitorArrivals) %>%
  arrange(Year)

write_csv(country_totals, file.path(figure_dir, "country_totals.csv"))
write_csv(purpose_totals, file.path(figure_dir, "purpose_totals.csv"))
write_csv(age_totals, file.path(figure_dir, "age_totals.csv"))
write_csv(sex_totals, file.path(figure_dir, "sex_totals.csv"))
write_csv(china_june_profile, file.path(figure_dir, "china_june_profile.csv"))
write_csv(
  australia_business_june_profile,
  file.path(figure_dir, "australia_business_june_profile.csv")
)

prediction_table <- read_csv(
  file.path(
    "output", "no_covid_all8_extension",
    "all_8_country_predictions_no_covid.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    CountryShort = recode(
      Country,
      UnitedKingdom = "United Kingdom"
    ),
    PredictionDate = as.Date("2026-06-01")
  )

profile_history <- visitor_data %>%
  filter(
    TravelPurpose == "Holiday/Vacation",
    Age == "30-34 years",
    Sex == "Male"
  )

country_colours <- c(
  "Australia" = "#087EB8",
  "Canada" = "#E9A400",
  "China" = "#E56816",
  "Germany" = "#08A77D",
  "Japan" = "#C976A6",
  "South Korea" = "#55ACE0",
  "United Kingdom" = "#7D55A0",
  "United States" = "#545454"
)

p_timeline <- ggplot() +
  annotate(
    "rect",
    xmin = as.Date("2020-03-01"), xmax = as.Date("2022-02-01"),
    ymin = -Inf, ymax = Inf, fill = "#D9E0E2", alpha = 0.65
  ) +
  geom_line(
    data = profile_history,
    aes(Date, VisitorArrivals, colour = CountryShort),
    linewidth = 0.75, alpha = 0.92
  ) +
  geom_point(
    data = prediction_table,
    aes(PredictionDate, RoundedPrediction),
    shape = 23, size = 4.4, fill = amber, colour = ink, stroke = 1.1
  ) +
  annotate(
    "text", x = as.Date("2021-03-01"), y = 4550,
    label = "excluded from model training", colour = muted,
    size = 3.7, fontface = "bold"
  ) +
  scale_colour_manual(values = country_colours) +
  scale_x_date(
    date_breaks = "2 years", date_labels = "%Y",
    limits = as.Date(c("2016-01-01", "2026-10-01")),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 5000), labels = comma,
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "COVID period removed from training + June predictions",
    subtitle = "Holiday/Vacation · age 30–34 · male · all eight countries",
    colour = NULL
  ) +
  presentation_theme +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8.5, colour = muted),
    legend.key.width = grid::unit(0.65, "cm"),
    panel.grid.major.y = element_line(colour = grid, linewidth = 0.4)
  )

ggsave(
  file.path(figure_dir, "q2_no_covid_timeline.png"),
  p_timeline, width = 10.5, height = 5.5, dpi = 200, bg = cream
)

cat("Presentation figures and source tables created in", figure_dir, "\n")
