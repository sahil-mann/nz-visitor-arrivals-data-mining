# Remote Data Miners — slide-ready evidence figures
#
# These figures visualise only saved, reproducible results from scripts 05–07.
# They do not introduce new methods or calculations.

library(tidyverse)

required_files <- c(
  file.path("output", "aggregate_recovery_2019_2025.csv"),
  file.path("output", "models", "recovery_regression_metrics.csv"),
  file.path("output", "models", "recovery_classification_metrics.csv")
)

if (any(!file.exists(required_files))) {
  stop("Required result tables are missing. Run scripts 05–07 first.")
}

ink <- "#172A33"
muted <- "#61747D"
blue <- "#147D92"
light_blue <- "#75B8C7"
orange <- "#E97836"
gold <- "#E3AE37"
light_grey <- "#DCE5E8"

slide_theme <- theme_minimal(base_size = 16) +
  theme(
    text = element_text(colour = ink),
    axis.text = element_text(colour = ink),
    axis.title = element_text(colour = ink),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = light_grey, linewidth = 0.45),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = "top",
    legend.title = element_blank()
  )

# -----------------------------------------------------------------------------
# Figure 11: the aggregate recovery paradox.
# -----------------------------------------------------------------------------

aggregate_recovery <- read_csv(required_files[1], show_col_types = FALSE) %>%
  mutate(
    MarketLabel = recode(
      MarketView,
      "AllEight" = "All eight countries",
      "ExcludingAustralia" = "Seven countries\nwithout Australia",
      "Australia" = "Australia"
    ),
    MarketLabel = factor(
      MarketLabel,
      levels = c("Australia", "All eight countries", "Seven countries\nwithout Australia")
    ),
    Highlight = if_else(MarketView == "ExcludingAustralia", "Hidden view", "Headline view")
  )

recovery_paradox_plot <- ggplot(
  aggregate_recovery,
  aes(x = RecoveryPercent, y = MarketLabel)
) +
  geom_vline(
    xintercept = 100,
    colour = muted,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_segment(
    aes(x = 0, xend = RecoveryPercent, yend = MarketLabel),
    colour = light_grey,
    linewidth = 2.8,
    lineend = "round"
  ) +
  geom_point(
    aes(colour = Highlight),
    size = 8
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", RecoveryPercent)),
    hjust = -0.25,
    size = 6,
    fontface = "bold",
    colour = ink
  ) +
  annotate(
    "text",
    x = 100,
    y = 3.35,
    label = "2019 level",
    hjust = 0.5,
    colour = muted,
    size = 4.8
  ) +
  scale_colour_manual(values = c("Headline view" = blue, "Hidden view" = orange)) +
  scale_x_continuous(
    limits = c(0, 112),
    breaks = c(0, 25, 50, 75, 100),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "2025 estimated arrivals relative to 2019", y = NULL) +
  slide_theme +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 15, face = "bold", lineheight = 0.9),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),
    plot.margin = margin(20, 70, 20, 20)
  )

ggsave(
  file.path("output", "figures", "11_recovery_paradox.png"),
  recovery_paradox_plot,
  width = 11.5,
  height = 5.7,
  dpi = 220,
  bg = "white"
)

# -----------------------------------------------------------------------------
# Figure 12: Module 2 chronological test comparison.
# -----------------------------------------------------------------------------

regression_metrics <- read_csv(required_files[2], show_col_types = FALSE) %>%
  filter(
    EvaluationDesign == "Chronological future check",
    Split == "Test"
  ) %>%
  mutate(
    ModelLabel = recode(
      Model,
      "Mean baseline" = "Mean\nbaseline",
      "Additive MLR" = "Additive\nMLR",
      "Country × Purpose interaction MLR" = "Country × Purpose\ninteraction MLR"
    ),
    ModelLabel = factor(
      ModelLabel,
      levels = c("Mean\nbaseline", "Additive\nMLR", "Country × Purpose\ninteraction MLR")
    ),
    Highlight = if_else(str_detect(Model, "interaction"), "Interaction", "Comparison")
  )

regression_plot <- ggplot(regression_metrics, aes(x = ModelLabel, y = RMSE)) +
  geom_col(
    aes(fill = Highlight),
    width = 0.62,
    colour = NA
  ) +
  geom_text(
    aes(label = sprintf("%.1f", RMSE)),
    vjust = -0.45,
    size = 6,
    fontface = "bold",
    colour = ink
  ) +
  annotate(
    "label",
    x = 3,
    y = 8,
    label = "11% lower RMSE\nthan the mean baseline",
    size = 4.8,
    fontface = "bold",
    colour = blue,
    fill = "#E9F4F6",
    linewidth = 0,
    label.padding = unit(0.55, "lines")
  ) +
  scale_fill_manual(values = c("Comparison" = light_grey, "Interaction" = blue)) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, 5),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(x = NULL, y = "RMSE (recovery-percentage points)") +
  slide_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, face = "bold", lineheight = 0.9),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, margin = margin(r = 12)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = light_grey, linewidth = 0.45),
    plot.margin = margin(20, 30, 20, 20)
  )

ggsave(
  file.path("output", "figures", "12_recovery_regression_rmse.png"),
  regression_plot,
  width = 10.8,
  height = 6.2,
  dpi = 220,
  bg = "white"
)

# -----------------------------------------------------------------------------
# Figure 13: the classification accuracy trap.
# -----------------------------------------------------------------------------

classification_metrics_data <- read_csv(required_files[3], show_col_types = FALSE) %>%
  filter(EvaluationDesign == "Chronological future check") %>%
  mutate(
    ModelLabel = recode(
      Model,
      "Majority-class baseline" = "Always say\nnot recovered",
      "Logistic regression: 0.50 threshold" = "Logistic\nthreshold 0.50",
      "Logistic regression: validation threshold" = "Logistic\nthreshold 0.06"
    ),
    ModelLabel = factor(
      ModelLabel,
      levels = c(
        "Always say\nnot recovered",
        "Logistic\nthreshold 0.50",
        "Logistic\nthreshold 0.06"
      )
    )
  ) %>%
  select(ModelLabel, Accuracy, Sensitivity, Specificity) %>%
  pivot_longer(
    cols = c(Accuracy, Sensitivity, Specificity),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    ValuePercent = 100 * Value,
    Metric = factor(Metric, levels = c("Accuracy", "Sensitivity", "Specificity"))
  )

classification_plot <- ggplot(
  classification_metrics_data,
  aes(x = ValuePercent, y = ModelLabel)
) +
  geom_line(
    aes(group = ModelLabel),
    colour = light_grey,
    linewidth = 2.5
  ) +
  geom_point(
    aes(colour = Metric),
    size = 6.8
  ) +
  geom_text(
    data = filter(classification_metrics_data, Metric == "Accuracy"),
    aes(
      label = sprintf("%.0f%%", ValuePercent),
      colour = Metric,
      hjust = ifelse(ValuePercent > 93, 1.25, -0.35)
    ),
    size = 4.5,
    fontface = "bold",
    nudge_y = -0.10,
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(classification_metrics_data, Metric == "Sensitivity"),
    aes(
      label = sprintf("%.0f%%", ValuePercent),
      colour = Metric,
      hjust = ifelse(ValuePercent > 93, 1.25, -0.35)
    ),
    size = 4.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = filter(classification_metrics_data, Metric == "Specificity"),
    aes(
      label = sprintf("%.0f%%", ValuePercent),
      colour = Metric,
      hjust = ifelse(ValuePercent > 93, 1.25, -0.35)
    ),
    size = 4.5,
    fontface = "bold",
    nudge_y = 0.10,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("Accuracy" = gold, "Sensitivity" = orange, "Specificity" = blue)
  ) +
  scale_x_continuous(
    limits = c(0, 104),
    breaks = c(0, 25, 50, 75, 100),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Chronological 2025 test performance", y = NULL) +
  slide_theme +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold", lineheight = 0.9),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 12)),
    plot.margin = margin(10, 55, 20, 20)
  )

ggsave(
  file.path("output", "figures", "13_classification_accuracy_trap.png"),
  classification_plot,
  width = 11.5,
  height = 6.2,
  dpi = 220,
  bg = "white"
)

# -----------------------------------------------------------------------------
# Figure 14: tuned-threshold chronological confusion matrix.
# -----------------------------------------------------------------------------

confusion_data <- tibble(
  Actual = factor(
    c("Not recovered", "Not recovered", "Recovered", "Recovered"),
    levels = c("Recovered", "Not recovered")
  ),
  Predicted = factor(
    c("Not recovered", "Recovered", "Not recovered", "Recovered"),
    levels = c("Not recovered", "Recovered")
  ),
  Count = c(193, 38, 20, 37),
  Result = c("Correct", "Incorrect", "Incorrect", "Correct")
)

confusion_plot <- ggplot(confusion_data, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Result), colour = "white", linewidth = 5) +
  geom_text(
    aes(label = Count),
    size = 12,
    fontface = "bold",
    colour = ink
  ) +
  scale_fill_manual(values = c("Correct" = "#CFE8E7", "Incorrect" = "#F7D5C2")) +
  labs(x = "Predicted", y = "Actual") +
  theme_minimal(base_size = 17) +
  theme(
    text = element_text(colour = ink),
    axis.text = element_text(colour = ink, face = "bold", size = 14),
    axis.title = element_text(colour = ink, face = "bold", size = 15),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

ggsave(
  file.path("output", "figures", "14_recovery_confusion_matrix.png"),
  confusion_plot,
  width = 7.2,
  height = 5.8,
  dpi = 220,
  bg = "white"
)

cat("Four slide-ready story figures created.\n")

# -----------------------------------------------------------------------------
# Figure 15: title-free Country × Purpose heatmap for a presentation slide.
# -----------------------------------------------------------------------------

country_purpose_recovery <- read_csv(
  file.path("output", "country_purpose_recovery_2019_2025.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    CountryShort = factor(
      CountryShort,
      levels = c(
        "China", "Japan", "Germany", "South Korea",
        "United Kingdom", "Canada", "Australia", "United States"
      )
    ),
    PurposeLabel = recode(
      TravelPurpose,
      "Business" = "Business",
      "Holiday/Vacation" = "Holiday",
      "Visit Friends/Relatives" = "Friends & relatives"
    ),
    PurposeLabel = factor(
      PurposeLabel,
      levels = c("Business", "Holiday", "Friends & relatives")
    )
  )

heatmap_slide_plot <- ggplot(
  country_purpose_recovery,
  aes(x = PurposeLabel, y = CountryShort, fill = RecoveryPercent)
) +
  geom_tile(colour = "white", linewidth = 4) +
  geom_text(
    aes(label = sprintf("%.0f%%", RecoveryPercent)),
    colour = ink,
    fontface = "bold",
    size = 5.4
  ) +
  scale_fill_gradient2(
    low = "#F1A078",
    mid = "#F4F1E9",
    high = "#8CBFC3",
    midpoint = 100,
    limits = c(30, 115),
    breaks = c(40, 60, 80, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(x = NULL, y = NULL, fill = "2025 / 2019") +
  theme_minimal(base_size = 16) +
  theme(
    text = element_text(colour = ink),
    axis.text = element_text(colour = ink),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(5, 20, 10, 5)
  )

ggsave(
  file.path("output", "figures", "15_country_purpose_heatmap_slide.png"),
  heatmap_slide_plot,
  width = 11.2,
  height = 5.5,
  dpi = 220,
  bg = "white"
)

# -----------------------------------------------------------------------------
# Figure 16: the observed aggregate time path that challenged the first model.
# -----------------------------------------------------------------------------

monthly_totals <- read_csv(
  file.path("output", "monthly_totals.csv"),
  show_col_types = FALSE
) %>%
  mutate(Date = as.Date(Date))

monthly_trend_story_plot <- ggplot(monthly_totals, aes(x = Date, y = VisitorArrivals / 1000)) +
  annotate(
    "rect",
    xmin = as.Date("2020-03-01"),
    xmax = as.Date("2022-07-01"),
    ymin = -Inf,
    ymax = Inf,
    fill = "#F7E2D6",
    alpha = 0.75
  ) +
  geom_line(colour = blue, linewidth = 1.25) +
  annotate(
    "text",
    x = as.Date("2021-05-01"),
    y = 315,
    label = "Abrupt collapse",
    colour = orange,
    fontface = "bold",
    size = 5
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 350),
    breaks = seq(0, 350, 50),
    labels = function(x) paste0(x, "k"),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(x = NULL, y = "Estimated passenger movements") +
  slide_theme +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14, margin = margin(r = 12)),
    panel.grid.major.y = element_line(colour = light_grey, linewidth = 0.45),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(15, 20, 15, 15)
  )

ggsave(
  file.path("output", "figures", "16_monthly_trend_story.png"),
  monthly_trend_story_plot,
  width = 10.8,
  height = 5.6,
  dpi = 220,
  bg = "white"
)

cat("Two additional slide-specific figures created.\n")
