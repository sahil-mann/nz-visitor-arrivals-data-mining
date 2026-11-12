# Remote Data Miners - A1 poster figures
#
# These figures are deliberately sized for one A1 poster column. They use large
# labels, direct annotations and minimal chart ink so they remain readable when
# printed. Run scripts 01-12 first.

library(tidyverse)

poster_dir <- file.path("output", "poster", "figures")
dir.create(poster_dir, recursive = TRUE, showWarnings = FALSE)

recovery_path <- file.path("output", "country_purpose_recovery_2019_2025.csv")
annual_country_path <- file.path("output", "annual_country.csv")
tree_metrics_path <- file.path(
  "output", "tree_methods", "audited_tree_model_metrics.csv"
)
final_metrics_path <- file.path(
  "output", "tree_methods", "final_future_test_metrics.csv"
)
final_importance_path <- file.path(
  "output", "tree_methods", "final_rf_importance.csv"
)

required_paths <- c(
  recovery_path,
  annual_country_path,
  tree_metrics_path,
  final_metrics_path,
  final_importance_path
)

if (any(!file.exists(required_paths))) {
  stop("Poster inputs are missing. Run scripts 01-12 first.")
}

C <- list(
  ink = "#123238",
  muted = "#5C6F73",
  teal = "#0B6E69",
  blue = "#2D9CDB",
  coral = "#E76F51",
  sand = "#F4EDE3",
  grid = "#DDE6E5"
)

poster_theme <- theme_minimal(base_size = 19) +
  theme(
    plot.title = element_text(face = "bold", colour = C$ink, size = 23),
    plot.subtitle = element_text(colour = C$muted, size = 16),
    axis.title = element_text(colour = C$ink, size = 17),
    axis.text = element_text(colour = C$ink, size = 16),
    legend.text = element_text(colour = C$ink, size = 15),
    legend.title = element_text(colour = C$ink, size = 15, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 14, 8, 8)
  )

save_poster_plot <- function(plot, filename, width, height) {
  ggsave(
    file.path(poster_dir, paste0(filename, ".png")),
    plot,
    width = width,
    height = height,
    dpi = 360,
    bg = "white"
  )
  ggsave(
    file.path(poster_dir, paste0(filename, ".pdf")),
    plot,
    width = width,
    height = height,
    device = "pdf",
    bg = "white"
  )
}

# 1. Country x purpose recovery heatmap.
country_order <- c(
  "United States", "United Kingdom", "South Korea", "Japan",
  "Germany", "China", "Canada", "Australia"
)
purpose_order <- c("Business", "Holiday/Vacation", "Visit Friends/Relatives")

recovery <- read_csv(recovery_path, show_col_types = FALSE) %>%
  mutate(
    CountryShort = factor(CountryShort, levels = rev(country_order)),
    TravelPurpose = factor(TravelPurpose, levels = purpose_order),
    Label = paste0(round(RecoveryPercent), "%")
  )

heatmap_plot <- ggplot(
  recovery,
  aes(x = TravelPurpose, y = CountryShort, fill = RecoveryPercent)
) +
  geom_tile(colour = "white", linewidth = 1.4) +
  geom_text(aes(label = Label), colour = C$ink, size = 5.4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#F0B998",
    mid = "#F5F0E8",
    high = "#A6CEC5",
    midpoint = 100,
    limits = c(30, 115),
    breaks = c(50, 75, 100),
    labels = scales::label_percent(scale = 1),
    oob = scales::squish
  ) +
  scale_x_discrete(labels = c(
    "Business",
    "Holiday /\nVacation",
    "Visit friends /\nrelatives"
  )) +
  labs(x = NULL, y = NULL, fill = "2025 vs 2019") +
  poster_theme +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.key.width = unit(1.1, "in"),
    axis.text.x = element_text(face = "bold", size = 15),
    axis.text.y = element_text(size = 15)
  )

save_poster_plot(heatmap_plot, "01_recovery_heatmap", 7.0, 5.0)

# 2. Recovery paradox.
annual_country <- read_csv(annual_country_path, show_col_types = FALSE)

recovery_percent <- function(data) {
  value_2025 <- data %>% filter(Year == 2025) %>% summarise(v = sum(VisitorArrivals)) %>% pull(v)
  value_2019 <- data %>% filter(Year == 2019) %>% summarise(v = sum(VisitorArrivals)) %>% pull(v)
  100 * value_2025 / value_2019
}

paradox <- tibble(
  Group = factor(
    c("Australia", "All eight countries", "Seven without Australia"),
    levels = c("Seven without Australia", "All eight countries", "Australia")
  ),
  Recovery = c(
    recovery_percent(annual_country %>% filter(CountryShort == "Australia")),
    recovery_percent(annual_country),
    recovery_percent(annual_country %>% filter(CountryShort != "Australia"))
  )
)

paradox_plot <- ggplot(paradox, aes(x = Recovery, y = Group)) +
  geom_segment(aes(x = 0, xend = Recovery, yend = Group), linewidth = 3.6, colour = C$grid) +
  geom_vline(xintercept = 100, linetype = "dashed", colour = C$muted, linewidth = 1) +
  geom_point(
    aes(colour = Group == "Seven without Australia"),
    size = 8,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Recovery)),
    hjust = -0.18,
    colour = C$ink,
    size = 6.0,
    fontface = "bold"
  ) +
  scale_colour_manual(values = c("TRUE" = C$coral, "FALSE" = C$teal)) +
  scale_x_continuous(
    limits = c(0, 110),
    breaks = c(0, 25, 50, 75, 100),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(x = "2025 arrivals relative to 2019", y = NULL) +
  poster_theme +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 15)
  )

save_poster_plot(paradox_plot, "02_recovery_paradox", 7.0, 3.2)

# 3. Final future-test model comparison.
final_metrics <- read_csv(final_metrics_path, show_col_types = FALSE)
tree_metrics <- read_csv(tree_metrics_path, show_col_types = FALSE)

single_tree <- tree_metrics %>%
  filter(Split == "Chronological", Model == "Regression tree") %>%
  slice(1) %>%
  transmute(Model = "Single regression tree", RMSE)

model_comparison <- bind_rows(
  final_metrics %>%
    transmute(
      Model = if_else(str_starts(Model, "Random forest"), "Random forest", Model),
      RMSE
    ),
  single_tree
) %>%
  mutate(
    Model = factor(
      Model,
      levels = c("Additive MLR", "Single regression tree", "Random forest")
    ),
    Colour = case_when(
      Model == "Random forest" ~ C$teal,
      Model == "Single regression tree" ~ C$blue,
      TRUE ~ C$coral
    )
  )

model_plot <- ggplot(model_comparison, aes(x = RMSE, y = Model, fill = Colour)) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_text(
    aes(label = round(RMSE, 1)),
    hjust = -0.15,
    colour = C$ink,
    size = 6.0,
    fontface = "bold"
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    limits = c(0, max(model_comparison$RMSE) * 1.18),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "Future-test RMSE (visitor arrivals)", y = NULL) +
  poster_theme +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 15)
  )

save_poster_plot(model_plot, "03_future_model_comparison", 7.0, 3.2)

# 4. Final random-forest importance.
importance <- read_csv(final_importance_path, show_col_types = FALSE) %>%
  mutate(Variable = forcats::fct_reorder(Variable, Importance))

importance_plot <- ggplot(importance, aes(x = Importance, y = Variable)) +
  geom_col(fill = C$teal, width = 0.66) +
  geom_text(
    aes(label = round(Importance)),
    hjust = -0.18,
    colour = C$ink,
    size = 5.2,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(0, max(importance$Importance) * 1.15),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Increase in error after shuffling each variable",
    y = NULL
  ) +
  poster_theme +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 15)
  )

save_poster_plot(importance_plot, "04_rf_importance", 7.0, 3.2)

cat("Poster figures written to ", poster_dir, "\n", sep = "")
