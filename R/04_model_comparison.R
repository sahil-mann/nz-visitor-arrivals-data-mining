# Remote Data Miners — first regression comparison
# Course methods: set.seed(), sample(), lm(), predict(), MSE and R-squared.

library(tidyverse)

clean_path <- file.path("data", "processed", "visitor_arrivals_clean.rds")

if (!file.exists(clean_path)) {
  stop("Processed data not found. Run R/02_clean_data.R first.")
}

age_levels <- c(
  "0-4 years", "5-9 years", "10-14 years", "15-19 years",
  "20-24 years", "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years", "50-54 years", "55-59 years",
  "60-64 years", "65-69 years", "70-74 years", "75 years and over"
)

model_data <- read_rds(clean_path) %>%
  mutate(
    RowID = row_number(),
    MonthFactor = factor(MonthNumber, levels = 1:12),
    CountryShort = factor(CountryShort),
    TravelPurpose = factor(TravelPurpose),
    AgeFactor = factor(as.character(Age), levels = age_levels),
    Sex = factor(Sex)
  )

# -----------------------------------------------------------------------------
# Split 1: Module 2 random 80/10/10 split.
# This aligns with the course, but rows from the same month occur in all sets.
# -----------------------------------------------------------------------------

set.seed(1)

number_of_rows <- nrow(model_data)
random_train_index <- sample(
  1:number_of_rows,
  size = floor(0.80 * number_of_rows)
)

random_remaining_index <- setdiff(1:number_of_rows, random_train_index)

random_validation_index <- sample(
  random_remaining_index,
  size = floor(0.50 * length(random_remaining_index))
)

random_test_index <- setdiff(random_remaining_index, random_validation_index)

random_split <- list(
  Train = model_data[random_train_index, ],
  Validation = model_data[random_validation_index, ],
  Test = model_data[random_test_index, ]
)

# -----------------------------------------------------------------------------
# Split 2: chronological 80/10/10 split by complete months.
# This is the more credible check for predicting future visitor arrivals.
# -----------------------------------------------------------------------------

all_months <- sort(unique(model_data$Date))
number_of_months <- length(all_months)
chronological_train_end <- floor(0.80 * number_of_months)
chronological_validation_end <- floor(0.90 * number_of_months)

chronological_train_months <- all_months[1:chronological_train_end]
chronological_validation_months <- all_months[
  (chronological_train_end + 1):chronological_validation_end
]
chronological_test_months <- all_months[
  (chronological_validation_end + 1):number_of_months
]

chronological_split <- list(
  Train = model_data %>% filter(Date %in% chronological_train_months),
  Validation = model_data %>% filter(Date %in% chronological_validation_months),
  Test = model_data %>% filter(Date %in% chronological_test_months)
)

split_summary <- bind_rows(
  tibble(
    Split = "Random",
    Set = names(random_split),
    Rows = map_int(random_split, nrow),
    FirstMonth = map_chr(random_split, ~ format(min(.x$Date), "%Y-%m")),
    LastMonth = map_chr(random_split, ~ format(max(.x$Date), "%Y-%m"))
  ),
  tibble(
    Split = "Chronological",
    Set = names(chronological_split),
    Rows = map_int(chronological_split, nrow),
    FirstMonth = map_chr(chronological_split, ~ format(min(.x$Date), "%Y-%m")),
    LastMonth = map_chr(chronological_split, ~ format(max(.x$Date), "%Y-%m"))
  )
)

write_csv(split_summary, file.path("output", "models", "split_summary.csv"))
print(split_summary)

# The three models answer increasingly demanding questions.
# Baseline: can a model beat simply predicting the training mean?
# Initial additive MLR: do month-of-year and demographic categories explain counts?
# MLR with TimeIndex + I(TimeIndex^2): does the taught nonlinear feature help
# predictions of later months when calendar-month seasonality is also retained?

additive_formula <- VisitorArrivals ~
  MonthFactor + CountryShort + TravelPurpose + AgeFactor + Sex

quadratic_time_formula <- VisitorArrivals ~
  TimeIndex + I(TimeIndex^2) + MonthFactor +
  CountryShort + TravelPurpose + AgeFactor + Sex

calculate_metrics <- function(actual, predicted) {
  errors <- actual - predicted
  mse <- mean(errors^2)

  tibble(
    MSE = mse,
    RMSE = sqrt(mse),
    MAE = mean(abs(errors)),
    R2 = 1 - sum(errors^2) / sum((actual - mean(actual))^2),
    NegativePredictions = sum(predicted < 0),
    NegativePredictionPercent = 100 * mean(predicted < 0)
  )
}

fit_and_evaluate <- function(split_list, split_name) {
  training_data <- split_list$Train
  validation_data <- split_list$Validation
  test_data <- split_list$Test

  training_mean <- mean(training_data$VisitorArrivals)
  additive_model <- lm(additive_formula, data = training_data)
  quadratic_time_model <- lm(quadratic_time_formula, data = training_data)

  score_dataset <- function(evaluation_data, set_name) {
    prediction_table <- bind_rows(
      evaluation_data %>%
        transmute(
          RowID, Date, Actual = VisitorArrivals,
          Model = "Mean baseline",
          Predicted = training_mean
        ),
      evaluation_data %>%
        transmute(
          RowID, Date, Actual = VisitorArrivals,
          Model = "Initial additive MLR",
          Predicted = as.numeric(predict(additive_model, newdata = evaluation_data))
        ),
      evaluation_data %>%
        transmute(
          RowID, Date, Actual = VisitorArrivals,
          Model = "MLR: TimeIndex + I(TimeIndex^2)",
          Predicted = as.numeric(predict(quadratic_time_model, newdata = evaluation_data))
        )
    )

    metrics <- prediction_table %>%
      group_by(Model) %>%
      group_modify(~ calculate_metrics(.x$Actual, .x$Predicted)) %>%
      ungroup() %>%
      mutate(Split = split_name, Set = set_name, .before = 1)

    list(Predictions = prediction_table, Metrics = metrics)
  }

  validation_results <- score_dataset(validation_data, "Validation")
  test_results <- score_dataset(test_data, "Test")

  list(
    Models = list(
      InitialAdditiveMLR = additive_model,
      QuadraticTimeMLR = quadratic_time_model
    ),
    Metrics = bind_rows(validation_results$Metrics, test_results$Metrics),
    TestPredictions = test_results$Predictions
  )
}

random_results <- fit_and_evaluate(random_split, "Random")
chronological_results <- fit_and_evaluate(
  chronological_split,
  "Chronological"
)

model_metrics <- bind_rows(
  random_results$Metrics,
  chronological_results$Metrics
) %>%
  arrange(Split, Set, RMSE)

write_csv(model_metrics, file.path("output", "models", "model_metrics.csv"))
write_csv(
  chronological_results$TestPredictions,
  file.path("output", "models", "chronological_test_predictions.csv")
)

write_rds(
  random_results$Models,
  file.path("output", "models", "random_split_models.rds")
)
write_rds(
  chronological_results$Models,
  file.path("output", "models", "chronological_split_models.rds")
)

capture.output(
  summary(random_results$Models$InitialAdditiveMLR),
  file = file.path("output", "models", "random_additive_model_summary.txt")
)
capture.output(
  summary(random_results$Models$TimeAwareMLR),
  file = file.path("output", "models", "random_time_aware_model_summary.txt")
)
capture.output(
  summary(chronological_results$Models$InitialAdditiveMLR),
  file = file.path("output", "models", "chronological_additive_model_summary.txt")
)
capture.output(
  summary(chronological_results$Models$TimeAwareMLR),
  file = file.path("output", "models", "chronological_time_aware_model_summary.txt")
)

print(model_metrics)

# -----------------------------------------------------------------------------
# Validation checks
# -----------------------------------------------------------------------------

stopifnot(
  length(intersect(random_train_index, random_validation_index)) == 0,
  length(intersect(random_train_index, random_test_index)) == 0,
  length(intersect(random_validation_index, random_test_index)) == 0,
  sum(map_int(random_split, nrow)) == nrow(model_data),
  length(intersect(chronological_train_months, chronological_validation_months)) == 0,
  length(intersect(chronological_train_months, chronological_test_months)) == 0,
  length(intersect(chronological_validation_months, chronological_test_months)) == 0,
  max(chronological_train_months) < min(chronological_validation_months),
  max(chronological_validation_months) < min(chronological_test_months),
  all(is.finite(model_metrics$MSE)),
  all(is.finite(model_metrics$RMSE)),
  all(is.finite(model_metrics$MAE)),
  all(is.finite(model_metrics$R2))
)

# -----------------------------------------------------------------------------
# Model-comparison figures
# -----------------------------------------------------------------------------

test_metrics <- model_metrics %>%
  filter(Set == "Test") %>%
  mutate(
    Model = factor(
      Model,
      levels = c(
        "Mean baseline",
        "Initial additive MLR",
        "MLR: TimeIndex + I(TimeIndex^2)"
      )
    )
  )

comparison_plot <- ggplot(
  test_metrics,
  aes(x = Model, y = RMSE, fill = Split)
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.68,
    colour = "#24313A",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = scales::number(RMSE, accuracy = 1)),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3.3,
    colour = "#24313A"
  ) +
  scale_fill_manual(values = c(
    "Chronological" = "#C28A28",
    "Random" = "#256A8A"
  )) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12)),
    labels = scales::label_number()
  ) +
  labs(
    title = "Test RMSE by model and evaluation split",
    subtitle = "Lower is better; chronological testing is the more realistic future-prediction check",
    x = NULL,
    y = "RMSE (visitor arrivals)",
    fill = "Evaluation split"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", colour = "#24313A", size = 15),
    plot.subtitle = element_text(colour = "#52616B", size = 10.5),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(colour = "#24313A"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

chronological_monthly_predictions <- chronological_results$TestPredictions %>%
  group_by(Date, Model) %>%
  summarise(
    Actual = sum(Actual),
    Predicted = sum(Predicted),
    .groups = "drop"
  )

forecast_plot <- ggplot(chronological_monthly_predictions, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual"), linewidth = 0.9) +
  geom_line(aes(y = Predicted, colour = "Predicted"), linewidth = 0.8) +
  facet_wrap(~ Model, ncol = 1) +
  scale_colour_manual(values = c("Actual" = "#24313A", "Predicted" = "#C28A28")) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  labs(
    title = "Actual and predicted monthly totals in the chronological test period",
    subtitle = paste0(
      format(min(chronological_test_months), "%b %Y"),
      "–",
      format(max(chronological_test_months), "%b %Y"),
      "; model predictions summed across the 768 monthly subgroups"
    ),
    x = NULL,
    y = "Visitor arrivals",
    colour = NULL
  ) +
  theme_minimal(base_size = 11.5) +
  theme(
    plot.title = element_text(face = "bold", colour = "#24313A", size = 15),
    plot.subtitle = element_text(colour = "#52616B", size = 10.5),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", colour = "#24313A"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  file.path("output", "figures", "05_model_rmse_comparison.png"),
  comparison_plot,
  width = 10,
  height = 6,
  dpi = 180,
  bg = "white"
)

ggsave(
  file.path("output", "figures", "06_chronological_predictions.png"),
  forecast_plot,
  width = 10,
  height = 9,
  dpi = 180,
  bg = "white"
)

message("Model comparison, validation checks, and figures completed.")
