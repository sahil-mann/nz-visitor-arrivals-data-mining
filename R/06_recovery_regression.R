# Remote Data Miners — Module 2 recovery regression
#
# Research question:
# Does the recovery path depend on the combination of Country and Travel
# Purpose, or is an additive model sufficient?
#
# LEARN alignment:
# - lm() and predict()
# - categorical predictors
# - I(x^2) nonlinear feature
# - interaction term CountryShort * TravelPurpose
# - validation/test MSE and RMSE

library(tidyverse)

input_path <- file.path("output", "recovery_classification_data.csv")

if (!file.exists(input_path)) {
  stop("Recovery data not found. Run R/05_recovery_investigation.R first.")
}

recovery_data <- read_csv(input_path, show_col_types = FALSE) %>%
  filter(Date <= as.Date("2025-12-01")) %>%
  mutate(
    Date = as.Date(Date),
    MonthFactor = factor(MonthNumber, levels = 1:12),
    CountryShort = factor(CountryShort),
    TravelPurpose = factor(TravelPurpose)
  )

stopifnot(
  nrow(recovery_data) == 41 * 8 * 3,
  n_distinct(recovery_data$CountryShort) == 8,
  n_distinct(recovery_data$TravelPurpose) == 3,
  all(is.finite(recovery_data$RecoveryPercent))
)

mean_baseline <- function(train_data, new_data) {
  rep(mean(train_data$RecoveryPercent), nrow(new_data))
}

metric_row <- function(actual, predicted, model_name, evaluation_design, split) {
  residuals <- actual - predicted
  tibble(
    EvaluationDesign = evaluation_design,
    Split = split,
    Model = model_name,
    N = length(actual),
    MSE = mean(residuals^2),
    RMSE = sqrt(mean(residuals^2)),
    MAE = mean(abs(residuals)),
    RSquared = 1 - sum(residuals^2) / sum((actual - mean(actual))^2),
    NegativePredictions = sum(predicted < 0),
    NegativePredictionPercent = 100 * mean(predicted < 0)
  )
}

additive_formula <-
  RecoveryPercent ~ MonthFactor + MonthsSinceReopening +
  I(MonthsSinceReopening^2) + CountryShort + TravelPurpose

interaction_formula <-
  RecoveryPercent ~ MonthFactor + MonthsSinceReopening +
  I(MonthsSinceReopening^2) + CountryShort * TravelPurpose

evaluate_models <- function(train_data, evaluation_data, design_name, split_name) {
  additive_model <- lm(additive_formula, data = train_data)
  interaction_model <- lm(interaction_formula, data = train_data)

  predictions <- list(
    "Mean baseline" = mean_baseline(train_data, evaluation_data),
    "Additive MLR" = predict(additive_model, newdata = evaluation_data),
    "Country × Purpose interaction MLR" =
      predict(interaction_model, newdata = evaluation_data)
  )

  metrics <- bind_rows(lapply(names(predictions), function(model_name) {
    metric_row(
      actual = evaluation_data$RecoveryPercent,
      predicted = predictions[[model_name]],
      model_name = model_name,
      evaluation_design = design_name,
      split = split_name
    )
  }))

  prediction_rows <- bind_rows(lapply(names(predictions), function(model_name) {
    evaluation_data %>%
      transmute(
        EvaluationDesign = design_name,
        Split = split_name,
        Model = model_name,
        Date,
        CountryShort,
        TravelPurpose,
        ActualRecoveryPercent = RecoveryPercent,
        PredictedRecoveryPercent = predictions[[model_name]],
        Error = RecoveryPercent - predictions[[model_name]]
      )
  }))

  list(
    metrics = metrics,
    predictions = prediction_rows,
    additive_model = additive_model,
    interaction_model = interaction_model
  )
}

# -----------------------------------------------------------------------------
# Evaluation 1: Module 2 random 80/10/10 split.
# This follows the course lab, but rows from nearby months can appear in all
# three sets. It is retained for course alignment, not as the main future test.
# -----------------------------------------------------------------------------

set.seed(462)
random_training_index <- sample(
  seq_len(nrow(recovery_data)),
  size = floor(0.80 * nrow(recovery_data))
)
random_remaining_index <- setdiff(seq_len(nrow(recovery_data)), random_training_index)
random_validation_index <- sample(
  random_remaining_index,
  size = floor(0.10 * nrow(recovery_data))
)
random_test_index <- setdiff(random_remaining_index, random_validation_index)

random_training <- recovery_data[random_training_index, ]
random_validation <- recovery_data[random_validation_index, ]
random_test <- recovery_data[random_test_index, ]

random_validation_results <- evaluate_models(
  random_training,
  random_validation,
  "Module 2 random 80/10/10",
  "Validation"
)

random_test_results <- evaluate_models(
  bind_rows(random_training, random_validation),
  random_test,
  "Module 2 random 80/10/10",
  "Test"
)

# -----------------------------------------------------------------------------
# Evaluation 2: chronological robustness check.
# Train: Aug 2022–Jun 2024; validate: Jul–Dec 2024; test: all of 2025.
# This asks the more realistic question: can past recovery paths describe a
# later complete calendar year?
# -----------------------------------------------------------------------------

chronological_training <- recovery_data %>%
  filter(Date <= as.Date("2024-06-01"))

chronological_validation <- recovery_data %>%
  filter(Date >= as.Date("2024-07-01"), Date <= as.Date("2024-12-01"))

chronological_test <- recovery_data %>%
  filter(Year == 2025)

stopifnot(
  nrow(chronological_training) == 23 * 8 * 3,
  nrow(chronological_validation) == 6 * 8 * 3,
  nrow(chronological_test) == 12 * 8 * 3
)

chronological_validation_results <- evaluate_models(
  chronological_training,
  chronological_validation,
  "Chronological future check",
  "Validation"
)

chronological_test_results <- evaluate_models(
  bind_rows(chronological_training, chronological_validation),
  chronological_test,
  "Chronological future check",
  "Test"
)

all_metrics <- bind_rows(
  random_validation_results$metrics,
  random_test_results$metrics,
  chronological_validation_results$metrics,
  chronological_test_results$metrics
) %>%
  arrange(EvaluationDesign, Split, RMSE)

all_predictions <- bind_rows(
  random_validation_results$predictions,
  random_test_results$predictions,
  chronological_validation_results$predictions,
  chronological_test_results$predictions
)

chronological_test_segment_error <- all_predictions %>%
  filter(
    EvaluationDesign == "Chronological future check",
    Split == "Test",
    Model != "Mean baseline"
  ) %>%
  group_by(Model, CountryShort, TravelPurpose) %>%
  summarise(
    MeanActualRecoveryPercent = mean(ActualRecoveryPercent),
    MeanPredictedRecoveryPercent = mean(PredictedRecoveryPercent),
    RMSE = sqrt(mean(Error^2)),
    MAE = mean(abs(Error)),
    .groups = "drop"
  ) %>%
  arrange(Model, desc(RMSE))

write_csv(all_metrics, file.path("output", "models", "recovery_regression_metrics.csv"))
write_csv(
  all_predictions,
  file.path("output", "models", "recovery_regression_predictions.csv")
)
write_csv(
  chronological_test_segment_error,
  file.path("output", "models", "recovery_regression_segment_error.csv")
)

final_additive_model <- lm(
  additive_formula,
  data = bind_rows(chronological_training, chronological_validation)
)
final_interaction_model <- lm(
  interaction_formula,
  data = bind_rows(chronological_training, chronological_validation)
)

saveRDS(final_additive_model, file.path("output", "models", "recovery_additive_mlr.rds"))
saveRDS(
  final_interaction_model,
  file.path("output", "models", "recovery_interaction_mlr.rds")
)

capture.output(
  summary(final_additive_model),
  file = file.path("output", "models", "recovery_additive_mlr_summary.txt")
)
capture.output(
  summary(final_interaction_model),
  file = file.path("output", "models", "recovery_interaction_mlr_summary.txt")
)

print(all_metrics)
print(chronological_test_segment_error)

cat("Recovery regression comparison complete.\n")
