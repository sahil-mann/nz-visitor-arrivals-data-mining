# Remote Data Miners — Module 3 binary recovery classification
#
# Research question:
# Can Country, Travel Purpose, calendar month and time since reopening identify
# whether a market-month has reached its matching 2019 level?
#
# LEARN alignment:
# - glm(..., family = binomial)
# - predict(..., type = "response")
# - ifelse() probability threshold
# - table() confusion matrix
# - accuracy, sensitivity, specificity and precision
#
# The response is genuinely binary. Ordinary logistic regression is not used
# to classify the three-category Travel Purpose variable.

library(tidyverse)

input_path <- file.path("output", "recovery_classification_data.csv")

if (!file.exists(input_path)) {
  stop("Recovery data not found. Run R/05_recovery_investigation.R first.")
}

recovery_data <- read_csv(input_path, show_col_types = FALSE) %>%
  filter(Date <= as.Date("2025-12-01")) %>%
  mutate(
    Date = as.Date(Date),
    Recovered = as.logical(Recovered),
    MonthFactor = factor(MonthNumber, levels = 1:12),
    CountryShort = factor(CountryShort),
    TravelPurpose = factor(TravelPurpose)
  )

stopifnot(
  nrow(recovery_data) == 41 * 8 * 3,
  all(!is.na(recovery_data$Recovered)),
  n_distinct(recovery_data$Recovered) == 2
)

classification_formula <-
  Recovered ~ MonthFactor + MonthsSinceReopening +
  I(MonthsSinceReopening^2) + CountryShort + TravelPurpose

safe_divide <- function(numerator, denominator) {
  ifelse(denominator == 0, NA_real_, numerator / denominator)
}

classification_metrics <- function(
    actual,
    predicted,
    model_name,
    evaluation_design,
    split,
    threshold
) {
  actual <- as.logical(actual)
  predicted <- as.logical(predicted)

  true_positive <- sum(predicted & actual)
  false_positive <- sum(predicted & !actual)
  true_negative <- sum(!predicted & !actual)
  false_negative <- sum(!predicted & actual)

  sensitivity <- safe_divide(true_positive, true_positive + false_negative)
  specificity <- safe_divide(true_negative, true_negative + false_positive)
  precision <- safe_divide(true_positive, true_positive + false_positive)
  recall <- sensitivity

  tibble(
    EvaluationDesign = evaluation_design,
    Split = split,
    Model = model_name,
    Threshold = threshold,
    N = length(actual),
    Positives = sum(actual),
    PositivePercent = 100 * mean(actual),
    TruePositive = true_positive,
    FalsePositive = false_positive,
    TrueNegative = true_negative,
    FalseNegative = false_negative,
    Accuracy = mean(actual == predicted),
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1 = ifelse(
      is.na(precision) || is.na(recall) || precision + recall == 0,
      NA_real_,
      2 * precision * recall / (precision + recall)
    ),
    BalancedAccuracy = mean(c(sensitivity, specificity), na.rm = TRUE)
  )
}

threshold_search <- function(actual, probability, evaluation_design) {
  bind_rows(lapply(seq(0.05, 0.95, by = 0.01), function(threshold) {
    predicted <- ifelse(probability >= threshold, TRUE, FALSE)
    classification_metrics(
      actual,
      predicted,
      "Multiple logistic regression",
      evaluation_design,
      "Validation threshold search",
      threshold
    )
  })) %>%
    arrange(desc(BalancedAccuracy), desc(F1), Threshold)
}

evaluate_design <- function(
    training_data,
    validation_data,
    test_data,
    evaluation_design
) {
  validation_model <- glm(
    classification_formula,
    data = training_data,
    family = binomial
  )

  validation_probability <- predict(
    validation_model,
    newdata = validation_data,
    type = "response"
  )

  threshold_results <- threshold_search(
    validation_data$Recovered,
    validation_probability,
    evaluation_design
  )
  selected_threshold <- threshold_results$Threshold[1]

  final_model <- glm(
    classification_formula,
    data = bind_rows(training_data, validation_data),
    family = binomial
  )

  test_probability <- predict(
    final_model,
    newdata = test_data,
    type = "response"
  )

  majority_class <- mean(bind_rows(training_data, validation_data)$Recovered) >= 0.5
  majority_prediction <- rep(majority_class, nrow(test_data))
  default_prediction <- ifelse(test_probability >= 0.50, TRUE, FALSE)
  tuned_prediction <- ifelse(
    test_probability >= selected_threshold,
    TRUE,
    FALSE
  )

  test_metrics <- bind_rows(
    classification_metrics(
      test_data$Recovered,
      majority_prediction,
      "Majority-class baseline",
      evaluation_design,
      "Test",
      NA_real_
    ),
    classification_metrics(
      test_data$Recovered,
      default_prediction,
      "Logistic regression: 0.50 threshold",
      evaluation_design,
      "Test",
      0.50
    ),
    classification_metrics(
      test_data$Recovered,
      tuned_prediction,
      "Logistic regression: validation threshold",
      evaluation_design,
      "Test",
      selected_threshold
    )
  )

  predictions <- test_data %>%
    transmute(
      EvaluationDesign = evaluation_design,
      Date,
      CountryShort,
      TravelPurpose,
      ActualRecovered = Recovered,
      PredictedProbability = test_probability,
      DefaultPrediction = default_prediction,
      TunedThreshold = selected_threshold,
      TunedPrediction = tuned_prediction
    )

  list(
    model = final_model,
    threshold_results = threshold_results,
    selected_threshold = selected_threshold,
    test_metrics = test_metrics,
    predictions = predictions
  )
}

# -----------------------------------------------------------------------------
# Evaluation 1: Module 3 model with the Module 2 random 80/10/10 split.
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

random_results <- evaluate_design(
  recovery_data[random_training_index, ],
  recovery_data[random_validation_index, ],
  recovery_data[random_test_index, ],
  "Module 2 random 80/10/10"
)

# -----------------------------------------------------------------------------
# Evaluation 2: chronological robustness check.
# Train: Aug 2022–Jun 2024; validate: Jul–Dec 2024; test: all of 2025.
# -----------------------------------------------------------------------------

chronological_training <- recovery_data %>%
  filter(Date <= as.Date("2024-06-01"))

chronological_validation <- recovery_data %>%
  filter(Date >= as.Date("2024-07-01"), Date <= as.Date("2024-12-01"))

chronological_test <- recovery_data %>%
  filter(Year == 2025)

chronological_results <- evaluate_design(
  chronological_training,
  chronological_validation,
  chronological_test,
  "Chronological future check"
)

all_metrics <- bind_rows(
  random_results$test_metrics,
  chronological_results$test_metrics
) %>%
  arrange(EvaluationDesign, desc(BalancedAccuracy))

all_threshold_results <- bind_rows(
  random_results$threshold_results,
  chronological_results$threshold_results
)

all_predictions <- bind_rows(
  random_results$predictions,
  chronological_results$predictions
)

write_csv(
  all_metrics,
  file.path("output", "models", "recovery_classification_metrics.csv")
)
write_csv(
  all_threshold_results,
  file.path("output", "models", "recovery_classification_thresholds.csv")
)
write_csv(
  all_predictions,
  file.path("output", "models", "recovery_classification_predictions.csv")
)

saveRDS(
  random_results$model,
  file.path("output", "models", "recovery_logistic_random.rds")
)
saveRDS(
  chronological_results$model,
  file.path("output", "models", "recovery_logistic_chronological.rds")
)

capture.output(
  summary(chronological_results$model),
  file = file.path("output", "models", "recovery_logistic_chronological_summary.txt")
)

cat("Selected random validation threshold:", random_results$selected_threshold, "\n")
cat(
  "Selected chronological validation threshold:",
  chronological_results$selected_threshold,
  "\n"
)

print(all_metrics)

cat("\nChronological tuned-threshold confusion matrix:\n")
print(table(
  Actual = chronological_results$predictions$ActualRecovered,
  Predicted = chronological_results$predictions$TunedPrediction
))

cat("Recovery classification comparison complete.\n")
