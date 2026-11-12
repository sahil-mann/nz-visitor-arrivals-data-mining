# Remote Data Miners - final Term 4 random forest
#
# Run R/11_tree_methods_audit.R first. That script selected mtry = 4 using
# validation performance while preferring the simpler model within 1% of the
# best validation RMSE. This script refits that selected model on all available
# development months and evaluates it once on the untouched future test period.

if (dir.exists(".Rlib")) {
  .libPaths(c(".Rlib", .libPaths()))
}

library(tidyverse)
library(randomForest)

clean_path <- file.path("data", "processed", "visitor_arrivals_clean.rds")
tuning_path <- file.path(
  "output", "tree_methods", "chronological_tuning_cache.csv"
)

if (!file.exists(clean_path) || !file.exists(tuning_path)) {
  stop("Run R/02_clean_data.R and R/11_tree_methods_audit.R first.")
}

age_levels <- c(
  "0-4 years", "5-9 years", "10-14 years", "15-19 years",
  "20-24 years", "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years", "50-54 years", "55-59 years",
  "60-64 years", "65-69 years", "70-74 years", "75 years and over"
)

model_data <- read_rds(clean_path) %>%
  filter(!(Date >= as.Date("2020-03-01") & Date <= as.Date("2022-02-01"))) %>%
  mutate(
    MonthFactor = factor(MonthNumber, levels = 1:12),
    CountryShort = factor(CountryShort),
    TravelPurpose = factor(TravelPurpose),
    AgeFactor = factor(as.character(Age), levels = age_levels),
    Sex = factor(Sex)
  )

development <- model_data %>% filter(Date <= as.Date("2025-04-01"))
future_test <- model_data %>% filter(Date >= as.Date("2025-05-01"))

predictor_formula <- VisitorArrivals ~
  CountryShort + TravelPurpose + AgeFactor + Sex + MonthFactor

tuning <- read_csv(tuning_path, show_col_types = FALSE)
best_validation_rmse <- min(tuning$RMSE)
selected_mtry <- tuning %>%
  filter(RMSE <= 1.01 * best_validation_rmse) %>%
  slice_min(mtry, n = 1, with_ties = FALSE) %>%
  pull(mtry)

metric_row <- function(actual, predicted, model_name) {
  mse <- mean((actual - predicted)^2)
  tibble(
    Model = model_name,
    MSE = mse,
    RMSE = sqrt(mse),
    MAE = mean(abs(actual - predicted)),
    R2 = 1 - sum((actual - predicted)^2) /
      sum((actual - mean(actual))^2),
    NegativePredictions = sum(predicted < -1e-8),
    NegativePredictionPercent = 100 * mean(predicted < -1e-8)
  )
}

cat("Fitting final additive MLR...\n")
final_mlr <- lm(predictor_formula, data = development)
mlr_prediction <- predict(final_mlr, newdata = future_test)

cat("Fitting final 100-tree random forest...\n")
set.seed(10)
final_rf <- randomForest(
  predictor_formula,
  data = development,
  mtry = selected_mtry,
  ntree = 100,
  importance = TRUE
)
rf_prediction <- predict(final_rf, newdata = future_test)

metrics <- bind_rows(
  metric_row(future_test$VisitorArrivals, mlr_prediction, "Additive MLR"),
  metric_row(
    future_test$VisitorArrivals,
    rf_prediction,
    paste0("Random forest (100 trees, mtry = ", selected_mtry, ")")
  )
)

predictions <- future_test %>%
  transmute(
    Date,
    CountryShort,
    TravelPurpose,
    Age = AgeFactor,
    Sex,
    Actual = VisitorArrivals,
    MLR = mlr_prediction,
    RandomForest = rf_prediction
  )

importance_matrix <- importance(final_rf, type = 1, scale = TRUE)
importance_table <- tibble(
  Variable = rownames(importance_matrix),
  Importance = as.numeric(importance_matrix[, 1])
) %>%
  mutate(
    Variable = recode(
      Variable,
      CountryShort = "Country",
      TravelPurpose = "Travel purpose",
      AgeFactor = "Age",
      MonthFactor = "Month"
    )
  ) %>%
  arrange(desc(Importance))

write_csv(
  metrics,
  file.path("output", "tree_methods", "final_future_test_metrics.csv")
)
write_csv(
  predictions,
  file.path("output", "tree_methods", "final_future_test_predictions.csv")
)
write_csv(
  importance_table,
  file.path("output", "tree_methods", "final_rf_importance.csv")
)
saveRDS(
  list(model = final_rf, mlr = final_mlr, selected_mtry = selected_mtry),
  file.path("output", "tree_methods", "final_future_models.rds")
)

importance_plot <- importance_table %>%
  mutate(Variable = forcats::fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Variable, y = Importance)) +
  geom_col(fill = "#0B6E69", width = 0.68) +
  coord_flip() +
  labs(
    title = "Age and country dominate predictive importance",
    subtitle = "Permutation importance from the final future-tested random forest",
    x = NULL,
    y = "Increase in prediction error after shuffling the variable"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 13)
  )

ggsave(
  file.path("output", "figures", "17_final_rf_variable_importance.png"),
  importance_plot,
  width = 10,
  height = 6,
  dpi = 320,
  bg = "white"
)
ggsave(
  file.path("output", "figures", "17_final_rf_variable_importance.pdf"),
  importance_plot,
  width = 10,
  height = 6,
  device = "pdf",
  bg = "white"
)

print(metrics)
print(importance_table)
