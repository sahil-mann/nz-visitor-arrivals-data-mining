# Remote Data Miners - audited Term 4 tree-based extension
#
# This script preserves the group's original Tree_based_methods.R file and
# rebuilds the strongest part of that analysis with two safeguards:
#   1. tuning decisions are made without using the final test set; and
#   2. a chronological holdout checks whether performance survives when the
#      model predicts genuinely later months.
#
# Course methods used: regression trees, cross-validated pruning, bagging,
# random forests, validation/test evaluation, MSE, RMSE, MAE and R-squared.

if (dir.exists(".Rlib")) {
  .libPaths(c(".Rlib", .libPaths()))
}

library(tidyverse)
library(tree)
library(randomForest)

clean_path <- file.path("data", "processed", "visitor_arrivals_clean.rds")

if (!file.exists(clean_path)) {
  stop("Processed data not found. Run R/02_clean_data.R first.")
}

dir.create(file.path("output", "tree_methods"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)

age_levels <- c(
  "0-4 years", "5-9 years", "10-14 years", "15-19 years",
  "20-24 years", "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years", "50-54 years", "55-59 years",
  "60-64 years", "65-69 years", "70-74 years", "75 years and over"
)

# Retain every profile cell, but remove the border-closure interval used in the
# group's supplied tree analysis. This keeps the comparison aligned with that
# work while preserving all eight countries, three purposes, 16 age groups and
# both sex categories outside the excluded interval.
model_data <- read_rds(clean_path) %>%
  filter(!(Date >= as.Date("2020-03-01") & Date <= as.Date("2022-02-01"))) %>%
  mutate(
    RowID = row_number(),
    MonthFactor = factor(MonthNumber, levels = 1:12),
    CountryShort = factor(CountryShort),
    TravelPurpose = factor(TravelPurpose),
    AgeFactor = factor(as.character(Age), levels = age_levels),
    Sex = factor(Sex)
  )

predictor_formula <- VisitorArrivals ~
  CountryShort + TravelPurpose + AgeFactor + Sex + MonthFactor

metric_row <- function(actual, predicted, split_name, set_name, model_name) {
  mse <- mean((actual - predicted)^2)
  tibble(
    Split = split_name,
    Set = set_name,
    Model = model_name,
    MSE = mse,
    RMSE = sqrt(mse),
    MAE = mean(abs(actual - predicted)),
    R2 = 1 - sum((actual - predicted)^2) /
      sum((actual - mean(actual))^2),
    # Treat machine-precision values such as -4e-13 as zero.
    NegativePredictions = sum(predicted < -1e-8),
    NegativePredictionPercent = 100 * mean(predicted < -1e-8)
  )
}

fit_rf_grid <- function(train_data, evaluation_data, split_name, set_name,
                        ntree = 10) {
  map_dfr(2:5, function(candidate_mtry) {
    set.seed(10)
    fitted_model <- randomForest(
      predictor_formula,
      data = train_data,
      mtry = candidate_mtry,
      ntree = ntree,
      importance = TRUE
    )

    predicted <- predict(fitted_model, newdata = evaluation_data)
    metric_row(
      evaluation_data$VisitorArrivals,
      predicted,
      split_name,
      set_name,
      paste0("Random forest (mtry = ", candidate_mtry, ")")
    ) %>%
      mutate(
        mtry = candidate_mtry,
        OOB_RMSE = sqrt(tail(fitted_model$mse, 1))
      )
  })
}

fit_selected_models <- function(train_data, test_data, split_name,
                                selected_mtry, ntree = 10) {
  mean_prediction <- rep(mean(train_data$VisitorArrivals), nrow(test_data))

  cat(split_name, ": additive MLR...\n", sep = "")
  additive_mlr <- lm(predictor_formula, data = train_data)
  mlr_prediction <- predict(additive_mlr, newdata = test_data)

  cat(split_name, ": regression tree...\n", sep = "")
  # cv.tree() refits the saved model call. Give that call a stable data name
  # for the duration of this function, then remove it automatically.
  assign(".tree_cv_train_data", train_data, envir = .GlobalEnv)
  on.exit(
    rm(".tree_cv_train_data", envir = .GlobalEnv),
    add = TRUE
  )
  regression_tree <- tree(
    predictor_formula,
    data = .tree_cv_train_data
  )
  tree_prediction <- predict(regression_tree, newdata = test_data)

  cat(split_name, ": pruned tree...\n", sep = "")
  set.seed(10)
  tree_cv <- cv.tree(regression_tree)
  best_size <- tree_cv$size[which.min(tree_cv$dev)]
  pruned_tree <- prune.tree(regression_tree, best = best_size)
  pruned_prediction <- predict(pruned_tree, newdata = test_data)

  cat(split_name, ": bagging...\n", sep = "")
  set.seed(10)
  bagged_model <- randomForest(
    predictor_formula,
    data = train_data,
    mtry = 5,
    ntree = ntree,
    importance = TRUE
  )
  bagged_prediction <- predict(bagged_model, newdata = test_data)

  cat(split_name, ": selected random forest...\n", sep = "")
  set.seed(10)
  rf_model <- randomForest(
    predictor_formula,
    data = train_data,
    mtry = selected_mtry,
    ntree = ntree,
    importance = TRUE
  )
  rf_prediction <- predict(rf_model, newdata = test_data)

  metrics <- bind_rows(
    metric_row(test_data$VisitorArrivals, mean_prediction,
               split_name, "Test", "Mean baseline"),
    metric_row(test_data$VisitorArrivals, mlr_prediction,
               split_name, "Test", "Additive MLR"),
    metric_row(test_data$VisitorArrivals, tree_prediction,
               split_name, "Test", "Regression tree"),
    metric_row(test_data$VisitorArrivals, pruned_prediction,
               split_name, "Test", "Pruned regression tree"),
    metric_row(test_data$VisitorArrivals, bagged_prediction,
               split_name, "Test", "Bagging (mtry = 5)"),
    metric_row(test_data$VisitorArrivals, rf_prediction,
               split_name, "Test",
               paste0("Random forest (mtry = ", selected_mtry, ")"))
  ) %>%
    mutate(
      SelectedMtry = selected_mtry,
      PrunedTreeSize = best_size
    )

  predictions <- test_data %>%
    transmute(
      Split = split_name,
      Date,
      CountryShort,
      TravelPurpose,
      Age = AgeFactor,
      Sex,
      Actual = VisitorArrivals,
      MLR = mlr_prediction,
      RandomForest = rf_prediction
    )

  list(
    metrics = metrics,
    predictions = predictions,
    random_forest = rf_model,
    regression_tree = regression_tree,
    pruned_tree = pruned_tree,
    tree_cv = tree_cv
  )
}

# -----------------------------------------------------------------------------
# Evaluation 1: reproduce the group's row-random 80/20 logic, but choose mtry
# from out-of-bag training error rather than from the held-out test set.
# -----------------------------------------------------------------------------

set.seed(1)
cat("Fitting row-random models...\n")
random_train_index <- sample(
  seq_len(nrow(model_data)),
  size = floor(0.80 * nrow(model_data))
)

random_train <- model_data[random_train_index, ]
random_test <- model_data[-random_train_index, ]

random_tuning_cache <- file.path(
  "output", "tree_methods", "row_random_tuning_cache.csv"
)

if (file.exists(random_tuning_cache)) {
  random_tuning <- read_csv(random_tuning_cache, show_col_types = FALSE)
} else {
  random_tuning <- fit_rf_grid(
    random_train,
    random_test,
    split_name = "Row-random",
    set_name = "Diagnostic test"
  )
  write_csv(random_tuning, random_tuning_cache)
}
cat("Row-random tuning finished.\n")

random_selected_mtry <- random_tuning %>%
  slice_min(OOB_RMSE, n = 1, with_ties = FALSE) %>%
  pull(mtry)

random_results <- fit_selected_models(
  random_train,
  random_test,
  split_name = "Row-random",
  selected_mtry = random_selected_mtry
)
cat("Row-random final models finished.\n")

# -----------------------------------------------------------------------------
# Evaluation 2: complete-month chronological train/validation/test split.
# The validation year chooses mtry. The final test contains only later months.
# -----------------------------------------------------------------------------

chronological_train <- model_data %>% filter(Date <= as.Date("2024-04-01"))
cat("Fitting chronological models...\n")
chronological_validation <- model_data %>%
  filter(Date >= as.Date("2024-05-01"), Date <= as.Date("2025-04-01"))
chronological_test <- model_data %>% filter(Date >= as.Date("2025-05-01"))

chronological_tuning_cache <- file.path(
  "output", "tree_methods", "chronological_tuning_cache.csv"
)

if (file.exists(chronological_tuning_cache)) {
  chronological_tuning <- read_csv(
    chronological_tuning_cache,
    show_col_types = FALSE
  )
} else {
  chronological_tuning <- fit_rf_grid(
    chronological_train,
    chronological_validation,
    split_name = "Chronological",
    set_name = "Validation"
  )
  write_csv(chronological_tuning, chronological_tuning_cache)
}
cat("Chronological tuning finished.\n")

chronological_best_validation_rmse <- min(chronological_tuning$RMSE)

# Prefer the smaller forest when it is within 1% of the best validation RMSE.
# This avoids calling mtry = 5 a random forest when it is effectively bagging.
chronological_selected_mtry <- chronological_tuning %>%
  filter(RMSE <= 1.01 * chronological_best_validation_rmse) %>%
  slice_min(mtry, n = 1, with_ties = FALSE) %>%
  pull(mtry)

chronological_results <- fit_selected_models(
  chronological_train,
  chronological_test,
  split_name = "Chronological",
  selected_mtry = chronological_selected_mtry
)
cat("Chronological final models finished.\n")

all_metrics <- bind_rows(
  random_results$metrics,
  chronological_results$metrics
) %>%
  arrange(Split, RMSE)

all_predictions <- bind_rows(
  random_results$predictions,
  chronological_results$predictions
)

tuning_results <- bind_rows(random_tuning, chronological_tuning) %>%
  arrange(Split, mtry)

split_summary <- tibble(
  Split = c(
    "Row-random", "Row-random",
    "Chronological", "Chronological", "Chronological"
  ),
  Set = c("Train", "Test", "Train", "Validation", "Test"),
  Rows = c(
    nrow(random_train), nrow(random_test),
    nrow(chronological_train), nrow(chronological_validation),
    nrow(chronological_test)
  ),
  FirstMonth = c(
    min(random_train$Date), min(random_test$Date),
    min(chronological_train$Date), min(chronological_validation$Date),
    min(chronological_test$Date)
  ),
  LastMonth = c(
    max(random_train$Date), max(random_test$Date),
    max(chronological_train$Date), max(chronological_validation$Date),
    max(chronological_test$Date)
  )
)

importance_matrix <- importance(
  chronological_results$random_forest,
  type = 1,
  scale = TRUE
)

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
  all_metrics,
  file.path("output", "tree_methods", "audited_tree_model_metrics.csv")
)
write_csv(
  tuning_results,
  file.path("output", "tree_methods", "random_forest_tuning.csv")
)
write_csv(
  all_predictions,
  file.path("output", "tree_methods", "audited_tree_predictions.csv")
)
write_csv(
  split_summary,
  file.path("output", "tree_methods", "tree_split_summary.csv")
)
write_csv(
  importance_table,
  file.path("output", "tree_methods", "chronological_rf_importance.csv")
)

saveRDS(
  list(
    random = random_results,
    chronological = chronological_results,
    tuning = tuning_results
  ),
  file.path("output", "tree_methods", "audited_tree_models.rds")
)

comparison_plot_data <- all_metrics %>%
  filter(Model %in% c(
    "Mean baseline", "Additive MLR", "Regression tree",
    "Pruned regression tree", "Bagging (mtry = 5)",
    paste0("Random forest (mtry = ", random_selected_mtry, ")"),
    paste0("Random forest (mtry = ", chronological_selected_mtry, ")")
  )) %>%
  mutate(
    ModelGroup = case_when(
      str_starts(Model, "Random forest") ~ "Random forest",
      str_starts(Model, "Bagging") ~ "Bagging",
      TRUE ~ Model
    ),
    ModelGroup = factor(
      ModelGroup,
      levels = c(
        "Mean baseline", "Additive MLR", "Regression tree",
        "Pruned regression tree", "Bagging", "Random forest"
      )
    )
  )

comparison_plot <- ggplot(
  comparison_plot_data,
  aes(x = ModelGroup, y = RMSE, fill = Split)
) +
  geom_col(position = position_dodge(width = 0.74), width = 0.66) +
  geom_text(
    aes(label = round(RMSE, 1)),
    position = position_dodge(width = 0.74),
    vjust = -0.35,
    size = 4.2,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("Row-random" = "#2D9CDB", "Chronological" = "#E76F51")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(
    title = "Tree ensembles reduce prediction error in both evaluations",
    subtitle = "The chronological holdout tests genuinely later months rather than another sample of familiar months",
    x = NULL,
    y = "Test RMSE (visitor arrivals)",
    fill = "Evaluation"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 24, hjust = 1),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 13)
  )

ggsave(
  file.path("output", "figures", "15_tree_method_evaluation_gap.png"),
  comparison_plot,
  width = 12,
  height = 7,
  dpi = 320,
  bg = "white"
)
ggsave(
  file.path("output", "figures", "15_tree_method_evaluation_gap.pdf"),
  comparison_plot,
  width = 12,
  height = 7,
  device = "pdf",
  bg = "white"
)

importance_plot <- importance_table %>%
  mutate(Variable = forcats::fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Variable, y = Importance)) +
  geom_col(fill = "#0B6E69", width = 0.68) +
  coord_flip() +
  labs(
    title = "Country carries the strongest predictive signal",
    subtitle = "Permutation importance from the chronological random forest",
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
  file.path("output", "figures", "16_chronological_rf_variable_importance.png"),
  importance_plot,
  width = 10,
  height = 6,
  dpi = 320,
  bg = "white"
)
ggsave(
  file.path("output", "figures", "16_chronological_rf_variable_importance.pdf"),
  importance_plot,
  width = 10,
  height = 6,
  device = "pdf",
  bg = "white"
)

cat("\nSelected mtry (row-random, OOB): ", random_selected_mtry, "\n", sep = "")
cat("Selected mtry (chronological validation): ", chronological_selected_mtry, "\n\n", sep = "")
print(all_metrics)
