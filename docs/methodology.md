# Methodology

## Data pipeline

The analysis preserves the source workbook and performs every transformation in
numbered R scripts. The audit checks dimensions, categories, missing values,
duplicates, date coverage, zero-valued cells and the intended observation grain.
The cleaning script adds consistent date, month, country and factor variables
without deleting observations.

## Exploratory analysis

Grouped summaries and `ggplot2` figures examine total arrivals, country shares,
travel purpose, age, sex and seasonality. Recovery summaries compare 2025 with
the corresponding 2019 period across all eight countries and all three travel
purposes.

## Regression and classification iterations

The first multiple linear regression provided a documented baseline but
produced negative arrival predictions. Later models added time terms and a
Country × Travel Purpose interaction. A binary logistic model classified
whether each country-purpose month recovered to its matching 2019 value and
compared accuracy with sensitivity, specificity and balanced accuracy.

## Tree-based models

The tree extension compared:

- regression tree;
- cross-validated pruning;
- bagging;
- random forest; and
- additive MLR and mean baselines.

The predictors were Country, Travel Purpose, Age, Sex and Month. March 2020
through February 2022 was excluded from this final modelling comparison because
the border-closure period was an exceptional structural break.

The audit used both a row-random split and a stricter chronological split. The
chronological design trained through April 2024, tuned on May 2024 through April
2025, and tested on May 2025 through May 2026. The final random forest selected
`mtry = 4` from validation results, refit the model on development data through
April 2025, then evaluated it once on the untouched future test period.

## Evaluation

Regression models use MSE, RMSE, MAE and R². The analysis also counts negative
predictions because visitor arrivals cannot be negative. Classification uses a
confusion matrix, accuracy, sensitivity, specificity, precision and balanced
accuracy. Variable importance uses the increase in prediction error after
shuffling each predictor.
