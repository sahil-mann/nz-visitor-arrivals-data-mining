# Investigation journey

## 1. Source audit

The project began by verifying the workbook structure, observation grain and
monthly completeness. This prevented the detailed cells from being described
as individual travellers.

## 2. First model and failure

An additive MLR created a numerical baseline but produced many negative arrival
predictions. The failure showed that a purely additive linear structure did not
capture the response well enough and that random row splitting alone would not
answer a future-prediction question.

## 3. A better research question

Exploratory work shifted the focus from “Have arrivals recovered?” to “Did the
same visitor market recover?” The eight-country total looked close to 2019, but
country-purpose segments followed very different paths.

## 4. Regression and classification

An interaction MLR tested whether Country and Travel Purpose should be read
together. A logistic model then classified recovered market-months. The
classification experiment showed that an apparently high accuracy could still
miss nearly all recovered cases when the positive class was uncommon.

## 5. Tree-based extension

Regression trees, pruning, bagging and random forests allowed nonlinear splits
and interactions without manually specifying every relationship. Validation
selected `mtry = 4`, and the final 100-tree random forest was refit on all
development months before one evaluation on the later holdout.

## 6. Communication and validation

The final workflow saved metrics, predictions and figures, then independently
checked the headline claims. The project was communicated through an A1 poster
and a presentation centred on the investigation process rather than a list of
model outputs.
