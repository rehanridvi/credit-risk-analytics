# Purpose: Evaluate credit scoring models on the internal held-out test split
#          drawn from the ENGINEERED modeling dataset (modeling_data.rds).
# Metrics: AUC, Gini, KS, Accuracy, Precision, Recall, F1.
# No cs-test.csv scoring — internal validation only.

suppressPackageStartupMessages({
  library(pROC)
  library(caret)
  library(dplyr)
})

set.seed(42)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

# Load artifacts (engineered pipeline)

artifacts <- readRDS("outputs/models/trained_models.rds")
test      <- readRDS("outputs/test_data.rds")
schema    <- readRDS("outputs/models/feature_schema.rds")

cat("Test rows:", nrow(test), "\n")
cat("Class distribution:\n"); print(table(test$defaulted))

cat("\n--- Feature schema used at training time ---\n")
cat(paste(schema$features, collapse = ", "), "\n\n")

# Sanity check: the held-out test set must contain every feature the model
# was trained on.
missing_feats <- setdiff(schema$features, names(test))
if (length(missing_feats) > 0) {
  stop("Test set is missing trained features: ",
       paste(missing_feats, collapse = ", "),
       "\nDid R/04_model_training.R load modeling_data.rds (engineered)?")
}
cat("Schema check passed: all", length(schema$features),
    "trained features present in test set.\n\n")


# Unified predict helper
# Handles caret `train` objects AND our native xgb_wrap (a list with $predict)

get_proba <- function(model, newdata) {
  if (inherits(model, "train")) {
    # caret models store the training-time predictor order; align columns
    train_feats <- model$finalModel$xNames
    if (is.null(train_feats)) train_feats <- setdiff(names(newdata), "defaulted")
    nd <- newdata[, train_feats, drop = FALSE]
    predict(model, nd, type = "prob")[, "Yes"]

  } else if (is.list(model) && !is.null(model$predict)) {
    # native xgb wrapper — closure selects feat_cols internally
    nd <- newdata[, setdiff(names(newdata), "defaulted"), drop = FALSE]
    as.numeric(model$predict(nd))

  } else {
    stop("Unknown model type")
  }
}
# Credit-model evaluator

evaluate_credit_model <- function(model, test_data, model_name) {
  proba  <- get_proba(model, test_data)
  actual <- as.integer(test_data$defaulted == "Yes")

  roc_obj <- pROC::roc(actual, proba, quiet = TRUE, direction = "<")
  auc     <- as.numeric(pROC::auc(roc_obj))
  gini    <- 2 * auc - 1
  ks      <- max(roc_obj$sensitivities + roc_obj$specificities - 1)

  thr <- as.numeric(
    pROC::coords(roc_obj, "best", best.method = "youden",
                 ret = "threshold", transpose = FALSE)$threshold[1]
  )

  pred <- as.integer(proba > thr)
  cm   <- caret::confusionMatrix(
    factor(pred,   levels = c(0, 1)),
    factor(actual, levels = c(0, 1)),
    positive = "1"
  )

  data.frame(
    model     = model_name,
    AUC       = round(auc,  4),
    Gini      = round(gini, 4),
    KS        = round(ks,   4),
    Threshold = round(thr,  4),
    Accuracy  = round(cm$overall["Accuracy"],  4),
    Precision = round(cm$byClass["Precision"], 4),
    Recall    = round(cm$byClass["Recall"],    4),
    F1        = round(cm$byClass["F1"],        4),
    stringsAsFactors = FALSE
  )
}


# Evaluate all three models on the engineered held-out split

cat("=========================================================\n")
cat("Internal held-out test split (", nrow(test), " rows )\n")
cat("=========================================================\n")

results <- bind_rows(
  evaluate_credit_model(artifacts$tuned$log, test, "Logistic"),
  evaluate_credit_model(artifacts$tuned$rf,  test, "Random Forest"),
  evaluate_credit_model(artifacts$tuned$xgb, test, "XGBoost")
)

print(results)
saveRDS(results, "outputs/model_metrics_internal.rds")


# Best model summary

best_idx  <- which.max(results$AUC)
best_name <- results$model[best_idx]
best_auc  <- results$AUC[best_idx]
best_gini <- results$Gini[best_idx]
best_ks   <- results$KS[best_idx]

cat("\n=========================================================\n")
cat("Best model:", best_name, "\n")
cat(sprintf("  AUC  : %.4f\n", best_auc))
cat(sprintf("  Gini : %.4f\n", best_gini))
cat(sprintf("  KS   : %.4f\n", best_ks))
cat("=========================================================\n")
cat("\nMetrics saved to outputs/model_metrics_internal.rds\n")