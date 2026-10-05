suppressPackageStartupMessages({
  library(caret)
  library(randomForest)
  library(xgboost)
  library(dplyr)
  library(glmnet)
  library(pROC)
})

set.seed(42)
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)

SAMPLE_N <- 150000

# 1. Load + prepare
df <- readRDS("data/modeling_data.rds")
df <- df %>% select(-any_of(c("X", "income_missing")))

df$defaulted <- factor(df$defaulted, levels = c(1, 0), labels = c("Yes", "No"))

df <- df %>%
  mutate(across(-defaulted, ~ {
    if (is.factor(.))         as.numeric(.)
    else if (is.character(.)) as.numeric(factor(.))
    else                      as.numeric(.)
  }))

cat("Original rows:", nrow(df), "\n")
cat("Features:", paste(setdiff(names(df), "defaulted"), collapse = ", "), "\n")
cat("Class distribution:\n"); print(table(df$defaulted))

#SAMPLING_DATA
if (SAMPLE_N < nrow(df)) {
  idx_keep <- createDataPartition(df$defaulted,
                                  p = SAMPLE_N / nrow(df),
                                  list = FALSE)
  df <- df[idx_keep, ]
}
gc()
cat("Sampled rows:", nrow(df), "\n")
cat("Sampled class distribution:\n"); print(table(df$defaulted))

#
idx   <- createDataPartition(df$defaulted, p = 0.70, list = FALSE)
train <- df[idx, ]
test  <- df[-idx, ]
gc()
cat("Train:", nrow(train), " Test:", nrow(test), "\n")


ctrl <- trainControl(
  method          = "cv",
  number          = 3,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel   = FALSE
)


cat("Fitting Logistic...\n")
log_model <- train(
  defaulted ~ ., data = train,
  method    = "glmnet",
  tuneGrid  = expand.grid(alpha = 0, lambda = c(0.001, 0.01, 0.1)),
  family    = "binomial",
  trControl = ctrl,
  metric    = "ROC"
)
gc()
cat("  Logistic done. lambda =", log_model$bestTune$lambda, "\n")


cat("Fitting Random Forest...\n")
rf_model <- train(
  defaulted ~ ., data = train,
  method    = "rf",
  tuneGrid  = expand.grid(mtry = 2),
  trControl = ctrl,
  metric    = "ROC",
  ntree     = 100
)
gc()
cat("  RF done.\n")


xgb_wrap <- function(train_df, test_df, nrounds = 200, max_depth = 3,
                     eta = 0.1, nthread = 1) {

  feat_cols <- setdiff(names(train_df), "defaulted")

  to_num_matrix <- function(d) {
    x <- d[, feat_cols, drop = FALSE]
    x[] <- lapply(x, function(col) {
      if (is.factor(col))         as.numeric(col)
      else if (is.character(col)) as.numeric(factor(col))
      else                        as.numeric(col)
    })
    as.matrix(x)
  }

  x_train <- to_num_matrix(train_df)
  x_test  <- to_num_matrix(test_df)
  y_train <- as.integer(train_df$defaulted == "Yes")
  y_test  <- as.integer(test_df$defaulted  == "Yes")

  dtrain <- xgb.DMatrix(x_train, label = y_train)
  dtest  <- xgb.DMatrix(x_test,  label = y_test)

  params <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    max_depth        = max_depth,
    eta              = eta,
    subsample        = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    nthread          = nthread
  )

  bst <- xgb.train(params = params, data = dtrain,
                   nrounds = nrounds, verbose = 0)

  list(
    booster   = bst,
    feat_cols = feat_cols,
    predict   = function(newdata) {
      x <- newdata[, feat_cols, drop = FALSE]
      x[] <- lapply(x, function(col) {
        if (is.factor(col))         as.numeric(col)
        else if (is.character(col)) as.numeric(factor(col))
        else                        as.numeric(col)
      })
      predict(bst, as.matrix(x))
    },
    params    = params,
    nrounds   = nrounds,
    bestTune  = data.frame(nrounds = nrounds, max_depth = max_depth, eta = eta)
  )
}

cat("Running XGB 3-fold CV...\n")
set.seed(42)
folds  <- createFolds(train$defaulted, k = 3)
cv_auc <- numeric(length(folds))

for (k in seq_along(folds)) {
  tr <- train[-folds[[k]], ]
  va <- train[ folds[[k]], ]
  m  <- xgb_wrap(tr, va, nrounds = 200, nthread = 1)
  p  <- m$predict(va)
  cv_auc[k] <- as.numeric(pROC::auc(
    pROC::roc(as.integer(va$defaulted == "Yes"), p, quiet = TRUE)
  ))
  cat("  Fold", k, "AUC:", round(cv_auc[k], 4), "\n")
  gc()
}
cat("XGB CV AUC:", round(mean(cv_auc), 4), "\n")

xgb_model <- xgb_wrap(train, test, nrounds = 200, nthread = 1)
gc()
cat("  XGB done.\n")


#Refit best configs on FULL 

none_ctrl <- trainControl(
  method          = "none",
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel   = FALSE
)

cat("Refitting on FULL sampled data (", nrow(df), " rows)...\n")

log_full <- train(
  defaulted ~ ., data = df,
  method    = "glmnet",
  tuneGrid  = log_model$bestTune,
  family    = "binomial",
  trControl = none_ctrl,
  metric    = "ROC"
)
gc()
cat("  Log full done.\n")

rf_full <- train(
  defaulted ~ ., data = df,
  method    = "rf",
  tuneGrid  = rf_model$bestTune,
  trControl = none_ctrl,
  metric    = "ROC",
  ntree     = 100
)
gc()
cat("  RF full done.\n")

xgb_full <- xgb_wrap(df, df, nrounds = 200, nthread = 1)
gc()
cat("  XGB full done.\n")

# Save artifacts
saveRDS(
  list(
    tuned = list(log = log_model, rf = rf_model, xgb = xgb_model),
    full  = list(log = log_full,  rf = rf_full,  xgb = xgb_full)
  ),
  "outputs/models/trained_models.rds"
)

saveRDS(test,  "outputs/test_data.rds")            # 18k held-out
saveRDS(df,    "outputs/train_full_clean.rds")     # full 60k for PSI
saveRDS(train, "outputs/train_only.rds")           # 42k train slice

saveRDS(
  list(features = setdiff(names(train), "defaulted"),
       target   = "defaulted"),
  "outputs/models/feature_schema.rds"
)

cat("\n=== Training complete ===\n")
cat("Sampled N       :", nrow(df),    "\n")
cat("Train N         :", nrow(train), "\n")
cat("Test N          :", nrow(test),  "\n")
cat("Best Log lambda :", log_model$bestTune$lambda, "\n")
cat("Best RF mtry    :", rf_model$bestTune$mtry,   "\n")
cat("XGB CV AUC      :", round(mean(cv_auc), 4),    "\n")
cat("Artifacts saved under outputs/\n")