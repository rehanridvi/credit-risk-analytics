# R/07_visualizations.R
# Purpose: Diagnostic plots for the credit scoring models.


suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(pROC)
  library(caret)
})

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)


artifacts <- readRDS("outputs/models/trained_models.rds")
test      <- readRDS("outputs/test_data.rds")

# Unified predict helper (same as R/05)
get_proba <- function(model, newdata) {
  if (inherits(model, "train")) {
    predict(model, newdata, type = "prob")[, "Yes"]
  } else if (is.list(model) && !is.null(model$predict)) {
    nd <- newdata[, setdiff(names(newdata), "defaulted"), drop = FALSE]
    as.numeric(model$predict(nd))
  } else stop("Unknown model type")
}

y_true <- as.integer(test$defaulted == "Yes")

preds <- list(
  Logistic      = get_proba(artifacts$tuned$log, test),
  `Random Forest` = get_proba(artifacts$tuned$rf,  test),
  XGBoost       = get_proba(artifacts$tuned$xgb, test)
)

palette <- c("Logistic"      = "#2E86AB",
             "Random Forest" = "#A23B72",
             "XGBoost"       = "#F18F01")


# A. ROC curves (overlaid)

roc_list <- lapply(names(preds), function(nm) {
  r <- pROC::roc(y_true, preds[[nm]], quiet = TRUE, direction = "<")
  data.frame(
    model = nm,
    fpr   = 1 - r$specificities,
    tpr   = r$sensitivities,
    auc   = as.numeric(pROC::auc(r))
  )
})
roc_df <- bind_rows(roc_list)

roc_labels <- roc_df %>%
  group_by(model) %>%
  summarise(auc = first(auc), .groups = "drop") %>%
  mutate(label = sprintf("%s (AUC = %.3f)", model, auc))

p_roc <- ggplot(roc_df, aes(fpr, tpr, color = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey60") +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = palette) +
  labs(title = "ROC Curves — Held-Out Test Set",
       subtitle = paste0("N = ", nrow(test), "  |  base rate = ",
                         round(100 * mean(y_true), 2), "%"),
       x = "False Positive Rate (1 − Specificity)",
       y = "True Positive Rate (Sensitivity)",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.75, 0.20),
        legend.background = element_rect(fill = "white", color = "grey80"))

ggsave("outputs/plots/11_roc_curves.png", p_roc,
       width = 8, height = 6, dpi = 300)


# B. Precision-Recall curves

pr_list <- lapply(names(preds), function(nm) {
  r <- pROC::roc(y_true, preds[[nm]], quiet = TRUE, direction = "<")
  # Sweep thresholds from high → low to trace the PR curve
  thrs <- sort(unique(preds[[nm]]), decreasing = TRUE)
  # Thin to ~200 points for a clean plot
  thrs <- thrs[seq(1, length(thrs), length.out = min(200, length(thrs)))]
  keep <- thrs[thrs > 0]
  tp <- sapply(keep, function(t) sum(preds[[nm]] >= t & y_true == 1))
  fp <- sapply(keep, function(t) sum(preds[[nm]] >= t & y_true == 0))
  fn <- sapply(keep, function(t) sum(preds[[nm]] <  t & y_true == 1))
  prec <- tp / pmax(tp + fp, 1)
  rec  <- tp / pmax(tp + fn, 1)
  data.frame(model = nm, recall = rec, precision = prec)
})
pr_df <- bind_rows(pr_list)

p_pr <- ggplot(pr_df, aes(recall, precision, color = model)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = palette) +
  labs(title = "Precision-Recall Curves",
       subtitle = "Critical for imbalanced credit data (base rate 6.7%)",
       x = "Recall (Sensitivity)",
       y = "Precision",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.75, 0.80),
        legend.background = element_rect(fill = "white", color = "grey80"))

ggsave("outputs/plots/12_precision_recall.png", p_pr,
       width = 8, height = 6, dpi = 300)


# C. Confusion matrices (one per model, Youden threshold)

cm_list <- lapply(names(preds), function(nm) {
  r   <- pROC::roc(y_true, preds[[nm]], quiet = TRUE, direction = "<")
  thr <- as.numeric(pROC::coords(r, "best", best.method = "youden",
                                 ret = "threshold",
                                 transpose = FALSE)$threshold[1])
  pred_cls <- as.integer(preds[[nm]] > thr)
  cm <- table(Predicted = factor(pred_cls, levels = c(0, 1)),
              Actual    = factor(y_true,    levels = c(0, 1)))
  df <- as.data.frame(cm)
  df$model <- nm
  df
})
cm_df <- bind_rows(cm_list)

p_cm <- ggplot(cm_df, aes(Actual, Predicted, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Freq), color = "white", size = 5, fontface = "bold") +
  facet_wrap(~ model, nrow = 1) +
  scale_fill_gradient(low = "#4A90D9", high = "#B22222") +
  labs(title = "Confusion Matrices @ Youden Threshold",
       subtitle = "Rows = predicted class, Cols = actual  (0 = No Default, 1 = Default)",
       fill = "Count") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid  = element_blank())

ggsave("outputs/plots/13_confusion_matrices.png", p_cm,
       width = 11, height = 4, dpi = 300)


# D. KS plot (XGBoost — best model)

ks_df <- {
  p <- preds[["XGBoost"]]
  ord <- order(p)
  d1 <- y_true[ord] == 1
  d0 <- y_true[ord] == 0
  cum_pos <- cumsum(d1) / sum(d1)
  cum_neg <- cumsum(d0) / sum(d0)
  data.frame(score = p[ord], cum_pos = cum_pos, cum_neg = cum_neg)
}
ks_stat <- max(abs(ks_df$cum_pos - ks_df$cum_neg))
ks_at   <- ks_df$score[which.max(abs(ks_df$cum_pos - ks_df$cum_neg))]

p_ks <- ggplot(ks_df, aes(x = score)) +
  geom_line(aes(y = cum_pos, color = "Defaulters"),     linewidth = 1.1) +
  geom_line(aes(y = cum_neg, color = "Non-defaulters"), linewidth = 1.1) +
  geom_segment(aes(x = ks_at, xend = ks_at,
                   y = cum_pos[which.max(abs(ks_df$cum_pos - ks_df$cum_neg))],
                   yend = cum_neg[which.max(abs(ks_df$cum_pos - ks_df$cum_neg))]),
               color = "black", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = c("Defaulters" = "#B22222",
                                "Non-defaulters" = "#2E86AB")) +
  labs(title = sprintf("KS Plot — XGBoost (KS = %.3f)", ks_stat),
       subtitle = sprintf("Maximum separation at PD = %.3f", ks_at),
       x = "Predicted Probability of Default",
       y = "Cumulative Proportion",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.75, 0.25),
        legend.background = element_rect(fill = "white", color = "grey80"))

ggsave("outputs/plots/14_ks_plot.png", p_ks,
       width = 8, height = 6, dpi = 300)


# E. Score distribution — defaulters vs non-defaulters

dist_df <- data.frame(
  pd     = preds[["XGBoost"]],
  actual = factor(ifelse(y_true == 1, "Defaulted", "Did not default"),
                  levels = c("Did not default", "Defaulted"))
)

p_dist <- ggplot(dist_df, aes(pd, fill = actual)) +
  geom_density(alpha = 0.55, color = NA) +
  scale_fill_manual(values = c("Did not default" = "#2E86AB",
                               "Defaulted"       = "#B22222")) +
  labs(title = "Predicted PD Distribution by Actual Outcome",
       subtitle = "XGBoost — good separation means the model discriminates well",
       x = "Predicted Probability of Default",
       y = "Density", fill = NULL) +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/15_pd_distribution.png", p_dist,
       width = 8, height = 5, dpi = 300)


# F. Decile lift chart

lift_df <- data.frame(pd = preds[["XGBoost"]], y = y_true) %>%
  mutate(decile = ntile(desc(pd), 10)) %>%     # decile 1 = riskiest
  group_by(decile) %>%
  summarise(n            = n(),
            n_default    = sum(y),
            default_rate = mean(y),
            .groups      = "drop") %>%
  mutate(
    lift         = default_rate / mean(y_true),
    cum_defaults = cumsum(n_default) / sum(y_true),
    cum_pop      = cumsum(n) / sum(n)
  )

p_lift <- ggplot(lift_df, aes(factor(decile), default_rate)) +
  geom_col(fill = "#B22222", alpha = 0.85) +
  geom_hline(yintercept = mean(y_true), linetype = "dashed",
             color = "grey40") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * default_rate)),
            vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Decile Lift Chart — XGBoost",
       subtitle = sprintf("Baseline default rate = %.2f%% (dashed line)",
                          100 * mean(y_true)),
       x = "Risk Decile  (1 = highest predicted PD, 10 = lowest)",
       y = "Actual Default Rate") +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/16_decile_lift.png", p_lift,
       width = 9, height = 5.5, dpi = 300)


# G. Calibration plot — predicted vs actual 

cal_df <- data.frame(pd = preds[["XGBoost"]], y = y_true) %>%
  mutate(bin = ntile(pd, 10)) %>%
  group_by(bin) %>%
  summarise(mean_pred = mean(pd),
            mean_act  = mean(y),
            n         = n(),
            .groups   = "drop")

p_cal <- ggplot(cal_df, aes(mean_pred, mean_act)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey60") +
  geom_point(aes(size = n), color = "#F18F01", alpha = 0.8) +
  geom_line(color = "#F18F01", linewidth = 0.9) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Calibration Plot — XGBoost",
       subtitle = "Points near dashed line = well-calibrated PDs",
       x = "Mean Predicted PD (per bin)",
       y = "Mean Actual Default Rate", size = "n") +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/17_calibration.png", p_cal,
       width = 7.5, height = 6, dpi = 300)


# H. Feature importance — XGBoost

imp_xgb <- xgboost::xgb.importance(
  model = artifacts$tuned$xgb$booster,
  feature_names = artifacts$tuned$xgb$feat_cols
)

p_imp_xgb <- xgboost::xgb.ggplot.importance(imp_xgb, top_n = 12) +
  labs(title = "XGBoost Feature Importance (Gain)",
       subtitle = "Contribution to model's discriminatory power") +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/18_xgb_importance.png", p_imp_xgb,
       width = 8, height = 6, dpi = 300)


# I. Feature importance — Random Forest

rf_imp <- caret::varImp(artifacts$tuned$rf)
rf_imp_df <- rf_imp$importance %>%
  tibble::rownames_to_column("feature") %>%
  arrange(desc(Overall)) %>%
  slice_head(n = 12)

p_imp_rf <- ggplot(rf_imp_df, aes(reorder(feature, Overall), Overall)) +
  geom_col(fill = "#A23B72", alpha = 0.85) +
  coord_flip() +
  labs(title = "Random Forest Feature Importance",
       subtitle = "Mean decrease in Gini (top 12)",
       x = NULL, y = "Importance") +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/19_rf_importance.png", p_imp_rf,
       width = 8, height = 6, dpi = 300)


# J. Summary metrics bar chart

metrics <- readRDS("outputs/model_metrics_internal.rds")
metrics_long <- metrics %>%
  select(model, AUC, Gini, KS, F1) %>%
  pivot_longer(-model, names_to = "metric", values_to = "value")

p_metrics <- ggplot(metrics_long, aes(model, value, fill = model)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.4, size = 3.2) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = palette) +
  labs(title = "Model Comparison — Key Metrics",
       subtitle = "Held-out test set (N = 44,998)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

ggsave("outputs/plots/20_metrics_comparison.png", p_metrics,
       width = 12, height = 5, dpi = 300)

cat("\n=== All plots written to outputs/plots/ ===\n")
cat("  11_roc_curves.png\n")
cat("  12_precision_recall.png\n")
cat("  13_confusion_matrices.png\n")
cat("  14_ks_plot.png\n")
cat("  15_pd_distribution.png\n")
cat("  16_decile_lift.png\n")
cat("  17_calibration.png\n")
cat("  18_xgb_importance.png\n")
cat("  19_rf_importance.png\n")
cat("  20_metrics_comparison.png\n")