# Purpose: Score the external cs-test.csv with the FULL-refit models and
#          generate production deliverables (PD, credit score, risk bands).
#
# Flow:
#   cs-test.csv
#     → clean_features()          (R/01_data_cleaning.R)
#     → engineer_features()       (mirrors the SQL in R/03_build_db.R)
#     → align to training schema  (outputs/models/feature_schema.rds)
#     → score with FULL models    (log / rf / xgb)
#     → PD → score → risk band
#     → outputs/predictions/scored_loans.csv

suppressPackageStartupMessages({
  library(dplyr)
})

set.seed(42)
dir.create("outputs/predictions", recursive = TRUE, showWarnings = FALSE)

artifacts <- readRDS("outputs/models/trained_models.rds")
schema    <- readRDS("outputs/models/feature_schema.rds")

cat("Feature schema (", length(schema$features), " features ):\n")
cat(paste(schema$features, collapse = ", "), "\n\n")

# Load + clean cs-test.csv with the SAME pipeline as training

source("R/01_data_cleaning.R", local = TRUE)   # provides clean_features()

cs_path <- "data/cs-test.csv"
if (!file.exists(cs_path)) stop("Missing: ", cs_path)

cs_raw <- read.csv(cs_path, stringsAsFactors = FALSE)
cat("cs-test.csv loaded:", nrow(cs_raw), "x", ncol(cs_raw), "\n")

# Preserve labels if present 
has_col    <- "SeriousDlqin2yrs" %in% names(cs_raw)
raw_labels <- if (has_col) cs_raw$SeriousDlqin2yrs else NULL
n_labeled  <- if (has_col) sum(!is.na(raw_labels)) else 0L
has_labels <- n_labeled > 0

if (has_col && !has_labels) {
  cat("Note: 'SeriousDlqin2yrs' present but all NA — scoring only.\n")
} else if (has_labels) {
  cat("Labels detected:", n_labeled, "/", nrow(cs_raw), "rows.\n")
} else {
  cat("No label column — scoring only.\n")
}

# Rename + clean (identical to training)
cs_clean <- clean_features(cs_raw)

engineer_features <- function(d) {
  d %>%
    mutate(
      is_young          = as.integer(age < 30),
      has_90plus_late   = as.integer(late_90 >= 1),
      high_utilization  = as.integer(revolving_util > 0.7),
      high_debt_ratio   = as.integer(debt_ratio > 0.5),
      income_per_person = monthly_income / ifelse(dependents + 1 == 0,
                                                  NA, dependents + 1),
      lines_per_income  = open_lines / ifelse(monthly_income == 0,
                                              NA, monthly_income),
      total_late_events = late_30_59 + late_60_89 + late_90
    ) %>%
    # SQL NULLIF → 0 convention; also kills any Inf
    mutate(across(everything(), ~ ifelse(!is.finite(.), 0, .)))
}

cs_eng <- engineer_features(cs_clean)
cat("Engineered features added. NAs remaining:",
    sum(is.na(cs_eng)), "\n")

# 4. Align to training schema (column order + missing cols)

for (f in schema$features) {
  if (!f %in% names(cs_eng)) cs_eng[[f]] <- 0
}
cs_X <- cs_eng[, schema$features, drop = FALSE]

cs_X <- cs_X %>%
  mutate(across(everything(), ~ {
    if (is.factor(.))         as.numeric(.)
    else if (is.character(.)) as.numeric(factor(.))
    else                      as.numeric(.)
  }))

cat("Scoring matrix:", nrow(cs_X), "x", ncol(cs_X), "\n\n")


# Unified predict helper (mirrors the one in R/05)

get_proba <- function(model, newdata) {
  if (inherits(model, "train")) {
    train_feats <- model$finalModel$xNames
    if (is.null(train_feats)) train_feats <- names(newdata)
    nd <- newdata[, train_feats, drop = FALSE]
    predict(model, nd, type = "prob")[, "Yes"]
  } else if (is.list(model) && !is.null(model$predict)) {
    as.numeric(model$predict(newdata))
  } else {
    stop("Unknown model type")
  }
}

# Score with each FULL model

cat("Scoring with FULL models...\n")
pd_log <- get_proba(artifacts$full$log, cs_X)
pd_rf  <- get_proba(artifacts$full$rf,  cs_X)
pd_xgb <- get_proba(artifacts$full$xgb, cs_X)

# Consensus PD (
pd_blend <- (pd_log + pd_rf + pd_xgb) / 3


# Convert PD → credit score (300-900) and risk bands

pd_to_score <- function(pd) round(300 + (1 - pd) * 600)

scored <- data.frame(
  row_id       = seq_len(nrow(cs_X)),
  pd_logistic  = round(pd_log,   6),
  pd_rf        = round(pd_rf,    6),
  pd_xgb       = round(pd_xgb,   6),
  pd_blend     = round(pd_blend, 6),
  score        = pd_to_score(pd_blend),
  stringsAsFactors = FALSE
)

scored <- scored %>%
  mutate(
    pd_quantile = ntile(pd_blend, 10),   # decile 1 = safest, 10 = riskiest
    risk_band = case_when(
      pd_quantile <= 5 ~ "Low Risk",        # bottom 50%
      pd_quantile <= 8 ~ "Medium Risk",     # 51–80th pct
      pd_quantile <= 9 ~ "High Risk",       # 81–90th pct
      TRUE             ~ "Very High Risk"   # top 10%
    )
  )

if (has_labels) {
  scored$defaulted_actual <- raw_labels
}

write.csv(scored, "outputs/predictions/scored_loans.csv", row.names = FALSE)
saveRDS(scored,  "outputs/predictions/scored_loans.rds")

cat("\n========== SCORING SUMMARY ==========\n")
cat("Rows scored:", nrow(scored), "\n")
cat("Score range:", min(scored$score), "–", max(scored$score), "\n")
cat("Mean PD (blend):", round(mean(scored$pd_blend), 4), "\n\n")

cat("Risk band distribution:\n")
band_tbl <- scored %>%
  count(risk_band) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(factor(risk_band,
                 levels = c("Low Risk", "Medium Risk",
                            "High Risk", "Very High Risk")))
print(band_tbl)

if (has_labels) {
  cat("\nActual default rate by band (validation):\n")
  val <- scored %>%
    group_by(risk_band) %>%
    summarise(n           = n(),
              actual_rate = round(100 * mean(defaulted_actual), 2),
              .groups     = "drop") %>%
    arrange(factor(risk_band,
                   levels = c("Low Risk", "Medium Risk",
                              "High Risk", "Very High Risk")))
  print(val)
}

cat("\nArtifacts written:\n")
cat("  outputs/predictions/scored_loans.csv\n")
cat("  outputs/predictions/scored_loans.rds\n")

