if (tolower(basename(getwd())) == "shiny") setwd("..")

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(pROC)
})

# 1. Paths

PATH_SCORED   <- "outputs/predictions/scored_loans.rds"
PATH_KPIS     <- "outputs/business_kpis.csv"
PATH_METRICS  <- "outputs/model_metrics_internal.rds"
PATH_MODELS   <- "outputs/models/trained_models.rds"
PATH_SCHEMA   <- "outputs/models/feature_schema.rds"
PATH_TEST     <- "outputs/test_data.rds"

# 2. Load artifacts (fail loudly if any are missing)

need <- function(p) if (!file.exists(p)) stop("Missing artifact: ", p, call. = FALSE)
invisible(lapply(c(PATH_SCORED, PATH_KPIS, PATH_METRICS,
                   PATH_MODELS, PATH_SCHEMA, PATH_TEST), need))

scored  <- readRDS(PATH_SCORED)      # 101,503 scored applicants
kpis    <- read.csv(PATH_KPIS, stringsAsFactors = FALSE)
metrics <- readRDS(PATH_METRICS)     # internal test-split metrics
models  <- readRDS(PATH_MODELS)      # tuned + full
schema  <- readRDS(PATH_SCHEMA)      # list(features, target)
test    <- readRDS(PATH_TEST)        # 44,998 labeled rows for validation views


# Reusable helpers 

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
    mutate(across(-any_of("defaulted"),
                  ~ ifelse(!is.finite(.), 0, .)))
}

get_proba <- function(model, newdata) {
  if (inherits(model, "train")) {
    train_feats <- model$finalModel$xNames
    if (is.null(train_feats)) train_feats <- names(newdata)
    nd <- newdata[, train_feats, drop = FALSE]
    as.numeric(predict(model, nd, type = "prob")[, "Yes"])
  } else if (is.list(model) && !is.null(model$predict)) {
    as.numeric(model$predict(newdata))
  } else stop("Unknown model type")
}


# Enrich scored data with raw features for segmentation
#    (scored_loans.rds only carries PDs + bands + row_id; join back to cs-test)

cs_raw <- read.csv("data/cs-test.csv", stringsAsFactors = FALSE)
source("R/01_data_cleaning.R", local = TRUE)   # clean_features()
cs_clean  <- clean_features(cs_raw)
cs_eng    <- engineer_features(cs_clean)

# Merge row_id-aligned engineered features + PDs + bands
scored_full <- bind_cols(
  cs_eng %>% select(-any_of("defaulted")),
  scored %>% select(pd_logistic, pd_rf, pd_xgb, pd_blend, score, risk_band)
) %>%
  mutate(
    age_group = cut(age, c(0, 30, 45, 60, 100),
                    labels = c("<30", "30-44", "45-59", "60+")),
    delinq_bucket = case_when(
      late_90 == 0 ~ "No 90+ late",
      late_90 <= 2 ~ "1-2 events",
      TRUE         ~ "3+ events"
    ),
    util_band = case_when(
      revolving_util < 0.3 ~ "Low",
      revolving_util < 0.7 ~ "Medium",
      TRUE                 ~ "High"
    )
  )

# Preserve band ordering
scored_full$risk_band  <- factor(scored_full$risk_band,
                                 levels = c("Low Risk", "Medium Risk",
                                            "High Risk", "Very High Risk"))
scored_full$age_group  <- factor(scored_full$age_group,
                                 levels = c("<30", "30-44", "45-59", "60+"))
scored_full$delinq_bucket <- factor(scored_full$delinq_bucket,
                                    levels = c("No 90+ late", "1-2 events", "3+ events"))
scored_full$util_band  <- factor(scored_full$util_band,
                                 levels = c("Low", "Medium", "High"))
