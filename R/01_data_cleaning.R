#renamed the columns to more meaningful names and cleaned the data by handling missing values, outliers, and creating new features.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

clean_features <- function(df) {

  df <- df %>%
    rename(
      defaulted         = SeriousDlqin2yrs,
      revolving_util    = RevolvingUtilizationOfUnsecuredLines,
      late_30_59        = NumberOfTime30.59DaysPastDueNotWorse,
      debt_ratio        = DebtRatio,
      monthly_income    = MonthlyIncome,
      open_lines        = NumberOfOpenCreditLinesAndLoans,
      late_90           = NumberOfTimes90DaysLate,
      real_estate_loans = NumberRealEstateLoansOrLines,
      late_60_89        = NumberOfTime60.89DaysPastDueNotWorse,
      dependents        = NumberOfDependents
    )

  df <- df %>%
    mutate(across(c(late_30_59, late_60_89, late_90),
                  ~ replace(., . %in% c(96, 98), NA)))

  df <- df %>%
    mutate(
      income_missing = as.integer(is.na(monthly_income)),
      monthly_income = ifelse(is.na(monthly_income),
                              median(monthly_income, na.rm = TRUE),
                              monthly_income)
    )

  df <- df %>%
    mutate(across(c(late_30_59, late_60_89, late_90),
                  ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

  df <- df %>%
    mutate(
      revolving_util = pmin(revolving_util, quantile(revolving_util, 0.99, na.rm = TRUE)),
      debt_ratio     = pmin(debt_ratio,     quantile(debt_ratio,     0.99, na.rm = TRUE))
    )

  df <- df %>%
    mutate(dependents = ifelse(is.na(dependents), 0, dependents)) %>%
    filter(age > 0)

  df
}

# only when sourced directly (not when source()'d from 05)
if (sys.nframe() == 0) {
  df <- read.csv("data/cs-training.csv")
  cat("\nTarget distribution (raw):\n"); print(table(df$SeriousDlqin2yrs))

  df <- clean_features(df)

  cat("\n--- After cleaning ---\n")
  cat("Shape:", nrow(df), "rows x", ncol(df), "columns\n")
  cat("Remaining NAs:", sum(is.na(df)), "\n")
  cat("Default rate:", round(mean(df$defaulted) * 100, 2), "%\n")

  saveRDS(df, "data/cleaned_data.rds")
  write.csv(df, "data/cleaned_data.csv", row.names = FALSE)

  cat("\nSaved: data/cleaned_data.rds and data/cleaned_data.csv\n")
  cat("Done.\n")
}