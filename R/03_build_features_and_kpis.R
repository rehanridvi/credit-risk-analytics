#Implemented feature engineering and KPI calculations for the credit risk dataset.
#Generated modelling dataset and saved it as RDS and CSV for downstream model training and evaluation.

library(DBI)
library(RSQLite)
library(dplyr)

#Loading cleaned data
df <- read.csv("data/cleaned_data.csv", stringsAsFactors = FALSE)
if ("X" %in% names(df)) df$X <- NULL
names(df) <- tolower(gsub("[^A-Za-z0-9_]", "_", names(df)))
df$dependents[is.na(df$dependents)] <- 0

cat("Loaded:", nrow(df), "x", ncol(df), "\n\n")

#Build SQLite DB
db_path <- "data/cleaned_data.db"
if (file.exists(db_path)) file.remove(db_path)

con <- dbConnect(SQLite(), db_path)
dbWriteTable(con, "loans", df, overwrite = TRUE)
cat("DB ready. Rows:", dbGetQuery(con, "SELECT COUNT(*) n FROM loans")$n, "\n\n")


#FEATURE-ENGINEERED DATASET

modeling_data <- dbGetQuery(con, "
  SELECT
      defaulted,
      revolving_util, age, late_30_59, debt_ratio,
      monthly_income, open_lines, late_90, real_estate_loans,
      late_60_89, dependents, income_missing,
      CASE WHEN age < 30             THEN 1 ELSE 0 END AS is_young,
      CASE WHEN late_90 >= 1         THEN 1 ELSE 0 END AS has_90plus_late,
      CASE WHEN revolving_util > 0.7 THEN 1 ELSE 0 END AS high_utilization,
      CASE WHEN debt_ratio > 0.5     THEN 1 ELSE 0 END AS high_debt_ratio,
      COALESCE(monthly_income / NULLIF(dependents + 1, 0), 0)   AS income_per_person,
      COALESCE(open_lines * 1.0 / NULLIF(monthly_income, 0), 0) AS lines_per_income,
      late_30_59 + late_60_89 + late_90            AS total_late_events
  FROM loans
  WHERE age > 0
")

#no NAs allowed downstream (caret/xgboost reject them)
modeling_data[is.na(modeling_data)] <- 0
modeling_data[!is.finite(as.matrix(modeling_data))] <- 0   # catches Inf/-Inf too

write.csv(modeling_data, "data/modeling_data.csv", row.names = FALSE)
cat("Modeling data:", nrow(modeling_data), "x", ncol(modeling_data), "\n\n")


kpi_row <- function(category, kpi, value, insight) {
  data.frame(category = category, kpi = kpi,
             value = as.character(value), insight = insight)
}

#QUERIES
overall <- dbGetQuery(con, "
  SELECT COUNT(*) n,
         SUM(defaulted) defs,
         ROUND(100.0*SUM(defaulted)/COUNT(*), 2) rate,
         ROUND(AVG(monthly_income),0) inc,
         ROUND(AVG(age),1) age,
         ROUND(AVG(debt_ratio),3) dti,
         ROUND(AVG(revolving_util),3) util
  FROM loans
")

#Segment-level KPIs
age_kpi <- dbGetQuery(con, "
  SELECT CASE WHEN age < 30 THEN '<30' WHEN age < 45 THEN '30-44'
              WHEN age < 60 THEN '45-59' ELSE '60+' END grp,
         ROUND(100.0*AVG(defaulted),2) rate
  FROM loans GROUP BY grp ORDER BY rate DESC LIMIT 1
")

delinq_kpi <- dbGetQuery(con, "
  SELECT CASE WHEN late_90=0 THEN 'No 90+ late'
              WHEN late_90<=2 THEN '1-2' ELSE '3+' END grp,
         ROUND(100.0*AVG(defaulted),2) rate
  FROM loans GROUP BY grp ORDER BY rate DESC LIMIT 1
")

util_kpi <- dbGetQuery(con, "
  SELECT CASE WHEN revolving_util < 0.3 THEN 'Low'
              WHEN revolving_util < 0.7 THEN 'Medium'
              ELSE 'High' END grp,
         ROUND(100.0*AVG(defaulted),2) rate
  FROM loans GROUP BY grp ORDER BY rate DESC LIMIT 1
")

income_kpi <- dbGetQuery(con, "
  WITH q AS (SELECT monthly_income, defaulted,
                    NTILE(4) OVER (ORDER BY monthly_income) q FROM loans)
  SELECT q AS grp, ROUND(100.0*AVG(defaulted),2) rate
  FROM q GROUP BY q ORDER BY rate DESC LIMIT 1
")

#High-risk segment 
high_risk <- dbGetQuery(con, "
  SELECT COUNT(*) n, ROUND(100.0*AVG(defaulted),2) rate,
         ROUND(AVG(monthly_income),0) inc
  FROM loans
  WHERE revolving_util > (SELECT AVG(revolving_util) FROM loans)
    AND late_90 >= 1
")

# Top correlations
num_df <- df %>% select(where(is.numeric))
corr <- cor(num_df, use = "complete.obs")[, "defaulted"]
corr <- sort(corr[names(corr) != "defaulted"], decreasing = TRUE)

#Build the KPI table
kpis <- rbind(
  # Tier 1 — Portfolio health
  kpi_row("Portfolio", "Total loans", overall$n, "Portfolio size"),
  kpi_row("Portfolio", "Total defaults", overall$defs, "Accounts that defaulted"),
  kpi_row("Portfolio", "Overall default rate (%)", overall$rate, "Baseline to beat"),
  kpi_row("Portfolio", "Imbalance ratio",
          round((overall$n - overall$defs) / overall$defs, 2),
          "Class imbalance"),
  kpi_row("Portfolio", "Avg monthly income ($)", overall$inc, "Affluence proxy"),
  kpi_row("Portfolio", "Avg age (yrs)", overall$age, "Demographics"),
  kpi_row("Portfolio", "Avg debt ratio", overall$dti, "Debt burden"),
  kpi_row("Portfolio", "Avg revolving utilization", overall$util, "Credit usage"),

  #Risk drivers (worst segment for each dimension)
  kpi_row("Risk Driver", "Worst age group", age_kpi$grp,
          paste0(age_kpi$rate, "% default")),
  kpi_row("Risk Driver", "Worst delinquency bucket", delinq_kpi$grp,
          paste0(delinq_kpi$rate, "% default")),
  kpi_row("Risk Driver", "Worst utilization band", util_kpi$grp,
          paste0(util_kpi$rate, "% default")),
  kpi_row("Risk Driver", "Worst income quartile", income_kpi$grp,
          paste0(income_kpi$rate, "% default")),

  #Model readiness
  kpi_row("Model", "Top correlated feature", names(corr)[1],
          paste0("r = ", round(corr[1], 3))),
  kpi_row("Model", "2nd correlated feature", names(corr)[2],
          paste0("r = ", round(corr[2], 3))),
  kpi_row("Model", "3rd correlated feature", names(corr)[3],
          paste0("r = ", round(corr[3], 3))),
  kpi_row("Model", "High-risk segment size", high_risk$n,
          "Util > avg AND 90+ late"),
  kpi_row("Model", "High-risk default rate (%)", high_risk$rate,
          "Target segment rate"),

  #Business impact (rough estimate)
  kpi_row("Business", "Assumed exposure per default ($)",
          round(overall$inc * 6, 0), "6× income proxy"),
  kpi_row("Business", "Estimated total exposure ($)",
          format(overall$defs * overall$inc * 6, big.mark = ","),
          "Defaults × exposure"),
  kpi_row("Business", "Preventable exposure @30% ($)",
          format(round(high_risk$n * (high_risk$rate/100) * overall$inc * 6 * 0.3, 0),
                 big.mark = ","),
          "If 30% of high-risk defaults prevented"),
  kpi_row("Business", "Approval rate if high-risk declined (%)",
          round((1 - high_risk$n / overall$n) * 100, 2),
          "Policy trade-off")
)

write.csv(kpis, "outputs/business_kpis.csv", row.names = FALSE)

cat("=========== BUSINESS KPIs ===========\n")
print(kpis, row.names = FALSE)

saveRDS(modeling_data, "data/modeling_data.rds")

cat("Modeling data (RDS):", nrow(modeling_data), "x", ncol(modeling_data), "\n")

dbDisconnect(con)
cat("   data/cleaned_data.db\n")
cat("   data/modeling_data.csv\n")
cat("   outputs/business_kpis.csv\n")