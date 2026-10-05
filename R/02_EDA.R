#plotted the distributions of numeric variables, checked for class balance,
# and visualized relationships between features and the target variable.

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(scales)

df <- readRDS("data/cleaned_data.rds")

#drop X column if it exists
if ("X" %in% colnames(df)) {
  df <- df %>% select(-X)
}
cat("Shape:", nrow(df), "x", ncol(df), "\n")

#CLASS BALANCE CHECK & PLOT
cat("\nTarget distribution:\n")

balance <- df %>%
  count(defaulted) %>%
  mutate(percent = round(n / sum(n) * 100, 2))
print(balance)


p1 <- ggplot(df, aes(x = factor(defaulted), fill = factor(defaulted))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(
    title = "Class Balance: Target Variable",
    subtitle = "Only ~6.7% are defaults → imbalanced dataset",
    x = "Defaulted (0 = No, 1 = Yes)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/plots/01_class_balance.png", p1, width = 7, height = 5)


#skewnewss and shapes through distribution plots


num_cols <- df %>% select(where(is.numeric)) %>% colnames()

p3 <- df %>%
  select(all_of(num_cols)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  facet_wrap(~name, scales = "free", ncol = 3) +
  labs(title = "Distributions of All Numeric Variables") +
  theme_minimal() +
  theme(strip.text = element_text(size = 9))

ggsave("outputs/plots/02_distributions.png", p3, width = 14, height = 10)

#boxplot for outlier decisions

cat("\n--- 4. Outlier Detection ---\n")

p4 <- df %>%
  select(all_of(num_cols)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "steelblue", outlier.color = "firebrick", outlier.size = 0.5) +
  facet_wrap(~name, scales = "free", ncol = 3) +
  labs(title = "Boxplots: Outlier Detection") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggsave("outputs/plots/03_boxplots.png", p4, width = 14, height = 10)


#CORRELATION MATRIX

cat("\n--- 5. Correlation ---\n")

cor_mat <- df %>%
  select(all_of(num_cols)) %>%
  cor(use = "complete.obs")

png("outputs/plots/04_correlation.png", width = 900, height = 800)
corrplot(cor_mat, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45)
dev.off()

#top correlations with target
cat("\nTop correlations with 'defaulted':\n")
target_corr <- cor_mat[, "defaulted"] %>%
  sort(decreasing = TRUE) %>%
  round(3)
print(target_corr)


#DEFAULT RATE BY AGE GROUP
cat("\n--- 6. Default by Age ---\n")

age_stats <- df %>%
  mutate(age_group = cut(age, c(0, 30, 45, 60, 100),
                         labels = c("<30", "30-45", "45-60", "60+"))) %>%
  group_by(age_group) %>%
  summarise(
    loans        = n(),
    default_rate = round(mean(defaulted) * 100, 2)
  )
print(age_stats)

p6 <- ggplot(age_stats, aes(x = age_group, y = default_rate, fill = age_group)) +
  geom_col() +
  geom_text(aes(label = paste0(default_rate, "%")), vjust = -0.5) +
  labs(title = "Default Rate by Age Group", y = "Default Rate (%)", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/plots/05_default_by_age.png", p6, width = 7, height = 5)


#DEFAULT RATE BY DELINQUENCY HISTORY

cat("\n--- 7. Default by Delinquency ---\n")

delinq_stats <- df %>%
  mutate(late_bucket = case_when(
    late_90 == 0 ~ "No 90+ late",
    late_90 <= 2 ~ "1-2 times",
    TRUE         ~ "3+ times"
  )) %>%
  group_by(late_bucket) %>%
  summarise(
    loans        = n(),
    default_rate = round(mean(defaulted) * 100, 2)
  )
print(delinq_stats)

p7 <- ggplot(delinq_stats, aes(x = late_bucket, y = default_rate, fill = late_bucket)) +
  geom_col() +
  geom_text(aes(label = paste0(default_rate, "%")), vjust = -0.5) +
  labs(title = "Default Rate by 90+ Days Late History",
       y = "Default Rate (%)", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/plots/06_default_by_delinquency.png", p7, width = 7, height = 5)

#DEFAULT RATE BY REVOLVING UTILIZATION

cat("\n--- 8. Default by Utilization ---\n")

util_stats <- df %>%
  mutate(util_band = case_when(
    revolving_util < 0.3 ~ "Low (<30%)",
    revolving_util < 0.7 ~ "Medium (30-70%)",
    TRUE                 ~ "High (>70%)"
  )) %>%
  group_by(util_band) %>%
  summarise(
    loans        = n(),
    default_rate = round(mean(defaulted) * 100, 2)
  )
print(util_stats)

p8 <- ggplot(util_stats, aes(x = util_band, y = default_rate, fill = util_band)) +
  geom_col() +
  geom_text(aes(label = paste0(default_rate, "%")), vjust = -0.5) +
  labs(title = "Default Rate by Credit Utilization",
       y = "Default Rate (%)", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/plots/07_default_by_utilization.png", p8, width = 7, height = 5)

# DEFAULT RATE BY INCOME BAND
cat("\n--- 9. Default by Income ---\n")

income_stats <- df %>%
  mutate(income_band = cut(monthly_income,
                           breaks = quantile(monthly_income,
                                             probs = seq(0, 1, 0.25), na.rm = TRUE),
                           labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
                           include.lowest = TRUE)) %>%
  group_by(income_band) %>%
  summarise(
    loans        = n(),
    default_rate = round(mean(defaulted) * 100, 2)
  )
print(income_stats)

p9 <- ggplot(income_stats, aes(x = income_band, y = default_rate, fill = income_band)) +
  geom_col() +
  geom_text(aes(label = paste0(default_rate, "%")), vjust = -0.5) +
  labs(title = "Default Rate by Income Quartile",
       y = "Default Rate (%)", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/plots/08_default_by_income.png", p9, width = 7, height = 5)


#DEFAULT RATE BY DEBT RATIO

cat("\n--- 10. Default by Debt Ratio ---\n")

debt_stats <- df %>%
  mutate(debt_band = case_when(
    debt_ratio < 0.3 ~ "Low (<0.3)",
    debt_ratio < 0.6 ~ "Medium (0.3-0.6)",
    TRUE             ~ "High (>0.6)"
  )) %>%
  group_by(debt_band) %>%
  summarise(
    loans        = n(),
    default_rate = round(mean(defaulted) * 100, 2)
  )
print(debt_stats)

p10 <- ggplot(debt_stats, aes(x = debt_band, y = default_rate, fill = debt_band)) +
  geom_col() +
  geom_text(aes(label = paste0(default_rate, "%")), vjust = -0.5) +
  labs(title = "Default Rate by Debt Ratio",
       y = "Default Rate (%)", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/plots/09_default_by_debt_ratio.png", p10, width = 7, height = 5)



#DEFAULT RATE BY NUMBER OF DEPENDENTS

cat("\n--- 11. Default by Dependents ---\n")

dep_stats <- df %>%
  mutate(dep_bucket = ifelse(dependents >= 3, "3+", as.character(dependents))) %>%
  group_by(dep_bucket) %>%
  summarise(
    loans        = n(),
    default_rate = round(mean(defaulted) * 100, 2)
  ) %>% arrange(dep_bucket)
print(dep_stats)



#DEFAULT RATE BY OPEN CREDIT LINES

cat("\n--- 12. Default by Open Lines ---\n")

open_stats <- df %>%
  mutate(open_band = cut(open_lines, c(0, 5, 10, 15, 60),
                         labels = c("0-5", "6-10", "11-15", "15+"))) %>%
  group_by(open_band) %>%
  summarise(
    loans        = n(),
    default_rate = round(mean(defaulted) * 100, 2)
  )
print(open_stats)



#DEFAULT RATE BY REAL ESTATE LOANS

cat("\n--- 13. Default by Real Estate Loans ---\n")

re_stats <- df %>%
  mutate(re_bucket = ifelse(real_estate_loans >= 3, "3+", as.character(real_estate_loans))) %>%
  group_by(re_bucket) %>%
  summarise(
    loans        = n(),
    default_rate = round(mean(defaulted) * 100, 2)
  ) %>% arrange(re_bucket)
print(re_stats)



#DENSITY: REVOLVING UTILIZATION BY DEFAULT STATUS

p14 <- df %>%
  filter(revolving_util <= quantile(revolving_util, 0.99)) %>%
  ggplot(aes(x = revolving_util, fill = factor(defaulted))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("steelblue", "firebrick"),
                    labels = c("No Default", "Default")) +
  labs(title = "Revolving Utilization: Defaulters vs Non-Defaulters",
       x = "Revolving Utilization", fill = "") +
  theme_minimal()

ggsave("outputs/plots/10_utilization_density.png", p14, width = 8, height = 5)



#KEY FINDINGS SUMMARY
cat("\n============================================================\n")
cat("KEY FINDINGS\n")
cat("============================================================\n")
cat("1. Imbalance: Only", round(mean(df$defaulted) * 100, 2), "% defaults\n")
cat("2. Missing: monthly_income was imputed; income_missing flag retained\n")
cat("3. Age: Youngest borrowers (<30) have highest default rate\n")
cat("4. Delinquency: 90+ days late history is the strongest predictor\n")
cat("5. Utilization: High utilization (>70%) correlates with defaults\n")
cat("6. DebtRatio: Higher debt ratio → higher default risk\n")
cat("============================================================\n")

cat("\nEDA complete. Plots saved to outputs/plots/\n")