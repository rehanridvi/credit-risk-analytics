SELECT
    COUNT(*) AS total_loans,
    SUM(defaulted)  AS total_defaults,
    ROUND(100.0 * SUM(defaulted) / COUNT(*), 2) AS default_rate_pct,
    ROUND(AVG(monthly_income), 0) AS avg_monthly_income,
    ROUND(AVG(age), 1) AS avg_age,
    ROUND(AVG(debt_ratio), 3) AS avg_debt_ratio,
    ROUND(AVG(revolving_util), 3) AS avg_revolving_util
FROM loans;

-- DEFAULT RATE BY AGE GROUP
SELECT
    CASE
        WHEN age < 30 THEN 'Under 30'
        WHEN age < 45 THEN '30-44'
        WHEN age < 60 THEN '45-59'
        ELSE '60+'
    END AS age_group,
    COUNT(*)  AS loans,
    SUM(defaulted)  AS defaults,
    ROUND(100.0 * SUM(defaulted) / COUNT(*), 2) AS default_rate_pct
FROM loans
GROUP BY age_group
ORDER BY default_rate_pct DESC;

-- DEFAULT RATE BY DELINQUENCY HISTORY

SELECT
    CASE
        WHEN late_90 = 0 THEN 'No 90+ late'
        WHEN late_90 BETWEEN 1 AND 2  THEN '1-2 times'
        ELSE '3+ times'
    END  AS delinquency_bucket,
    COUNT(*)  AS loans,
    SUM(defaulted) AS defaults,
    ROUND(100.0 * SUM(defaulted) / COUNT(*), 2) AS default_rate_pct
FROM loans
GROUP BY delinquency_bucket
ORDER BY default_rate_pct DESC;


--REVOLVING UTILIZATION BANDS 

SELECT
    CASE
        WHEN revolving_util < 0.3 THEN 'Low (<30%)'
        WHEN revolving_util < 0.7 THEN 'Medium (30-70%)'
        WHEN revolving_util < 1.0 THEN 'High (70-100%)'
        ELSE 'Over limit (100%+)'
    END  AS utilization_band,
    COUNT(*) AS loans,
    ROUND(100.0 * AVG(defaulted), 2)AS default_rate_pct,
    ROUND(AVG(monthly_income), 0) AS avg_income
FROM loans
GROUP BY utilization_band
ORDER BY default_rate_pct DESC;


--MULTI-DIMENSIONAL RISK SEGMENTS (GROUP BY multiple cols)

SELECT
    CASE WHEN age < 40 THEN 'Young' ELSE 'Older' END  AS age_band,
    CASE
        WHEN revolving_util < 0.5 THEN 'Low Util'
        ELSE 'High Util'
    END  AS util_band,
    COUNT(*) AS loans,
    ROUND(100.0 * AVG(defaulted), 2) AS default_rate_pct
FROM loans
GROUP BY age_band, util_band
HAVING COUNT(*) > 1000
ORDER BY default_rate_pct DESC;


--Borrowers riskier than portfolio average

SELECT
    COUNT(*) AS high_risk_borrowers,
    ROUND(100.0 * AVG(defaulted), 2) AS their_default_rate
FROM loans
WHERE revolving_util > (
    SELECT AVG(revolving_util) FROM loans
)
AND late_90 >= 1;



--Income quartile analysis

WITH income_quartiles AS (
    SELECT
        monthly_income,
        defaulted,
        NTILE(4) OVER (ORDER BY monthly_income) AS quartile
    FROM loans
)
SELECT
    quartile,
    COUNT(*) AS loans,
    ROUND(AVG(monthly_income), 0) AS avg_income,
    ROUND(100.0 * AVG(defaulted), 2)  AS default_rate_pct
FROM income_quartiles
GROUP BY quartile
ORDER BY quartile;



--Rank risk segments by default rate

WITH segment_risk AS (
    SELECT
        CASE
            WHEN late_90 >= 3 THEN 'Very High Risk'
            WHEN late_90 >= 1 THEN 'High Risk'
            WHEN revolving_util > 0.7 THEN 'Medium Risk'
            ELSE 'Low Risk'
        END  AS risk_segment,
        COUNT(*) AS loans,
        100.0 * AVG(defaulted)AS default_rate
    FROM loans
    GROUP BY risk_segment
)
SELECT
    risk_segment,
    loans,
    ROUND(default_rate, 2) AS default_rate_pct,
    RANK() OVER (ORDER BY default_rate DESC) AS risk_rank
FROM segment_risk
ORDER BY risk_rank;


--HIGH-RISK PROFILE FILTER 

SELECT
    ROUND(AVG(age), 1)  AS avg_age,
    ROUND(AVG(monthly_income), 0)  AS avg_income,
    ROUND(AVG(debt_ratio), 3) AS avg_debt_ratio,
    ROUND(AVG(revolving_util), 3) AS avg_util,
    COUNT(*)  AS count
FROM loans
WHERE late_90 >= 2
  AND revolving_util > 0.5
  AND debt_ratio > 0.4;


--Default rate by age bucket + dependents
--(Simulates cohort tracking for credit policy)

SELECT
    CASE
        WHEN age < 30 THEN '<30'
        WHEN age < 45 THEN '30-44'
        ELSE '45+'
    END  AS age_group,
    CASE
        WHEN dependents = 0 THEN 'No deps'
        WHEN dependents <= 2 THEN '1-2 deps'
        ELSE '3+ deps'
    END  AS dependent_group,
    COUNT(*) AS loans,
    ROUND(100.0 * AVG(defaulted), 2)AS default_rate_pct
FROM loans
GROUP BY age_group, dependent_group
ORDER BY default_rate_pct DESC;