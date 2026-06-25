# HFPS Consumption Analysis

Phân tích tác động cú sốc thu nhập COVID-19 đến hành vi cắt giảm tiêu dùng thiết yếu của hộ gia đình đô thị Việt Nam.

## Dataset
- World Bank High-Frequency Phone Survey (HFPS) — Round 7
- N = 3,791 urban households

## Methods
- Binary Probit & Logistic Regression (robustness check)
- Average Marginal Effects (AME)
- VIF test (max 2.1), Hosmer-Lemeshow test (p = 0.506)
- Subgroup analysis: labor group & households with children
- Tools: R (glm, probitmfx, marginaleffects, sandwich/HC3, stargazer)

## Key Result
Income shock increases probability of essential consumption reduction by **11.6 percentage points** (p < 0.001), consistent with Permanent Income Hypothesis (Friedman, 1957).
