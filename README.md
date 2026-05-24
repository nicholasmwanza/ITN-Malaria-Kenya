# ITN Use and Malaria in Kenya

**Author:** Nicholas Mwanza  
**Institution:** AIMS Cameroon  
**Date:** April 2026  

## Overview

This project estimates the causal effect of Insecticide-Treated Net (ITN) use on malaria infection among children under five in Kenya using data from the 2015 and 2020 Kenya Malaria Indicator Surveys (KMIS).

## Methods

Six aasociation and causal inference methods are implemented:
# Estimating the Causal Effect of ITN Use on Malaria Infection Among Children Under Five in Kenya

**Author:** Nicholas Mwanza  
**Supervisor:** Dr. Haisa Osmanli  
**Institution:** African Institute for Mathematical Sciences (AIMS) Cameroon  
**Date:** April 2026  

---

## Abstract

Malaria remains a leading cause of child mortality in sub-Saharan Africa. While insecticide-treated nets (ITNs) are the primary prevention tool, estimating their real-world effectiveness using observational data is challenging due to confounding from environmental, socioeconomic, and demographic factors.

This study estimates the causal effect of ITN use on malaria infection among children under five in Kenya using KMIS 2015 and 2020 data. The analytical sample comprises 6,544 children across 528 clusters. Six causal inference methods are implemented and compared: logistic regression, generalized linear mixed models (GLMM), propensity score matching (PSM), inverse probability of treatment weighting (IPTW), doubly robust (DR) estimation, and double machine learning (DML).

Key findings show substantial confounding. Unadjusted models show no association, but fully adjusted models reveal protective effects with odds ratio 0.777 and 95 percent confidence interval 0.605 to 0.999. GLMM-based causal methods produce stronger cluster-specific effects with odds ratios ranging from 0.392 to 0.562, representing 44 to 61 percent reduction. Double machine learning estimates population-average effects with odds ratio 0.972, representing a 2.8 percent reduction. The discrepancy is consistent with non-collapsibility given high intracluster correlation of 50 to 58 percent. E-value analysis gives 3.05, suggesting moderate robustness to unmeasured confounding.

Heterogeneity analysis reveals stronger effects among poorer households, urban areas, older children aged 36 to 59 months, and high-rainfall regions.

---

## Research Question

**Primary question:** What is the causal effect of ITN use on malaria infection among children under five in Kenya?

**Secondary questions:**

1. How do cluster-specific (conditional) effects compare with population-average (marginal) effects?

2. Does the effect vary across subgroups defined by wealth, residence, age, and rainfall?

---

## Data

### Sources

- KMIS PR file (2015, 2020): Individual-level malaria test results (hml32) and ITN use (hml12)
- KMIS HR file (2015, 2020): Household wealth, residence, water, sanitation, electricity
- DHS Geocov (2015, 2020): Rainfall, temperature, EVI, aridity, wet days, population
- Shapefiles (2015, 2020): GPS coordinates for spatial mapping

### Sample

After merging and complete-case analysis:

- KMIS 2015: 3,059 children, 245 clusters, malaria prevalence 5.1 percent, ITN use 58.0 percent
- KMIS 2020: 3,485 children, 296 clusters, malaria prevalence 4.5 percent, ITN use 50.5 percent
- Combined: 6,544 children, 528 clusters, malaria prevalence 4.8 percent, ITN use 54.1 percent

### Variables

**Outcome:** malaria - Blood smear test result (0 = negative, 1 = positive)

**Treatment:** itn_use - Slept under ITN last night (0 = no, 1 = yes)

**Covariates - Demographic:** age_months (child age in months), sex (Male/Female)

**Covariates - Socioeconomic:** wealth (Poorest, Poorer, Middle, Richer, Richest), residence (Urban/Rural)

**Covariates - Household:** electricity, improved_water, improved_sanitation

**Covariates - Environmental:** rainfall (mm), temp_mean (degrees Celsius), evi (Enhanced Vegetation Index), aridity, wet_days, population density

---

## Methods

### Identification Assumptions

Under the potential outcomes framework (Rubin, 1974), causal identification requires four assumptions:

1. Conditional ignorability: Given observed covariates, treatment assignment is independent of potential outcomes.

2. Positivity: 0 < P(T=1|X) < 1 for all X. Every combination of covariates must have nonzero probability of both treatment and control.

3. Consistency: Observed outcome equals potential outcome under treatment actually received.

4. No interference (SUTVA): One child's treatment does not affect another child's outcome.

### Statistical Methods

**Logistic regression (associational, marginal approximates):** Standard logistic regression with cluster-robust standard errors using R survey package. Three specifications: unadjusted, demographic+household, fully adjusted.

**Generalized Linear Mixed Models (associational, conditional, cluster-specific):** Logistic GLMM with random intercept for cluster using lme4. Estimates within-cluster effects. Intracluster correlation (ICC) calculated.

**Propensity Score Matching (causal, conditional, cluster-specific):** Propensity scores estimated using GLMM to preserve clustering. Nearest neighbor matching with caliper 0.05 standard deviations. Outcome model: GLMM with cluster random intercept. Implemented using MatchIt.

**Inverse Probability of Treatment Weighting (causal, conditional, cluster-specific):** Stabilized weights with truncation at maximum of 4. Outcome model: GLMM with cluster random intercept.

**Doubly Robust Estimation (causal, marginal, population-average):** Combines IPTW weights with outcome regression. Consistent if either propensity score or outcome model is correct. Implemented using marginaleffects package.

**Double Machine Learning (causal, marginal, population-average):** Partially linear model with Neyman orthogonal scores and 5-fold cross-fitting. Cluster-aware cross-fitting: all children from same cluster assigned to same fold. Primary learner: XGBoost (100 rounds, max depth 3, learning rate 0.1). Alternative learners: Random Forest (500-1000 trees, node size 5-10). Implemented using DoubleML package.

### Key Technical Features

- Cluster-aware cross-fitting: All children from same cluster assigned to same DML fold to preserve cluster structure and prevent information leakage.

- GLMM for propensity scores: Preserves hierarchical structure in treatment assignment model, unlike standard logistic regression.

- E-value sensitivity analysis: Quantifies minimum strength of unmeasured confounding needed to explain away results (VanderWeele & Ding, 2017). Higher E-values indicate greater robustness.

- Multiple learners: Random Forest and XGBoost tested for robustness of DML estimates.

---

## Results

### Main Findings

**Logistic regression (fully adjusted):** Combined sample odds ratio = 0.777, 95 percent confidence interval 0.605 to 0.999, p = 0.049.

**GLMM (cluster-specific association):** KMIS 2015 odds ratio = 0.557, 95 percent CI 0.352 to 0.879, p = 0.012. KMIS 2020 odds ratio = 0.562, 95 percent CI 0.378 to 0.836, p = 0.004. Combined odds ratio = 0.588, 95 percent CI 0.436 to 0.792, p = 0.001.

**PSM (causal, conditional):** KMIS 2015 odds ratio = 0.392, 95 percent CI 0.194 to 0.791, p = 0.009. KMIS 2020 odds ratio = 0.466, 95 percent CI 0.242 to 0.896, p = 0.022.

**IPTW (causal, conditional):** KMIS 2015 odds ratio = 0.482, 95 percent CI 0.319 to 0.728, p < 0.001. KMIS 2020 odds ratio = 0.511, 95 percent CI 0.349 to 0.750, p < 0.001.

**Doubly Robust (causal, marginal):** KMIS 2015 odds ratio = 0.994, 95 percent CI 0.972 to 1.017, p = 0.618. KMIS 2020 odds ratio = 0.992, 95 percent CI 0.976 to 1.009, p = 0.370.

**DML XGBoost (causal, marginal, primary):** KMIS 2015 odds ratio = 0.978, 95 percent CI 0.961 to 0.996, p = 0.018. KMIS 2020 odds ratio = 0.976, 95 percent CI 0.961 to 0.991, p = 0.002. Combined odds ratio = 0.972, 95 percent CI 0.960 to 0.984, p < 0.001.

### Heterogeneity Analysis (DML, Combined Sample)

- Wealth - Poorer (second quintile): N = 1,448, odds ratio = 0.919, p = 0.001
- Residence - Urban: N = 2,240, odds ratio = 0.974, p = 0.003
- Age - 36 to 59 months: N = 3,075, odds ratio = 0.963, p < 0.001
- Rainfall - Medium: N = 2,227, odds ratio = 0.950, p < 0.001
- Rainfall - High: N = 2,156, odds ratio = 0.967, p = 0.025

No statistically significant effects were detected for the poorest wealth quintile, rural residence, younger age groups (0-11, 12-23, 24-35 months), or low rainfall areas.

### E-value Sensitivity Analysis

For the primary DML estimate (odds ratio = 0.972):

- E-value (point estimate) = 3.05
- Interpretation: An unmeasured confounder would need to be associated with both ITN use and malaria infection by risk ratios of at least 3.05 to fully explain away the observed effect.
- Context: In these data, the measured confounder wealth (poorest versus richest) is associated with malaria prevalence by a risk ratio of approximately 7, but wealth is already conditioned upon. An unmeasured confounder would need an association comparable to the strongest measured confounders to nullify the findings.

### Intracluster Correlation

Estimated ICC from GLMM ranges from 50 to 58 percent across surveys, indicating that more than half of the variation in malaria risk occurs between clusters rather than within clusters. This justifies the use of multilevel models.

---

## Repository Structure

- Scripts/1_data_loading.R - Load raw KMIS data
- Scripts/1_data_loading.Rmd - R Markdown version
- Scripts/2_data_cleaning.R - Clean and construct variables
- Scripts/2_data_cleaning.Rmd - R Markdown version
- Scripts/3_descriptive_analysis.R - Summary statistics, tables, maps
- Scripts/3_descriptive_analysis.Rmd - R Markdown version
- Scripts/4_baseline_regression.R - Logistic, GLMM, PSM, IPTW, DR
- Scripts/4_baseline_regression.Rmd - R Markdown version
- Scripts/5_double_machine_learning.R - DML with cluster-aware cross-fitting
- Scripts/5_double_machine_learning.Rmd - R Markdown version
- Scripts/6_robustness_heterogeneity.R - Robustness checks and subgroup analysis
- Scripts/6_robustness_heterogeneity.Rmd - R Markdown version

---

## How to Reproduce

### Prerequisites

R version 4.2 or higher. Required packages can be installed in R using:

install.packages(c("dplyr", "tidyr", "ggplot2", "sf", "haven", "survey", "lme4", "MatchIt", "cobalt", "marginaleffects", "DoubleML", "mlr3", "mlr3learners", "ranger", "xgboost"))

### Data Access

1. Register at the DHS Program website (https://dhsprogram.com)
2. Download KMIS 2015 and KMIS 2020 datasets including PR, HR, KR, Geocov, and Shapefiles
3. Place raw data in a folder named data/raw/ maintaining the original folder structure

### Run Analysis

In R or RStudio, run scripts in order from 1 to 6:

source("Scripts/1_data_loading.R")
source("Scripts/2_data_cleaning.R")
source("Scripts/3_descriptive_analysis.R")
source("Scripts/4_baseline_regression.R")
source("Scripts/5_double_machine_learning.R")
source("Scripts/6_robustness_heterogeneity.R")

Alternatively, knit each .Rmd file to generate HTML output showing both code and results.

---

## Key Findings Summary

1. Confounding is substantial. Unadjusted models show no effect; fully adjusted models reveal protective associations.

2. Clustering matters. Intracluster correlation of 50 to 58 percent justifies multilevel models.

3. Conditional versus marginal effects differ substantially. GLMM, PSM, and IPTW show 44 to 61 percent odds reduction (cluster-specific effects). DML shows 2.8 percent odds reduction (population-average effect). Both are valid for different estimands.

4. Effect heterogeneity exists. Stronger effects in poorer households, urban areas, older children (36-59 months), and high-rainfall regions.

5. Moderate robustness to unmeasured confounding. E-value = 3.05.

6. ITN use declined from 58.0 percent in 2015 to 50.5 percent in 2020, suggesting distribution alone is insufficient and behavior change communication is needed.

---

## Limitations

- Conditional ignorability cannot be empirically verified; it must be justified by subject-matter knowledge.

- Cross-sectional data cannot separate personal protection from community-level mosquito suppression.

- Near-positivity violations: approximately 2.4 percent of treated units and 6 to 8 percent of control units had propensity scores outside the overlap region.

- SUTVA may be violated due to community effects where one child's ITN use affects malaria risk for other children.

- Limited generalizability beyond Kenya and the 2015 to 2020 period.

---

## Software and Packages

- DoubleML version 0.5.0: Double Machine Learning
- lme4 version 1.1-31: GLMM estimation
- MatchIt version 4.5.1: Propensity score matching
- marginaleffects version 0.8.0: Doubly robust estimation
- ranger version 0.14.1: Random forest
- xgboost version 1.7.3.1: Gradient boosting

---

## References

1. Austin, P.C. (2011). An introduction to propensity score methods for reducing the effects of confounding in observational studies. Multivariate Behavioral Research, 46(3), 399-424.

2. Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., & Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters.

3. Greenland, S., Pearl, J., & Robins, J.M. (1999). Confounding and collapsibility in causal inference. Statistical Science, 14(1), 29-46.

4. Killip, S., Mahfoud, Z., & Pearce, K. (2004). What is an intracluster correlation coefficient? Crucial concepts for primary care researchers. The Annals of Family Medicine, 2(3), 204-208.

5. Lengeler, C. (2004). Insecticide-treated bed nets and curtains for preventing malaria. Cochrane Database of Systematic Reviews, (2).

6. Pryce, J., Richardson, M., & Lengeler, C. (2018). Insecticide-treated nets for preventing malaria. Cochrane Database of Systematic Reviews, (11).

7. Rosenbaum, P.R., & Rubin, D.B. (1983). The central role of the propensity score in observational studies for causal effects. Biometrika, 70(1), 41-55.

8. VanderWeele, T.J., & Ding, P. (2017). Sensitivity analysis in observational research: introducing the E-value. Annals of Internal Medicine, 167(4), 268-274.

---

## Contact

Nicholas Mwanza  
African Institute for Mathematical Sciences (AIMS) Cameroon  
Email: nicholas.mwanza@aims-cameroon.org  
GitHub: nicholasmwanza

---

## License

MIT License.
