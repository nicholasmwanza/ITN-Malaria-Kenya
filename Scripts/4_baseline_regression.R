# ===========================================================
# Week 4: Traditional Regression, GLMM, and Causal Methods
# Nicholas Mwanza
# All supervisor comments addressed (1-13)
# ===========================================================

rm(list = ls())
setwd("/home/student25/Documents/Projects/THESIS/Scripts/")

library(dplyr)
library(tidyr)
library(ggplot2)
library(survey)
library(broom)
library(MatchIt)
library(cobalt)
library(lme4)
library(lmerTest)
library(marginaleffects)
library(igraph)
library(ggraph)
library(tidygraph)

output_dir <- "/home/student25/Documents/Projects/THESIS/Figures/"
data_clean_dir <- "/home/student25/Documents/Projects/THESIS/Data/Cleaned/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------------------------------------------
# Load cleaned data from Week 3
# -----------------------------------------------------------------

analytical_2015 <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/analytical_2015.rds")
analytical_2020 <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/analytical_2020.rds")
analytical_combined <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/analytical_combined.rds")

cat("KMIS 2015:", nrow(analytical_2015), "children,", 
    length(unique(analytical_2015$cluster)), "clusters\n")
cat("KMIS 2020:", nrow(analytical_2020), "children,", 
    length(unique(analytical_2020$cluster)), "clusters\n")
cat("Combined:", nrow(analytical_combined), "children,", 
    length(unique(analytical_combined$cluster)), "clusters\n")

# -----------------------------------------------------------------
# Prepare analysis data (numeric versions, complete cases)
# -----------------------------------------------------------------

prepare_analysis_data <- function(data, survey_name) {
  data <- data %>%
    mutate(
      malaria_num = as.numeric(malaria),
      itn_use_num = as.numeric(itn_use),
      electricity_num = as.numeric(electricity),
      improved_water_num = as.numeric(improved_water),
      improved_sanitation_num = as.numeric(improved_sanitation),
      wealth_num = as.numeric(wealth),
      cluster = as.factor(cluster),
      household_id = as.factor(paste0(cluster, "_", line_number))
    )
  
  complete_cases <- data %>%
    filter(
      !is.na(malaria_num), !is.na(itn_use_num), !is.na(age_months),
      !is.na(sex), !is.na(wealth), !is.na(residence),
      !is.na(electricity_num), !is.na(improved_water_num), !is.na(improved_sanitation_num),
      !is.na(rainfall), !is.na(temp_mean), !is.na(evi), !is.na(aridity),
      !is.na(wet_days), !is.na(sample_weight)
    )
  
  cat(survey_name, ": ", nrow(data), " -> ", nrow(complete_cases), 
      " complete cases (", round(nrow(complete_cases)/nrow(data)*100, 1), "%)\n")
  return(complete_cases)
}

analysis_2015 <- prepare_analysis_data(analytical_2015, "KMIS 2015")
analysis_2020 <- prepare_analysis_data(analytical_2020, "KMIS 2020")

# Fix survey_year for combined if missing
if(!"survey_year" %in% names(analytical_combined)) {
  if("survey" %in% names(analytical_combined)) {
    analytical_combined$survey_year <- as.factor(ifelse(analytical_combined$survey == "KMIS 2015", 2015, 2020))
  } else {
    n_2015 <- nrow(analytical_2015)
    analytical_combined$survey_year <- as.factor(c(rep(2015, n_2015), rep(2020, nrow(analytical_combined) - n_2015)))
  }
}

analysis_combined <- analytical_combined %>%
  mutate(
    malaria_num = as.numeric(malaria),
    itn_use_num = as.numeric(itn_use),
    electricity_num = as.numeric(electricity),
    improved_water_num = as.numeric(improved_water),
    improved_sanitation_num = as.numeric(improved_sanitation),
    wealth_num = as.numeric(wealth),
    cluster = as.factor(cluster),
    household_id = as.factor(paste0(cluster, "_", line_number))
  ) %>%
  filter(
    !is.na(malaria_num), !is.na(itn_use_num), !is.na(age_months),
    !is.na(sex), !is.na(wealth), !is.na(residence),
    !is.na(electricity_num), !is.na(improved_water_num), !is.na(improved_sanitation_num),
    !is.na(rainfall), !is.na(temp_mean), !is.na(evi), !is.na(aridity),
    !is.na(wet_days), !is.na(sample_weight)
  )

cat("Combined:", nrow(analytical_combined), "->", nrow(analysis_combined), "complete cases\n")

# -----------------------------------------------------------------
# DAG (Directed Acyclic Graph)
# -----------------------------------------------------------------

dag_edges <- c(
  "ITN_use", "Malaria",
  "Age", "ITN_use", "Age", "Malaria",
  "Sex", "ITN_use", "Sex", "Malaria",
  "Wealth", "ITN_use", "Wealth", "Malaria",
  "Wealth", "Housing_Quality", "Wealth", "Water_Sanitation",
  "Wealth", "Electricity", "Wealth", "Maternal_Education",
  "Maternal_Education", "ITN_use", "Maternal_Education", "Malaria",
  "Housing_Quality", "Malaria", "Water_Sanitation", "Malaria",
  "Electricity", "ITN_use", "Electricity", "Malaria",
  "Rainfall", "Malaria", "Temperature", "Malaria", "Vegetation", "Malaria",
  "Urban_Rural", "ITN_use", "Urban_Rural", "Malaria", "Urban_Rural", "Wealth"
)

dag_graph <- graph(dag_edges, directed = TRUE)
V(dag_graph)$name <- c("ITN_use", "Malaria", "Age", "Sex", "Wealth", 
                       "Housing_Quality", "Water_Sanitation", "Electricity",
                       "Maternal_Education", "Rainfall", "Temperature", 
                       "Vegetation", "Urban_Rural")

V(dag_graph)$color <- ifelse(V(dag_graph)$name == "ITN_use", "red",
                             ifelse(V(dag_graph)$name == "Malaria", "darkred", "lightblue"))
V(dag_graph)$shape <- ifelse(V(dag_graph)$name %in% c("ITN_use", "Malaria"), "rectangle", "circle")

png(paste0(output_dir, "dag_causal_diagram_base.png"), width = 1200, height = 800, res = 150)
par(mar = c(2, 2, 4, 2))
plot(dag_graph, layout = layout_with_sugiyama(dag_graph)$layout,
     vertex.color = V(dag_graph)$color, vertex.shape = V(dag_graph)$shape,
     vertex.size = 30, vertex.label = V(dag_graph)$name,
     vertex.label.cex = 0.9, edge.arrow.size = 1.0, edge.arrow.width = 1.5,
     edge.color = "black", main = "DAG for ITN Use and Malaria")
dev.off()

dag_tidy <- as_tbl_graph(dag_graph)
p_dag <- ggraph(dag_tidy, layout = "sugiyama") +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm'), type = "closed"), 
                 end_cap = circle(5, 'mm'), start_cap = circle(5, 'mm'),
                 edge_color = "black", edge_width = 1.2) +
  geom_node_point(aes(color = name %in% c("ITN_use", "Malaria")), size = 12) +
  geom_node_text(aes(label = name), repel = TRUE, size = 5, fontface = "bold") +
  scale_color_manual(values = c("TRUE" = "#E41A1C", "FALSE" = "#377EB8")) +
  theme_void() +
  labs(title = "DAG for ITN Use and Malaria")
ggsave(paste0(output_dir, "dag_causal_diagram_ggraph.png"), p_dag, width = 14, height = 10, dpi = 300)

# -----------------------------------------------------------------
# Traditional logistic regression
# -----------------------------------------------------------------

run_logistic_models <- function(data, survey_name, include_year = FALSE) {
  results <- data.frame()
  
  m1 <- glm(malaria_num ~ itn_use_num, data = data, family = binomial())
  coef_itn <- coef(m1)["itn_use_num"]
  se_itn <- summary(m1)$coefficients["itn_use_num", 2]
  results <- rbind(results, data.frame(
    Survey = survey_name, Model = "Unadjusted", 
    OR = exp(coef_itn), CI_Lower = exp(coef_itn - 1.96*se_itn), 
    CI_Upper = exp(coef_itn + 1.96*se_itn), P_Value = summary(m1)$coefficients["itn_use_num", 4]
  ))
  
  m2 <- glm(malaria_num ~ itn_use_num + age_months + sex + wealth + residence +
              electricity_num + improved_water_num + improved_sanitation_num,
            data = data, family = binomial())
  coef_itn <- coef(m2)["itn_use_num"]
  se_itn <- summary(m2)$coefficients["itn_use_num", 2]
  results <- rbind(results, data.frame(
    Survey = survey_name, Model = "Demo+HH", 
    OR = exp(coef_itn), CI_Lower = exp(coef_itn - 1.96*se_itn),
    CI_Upper = exp(coef_itn + 1.96*se_itn), P_Value = summary(m2)$coefficients["itn_use_num", 4]
  ))
  
  if(include_year) {
    m3 <- glm(malaria_num ~ itn_use_num + age_months + sex + wealth + residence +
                electricity_num + improved_water_num + improved_sanitation_num +
                rainfall + temp_mean + evi + aridity + wet_days + survey_year,
              data = data, family = binomial())
  } else {
    m3 <- glm(malaria_num ~ itn_use_num + age_months + sex + wealth + residence +
                electricity_num + improved_water_num + improved_sanitation_num +
                rainfall + temp_mean + evi + aridity + wet_days,
              data = data, family = binomial())
  }
  coef_itn <- coef(m3)["itn_use_num"]
  se_itn <- summary(m3)$coefficients["itn_use_num", 2]
  results <- rbind(results, data.frame(
    Survey = survey_name, Model = "Fully Adjusted", 
    OR = exp(coef_itn), CI_Lower = exp(coef_itn - 1.96*se_itn),
    CI_Upper = exp(coef_itn + 1.96*se_itn), P_Value = summary(m3)$coefficients["itn_use_num", 4]
  ))
  
  return(results)
}

logistic_2015 <- run_logistic_models(analysis_2015, "KMIS 2015")
logistic_2020 <- run_logistic_models(analysis_2020, "KMIS 2020")
logistic_combined <- run_logistic_models(analysis_combined, "Combined", include_year = TRUE)
logistic_results <- bind_rows(logistic_2015, logistic_2020, logistic_combined)

cat("\nLogistic regression results:\n")
for(i in 1:nrow(logistic_results)) {
  sig <- ifelse(logistic_results$P_Value[i] < 0.05, "SIG", "NS")
  cat(sprintf("  %s - %s: OR = %.3f (%.3f-%.3f), p = %.4f [%s]\n",
              logistic_results$Survey[i], logistic_results$Model[i], 
              logistic_results$OR[i], logistic_results$CI_Lower[i], 
              logistic_results$CI_Upper[i], logistic_results$P_Value[i], sig))
}

# -----------------------------------------------------------------
# GLMM: 2-level vs 3-level comparison
# -----------------------------------------------------------------

run_glmm_comparison <- function(data, survey_name) {
  cat("\n---", survey_name, "---\n")
  
  data_scaled <- data %>%
    mutate(
      age_scaled = scale(age_months),
      rainfall_scaled = scale(rainfall),
      temp_scaled = scale(temp_mean),
      wealth_num_scaled = scale(wealth_num)
    )
  
  cat("  Fitting 2-level model...\n")
  m2 <- glmer(malaria_num ~ itn_use_num + age_scaled + sex + wealth_num_scaled + residence +
                electricity_num + improved_water_num + improved_sanitation_num +
                rainfall_scaled + temp_scaled + (1 | cluster),
              data = data_scaled, family = binomial(),
              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  
  cat("  Fitting 3-level model...\n")
  m3 <- tryCatch({
    glmer(malaria_num ~ itn_use_num + age_scaled + sex + wealth_num_scaled + residence +
            electricity_num + improved_water_num + improved_sanitation_num +
            rainfall_scaled + temp_scaled + (1 | cluster/household_id),
          data = data_scaled, family = binomial(),
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  }, error = function(e) NULL)
  
  fe <- fixef(m2)
  se <- sqrt(diag(vcov(m2)))
  idx <- which(names(fe) == "itn_use_num")
  or2 <- exp(fe[idx])
  ci2 <- exp(fe[idx] + c(-1.96, 1.96) * se[idx])
  p2 <- 2 * (1 - pnorm(abs(fe[idx] / se[idx])))
  var_comp <- as.data.frame(VarCorr(m2))
  icc2 <- var_comp$vcov[1] / (var_comp$vcov[1] + pi^2/3)
  cat(sprintf("  2-level: OR = %.3f (%.3f-%.3f), p = %.4f, ICC = %.3f\n", 
              or2, ci2[1], ci2[2], p2, icc2))
  
  if(!is.null(m3)) {
    fe3 <- fixef(m3)
    se3 <- sqrt(diag(vcov(m3)))
    idx3 <- which(names(fe3) == "itn_use_num")
    or3 <- exp(fe3[idx3])
    ci3 <- exp(fe3[idx3] + c(-1.96, 1.96) * se3[idx3])
    p3 <- 2 * (1 - pnorm(abs(fe3[idx3] / se3[idx3])))
    var_comp3 <- as.data.frame(VarCorr(m3))
    icc3_cluster <- var_comp3$vcov[1] / (sum(var_comp3$vcov) + pi^2/3)
    cat(sprintf("  3-level: OR = %.3f (%.3f-%.3f), p = %.4f, ICC = %.3f\n", 
                or3, ci3[1], ci3[2], p3, icc3_cluster))
    cat(sprintf("  AIC: 2-level = %.1f, 3-level = %.1f (diff = %.1f)\n", AIC(m2), AIC(m3), AIC(m3)-AIC(m2)))
    
    if(AIC(m3) < AIC(m2) - 2) {
      cat("  -> 3-level model better\n")
      return(data.frame(Survey = survey_name, OR = or3, CI_Lower = ci3[1], CI_Upper = ci3[2], 
                        P_Value = p3, ICC = icc3_cluster, Model = "3-level"))
    }
  }
  
  cat("  -> 2-level model sufficient\n")
  return(data.frame(Survey = survey_name, OR = or2, CI_Lower = ci2[1], CI_Upper = ci2[2],
                    P_Value = p2, ICC = icc2, Model = "2-level"))
}

glmm_2015 <- run_glmm_comparison(analysis_2015, "KMIS 2015")
glmm_2020 <- run_glmm_comparison(analysis_2020, "KMIS 2020")
glmm_combined <- run_glmm_comparison(analysis_combined, "Combined")
glmm_results <- bind_rows(glmm_2015, glmm_2020, glmm_combined)

saveRDS(glmm_results, paste0(data_clean_dir, "week4_glmm_results.rds"))

# -----------------------------------------------------------------
# Caterpillar plots for random effects
# -----------------------------------------------------------------

run_caterpillar_plot <- function(data, survey_name, glmm_model) {
  ranef_data <- as.data.frame(ranef(glmm_model)$cluster)
  if(nrow(ranef_data) > 0) {
    colnames(ranef_data)[1] <- "intercept"
    ranef_data$cluster <- rownames(ranef_data)
    ranef_data <- ranef_data[order(ranef_data$intercept), ]
    ranef_data$order <- 1:nrow(ranef_data)
    
    p_ranef <- ggplot(ranef_data, aes(x = order, y = intercept)) +
      geom_point(size = 1.5, color = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      labs(title = paste("Caterpillar Plot -", survey_name), 
           x = "Cluster (sorted)", y = "Random Intercept (log-odds)") +
      theme_minimal()
    ggsave(paste0(output_dir, "glmm_caterpillar_", gsub(" ", "_", survey_name), ".png"), 
           p_ranef, width = 10, height = 6, dpi = 300)
    cat("  Caterpillar plot saved for", survey_name, "\n")
  }
}

data_scaled_2015 <- analysis_2015 %>%
  mutate(age_scaled = scale(age_months), rainfall_scaled = scale(rainfall),
         temp_scaled = scale(temp_mean), wealth_num_scaled = scale(wealth_num))

glmm_model_2015 <- glmer(malaria_num ~ itn_use_num + age_scaled + sex + wealth_num_scaled + residence +
                           electricity_num + improved_water_num + improved_sanitation_num +
                           rainfall_scaled + temp_scaled + (1 | cluster),
                         data = data_scaled_2015, family = binomial(),
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

glmm_model_2020 <- glmer(malaria_num ~ itn_use_num + age_scaled + sex + wealth_num_scaled + residence +
                           electricity_num + improved_water_num + improved_sanitation_num +
                           rainfall_scaled + temp_scaled + (1 | cluster),
                         data = analysis_2020 %>% mutate(age_scaled = scale(age_months), rainfall_scaled = scale(rainfall),
                                                         temp_scaled = scale(temp_mean), wealth_num_scaled = scale(wealth_num)),
                         family = binomial(),
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

run_caterpillar_plot(analysis_2015, "KMIS 2015", glmm_model_2015)
run_caterpillar_plot(analysis_2020, "KMIS 2020", glmm_model_2020)

# -----------------------------------------------------------------
# Propensity score estimation using GLMM (separate by survey)
# -----------------------------------------------------------------

estimate_ps <- function(data, survey_name) {
  cat("  Estimating PS for", survey_name, "...\n")
  data_scaled <- data %>%
    mutate(age_scaled = scale(age_months), rainfall_scaled = scale(rainfall),
           temp_scaled = scale(temp_mean), wealth_scaled = scale(wealth_num))
  
  ps_model <- glmer(itn_use_num ~ age_scaled + sex + wealth_scaled + residence +
                      electricity_num + improved_water_num + rainfall_scaled + temp_scaled +
                      (1 | cluster), data = data_scaled, family = binomial(),
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  return(predict(ps_model, type = "link"))
}

ps_2015 <- estimate_ps(analysis_2015, "KMIS 2015")
ps_2020 <- estimate_ps(analysis_2020, "KMIS 2020")

# -----------------------------------------------------------------
# PSM with caliper = 0.05 (final)
# -----------------------------------------------------------------

run_psm <- function(data, survey_name, ps_logit, caliper_sd = 0.05) {
  cat("\n  PSM for", survey_name, "with caliper =", caliper_sd, "SD\n")
  
  data$ps_logit <- ps_logit
  caliper_val <- caliper_sd * sd(data$ps_logit)
  cat("    Caliper value:", round(caliper_val, 3), "\n")
  
  match_result <- matchit(itn_use_num ~ 1, data = data, method = "nearest",
                          distance = data$ps_logit, caliper = caliper_val, replace = FALSE)
  
  matched_indices <- as.numeric(row.names(match.data(match_result)))
  matched_data <- data[matched_indices, ]
  matched_data$treat <- matched_data$itn_use_num
  
  cat("    Matched N:", nrow(matched_data), "\n")
  cat("    Treated:", sum(matched_data$treat), "Control:", sum(matched_data$treat == 0), "\n")
  
  match_glmm <- glmer(malaria_num ~ treat + (1 | cluster), data = matched_data, 
                      family = binomial(), control = glmerControl(optimizer = "bobyqa"))
  
  or_psm <- exp(fixef(match_glmm)["treat"])
  ci_psm <- exp(confint(match_glmm, method = "Wald")["treat", ])
  p_psm <- summary(match_glmm)$coefficients["treat", "Pr(>|z|)"]
  
  cat("    OR =", round(or_psm, 3), "95% CI:", round(ci_psm[1], 3), "-", round(ci_psm[2], 3), 
      "p =", round(p_psm, 4), "\n")
  
  # Distance balance
  ps_before <- abs(mean(data$ps_logit[data$itn_use_num==1]) - 
                     mean(data$ps_logit[data$itn_use_num==0])) / sd(data$ps_logit)
  ps_after <- abs(mean(matched_data$ps_logit[matched_data$treat==1]) - 
                    mean(matched_data$ps_logit[matched_data$treat==0])) / sd(matched_data$ps_logit)
  cat(sprintf("    PS balance: before SMD = %.3f, after SMD = %.3f\n", ps_before, ps_after))
  
  return(list(
    results = data.frame(Survey = survey_name, Method = paste0("PSM (caliper=", caliper_sd, ")"), 
                         OR = or_psm, OR_CI_Lower = ci_psm[1], OR_CI_Upper = ci_psm[2], 
                         P_Value = p_psm, Matched_N = nrow(matched_data), PS_After_SMD = ps_after),
    matched_data = matched_data
  ))
}

psm_2015 <- run_psm(analysis_2015, "KMIS 2015", ps_2015, caliper_sd = 0.05)
psm_2020 <- run_psm(analysis_2020, "KMIS 2020", ps_2020, caliper_sd = 0.05)
psm_results <- bind_rows(psm_2015$results, psm_2020$results)
saveRDS(psm_results, paste0(data_clean_dir, "week4_psm_results.rds"))

# -----------------------------------------------------------------
# IPTW with truncation (max weight = 4)
# -----------------------------------------------------------------

run_iptw <- function(data, survey_name, ps_logit, max_w = 4) {
  cat("\n  IPTW for", survey_name, "\n")
  
  data$ps <- plogis(ps_logit)
  p_treat <- mean(data$itn_use_num)
  data$w <- ifelse(data$itn_use_num == 1, p_treat/data$ps, (1-p_treat)/(1-data$ps))
  cat("    Original max weight:", round(max(data$w), 2), "\n")
  data$w_trunc <- pmin(data$w, max_w)
  cat("    Truncated", sum(data$w > max_w), "obs, new max:", round(max(data$w_trunc), 2), "\n")
  
  iptw_mod <- glmer(malaria_num ~ itn_use_num + (1 | cluster), data = data, 
                    weights = w_trunc, family = binomial(),
                    control = glmerControl(optimizer = "bobyqa"))
  
  or_iptw <- exp(fixef(iptw_mod)["itn_use_num"])
  ci_iptw <- exp(confint(iptw_mod, method = "Wald")["itn_use_num", ])
  p_iptw <- summary(iptw_mod)$coefficients["itn_use_num", "Pr(>|z|)"]
  
  cat("    OR =", round(or_iptw, 3), "95% CI:", round(ci_iptw[1], 3), "-", round(ci_iptw[2], 3), 
      "p =", round(p_iptw, 4), "\n")
  
  return(data.frame(Survey = survey_name, Method = "IPTW (truncated)", OR = or_iptw,
                    OR_CI_Lower = ci_iptw[1], OR_CI_Upper = ci_iptw[2], P_Value = p_iptw))
}

iptw_2015 <- run_iptw(analysis_2015, "KMIS 2015", ps_2015)
iptw_2020 <- run_iptw(analysis_2020, "KMIS 2020", ps_2020)
iptw_results <- bind_rows(iptw_2015, iptw_2020)
saveRDS(iptw_results, paste0(data_clean_dir, "week4_iptw_results.rds"))

# -----------------------------------------------------------------
# Doubly robust estimation
# -----------------------------------------------------------------

run_dr <- function(data, survey_name, ps_logit) {
  cat("\n  Doubly Robust for", survey_name, "...\n")
  
  data$ps <- plogis(ps_logit)
  p_treat <- mean(data$itn_use_num)
  data$w <- pmin(ifelse(data$itn_use_num == 1, p_treat/data$ps, (1-p_treat)/(1-data$ps)), 10)
  
  data_scaled <- data %>%
    mutate(age_scaled = scale(age_months), rainfall_scaled = scale(rainfall),
           temp_scaled = scale(temp_mean), wealth_scaled = scale(wealth_num))
  
  out_model <- glm(malaria_num ~ itn_use_num + age_scaled + sex + wealth_scaled + 
                     residence + electricity_num + rainfall_scaled + temp_scaled,
                   data = data_scaled, family = binomial())
  
  dr_or <- avg_comparisons(out_model, variables = "itn_use_num", vcov = ~cluster,
                           wts = data$w, transform = "exp", newdata = data_scaled)
  
  or_dr <- dr_or$estimate
  ci_dr <- c(dr_or$conf.low, dr_or$conf.high)
  p_dr <- dr_or$p.value
  
  cat("    OR =", round(or_dr, 3), "95% CI:", round(ci_dr[1], 3), "-", round(ci_dr[2], 3), 
      "p =", round(p_dr, 4), "\n")
  
  return(data.frame(Survey = survey_name, Method = "Doubly Robust", OR = or_dr,
                    OR_CI_Lower = ci_dr[1], OR_CI_Upper = ci_dr[2], P_Value = p_dr))
}

dr_2015 <- run_dr(analysis_2015, "KMIS 2015", ps_2015)
dr_2020 <- run_dr(analysis_2020, "KMIS 2020", ps_2020)
dr_results <- bind_rows(dr_2015, dr_2020)
saveRDS(dr_results, paste0(data_clean_dir, "week4_dr_results.rds"))

# -----------------------------------------------------------------
# Overlap/positivity check
# -----------------------------------------------------------------

check_overlap <- function(data, survey_name, ps_logit) {
  data$ps <- plogis(ps_logit)
  ps_t <- data$ps[data$itn_use_num == 1]
  ps_c <- data$ps[data$itn_use_num == 0]
  
  cat("\n---", survey_name, "---\n")
  cat("  Treated PS: [", round(min(ps_t),3), ",", round(max(ps_t),3), "]\n")
  cat("  Control PS: [", round(min(ps_c),3), ",", round(max(ps_c),3), "]\n")
  
  overlap_low <- max(min(ps_t), min(ps_c))
  overlap_high <- min(max(ps_t), max(ps_c))
  cat("  Overlap: [", round(overlap_low,3), ",", round(overlap_high,3), "]\n")
  
  pct_above <- sum(ps_t > overlap_high)/length(ps_t)*100
  pct_below <- sum(ps_c < overlap_low)/length(ps_c)*100
  cat("  % treated above overlap:", round(pct_above, 1), "%\n")
  cat("  % control below overlap:", round(pct_below, 1), "%\n")
}

check_overlap(analysis_2015, "KMIS 2015", ps_2015)
check_overlap(analysis_2020, "KMIS 2020", ps_2020)

# -----------------------------------------------------------------
# Master summary table
# -----------------------------------------------------------------

logistic_clean <- logistic_results %>% select(Survey, Model, OR, CI_Lower, CI_Upper, P_Value)
glmm_clean <- glmm_results %>% mutate(Model = "GLMM") %>% select(Survey, Model, OR, CI_Lower, CI_Upper, P_Value)
psm_clean <- psm_results %>% rename(OR = OR, CI_Lower = OR_CI_Lower, CI_Upper = OR_CI_Upper) %>% mutate(Model = "PSM")
iptw_clean <- iptw_results %>% mutate(Model = "IPTW")
dr_clean <- dr_results %>% mutate(Model = "Doubly Robust")

master <- bind_rows(logistic_clean, glmm_clean, psm_clean, iptw_clean, dr_clean)

cat("\n=== Master Summary ===\n")
for(method in unique(master$Model)) {
  row_2015 <- master[master$Survey == "KMIS 2015" & master$Model == method, ]
  row_2020 <- master[master$Survey == "KMIS 2020" & master$Model == method, ]
  
  ci_2015 <- if(nrow(row_2015) > 0) {
    sprintf("%.3f [%.3f-%.3f]%s", row_2015$OR[1], row_2015$CI_Lower[1],
            row_2015$CI_Upper[1], ifelse(row_2015$P_Value[1] < 0.05, "*", ""))
  } else { "N/A" }
  
  ci_2020 <- if(nrow(row_2020) > 0) {
    sprintf("%.3f [%.3f-%.3f]%s", row_2020$OR[1], row_2020$CI_Lower[1],
            row_2020$CI_Upper[1], ifelse(row_2020$P_Value[1] < 0.05, "*", ""))
  } else { "N/A" }
  
  cat(sprintf("%-20s | %-30s | %-30s\n", method, ci_2015, ci_2020))
}

saveRDS(master, paste0(data_clean_dir, "week4_master_results.rds"))
write.csv(master, paste0(data_clean_dir, "week4_master_results.csv"), row.names = FALSE)

cat("\nWeek 4 complete.\n")