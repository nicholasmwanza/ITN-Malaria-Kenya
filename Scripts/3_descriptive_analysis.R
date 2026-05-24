# ===========================================================
# Week 3: Descriptive Analysis
# Nicholas Mwanza
# April 2026
# ===========================================================
# Uses cleaned data from Week 2
# Outcome: malaria (from hml32)
# Treatment: itn_use (from hml12)
# ===========================================================

rm(list = ls())
setwd("/home/student25/Documents/Projects/THESIS/Scripts/")

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

# -----------------------------------------------------------------
# Load cleaned data from Week 2
# -----------------------------------------------------------------

analytical_2015 <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/analytical_2015.rds")
analytical_2020 <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/analytical_2020.rds")
analytical_combined <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/analytical_combined.rds")
cluster_level_2015 <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/cluster_level_2015.rds")
cluster_level_2020 <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/cluster_level_2020.rds")
cluster_level_combined <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/cluster_level_combined.rds")
shape_2015 <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/shape_2015.rds")
shape_2020 <- readRDS("/home/student25/Documents/Projects/THESIS/Data/Cleaned/shape_2020.rds")

cat("Loaded: 2015 N =", nrow(analytical_2015), 
    "2020 N =", nrow(analytical_2020),
    "combined N =", nrow(analytical_combined), "\n")

# -----------------------------------------------------------------
# Weighted mean function (survey weights)
# -----------------------------------------------------------------

weighted_mean <- function(x, w) {
  if(all(is.na(x))) return(NA)
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

# -----------------------------------------------------------------
# Descriptive statistics table
# -----------------------------------------------------------------

desc_table <- data.frame(
  Survey = c("KMIS 2015", "KMIS 2020", "Combined"),
  N_Children = c(nrow(analytical_2015), nrow(analytical_2020), nrow(analytical_combined)),
  N_Clusters = c(length(unique(analytical_2015$cluster)), 
                 length(unique(analytical_2020$cluster)),
                 length(unique(analytical_combined$cluster))),
  Malaria_Prevalence = c(
    round(weighted_mean(analytical_2015$malaria, analytical_2015$sample_weight) * 100, 1),
    round(weighted_mean(analytical_2020$malaria, analytical_2020$sample_weight) * 100, 1),
    round(weighted_mean(analytical_combined$malaria, analytical_combined$sample_weight) * 100, 1)
  ),
  ITN_Use_Rate = c(
    round(weighted_mean(analytical_2015$itn_use, analytical_2015$sample_weight) * 100, 1),
    round(weighted_mean(analytical_2020$itn_use, analytical_2020$sample_weight) * 100, 1),
    round(weighted_mean(analytical_combined$itn_use, analytical_combined$sample_weight) * 100, 1)
  ),
  Urban_Percent = c(
    round(weighted_mean(analytical_2015$residence == "Urban", analytical_2015$sample_weight) * 100, 1),
    round(weighted_mean(analytical_2020$residence == "Urban", analytical_2020$sample_weight) * 100, 1),
    round(weighted_mean(analytical_combined$residence == "Urban", analytical_combined$sample_weight) * 100, 1)
  )
)

print(desc_table)
write.csv(desc_table, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/descriptive_table.csv", row.names = FALSE)

# -----------------------------------------------------------------
# Malaria prevalence by wealth quintile
# -----------------------------------------------------------------

wealth_malaria <- function(data, survey_name) {
  data %>%
    group_by(wealth) %>%
    summarise(
      malaria_prev = weighted_mean(malaria, sample_weight) * 100,
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(survey = survey_name)
}

wealth_2015 <- wealth_malaria(analytical_2015, "KMIS 2015")
wealth_2020 <- wealth_malaria(analytical_2020, "KMIS 2020")
wealth_combined <- wealth_malaria(analytical_combined, "Combined")
wealth_all <- bind_rows(wealth_2015, wealth_2020, wealth_combined)

p_wealth <- ggplot(wealth_all, aes(x = wealth, y = malaria_prev, 
                                   color = survey, group = survey)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Malaria Prevalence by Wealth Quintile",
       x = "Wealth Quintile", y = "Malaria Prevalence (%)") +
  theme_minimal()
print(p_wealth)
ggsave("/home/student25/Documents/Projects/THESIS/Figures/malaria_by_wealth.png", 
       p_wealth, width = 8, height = 5)

# -----------------------------------------------------------------
# ITN use by wealth quintile
# -----------------------------------------------------------------

wealth_itn <- function(data, survey_name) {
  data %>%
    group_by(wealth) %>%
    summarise(
      itn_use_rate = weighted_mean(itn_use, sample_weight) * 100,
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(survey = survey_name)
}

itn_2015 <- wealth_itn(analytical_2015, "KMIS 2015")
itn_2020 <- wealth_itn(analytical_2020, "KMIS 2020")
itn_combined <- wealth_itn(analytical_combined, "Combined")
itn_all <- bind_rows(itn_2015, itn_2020, itn_combined)

p_itn <- ggplot(itn_all, aes(x = wealth, y = itn_use_rate, 
                             color = survey, group = survey)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "ITN Use by Wealth Quintile",
       x = "Wealth Quintile", y = "ITN Use (%)") +
  theme_minimal()
print(p_itn)
ggsave("/home/student25/Documents/Projects/THESIS/Figures/itn_by_wealth.png", 
       p_itn, width = 8, height = 5)

# -----------------------------------------------------------------
# Malaria by urban/rural
# -----------------------------------------------------------------

residence_malaria <- function(data, survey_name) {
  data %>%
    group_by(residence) %>%
    summarise(
      malaria_prev = weighted_mean(malaria, sample_weight) * 100,
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(survey = survey_name)
}

res_2015 <- residence_malaria(analytical_2015, "KMIS 2015")
res_2020 <- residence_malaria(analytical_2020, "KMIS 2020")
res_combined <- residence_malaria(analytical_combined, "Combined")
res_all <- bind_rows(res_2015, res_2020, res_combined)

p_res <- ggplot(res_all, aes(x = residence, y = malaria_prev, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Malaria Prevalence by Residence",
       x = "Residence", y = "Malaria Prevalence (%)") +
  theme_minimal()
print(p_res)
ggsave("/home/student25/Documents/Projects/THESIS/Figures/malaria_by_residence.png", 
       p_res, width = 6, height = 5)

# -----------------------------------------------------------------
# Malaria by child age
# -----------------------------------------------------------------

age_malaria <- function(data, survey_name) {
  data %>%
    mutate(
      age_group = cut(age_months, 
                      breaks = c(0, 12, 24, 36, 48, 60),
                      labels = c("0-11", "12-23", "24-35", "36-47", "48-59"),
                      include.lowest = TRUE)
    ) %>%
    group_by(age_group) %>%
    summarise(
      malaria_prev = weighted_mean(malaria, sample_weight) * 100,
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(survey = survey_name)
}

age_2015 <- age_malaria(analytical_2015, "KMIS 2015")
age_2020 <- age_malaria(analytical_2020, "KMIS 2020")
age_combined <- age_malaria(analytical_combined, "Combined")
age_all <- bind_rows(age_2015, age_2020, age_combined)

p_age <- ggplot(age_all, aes(x = age_group, y = malaria_prev, 
                             color = survey, group = survey)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Malaria Prevalence by Child Age",
       x = "Age (months)", y = "Malaria Prevalence (%)") +
  theme_minimal()
print(p_age)
ggsave("/home/student25/Documents/Projects/THESIS/Figures/malaria_by_age.png", 
       p_age, width = 8, height = 5)

# -----------------------------------------------------------------
# ITN use by child age
# -----------------------------------------------------------------

age_itn <- function(data, survey_name) {
  data %>%
    mutate(
      age_group = cut(age_months, 
                      breaks = c(0, 12, 24, 36, 48, 60),
                      labels = c("0-11", "12-23", "24-35", "36-47", "48-59"),
                      include.lowest = TRUE)
    ) %>%
    group_by(age_group) %>%
    summarise(
      itn_use_rate = weighted_mean(itn_use, sample_weight) * 100,
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(survey = survey_name)
}

itn_age_2015 <- age_itn(analytical_2015, "KMIS 2015")
itn_age_2020 <- age_itn(analytical_2020, "KMIS 2020")
itn_age_combined <- age_itn(analytical_combined, "Combined")
itn_age_all <- bind_rows(itn_age_2015, itn_age_2020, itn_age_combined)

p_age_itn <- ggplot(itn_age_all, aes(x = age_group, y = itn_use_rate, 
                                     color = survey, group = survey)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "ITN Use by Child Age",
       x = "Age (months)", y = "ITN Use (%)") +
  theme_minimal()
print(p_age_itn)
ggsave("/home/student25/Documents/Projects/THESIS/Figures/itn_by_age.png", 
       p_age_itn, width = 8, height = 5)

# -----------------------------------------------------------------
# Malaria prevalence maps by cluster
# -----------------------------------------------------------------

shape_2015 <- st_transform(shape_2015, crs = 4326)
shape_2020 <- st_transform(shape_2020, crs = 4326)

create_malaria_map <- function(shape, cluster_data, survey_name, year) {
  shape_merged <- shape %>%
    mutate(DHSCLUST = as.numeric(DHSCLUST)) %>%
    left_join(cluster_data, by = c("DHSCLUST" = "cluster"))
  
  p <- ggplot() +
    geom_sf(data = shape_merged, aes(fill = malaria_prevalence), 
            color = "gray70", size = 0.2) +
    scale_fill_gradient(low = "#ffffcc", high = "#cc4c02", 
                        name = "Malaria\nPrevalence (%)",
                        na.value = "gray95") +
    labs(title = paste("Malaria Prevalence -", survey_name, year)) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  return(p)
}

map_2015 <- create_malaria_map(shape_2015, cluster_level_2015, "KMIS", "2015")
map_2020 <- create_malaria_map(shape_2020, cluster_level_2020, "KMIS", "2020")

ggsave("/home/student25/Documents/Projects/THESIS/Figures/malaria_map_2015.png", 
       map_2015, width = 10, height = 8)
ggsave("/home/student25/Documents/Projects/THESIS/Figures/malaria_map_2020.png", 
       map_2020, width = 10, height = 8)

# -----------------------------------------------------------------
# ITN use maps
# -----------------------------------------------------------------

create_itn_map <- function(shape, cluster_data, survey_name, year) {
  shape_merged <- shape %>%
    mutate(DHSCLUST = as.numeric(DHSCLUST)) %>%
    left_join(cluster_data, by = c("DHSCLUST" = "cluster"))
  
  p <- ggplot() +
    geom_sf(data = shape_merged, aes(fill = itn_use_rate), 
            color = "gray70", size = 0.2) +
    scale_fill_gradient(low = "#ffffcc", high = "#238b45", 
                        name = "ITN Use (%)", na.value = "gray95") +
    labs(title = paste("ITN Use Rate -", survey_name, year)) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  return(p)
}

map_itn_2015 <- create_itn_map(shape_2015, cluster_level_2015, "KMIS", "2015")
map_itn_2020 <- create_itn_map(shape_2020, cluster_level_2020, "KMIS", "2020")

ggsave("/home/student25/Documents/Projects/THESIS/Figures/itn_map_2015.png", 
       map_itn_2015, width = 10, height = 8)
ggsave("/home/student25/Documents/Projects/THESIS/Figures/itn_map_2020.png", 
       map_itn_2020, width = 10, height = 8)

# -----------------------------------------------------------------
# Environmental summary
# -----------------------------------------------------------------

env_summary <- analytical_combined %>%
  summarise(
    rainfall_mean = mean(rainfall, na.rm = TRUE),
    rainfall_sd = sd(rainfall, na.rm = TRUE),
    temp_mean = mean(temp_mean, na.rm = TRUE),
    temp_sd = sd(temp_mean, na.rm = TRUE),
    evi_mean = mean(evi, na.rm = TRUE),
    evi_sd = sd(evi, na.rm = TRUE),
    aridity_mean = mean(aridity, na.rm = TRUE),
    aridity_sd = sd(aridity, na.rm = TRUE),
    wet_days_mean = mean(wet_days, na.rm = TRUE),
    wet_days_sd = sd(wet_days, na.rm = TRUE)
  )

print(env_summary)

# -----------------------------------------------------------------
# Save all results
# -----------------------------------------------------------------

saveRDS(desc_table, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/descriptive_summary.rds")
saveRDS(wealth_all, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/wealth_analysis.rds")
saveRDS(age_all, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/age_analysis.rds")
saveRDS(res_all, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/residence_analysis.rds")
saveRDS(env_summary, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/environmental_summary.rds")

write.csv(desc_table, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/descriptive_summary.csv", row.names = FALSE)
write.csv(wealth_all, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/wealth_analysis.csv", row.names = FALSE)
write.csv(age_all, "/home/student25/Documents/Projects/THESIS/Data/Cleaned/age_analysis.csv", row.names = FALSE)

cat("\nWeek 3 complete.\n")