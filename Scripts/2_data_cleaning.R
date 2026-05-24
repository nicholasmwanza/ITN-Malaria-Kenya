# ===========================================================
# Week 2: Data Cleaning and Variable Construction
# Nicholas Mwanza
# April 2026
# ===========================================================
# Uses KMIS 2015 and 2020 only
# Outcome: hml32 (malaria blood smear)
# Treatment: hml12 (ITN use last night)
# Includes shapefiles for GPS coordinates
# ===========================================================

rm(list = ls())
setwd("/home/student25/Documents/Projects/THESIS/Scripts/")

library(haven)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)

data_dir <- "/home/student25/Documents/Projects/THESIS/Data/Raw/"
clean_dir <- "/home/student25/Documents/Projects/THESIS/Data/Cleaned/"

if(!dir.exists(clean_dir)) dir.create(clean_dir, recursive = TRUE)

# -----------------------------------------------------------------
# Load KMIS 2015
# -----------------------------------------------------------------

cat("\nLoading KMIS 2015...\n")

kmis2015_pr <- read_dta(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEPR7ADT/KEPR7AFL.DTA"))
kmis2015_hr <- read_dta(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEHR7ADT/KEHR7AFL.DTA"))
kmis2015_geocov <- read.csv(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEGC7BFL/KEGC7BFL.csv"))
kmis2015_shape <- st_read(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEGE7AFL/KEGE7AFL.shp"))

cat("KMIS 2015: PR", nrow(kmis2015_pr), "HR", nrow(kmis2015_hr), 
    "Geocov", nrow(kmis2015_geocov), "Shape", nrow(kmis2015_shape), "\n")

# -----------------------------------------------------------------
# Load KMIS 2020
# -----------------------------------------------------------------

cat("\nLoading KMIS 2020...\n")

kmis2020_pr <- read_dta(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEPR81DT/KEPR81FL.DTA"))
kmis2020_hr <- read_dta(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEHR81DT/KEHR81FL.DTA"))
kmis2020_geocov <- read.csv(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEGC81FL/KEGC81FL/KEGC81FL.csv"))
kmis2020_shape <- st_read(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEGE81FL/KEGE81FL.shp"))

cat("KMIS 2020: PR", nrow(kmis2020_pr), "HR", nrow(kmis2020_hr), 
    "Geocov", nrow(kmis2020_geocov), "Shape", nrow(kmis2020_shape), "\n")

# -----------------------------------------------------------------
# Clean geocov (replace -9999 and negatives with NA)
# -----------------------------------------------------------------

clean_geocov <- function(data, survey_year) {
  
  rain_cols <- grep("Rainfall", names(data), value = TRUE)
  for(col in rain_cols) {
    if(is.numeric(data[[col]])) {
      data[[col]][data[[col]] < 0] <- NA
    }
  }
  
  temp_cols <- grep("Temperature|Temp", names(data), value = TRUE)
  for(col in temp_cols) {
    if(is.numeric(data[[col]])) {
      data[[col]][data[[col]] < -50 | data[[col]] > 60] <- NA
    }
  }
  
  evi_cols <- grep("Vegetation_Index", names(data), value = TRUE)
  for(col in evi_cols) {
    if(is.numeric(data[[col]])) {
      data[[col]][data[[col]] < 0] <- NA
    }
  }
  
  arid_cols <- grep("Aridity", names(data), value = TRUE)
  for(col in arid_cols) {
    if(is.numeric(data[[col]])) {
      data[[col]][data[[col]] < 0] <- NA
    }
  }
  
  wet_cols <- grep("Wet_Days", names(data), value = TRUE)
  for(col in wet_cols) {
    if(is.numeric(data[[col]])) {
      data[[col]][data[[col]] < 0] <- NA
    }
  }
  
  pop_cols <- grep("Population", names(data), value = TRUE)
  for(col in pop_cols) {
    if(is.numeric(data[[col]])) {
      data[[col]][data[[col]] <= 0] <- NA
    }
  }
  
  cat("Cleaned", survey_year, "geocov\n")
  return(data)
}

kmis2015_geocov_clean <- clean_geocov(kmis2015_geocov, "2015")
kmis2020_geocov_clean <- clean_geocov(kmis2020_geocov, "2020")

# -----------------------------------------------------------------
# Extract GPS from shapefiles
# -----------------------------------------------------------------

kmis2015_coords <- kmis2015_shape %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(DHSCLUST, longitude, latitude)

kmis2020_coords <- kmis2020_shape %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(DHSCLUST, longitude, latitude)

cat("GPS extracted: 2015 =", nrow(kmis2015_coords), 
    "2020 =", nrow(kmis2020_coords), "\n")

# -----------------------------------------------------------------
# Create analytical dataset for 2015
# -----------------------------------------------------------------

cat("\nCreating analytical dataset for KMIS 2015...\n")

analytical_2015 <- kmis2015_pr %>%
  filter(hv105 >= 0 & hv105 <= 4) %>%
  mutate(
    malaria = case_when(
      hml32 == 1 ~ 1,
      hml32 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    itn_use = case_when(
      hml12 == 1 ~ 1,
      hml12 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    age_months = hv105 * 12,
    child_sex = hv104,
    cluster = hv001,
    household = hv002,
    line_number = hvidx
  ) %>%
  select(cluster, household, line_number, malaria, itn_use, age_months, child_sex)

hr_subset_2015 <- kmis2015_hr %>%
  select(cluster = hv001, household = hv002,
         wealth = hv270,
         urban_rural = hv025,
         electricity = hv206,
         water_source = hv201,
         sanitation = hv205,
         floor_material = hv213,
         sample_weight = hv005)

analytical_2015 <- analytical_2015 %>%
  left_join(hr_subset_2015, by = c("cluster", "household"))

geocov_subset_2015 <- kmis2015_geocov_clean %>%
  select(DHSCLUST,
         rainfall = Rainfall_2015,
         temp_mean = Mean_Temperature_2015,
         evi = Enhanced_Vegetation_Index_2015,
         aridity = Aridity_2015,
         wet_days = Wet_Days_2015,
         population = All_Population_Count_2015)

analytical_2015 <- analytical_2015 %>%
  left_join(geocov_subset_2015, by = c("cluster" = "DHSCLUST"))

analytical_2015 <- analytical_2015 %>%
  left_join(kmis2015_coords, by = c("cluster" = "DHSCLUST"))

analytical_2015 <- analytical_2015 %>%
  mutate(
    wealth = factor(wealth, levels = 1:5,
                    labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    residence = factor(urban_rural, levels = 1:2,
                       labels = c("Urban", "Rural")),
    sex = factor(child_sex, levels = 1:2,
                 labels = c("Male", "Female")),
    electricity = ifelse(electricity == 1, 1, 0),
    improved_water = ifelse(water_source %in% c(11,12,13,14,21,22,31,32,41,51,61,71), 1, 0),
    improved_sanitation = ifelse(sanitation %in% c(11,12,13,14,15,21,22,23,41), 1, 0),
    sample_weight = sample_weight / 1000000,
    survey_year = 2015
  ) %>%
  filter(!is.na(malaria), !is.na(itn_use))

cat("KMIS 2015: N =", nrow(analytical_2015), 
    "malaria positivity =", round(mean(analytical_2015$malaria)*100,1), "%",
    "ITN use =", round(mean(analytical_2015$itn_use)*100,1), "%",
    "clusters =", length(unique(analytical_2015$cluster)), "\n")

# -----------------------------------------------------------------
# Create analytical dataset for 2020
# -----------------------------------------------------------------

cat("\nCreating analytical dataset for KMIS 2020...\n")

analytical_2020 <- kmis2020_pr %>%
  filter(hv105 >= 0 & hv105 <= 4) %>%
  mutate(
    malaria = case_when(
      hml32 == 1 ~ 1,
      hml32 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    itn_use = case_when(
      hml12 == 1 ~ 1,
      hml12 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    age_months = hv105 * 12,
    child_sex = hv104,
    cluster = hv001,
    household = hv002,
    line_number = hvidx
  ) %>%
  select(cluster, household, line_number, malaria, itn_use, age_months, child_sex)

hr_subset_2020 <- kmis2020_hr %>%
  select(cluster = hv001, household = hv002,
         wealth = hv270,
         urban_rural = hv025,
         electricity = hv206,
         water_source = hv201,
         sanitation = hv205,
         floor_material = hv213,
         sample_weight = hv005)

analytical_2020 <- analytical_2020 %>%
  left_join(hr_subset_2020, by = c("cluster", "household"))

geocov_subset_2020 <- kmis2020_geocov_clean %>%
  select(DHSCLUST,
         rainfall = Rainfall_2020,
         temp_mean = Mean_Temperature_2020,
         evi = Enhanced_Vegetation_Index_2020,
         aridity = Aridity_2020,
         wet_days = Wet_Days_2020,
         population = All_Population_Count_2020,
         elevation = Elevation)

analytical_2020 <- analytical_2020 %>%
  left_join(geocov_subset_2020, by = c("cluster" = "DHSCLUST"))

analytical_2020 <- analytical_2020 %>%
  left_join(kmis2020_coords, by = c("cluster" = "DHSCLUST"))

analytical_2020 <- analytical_2020 %>%
  mutate(
    wealth = factor(wealth, levels = 1:5,
                    labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    residence = factor(urban_rural, levels = 1:2,
                       labels = c("Urban", "Rural")),
    sex = factor(child_sex, levels = 1:2,
                 labels = c("Male", "Female")),
    electricity = ifelse(electricity == 1, 1, 0),
    improved_water = ifelse(water_source %in% c(11,12,13,14,21,22,31,32,41,51,61,71), 1, 0),
    improved_sanitation = ifelse(sanitation %in% c(11,12,13,14,15,21,22,23,41), 1, 0),
    sample_weight = sample_weight / 1000000,
    survey_year = 2020
  ) %>%
  filter(!is.na(malaria), !is.na(itn_use))

cat("KMIS 2020: N =", nrow(analytical_2020), 
    "malaria positivity =", round(mean(analytical_2020$malaria)*100,1), "%",
    "ITN use =", round(mean(analytical_2020$itn_use)*100,1), "%",
    "clusters =", length(unique(analytical_2020$cluster)), "\n")

# -----------------------------------------------------------------
# Combine datasets
# -----------------------------------------------------------------

analytical_combined <- bind_rows(analytical_2015, analytical_2020)

cat("\nCombined dataset: N =", nrow(analytical_combined), 
    "malaria positivity =", round(mean(analytical_combined$malaria)*100,1), "%",
    "ITN use =", round(mean(analytical_combined$itn_use)*100,1), "%",
    "clusters =", length(unique(analytical_combined$cluster)), "\n")

# -----------------------------------------------------------------
# Create cluster-level datasets for mapping
# -----------------------------------------------------------------

cluster_level_2015 <- analytical_2015 %>%
  group_by(cluster, longitude, latitude, survey_year) %>%
  summarise(
    n_children = n(),
    malaria_prevalence = mean(malaria, na.rm = TRUE) * 100,
    itn_use_rate = mean(itn_use, na.rm = TRUE) * 100,
    mean_rainfall = mean(rainfall, na.rm = TRUE),
    mean_temp = mean(temp_mean, na.rm = TRUE),
    mean_evi = mean(evi, na.rm = TRUE),
    .groups = "drop"
  )

cluster_level_2020 <- analytical_2020 %>%
  group_by(cluster, longitude, latitude, survey_year) %>%
  summarise(
    n_children = n(),
    malaria_prevalence = mean(malaria, na.rm = TRUE) * 100,
    itn_use_rate = mean(itn_use, na.rm = TRUE) * 100,
    mean_rainfall = mean(rainfall, na.rm = TRUE),
    mean_temp = mean(temp_mean, na.rm = TRUE),
    mean_evi = mean(evi, na.rm = TRUE),
    .groups = "drop"
  )

cluster_level_combined <- bind_rows(cluster_level_2015, cluster_level_2020)

cat("Cluster-level: 2015 =", nrow(cluster_level_2015), 
    "2020 =", nrow(cluster_level_2020), 
    "combined =", nrow(cluster_level_combined), "\n")

# -----------------------------------------------------------------
# Save everything
# -----------------------------------------------------------------

saveRDS(analytical_2015, paste0(clean_dir, "analytical_2015.rds"))
saveRDS(analytical_2020, paste0(clean_dir, "analytical_2020.rds"))
saveRDS(analytical_combined, paste0(clean_dir, "analytical_combined.rds"))
saveRDS(cluster_level_2015, paste0(clean_dir, "cluster_level_2015.rds"))
saveRDS(cluster_level_2020, paste0(clean_dir, "cluster_level_2020.rds"))
saveRDS(cluster_level_combined, paste0(clean_dir, "cluster_level_combined.rds"))
saveRDS(kmis2015_shape, paste0(clean_dir, "shape_2015.rds"))
saveRDS(kmis2020_shape, paste0(clean_dir, "shape_2020.rds"))

write.csv(analytical_2015, paste0(clean_dir, "analytical_2015.csv"), row.names = FALSE)
write.csv(analytical_2020, paste0(clean_dir, "analytical_2020.csv"), row.names = FALSE)
write.csv(analytical_combined, paste0(clean_dir, "analytical_combined.csv"), row.names = FALSE)
write.csv(cluster_level_combined, paste0(clean_dir, "cluster_level_combined.csv"), row.names = FALSE)

cat("\nWeek 2 complete. All saved to", clean_dir, "\n")