# ===========================================================
# Week 1: Data Familiarization - KMIS 2015 and 2020
# Nicholas Mwanza
# ===========================================================

rm(list = ls())
setwd("/home/student25/Documents/Projects/THESIS/Scripts/")

library(haven)
library(dplyr)
library(tidyr)
library(sf)

data_dir <- "/home/student25/Documents/Projects/THESIS/Data/Raw/"
clean_dir <- "/home/student25/Documents/Projects/THESIS/Data/Cleaned/"

if(!dir.exists(clean_dir)) dir.create(clean_dir, recursive = TRUE)

# -----------------------------------------------------------------
# Load KMIS 2015
# -----------------------------------------------------------------

cat("\nLoading KMIS 2015...\n")

kmis2015_pr <- read_dta(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEPR7ADT/KEPR7AFL.DTA"))
kmis2015_hr <- read_dta(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEHR7ADT/KEHR7AFL.DTA"))
kmis2015_kr <- read_dta(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEKR7ADT/KEKR7AFL.DTA"))
kmis2015_shape <- st_read(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEGE7AFL/KEGE7AFL.shp"))
kmis2015_geocov <- read.csv(paste0(data_dir, "KE_2015_MIS_04012026_1540_243961/KEGC7BFL/KEGC7BFL.csv"))

cat("KMIS 2015 loaded. PR:", nrow(kmis2015_pr), "rows\n")

# -----------------------------------------------------------------
# Load KMIS 2020
# -----------------------------------------------------------------

cat("\nLoading KMIS 2020...\n")

kmis2020_pr <- read_dta(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEPR81DT/KEPR81FL.DTA"))
kmis2020_hr <- read_dta(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEHR81DT/KEHR81FL.DTA"))
kmis2020_kr <- read_dta(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEKR81DT/KEKR81FL.DTA"))
kmis2020_shape <- st_read(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEGE81FL/KEGE81FL.shp"))
kmis2020_geocov <- read.csv(paste0(data_dir, "KE_2020_MIS_03182026_1816_243961/KEGC81FL/KEGC81FL/KEGC81FL.csv"))

cat("KMIS 2020 loaded. PR:", nrow(kmis2020_pr), "rows\n")

# -----------------------------------------------------------------
# Clean geocov data (replace negative flags with NA)
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
  
  cat("Cleaned", survey_year, "geocov data\n")
  return(data)
}

kmis2015_geocov_clean <- clean_geocov(kmis2015_geocov, "2015")
kmis2020_geocov_clean <- clean_geocov(kmis2020_geocov, "2020")

# -----------------------------------------------------------------
# Merge geocov to shapefile (survey-specific variables)
# -----------------------------------------------------------------

kmis2015_geocov_select <- kmis2015_geocov_clean %>%
  select(DHSCLUST,
         Rainfall_2015,
         Mean_Temperature_2015,
         Enhanced_Vegetation_Index_2015,
         Aridity_2015,
         Wet_Days_2015,
         All_Population_Count_2015)

kmis2015_shape <- kmis2015_shape %>%
  left_join(kmis2015_geocov_select, by = "DHSCLUST")

kmis2020_geocov_select <- kmis2020_geocov_clean %>%
  select(DHSCLUST,
         Rainfall_2020,
         Mean_Temperature_2020,
         Enhanced_Vegetation_Index_2020,
         Aridity_2020,
         Wet_Days_2020,
         All_Population_Count_2020,
         Elevation)

kmis2020_shape <- kmis2020_shape %>%
  left_join(kmis2020_geocov_select, by = "DHSCLUST")

# -----------------------------------------------------------------
# Filter to children under 5
# -----------------------------------------------------------------

kmis2015_u5 <- kmis2015_pr %>% filter(hv105 >= 0 & hv105 <= 4)
kmis2020_u5 <- kmis2020_pr %>% filter(hv105 >= 0 & hv105 <= 4)

cat("Under-5 children - 2015:", nrow(kmis2015_u5), "2020:", nrow(kmis2020_u5), "\n")

# -----------------------------------------------------------------
# Create analytical dataset for 2015
# -----------------------------------------------------------------

create_analytical_2015 <- function(pr_data, kr_data, hr_data, shape_data) {
  
  df <- pr_data %>%
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
      )
    ) %>%
    select(
      cluster = hv001,
      household = hv002,
      line = hvidx,
      malaria,
      itn_use,
      age_years = hv105,
      sex = hv104
    )
  
  if("bidx" %in% names(kr_data)) {
    kr_subset <- kr_data %>%
      select(cluster = v001, household = v002, line = bidx, maternal_ed = v106)
    df <- df %>% left_join(kr_subset, by = c("cluster", "household", "line"))
  }
  
  hr_subset <- hr_data %>%
    select(cluster = hv001, household = hv002, wealth = hv270, urban_rural = hv025)
  df <- df %>% left_join(hr_subset, by = c("cluster", "household"))
  
  shape_subset <- shape_data %>%
    st_drop_geometry() %>%
    select(DHSCLUST, 
           rainfall = Rainfall_2015,
           temp_mean = Mean_Temperature_2015,
           evi = Enhanced_Vegetation_Index_2015,
           aridity = Aridity_2015,
           wet_days = Wet_Days_2015,
           population = All_Population_Count_2015)
  
  df <- df %>% left_join(shape_subset, by = c("cluster" = "DHSCLUST"))
  df$survey_year <- 2015
  
  df <- df %>%
    mutate(
      wealth = factor(wealth, levels = 1:5, 
                      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
      urban_rural = factor(urban_rural, levels = 1:2, labels = c("Urban", "Rural")),
      sex = factor(sex, levels = 1:2, labels = c("Male", "Female")),
      maternal_ed = factor(maternal_ed, levels = 0:3,
                           labels = c("None", "Primary", "Secondary", "Higher"))
    )
  
  return(df)
}

# -----------------------------------------------------------------
# Create analytical dataset for 2020
# -----------------------------------------------------------------

create_analytical_2020 <- function(pr_data, kr_data, hr_data, shape_data) {
  
  df <- pr_data %>%
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
      )
    ) %>%
    select(
      cluster = hv001,
      household = hv002,
      line = hvidx,
      malaria,
      itn_use,
      age_years = hv105,
      sex = hv104
    )
  
  if("bidx" %in% names(kr_data)) {
    kr_subset <- kr_data %>%
      select(cluster = v001, household = v002, line = bidx, maternal_ed = v106)
    df <- df %>% left_join(kr_subset, by = c("cluster", "household", "line"))
  }
  
  hr_subset <- hr_data %>%
    select(cluster = hv001, household = hv002, wealth = hv270, urban_rural = hv025)
  df <- df %>% left_join(hr_subset, by = c("cluster", "household"))
  
  shape_subset <- shape_data %>%
    st_drop_geometry() %>%
    select(DHSCLUST, 
           rainfall = Rainfall_2020,
           temp_mean = Mean_Temperature_2020,
           evi = Enhanced_Vegetation_Index_2020,
           aridity = Aridity_2020,
           wet_days = Wet_Days_2020,
           population = All_Population_Count_2020,
           elevation = Elevation)
  
  df <- df %>% left_join(shape_subset, by = c("cluster" = "DHSCLUST"))
  df$survey_year <- 2020
  
  df <- df %>%
    mutate(
      wealth = factor(wealth, levels = 1:5, 
                      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
      urban_rural = factor(urban_rural, levels = 1:2, labels = c("Urban", "Rural")),
      sex = factor(sex, levels = 1:2, labels = c("Male", "Female")),
      maternal_ed = factor(maternal_ed, levels = 0:3,
                           labels = c("None", "Primary", "Secondary", "Higher"))
    )
  
  return(df)
}

analytical_2015 <- create_analytical_2015(kmis2015_u5, kmis2015_kr, kmis2015_hr, kmis2015_shape)
analytical_2020 <- create_analytical_2020(kmis2020_u5, kmis2020_kr, kmis2020_hr, kmis2020_shape)
analytical_combined <- bind_rows(analytical_2015, analytical_2020)

cat("Analytical datasets: 2015 =", nrow(analytical_2015), 
    "2020 =", nrow(analytical_2020), 
    "combined =", nrow(analytical_combined), "\n")

# -----------------------------------------------------------------
# Complete case analysis
# -----------------------------------------------------------------

complete_2015 <- analytical_2015 %>%
  filter(!is.na(malaria), !is.na(itn_use), !is.na(wealth))

complete_2020 <- analytical_2020 %>%
  filter(!is.na(malaria), !is.na(itn_use), !is.na(wealth))

complete_combined <- analytical_combined %>%
  filter(!is.na(malaria), !is.na(itn_use), !is.na(wealth))

# -----------------------------------------------------------------
# Summary statistics
# -----------------------------------------------------------------

pos_2015 <- sum(complete_2015$malaria == 1, na.rm = TRUE)
pos_2020 <- sum(complete_2020$malaria == 1, na.rm = TRUE)
pos_combined <- sum(complete_combined$malaria == 1, na.rm = TRUE)

itn_2015 <- mean(complete_2015$itn_use, na.rm = TRUE) * 100
itn_2020 <- mean(complete_2020$itn_use, na.rm = TRUE) * 100
itn_combined <- mean(complete_combined$itn_use, na.rm = TRUE) * 100

clusters_2015 <- length(unique(complete_2015$cluster))
clusters_2020 <- length(unique(complete_2020$cluster))
clusters_combined <- length(unique(complete_combined$cluster))

cat("\n--- Sample summary ---\n")
cat("KMIS 2015: N=", nrow(complete_2015), 
    "positive=", pos_2015, "(", round(pos_2015/nrow(complete_2015)*100,1), "%)",
    "ITN use=", round(itn_2015,1), "%",
    "clusters=", clusters_2015, "\n")
cat("KMIS 2020: N=", nrow(complete_2020), 
    "positive=", pos_2020, "(", round(pos_2020/nrow(complete_2020)*100,1), "%)",
    "ITN use=", round(itn_2020,1), "%",
    "clusters=", clusters_2020, "\n")
cat("Combined: N=", nrow(complete_combined), 
    "positive=", pos_combined, "(", round(pos_combined/nrow(complete_combined)*100,1), "%)",
    "ITN use=", round(itn_combined,1), "%",
    "clusters=", clusters_combined, "\n")

# -----------------------------------------------------------------
# DML feasibility check
# -----------------------------------------------------------------

cat("\n--- DML feasibility ---\n")
cat("N > 500:", ifelse(nrow(complete_combined) > 500, "PASS", "FAIL"), 
    "(", nrow(complete_combined), ")\n")
cat("Positive cases > 200:", ifelse(pos_combined > 200, "PASS", "FAIL"), 
    "(", pos_combined, ")\n")
cat("Clusters > 9:", ifelse(clusters_combined > 9, "PASS", "FAIL"), 
    "(", clusters_combined, ")\n")

if(nrow(complete_combined) > 500 & pos_combined > 200 & clusters_combined > 9) {
  cat("Proceed with combined dataset.\n")
} else {
  cat("Check missingness before proceeding.\n")
}

# -----------------------------------------------------------------
# Save cleaned data
# -----------------------------------------------------------------

saveRDS(analytical_2015, paste0(clean_dir, "analytical_2015.rds"))
saveRDS(analytical_2020, paste0(clean_dir, "analytical_2020.rds"))
saveRDS(analytical_combined, paste0(clean_dir, "analytical_combined.rds"))
saveRDS(complete_2015, paste0(clean_dir, "complete_2015.rds"))
saveRDS(complete_2020, paste0(clean_dir, "complete_2020.rds"))
saveRDS(complete_combined, paste0(clean_dir, "complete_combined.rds"))
saveRDS(kmis2015_shape, paste0(clean_dir, "shape_2015_env.rds"))
saveRDS(kmis2020_shape, paste0(clean_dir, "shape_2020_env.rds"))

cat("\nSaved to:", clean_dir, "\n")
cat("Week 1 complete.\n")