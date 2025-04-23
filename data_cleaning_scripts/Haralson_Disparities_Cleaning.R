# Load libraries
library(readr)
library(dplyr)
library(here)

#Load dataset
PLACES_file <- here::here("Haralson_County_Final", "data", "PLACES__Local_Data_for_Better_Health__Place_Data_2022_release_20250409.csv")
df <- read_csv(PLACES_file, show_col_types = FALSE)


haralson_county <- c("Bremen", "Buchanan", "Tallapoosa", "Waco")

#Defining disparities and measures
disparities <- tribble(
  ~Disparity,        ~Measure,                                                                 ~HP2030,
  "Hypertension",    "High blood pressure among adults aged >=18 years",                       41.9,
  "Diabetes",        "Diagnosed diabetes among adults aged >=18 years",                        55.2,
  "Mental Health",   "Suicide & Self-Inflicted Injury deaths",                                 1.28,
  "Mental Health",   "Depression among adults aged >=18 years",                                65.6,
  "Women’s Health",  "Cervical cancer screening among adult women aged 21-65 years",           79.2,
  "Women’s Health",  "Teen births 15-19 years",                                                3.14,
  "Asthma",          "Current asthma among adults aged >=18 years",                            35.1
)

# Haralson County Averages
haralson_df <- df %>%
  filter(StateDesc == "Georgia", LocationName %in% haralson_county) %>%
  inner_join(disparities, by = "Measure") %>%
  group_by(Disparity, Measure) %>%
  summarize(`Haralson County Avg` = round(mean(Data_Value, na.rm = TRUE), 1), .groups = "drop")

#Georgia State Averages
ga_avg <- df %>%
  filter(StateDesc == "Georgia") %>%
  inner_join(disparities, by = "Measure") %>%
  group_by(Disparity, Measure) %>%
  summarize(`Georgia Avg` = round(mean(Data_Value, na.rm = TRUE), 1), .groups = "drop")

#US Averages
us_avg <- df %>%
  inner_join(disparities, by = "Measure") %>%
  group_by(Disparity, Measure) %>%
  summarize(`US Avg` = round(mean(Data_Value, na.rm = TRUE), 1), .groups = "drop")

#Merge all data
comparison_df <- haralson_df %>%
  inner_join(ga_avg, by = c("Disparity", "Measure")) %>%
  inner_join(us_avg, by = c("Disparity", "Measure")) %>%
  inner_join(disparities, by = c("Disparity", "Measure")) %>%
  rename(`HP2030 Target` = HP2030)


manual_additions <- tribble(
  ~Disparity, ~Measure, ~`Haralson County Avg`, ~`Georgia Avg`, ~`US Avg`, ~`HP2030 Target`,
  "Mental Health", "Suicide & Self-Inflicted Injury Death Rates", 2.41, 1.46, 1.1, 1.28,
  "Women’s Health", "Teen Births", 11.6, 1.66, 6.3, 3.14
)

comparison_df <- bind_rows(comparison_df, manual_additions)

print(comparison_df)
output_path <- here::here("Haralson_County_Final", "data", "FQHC_evaluation_data.csv")
write_csv(comparison_df, output_path)
