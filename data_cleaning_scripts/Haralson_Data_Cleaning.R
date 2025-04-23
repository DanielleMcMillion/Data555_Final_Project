library(here)
library(tidyverse)

# Loading CSVs
poverty <- read_csv(here("Haralson_County_Final", "data", "ACSST5Y2023.S1701_Poverty_Status.csv"), skip = 1)
private_ins <- read_csv(here("Haralson_County_Final", "data", "ACSST5Y2023.S2703_Private_Health_Insurance.csv"), skip = 1)
public_ins <- read_csv(here("Haralson_County_Final", "data", "ACSST5Y2023.S2704_Public_Health_Insurance.csv"), skip = 1)
uninsured <- read_csv(here("Haralson_County_Final", "data", "ACSST5Y2023.S2701-2025-04-07T040103.csv"), skip = 1)

#cleaning
clean_num <- function(x) as.numeric(gsub(",", "", str_remove(x, "%")))

# Poverty Data (First appearances only)
poverty_clean <- poverty %>%
  select(Group = `Population for whom poverty status is determined`,
         Poverty_Total = `30,445`,
         Poverty_Count = `4,596`,
         Poverty_Percent = `15.1%`) %>%
  filter(!Poverty_Percent %in% c("-", "(X)")) %>%
  slice_head(n = 16) %>% 
  mutate(
    Age_Group = case_when(
      str_detect(Group, "Under 18 years") ~ "18 and under",
      str_detect(Group, "18 to 34 years") ~ "19-34",
      str_detect(Group, "35 to 64 years") ~ "35-64",
      str_detect(Group, "65 years and over") ~ "65+",
      TRUE ~ NA_character_
    ),
    Poverty_Total = clean_num(Poverty_Total),
    Poverty_Count = clean_num(Poverty_Count),
    Poverty_Percent = clean_num(Poverty_Percent)
  ) %>%
  filter(!is.na(Age_Group)) %>%
  distinct(Age_Group, .keep_all = TRUE)

# Private Insurance (Alone or in combination only)
private_section_start <- which(str_detect(private_ins$`Civilian noninstitutionalized population`, "PRIVATE HEALTH INSURANCE ALONE OR IN COMBINATION"))[1]
private_clean <- private_ins %>%
  slice((private_section_start + 1):n()) %>%
  filter(str_detect(`Civilian noninstitutionalized population`, "Under|to|years")) %>%
  select(Group = `Civilian noninstitutionalized population`,
         Private_Total = `30,413`,
         Private_Count = `18,838`,
         Private_Percent = `61.90%`) %>%
  mutate(
    Age_Group = case_when(
      str_detect(Group, "Under 6|6 to 18|Under 19") ~ "18 and under",
      str_detect(Group, "19 to 25|26 to 34") ~ "19-34",
      str_detect(Group, "35 to 44|45 to 54|55 to 64|19 to 64") ~ "35-64",
      str_detect(Group, "65 to 74|75") ~ "65+",
      TRUE ~ NA_character_
    ),
    Private_Total = clean_num(Private_Total),
    Private_Count = clean_num(Private_Count),
    Private_Percent = clean_num(Private_Percent)
  ) %>%
  filter(!is.na(Age_Group)) %>%
  group_by(Age_Group) %>%
  summarise(
    Private_Total = first(Private_Total[!is.na(Private_Total)]),
    Private_Count = sum(Private_Count, na.rm = TRUE),
    Private_Percent = mean(Private_Percent, na.rm = TRUE)
  ) %>%
  ungroup()


# Public Insurance (Alone or in combination only)
public_section_start <- which(str_detect(public_ins$`Civilian noninstitutionalized population`, "PUBLIC HEALTH INSURANCE ALONE OR IN COMBINATION"))[1]
public_clean <- public_ins %>%
  slice((public_section_start + 1):n()) %>%
  filter(str_detect(`Civilian noninstitutionalized population`, "Under|to|years")) %>%
  select(Group = `Civilian noninstitutionalized population`,
         Public_Total = `30,413`,
         Public_Count = `11,634`,
         Public_Percent = `38.30%`) %>%
  mutate(
    Age_Group = case_when(
      str_detect(Group, "Under 6|6 to 18|Under 19") ~ "18 and under",
      str_detect(Group, "19 to 25|26 to 34") ~ "19-34",
      str_detect(Group, "35 to 44|45 to 54|55 to 64") ~ "35-64",
      str_detect(Group, "65 to 74|75") ~ "65+",
      TRUE ~ NA_character_
    ),
    Public_Total = clean_num(Public_Total),
    Public_Count = clean_num(Public_Count),
    Public_Percent = clean_num(Public_Percent)
  ) %>%
  filter(!is.na(Age_Group)) %>%
  group_by(Age_Group) %>%
  summarise(
    Public_Total = first(Public_Total[!is.na(Public_Total)]),
    Public_Count = sum(Public_Count, na.rm = TRUE),
    Public_Percent = mean(Public_Percent, na.rm = TRUE)
  ) %>%
  ungroup()


# Uninsured (only first time age groups appear)
uninsured_clean <- uninsured %>%
  select(Group = `Civilian noninstitutionalized population`,
         Uninsured_Total = `30,413`,
         Uninsured_Count = `3,650`,
         Uninsured_Percent = `12.0%`) %>%
  slice_head(n = 30) %>%  
  mutate(
    Age_Group = case_when(
      str_detect(Group, "Under 6|6 to 18") ~ "18 and under",
      str_detect(Group, "19 to 25|26 to 34") ~ "19-34",
      str_detect(Group, "35 to 44|45 to 54|55 to 64") ~ "35-64",
      str_detect(Group, "65 to 74|75 years and older") ~ "65+",
      TRUE ~ NA_character_
    ),
    Uninsured_Total = clean_num(Uninsured_Total),
    Uninsured_Count = clean_num(Uninsured_Count),
    Uninsured_Percent = clean_num(Uninsured_Percent)
  ) %>%
  filter(!is.na(Age_Group)) %>%
  group_by(Age_Group) %>%
  summarise(
    Uninsured_Total = first(Uninsured_Total[!is.na(Uninsured_Total)]),  # or use max()
    Uninsured_Count = sum(Uninsured_Count, na.rm = TRUE),
    Uninsured_Percent = mean(Uninsured_Percent, na.rm = TRUE)
  ) %>%
  ungroup()

# Combine all datasets
combined_data <- poverty_clean %>%
  full_join(private_clean, by = "Age_Group") %>%
  full_join(public_clean, by = "Age_Group") %>%
  full_join(uninsured_clean, by = "Age_Group") %>%
  mutate(
    Total_Pop = coalesce(Poverty_Total, Private_Total, Public_Total, Uninsured_Total)
  ) %>%
  select(
    Age_Group, Total_Pop,
    Poverty_Count, Poverty_Percent,
    Private_Count, Private_Percent,
    Public_Count, Public_Percent,
    Uninsured_Count, Uninsured_Percent
  ) %>%
  distinct()

# Order
age_order <- c("18 and under" = 1, "19-34" = 2, "35-64" = 3, "65+" = 4)
combined_data <- combined_data %>%
  mutate(Age_Order = age_order[Age_Group]) %>%
  arrange(Age_Order) %>%
  select(-Age_Order)

#Export (and view)
View(combined_data)
write_csv(combined_data, here("Haralson_County_Final", "data", "haralson_data_cleaned.csv"))
