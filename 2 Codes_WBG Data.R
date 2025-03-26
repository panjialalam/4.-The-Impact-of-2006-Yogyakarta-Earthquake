###############################################################
## Type: Research
## Title: Codes - WBG Data
## Author: Panji Al 'Alam
## Email: panjialalam@outlook.com
################################################################

library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)
library(haven)
library(fixest)
library(lfe)
library(car)
library(lmtest)
library(did)
library(foreign)
library(Synth)

##--------------------------------------------------------------
## Section 1: CPI Data Preparation
##--------------------------------------------------------------

file_path <- ("/Users/panjialalam/Documents/GitHub/4.-The-Impact-of-2006-Yogyakarta-Earthquake/Data/")

grdp_java <- read_excel(paste0(file_path, "WBG/GDP_IndoDapoer.xlsx"), sheet = "Data")

grdp_java <- grdp_java |>
  filter(`Series Name` %in% c("Total GDP excluding Oil and Gas (in IDR Million), Constant Price",
                              "Total Population (in number of people)")) |>
  mutate(
    across(everything(), ~ na_if(., ".."))
  )

colnames(grdp_java) <- gsub("\\s*\\[YR[0-9]{4}\\]", "", colnames(grdp_java))

# Transform the data
grdp_pivot <- grdp_java |>
  pivot_longer(
    cols = `2000`:`2013`,
    names_to = "year",
    values_to = "gdp_current"
  ) |>
  pivot_wider(
    id_cols = c(`Level`, `Provinces Name`, year),
    names_from = `Series Name`,
    values_from = "gdp_current"
  ) |>
  rename(
    total_grdp = `Total GDP excluding Oil and Gas (in IDR Million), Constant Price`,
    total_pop = `Total Population (in number of people)`
  )

# Calculate the GRDP per-capita for each district
grdpc_java <- grdp_pivot |>
  mutate(
    total_grdp = as.numeric(total_grdp),
    total_pop = as.numeric(total_pop),
    grdp_percapita = total_grdp / total_pop
  ) |>
  group_by(`Provinces Name`) |>
  mutate(
    perc_change_grdpc = ((grdp_percapita / lag(grdp_percapita)) - 1) * 100,
    perc_change_grdp = ((total_grdp / lag(total_grdp)) - 1) * 100,
    change_grdpc = (grdp_percapita - lag(grdp_percapita)),
    change_grdp = (total_grdp - lag(total_grdp))
  ) |>
  ungroup()

# Mark the earthquake districts and before-after earthquake
earthquake_districts <- c("Bantul, Kab.", "Gunung Kidul, Kab.", "Kulon Progo, Kab.",
                          "Sleman, Kab.", "Yogyakarta, Kota", "Purworejo, Kab.",
                          "Magelang, Kab.", "Magelang, Kota", "Boyolali, Kab.",
                          "Klaten, Kab.", "Sukoharjo, Kab.", "Wonogiri, Kab.")

grdpc_quake <- grdpc_java |>
  mutate(
    year = as.numeric(year),
    post = if_else(year < 2006, 0, 1),
    earthquake = if_else(
      `Provinces Name` %in% earthquake_districts, 1, 0),
    earthquake_year = if_else(
      `Provinces Name` %in% earthquake_districts, 2006, 0)
  ) |>
  mutate(
    city_district = ifelse(grepl("Kota", `Provinces Name`), 1, 0)
  )

# Create the interaction term between earthquake and post
grdpc_quake <- grdpc_quake |>
  mutate(D = case_when(earthquake == 1 & post == 1 ~ 1,
                       earthquake == 1 & post == 0 ~ 0,
                       earthquake == 0 ~ 0))

##--------------------------------------------------------------
## Section 2a: Covariates Data Preparation
##--------------------------------------------------------------

# 1. Population
# --------------------------------------------------------------
population <- read_excel(paste0(file_path, "WBG/Population_IndoDapoer.xlsx"), sheet = "Data")

colnames(population) <- gsub("\\s*\\[YR[0-9]{4}\\]", "", colnames(population))

# Add ovariates: total population, urban population percentage
urban_pop <- population |> 
  mutate(
    across(where(is.character), ~ na_if(., "..")),
    across(`2000`:`2015`, as.numeric)
  ) |>
  select(!c(`2014`, `2015`)) |>
  pivot_longer(
    cols = `2000`:`2013`,
    names_to = "year",
    values_to = "value") |>
  pivot_wider(
    id_cols = c(`Level`, `Provinces Name`, year),
    names_from = `Series Name`,
    values_from = "value"
  ) |>
  rename(
    urban_perc = `Percentage of Population in Urban Areas (in % of Total Population), SUSENAS`,
    total_pop = `Total Population (in number of people)`
  ) |>
  mutate(
    year = as.numeric(year)
  )

# Add covariates: population growth
urban_pop <- urban_pop |>
  group_by(`Provinces Name`) |>
  mutate(
    total_pop_growth = (total_pop - lag(total_pop)) / lag(total_pop)
  ) |>
  ungroup()

# 2. GDP sectoral
# --------------------------------------------------------------
gdp_sector <- read_excel(paste0(file_path, "WBG/GDP_Sector_IndoDapoer.xlsx"), sheet = "Data")

colnames(gdp_sector) <- gsub("\\s*\\[YR[0-9]{4}\\]", "", colnames(gdp_sector))

gdp_sector <- gdp_sector |> 
  mutate(
    across(where(is.character), ~ na_if(., "..")),
    across(`2000`:`2015`, as.numeric)
  ) |>
  select(!c(`2014`, `2015`)) |>
  pivot_longer(
    cols = `2000`:`2013`,
    names_to = "year",
    values_to = "gdp") |>
  pivot_wider(
    id_cols = c(`Level`, `Provinces Name`, year),
    names_from = `Series Name`,
    values_from = "gdp"
  ) |>
  mutate(
    year = as.numeric(year)
  )

gdp_sector <- gdp_sector |>
  rename(
    agriculture = `GDP on Agriculture Sector (in IDR Million), Constant Price`,
    construction = `GDP on Construction Sector (in IDR Million), Constant Price`,
    finance = `GDP on Financial Service Sector (in IDR Million), Constant Price`,
    manufacturing = `GDP on Manufacturing Sector (in IDR Million), Constant Price`,
    other_service = `GDP on Other Service Sector (in IDR Million), Constant Price`,
    mining = `GDP on Mining and Quarrying Sector (in IDR Million), Constant Price`,
    trade_accomm = `GDP on Trade, Hotel and Restaurant Sector (in IDR Million), Constant Price`,
    transport_telecom = `GDP on Transportation and Telecommunication Sector (in IDR Million), Constant Price`,
    utilities = `GDP on Utilities Sector (in IDR Million), Constant Price`
  )

# Add covariates: Three-sector model
gdp_sector_aggregate <- gdp_sector |>
  mutate(
    primary_sector = (agriculture + mining + utilities),
    secondary_sector = (manufacturing + construction),
    tertiary_sector = (trade_accomm + transport_telecom + finance + other_service),
    year = as.numeric(year)
  ) |>
  mutate(
    total_gdp_sector = primary_sector + secondary_sector + tertiary_sector,
    primary_percent = (primary_sector / total_gdp_sector),
    secondary_percent = (secondary_sector / total_gdp_sector),
    tertiary_percent = (tertiary_sector / total_gdp_sector)
  )

# 3. Transfers and revenues
# --------------------------------------------------------------
transfer <- read_excel(paste0(file_path, "WBG/Revenue_IndoDapoer.xlsx"), sheet = "Data")

colnames(transfer) <- gsub("\\s*\\[YR[0-9]{4}\\]", "", colnames(transfer))

transfer <- transfer |> 
  mutate(
    across(where(is.character), ~ na_if(., "..")),
    across(`2000`:`2015`, as.numeric)
  ) |>
  select(!c(`2014`, `2015`)) |>
  pivot_longer(
    cols = `2000`:`2013`,
    names_to = "year",
    values_to = "value") |>
  pivot_wider(
    id_cols = c(`Level`, `Provinces Name`, year),
    names_from = `Series Name`,
    values_from = "value"
  ) |>
  select(!c(`Total Specific Allocation Grant for Agriculture (in IDR Billion)`,
            `Total Specific Allocation Grant for Demographic (in IDR Billion)`,
            `Total Specific Allocation Grant for Environment (in IDR Billion)`,
            `Total Specific Allocation Grant for Health (in IDR Billion)`,
            `Total Specific Allocation Grant for Infrastructure (in IDR Billion)`,
            `Total Natural Resources Revenue Sharing from Oil (in IDR, realization value)`,
            `Total Natural Resources Revenue Sharing from Mining (in IDR, realization value)`,
            `Total Natural Resources Revenue Sharing from Forestry (in IDR, realization value)`)) |>
  mutate(
    year = as.numeric(year)
  )

transfer_revenue <- transfer |>
  rename(
    general_alloc = `Total General Allocation Grant/DAU (in IDR)`,
    special_alloc = `Total Special Allocation Grant/DAK (in IDR)`,
    resource_rev_share = `Total Natural Resource Revenue Sharing/DBH SDA (in IDR)`,
    tax_rev_share = `Total Tax Revenue Sharing/DBH Pajak (in IDR)`,
    own_source_rev = `Total Own Source Revenue/PAD (in IDR)`,
    total_rev = `Total Revenue (in IDR)`,
    total_rev_share = `Total Revenue Sharing`
  )

# 4. Area
# --------------------------------------------------------------
area <- read_excel(paste0(file_path, "WBG/Area_IndoDapoer.xlsx"), sheet = "Data")

colnames(area) <- gsub("\\s*\\[YR[0-9]{4}\\]", "", colnames(area))

district_area <- area |> 
  mutate(
    across(where(is.character), ~ na_if(., "..")),
    across(`2000`:`2015`, as.numeric)
  ) |>
  select(!c(`2014`, `2015`)) |>
  pivot_longer(
    cols = `2000`:`2013`,
    names_to = "year",
    values_to = "value") |>
  pivot_wider(
    id_cols = c(`Level`, `Provinces Name`, year),
    names_from = `Series Name`,
    values_from = "value"
  ) |>
  rename(
    area = `Total Area (in Km²)`) |>
  mutate(
    year = as.numeric(year)
  )

# Join the data frames to add covariates
merged_data <- grdpc_quake |>
  left_join(urban_pop, by = c("Level", "Provinces Name", "year")) |>
  left_join(gdp_sector_aggregate, by = c("Level", "Provinces Name", "year")) |>
  left_join(transfer_revenue, by = c("Level", "Provinces Name", "year")) |>
  left_join(district_area, by = c("Level", "Provinces Name", "year")) |>
  select(-total_pop.y) |>
  rename(
    total_pop = total_pop.x
  )

grdpc_data_sectoral <- merged_data |>
  mutate(
    grdpc_primary_sector = grdp_percapita * primary_percent,
    grdpc_secondary_sector = grdp_percapita * secondary_percent,
    grdpc_tertiary_sector = grdp_percapita * tertiary_percent
  ) |>
  rename(
    province = `Level`,
    district = `Provinces Name`
  ) |>
  select(
    province, district, year, earthquake, post, everything()
  ) |>
  mutate(district_id = as.numeric(dense_rank(district)),
         year = as.numeric(year)
  ) |>
  relocate(
    district_id, .after = district
  ) |>
  relocate(
    D, .after = post
  )

# Calculate population density
grdpc_data_sectoral <- grdpc_data_sectoral |>
  mutate(
    pop_density = total_pop / area
  )

# Add lag GRDP per capita
grdpc_data_sectoral <- grdpc_data_sectoral |> 
  arrange(district, year) |> 
  group_by(district) |> 
  mutate(grdp_percapita_lag = lag(grdp_percapita))

# Data structure examination
na_counts_df <- grdpc_data_sectoral |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(cols = province:grdpc_tertiary_sector, 
               names_to = "variable", 
               values_to = "missing") |>
  group_by(variable) |>
  summarize(
    missing = sum(missing)
  )

# Quick data check
check_na <- grdpc_data_sectoral |>
  filter(is.na(resource_rev_share)) |>
  select(district, year, resource_rev_share)

missing_summary <- grdpc_data_sectoral |>
  summarize(across(everything(), ~ sum(is.na(.)))) |>
  group_by(district) |>
  pivot_longer(
    cols = province:grdp_percapita_lag,
    names_to = "variables",
    values_to = "missing"
  )

##--------------------------------------------------------------
## Section 2a: Assumption Check (Plotting)
##--------------------------------------------------------------

# Visual analysis
grdpc_graph <- grdpc_quake |>
  group_by(earthquake, year, post) |>
  summarize(
    avg_grdpc = mean(grdp_percapita, na.rm = TRUE),
    avg_grdp = mean(total_grdp, na.rm = TRUE),
    avg_change_grdpc = mean(change_grdpc, na.rm = TRUE),
    avg_change_grdp = mean(change_grdp, na.rm = TRUE),
    .groups = "keep"
  )

ggplot(grdpc_graph, aes(x = year, y = avg_grdpc, color = factor(earthquake), group = earthquake)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2006, color = "firebrick", linetype = "dashed", size = 0.7) +
  annotate(
    "text", 
    x = 2006.5, y = (mean(grdpc_graph$avg_grdpc)) * 1.2,
    label = "2006 earthquake", 
    color = "firebrick", 
    angle = 90, 
    hjust = 0
  ) +
  labs(
    title = "Adjusted GRDPc by District Over Time\n(IDR, 2000 constant prices)",
    x = "Year",
    y = "Average GRDPc",
    color = "Earthquake"                      
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " million")
  ) +
  scale_x_continuous(
    breaks = seq(min(grdpc_graph$year), max(grdpc_graph$year), by = 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Parallel trends examination
grdpc_pre <- grdpc_quake |>
  filter(year < 2006) |>
  mutate(year = as.character(year))

# Apply the T-test
model <- lm(grdp_percapita ~ earthquake * year, data = grdpc_pre)
summary(model)

##--------------------------------------------------------------
## Section 2b: Assumption Check (Event Study Design)
##--------------------------------------------------------------

# Prepare event study data
event_study_data <- grdpc_data_sectoral |>  
  mutate(ever_treated = ifelse(earthquake == 1, 1, 0),  
         event_time = year - 2006, 
         D = factor(ifelse(earthquake == 1, event_time, 0)), 
         D = relevel(D, ref = "-1")) 

event_study_reg <- felm(grdp_percapita ~ D +  
                          total_pop + urban_perc | district + year | 0 | district,  
                        data = event_study_data)

summary(event_study_reg)

# Event study plot
res <- as.data.frame(summary(event_study_reg)$coefficients)
res$low <- res$Estimate - qnorm(1 - 0.05/2)*res$`Cluster s.e.`
res$high <- res$Estimate + qnorm(1 - 0.05/2)*res$`Cluster s.e.`

res <- res |>
  slice(1:(n() - 2)) |>
  mutate(
    event_time_value = c(0, -6:-2, 1:7))

res <- res |> 
  select(Estimate, event_time_value, low, high)

omitted <- data.frame("Estimate" = 0, 
                      "event_time_value" = -1, # Add an observation for event time -1
                      "low" = 0,
                      "high" = 0)

res <- res |> 
  rbind(omitted) |>
  arrange(event_time_value)

res |> 
  ggplot(aes(x = event_time_value, y = Estimate)) + 
  geom_point(color = "black") + 
  geom_line(color = "firebrick") + 
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = unique(res$event_time_value)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Coefficient estimate graph",
    x = "Years relative to 2006",
    y = "Estimate"
  ) + 
  theme_minimal()

# Check for multicollinearity among event time variables
summary(event_study_data$event_time)
table(event_study_data$event_time, event_study_data$D)

# Check using DiD package by Callaway
reg_did <- att_gt(
  yname = "grdp_percapita",
  gname = "earthquake_year",
  idname = "district_id",
  tname = "year",
  xformla = ~1,
  data = grdpc_data_sectoral,
  est_method = "reg"
)

summary(reg_did)

es <- aggte(reg_did, type = "dynamic")
ggdid(es)

##--------------------------------------------------------------
## Section 3a: Regression Models
##--------------------------------------------------------------

# 1. Fixed effect regression model (full model)
# --------------------------------------------------------------

# With no control and clustered in district-level
fe_reg_controls <- felm(grdp_percapita ~ D | district + year | 0 | district,  
                        data = grdpc_data_sectoral)

# With controls and clustered in district-level
fe_reg_controls <- felm(grdp_percapita ~ D + area + tax_rev_share +
                          total_pop + urban_perc + general_alloc + special_alloc +
                          own_source_rev | district + year | 0 | district,  
                        data = grdpc_data_sectoral)

summary(fe_reg_controls)

# I
fe_reg_interaction <- felm(grdp_percapita ~ D * city_district + area + tax_rev_share +
                             total_pop + urban_perc + general_alloc + special_alloc +
                             own_source_rev | district + year | 0 | district,
                           data = grdpc_data_sectoral)

summary(fe_reg_interaction)

##--------------------------------------------------------------
## Section 4b: Sectoral Regression Models
##--------------------------------------------------------------

# Primary sector
grdpc_data_sectoral <- grdpc_data_sectoral |>
  mutate(grdpc_primary_lag = lag(grdpc_primary_sector),
         grdpc_secondary_lag = lag(grdpc_secondary_sector),
         grdpc_tertiary_lag = lag(grdpc_tertiary_sector))


fe_reg_primary <- felm(grdpc_primary_sector ~ D + area + tax_rev_share +
                         total_pop + urban_perc + general_alloc + special_alloc +
                         own_source_rev | district + year | 0 | district,  
                       data = grdpc_data_sectoral)

summary(fe_reg_primary)

# Secondary sector
fe_reg_secondary <- felm(grdpc_secondary_sector ~ D + area + tax_rev_share +
                           total_pop + urban_perc + general_alloc + special_alloc +
                           own_source_rev | district + year | 0 | district,  
                         data = grdpc_data_sectoral)

summary(fe_reg_secondary)

# Tertiary sector
fe_reg_tertiary <- felm(grdpc_secondary_sector ~ D + area + tax_rev_share +
                          total_pop + urban_perc + general_alloc + special_alloc +
                          own_source_rev | district + year | 0 | district,  
                        data = grdpc_data_sectoral)

summary(fe_reg_secondary)

##--------------------------------------------------------------
## Section 5: Robustness Checks
##--------------------------------------------------------------

# 1. Check for multicollinearity
# --------------------------------------------------------------
vif(lm(grdp_percapita ~ D + area + tax_rev_share +
         total_pop + urban_perc + general_alloc + special_alloc + 
         own_source_rev,
       data = grdpc_data_sectoral))

# 2. Check for heteroskedasticity
# --------------------------------------------------------------
bp_test <- bptest(lm(grdp_percapita ~ D + area + tax_rev_share +
                       total_pop + urban_perc + general_alloc + special_alloc + 
                       own_source_rev, 
                     data = grdpc_data_sectoral))

print(bp_test)

# Cluster at province-level
fe_reg_province <- felm(grdp_percapita ~ D + area + tax_rev_share +
                          total_pop + urban_perc + general_alloc + special_alloc +
                          own_source_rev | province + year | 0 | province,  
                        data = grdpc_data_sectoral)

summary(fe_reg_province)


# 3. Placebo test
# --------------------------------------------------------------
placebo_data <- grdpc_data_sectoral |>
  mutate(
    post = if_else(year < 2003, 0, 1)
  ) |>
  mutate(D = case_when(earthquake == 1 & post == 1 ~ 1,
                       earthquake == 1 & post == 0 ~ 0,
                       earthquake == 0 ~ 0))


# Placebo data fixed-effect regression
fe_reg_placebo <- felm(grdp_percapita ~ D + area + tax_rev_share + resource_rev_share +
                         total_pop + urban_perc + general_alloc + special_alloc +
                         own_source_rev | district + year | 0 | district,
                       data = placebo_data)

summary(fe_reg_placebo)

fe_reg_placebo <- felm(grdp_percapita ~ D | district + year | 0 | district,
                       data = placebo_data)

summary(fe_reg_placebo)

placebo_event_study <- placebo_data |>  
  mutate(ever_treated = ifelse(earthquake == 1, 1, 0),  
         event_time = year - 2003, 
         D = factor(ifelse(earthquake == 1, event_time, 0)), 
         D = relevel(D, ref = "-1")) 

event_reg_placebo <- felm(grdp_percapita ~ D +
                            total_pop + urban_perc | district + year | 0 | province,
                          data = placebo_event_study)

summary(event_reg_placebo)

res <- as.data.frame(summary(event_reg_placebo)$coefficients)
res$low <- res$Estimate - qnorm(1 - 0.05/2)*res$`Cluster s.e.`
res$high <- res$Estimate + qnorm(1 - 0.05/2)*res$`Cluster s.e.`

res <- res |>
  slice(1:(n() - 2)) |>
  mutate(
    event_time_value = c(0, -3:-2, 1:10))

res <- res |> 
  select(Estimate, event_time_value, low, high)

omitted <- data.frame("Estimate" = 0, 
                      "event_time_value" = -1, # Add an observation for event time -1
                      "low" = 0,
                      "high" = 0)

res <- res |> 
  rbind(omitted) |>
  arrange(event_time_value)

res |> 
  ggplot(aes(x = event_time_value, y = Estimate)) + 
  geom_point(color = "black") + 
  geom_line(color = "firebrick") + 
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = unique(res$event_time_value)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Coefficient estimate graph (Placebo test)",
    x = "Years relative to 2003",
    y = "Estimate"
  ) + 
  theme_minimal()

# Check trends for earthquake districts
grdpc_check <- grdpc_quake |>
  filter(`Provinces Name` %in% c("Bantul, Kab.", "Gunung Kidul, Kab.", "Kulon Progo, Kab.",
                                 "Sleman, Kab.", "Yogyakarta, Kota"))

ggplot(grdpc_check, aes(x = year, y = grdp_percapita, color = `Provinces Name`, group = `Provinces Name`)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2006, color = "firebrick", linetype = "dashed", size = 0.7) +
  annotate(
    "text", 
    x = 2006.5, y = (mean(grdpc_graph$avg_grdpc)) * 1.2,
    label = "2006 earthquake", 
    color = "firebrick", 
    angle = 90, 
    hjust = 0
  ) +
  geom_text(data = grdpc_check |> group_by(`Provinces Name`) |> slice_min(year),
            aes(label = `Provinces Name`),
            hjust = -0.2, size = 3.5, show.legend = FALSE) +
  labs(
    title = "Adjusted GRDPc by District Over Time\n(IDR, 2000 constant prices)",
    x = "Year",
    y = "Average GRDPc",
    color = "Earthquake"                      
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " million")
  ) +
  scale_x_continuous(
    breaks = seq(min(grdpc_graph$year), max(grdpc_graph$year), by = 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

##--------------------------------------------------------------
## Section 6: Synthetic Control Method
##--------------------------------------------------------------

# Create the data frame only type of data
is.data.frame(grdpc_data_sectoral)
class(grdpc_data_sectoral)

grdpc_synth <- as.data.frame(grdpc_data_sectoral)
is.data.frame(grdpc_synth)
class(grdpc_synth)

##--------------------------------------------------------------
## Section 6a: Synthetic Control Method (Bantul district)
##--------------------------------------------------------------

# Get earthquake districts other than Kabupaten Bantul
earthquake_district_id <- grdpc_synth |>
  filter(district %in% earthquake_districts & district_id != 7) |>
  pull(district_id) |>
  unique()

# Get the invalid controls due to missing data in GRDP per capita
invalid_controls <- grdpc_synth |>
  select(c(district_id, district, grdp_percapita, total_pop, urban_perc)) |>
  filter(is.na(grdp_percapita),
         is.na(total_pop),
         is.na(urban_perc)) |>
  pull(district_id) |>
  unique()

# Set the valid controls district
all_controls <- grdpc_synth |>
  pull(district_id) |>
  unique()

valid_controls <- setdiff(all_controls, earthquake_district_id)
valid_controls <- setdiff(all_controls, invalid_controls)
valid_controls <- setdiff(valid_controls, 7)

# Set up the SCM
dataprep_out <- dataprep(
  foo = grdpc_synth,
  predictors = c("grdp_percapita", "total_pop", "urban_perc"),
  special.predictors = list(
    list("general_alloc", 2006:2013, c("mean")),
    list("area", 2006, c("mean")),
    list("own_source_rev", 2006, c("median")),
    list("tax_rev_share", 2006, c("median"))
  ),
  dependent = "grdp_percapita",
  unit.variable = "district_id",
  time.variable = "year",
  treatment.identifier = 7,
  controls.identifier = valid_controls, 
  time.predictors.prior = 2000:2005,
  time.optimize.ssr = 2000:2005,
  unit.names.variable = "district",
  time.plot = 2000:2013
)

synth_out <- synth(dataprep_out)

path.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 2006,
          Ylab = "Per capita GDP",
          Xlab = "Year",
          Legend = c("Bantul District", "Synth. Bantul District"),
          Main = "Bantul District vs Synth. Bantul District")

gaps.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 2006,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between per capita GDP in Bantul District and its synthetic version")

##--------------------------------------------------------------
## Section 6b: Synthetic Control Method (Klaten district)
##--------------------------------------------------------------

# Get earthquake districts other than Kabupaten Bantul
earthquake_district_id <- grdpc_synth |>
  filter(district %in% earthquake_districts & district_id != 46) |>
  pull(district_id) |>
  unique()

# Get the invalid controls due to missing data in GRDP per capita
invalid_controls <- grdpc_synth |>
  select(c(district_id, district, grdp_percapita, total_pop, urban_perc)) |>
  filter(is.na(grdp_percapita),
         is.na(total_pop),
         is.na(urban_perc)) |>
  pull(district_id) |>
  unique()

# Set the valid controls district
all_controls <- grdpc_synth |>
  pull(district_id) |>
  unique()

valid_controls <- setdiff(all_controls, earthquake_district_id)
valid_controls <- setdiff(all_controls, invalid_controls)
valid_controls <- setdiff(valid_controls, 46)

# Set up the SCM
dataprep_out <- dataprep(
  foo = grdpc_synth,
  predictors = c("grdp_percapita", "total_pop", "urban_perc"),
  special.predictors = list(
    list("general_alloc", 2006:2013, c("mean")),
    list("area", 2006, c("mean")),
    list("own_source_rev", 2006, c("median")),
    list("tax_rev_share", 2006, c("median"))
  ),
  dependent = "grdp_percapita",
  unit.variable = "district_id",
  time.variable = "year",
  treatment.identifier = 46,
  controls.identifier = valid_controls, 
  time.predictors.prior = 2000:2005,
  time.optimize.ssr = 2000:2005,
  unit.names.variable = "district",
  time.plot = 2000:2013
)

synth_out <- synth(dataprep_out)

path.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 2006,
          Ylab = "Per capita GDP",
          Xlab = "Year",
          Legend = c("Klaten District", "Synth. Klaten District"),
          Main = "Klaten District vs Synth. Klaten District")

gaps.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 2006,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between per capita GDP in Klaten District and its synthetic version")

##--------------------------------------------------------------
## Section 6c: Synthetic Control Method (Bantul and Klaten)
##--------------------------------------------------------------

# Create a new treated unit
treated_avg <- grdpc_data_sectoral |>
  filter(district %in% c("Bantul, Kab.", "Klaten, Kab.")) |>
  group_by(year) |>
  summarise(
    district = "Treated_Pooled",
    district_id = 999,
    grdp_percapita = mean(grdp_percapita, na.rm = TRUE),
    total_pop = mean(total_pop, na.rm = TRUE),
    urban_perc = mean(urban_perc, na.rm = TRUE),
    tax_rev_share = mean(tax_rev_share, na.rm = TRUE),
    own_source_rev = mean(own_source_rev, na.rm = TRUE),
    general_alloc = mean(general_alloc, na.rm = TRUE),
    special_alloc = mean(special_alloc, na.rm = TRUE)
  )

grdpc_synth_pool <- grdpc_data_sectoral |>
  select(
    "year", "district", "district_id", "grdp_percapita",
    "total_pop", "urban_perc", "tax_rev_share", "own_source_rev",
    "general_alloc", "special_alloc"
  ) |>
  filter(
    (!district %in% c("Bantul, Kab.", "Klaten, Kab."))
  ) |>
  bind_rows(treated_avg)

grdpc_synth_pool <- as.data.frame(grdpc_synth_pool)

# Get earthquake districts other than Kabupaten Bantul
earthquake_district_id <- grdpc_synth_pool |>
  filter(district %in% earthquake_districts & district_id != 999) |>
  pull(district_id) |>
  unique()

# Get the invalid controls due to missing data in GRDP per capita
invalid_controls <- grdpc_synth_pool |>
  select(c(district_id, district, grdp_percapita, total_pop, urban_perc)) |>
  filter(is.na(grdp_percapita),
         is.na(total_pop),
         is.na(urban_perc)) |>
  pull(district_id) |>
  unique()

# Set the valid controls district
all_controls <- grdpc_synth_pool |>
  pull(district_id) |>
  unique()

valid_controls <- setdiff(all_controls, earthquake_district_id)
valid_controls <- setdiff(all_controls, invalid_controls)
valid_controls <- setdiff(valid_controls, 999)

# Set up the SCM
dataprep_out <- dataprep(
  foo = grdpc_synth_pool,
  predictors = c("grdp_percapita", "total_pop", "urban_perc"),
  dependent = "grdp_percapita",
  unit.variable = "district_id",
  time.variable = "year",
  treatment.identifier = 999,
  controls.identifier = valid_controls, 
  time.predictors.prior = 2000:2005,
  time.optimize.ssr = 2000:2005,
  unit.names.variable = "district",
  time.plot = 2000:2013
)

synth_out <- synth(dataprep_out)

path.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 2006,
          Ylab = "Per capita GDP",
          Xlab = "Year",
          Legend = c("Earthquake Districts", "Synth. Earthquake Districts"),
          Main = "Earthquake Districts vs Synth. Earthquake Districts")

gaps.plot(synth.res = synth_out,
          dataprep.res = dataprep_out,
          tr.intake = 2006,
          Ylab = "Effect",
          Xlab = "Year",
          Main = " Gap between per capita GDP in Earthquake Districts and its synthetic version")
