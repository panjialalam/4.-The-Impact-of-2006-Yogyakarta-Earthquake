###############################################################
## Type: Research
## Title: Codes - BPS Data
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
library(lfe)

##--------------------------------------------------------------
## Section 1: Data Preparation
##--------------------------------------------------------------

# Load the data sets
file_path <- ("/Users/panjialalam/Documents/GitHub/4.-The-Impact-of-2006-Yogyakarta-Earthquake/Data/")

## CPI Data Preparation ##
##-------------------------------------------------------------------------------------##
cpi_file <- list.files(path = paste0(file_path, "BPS/BPS_CPI Java Districts"), 
                       pattern = "*.xlsx", full.names = TRUE)
district_java <- read_excel(paste0(file_path, "BPS/district_province.xlsx"), sheet = "java")

# Read CPI data
years <- 2000:2015
data <- list()
districts <- district_java |> # List of districts in Java, except Jakarta
  pull(district)

for (i in 1:length(cpi_file)) {
  # Generate dynamic name for the data frames
  df_name <- paste0("cpi_", years[i])
  data[[i]] <- assign(df_name, read_excel(cpi_file[i]))
  
  # Add year column
  data[[i]] <- data[[i]] |>
    mutate(year = years[i])
  
  # Filter the districts
  data[[i]] <- data[[i]] |>
    filter(`Kota Inflasi` %in% districts)
  
  assign(df_name, data[[i]])
}

# Combine all years into one data frame
cpi_java <- bind_rows(data)

colnames(cpi_java) <- c(
  "district", "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December", "average", "year"
)

cpi_java <- cpi_java |>
  mutate(across(January:December, ~ as.numeric(gsub("[^0-9.]", "", .)))) |>
  rowwise() |>
  mutate(average = mean(c_across(January:December), na.rm = TRUE)) |>
  ungroup()

# Join the districts and provinces
cpi_java <- cpi_java |>
  left_join(district_java, by = "district") |>
  mutate(
    base_year = case_when( # Add base years
      year %in% c(2000, 2001, 2002, 2003) ~ 1996,
      year %in% c(2004, 2005, 2006, 2007) ~ 2002,
      year %in% c(2008, 2009, 2010, 2011, 2012, 2013) ~ 2007,
      year %in% c(2014, 2015) ~ 2012
    ))

cpi_province <- cpi_java |> 
  group_by(province, year, base_year) |> 
  summarize(cpi = mean(average, na.rm = TRUE))

##--------------------------------------------------------------
## Section 2: GRDP Per Capita Data Preparation
##--------------------------------------------------------------
grdpc_java <- read_excel(paste0(file_path, "BPS/BPS_GRDP Java/grdp_java.xlsx"), sheet = "grdpc")

grdpc_long <- grdpc_java |>
  pivot_longer(
    cols = `2000`:`2015`,
    names_to = "Year",       
    values_to = "GRDPC"
  ) |>
  mutate(Year = as.numeric(Year))

# Adjust for oil and gas GRDPc
grdpc_oilgas <- read_excel(paste0(file_path, "BPS/BPS_GRDP Java/grdp_java.xlsx"), sheet = "grdpc_oilgas")

grdpc_oilgas <- grdpc_oilgas |>
  mutate(oil_gas_share = (1 - (without_oilgas / with_oilgas)) * 100)

# Prepare the train data and empty data frame for predictions
train_data <- grdpc_oilgas |>
  filter(year < 2010)

predictions <- grdpc_oilgas |>
  filter(year > 2010) |>
  select(district, year) |>
  mutate(oil_gas_share = NA)

oilgas_district <- unique(grdpc_oilgas$district)

# Predict the oil and gas share for each district
for (dist in unique(grdpc_oilgas$district)) {
  # Subset data for a specific district
  dist_data <- train_data |> filter(district == dist)
  
  # Fit a linear trend model (you can replace with other models if needed)
  model <- lm(oil_gas_share ~ year, data = dist_data)
  
  # Predict for 2011–2015
  future_years <- data.frame(year = 2011:2015)
  predicted_shares <- predict(model, newdata = future_years)
  
  # Update predictions
  predictions <- predictions |>
    mutate(oil_gas_share = ifelse(district == dist & year %in% 2011:2015,
                                  predicted_shares, oil_gas_share))
}

# Calculate the predicted GRDPc without oil gas
grdpc_oilgas <- grdpc_oilgas |>
  left_join(predictions, by = c("district", "year")) |>
  mutate(oil_gas_share = coalesce(oil_gas_share.x, oil_gas_share.y)) |>
  select(-oil_gas_share.x, -oil_gas_share.y)

grdpc_oilgas <- grdpc_oilgas |>
  mutate(without_oilgas = (1 - oil_gas_share / 100) * with_oilgas)

# Join the non oilgas GRDPc
grdpc_long <- grdpc_long |>
  left_join(grdpc_oilgas, by = c("District" = "district", "Year" = "year")) |>
  mutate(
    GRDPC = if_else(District %in% oilgas_district, without_oilgas, GRDPC)
  ) |>
  select(!c(with_oilgas, without_oilgas, oil_gas_share))

##--------------------------------------------------------------
## Section 3: GRDP Per Capita Data Analysis
##--------------------------------------------------------------

# Get base year CPI values for each province
cpi_base <- cpi_province |>
  filter(year %in% c(2000, 2002, 2007, 2012, 2014)) |>
  select(province, year, cpi) |>
  pivot_wider(
    names_from = year, 
    values_from = cpi,
    names_prefix = "cpi_") |>
  mutate("1996" = (cpi_2000/100),
         "2002" = (100/cpi_2002) * (cpi_2000/100),
         "2007" = (100/cpi_2002) * (100/cpi_2007) * (cpi_2000/100),
         "2012" = (100/cpi_2002) * (100/cpi_2007) * (100/cpi_2012) * (cpi_2000/100)) |>
  select("1996", "2002", "2007", "2012") |>
  pivot_longer(
    cols = `1996`:`2012`,
    names_to = "year",
    values_to = "cpi_factor"
  ) |>
  mutate(year = as.numeric(year))

cpi_province <- cpi_province |>
  left_join(cpi_base, by = c("province", "base_year" = "year")) |>
  mutate(
    adj_factor = (100/cpi) * cpi_factor)

# Join the CPI base
grdpc_long <- grdpc_long |>
  left_join(cpi_province, by = c("Province" = "province", "Year" = "year"))

# Compute adjusted GRDPc data
grdpc_adjusted <- grdpc_long |>
  mutate(
    real_grdpc = (GRDPC * adj_factor)
    ) |>
  select(c(District, Province, Earthquake, Year, real_grdpc)) |>
  mutate(
    Year = as.character(Year),
    Post = if_else(Year < 2006, 0, 1),
    Year = as.numeric(Year)
  ) |>
  filter(!District %in% c("Cilacap", "Kota Kediri", "Kota Surabaya", "Kota Cilegon"))

##--------------------------------------------------------------
## Section 4: Assumption Check
##--------------------------------------------------------------

# Parallel trends examination
grdpc_pre <- grdpc_adjusted |>
  filter(Year < 2006) |>
  mutate(Year = as.character(Year))

# Apply the T-test
model <- lm(real_grdpc ~ Earthquake * Year, data = grdpc_pre)
summary(model)

grdpc_graph <- grdpc_adjusted |>
  group_by(Earthquake, Year, Post) |>
  summarize(
    Average = mean(real_grdpc, na.rm = TRUE),
    .groups = "keep"
  )

# Calculate the Difference in Means
post_treat <- grdpc_adjusted |>
  group_by(Earthquake, Post) |>
  summarize(
    avg_grdpc = mean(real_grdpc, na.rm = TRUE)
  )

(post_treat$avg_grdpc[4] - post_treat$avg_grdpc[3]) - (post_treat$avg_grdpc[2] - post_treat$avg_grdpc[1])

# Simple regression model (model specification)
dd_reg <- lm(real_grdpc ~ Earthquake + Post + Earthquake * Post,
             data = grdpc_adjusted)
summary(dd_reg)

# Visual analysis
ggplot(grdpc_graph, aes(x = Year, y = Average, color = factor(Earthquake), group = Earthquake)) +
  geom_line(size = 0.5) +
  geom_point(size = 1.2) +
  geom_vline(xintercept = 2006, color = "firebrick", linetype = "dashed", size = 0.7) +
  annotate(
    "text", 
    x = 2006.5, y = (mean(grdpc_graph$Average)) * 1.2,
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
    labels = label_number(scale = 1e-6, suffix = " million")
    ) +
  scale_x_continuous(
    breaks = seq(min(grdpc_graph$Year), max(grdpc_graph$Year), by = 1)
    ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Fixed effect regression
grdpc_adjusted <- grdpc_adjusted |>
  mutate(D = case_when(Earthquake == 1 & Post == 1 ~ 1,
                       Earthquake == 1 & Post == 0 ~ 0,
                       Earthquake == 0 ~ 0))

fe_reg2 <- felm(real_grdpc ~ D|District + Year|0|District,
                data = grdpc_adjusted)
summary(fe_reg2)

# Event study design
event_study <- grdpc_adjusted |>
  mutate(ever_treated = case_when(Earthquake == 1 ~ 1,
                                  Earthquake == 0 ~ 0),
         event_time = ifelse(Earthquake == 1, Year - 2006, 0),
         D = factor(ever_treated * event_time),
         D = relevel(D, ref = "-1"))

fe_reg3 <- felm(real_grdpc ~ D|District + Year|0|District, 
                data = event_study)
summary(fe_reg3)

# Event study plot
res <- as.data.frame(summary(fe_reg3)$coefficients)
res$low <- res$Estimate - qnorm(1 - 0.05/2)*res$`Cluster s.e.`
res$high <- res$Estimate + qnorm(1 - 0.05/2)*res$`Cluster s.e.`
res$event_time_value <- c(-6:-2, 0:9)
res <- res %>% dplyr::select(Estimate, event_time_value, low, high)

omitted <- data.frame("Estimate" = 0, 
                      "event_time_value" = -1,
                      "low" = 0,
                      "high" = 0)
res <- res |> rbind(omitted)

res |>
  ggplot(aes(x = event_time_value, y = Estimate)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = res$event_time_value) +
  labs(
    title = "Coefficient estimate graph",
    x = "Estimate",
    y = "Event time value"
  )

