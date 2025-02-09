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

file_path <- ("/Users/panjialalam/Documents/GitHub/4.-The-Impact-of-2006-Yogyakarta-Earthquake/Data/")

##--------------------------------------------------------------
## Section 1: Data Preparation
##--------------------------------------------------------------
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
grdp_java <- read_excel(paste0(file_path, "WBG/GDP_IndoDapoer.xlsx"), sheet = "Data")

grdp_java <- grdp_java |>
  filter(`Series Name` %in% c("Total GDP excluding Oil and Gas (in IDR Million), Constant Price",
                              "Total Population (in number of people)")) |>
  mutate(
    across(everything(), ~ na_if(., ".."))
    )

colnames(grdp_java) <- gsub("\\s*\\[YR[0-9]{4}\\]", "", colnames(grdp_java)) # Simplify the column names

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
earthuake_districts <- c("Bantul, Kab.", "Gunung Kidul, Kab.", "Kulon Progo, Kab.",
                         "Sleman, Kab.", "Yogyakarta, Kota", "Purworejo, Kab.",
                         "Magelang, Kab.", "Magelang, Kota", "Boyolali, Kab.",
                         "Klaten, Kab.", "Sukoharjo, Kab.", "Wonogiri, Kab.")

grdpc_quake <- grdpc_java |>
  mutate(
    year = as.numeric(year),
    post = if_else(year < 2006, 0, 1),
    earthquake = if_else(
      `Provinces Name` %in% earthuake_districts, 1, 0
    )
  )

##--------------------------------------------------------------
## Section 3: Assumption Check
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
## Section 4: Regression Models
##--------------------------------------------------------------

# Calculate the Difference in Means
post_treat <- grdpc_quake |>
  group_by(earthquake, post) |>
  summarize(
    avg = mean(grdp_percapita, na.rm = TRUE)
  )

(post_treat$avg[4] - post_treat$avg[3]) - (post_treat$avg[2] - post_treat$avg[1])

# Simple regression model (model specification)
dd_reg <- lm(grdp_percapita ~ earthquake + post + earthquake * post,
             data = grdpc_quake)
summary(dd_reg)

# Fixed effect regression
grdpc_quake <- grdpc_quake |>
  mutate(D = case_when(earthquake == 1 & post == 1 ~ 1,
                       earthquake == 1 & post == 0 ~ 0,
                       earthquake == 0 ~ 0))

fe_reg <- felm(grdp_percapita ~ D|`Provinces Name` + year|0|`Provinces Name`,
               data = grdpc_quake)
summary(fe_reg)

##--------------------------------------------------------------
## Section 4a: Event Study Design
##--------------------------------------------------------------

event_study <- grdpc_quake |>
  mutate(ever_treated = case_when(earthquake == 1 ~ 1,
                                  earthquake == 0 ~ 0),
         event_time = ifelse(earthquake == 1, year - 2006, 0),
         D = factor(ever_treated * event_time),
         D = relevel(D, ref = "-1"))

fe_reg2 <- felm(grdp_percapita ~ D|`Provinces Name` + year|0|`Provinces Name`, 
                data = event_study)
summary(fe_reg2)

# Event study plot
res <- as.data.frame(summary(fe_reg2)$coefficients)
res$low <- res$Estimate - qnorm(1 - 0.05/2)*res$`Cluster s.e.`
res$high <- res$Estimate + qnorm(1 - 0.05/2)*res$`Cluster s.e.`
res$event_time_value <- c(-6:-2, 0:7)
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
    x = "Event time value",
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

