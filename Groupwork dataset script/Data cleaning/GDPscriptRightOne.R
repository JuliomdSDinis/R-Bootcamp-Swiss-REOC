# ============================================================
# SWISS GDP BY TYPE OF PRODUCTION - DATA CLEANING & ANALYSIS
# ============================================================

# Install packages
install.packages(c("tidyverse", "skimr", "janitor", "corrplot"))
install.packages("readxl")   

# Load packages 
library(readxl)
library(skimr)
library(janitor)
library(tidyverse)
library(corrplot)

base_url <- "https://data.snb.ch/en/topics/uvo/cube/gdppr?fromDate=1990-Q1"

# ============================================================
# 1. LOAD THE DATASET
# ============================================================

GDP <- read_excel("./Dataset 2 excel/SNB - Gross domestic product by type of production – real.xlsx",
                  skip = 17,
                  col_names = FALSE
)

# ============================================================
# 2. GIVE NAMES TO COLUMNS
# ============================================================

colnames(GDP) <- c(
  "quarter",
  "agriculture_forestry_fishing",
  "mining_quarrying",
  "manufacturing_total",
  "manufacturing_chemicals_pharmaceuticals",
  "manufacturing_other",
  "electricity_gas_steam",
  "water_waste",
  "construction",
  "trade_total",
  "retail_trade",
  "transportation_storage",
  "accommodation_food",
  "information_communication",
  "financial_insurance_total",
  "financial_activities",
  "insurance_activities",
  "real_estate_professional_technical",
  "public_administration",
  "education",
  "health_social_work",
  "arts_entertainment_recreation",
  "other_service_activities",
  "households_as_employers",
  "taxes_products",
  "subsidies_products",
  "gdp"
)

# ============================================================
# 3. KEEP ONLY GDP VALUES
# ============================================================

GDP <- GDP %>%
  select(1:27)

colnames(GDP) <- c(
  "quarter",
  "agriculture_forestry_fishing",
  "mining_quarrying",
  "manufacturing_total",
  "manufacturing_chemicals_pharmaceuticals",
  "manufacturing_other",
  "electricity_gas_steam",
  "water_waste",
  "construction",
  "trade_total",
  "retail_trade",
  "transportation_storage",
  "accommodation_food",
  "information_communication",
  "financial_insurance_total",
  "financial_activities",
  "insurance_activities",
  "real_estate_professional_technical",
  "public_administration",
  "education",
  "health_social_work",
  "arts_entertainment_recreation",
  "other_service_activities",
  "households_as_employers",
  "taxes_products",
  "subsidies_products",
  "gdp"
)

# ============================================================
# 4. CONVERT GDP COLUMNS TO NUMERIC
# ============================================================

GDP <- GDP %>%
  mutate(
    across(
      -quarter,
      as.numeric
    )
  )

str(GDP)

# ============================================================
# 5. REMOVE DUPLICATES
# ============================================================

GDP <- GDP %>%
  distinct()

sum(duplicated(GDP))

# ============================================================
# 6. CHECK THE QUARTERLY OBSERVATIONS
# ============================================================

head(GDP$quarter, 10)
tail(GDP$quarter, 10)

# ============================================================
# 7. CONVERT QUARTER INTO A PROPER DATE
# ============================================================

GDP <- GDP %>%
  mutate(
    quarter_date = case_when(
      str_detect(quarter, "-Q1") ~ as.Date(paste0(str_sub(quarter, 1, 4), "-01-01")),
      str_detect(quarter, "-Q2") ~ as.Date(paste0(str_sub(quarter, 1, 4), "-04-01")),
      str_detect(quarter, "-Q3") ~ as.Date(paste0(str_sub(quarter, 1, 4), "-07-01")),
      str_detect(quarter, "-Q4") ~ as.Date(paste0(str_sub(quarter, 1, 4), "-10-01"))
    )
  )

GDP %>%
  select(quarter, quarter_date) %>%
  head(10)

# ============================================================
# 8. CHECK MISSING VALUES
# ============================================================

missing_values <- GDP %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  mutate(
    missing_percentage = missing_count / nrow(GDP) * 100
  )

missing_values

# ============================================================
# 9. CONVERT QUARTERLY GDP TO MONTHLY
# ============================================================

# These monthly observations are estimated values.
# They are not official monthly SNB observations.

GDP_monthly <- GDP %>%
  select(-quarter) %>%
  arrange(quarter_date)

monthly_dates <- seq(
  from = min(GDP$quarter_date),
  to = max(GDP$quarter_date),
  by = "month"
)

GDP_monthly <- GDP_monthly %>%
  complete(
    quarter_date = monthly_dates
  ) %>%
  arrange(quarter_date)

GDP_monthly <- GDP_monthly %>%
  complete(
    quarter_date = monthly_dates
  ) %>%
  arrange(quarter_date)

# ============================================================
# 10. RENAME THE DATE COLUMN
# ============================================================

GDP_monthly <- GDP_monthly %>%
  rename(month = quarter_date)

# ============================================================
# 11. CHECK THE MONTHLY DATASET
# ============================================================

head(GDP_monthly, 20)
tail(GDP_monthly, 20)
str(GDP_monthly)

# ============================================================
# 12. CHECK MISSING VALUES IN MONTHLY DATA
# ============================================================

GDP_monthly %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

# ============================================================
# 13. CHECK FOR DUPLICATED MONTHS
# ============================================================

sum(duplicated(GDP_monthly$month))

# ============================================================
# 14. PLOT MONTHLY GDP
# ============================================================

ggplot(
  GDP_monthly,
  aes(
    x = month,
    y = agriculture_forestry_fishing
  )
) +
  geom_line() +
  labs(
    title = "Swiss GDP – Agriculture, Forestry and Fishing",
    x = "Month",
    y = "GDP (CHF millions)"
  ) +
  theme_minimal()

# ============================================================
# 15. COMPARE QUARTERLY VS MONTHLY
# ============================================================

ggplot() +
  
  geom_line(
    data = GDP_monthly,
    aes(
      x = month,
      y = agriculture_forestry_fishing
    )
  ) +
  
  geom_point(
    data = GDP,
    aes(
      x = quarter_date,
      y = agriculture_forestry_fishing
    )
  ) +
  
  labs(
    title = "Quarterly GDP and Interpolated Monthly GDP",
    x = "Date",
    y = "GDP (CHF millions)"
  ) +
  
  theme_minimal()