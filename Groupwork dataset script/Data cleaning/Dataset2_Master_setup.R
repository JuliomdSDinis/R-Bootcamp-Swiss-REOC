# ============================================================
# MACRO MASTER DATASET
# Merge cleaned macroeconomic variables
# Monthly + Yearly
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)

# ============================================================
# 1. RUN INDIVIDUAL MACRO CLEANING SCRIPTS
# ============================================================

source("Groupwork dataset script/Data cleaning/GDPscriptRightOne.R")
source("Groupwork dataset script/Data cleaning/InterestRateScript.R")
source("Groupwork dataset script/Data cleaning/PropertyPriceScript.R")
source("Groupwork dataset script/Data cleaning/UnemploymentScript.R")
source("Groupwork dataset script/Data cleaning/ConsumerPriceScript.R")
source("Groupwork dataset script/Data cleaning/YieldScript.R")

# ============================================================
# 2. STANDARDIZE MONTHLY DATE KEYS
# ============================================================

# Interest rates
interest_macro <- Interest_Rates |>
  mutate(
    month = floor_date(as.Date(month), "month")
  )

# Yields
yield_macro <- Yields |>
  mutate(
    month = floor_date(as.Date(month), "month")
  )

# Unemployment
unemployment_macro <- Unemployment |>
  rename(
    month = month_date
  ) |>
  mutate(
    month = floor_date(as.Date(month), "month")
  )

# Consumer prices / inflation
inflation_macro <- ConsumerPrice |>
  mutate(
    month = as.Date(
      paste0(overview, "-01")
    )
  ) |>
  select(
    -overview
  )

# Property prices
property_macro <- Property_monthly |>
  mutate(
    month = floor_date(as.Date(month), "month")
  )

# GDP
gdp_macro <- GDP_monthly |>
  mutate(
    month = floor_date(as.Date(month), "month")
  )

# ============================================================
# 3. CHECK UNIQUE MONTH KEYS BEFORE MERGING
# ============================================================

interest_macro |>
  count(month) |>
  filter(n > 1)

yield_macro |>
  count(month) |>
  filter(n > 1)

unemployment_macro |>
  count(month) |>
  filter(n > 1)

inflation_macro |>
  count(month) |>
  filter(n > 1)

property_macro |>
  count(month) |>
  filter(n > 1)

gdp_macro |>
  count(month) |>
  filter(n > 1)

# ============================================================
# 4. CREATE MONTHLY MACRO MASTER DATASET
# ============================================================

macro_monthly <- interest_macro |>
  full_join(
    yield_macro,
    by = "month"
  ) |>
  full_join(
    unemployment_macro,
    by = "month"
  ) |>
  full_join(
    inflation_macro,
    by = "month"
  ) |>
  full_join(
    property_macro,
    by = "month"
  ) |>
  full_join(
    gdp_macro,
    by = "month"
  ) |>
  arrange(month)

# ============================================================
# 5. VALIDATE MONTHLY MACRO MASTER
# ============================================================

glimpse(macro_monthly)

macro_monthly |>
  summarise(
    observations = n(),
    first_month = min(month, na.rm = TRUE),
    last_month = max(month, na.rm = TRUE)
  )

macro_monthly |>
  count(month) |>
  filter(n > 1)

# ============================================================
# 6. REMOVE EMPTY MONTH ROW
# ============================================================

macro_monthly <- macro_monthly |>
  filter(!is.na(month)) |>
  arrange(month)

# ============================================================
# 8. CREATE MONTHLY MACRO DATASET FROM 2000
# ============================================================

macro_monthly_2000 <- macro_monthly |>
  filter(
    month >= as.Date("2000-01-01")
  ) |>
  arrange(month)

# ============================================================
# 9. VALIDATE MONTHLY MACRO DATASET
# ============================================================

macro_monthly_2000 |>
  summarise(
    observations = n(),
    first_month = min(month),
    last_month = max(month),
    missing_month = sum(is.na(month))
  )

macro_monthly_2000 |>
  count(month) |>
  filter(n > 1)

# ============================================================
# 10. CREATE YEARLY MACRO DATASET FROM 2000
# ============================================================

macro_yearly_2000 <- macro_monthly_2000 |>
  mutate(
    year = year(month)
  ) |>
  group_by(year) |>
  summarise(
    across(
      where(is.numeric),
      ~ if (all(is.na(.))) {
        NA_real_
      } else {
        mean(., na.rm = TRUE)
      }
    ),
    .groups = "drop"
  ) |>
  arrange(year)

# ============================================================
# 11. VALIDATE YEARLY MACRO DATASET
# ============================================================

macro_yearly_2000 |>
  summarise(
    observations = n(),
    first_year = min(year),
    last_year = max(year)
  )

macro_yearly_2000 |>
  count(year) |>
  filter(n > 1)

glimpse(macro_yearly_2000)

# ============================================================
# 12. CHECK GDP OBSERVATIONS USED PER YEAR
# ============================================================

macro_monthly_2000 |>
  mutate(
    year = year(month)
  ) |>
  group_by(year) |>
  summarise(
    gdp_observations = sum(!is.na(gdp)),
    .groups = "drop"
  ) |>
  print(n = Inf)

# ============================================================
# 13. SAVE MACRO MASTER DATASETS
# ============================================================

write_csv(
  macro_monthly_2000,
  "Groupwork dataset script/Dataset 2 excel/macro_monthly_2000.csv"
)

write_csv(
  macro_yearly_2000,
  "Groupwork dataset script/Dataset 2 excel/macro_yearly_2000.csv"
)