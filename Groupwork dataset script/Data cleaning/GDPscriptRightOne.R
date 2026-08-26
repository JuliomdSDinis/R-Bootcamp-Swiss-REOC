# Install packages

install.packages(c("tidyverse", "skimr", "janitor", "corrplot"))
install.packages("readxl")   

# Load packages 

library(readxl)
library(skimr)
library(janitor)
library(tidyverse)
library(corrplot)

# 1. Load the dataset

GDP <- read_excel("./Dataset 2 excel/SNB - Gross domestic product by type of production – real.xlsx",
              skip = 17,
              col_names = FALSE
)

# 2. Give name to columns

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

# 4. Keep only GDP values

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

# 5. Convert GDP columns to numeric

GDP <- GDP %>%
  mutate(
    across(
      -quarter,
      as.numeric
    )
  )

# Check the structure

str(GDP)

# 6. Remove duplicates

GDP <- GDP %>%
  distinct()

sum(duplicated(GDP))

# 7. Check the quarterly observations

head(GDP$quarter, 10)
tail(GDP$quarter, 10)

# 8. Convert quarter into a proper date

GDP <- GDP %>%
  mutate(
    quarter_date = case_when(
      str_detect(quarter, "-Q1") ~ as.Date(paste0(str_sub(quarter, 1, 4), "-01-01")),
      str_detect(quarter, "-Q2") ~ as.Date(paste0(str_sub(quarter, 1, 4), "-04-01")),
      str_detect(quarter, "-Q3") ~ as.Date(paste0(str_sub(quarter, 1, 4), "-07-01")),
      str_detect(quarter, "-Q4") ~ as.Date(paste0(str_sub(quarter, 1, 4), "-10-01"))
    )
  )

# Check

GDP %>%
  select(quarter, quarter_date) %>%
  head(10)

# 9. Check missing values

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

# 10. Convert quarterly GDP to monthly

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

# Interpolate

GDP_monthly <- GDP_monthly %>%
  complete(
    quarter_date = monthly_dates
  ) %>%
  arrange(quarter_date)

# Interpolate all numeric variables

GDP_monthly <- GDP_monthly %>%
  complete(
    quarter_date = monthly_dates
  ) %>%
  arrange(quarter_date)

# Rename the date

GDP_monthly <- GDP_monthly %>%
  rename(month = quarter_date)

# 11. Check the monthly dataset

head(GDP_monthly, 20)
tail(GDP_monthly, 20)
str(GDP_monthly)

# 12. Check missing values

GDP_monthly %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

# 13. Check duplicated months

sum(duplicated(GDP_monthly$month))

# 14. Plot

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

# 15. Compare quarterly vs monthly

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
