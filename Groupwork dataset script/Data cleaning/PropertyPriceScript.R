# 1. Load packages

library(readxl)
library(tidyverse)
library(skimr)
library(janitor)

# 2. Load the dataset

Property <- read_excel(
  "./Dataset 2 excel/SNB - Property princing index.xlsx",
  skip = 21
)

# 3. Preview the dataset

head(Property)
summary(Property)
str(Property)
skim(Property)

# 4. Remove duplicate rows

Property <- Property %>%
  distinct()

sum(duplicated(Property))

# 5. Check the dimensions

dim(Property)

# 6. Rename the columns

colnames(Property) <- c(
  "quarter",
  "apartments_fso",
  "apartments_fahrländer",
  "apartments_iazi",
  "apartments_wuest_asking",
  "apartments_wuest_transaction",
  "houses_fso",
  "houses_fahrländer",
  "houses_iazi",
  "houses_wuest_asking",
  "houses_wuest_transaction",
  "apartment_buildings_fahrländer",
  "apartment_buildings_iazi",
  "apartment_buildings_wuest_transaction",
  "single_family_houses_wuest_asking",
  "commercial_office_wuest_asking",
  "commercial_retail_wuest_asking",
  "commercial_industrial_wuest_asking"
)


# Check column names

colnames(Property)

# 7. Check the quarterly dates

Property %>%
  select(quarter) %>%
  head(20)


# First and last observation

head(Property$quarter)
tail(Property$quarter)

# 8. Convert property price variables to numeric

Property <- Property %>%
  mutate(
    across(
      -quarter,
      as.numeric
    )
  )


# Check structure

str(Property)

# 9. Check missing values

missing_values <- Property %>%
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
    missing_percentage =
      missing_count / nrow(Property) * 100
  )

missing_values

# 10 Convert quarter to a proper date

Property <- Property %>%
  mutate(
    quarter_date = case_when(
      str_detect(quarter, "-Q1") ~
        as.Date(paste0(str_sub(quarter, 1, 4), "-01-01")),
      
      str_detect(quarter, "-Q2") ~
        as.Date(paste0(str_sub(quarter, 1, 4), "-04-01")),
      
      str_detect(quarter, "-Q3") ~
        as.Date(paste0(str_sub(quarter, 1, 4), "-07-01")),
      
      str_detect(quarter, "-Q4") ~
        as.Date(paste0(str_sub(quarter, 1, 4), "-10-01"))
    )
  )


# Check

Property %>%
  select(quarter, quarter_date) %>%
  head(10)

# 11 Check for duplicated quarters

Property %>%
  count(quarter) %>%
  filter(n > 1)

sum(duplicated(Property$quarter))

# 12 Check the continuity of the quarterly data

Property %>%
  arrange(quarter_date) %>%
  mutate(
    difference = as.numeric(
      quarter_date - lag(quarter_date)
    )
  ) %>%
  filter(
    !is.na(difference),
    difference > 100
  )

# 13 Create monthly dates

monthly_dates <- seq(
  from = min(Property$quarter_date),
  to = max(Property$quarter_date),
  by = "month"
)

head(monthly_dates)
tail(monthly_dates)

length(monthly_dates)

# 14 Interpolate quarterly data to monthly data

Property_monthly <- data.frame(
  month = monthly_dates
)


for (variable in names(Property)[2:18]) {
  
  valid_data <- Property %>%
    filter(!is.na(.data[[variable]]))
  
  Property_monthly[[variable]] <- approx(
    x = valid_data$quarter_date,
    y = valid_data[[variable]],
    xout = monthly_dates,
    method = "linear",
    rule = 1
  )$y
}

# 15 Preview the monthly dataset

head(Property_monthly, 20)
tail(Property_monthly, 20)
str(Property_monthly)

# 16 Check missing values again

missing_monthly <- Property_monthly %>%
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
    missing_percentage =
      missing_count / nrow(Property_monthly) * 100
  )

missing_monthly

# 17 Check duplicated months

sum(duplicated(Property_monthly$month))

# 18 Check monthly continuity

Property_monthly %>%
  arrange(month) %>%
  mutate(
    difference = as.numeric(
      month - lag(month)
    )
  ) %>%
  summarise(
    minimum_difference = min(
      difference,
      na.rm = TRUE
    ),
    maximum_difference = max(
      difference,
      na.rm = TRUE
    )
  )

# 19 Plot one property-price index

ggplot(
  Property_monthly,
  aes(
    x = month,
    y = apartments_fahrländer
  )
) +
  geom_line() +
  labs(
    title = "Swiss Property Price Index – Privately Owned Apartments",
    subtitle = "Fahrländer Partner – Interpolated Monthly Data",
    x = "Month",
    y = "Index (Q1 2000 = 100)"
  ) +
  theme_minimal()

# 20 Compare quarterly and monthlty data

ggplot() +
  
  geom_line(
    data = Property_monthly,
    aes(
      x = month,
      y = apartments_fahrländer
    )
  ) +
  
  geom_point(
    data = Property,
    aes(
      x = quarter_date,
      y = apartments_fahrländer
    )
  ) +
  
  labs(
    title = "Quarterly and Interpolated Monthly Property Price Index",
    subtitle = "Privately Owned Apartments – Fahrländer Partner",
    x = "Date",
    y = "Index (Q1 2000 = 100)"
  ) +
  
  theme_minimal()