# ============================================================
# LOAD SWISS PROPERTY PRICING INDEX FROM SNB API
# ============================================================

library(readxl)
library(tidyverse)
library(skimr)
library(janitor)

base_url <- "https://data.snb.ch/en/topics/uvo/cube/plimoinchq?fromDate=2000-Q1"

# ============================================================
# 1. LOAD THE DATASET
# ============================================================

Property <- read_excel(
  "./Dataset 2 excel/SNB - Property princing index.xlsx",
  skip = 21
)

# ============================================================
# 2. PREVIEW THE DATASET
# ============================================================

head(Property)
summary(Property)
str(Property)
skim(Property)

# ============================================================
# 3. REMOVE DUPLICATE ROWS
# ============================================================

Property <- Property %>%
  distinct()

sum(duplicated(Property))

# ============================================================
# 4. CHECK THE DIMENSIONS
# ============================================================

dim(Property)

# ============================================================
# 5. RENAME THE COLUMNS
# ============================================================

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

colnames(Property)

# ============================================================
# 6. CHECK THE QUARTERLY DATES
# ============================================================

Property %>%
  select(quarter) %>%
  head(20)

head(Property$quarter)
tail(Property$quarter)

# ============================================================
# 7. CONVERT PROPERTY PRICE VARIABLES TO NUMERIC
# ============================================================

Property <- Property %>%
  mutate(
    across(
      -quarter,
      as.numeric
    )
  )

str(Property)

# ============================================================
# 8. CHECK MISSING VALUES
# ============================================================

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

# ============================================================
# 9. CONVERT QUARTER TO A PROPER DATE
# ============================================================

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

Property %>%
  select(quarter, quarter_date) %>%
  head(10)

# ============================================================
# 10. CHECK FOR DUPLICATED QUARTERS
# ============================================================

Property %>%
  count(quarter) %>%
  filter(n > 1)

sum(duplicated(Property$quarter))

# ============================================================
# 11. CHECK THE CONTINUITY OF THE QUARTERLY DATA
# ============================================================

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

# ============================================================
# 12. CREATE MONTHLY DATES
# ============================================================

monthly_dates <- seq(
  from = min(Property$quarter_date),
  to = max(Property$quarter_date),
  by = "month"
)

head(monthly_dates)
tail(monthly_dates)

length(monthly_dates)

# ============================================================
# 13. INTERPOLATE QUARTERLY DATA TO MONTHLY DATA
# ============================================================

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

# ============================================================
# 14. PREVIEW THE MONTHLY DATASET
# ============================================================

head(Property_monthly, 20)
tail(Property_monthly, 20)
str(Property_monthly)

# ============================================================
# 15. CHECK MISSING VALUES AGAIN
# ============================================================

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

# ============================================================
# 16. CHECK DUPLICATED MONTHS
# ============================================================

sum(duplicated(Property_monthly$month))

# ============================================================
# 17. CHECK MONTHLY CONTINUITY
# ============================================================

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

# ============================================================
# 18. PLOT ONE PROPERTY-PRICE INDEX
# ============================================================

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

# ============================================================
# 19. COMPARE QUARTERLY AND MONTHLY DATA
# ============================================================

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