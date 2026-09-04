# ============================================================
# LOAD SWISS LABOUR MARKET DATA FROM SNB API
# ============================================================

library(readxl)
library(tidyverse)
library(skimr)
library(janitor)

base_url <- "https://data.snb.ch/en/topics/uvo/cube/amarbma?fromDate=2000-01"

# ============================================================
# 1. LOAD THE DATASET
# ============================================================

Unemployment <- read_excel(
  "./Dataset 2 excel/SNB - Unemployment.xlsx",
  skip = 21,
  col_names = FALSE
)

# ============================================================
# 2. PREVIEW THE DATASET
# ============================================================

head(Unemployment)
summary(Unemployment)
str(Unemployment)
skim(Unemployment)

# ============================================================
# 3. RENAME THE COLUMNS
# ============================================================

colnames(Unemployment) <- c(
  "month",
  "workers_short_time",
  "registered_unemployed_total",
  "registered_unemployed_sa",
  "jobless_rate_total",
  "jobless_rate_sa",
  "job_vacancies_total",
  "job_vacancies_sa",
  "registered_job_seekers",
  "labour_force"
)

colnames(Unemployment)

# ============================================================
# 4. REMOVE DUPLICATE ROWS
# ============================================================

Unemployment <- Unemployment %>%
  distinct()

sum(duplicated(Unemployment))

# ============================================================
# 5. CHECK THE DIMENSIONS
# ============================================================

dim(Unemployment)

# ============================================================
# 6. CHECK THE MONTHLY DATES
# ============================================================

Unemployment %>%
  select(month) %>%
  head(20)

head(Unemployment$month)
tail(Unemployment$month)

# ============================================================
# 7. CONVERT UNEMPLOYMENT VARIABLES TO NUMERIC
# ============================================================

Unemployment <- Unemployment %>%
  mutate(
    across(
      -month,
      as.numeric
    )
  )

str(Unemployment)

# ============================================================
# 8. CHECK MISSING VALUES
# ============================================================

missing_values <- Unemployment %>%
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
      missing_count / nrow(Unemployment) * 100
  )

missing_values

# ============================================================
# 9. CONVERT MONTH TO A PROPER DATE
# ============================================================

Unemployment <- Unemployment %>%
  mutate(
    month_date = as.Date(
      paste0(month, "-01")
    )
  )

Unemployment %>%
  select(month, month_date) %>%
  head(10)

# ============================================================
# 10. CHECK FOR DUPLICATED MONTHS
# ============================================================

Unemployment %>%
  count(month) %>%
  filter(n > 1)

sum(duplicated(Unemployment$month))

# ============================================================
# 11. CHECK THE CONTINUITY OF THE MONTHLY DATA
# ============================================================

Unemployment %>%
  arrange(month_date) %>%
  mutate(
    difference = as.numeric(
      month_date - lag(month_date)
    )
  ) %>%
  filter(
    !is.na(difference),
    difference > 31
  )

# ============================================================
# 12. CHECK THE COMPLETE MONTHLY SEQUENCE
# ============================================================

expected_months <- seq(
  from = min(Unemployment$month_date),
  to = max(Unemployment$month_date),
  by = "month"
)

missing_months <- setdiff(
  expected_months,
  Unemployment$month_date
)

missing_months

length(missing_months)

# ============================================================
# 13. CHECK THE DATE RANGE
# ============================================================

min(Unemployment$month_date)
max(Unemployment$month_date)

# ============================================================
# 14. CHECK WHETHER THE MONTHLY DATASET CONTAINS
#     ONE OBSERVATION PER MONTH
# ============================================================

Unemployment %>%
  summarise(
    number_of_observations = n(),
    number_of_unique_months = n_distinct(month_date)
  )

# ============================================================
# 15. CHECK MISSING VALUES BY MONTH
# ============================================================

Unemployment %>%
  filter(
    if_any(
      everything(),
      is.na
    )
  )

# ============================================================
# 16. SELECT FINAL COLUMNS
# ============================================================

Unemployment <- Unemployment %>%
  select(
    month_date,
    workers_short_time,
    registered_unemployed_total,
    registered_unemployed_sa,
    jobless_rate_total,
    jobless_rate_sa,
    job_vacancies_total,
    job_vacancies_sa,
    registered_job_seekers,
    labour_force
  )

# ============================================================
# 17. FINAL CHECKS
# ============================================================

head(Unemployment)
tail(Unemployment)
str(Unemployment)
summary(Unemployment)
skim(Unemployment)

sum(duplicated(Unemployment$month_date))

Unemployment %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

# ============================================================
# 18. PLOT REGISTERED UNEMPLOYMENT
# ============================================================

ggplot(
  Unemployment,
  aes(
    x = month_date,
    y = registered_unemployed_total
  )
) +
  geom_line() +
  labs(
    title = "Registered Unemployed in Switzerland",
    subtitle = "SNB – Monthly Data",
    x = "Month",
    y = "Registered unemployed"
  ) +
  theme_minimal()

# ============================================================
# 19. PLOT JOBLESS RATE
# ============================================================

ggplot(
  Unemployment,
  aes(
    x = month_date,
    y = jobless_rate_total
  )
) +
  geom_line() +
  labs(
    title = "Swiss Jobless Rate",
    subtitle = "SNB – Monthly Data",
    x = "Month",
    y = "Jobless rate (%)"
  ) +
  theme_minimal()

# ============================================================
# 20. COMPARE TOTAL AND SEASONALLY ADJUSTED UNEMPLOYMENT
# ============================================================

ggplot() +
  
  geom_line(
    data = Unemployment,
    aes(
      x = month_date,
      y = registered_unemployed_total,
      linetype = "Total"
    )
  ) +
  
  geom_line(
    data = Unemployment,
    aes(
      x = month_date,
      y = registered_unemployed_sa,
      linetype = "Seasonally adjusted"
    )
  ) +
  
  labs(
    title = "Registered Unemployment in Switzerland",
    subtitle = "Total vs. Seasonally Adjusted",
    x = "Month",
    y = "Registered unemployed",
    linetype = "Series"
  ) +
  
  theme_minimal()