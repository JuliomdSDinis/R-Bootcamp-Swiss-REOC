
# 1. Load packages

library(readxl)
library(tidyverse)
library(skimr)
library(janitor)

# 2. Load the dataset

Unemployment <- read_excel(
  "C:/Users/Alix/Desktop/HSLU/Data science/Programming R/R-Bootcamp-Swiss-REOC/Dataset 2 excel/SNB - Unemployment.xlsx",
  skip = 21,
  col_names = FALSE
)

# 3. Preview the dataset

head(Unemployment)

summary(Unemployment)

str(Unemployment)

skim(Unemployment)

# 4. Rename the columns

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


# Check column names

colnames(Unemployment)

# 5. Remove duplicate rows

Unemployment <- Unemployment %>%
  distinct()


# Check for duplicates

sum(duplicated(Unemployment))

# 6. Check the dimensions

dim(Unemployment)

# 7. Check the monthly dates

Unemployment %>%
  select(month) %>%
  head(20)


# First observations

head(Unemployment$month)


# Last observations

tail(Unemployment$month)

# 8. Convert unemployment variables to numeric

Unemployment <- Unemployment %>%
  mutate(
    across(
      -month,
      as.numeric
    )
  )


# Check structure

str(Unemployment)

# 9. Check missing values

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

# 10. Convert month to a proper date

Unemployment <- Unemployment %>%
  mutate(
    month_date = as.Date(
      paste0(month, "-01")
    )
  )


# Check

Unemployment %>%
  select(month, month_date) %>%
  head(10)

# 11. Check for duplicated months

Unemployment %>%
  count(month) %>%
  filter(n > 1)


sum(duplicated(Unemployment$month))

# 12. Check the continuity of the monthly data

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

# 13. Check the complete monthly sequence

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


# Number of missing months

length(missing_months)

# 14. Check the date range

min(Unemployment$month_date)

max(Unemployment$month_date)

# 15. Check whether the monthly dataset contains
#     one observation per month

Unemployment %>%
  summarise(
    number_of_observations = n(),
    number_of_unique_months = n_distinct(month_date)
  )

# 16. Check missing values by month

Unemployment %>%
  filter(
    if_any(
      everything(),
      is.na
    )
  )

# 17. Final cleaned dataset

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
# 18. Final checks

head(Unemployment)

tail(Unemployment)

str(Unemployment)

summary(Unemployment)

skim(Unemployment)


# Check duplicates

sum(duplicated(Unemployment$month_date))


# Check missing values

Unemployment %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )
# 19. Plot registered unemployment

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

# 20. Plot jobless rate

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

# 21. Compare total and seasonally adjusted unemployment

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
