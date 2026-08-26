# 1. Load packages

library(readxl)
library(tidyverse)
library(skimr)
library(janitor)

# 2. Load the dataset

Yields <- read_excel(
  "./Dataset 2 excel/SNB - Yields on bond issues.xlsx",
  skip = 19,
  col_names = FALSE
)

# 3. Preview the dataset

head(Yields)

summary(Yields)

str(Yields)

skim(Yields)

# 4. Rename the columns

colnames(Yields) <- c(
  "date",
  "confederation_5y",
  "confederation_8y",
  "confederation_10y",
  "confederation_bonds",
  "cantons",
  "mortgage_bond_institutions",
  "commercial_banks",
  "other_banks",
  "AAA",
  "AA",
  "A"
)

# Check column names

colnames(Yields)

# 5. Remove duplicate rows

Yields <- Yields %>%
  distinct()


# Check for duplicates

sum(duplicated(Yields))

# 6. Check the dimensions

dim(Yields)

# 7. Check the dates

Yields %>%
  select(date) %>%
  head(20)


# First observation

head(Yields$date)


# Last observation

tail(Yields$date)

# 8. Convert yield variables to numeric

Yields <- Yields %>%
  mutate(
    across(
      -date,
      as.numeric
    )
  )


# Check structure

str(Yields)

# 9. Check missing values

missing_values <- Yields %>%
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
      missing_count / nrow(Yields) * 100
  )


missing_values

# 10. Convert date to a proper date

Yields <- Yields %>%
  mutate(
    date = as.Date(date)
  )


# Check

Yields %>%
  select(date) %>%
  head(10)


# Check structure

str(Yields)

# 11. Check for duplicated dates

Yields %>%
  count(date) %>%
  filter(n > 1)


sum(duplicated(Yields$date))

# 12. Check continuity of the daily data

Yields %>%
  arrange(date) %>%
  mutate(
    difference = as.numeric(
      date - lag(date)
    )
  ) %>%
  filter(
    !is.na(difference),
    difference > 7
  )

# 13. Create monthly dates

Yields <- Yields %>%
  mutate(
    month = floor_date(
      date,
      unit = "month"
    )
  )


# Check

Yields %>%
  select(date, month) %>%
  head(20)

# 14. Convert daily data to monthly data

# We calculate the monthly average of the daily yields.

# na.rm = TRUE means that missing daily observations
# are ignored when calculating the monthly average.


Yields <- Yields %>%
  group_by(month) %>%
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# 15. Preview the monthly dataset

head(Yields, 20)

tail(Yields, 20)

str(Yields)

# 16. Check missing values again

missing_monthly <- Yields %>%
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
      missing_count / nrow(Yields) * 100
  )


missing_monthly

# 17. Check duplicated months

sum(duplicated(Yields$month))

# 18. Check monthly continuity

Yields %>%
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

# 19. Check for missing months

Yields <- Yields %>%
  mutate(
    month = as.Date(month)
  )


expected_months <- seq.Date(
  from = min(Yields$month, na.rm = TRUE),
  to = max(Yields$month, na.rm = TRUE),
  by = "month"
)


missing_months <- setdiff(
  expected_months,
  Yields$month
)


missing_months


# Number of missing months

length(missing_months)

# 20. Check the date range

min(Yields$month)

max(Yields$month)

# 21. Final structure

head(Yields)

tail(Yields)

str(Yields)

summary(Yields)

skim(Yields)

# 22. Plot the 10-year Confederation bond yield

ggplot(
  Yields,
  aes(
    x = month,
    y = confederation_10y
  )
) +
  geom_line() +
  labs(
    title = "10-Year Swiss Confederation Bond Yield",
    subtitle = "SNB – Monthly Average",
    x = "Month",
    y = "Yield (%)"
  ) +
  theme_minimal()

# 23. Plot different Confederation bond maturities

ggplot(
  Yields,
  aes(
    x = month
  )
) +
  
  geom_line(
    aes(
      y = confederation_5y,
      linetype = "5 years"
    )
  ) +
  
  geom_line(
    aes(
      y = confederation_8y,
      linetype = "8 years"
    )
  ) +
  
  geom_line(
    aes(
      y = confederation_10y,
      linetype = "10 years"
    )
  ) +
  
  labs(
    title = "Swiss Confederation Bond Yields",
    subtitle = "Monthly Average",
    x = "Month",
    y = "Yield (%)",
    linetype = "Maturity"
  ) +
  
  theme_minimal()