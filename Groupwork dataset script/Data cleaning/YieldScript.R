# ============================================================
# LOAD SWISS BOND YIELDS DATA FROM SNB API
# ============================================================

library(readxl)
library(tidyverse)
library(skimr)
library(janitor)

base_url <- "https://data.snb.ch/en/topics/ziredev/cube/rendoblid?fromDate=2000-01-01&dimSel=D0(5J,8J,10J0,E,K,P,GK,IKH,AAA,AA,A)"

# ============================================================
# 1. LOAD THE DATASET
# ============================================================

Yields <- read_excel(
  "./Dataset 2 excel/SNB - Yields on bond issues.xlsx",
  skip = 19,
  col_names = FALSE
)

# ============================================================
# 2. PREVIEW THE DATASET
# ============================================================

head(Yields)
summary(Yields)
str(Yields)
skim(Yields)

# ============================================================
# 3. RENAME THE COLUMNS
# ============================================================

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

colnames(Yields)

# ============================================================
# 4. REMOVE DUPLICATE ROWS
# ============================================================

Yields <- Yields %>%
  distinct()

sum(duplicated(Yields))

# ============================================================
# 5. CHECK THE DIMENSIONS
# ============================================================

dim(Yields)

# ============================================================
# 6. CHECK THE DATES
# ============================================================

Yields %>%
  select(date) %>%
  head(20)

head(Yields$date)
tail(Yields$date)

# ============================================================
# 7. CONVERT YIELD VARIABLES TO NUMERIC
# ============================================================

Yields <- Yields %>%
  mutate(
    across(
      -date,
      as.numeric
    )
  )

str(Yields)

# ============================================================
# 8. CHECK MISSING VALUES
# ============================================================

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

# ============================================================
# 9. CONVERT DATE TO A PROPER DATE
# ============================================================

Yields <- Yields %>%
  mutate(
    date = as.Date(date)
  )

Yields %>%
  select(date) %>%
  head(10)

str(Yields)

# ============================================================
# 10. CHECK FOR DUPLICATED DATES
# ============================================================

Yields %>%
  count(date) %>%
  filter(n > 1)

sum(duplicated(Yields$date))

# ============================================================
# 11. CHECK CONTINUITY OF THE DAILY DATA
# ============================================================

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

# ============================================================
# 12. CREATE MONTHLY DATES
# ============================================================

Yields <- Yields %>%
  mutate(
    month = floor_date(
      date,
      unit = "month"
    )
  )

Yields %>%
  select(date, month) %>%
  head(20)

# ============================================================
# 13. CONVERT DAILY DATA TO MONTHLY AVERAGES
# ============================================================

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

# ============================================================
# 14. PREVIEW THE MONTHLY DATASET
# ============================================================

head(Yields, 20)
tail(Yields, 20)
str(Yields)

# ============================================================
# 15. CHECK MISSING VALUES AGAIN
# ============================================================

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

# ============================================================
# 16. CHECK FOR DUPLICATED MONTHS
# ============================================================

sum(duplicated(Yields$month))

# ============================================================
# 17. CHECK MONTHLY CONTINUITY
# ============================================================

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

# ============================================================
# 18. CHECK FOR MISSING MONTHS
# ============================================================

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

length(missing_months)

# ============================================================
# 19. CHECK THE DATE RANGE
# ============================================================

min(Yields$month)
max(Yields$month)

# ============================================================
# 20. FINAL STRUCTURE
# ============================================================

head(Yields)
tail(Yields)
str(Yields)
summary(Yields)
skim(Yields)

# ============================================================
# 21. PLOT THE 10-YEAR CONFEDERATION BOND YIELD
# ============================================================

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

# ============================================================
# 22. PLOT DIFFERENT CONFEDERATION BOND MATURITIES
# ============================================================

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