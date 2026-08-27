# 1. Load packages

library(readxl)
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)

# 2. Load the first dataset
#    Daily data: 2000-2019

Interest_2000_2019 <- read_excel(
  "./Dataset 2 excel/SNB - Interest Rate Y2000-Y2019.xlsx",
  skip = 15,
  col_names = FALSE
)


# Preview

head(Interest_2000_2019)

str(Interest_2000_2019)

summary(Interest_2000_2019)

skim(Interest_2000_2019)

# 3. Rename the first dataset

colnames(Interest_2000_2019) <- c(
  "date",
  "snb_target_lower",
  "snb_target_upper",
  "libor_3m_chf"
)


# Check column names

colnames(Interest_2000_2019)

# 4. Remove duplicate rows

Interest_2000_2019 <- Interest_2000_2019 %>%
  distinct()


# Check duplicates

sum(duplicated(Interest_2000_2019))

# 5. Convert variables to the correct format

Interest_2000_2019 <- Interest_2000_2019 %>%
  mutate(
    date = as.Date(date),
    across(
      -date,
      as.numeric
    )
  )


# Check structure

str(Interest_2000_2019)


# 6. Check date range


min(Interest_2000_2019$date, na.rm = TRUE)

max(Interest_2000_2019$date, na.rm = TRUE)


# First observations

head(Interest_2000_2019, 10)


# Last observations

tail(Interest_2000_2019, 10)


# 7. Check missing values

missing_2000_2019 <- Interest_2000_2019 %>%
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
      missing_count / nrow(Interest_2000_2019) * 100
  )


missing_2000_2019

# 8. Check duplicated dates

Interest_2000_2019 %>%
  count(date) %>%
  filter(n > 1)


sum(duplicated(Interest_2000_2019$date))

# 9. Create monthly dates

Interest_2000_2019 <- Interest_2000_2019 %>%
  mutate(
    month = floor_date(
      date,
      unit = "month"
    )
  )


# Check

Interest_2000_2019 %>%
  select(date, month) %>%
  head(20)

# 10. Convert daily data to monthly averages

Interest_monthly_old <- Interest_2000_2019 %>%
  group_by(month) %>%
  summarise(
    
    snb_target_lower =
      mean(
        snb_target_lower,
        na.rm = TRUE
      ),
    
    snb_target_upper =
      mean(
        snb_target_upper,
        na.rm = TRUE
      ),
    
    libor_3m_chf =
      mean(
        libor_3m_chf,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

# 11. Preview monthly dataset

head(Interest_monthly_old, 20)

tail(Interest_monthly_old, 20)

str(Interest_monthly_old)

# 12. Load second dataset
#     Monthly data: 2019-2026

Interest_2019_2026 <- read_excel(
  "./Dataset 2 excel/SNB - Interest Rate Y2019-Y2026.xlsx",
  skip = 23,
  col_names = FALSE
)


# Preview

head(Interest_2019_2026)

tail(Interest_2019_2026)

str(Interest_2019_2026)

# 13. Rename columns

colnames(Interest_2019_2026) <- c(
  "month",
  "snb_policy_rate"
)


# Check column names

colnames(Interest_2019_2026)

# 14. Convert month to proper Date

Interest_2019_2026 <- Interest_2019_2026 %>%
  mutate(
    month = as.Date(
      paste0(month, "-01")
    ),
    snb_policy_rate = as.numeric(
      snb_policy_rate
    )
  )


# Check

head(Interest_2019_2026, 20)

str(Interest_2019_2026)

# 15. Convert month to proper Date

Interest_2019_2026 <- Interest_2019_2026 %>%
  mutate(
    month = as.Date(
      paste0(month, "-01")
    ),
    
    snb_policy_rate = as.numeric(
      snb_policy_rate
    )
  )


# Check

head(Interest_2019_2026, 20)

str(Interest_2019_2026)

# 16. Check duplicate months

Interest_2019_2026 %>%
  count(month) %>%
  filter(n > 1)


sum(duplicated(Interest_2019_2026$month))

# 17. Check date range

min(
  Interest_2019_2026$month,
  na.rm = TRUE
)

max(
  Interest_2019_2026$month,
  na.rm = TRUE
)


# Preview

head(Interest_2019_2026)

tail(Interest_2019_2026)

# 18. Check missing values

missing_2019_2026 <- Interest_2019_2026 %>%
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
      missing_count / nrow(Interest_2019_2026) * 100
  )


missing_2019_2026

# 19. Join the two monthly datasets

Interest_Rates <- full_join(
  Interest_monthly_old,
  Interest_2019_2026,
  by = "month"
)


# Sort by month

Interest_Rates <- Interest_Rates %>%
  arrange(month)

# 20. Preview final dataset

head(
  Interest_Rates,
  20
)

tail(
  Interest_Rates,
  20
)


# Structure

str(Interest_Rates)

# 21. Inspect the 2019 transition

Interest_Rates %>%
  filter(
    month >= as.Date("2019-01-01"),
    month <= as.Date("2019-12-01")
  )

# 22. Check duplicated months

Interest_Rates %>%
  count(month) %>%
  filter(n > 1)


sum(
  duplicated(Interest_Rates$month)
)

# 23. Check monthly continuity

Interest_Rates %>%
  arrange(month) %>%
  mutate(
    difference = as.numeric(
      month - lag(month)
    )
  ) %>%
  summarise(
    minimum_difference =
      min(
        difference,
        na.rm = TRUE
      ),
    
    maximum_difference =
      max(
        difference,
        na.rm = TRUE
      )
  )

# 24. Check for missing months

expected_months <- seq.Date(
  from = min(
    Interest_Rates$month,
    na.rm = TRUE
  ),
  
  to = max(
    Interest_Rates$month,
    na.rm = TRUE
  ),
  
  by = "month"
)


missing_months <- setdiff(
  expected_months,
  Interest_Rates$month
)


missing_months


# Number of missing months

length(missing_months)

# 25. Missing values in final dataset

missing_final <- Interest_Rates %>%
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
      missing_count / nrow(Interest_Rates) * 100
  )


missing_final

# 26. Final date range

min(
  Interest_Rates$month,
  na.rm = TRUE
)

max(
  Interest_Rates$month,
  na.rm = TRUE
)

# 27. Final inspection

head(
  Interest_Rates,
  20
)

tail(
  Interest_Rates,
  20
)

summary(
  Interest_Rates
)

skim(
  Interest_Rates
)

# 28. Save cleaned dataset

write_csv(
  Interest_Rates,
  "./Dataset 2 excel/SNB Interest Rates Monthly Clean.csv"
)
