# ============================================================
# LOAD SWISS NATIONAL BANK INTEREST RATES
# ============================================================

library(readxl)
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)

base_url_2000_2019 <- "https://data.snb.ch/en/topics/snb/cube/snbband?fromDate=2000-01-01"
base_url_2019_2026 <- "https://data.snb.ch/de/topics/snb/cube/snboffzisa?fromDate=2019-01&dimSel=D0(LZ)"

# ============================================================
# 1. LOAD FIRST DATASET (2000-2019)
# ============================================================

# Daily data: 2000-2019

Interest_2000_2019 <- read_excel(
  "./Dataset 2 excel/SNB - Interest Rate Y2000-Y2019.xlsx",
  skip = 15,
  col_names = FALSE
)

# ============================================================
# 2. PREVIEW FIRST DATASET
# ============================================================

head(Interest_2000_2019)
str(Interest_2000_2019)
summary(Interest_2000_2019)
skim(Interest_2000_2019)

# ============================================================
# 3. RENAME COLUMNS IN FIRST DATASET
# ============================================================

colnames(Interest_2000_2019) <- c(
  "date",
  "snb_target_lower",
  "snb_target_upper",
  "libor_3m_chf"
)

colnames(Interest_2000_2019)

# ============================================================
# 4. REMOVE DUPLICATE ROWS
# ============================================================

Interest_2000_2019 <- Interest_2000_2019 %>%
  distinct()

sum(duplicated(Interest_2000_2019))

# ============================================================
# 5. CONVERT VARIABLES TO CORRECT FORMAT
# ============================================================

Interest_2000_2019 <- Interest_2000_2019 %>%
  mutate(
    date = as.Date(date),
    across(
      -date,
      as.numeric
    )
  )

str(Interest_2000_2019)

# ============================================================
# 6. CHECK DATE RANGE
# ============================================================

min(Interest_2000_2019$date, na.rm = TRUE)
max(Interest_2000_2019$date, na.rm = TRUE)

head(Interest_2000_2019, 10)
tail(Interest_2000_2019, 10)

# ============================================================
# 7. CHECK MISSING VALUES IN FIRST DATASET
# ============================================================

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

# ============================================================
# 8. CHECK FOR DUPLICATED DATES
# ============================================================

Interest_2000_2019 %>%
  count(date) %>%
  filter(n > 1)

sum(duplicated(Interest_2000_2019$date))

# ============================================================
# 9. CREATE MONTHLY DATES
# ============================================================

Interest_2000_2019 <- Interest_2000_2019 %>%
  mutate(
    month = floor_date(
      date,
      unit = "month"
    )
  )

Interest_2000_2019 %>%
  select(date, month) %>%
  head(20)

# ============================================================
# 10. CONVERT DAILY DATA TO MONTHLY AVERAGES
# ============================================================

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

# ============================================================
# 11. PREVIEW MONTHLY DATASET
# ============================================================

head(Interest_monthly_old, 20)
tail(Interest_monthly_old, 20)
str(Interest_monthly_old)

# ============================================================
# 12. LOAD SECOND DATASET (2019-2026)
# ============================================================

# Monthly data: 2019-2026

Interest_2019_2026 <- read_excel(
  "./Dataset 2 excel/SNB - Interest Rate Y2019-Y2026.xlsx",
  skip = 23,
  col_names = FALSE
)

head(Interest_2019_2026)
tail(Interest_2019_2026)
str(Interest_2019_2026)

# ============================================================
# 13. RENAME COLUMNS IN SECOND DATASET
# ============================================================

colnames(Interest_2019_2026) <- c(
  "month",
  "snb_policy_rate"
)

colnames(Interest_2019_2026)

# ============================================================
# 14. CONVERT MONTH TO PROPER DATE FORMAT
# ============================================================

Interest_2019_2026 <- Interest_2019_2026 %>%
  mutate(
    month = as.Date(
      paste0(month, "-01")
    ),
    snb_policy_rate = as.numeric(
      snb_policy_rate
    )
  )

head(Interest_2019_2026, 20)
str(Interest_2019_2026)

# ============================================================
# 15. CHECK FOR DUPLICATE MONTHS
# ============================================================

Interest_2019_2026 %>%
  count(month) %>%
  filter(n > 1)

sum(duplicated(Interest_2019_2026$month))

# ============================================================
# 16. CHECK DATE RANGE IN SECOND DATASET
# ============================================================

min(
  Interest_2019_2026$month,
  na.rm = TRUE
)

max(
  Interest_2019_2026$month,
  na.rm = TRUE
)

head(Interest_2019_2026)
tail(Interest_2019_2026)

# ============================================================
# 17. CHECK MISSING VALUES IN SECOND DATASET
# ============================================================

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

# ============================================================
# 18. JOIN THE TWO MONTHLY DATASETS
# ============================================================

Interest_Rates <- full_join(
  Interest_monthly_old,
  Interest_2019_2026,
  by = "month"
)

Interest_Rates <- Interest_Rates %>%
  arrange(month)

# ============================================================
# 19. PREVIEW FINAL DATASET
# ============================================================

head(
  Interest_Rates,
  20
)

tail(
  Interest_Rates,
  20
)

str(Interest_Rates)

# ============================================================
# 20. INSPECT THE 2019 TRANSITION
# ============================================================

Interest_Rates %>%
  filter(
    month >= as.Date("2019-01-01"),
    month <= as.Date("2019-12-01")
  )

# ============================================================
# 21. CHECK FOR DUPLICATED MONTHS IN FINAL DATASET
# ============================================================

Interest_Rates %>%
  count(month) %>%
  filter(n > 1)

sum(
  duplicated(Interest_Rates$month)
)

# ============================================================
# 22. CHECK MONTHLY CONTINUITY
# ============================================================

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

# ============================================================
# 23. CHECK FOR MISSING MONTHS
# ============================================================

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

length(missing_months)

# ============================================================
# 24. CHECK MISSING VALUES IN FINAL DATASET
# ============================================================

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

# ============================================================
# 25. CHECK FINAL DATE RANGE
# ============================================================

min(
  Interest_Rates$month,
  na.rm = TRUE
)

max(
  Interest_Rates$month,
  na.rm = TRUE
)

# ============================================================
# 26. FINAL INSPECTION
# ============================================================

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

# ============================================================
# 27. SAVE CLEANED DATASET
# ============================================================

write_csv(
  Interest_Rates,
  "./Dataset 2 excel/SNB Interest Rates Monthly Clean.csv"
)