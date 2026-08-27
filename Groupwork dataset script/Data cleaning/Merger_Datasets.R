# ============================================================
# MERGE DATASET 1 WITH MACRO DATA
# HP MONTHLY PANEL + FA YEARLY PANEL
# ============================================================

library(tidyverse)
library(janitor)
library(lubridate)

# ============================================================
# 1. IMPORT DATASET 1
# ============================================================

# Yearly Financial Accounting data
fa <- read_csv(
  "Groupwork dataset script/Dataset 1 excel/swiss_reoc_yearly_FA.csv",
  show_col_types = FALSE
) |>
  clean_names()

# Monthly Historical Prices / Returns data
hp <- read_csv(
  "Groupwork dataset script/Dataset 1 excel/swiss_reoc_monthly_HP.csv",
  show_col_types = FALSE
) |>
  clean_names()


# ============================================================
# 2. IMPORT MACRO DATA
# ============================================================
#

macro_monthly <- read_csv(
  "Groupwork dataset script/Dataset 2 excel/macro_monthly_2000.csv",
  show_col_types = FALSE
) |>
  clean_names()

macro_yearly <- read_csv(
  "Groupwork dataset script/Dataset 2 excel/macro_yearly_2000.csv",
  show_col_types = FALSE
) |>
  clean_names()

# ============================================================
# 2.1 INSPECT DATASETS BEFORE MERGING
# ============================================================

# Financial Accounting panel
glimpse(fa)

# Historical Prices panel
glimpse(hp)

# Macro datasets
glimpse(macro_monthly)
glimpse(macro_yearly)

# ============================================================
# 3. PREPARE JOIN KEYS
# ============================================================

# ------------------------------------------------------------
# 3.1 FA YEARLY PANEL
# ------------------------------------------------------------

# Ensure year has the same type in both datasets
fa <- fa |>
  mutate(
    year = as.integer(year)
  )

macro_yearly <- macro_yearly |>
  mutate(
    year = as.integer(year)
  )


# ------------------------------------------------------------
# 3.2 HP MONTHLY PANEL
# ------------------------------------------------------------

# HP dates are month-end trading dates.
# Convert them to the first day of the corresponding month
# to match macro_monthly.

hp <- hp |>
  mutate(
    month = floor_date(date, unit = "month")
  )

# Check
hp |>
  select(
    ticker,
    date,
    month
  ) |>
  head(12)

# ============================================================
# 4. CHECK JOIN KEYS BEFORE MERGING
# ============================================================

# Macro monthly should have one row per month
macro_monthly |>
  count(month) |>
  filter(n > 1)

# Macro yearly should have one row per year
macro_yearly |>
  count(year) |>
  filter(n > 1)

# FA should have maximum one observation per company-year
fa |>
  count(ticker, year) |>
  filter(n > 1)

# HP should have maximum one observation per company-month
hp |>
  count(ticker, month) |>
  filter(n > 1)

# ============================================================
# 5. MERGE FA WITH YEARLY MACRO DATA
# ============================================================

fa_macro <- fa |>
  left_join(
    macro_yearly,
    by = "year"
  )

# Check dimensions
dim(fa)
dim(fa_macro)

glimpse(fa_macro)

# ============================================================
# 6. MERGE HP WITH MONTHLY MACRO DATA
# ============================================================

hp_macro <- hp |>
  left_join(
    macro_monthly,
    by = "month"
  )

# Check dimensions
dim(hp)
dim(hp_macro)

glimpse(hp_macro)

# ============================================================
# 7. VALIDATE MERGED PANELS
# ============================================================

# ------------------------------------------------------------
# FA panel
# ------------------------------------------------------------

fa_macro |>
  summarise(
    observations = n(),
    companies = n_distinct(ticker),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE)
  )


# ------------------------------------------------------------
# HP panel
# ------------------------------------------------------------

hp_macro |>
  summarise(
    observations = n(),
    companies = n_distinct(ticker),
    first_month = min(month, na.rm = TRUE),
    last_month = max(month, na.rm = TRUE)
  )

# ============================================================
# 8. CHECK MACRO MATCHING
# ============================================================

# Yearly panel
fa_macro |>
  select(
    ticker,
    year,
    debt_to_assets,
    net_income,
    libor_3m_chf,
    confederation_10y,
    jobless_rate_sa,
    sfso_inflation_according_to_the_national_consumer_price_index,
    gdp
  ) |>
  head(20)


# Monthly panel
hp_macro |>
  select(
    ticker,
    date,
    month,
    monthly_return,
    libor_3m_chf,
    confederation_10y,
    jobless_rate_sa,
    sfso_inflation_according_to_the_national_consumer_price_index
  ) |>
  head(20)

# ============================================================
# 9. SAVE FINAL MERGED DATASETS
# ============================================================

write_csv(
  hp_macro,
  "Groupwork dataset script/Data cleaning/swiss_reoc_monthly_HP_macro.csv.csv"
)

write_csv(
  fa_macro,
  "Groupwork dataset script/Data cleaning/swiss_reoc_yearly_FA_macro.csv"
)