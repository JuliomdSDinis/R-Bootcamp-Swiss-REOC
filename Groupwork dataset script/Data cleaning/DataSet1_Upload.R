### IMPORT DATASET 1 CSVs ###

library(tidyverse)
library(janitor)

REOC_Monthly_HP <- read_csv("Groupwork dataset script/Dataset 1 excel/swiss_reoc_monthly_HP.csv") |>
  clean_names()

REOC_Yearly_FA <- read_csv("Groupwork dataset script/Dataset 1 excel/swiss_reoc_yearly_FA.csv") |>
  clean_names()

# ============================================================
# PREPARE MONTHLY PORTFOLIO DATA
# ============================================================

portfolio_data <- REOC_Monthly_HP |>
  mutate(
    month = floor_date(date, "month")
  ) |>
  arrange(ticker, month) |>
  group_by(ticker) |>
  mutate(
    lag_market_cap = lag(market_cap)
  ) |>
  ungroup()

# ============================================================
# EQUAL-WEIGHTED PORTFOLIO
# ============================================================

portfolio_ew <- portfolio_data |>
  filter(!is.na(monthly_return)) |>
  group_by(month) |>
  summarise(
    ew_return = mean(monthly_return, na.rm = TRUE),
    n_firms = n_distinct(ticker),
    .groups = "drop"
  )

# ============================================================
# VALUE-WEIGHTED PORTFOLIO
# Uses previous month's market capitalization
# ============================================================

portfolio_vw <- portfolio_data |>
  filter(
    !is.na(monthly_return),
    !is.na(lag_market_cap),
    lag_market_cap > 0
  ) |>
  group_by(month) |>
  summarise(
    vw_return = weighted.mean(
      monthly_return,
      lag_market_cap,
      na.rm = TRUE
    ),
    total_market_cap = sum(lag_market_cap, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# FINAL MONTHLY PORTFOLIO
# ============================================================

portfolio <- portfolio_ew |>
  inner_join(portfolio_vw, by = "month") |>
  arrange(month) |>
  filter(
    is.finite(ew_return),
    is.finite(vw_return)
  )

##
# SANITY CHECK
##
portfolio |>
  count(month) |>
  filter(n > 1)

expected_months <- tibble(
  month = seq(
    min(portfolio$month),
    max(portfolio$month),
    by = "month"
  )
)

expected_months |>
  anti_join(portfolio, by = "month")

###
# REbuilding cumulative indices
###

portfolio <- portfolio |>
  mutate(
    ew_growth = cumprod(1 + ew_return),
    vw_growth = cumprod(1 + vw_return),
    
    ew_index = 100 * ew_growth / first(ew_growth),
    vw_index = 100 * vw_growth / first(vw_growth)
  )

# ============================================================
# IMPORT EXTERNAL BENCHMARKS
# ============================================================

library(tidyverse)
library(janitor)

SPI_TR <- read_csv(
  "Groupwork dataset script/Benchmarks/swiss_spi_tr.csv"
) |>
  clean_names()

RE_Benchmark <- read_csv(
  "Groupwork dataset script/Benchmarks/swiss_re_benchmark.csv"
) |>
  clean_names()

glimpse(SPI_TR)
glimpse(RE_Benchmark)

head(SPI_TR)
head(RE_Benchmark)

names(SPI_TR)
names(RE_Benchmark)

# ============================================================
# CLEAN SPI BENCHMARK
# ============================================================

Benchmark_SPI_daily <- SPI_TR |>
  slice(-(1:4)) |>
  transmute(
    date = as.Date(isin, format = "%d.%m.%Y"),
    spi_index = as.numeric(ch0009987501)
  ) |>
  filter(
    !is.na(date),
    !is.na(spi_index)
  ) |>
  arrange(date)

glimpse(Benchmark_SPI_daily)
head(Benchmark_SPI_daily)
tail(Benchmark_SPI_daily)

# ============================================================
# CLEAN REAL ESTATE BENCHMARK
# ============================================================

Benchmark_REAL_daily <- RE_Benchmark |>
  slice(-(1:4)) |>
  transmute(
    date = as.Date(isin, format = "%d.%m.%Y"),
    real_index = as.numeric(ch0042660313)
  ) |>
  filter(
    !is.na(date),
    !is.na(real_index)
  ) |>
  arrange(date)

glimpse(Benchmark_REAL_daily)
head(Benchmark_REAL_daily)
tail(Benchmark_REAL_daily)

######
# Converting daily obsevations into month
#####

library(lubridate)

Benchmark_SPI_monthly <- Benchmark_SPI_daily |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(month) |>
  mutate(
    spi_return = spi_index / lag(spi_index) - 1,
    spi_log_return = log(spi_index / lag(spi_index))
  ) |>
  select(
    month,
    spi_date = date,
    spi_index,
    spi_return,
    spi_log_return
  )

Benchmark_REAL_monthly <- Benchmark_REAL_daily |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(month) |>
  mutate(
    real_return = real_index / lag(real_index) - 1,
    real_log_return = log(real_index / lag(real_index))
  ) |>
  select(
    month,
    real_date = date,
    real_index,
    real_return,
    real_log_return
  )


####
# Combine both benchmarks
###

Benchmarks_monthly <- Benchmark_SPI_monthly |>
  full_join(
    Benchmark_REAL_monthly,
    by = "month"
  ) |>
  arrange(month)

glimpse(Benchmarks_monthly)
head(Benchmarks_monthly, 12)
tail(Benchmarks_monthly, 12)

# ============================================================
# MERGE PORTFOLIOS WITH EXTERNAL BENCHMARKS
# ============================================================

portfolio_benchmarks <- portfolio |>
  left_join(
    Benchmarks_monthly,
    by = "month"
  ) |>
  arrange(month)

glimpse(portfolio_benchmarks)

#Check if merge worked
portfolio_benchmarks |>
  select(
    month,
    ew_return,
    vw_return,
    spi_return,
    real_return
  ) |>
  tail(12)