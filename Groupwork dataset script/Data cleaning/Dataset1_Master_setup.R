# ============================================================
# DATASET 1 - MASTER SETUP
# Swiss REOCs + portfolios + benchmarks
# ============================================================

library(tidyverse)
library(janitor)
library(lubridate)
library(here)

# ============================================================
# 1. IMPORT REOC DATA
# ============================================================

REOC_Monthly_HP <- read_csv(
  here(
    "Groupwork dataset script",
    "Dataset 1 excel",
    "swiss_reoc_monthly_HP.csv"
  ),
  show_col_types = FALSE
) |>
  clean_names()


REOC_Yearly_FA <- read_csv(
  here(
    "Groupwork dataset script",
    "Dataset 1 excel",
    "swiss_reoc_yearly_FA.csv"
  ),
  show_col_types = FALSE
) |>
  clean_names()


# ============================================================
# 2. PREPARE MONTHLY REOC DATA
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
# 3. EQUAL-WEIGHTED PORTFOLIO
# ============================================================

portfolio_ew <- portfolio_data |>
  filter(!is.na(monthly_return)) |>
  group_by(month) |>
  summarise(
    ew_return = mean(
      monthly_return,
      na.rm = TRUE
    ),
    n_firms = n_distinct(ticker),
    .groups = "drop"
  )


# ============================================================
# 4. VALUE-WEIGHTED PORTFOLIO
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
    total_market_cap = sum(
      lag_market_cap,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ============================================================
# 5. MERGE EW + VW PORTFOLIOS
# ============================================================

portfolio <- portfolio_ew |>
  inner_join(
    portfolio_vw,
    by = "month"
  ) |>
  arrange(month) |>
  filter(
    is.finite(ew_return),
    is.finite(vw_return)
  ) |>
  mutate(
    ew_index = 100 * cumprod(
      1 + lag(
        ew_return,
        default = 0
      )
    ),
    
    vw_index = 100 * cumprod(
      1 + lag(
        vw_return,
        default = 0
      )
    )
  )


# ============================================================
# 6. IMPORT RAW BENCHMARK CSVs
# ============================================================

SPI_TR <- read_csv(
  here(
    "Groupwork dataset script",
    "Benchmarks",
    "swiss_spi_tr.csv"
  ),
  show_col_types = FALSE
) |>
  clean_names()


RE_Benchmark <- read_csv(
  here(
    "Groupwork dataset script",
    "Benchmarks",
    "swiss_re_benchmark.csv"
  ),
  show_col_types = FALSE
) |>
  clean_names()


# ============================================================
# 7. CLEAN DAILY SPI BENCHMARK
# ============================================================

Benchmark_SPI_daily <- SPI_TR |>
  slice(-(1:4)) |>
  transmute(
    date = as.Date(
      isin,
      format = "%d.%m.%Y"
    ),
    
    spi_index = as.numeric(
      ch0009987501
    )
  ) |>
  filter(
    !is.na(date),
    !is.na(spi_index)
  ) |>
  arrange(date)


# ============================================================
# 8. CLEAN DAILY REAL ESTATE BENCHMARK
# ============================================================

Benchmark_REAL_daily <- RE_Benchmark |>
  slice(-(1:4)) |>
  transmute(
    date = as.Date(
      isin,
      format = "%d.%m.%Y"
    ),
    
    real_index = as.numeric(
      ch0042660313
    )
  ) |>
  filter(
    !is.na(date),
    !is.na(real_index)
  ) |>
  arrange(date)


# ============================================================
# 9. CONVERT SPI TO MONTHLY
# ============================================================

Benchmark_SPI_monthly <- Benchmark_SPI_daily |>
  mutate(
    month = floor_date(
      date,
      "month"
    )
  ) |>
  group_by(month) |>
  slice_max(
    date,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  arrange(month) |>
  mutate(
    spi_return =
      spi_index /
      lag(spi_index) - 1,
    
    spi_log_return =
      log(
        spi_index /
          lag(spi_index)
      )
  ) |>
  select(
    month,
    spi_date = date,
    spi_index,
    spi_return,
    spi_log_return
  )


# ============================================================
# 10. CONVERT REAL TO MONTHLY
# ============================================================

Benchmark_REAL_monthly <- Benchmark_REAL_daily |>
  mutate(
    month = floor_date(
      date,
      "month"
    )
  ) |>
  group_by(month) |>
  slice_max(
    date,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  arrange(month) |>
  mutate(
    real_return =
      real_index /
      lag(real_index) - 1,
    
    real_log_return =
      log(
        real_index /
          lag(real_index)
      )
  ) |>
  select(
    month,
    real_date = date,
    real_index,
    real_return,
    real_log_return
  )


# ============================================================
# 11. COMBINE BENCHMARKS
# ============================================================

Benchmarks_monthly <- Benchmark_SPI_monthly |>
  full_join(
    Benchmark_REAL_monthly,
    by = "month"
  ) |>
  arrange(month)


# ============================================================
# 12. MERGE PORTFOLIOS + BENCHMARKS
# ============================================================

portfolio_benchmarks <- portfolio |>
  left_join(
    Benchmarks_monthly,
    by = "month"
  ) |>
  arrange(month)


# ============================================================
# 13. BASIC VALIDATION
# ============================================================

cat("\nDataset 1 loaded successfully\n")

cat(
  "\nMonthly REOC rows:",
  nrow(REOC_Monthly_HP)
)

cat(
  "\nREOC firms:",
  n_distinct(REOC_Monthly_HP$ticker)
)

cat(
  "\nPortfolio months:",
  nrow(portfolio)
)

cat(
  "\nBenchmark months:",
  nrow(Benchmarks_monthly)
)

cat("\n\nObjects created:\n")
cat(
  "REOC_Monthly_HP\n",
  "REOC_Yearly_FA\n",
  "portfolio\n",
  "Benchmarks_monthly\n",
  "portfolio_benchmarks\n"
)