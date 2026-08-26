### PORTFOLIO ANALYSIS ###

library(tidyverse)
library(lubridate)

# ============================================================
# 1. EW vs VW PORTFOLIO PERFORMANCE
# ============================================================

portfolio_long <- portfolio |>
  select(month, ew_index, vw_index) |>
  pivot_longer(
    cols = c(ew_index, vw_index),
    names_to = "portfolio_type",
    values_to = "index_value"
  ) |>
  mutate(
    portfolio_type = recode(
      portfolio_type,
      ew_index = "Equal Weighted",
      vw_index = "Value Weighted"
    )
  )

ggplot(
  portfolio_long,
  aes(
    x = month,
    y = index_value,
    color = portfolio_type
  )
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Swiss Listed Real Estate Portfolio Performance",
    subtitle = "Equal-weighted vs. value-weighted portfolio",
    x = NULL,
    y = "Total Return Index (Base = 100)",
    color = "Portfolio"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# ============================================================
# 2. INDIVIDUAL FIRM PERFORMANCE
# ============================================================

firm_performance <- REOC_Monthly_HP |>
  filter(!is.na(total_return_index)) |>
  arrange(ticker, date) |>
  group_by(ticker) |>
  mutate(
    firm_index = 100 * total_return_index / first(total_return_index)
  ) |>
  ungroup()

ggplot(
  firm_performance,
  aes(
    x = date,
    y = firm_index,
    color = ticker
  )
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Performance of Swiss Listed Real Estate Companies",
    subtitle = "Each company indexed to 100 at its first observation",
    x = NULL,
    y = "Total Return Index (Base = 100)",
    color = "Company"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# ============================================================
# 3. INDIVIDUAL FIRMS + PORTFOLIO EW/VW
# ============================================================

portfolio_comparison <- portfolio |>
  select(month, ew_index, vw_index) |>
  pivot_longer(
    cols = c(ew_index, vw_index),
    names_to = "series",
    values_to = "index_value"
  ) |>
  mutate(
    series = recode(
      series,
      ew_index = "Portfolio EW",
      vw_index = "Portfolio VW"
    )
  )

firms_comparison <- firm_performance |>
  mutate(
    month = floor_date(date, "month")
  ) |>
  select(month, ticker, firm_index) |>
  rename(
    series = ticker,
    index_value = firm_index
  )

# ============================================================
# PLOT 11 FIRMS + EW/VW PORTFOLIOS
# ============================================================

ggplot() +
  
  # Individual firms - colored solid lines
  geom_line(
    data = firms_comparison,
    aes(
      x = month,
      y = index_value,
      group = series,
      color = series
    ),
    linewidth = 0.7
  ) +
  
  # EW portfolio - black dashed
  geom_line(
    data = portfolio_comparison |>
      filter(series == "Portfolio EW"),
    aes(
      x = month,
      y = index_value,
      linetype = series
    ),
    color = "black",
    linewidth = 1.1
  ) +
  
  # VW portfolio - black dashed/dotted
  geom_line(
    data = portfolio_comparison |>
      filter(series == "Portfolio VW"),
    aes(
      x = month,
      y = index_value,
      linetype = series
    ),
    color = "black",
    linewidth = 1.1
  ) +
  
  scale_linetype_manual(
    values = c(
      "Portfolio EW" = "dashed",
      "Portfolio VW" = "dotdash"
    )
  ) +
  
  labs(
    title = "Swiss Listed Real Estate Market Performance",
    subtitle = "Individual companies and EW/VW portfolios",
    x = NULL,
    y = "Total Return Index (Base = 100)",
    color = "Company",
    linetype = "Portfolio"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "bottom"
  )

# ============================================================
# MONTHLY RETURN COMPARISON DATA
# ============================================================

firm_returns_monthly <- REOC_Monthly_HP |>
  mutate(
    month = floor_date(date, "month")
  ) |>
  select(
    month,
    ticker,
    monthly_return
  ) |>
  filter(!is.na(monthly_return))

#Join Benchmarks
firm_returns_benchmarks <- firm_returns_monthly |>
  left_join(
    Benchmarks_monthly |>
      select(
        month,
        real_return,
        spi_return
      ),
    by = "month"
  )

# ============================================================
# MONTHLY RETURNS: 11 REOCs VS REAL ESTATE BENCHMARK
# From 2000 onwards
# ============================================================

plot_start <- as.Date("2000-01-01")

ggplot() +
  
  # Individual REOCs
  geom_line(
    data = firm_returns_benchmarks |>
      filter(month >= plot_start),
    aes(
      x = month,
      y = monthly_return,
      group = ticker,
      color = ticker
    ),
    linewidth = 0.45,
    alpha = 0.65
  ) +
  
  # REAL benchmark
  geom_line(
    data = Benchmarks_monthly |>
      filter(month >= plot_start),
    aes(
      x = month,
      y = real_return
    ),
    color = "black",
    linewidth = 1
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  scale_y_continuous(
    labels = scales::percent
  ) +
  
  labs(
    title = "Monthly Returns of Swiss Listed Real Estate Companies",
    subtitle = "Individual REOCs vs. SXI Real Estate Shares Broad TR",
    x = NULL,
    y = "Monthly Return",
    color = "Company"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "bottom"
  )

# ============================================================
# BENCHMARK CORRELATION ANALYSIS
# ============================================================

library(tidyverse)
library(lubridate)

analysis_start <- as.Date("2000-01-01")
analysis_end   <- as.Date("2026-04-30")


# ------------------------------------------------------------
# 1. Firm monthly returns
# ------------------------------------------------------------

firm_returns_corr <- REOC_Monthly_HP |>
  mutate(
    month = floor_date(date, "month")
  ) |>
  filter(
    month >= analysis_start,
    month <= analysis_end
  ) |>
  select(
    month,
    ticker,
    monthly_return
  )


# ------------------------------------------------------------
# 2. Add benchmark returns to every firm-month
# ------------------------------------------------------------

firm_benchmark_returns <- firm_returns_corr |>
  left_join(
    Benchmarks_monthly |>
      select(
        month,
        real_return,
        spi_return
      ),
    by = "month"
  )

glimpse(firm_benchmark_returns)

range(firm_benchmark_returns$month)

firm_benchmark_returns |>
  summarise(
    n_real = sum(!is.na(real_return)),
    n_spi  = sum(!is.na(spi_return))
  )

firm_benchmark_correlations <- firm_benchmark_returns |>
  group_by(ticker) |>
  summarise(
    observations_real = sum(
      complete.cases(monthly_return, real_return)
    ),
    
    correlation_REAL = cor(
      monthly_return,
      real_return,
      use = "complete.obs"
    ),
    
    observations_spi = sum(
      complete.cases(monthly_return, spi_return)
    ),
    
    correlation_SPI = cor(
      monthly_return,
      spi_return,
      use = "complete.obs"
    ),
    
    .groups = "drop"
  )

firm_benchmark_correlations

portfolio_benchmark_correlations <- portfolio_benchmarks |>
  filter(
    month >= analysis_start,
    month <= analysis_end
  ) |>
  summarise(
    
    EW_REAL = cor(
      ew_return,
      real_return,
      use = "complete.obs"
    ),
    
    EW_SPI = cor(
      ew_return,
      spi_return,
      use = "complete.obs"
    ),
    
    VW_REAL = cor(
      vw_return,
      real_return,
      use = "complete.obs"
    ),
    
    VW_SPI = cor(
      vw_return,
      spi_return,
      use = "complete.obs"
    )
  )

portfolio_benchmark_correlations

## ===============================
# FINAL CORRELATION TABLE
## ===============================
portfolio_corr_table <- tibble(
  ticker = c("Portfolio EW", "Portfolio VW"),
  
  correlation_REAL = c(
    portfolio_benchmark_correlations$EW_REAL,
    portfolio_benchmark_correlations$VW_REAL
  ),
  
  correlation_SPI = c(
    portfolio_benchmark_correlations$EW_SPI,
    portfolio_benchmark_correlations$VW_SPI
  )
)


benchmark_correlation_table <- firm_benchmark_correlations |>
  select(
    ticker,
    correlation_REAL,
    correlation_SPI
  ) |>
  bind_rows(portfolio_corr_table) |>
  arrange(desc(correlation_REAL))

benchmark_correlation_table
#save table
write_csv(
  benchmark_correlation_table,
  "Groupwork dataset script/Data Analysis/Outputs/benchmark_correlation_table.csv"
)

## ===============================
# Dot Plots
## ===============================

benchmark_corr_long <- benchmark_correlation_table |>
  pivot_longer(
    cols = c(
      correlation_REAL,
      correlation_SPI
    ),
    names_to = "benchmark",
    values_to = "correlation"
  ) |>
  mutate(
    benchmark = recode(
      benchmark,
      correlation_REAL = "REAL",
      correlation_SPI = "SPI"
    )
  )

ggplot(
  benchmark_corr_long,
  aes(
    x = correlation,
    y = reorder(ticker, correlation),
    color = benchmark
  )
) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  labs(
    title = "Correlation with Swiss Market Benchmarks",
    subtitle = "Monthly returns of individual REOCs and constructed portfolios",
    x = "Return Correlation",
    y = NULL,
    color = "Benchmark"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

## ===============================
# rolling correlations
## ===============================

library(slider)
rolling_correlations <- portfolio_benchmarks |>
  filter(
    month >= analysis_start,
    month <= analysis_end
  ) |>
  arrange(month) |>
  mutate(
    
    EW_REAL = slide2_dbl(
      ew_return,
      real_return,
      ~ cor(.x, .y, use = "complete.obs"),
      .before = 35,
      .complete = TRUE
    ),
    
    VW_REAL = slide2_dbl(
      vw_return,
      real_return,
      ~ cor(.x, .y, use = "complete.obs"),
      .before = 35,
      .complete = TRUE
    ),
    
    EW_SPI = slide2_dbl(
      ew_return,
      spi_return,
      ~ cor(.x, .y, use = "complete.obs"),
      .before = 35,
      .complete = TRUE
    ),
    
    VW_SPI = slide2_dbl(
      vw_return,
      spi_return,
      ~ cor(.x, .y, use = "complete.obs"),
      .before = 35,
      .complete = TRUE
    )
  )

rolling_corr_long <- rolling_correlations |>
  select(
    month,
    EW_REAL,
    VW_REAL,
    EW_SPI,
    VW_SPI
  ) |>
  pivot_longer(
    cols = -month,
    names_to = "series",
    values_to = "correlation"
  )

## PLOT

ggplot(
  rolling_corr_long,
  aes(
    x = month,
    y = correlation,
    color = series
  )
) +
  geom_line(linewidth = 0.8) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Rolling Correlation with Swiss Market Benchmarks",
    subtitle = "36-month rolling correlations of EW and VW portfolio returns",
    x = NULL,
    y = "36-Month Correlation",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# ============================================================
# SAVE CORRELATION TABLES
# ============================================================

dir.create(
  "Groupwork dataset script/Data Analysis/Outputs/Tables",
  recursive = TRUE,
  showWarnings = FALSE
)
write_csv(
  benchmark_correlation_table,
  "Groupwork dataset script/Data Analysis/Outputs/Tables/benchmark_correlation_table.csv"
)

write_csv(
  firm_benchmark_correlations,
  "Groupwork dataset script/Data Analysis/Outputs/Tables/firm_benchmark_correlations.csv"
)

# ============================================================
# DEFINE MACROECONOMIC REGIMES
# Same methodology as previous project
# ============================================================

portfolio_regimes <- portfolio_benchmarks |>
  filter(
    month >= as.Date("2000-01-01"),
    month <= as.Date("2026-04-01")
  ) |>
  mutate(
    regime = case_when(
      
      month >= as.Date("2000-01-01") &
        month <= as.Date("2007-12-31") ~
        "Pre-GFC Expansion",
      
      month >= as.Date("2008-01-01") &
        month <= as.Date("2009-12-31") ~
        "Global Financial Crisis",
      
      month >= as.Date("2010-01-01") &
        month <= as.Date("2019-12-31") ~
        "Low-Rate Expansion",
      
      month >= as.Date("2020-01-01") &
        month <= as.Date("2021-12-31") ~
        "COVID Shock",
      
      month >= as.Date("2022-01-01") ~
        "Rate Shock / Tightening",
      
      TRUE ~ NA_character_
    )
  )

portfolio_regimes |>
  count(regime)

# ============================================================
# CORRELATIONS BY MACROECONOMIC REGIME
# ============================================================

regime_correlations <- portfolio_regimes |>
  filter(!is.na(regime)) |>
  group_by(regime) |>
  summarise(
    
    observations = n(),
    
    EW_REAL = cor(
      ew_return,
      real_return,
      use = "complete.obs"
    ),
    
    VW_REAL = cor(
      vw_return,
      real_return,
      use = "complete.obs"
    ),
    
    EW_SPI = cor(
      ew_return,
      spi_return,
      use = "complete.obs"
    ),
    
    VW_SPI = cor(
      vw_return,
      spi_return,
      use = "complete.obs"
    ),
    
    .groups = "drop"
  )

# ============================================================
# ordered table
# ============================================================

regime_correlations <- regime_correlations |>
  mutate(
    regime = factor(
      regime,
      levels = c(
        "Pre-GFC Expansion",
        "Global Financial Crisis",
        "Low-Rate Expansion",
        "COVID Shock",
        "Rate Shock / Tightening"
      )
    )
  ) |>
  arrange(regime)

regime_correlations

regime_correlation_table <- regime_correlations |>
  mutate(
    across(
      c(EW_REAL, VW_REAL, EW_SPI, VW_SPI),
      ~ round(.x, 2)
    )
  )

regime_correlation_table

#####
# HEATMAP
#####

regime_correlations_long <- regime_correlations |>
  select(
    regime,
    EW_REAL,
    VW_REAL,
    EW_SPI,
    VW_SPI
  ) |>
  pivot_longer(
    cols = -regime,
    names_to = "relationship",
    values_to = "correlation"
  ) |>
  mutate(
    relationship = recode(
      relationship,
      EW_REAL = "EW – REAL",
      VW_REAL = "VW – REAL",
      EW_SPI = "EW – SPI",
      VW_SPI = "VW – SPI"
    )
  )

ggplot(
  regime_correlations_long,
  aes(
    x = relationship,
    y = regime,
    fill = correlation
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = round(correlation, 2)),
    size = 4
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Portfolio Correlations Across Macroeconomic Regimes",
    subtitle = "EW and VW portfolios vs. Swiss real estate and equity benchmarks",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )


###
# Firm x regime corelation heatmap against REAL
###

firm_regime_correlations <- firm_benchmark_returns |>
  mutate(
    regime = case_when(
      month >= as.Date("2000-01-01") &
        month <= as.Date("2007-12-31") ~ "Pre-GFC Expansion",
      
      month >= as.Date("2008-01-01") &
        month <= as.Date("2009-12-31") ~ "Global Financial Crisis",
      
      month >= as.Date("2010-01-01") &
        month <= as.Date("2019-12-31") ~ "Low-Rate Expansion",
      
      month >= as.Date("2020-01-01") &
        month <= as.Date("2021-12-31") ~ "COVID Shock",
      
      month >= as.Date("2022-01-01") ~ "Rate Shock / Tightening",
      
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(regime)) |>
  group_by(ticker, regime) |>
  summarise(
    observations = sum(
      complete.cases(monthly_return, real_return)
    ),
    
    correlation_REAL = if_else(
      observations >= 12,
      cor(
        monthly_return,
        real_return,
        use = "complete.obs"
      ),
      NA_real_
    ),
    
    .groups = "drop"
  )

# ordered regimes

firm_regime_correlations <- firm_regime_correlations |>
  mutate(
    regime = factor(
      regime,
      levels = c(
        "Pre-GFC Expansion",
        "Global Financial Crisis",
        "Low-Rate Expansion",
        "COVID Shock",
        "Rate Shock / Tightening"
      )
    )
  )

# Heatmap

ggplot(
  firm_regime_correlations,
  aes(
    x = regime,
    y = ticker,
    fill = correlation_REAL
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  geom_text(
    aes(
      label = ifelse(
        is.na(correlation_REAL),
        "",
        round(correlation_REAL, 2)
      )
    ),
    size = 3.5
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "grey90"
  ) +
  labs(
    title = "Firm Correlations with the Swiss Real Estate Benchmark",
    subtitle = "Monthly return correlations across macroeconomic regimes",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )


# ============================================================
# EVOLUTION OF MARKET CONCENTRATION
# Top-3 concentration and HHI
# ============================================================

concentration_evolution <- concentration_yearly |>
  select(
    year,
    top_3_share,
    hhi
  ) |>
  pivot_longer(
    cols = c(top_3_share, hhi),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = recode(
      measure,
      top_3_share = "Top-3 Concentration",
      hhi = "HHI"
    )
  )

ggplot(
  concentration_evolution,
  aes(
    x = year,
    y = value,
    color = measure
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Evolution of Market Concentration",
    subtitle = "Large-cap Swiss listed real estate companies in the sample",
    x = NULL,
    y = "Concentration",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# ============================================================
# FIND COMMON SAMPLE PERIOD FOR ALL 11 FIRMS
# ============================================================

common_sample_check <- market_weights_monthly |>
  group_by(month) |>
  summarise(
    n_firms = n_distinct(ticker),
    .groups = "drop"
  )

common_sample_check |>
  filter(n_firms == 11) |>
  summarise(
    first_common_month = min(month),
    last_common_month = max(month),
    months = n()
  )

### avg size through common period
common_start <- common_sample_check |>
  filter(n_firms == 11) |>
  summarise(first_month = min(month)) |>
  pull(first_month)

common_end <- common_sample_check |>
  filter(n_firms == 11) |>
  summarise(last_month = max(month)) |>
  pull(last_month)

common_start
common_end

firm_average_size <- market_weights_monthly |>
  filter(
    month >= common_start,
    month <= common_end
  ) |>
  group_by(ticker) |>
  summarise(
    average_market_weight = mean(
      market_weight,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

firm_average_size |>
  arrange(desc(average_market_weight))

firm_REAL_common <- firm_benchmark_returns |>
  filter(
    month >= common_start,
    month <= common_end
  ) |>
  group_by(ticker) |>
  summarise(
    observations = sum(
      complete.cases(
        monthly_return,
        real_return
      )
    ),
    
    
    ### SCATERPLOT
    
    ggplot(
      size_correlation_analysis,
      aes(
        x = average_market_weight,
        y = correlation_REAL
      )
    ) +
      geom_point(size = 3) +
      geom_text(
        aes(label = ticker),
        nudge_y = 0.025,
        size = 3.5
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        linewidth = 0.8
      ) +
      scale_x_continuous(
        labels = scales::percent_format(accuracy = 1)
      ) +
      scale_y_continuous(
        limits = c(0, 1)
      ) +
      labs(
        title = "Firm Size and Real Estate Benchmark Correlation",
        subtitle = "Average sample market weight vs. correlation with REAL over the common sample period",
        x = "Average Share of Sample Market Capitalization",
        y = "Correlation with REAL"
      ) +
      theme_minimal()
    correlation_REAL = cor(
      monthly_return,
      real_return,
      use = "complete.obs"
    ),
    
    .groups = "drop"
  )

size_correlation_analysis <- firm_average_size |>
  left_join(
    firm_REAL_common,
    by = "ticker"
  )

size_correlation_analysis |>
  arrange(desc(average_market_weight))

## Quantify the relationship
cor(
  size_correlation_analysis$average_market_weight,
  size_correlation_analysis$correlation_REAL,
  use = "complete.obs"
)

cor(
  size_correlation_analysis$average_market_weight,
  size_correlation_analysis$correlation_REAL,
  method = "spearman",
  use = "complete.obs"
)