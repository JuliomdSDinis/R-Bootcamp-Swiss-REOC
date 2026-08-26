#############################################
####  DESCRIPTIVE STATS FOR EACH COMPANY  ###
#############################################

#FOR EACH COMPANY
firm_stats <- REOC_Monthly_HP |>
  group_by(ticker) |>
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    observations = sum(!is.na(monthly_return)),
    
    mean_monthly_return = mean(monthly_return, na.rm = TRUE),
    
    annualized_return =
      (prod(1 + monthly_return, na.rm = TRUE)^(12 / sum(!is.na(monthly_return)))) - 1,
    
    annualized_volatility =
      sd(monthly_return, na.rm = TRUE) * sqrt(12),
    
    min_monthly_return = min(monthly_return, na.rm = TRUE),
    max_monthly_return = max(monthly_return, na.rm = TRUE)
  )

firm_stats |>
  mutate(
    across(
      c(
        mean_monthly_return,
        annualized_return,
        annualized_volatility,
        min_monthly_return,
        max_monthly_return
      ),
      ~ round(.x * 100, 2)
    )
  )

#SAVING TABLE
firm_stats_table <- firm_stats |>
  mutate(
    across(
      c(
        mean_monthly_return,
        annualized_return,
        annualized_volatility,
        min_monthly_return,
        max_monthly_return
      ),
      ~ round(.x * 100, 2)
    )
  )

write_csv(
  firm_stats_table,
  "Groupwork dataset script/Data Analysis/Output/firm_descriptive_statistics_percent.csv"
)

#PLOT: RETURN VS RISK
ggplot(
  firm_stats,
  aes(
    x = annualized_volatility,
    y = annualized_return,
    label = ticker
  )
) +
  geom_point(size = 3) +
  geom_text(
    nudge_y = 0.01,
    check_overlap = TRUE
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Risk-Return Profile of Swiss Listed Real Estate Companies",
    x = "Annualized Volatility",
    y = "Annualized Return"
  ) +
  theme_minimal()

# ============================================================
# MARKET CAPITALIZATION ANALYSIS
# ============================================================

latest_market_cap <- REOC_Monthly_HP |>
  filter(!is.na(market_cap)) |>
  group_by(ticker) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ticker, date, market_cap) |>
  arrange(desc(market_cap))

latest_market_cap

#Calculating weights based on the latest market cap
latest_market_cap <- latest_market_cap |>
  mutate(
    market_weight = market_cap / sum(market_cap)
  )
latest_market_cap
sum(latest_market_cap$market_weight)

#plot market weights
ggplot(
  latest_market_cap,
  aes(
    x = reorder(ticker, market_weight),
    y = market_weight
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Market Weights of Swiss Listed Real Estate Companies",
    subtitle = "Based on latest available market capitalization",
    x = NULL,
    y = "Share of Sample Market Capitalization"
  ) +
  theme_minimal()

#Calculate concentration
market_concentration <- latest_market_cap |>
  arrange(desc(market_weight)) |>
  summarise(
    largest_firm = max(market_weight),
    top_3_share = sum(head(market_weight, 3)),
    top_5_share = sum(head(market_weight, 5)),
    hhi = sum(market_weight^2)
  )

market_concentration

#SAVING TABLE
market_concentration_table <- market_concentration |>
  mutate(
    largest_firm = round(largest_firm * 100, 2),
    top_3_share = round(top_3_share * 100, 2),
    top_5_share = round(top_5_share * 100, 2),
    hhi = round(hhi, 3)
  )

write_csv(
  market_concentration_table,
  "Groupwork dataset script/Data Analysis/Output/market_concentration_table.csv"
)

# ============================================================
# MARKET weights over TIME
# ============================================================
library(tidyverse)
library(lubridate)

market_weights_monthly <- REOC_Monthly_HP |>
  filter(
    !is.na(market_cap),
    market_cap > 0
  ) |>
  mutate(
    month = floor_date(date, "month")
  ) |>
  group_by(month, ticker) |>
  summarise(
    market_cap = last(market_cap),
    .groups = "drop"
  ) |>
  group_by(month) |>
  mutate(
    market_weight = market_cap / sum(market_cap)
  ) |>
  ungroup()

# 1. Every month should sum to 100%
market_weights_monthly |>
  group_by(month) |>
  summarise(
    total_weight = sum(market_weight),
    n_firms = n()
  ) |>
  arrange(desc(total_weight)) |>
  print(n = 30)


# 2. There should be no duplicate firm-month observations
market_weights_monthly |>
  count(month, ticker) |>
  filter(n > 1)


# 3. Inspect the number of firms through time
market_weights_monthly |>
  group_by(month) |>
  summarise(n_firms = n_distinct(ticker)) |>
  count(n_firms)


# MARKET WEIGHT EVOLUTION PLOT

ggplot(
  market_weights_monthly,
  aes(
    x = month,
    y = market_weight,
    fill = ticker
  )
) +
  geom_area() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Evolution of Market Weights",
    subtitle = "Large-cap Swiss listed real estate companies in the sample",
    x = NULL,
    y = "Share of Sample Market Capitalization",
    fill = "Company"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# ============================================================
# FINAL YEARLY MARKET WEIGHT & CONCENTRATION TABLE
# ============================================================

# Concentration measures by year
concentration_yearly <- market_weights_yearly |>
  group_by(year) |>
  arrange(desc(market_weight), .by_group = TRUE) |>
  summarise(
    n_firms = n_distinct(ticker),
    top_3_share = sum(head(market_weight, 3)),
    hhi = sum(market_weight^2),
    .groups = "drop"
  )


# Individual company market weights
weights_yearly_wide <- market_weights_yearly |>
  select(year, ticker, market_weight) |>
  pivot_wider(
    names_from = ticker,
    values_from = market_weight
  )


# Combine weights + concentration measures
market_concentration_table <- weights_yearly_wide |>
  left_join(concentration_yearly, by = "year") |>
  relocate(
    n_firms,
    hhi,
    top_3_share,
    .after = year
  ) |>
  arrange(year)


#display your display formatting:
market_concentration_table_display <- market_concentration_table |>
  mutate(
    across(
      c(ALLN, CHAM, EPIC, HIAG, IREN, ISN,
        MOBN, PSPN, SPSN, ZUGEST, ZUGN),
      ~ round(.x * 100, 1)
    ),
    hhi = round(hhi, 3),
    top_3_share = round(top_3_share * 100, 1)
  )

View(market_concentration_table_display)

#Saving the table
write_csv(
  market_concentration_table_display,
  "Groupwork dataset script/Data Analysis/market_weights_concentration_yearly.csv"
)
# ============================================================
# CORRELATION ANALYSIS
# ============================================================

returns_wide <- REOC_Monthly_HP |>
  select(date, ticker, monthly_return) |>
  pivot_wider(
    names_from = ticker,
    values_from = monthly_return
  )
head(returns_wide)

#calculate correclations
cor_matrix <- returns_wide |>
  select(-date) |>
  cor(
    use = "pairwise.complete.obs"
  )

round(cor_matrix, 2)

#Correlation Heatmap
cor_long <- as.data.frame(cor_matrix) |>
  rownames_to_column("firm_1") |>
  pivot_longer(
    -firm_1,
    names_to = "firm_2",
    values_to = "correlation"
  )

# ============================================================
# MONTHLY RETURN DISTRIBUTIONS BY FIRM
# ============================================================

firm_return_distribution <- REOC_Monthly_HP |>
  filter(
    date >= as.Date("2000-01-01"),
    !is.na(monthly_return)
  )

## Sanity Checks
firm_return_distribution |>
  group_by(ticker) |>
  summarise(
    observations = n(),
    first_date = min(date),
    last_date = max(date),
    mean_return = mean(monthly_return),
    median_return = median(monthly_return),
    sd_return = sd(monthly_return),
    min_return = min(monthly_return),
    max_return = max(monthly_return),
    .groups = "drop"
  ) |>
  arrange(desc(sd_return))


###
# Plotting Boxplots
###
ggplot(
  firm_return_distribution,
  aes(
    x = reorder(ticker, monthly_return, FUN = median),
    y = monthly_return
  )
) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Distribution of Monthly Returns by Firm",
    subtitle = "Swiss listed real estate companies, 2000–2026",
    x = NULL,
    y = "Monthly Return"
  ) +
  theme_minimal()
