# Install packages

install.packages(c("tidyverse", "skimr", "janitor", "corrplot"))

# Load packages 

library(readxl)
library(skimr)
library(janitor)
library(tidyverse)
library(corrplot)


# 1. Load the dataset, remove the 17 first rows

ConsumerPrice <- read_excel("C:/Users/Alix/Downloads/SNB - Consumer prices – SNB and SFSO core inflation rates.xlsx",
                   skip=17)

# 2. Preview of the dataset

ConsumerPrice <- ConsumerPrice
head(ConsumerPrice)
summary(ConsumerPrice)
str(ConsumerPrice)
skim(ConsumerPrice)


# 3. Remove duplicate rows

ConsumerPrice <- ConsumerPrice %>% distinct()
sum(duplicated(ConsumerPrice))

# 4. Remove the first row

ConsumerPrice <- ConsumerPrice[-1, ]
head(ConsumerPrice)

# 5.Clean column names

ConsumerPrice <- ConsumerPrice %>% 
  clean_names()
colnames(ConsumerPrice)


# 6. Convert column to numeric 

ConsumerPrice <- ConsumerPrice %>%
  mutate(
    snb_core_inflation_trimmed_mean1 = as.numeric(snb_core_inflation_trimmed_mean1),
    sfso_core_inflation_12 = as.numeric(sfso_core_inflation_12),
    sfso_core_inflation_23 = as.numeric(sfso_core_inflation_23),
    sfso_inflation_according_to_the_national_consumer_price_index =
      as.numeric(sfso_inflation_according_to_the_national_consumer_price_index)
  )

str(ConsumerPrice)

# 7. Check missing values

missing_values <- ConsumerPrice %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  mutate(
    missing_percentage = missing_count / nrow(ConsumerPrice) * 100
  )

missing_values

# 8. Check missing value percentage

sapply(ConsumerPrice, function(x) sum(is.na(x)) / length(x) * 100)

# 9. Summary statistics

summary(ConsumerPrice)

# 10. Check for impossible/unusual values

ConsumerPrice %>%
  summarise(
    across(
      where(is.numeric),
      list(
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE)
      )
    )
  )

# 11. Check whether the dates are continuous

ConsumerPrice %>%
  summarise(
    first_month = min(overview),
    last_month = max(overview),
    number_of_months = n()
  )

# 12. Check for duplicated dates

sum(duplicated(ConsumerPrice$overview))

# 13. Check the data visually

ggplot(
  ConsumerPrice,
  aes(
    x = as.Date(paste0(overview, "-01")),
    y = snb_core_inflation_trimmed_mean1
  )
) +
  geom_line() +
  labs(
    title = "SNB Core Inflation – Trimmed Mean",
    x = "Month",
    y = "Inflation (%)"
  ) +
  theme_minimal()

