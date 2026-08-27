# ============================================================
# TOP 3 PROPERTY PORTFOLIOS
# EXPLORATORY DATA VISUALISATION
# ============================================================

library(tidyverse)
library(scales)

# ============================================================
# PLOT 1 — NUMBER OF PROPERTIES BY COMPANY
# ============================================================

portfolio_counts <- tibble(
  company = c(
    "PSP Swiss Property",
    "Swiss Prime Site",
    "Allreal"
  ),
  properties = c(
    nrow(PSPN_properties_geo),
    nrow(SPSN_properties_geo),
    nrow(ALLN_properties_geo)
  )
)

portfolio_counts


plot_1 <- ggplot(
  portfolio_counts,
  aes(
    x = company,
    y = properties,
    fill = company
  )
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(label = properties),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Number of Properties by Company",
    subtitle = "Properties reported in the 2025 property portfolios",
    x = NULL,
    y = "Number of properties"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

plot_1

# ============================================================
# PREPARE CONSTRUCTION YEAR DATA
# ============================================================

construction_data <- bind_rows(
  
  PSPN_properties_geo |>
    transmute(
      company = "PSP Swiss Property",
      property_id,
      construction_year =
        str_extract(
          as.character(year_construction),
          "\\d{4}"
        ) |>
        as.numeric()
    ),
  
  SPSN_properties_geo |>
    transmute(
      company = "Swiss Prime Site",
      property_id,
      construction_year =
        str_extract(
          as.character(year_construction),
          "\\d{4}"
        ) |>
        as.numeric()
    ),
  
  ALLN_properties_geo |>
    transmute(
      company = "Allreal",
      property_id,
      construction_year =
        str_extract(
          as.character(year_construction),
          "\\d{4}"
        ) |>
        as.numeric()
    )
  
) |>
  filter(
    !is.na(construction_year),
    construction_year >= 1600,
    construction_year <= 2025
  )

# ============================================================
# PLOT 2 — CONSTRUCTION YEAR HISTOGRAM
# ============================================================

plot_2 <- ggplot(
  construction_data,
  aes(
    x = construction_year,
    fill = company
  )
) +
  geom_histogram(
    binwidth = 10,
    boundary = 1900,
    color = "white"
  ) +
  facet_wrap(
    ~ company,
    ncol = 1
  ) +
  labs(
    title = "Distribution of Property Construction Years",
    subtitle = "Construction years grouped into 10-year intervals",
    x = "Year of construction",
    y = "Number of properties"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

plot_2

# ============================================================
# PLOT 3 — CONSTRUCTION YEAR BOXPLOT
# ============================================================

plot_3 <- ggplot(
  construction_data,
  aes(
    x = company,
    y = construction_year,
    fill = company
  )
) +
  geom_boxplot(
    alpha = 0.75,
    width = 0.6
  ) +
  labs(
    title = "Construction Year by Property Portfolio",
    subtitle = "Comparison of the age distribution of properties",
    x = NULL,
    y = "Year of construction"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

plot_3

# ============================================================
# PREPARE PROPERTY AREA DATA
# ============================================================

property_area_data <- bind_rows(
  
  PSPN_properties_geo |>
    transmute(
      company = "PSP Swiss Property",
      property_id,
      site_area_m2 = NA_real_,
      usable_area_m2 = rentable_area_m2
    ),
  
  SPSN_properties_geo |>
    transmute(
      company = "Swiss Prime Site",
      property_id,
      site_area_m2 = site_area_m2,
      usable_area_m2 = commercial_area_m2
    ),
  
  ALLN_properties_geo |>
    transmute(
      company = "Allreal",
      property_id,
      site_area_m2 = site_area_m2,
      usable_area_m2 = floor_space_m2
    )
  
)

# ============================================================
# PLOT 4 — SITE AREA VS FLOOR / COMMERCIAL AREA
# ============================================================

property_area_scatter <- bind_rows(
  
  SPSN_properties_geo |>
    transmute(
      company = "Swiss Prime Site",
      property_id,
      site_area_m2,
      property_area_m2 = commercial_area_m2
    ),
  
  ALLN_properties_geo |>
    transmute(
      company = "Allreal",
      property_id,
      site_area_m2,
      property_area_m2 = floor_space_m2
    )
  
) |>
  filter(
    !is.na(site_area_m2),
    !is.na(property_area_m2),
    site_area_m2 > 0,
    property_area_m2 > 0
  )

plot_4 <- ggplot(
  property_area_scatter,
  aes(
    x = site_area_m2,
    y = property_area_m2,
    color = company
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2.5
  ) +
  labs(
    title = "Site Area and Property Floor Area",
    subtitle = "Swiss Prime Site and Allreal properties",
    x = "Site area (m²)",
    y = "Commercial / floor area (m²)",
    color = "Portfolio"
  ) +
  scale_x_continuous(
    labels = label_number(
      big.mark = "'"
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = "'"
    )
  ) +
  theme_minimal()

plot_4

plot_4_regression <- ggplot(
  property_area_scatter,
  aes(
    x = site_area_m2,
    y = property_area_m2,
    color = company
  )
) +
  geom_point(
    alpha = 0.65,
    size = 2.5
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Site and Property Area",
    subtitle = "Linear trend shown separately for each portfolio",
    x = "Site area (m²)",
    y = "Commercial / floor area (m²)",
    color = "Portfolio"
  ) +
  scale_x_continuous(
    labels = label_number(big.mark = "'")
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = "'")
  ) +
  theme_minimal()

plot_4_regression

# ============================================================
# PREPARE GEOGRAPHIC DATA
# ============================================================

city_data <- bind_rows(
  
  PSPN_properties_geo |>
    transmute(
      company = "PSP Swiss Property",
      property_id,
      city
    ),
  
  SPSN_properties_geo |>
    transmute(
      company = "Swiss Prime Site",
      property_id,
      city
    ),
  
  ALLN_properties_geo |>
    transmute(
      company = "Allreal",
      property_id,
      city
    )
  
) |>
  mutate(
    city = case_when(
      
      city %in% c(
        "Zurich",
        "Zürich"
      ) ~ "Zurich",
      
      city %in% c(
        "Geneva",
        "Genève",
        "Genf"
      ) ~ "Geneva",
      
      city %in% c(
        "Bern",
        "Berne"
      ) ~ "Bern",
      
      TRUE ~ city
    )
  )

city_counts <- city_data |>
  count(
    city,
    company,
    name = "properties"
  ) |>
  group_by(city) |>
  mutate(
    total_properties =
      sum(properties)
  ) |>
  ungroup()

top_cities <- city_counts |>
  distinct(
    city,
    total_properties
  ) |>
  slice_max(
    total_properties,
    n = 10
  ) |>
  pull(city)

# ============================================================
# PLOT 5 — PROPERTIES BY CITY AND COMPANY
# ============================================================

plot_5 <- city_counts |>
  filter(
    city %in% top_cities
  ) |>
  
  ggplot(
    aes(
      x = reorder(
        city,
        total_properties
      ),
      y = properties,
      fill = company
    )
  ) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Geographic Concentration of Property Portfolios",
    subtitle = "Top 10 cities by number of properties",
    x = NULL,
    y = "Number of properties",
    fill = "Portfolio"
  ) +
  
  theme_minimal()

plot_5

plot_5_grouped <- city_counts |>
  filter(
    city %in% top_cities
  ) |>
  
  ggplot(
    aes(
      x = reorder(
        city,
        total_properties
      ),
      y = properties,
      fill = company
    )
  ) +
  
  geom_col(
    position = "dodge"
  ) +
  
  coord_flip() +
  
  labs(
    title = "Properties by City and Portfolio",
    subtitle = "Top 10 cities by number of properties",
    x = NULL,
    y = "Number of properties",
    fill = "Portfolio"
  ) +
  
  theme_minimal()

plot_5_grouped