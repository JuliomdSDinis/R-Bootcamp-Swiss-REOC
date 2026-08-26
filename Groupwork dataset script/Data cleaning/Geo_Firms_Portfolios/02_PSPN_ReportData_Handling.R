# ============================================================
# PSPN - MERGE GEOCODED PROPERTY DATA WITH 2025 REPORT DATA
# ============================================================

library(tidyverse)
library(stringr)
library(stringi)


# ============================================================
# 1. LOAD BOTH FILES
# ============================================================

PSPN_geo <- read_csv(
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/PSPN_properties.csv",
  show_col_types = FALSE
)

PSPN_report <- read_csv(
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/PSPN_property_report_2025_table.csv",
  show_col_types = FALSE
)


# ============================================================
# 2. INSPECT
# ============================================================

glimpse(PSPN_geo)
glimpse(PSPN_report)

nrow(PSPN_geo)      # should be 148
nrow(PSPN_report)   # should be 150

# ============================================================
# 3. FUNCTION TO STANDARDISE ADDRESSES FOR MATCHING
# ============================================================

normalize_address <- function(x) {
  
  x |>
    str_to_lower() |>
    
    # Convert accents:
    # Zürich -> Zurich
    # Genève -> Geneve
    stringi::stri_trans_general("Latin-ASCII") |>
    
    # Remove punctuation differences
    str_replace_all("[/,-]", " ") |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    
    # Remove repeated spaces
    str_squish()
}

# ============================================================
# 4. CREATE MATCHING KEY - EXISTING GEOCODED DATA
# ============================================================

PSPN_geo <- PSPN_geo |>
  mutate(
    
    geo_property = paste(
      city,
      address,
      sep = ", "
    ),
    
    match_key = normalize_address(
      geo_property
    )
  )

# ============================================================
# 5. CREATE MATCHING KEY - ANNUAL REPORT DATA
# ============================================================

PSPN_report <- PSPN_report |>
  mutate(
    match_key = normalize_address(
      report_property
    )
  )

# ============================================================
# 6. SELECT VARIABLES FROM ANNUAL REPORT
# ============================================================

PSPN_report_selected <- PSPN_report |>
  select(
    match_key,
    report_property,
    land_area_m2,
    office_area_m2,
    retail_area_m2,
    gastronomy_area_m2,
    other_area_m2,
    rentable_area_m2,
    year_construction
  )

# ============================================================
# 7. JOIN REPORT VARIABLES TO EXISTING PROPERTY DATA
# ============================================================

PSPN_properties_final <- PSPN_geo |>
  left_join(
    PSPN_report_selected,
    by = "match_key"
  )

# ============================================================
# 8. MATCHING SANITY CHECK
# ============================================================

PSPN_properties_final |>
  summarise(
    properties = n(),
    
    matched_report =
      sum(!is.na(report_property)),
    
    unmatched_report =
      sum(is.na(report_property))
  )

# ============================================================
# 9. INSPECT UNMATCHED PROPERTIES
# ============================================================

PSPN_properties_final |>
  filter(
    is.na(report_property)
  ) |>
  select(
    property_id,
    address,
    city,
    geo_property
  ) |>
  print(n = 50)

# ============================================================
# FINAL PSPN MERGE
# Report = master dataset
# Geo CSV = adds IDs, addresses and coordinates where matched
# ============================================================

PSPN_properties_2025 <- PSPN_report |>
  
  # Keep the report variables we want
  select(
    match_key,
    report_property,
    land_area_m2,
    office_area_m2,
    retail_area_m2,
    gastronomy_area_m2,
    other_area_m2,
    rentable_area_m2,
    year_construction
  ) |>
  
  # Add existing property IDs + geographic information
  left_join(
    PSPN_geo |>
      select(
        match_key,
        property_id,
        ticker,
        company,
        address,
        city,
        full_address,
        latitude,
        longitude,
        source_url,
        scrape_date
      ),
    by = "match_key"
  )

PSPN_properties_2025 <- PSPN_properties_2025 |>
  mutate(
    ticker = coalesce(
      ticker,
      "PSPN"
    ),
    
    company = coalesce(
      company,
      "PSP Swiss Property AG"
    ),
    
    report_date = as.Date("2025-12-31")
  )

PSPN_properties_2025 <- PSPN_properties_2025 |>
  mutate(
    office_pct =
      100 * office_area_m2 / rentable_area_m2,
    
    retail_pct =
      100 * retail_area_m2 / rentable_area_m2,
    
    gastronomy_pct =
      100 * gastronomy_area_m2 / rentable_area_m2,
    
    other_pct =
      100 * other_area_m2 / rentable_area_m2
  )

PSPN_properties_2025 <- PSPN_properties_2025 |>
  select(
    property_id,
    ticker,
    company,
    report_date,
    
    report_property,
    
    address,
    city,
    full_address,
    
    latitude,
    longitude,
    
    year_construction,
    
    land_area_m2,
    office_area_m2,
    retail_area_m2,
    gastronomy_area_m2,
    other_area_m2,
    rentable_area_m2,
    
    office_pct,
    retail_pct,
    gastronomy_pct,
    other_pct,
    
    source_url,
    scrape_date,
    
    match_key
  )

# ============================================================
# ADD ADDRESS + GEO DATA FOR THE 5 MISSING PSPN PROPERTIES
# ============================================================

library(tidyverse)
library(tidygeocoder)


# ============================================================
# 1. MANUALLY DEFINE CLEAN ADDRESSES
# ============================================================

PSPN_missing_geo <- tibble(
  report_property = c(
    "Urdorf, Heinrich Stutz-Strasse 23, 25",
    "Carouge, Route des Acacias 50, 52",
    "Basel, Marktplatz 30, 30a",
    "Bern, Eigerstrasse 2",
    "Aarau, lgelweid 1"
  ),
  
  address = c(
    "Heinrich Stutz-Strasse 23",
    "Route des Acacias 50",
    "Marktplatz 30",
    "Eigerstrasse 2",
    "Igelweid 1"
  ),
  
  city = c(
    "Urdorf",
    "Carouge",
    "Basel",
    "Bern",
    "Aarau"
  )
) |>
  
  mutate(
    full_address = paste(
      address,
      city,
      "Switzerland",
      sep = ", "
    )
  )


# ============================================================
# 2. GEOCODE THE FIVE PROPERTIES
# ============================================================

PSPN_missing_geo <- PSPN_missing_geo |>
  geocode(
    address = full_address,
    method = "osm",
    lat = latitude_new,
    long = longitude_new
  )

# ============================================================
# 3. ADD RESULTS BACK INTO MASTER PSPN DATASET
# ============================================================

PSPN_properties_2025 <- PSPN_properties_2025 |>
  left_join(
    PSPN_missing_geo |>
      select(
        report_property,
        address_new = address,
        city_new = city,
        full_address_new = full_address,
        latitude_new,
        longitude_new
      ),
    by = "report_property"
  ) |>
  
  mutate(
    address = coalesce(
      address,
      address_new
    ),
    
    city = coalesce(
      city,
      city_new
    ),
    
    full_address = coalesce(
      full_address,
      full_address_new
    ),
    
    latitude = coalesce(
      latitude,
      latitude_new
    ),
    
    longitude = coalesce(
      longitude,
      longitude_new
    )
  ) |>
  
  select(
    -address_new,
    -city_new,
    -full_address_new,
    -latitude_new,
    -longitude_new
  )

# ============================================================
# 5. SAVE FINAL PSPN PROPERTY DATASET
# ============================================================

write_csv(
  PSPN_properties_2025,
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/DataTop3Properties/PSPN_properties_2025_final.csv"
)
