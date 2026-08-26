# ============================================================
# SCRAPE PSP SWISS PROPERTY - CURRENT PROPERTY PORTFOLIO
# ============================================================

library(rvest)
library(tidyverse)
library(stringr)

url <- "https://www.psp.info/en/portfolio/investment-properties/all-properties"

page <- read_html(url)

# ============================================================
# INSPECT IMAGE ALT TEXT
# ============================================================

img_alt <- page |>
  html_elements("img") |>
  html_attr("alt") |>
  na.omit()

head(img_alt, 50)


# ============================================================
# EXTRACT PROPERTY LINKS
# ============================================================

property_nodes <- page |>
  html_elements("a")

property_links <- tibble(
  href = property_nodes |> html_attr("href"),
  text = property_nodes |> html_text2()
) |>
  filter(
    !is.na(href),
    str_detect(href, "/portfolio/object/")
  ) |>
  distinct(href, .keep_all = TRUE) |>
  mutate(
    source_url = if_else(
      str_starts(href, "http"),
      href,
      paste0("https://www.psp.info", href)
    )
  )

property_links |>
  select(href, text, source_url) |>
  print(n = 30)

# ============================================================
# EXTRACT PROPERTY IMAGE LABELS
# ============================================================

property_addresses <- page |>
  html_elements("img") |>
  html_attr("alt") |>
  tibble(property_label = _) |>
  filter(
    !is.na(property_label),
    str_detect(property_label, ", ")
  )

property_addresses |>
  print(n = 30)

nrow(property_addresses)
nrow(property_links)

# ============================================================
# BUILD PSP PROPERTY DATASET FROM PROPERTY IMAGES
# ============================================================

# Get all image nodes
all_images <- page |>
  html_elements("img")

# Extract alt text
all_alts <- all_images |>
  html_attr("alt")

# Identify property images
property_mask <- !is.na(all_alts) &
  str_detect(all_alts, ", ")

# Keep only property image nodes
property_images <- all_images[property_mask]

# Check
length(property_images)


### extract adress from properly URL
PSPN_properties <- tibble(
  
  property_label = property_images |>
    html_attr("alt"),
  
  href = property_images |>
    html_element(
      xpath = "ancestor::a[contains(@href, '/portfolio/object/')][1]"
    ) |>
    
    
    html_attr("href")
)


PSPN_properties |>
  print(n = 30)

nrow(PSPN_properties)

sum(is.na(PSPN_properties$href))


# ============================================================
# CLEAN PSP PROPERTY DATASET
# ============================================================

PSPN_properties <- PSPN_properties |>
  mutate(
    
    # Standardize whitespace
    property_label = str_squish(property_label),
    
    # Convert relative URLs into full URLs
    source_url = if_else(
      str_starts(href, "http"),
      href,
      paste0("https://www.psp.info", href)
    ),
    
    # Company identifiers
    ticker = "PSPN",
    company = "PSP Swiss Property AG",
    
    # City = everything after final comma
    city = str_extract(
      property_label,
      "[^,]+$"
    ) |>
      str_trim(),
    
    # Address = everything before final comma
    address = str_remove(
      property_label,
      ",\\s*[^,]+$"
    ) |>
      str_trim()
  ) |>
  select(
    ticker,
    company,
    address,
    city,
    source_url
  )

glimpse(PSPN_properties)

PSPN_properties |>
  print(n = 30)

#####  STABLE PROPERTY ID

PSPN_properties <- PSPN_properties |>
  mutate(
    property_id = paste0(
      "PSPN_",
      str_pad(
        row_number(),
        width = 3,
        pad = "0"
      )
    ),
    scrape_date = Sys.Date()
  ) |>
  relocate(
    property_id,
    ticker,
    company
  )


### SANITY CHECKS
# Number of properties
nrow(PSPN_properties)

# Missing addresses / cities
PSPN_properties |>
  filter(
    is.na(address) |
      address == "" |
      is.na(city) |
      city == ""
  )

# Duplicate URLs
PSPN_properties |>
  count(source_url) |>
  filter(n > 1)

# Properties by city
PSPN_properties |>
  count(city, sort = TRUE)

# ============================================================
# Simplfying complex multi adresses
# ============================================================
library(stringr)
library(dplyr)
library(tidygeocoder)

PSPN_properties_geo <- PSPN_properties_geo |>
  mutate(
    simplified_address = address |>
      
      # Keep only first street if multiple streets are separated by /
      str_remove("\\s*/.*$") |>
      
      # For multiple house numbers, keep first number
      # e.g. "Kirschgartenstrasse 12, 14" -> "Kirschgartenstrasse 12"
      str_replace(
        "(\\d+[A-Za-z]?)(\\s*,\\s*\\d+[A-Za-z]?)+$",
        "\\1"
      ) |>
      
      str_squish(),
    
    simplified_full_address = paste(
      simplified_address,
      city,
      "Switzerland",
      sep = ", "
    )
  )

# ============================================================
# GEOCODE PSP PROPERTY ADDRESSES
# ============================================================

library(tidygeocoder)
library(tidyverse)

# Create full address for geocoding
PSPN_properties <- PSPN_properties |>
  mutate(
    full_address = paste(
      address,
      city,
      "Switzerland",
      sep = ", "
    )
  )

# Check addresses before geocoding
PSPN_properties |>
  select(address, city, full_address) |>
  print(n = 20)
# ============================================================
# GEOCODE WITH OPENSTREETMAP / NOMINATIM
# ============================================================

PSPN_properties_geo <- PSPN_properties |>
  geocode(
    address = full_address,
    method = "osm",
    lat = latitude,
    long = longitude
  )

# ============================================================
# SECOND GEOCODING PASS
# Only properties that failed the first geocoding
# ============================================================

missing_geocodes <- PSPN_properties_geo |>
  filter(
    is.na(latitude) |
      is.na(longitude)
  ) |>
  select(
    property_id,
    simplified_full_address
  ) |>
  geocode(
    address = simplified_full_address,
    method = "osm",
    lat = latitude_new,
    long = longitude_new
  )

# ============================================================
# MANUAL-CLEANING PASS
# ============================================================

manual_geocodes <- tibble(
  property_id = c(
    "PSPN_032",
    "PSPN_038",
    "PSPN_042",
    "PSPN_043",
    "PSPN_055"
  ),
  manual_address = c(
    "Rue de Hesse 18, Genève, Switzerland",
    "Rue des Bains 31bis, Genève, Switzerland",
    "Rue du 31 Décembre 8, Genève, Switzerland",
    "Rue François-Bonivard 12, Genève, Switzerland",
    "Chemin des Bossons 2, Lausanne, Switzerland"
  )
)

manual_geocodes <- manual_geocodes |>
  geocode(
    address = manual_address,
    method = "osm",
    lat = latitude_manual,
    long = longitude_manual
  )

### JOINING DATA OF GEO CODED

PSPN_properties_geo <- PSPN_properties_geo |>
  left_join(
    missing_geocodes |>
      select(
        property_id,
        latitude_new,
        longitude_new
      ),
    by = "property_id"
  ) |>
  mutate(
    latitude = coalesce(latitude, latitude_new),
    longitude = coalesce(longitude, longitude_new)
  ) |>
  select(
    -latitude_new,
    -longitude_new
  )

PSPN_properties_geo <- PSPN_properties_geo |>
  left_join(
    manual_geocodes |>
      select(
        property_id,
        latitude_manual,
        longitude_manual
      ),
    by = "property_id"
  ) |>
  mutate(
    latitude = coalesce(latitude, latitude_manual),
    longitude = coalesce(longitude, longitude_manual)
  ) |>
  select(
    -latitude_manual,
    -longitude_manual
  )

# ============================================================
# SAVE PSP PROPERTY DATASET
# ============================================================

write_csv(
  PSPN_properties_geo,
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/PSPN_properties.csv"
)
