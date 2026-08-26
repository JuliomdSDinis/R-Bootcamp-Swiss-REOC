# ============================================================
# SCRAPE SWISS PRIME SITE - CURRENT PROPERTY PORTFOLIO
# ============================================================

library(rvest)
library(tidyverse)
library(stringr)
library(purrr)

base_url <- "https://sps.swiss"

# ============================================================
# 1. PORTFOLIO CATEGORY PAGES
# ============================================================

portfolio_pages <- tibble(
  category = c(
    "Office",
    "Special",
    "Retail",
    "Hotel/Gastronomy",
    "Residential"
  ),
  
  url = c(
    "https://sps.swiss/en/group/real-estate/portfolio/office",
    "https://sps.swiss/en/group/real-estate/portfolio/special",
    "https://sps.swiss/en/group/real-estate/portfolio/retail",
    "https://sps.swiss/en/group/real-estate/portfolio/hotel/gastronomy",
    "https://sps.swiss/en/group/real-estate/portfolio/residential"
  )
)

# ============================================================
# 2. EXTRACT PROPERTY CARDS
# ============================================================

extract_sps_cards <- function(category, url) {
  
  page <- read_html(url)
  
  nodes <- page |>
    html_elements("a")
  
  tibble(
    href = nodes |> html_attr("href"),
    raw_text = nodes |> html_text2()
  ) |>
    
    filter(
      !is.na(href),
      !is.na(raw_text),
      str_detect(raw_text, regex("Address", ignore_case = TRUE)),
      str_detect(raw_text, regex("Canton", ignore_case = TRUE)),
      str_detect(raw_text, regex("Type of use", ignore_case = TRUE))
    ) |>
    
    mutate(
      
      raw_text = str_squish(raw_text),
      
      category_page = category,
      
      source_url = if_else(
        str_starts(href, "http"),
        href,
        paste0(base_url, href)
      )
    ) |>
    
    select(
      category_page,
      raw_text,
      source_url
    )
}

# ============================================================
# 3. SCRAPE ALL CATEGORY PAGES
# ============================================================

SPSN_cards_raw <- portfolio_pages |>
  pmap_dfr(
    ~ extract_sps_cards(..1, ..2)
  ) |>
  distinct(source_url, .keep_all = TRUE)

nrow(SPSN_cards_raw)

SPSN_cards_raw |>
  print(n = 50)

# ============================================================
# 4. PARSE PROPERTY CARD INFORMATION
# ============================================================

SPSN_properties <- SPSN_cards_raw |>
  mutate(
    
    property_name = str_extract(
      raw_text,
      "^.*?(?=\\s*Address)"
    ) |>
      str_squish(),
    
    address_full = str_match(
      raw_text,
      regex(
        "Address\\s*(.*?)\\s*Canton",
        ignore_case = TRUE
      )
    )[, 2] |>
      str_squish(),
    
    canton = str_match(
      raw_text,
      regex(
        "Canton\\s*(.*?)\\s*Type of use",
        ignore_case = TRUE
      )
    )[, 2] |>
      str_squish(),
    
    type_of_use = str_match(
      raw_text,
      regex(
        "Type of use\\s*(.*)$",
        ignore_case = TRUE
      )
    )[, 2] |>
      str_squish()
  )


## CLEAN UP THE ADRESS 
SPSN_properties <- SPSN_properties |>
  mutate(
    
    address = str_remove(
      address_full,
      ",\\s*\\d{4}\\s+.*$"
    ) |>
      str_trim(),
    
    postal_code = str_extract(
      address_full,
      "\\b\\d{4}\\b"
    ),
    
    city = str_extract(
      address_full,
      "(?<=\\d{4}\\s).*$"
    ) |>
      str_trim(),
    
    ticker = "SPSN",
    
    company = "Swiss Prime Site AG",
    
    scrape_date = Sys.Date()
  ) |>
  
  select(
    ticker,
    company,
    property_name,
    address,
    postal_code,
    city,
    canton,
    type_of_use,
    source_url,
    scrape_date
  )

# ============================================================
# 5. INSPECT ONE INDIVIDUAL PROPERTY PAGE
# ============================================================

test_url <- SPSN_properties$source_url[1]

test_page <- read_html(test_url)

test_page |>
  html_elements("h1, h2, h3, p") |>
  html_text2() |>
  str_squish() |>
  print()

# ============================================================
# 5. SCRAPE YEAR BUILT + FLOOR SPACE FROM PROPERTY PAGES
# ============================================================

scrape_sps_details <- function(url) {
  
  page <- read_html(url)
  
  txt <- page |>
    html_text2() |>
    str_squish()
  
  tibble(
    source_url = url,
    
    year_built = str_match(
      txt,
      regex(
        "Year of construction\\s*(\\d{4})",
        ignore_case = TRUE
      )
    )[, 2],
    
    floor_space_m2 = str_match(
      txt,
      regex(
        "Floor Space\\s*m\\s*2\\s*([0-9\\s'’,.]+)",
        ignore_case = TRUE
      )
    )[, 2]
  )
}