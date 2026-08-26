# ============================================================
# SPSN PROPERTY REPORT 2025
# EXTRACT STRUCTURED PROPERTY TABLE USING PDF COORDINATES
# ============================================================

library(tidyverse)
library(pdftools)
library(stringr)

pdf_file <- paste0(
  "Groupwork dataset script/",
  "Data cleaning/Geo_Firms_Portfolios/",
  "DataTop3Properties/",
  "SPSN_Property_Report_2025.pdf"
)

# ============================================================
# 1. READ PDF AS POSITIONED WORD DATA
# ============================================================

SPSN_pdf_data <- pdf_data(pdf_file)

length(SPSN_pdf_data)

# ============================================================
# 2. HELPER FUNCTIONS
# ============================================================

collapse_words <- function(x) {
  x |>
    arrange(x) |>
    pull(text) |>
    paste(collapse = " ") |>
    str_squish()
}

clean_num <- function(x) {
  
  x <- str_squish(x)
  
  if (
    is.na(x) ||
    x == "" ||
    x == "–" ||
    x == "-"
  ) {
    return(NA_real_)
  }
  
  x |>
    str_replace_all(" ", "") |>
    str_replace_all("’", "") |>
    str_replace_all("'", "") |>
    as.numeric()
}

# ============================================================
# 3. PARSE ONE PDF PAGE
# ============================================================

parse_sps_page <- function(page_data, page_number) {
  
  dat <- page_data |>
    as_tibble() |>
    rename(
      x = x,
      y = y
    )
  
  # Keep body of table only
  dat <- dat |>
    filter(
      y > 210,
      y < 750
    )
  
  # Identify rows from property text in first column
  property_rows <- dat |>
    filter(
      x < 225,
      str_detect(
        text,
        "^[[:alpha:]À-ÿ]"
      )
    ) |>
    group_by(y) |>
    summarise(
      property = collapse_words(cur_data()),
      .groups = "drop"
    ) |>
    filter(
      str_detect(property, ",")
    )
  
  if (nrow(property_rows) == 0) {
    return(tibble())
  }
  
  # Assign every word to the closest property row
  dat2 <- dat |>
    mutate(
      property_y = map_dbl(
        y,
        function(current_y) {
          
          candidates <- property_rows$y[
            property_rows$y <= current_y + 4
          ]
          
          if (length(candidates) == 0) {
            return(NA_real_)
          }
          
          max(candidates)
        }
      )
    ) |>
    filter(!is.na(property_y))
  
  rows <- split(
    dat2,
    dat2$property_y
  )
  
  map_dfr(
    rows,
    function(r) {
      
      get_col <- function(lower, upper = Inf) {
        
        z <- r |>
          filter(
            x >= lower,
            x < upper
          )
        
        if (nrow(z) == 0) {
          return(NA_character_)
        }
        
        collapse_words(z)
      }
      
      tibble(
        report_property = get_col(0, 225),
        
        target_rental_income_tchf =
          clean_num(get_col(225, 255)),
        
        vacancy_rate_pct =
          clean_num(get_col(255, 273)),
        
        ownership_status =
          get_col(273, 308),
        
        year_construction =
          get_col(308, 334),
        
        year_renovation =
          get_col(334, 360),
        
        site_area_m2 =
          clean_num(get_col(360, 397)),
        
        commercial_area_m2 =
          clean_num(get_col(397, 427)),
        
        retail_pct =
          clean_num(get_col(427, 447)),
        
        office_pct =
          clean_num(get_col(447, 470)),
        
        hotel_gastronomy_pct =
          clean_num(get_col(470, 493)),
        
        assisted_living_pct =
          clean_num(get_col(493, 515)),
        
        storage_pct =
          clean_num(get_col(515, 538)),
        
        other_pct =
          clean_num(get_col(538, Inf)),
        
        source_pdf_page = page_number
      )
    }
  )
}

# ============================================================
# 4. EXTRACT PAGES 2–5
# ============================================================

SPSN_properties_raw <- map2_dfr(
  SPSN_pdf_data[2:5],
  2:5,
  parse_sps_page
)

# ============================================================
# 5. CLEAN TO CURRENT INVESTMENT PROPERTIES
# ============================================================

SPSN_properties_2025 <- SPSN_properties_raw |>
  
  # Remove obvious non-property/header rows
  filter(
    !is.na(report_property),
    report_property != "",
    !str_detect(
      report_property,
      regex(
        "Total properties|Total building land|Overall total|Property details",
        ignore_case = TRUE
      )
    )
  ) |>
  
  # Remove properties sold during 2025
  filter(
    !str_detect(
      ownership_status,
      "^\\d{2}\\.\\d{2}\\.2025$"
    )
  )

# ============================================================
# REMOVE BUILDING LAND + DEVELOPMENT SECTION
# ============================================================

SPSN_properties_2025 <- SPSN_properties_2025 |>
  filter(
    !report_property %in% c(
      "Augst, Rheinstrasse 54",
      "Dietikon, Bodacher 23",
      "Dietikon, Bodacher/Im Maienweg",
      "Dietikon, Bodacher/Ziegelägerten 10",
      "Meyrin, Route de Pré-Bois 35",
      "Spreitenbach, Joosäcker 7",
      "Zurich, Oleanderstrasse 1",
      "Basel, Steinenvorstadt 5",
      "Berne, Stauffacherstrasse 131/Bern 131",
      "Plan-les-Ouates, Chemin des Aulx/Espace Tourbillon building A",
      "Zurich, Albisriederstrasse 203, 207, 243",
      "Zurich, Seidengasse 1/Jelmoli"
    )
  )

SPSN_properties_2025 <- SPSN_properties_2025 |>
  mutate(
    ticker = "SPSN",
    company = "Swiss Prime Site AG",
    report_date = as.Date("2025-12-31")
  ) |>
  relocate(
    ticker,
    company,
    report_date,
    report_property
  )

nrow(SPSN_properties_2025)

SPSN_properties_2025 |>
  summarise(
    properties = n(),
    
    total_site_area_m2 =
      sum(site_area_m2, na.rm = TRUE),
    
    total_commercial_area_m2 =
      sum(commercial_area_m2, na.rm = TRUE)
  )

# ============================================================
# REMOVE PAGE 5 TOTALS, BUILDING LAND AND DEVELOPMENT ASSETS
# ============================================================

SPSN_properties_2025 <- SPSN_properties_2025 |>
  
  filter(
    
    # Remove malformed total / summary rows
    !str_detect(
      report_property,
      regex(
        "Total|Overall|building land|under construction|development",
        ignore_case = TRUE
      )
    ),
    
    # Remove building-land properties
    !str_detect(
      report_property,
      regex(
        paste(
          c(
            "Rheinstrasse 54",
            "Bodacher",
            "Route de Pré-Bois",
            "Joosäcker",
            "Oleanderstrasse"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    ),
    
    # Remove properties under construction / development
    !str_detect(
      report_property,
      regex(
        paste(
          c(
            "Steinenvorstadt 5",
            "Stauffacherstrasse 131",
            "Chemin des Aulx",
            "Albisriederstrasse 203",
            "Seidengasse 1"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    )
  )


# ============================================================
# FIND THE 3 SPSN PROPERTIES MISSED BY THE TABLE PARSER
# ============================================================

# Extract text from the four detailed property pages
property_text_check <- SPSN_pdf[2:5] |>
  paste(collapse = "\n")

property_lines_check <- property_text_check |>
  str_split("\n") |>
  unlist() |>
  str_squish()

# Keep lines that look like property addresses
SPSN_address_list <- tibble(
  report_property = property_lines_check
) |>
  filter(
    str_detect(
      report_property,
      "^[[:alpha:]À-ÿ .'-]+,"
    )
  ) |>
  
  # Remove headers
  filter(
    !str_detect(
      report_property,
      regex(
        "Offices, medical|Property details",
        ignore_case = TRUE
      )
    )
  )

# Stop before building-land section
building_land_start <- which(
  str_detect(
    SPSN_address_list$report_property,
    "^Augst, Rheinstrasse 54"
  )
)[1]

SPSN_address_list <- SPSN_address_list |>
  slice(
    1:(building_land_start - 1)
  )

sold_properties <- c(
  "Aarau, Bahnhofstrasse 23",
  "Biel, Solothurnstrasse 122",
  "Brugg, Hauptstrasse 2",
  "Buchs ZH, Mülibachstrasse 41",
  "Dietikon, Bahnhofplatz 11/Neumattstrasse 24",
  "Oftringen, Aussenparkplatz Spitalweid",
  "Oftringen, Baurecht Spitalweid",
  "Romanel, Chemin du Marais 8",
  "Winterthur, Untertor 24"
)

SPSN_address_list <- SPSN_address_list |>
  filter(
    !report_property %in% sold_properties
  ) |>
  distinct()

nrow(SPSN_address_list)

# ============================================================
# COMPARE PDF ADDRESS LIST WITH PARSED TABLE
# ============================================================

missing_from_parser <- SPSN_address_list |>
  anti_join(
    SPSN_properties_2025 |>
      select(report_property),
    by = "report_property"
  )

missing_from_parser |>
  print(n = 20)

# ============================================================
# CREATE CLEAN PROPERTY NAMES FROM PDF TEXT
# ============================================================

SPSN_address_list_clean <- SPSN_address_list |>
  mutate(
    
    property_clean = str_extract(
      report_property,
      paste0(
        "^.*?",
        "(?=\\s+[0-9][0-9 ]*\\s+",
        "(?:[0-9]+\\.[0-9]+|–)\\s+",
        "(?:sole ownership|freehold|land lease))"
      )
    ) |>
      str_squish()
    
  ) |>
  filter(
    !is.na(property_clean)
  ) |>
  distinct(property_clean)

SPSN_address_list_clean |>
  print(n = 150)

# ============================================================
# FIND PROPERTIES MISSING FROM PARSER
# ============================================================

missing_from_parser <- SPSN_address_list_clean |>
  anti_join(
    SPSN_properties_2025 |>
      transmute(
        property_clean = str_squish(report_property)
      ),
    by = "property_clean"
  )

missing_from_parser |>
  print(n = 20)

SPSN_properties_2025 |>
  select(report_property) |>
  arrange(report_property) |>
  print(n = 150)

SPSN_properties_raw |>
  select(
    report_property,
    ownership_status,
    site_area_m2,
    commercial_area_m2,
    source_pdf_page
  ) |>
  slice_tail(n = 40) |>
  print(n = 40)

# ============================================================
# REBUILD CURRENT SPSN INVESTMENT PROPERTY PORTFOLIO
# ============================================================

# Properties explicitly sold during 2025
sold_properties <- c(
  "Aarau, Bahnhofstrasse 23",
  "Biel, Solothurnstrasse 122",
  "Brugg, Hauptstrasse 2",
  "Buchs ZH, Mülibachstrasse 41",
  "Dietikon, Bahnhofplatz 11/Neumattstrasse 24",
  "Oftringen, Aussenparkplatz Spitalweid",
  "Oftringen, Baurecht Spitalweid",
  "Romanel, Chemin du Marais 8",
  "Winterthur, Untertor 24"
)

# Find the malformed Vulkanstrasse + total row
vulkan_row <- which(
  str_detect(
    SPSN_properties_raw$report_property,
    "Vulkanstrasse 126"
  )
)[1]

vulkan_row

SPSN_current <- SPSN_properties_raw |>
  
  # Everything up to and including the malformed Vulkan row
  slice(1:vulkan_row) |>
  
  # Remove sold properties
  filter(
    !report_property %in% sold_properties
  ) |>
  
  # Remove malformed Vulkan + Total row
  filter(
    !str_detect(
      report_property,
      "Vulkanstrasse 126"
    )
  )

# ============================================================
# MANUALLY RESTORE VULKANSTRASSE 126
# ============================================================

vulkan_correct <- tibble(
  report_property = "Zurich, Vulkanstrasse 126",
  target_rental_income_tchf = 50,
  vacancy_rate_pct = NA_real_,
  ownership_status = "sole ownership",
  year_construction = "1942/1972/1979",
  year_renovation = NA_character_,
  site_area_m2 = 4298,
  commercial_area_m2 = 2273,
  retail_pct = NA_real_,
  office_pct = 17.1,
  hotel_gastronomy_pct = NA_real_,
  assisted_living_pct = NA_real_,
  storage_pct = 82.9,
  other_pct = NA_real_,
  source_pdf_page = 5L
)

SPSN_properties_2025 <- bind_rows(
  SPSN_current,
  vulkan_correct
)

SPSN_properties_2025 <- SPSN_properties_2025 |>
  mutate(
    ticker = "SPSN",
    company = "Swiss Prime Site AG",
    report_date = as.Date("2025-12-31")
  ) |>
  relocate(
    ticker,
    company,
    report_date,
    report_property
  )

nrow(SPSN_properties_2025)

SPSN_properties_2025 |>
  summarise(
    properties = n(),
    total_site_area_m2 =
      sum(site_area_m2, na.rm = TRUE),
    total_commercial_area_m2 =
      sum(commercial_area_m2, na.rm = TRUE)
  )

# ============================================================
# PREPARE SPSN PROPERTY DATA FOR GEOCODING
# ============================================================

SPSN_properties_2025 <- SPSN_properties_2025 |>
  mutate(
    property_id = sprintf(
      "SPSN_%03d",
      row_number()
    ),
    
    city = str_extract(
      report_property,
      "^[^,]+"
    ) |>
      str_trim(),
    
    address = str_remove(
      report_property,
      "^[^,]+,\\s*"
    ) |>
      str_trim()
  ) |>
  relocate(
    property_id,
    ticker,
    company,
    report_date,
    address,
    city,
    report_property
  )

SPSN_properties_2025 <- SPSN_properties_2025 |>
  mutate(
    city = recode(
      city,
      "Zurich" = "Zürich",
      "Geneva" = "Genève",
      "Berne" = "Bern",
      "Lucerne" = "Luzern"
    ),
    
    full_address = paste(
      address,
      city,
      "Switzerland",
      sep = ", "
    )
  )

# ============================================================
# CREATE SIMPLIFIED ADDRESSES FOR GEOCODING
# ============================================================

library(tidyverse)
library(stringr)
library(tidygeocoder)

SPSN_properties_2025 <- SPSN_properties_2025 |>
  mutate(
    
    # Start from the report address
    simplified_address = address,
    
    # Remove common building/project names after "/"
    # but only when the part after "/" does NOT look like another street address
    simplified_address = case_when(
      
      # Keep slash if second part contains a number -> probably another street address
      str_detect(
        simplified_address,
        "/[^/]*\\d"
      ) ~ simplified_address,
      
      # Otherwise remove building/project name after slash
      TRUE ~ str_remove(
        simplified_address,
        "/.*$"
      )
    ),
    
    # Clean whitespace
    simplified_address = str_squish(
      simplified_address
    ),
    
    # Create full address for geocoding
    simplified_full_address = paste(
      simplified_address,
      city,
      "Switzerland",
      sep = ", "
    )
  )

# ============================================================
# GEOCODE SPSN PROPERTIES
# ============================================================

SPSN_properties_geo <- SPSN_properties_2025 |>
  geocode(
    address = simplified_full_address,
    method = "osm",
    lat = latitude,
    long = longitude
  )

# ============================================================
# SECOND-PASS ADDRESS CLEANING FOR FAILED SPSN GEOCODES
# ============================================================

SPSN_missing_geo <- SPSN_properties_geo |>
  filter(
    is.na(latitude) |
      is.na(longitude)
  ) |>
  mutate(
    
    # Start from original address
    second_pass_address = address,
    
    # Remove building / project names after slash
    second_pass_address = str_remove(
      second_pass_address,
      "/.*$"
    ),
    
    # If there are multiple street-number groups separated by comma,
    # keep only the first address portion
    second_pass_address = str_extract(
      second_pass_address,
      "^[^,]+(?:,\\s*[^,]+)?"
    ),
    
    # Convert ranges such as 5–7 to first number: 5
    second_pass_address = str_replace(
      second_pass_address,
      "(\\d+)\\s*[–-]\\s*\\d+[a-z]?",
      "\\1"
    ),
    
    # Reduce multiple house numbers such as:
    # Grabenstrasse 17, 19 -> Grabenstrasse 17
    second_pass_address = str_replace(
      second_pass_address,
      "(\\d+[a-z]?),\\s*\\d+[a-z]?(?:,\\s*\\d+[a-z]?)*$",
      "\\1"
    ),
    
    second_pass_address = str_squish(
      second_pass_address
    ),
    
    second_pass_full_address = paste(
      second_pass_address,
      city,
      "Switzerland",
      sep = ", "
    )
  )

# ============================================================
# SECOND-PASS GEOCODING
# ============================================================

SPSN_missing_geo <- SPSN_missing_geo |>
  geocode(
    address = second_pass_full_address,
    method = "osm",
    lat = latitude_new,
    long = longitude_new
  )

SPSN_missing_geo |>
  summarise(
    attempted = n(),
    successfully_geocoded =
      sum(!is.na(latitude_new) & !is.na(longitude_new)),
    still_missing =
      sum(is.na(latitude_new) | is.na(longitude_new))
  )

# ============================================================
# ADD SECOND-PASS COORDINATES BACK TO MASTER TABLE
# ============================================================

SPSN_properties_geo <- SPSN_properties_geo |>
  left_join(
    SPSN_missing_geo |>
      select(
        property_id,
        latitude_new,
        longitude_new,
        second_pass_address,
        second_pass_full_address
      ),
    by = "property_id"
  ) |>
  mutate(
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
    -latitude_new,
    -longitude_new
  )

# ============================================================
# THIRD PASS - MANUAL GEOCODING ADDRESSES
# ============================================================

SPSN_manual_addresses <- tibble(
  property_id = c(
    "SPSN_014",
    "SPSN_031",
    "SPSN_036",
    "SPSN_080",
    "SPSN_084",
    "SPSN_093",
    "SPSN_107"
  ),
  
  manual_geocode_address = c(
    "Hochbergerstrasse 60, Basel, Switzerland",
    "Rue du Rhône 8, Genève, Switzerland",
    "Place des Alpes 1, Genève, Switzerland",
    "Theaterstrasse 15, Winterthur, Switzerland",
    "Dammstrasse 19, Zug, Switzerland",
    "Beethovenstrasse 33, Zürich, Switzerland",
    "Kappenbühlweg 9, Zürich, Switzerland"
  )
)

# ============================================================
# GEOCODE THE 7 MANUAL ADDRESSES
# ============================================================

SPSN_manual_geo <- SPSN_manual_addresses |>
  geocode(
    address = manual_geocode_address,
    method = "osm",
    lat = latitude_manual,
    long = longitude_manual
  )

SPSN_manual_geo

SPSN_manual_geo |>
  select(
    property_id,
    manual_geocode_address,
    latitude_manual,
    longitude_manual
  ) |>
  print(n = 10)

# ============================================================
# ADD MANUAL COORDINATES TO FINAL DATASET
# ============================================================

SPSN_properties_geo <- SPSN_properties_geo |>
  left_join(
    SPSN_manual_geo,
    by = "property_id"
  ) |>
  mutate(
    latitude = coalesce(
      latitude,
      latitude_manual
    ),
    
    longitude = coalesce(
      longitude,
      longitude_manual
    )
  ) |>
  select(
    -latitude_manual,
    -longitude_manual
  )

# ============================================================
# 5. SAVE FINAL SPSN PROPERTY DATASET
# ============================================================

write_csv(
  SPSN_properties_geo,
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/DataTop3Properties/SPSN_properties_2025_final.csv"
)