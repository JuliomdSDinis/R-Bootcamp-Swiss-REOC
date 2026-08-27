# ============================================================
# ALLN - RESIDENTIAL PROPERTY TABLE
# ============================================================

library(tidyverse)
library(stringr)
library(pdftools)
library(purrr)

pdf_file <- paste0(
  "Groupwork dataset script/",
  "Data cleaning/Geo_Firms_Portfolios/",
  "DataTop3Properties/",
  "ALLN_Property_Report_2025.pdf"
)

ALLN_pdf <- pdf_text(pdf_file)

# ============================================================
# 1. RESIDENTIAL TEXT
# ============================================================

residential_text <- paste(
  ALLN_pdf[1],
  ALLN_pdf[2],
  sep = "\n"
)

residential_lines <- residential_text |>
  str_split("\n") |>
  unlist() |>
  str_squish()

residential_lines <- residential_lines[
  residential_lines != ""
]

# ============================================================
# 2. EXTRACT RESIDENTIAL PROPERTY ROWS
# ============================================================

ALLN_residential_raw <- tibble(
  raw_line = residential_lines
) |>
  filter(
    str_detect(
      raw_line,
      "\\s(SO|LO)\\s"
    )
  )

# ============================================================
# 3. PARSE RESIDENTIAL COMMON FIELDS
# ============================================================

ALLN_residential <- ALLN_residential_raw |>
  mutate(
    
    ownership_status = str_match(
      raw_line,
      "\\s(SO|LO)\\s"
    )[, 2],
    
    year_acquired = str_match(
      raw_line,
      "\\s(?:SO|LO)\\s+(\\d{4})"
    )[, 2],
    
    year_construction = str_match(
      raw_line,
      "\\s(?:SO|LO)\\s+\\d{4}\\s+(\\d{4})"
    )[, 2],
    
    # Everything before ownership = location + address
    property_text = str_remove(
      raw_line,
      "\\s(?:SO|LO)\\s.*$"
    ) |>
      str_squish(),
    
    ticker = "ALLN",
    company = "Allreal Holding AG",
    property_type = "Residential",
    report_date = as.Date("2025-12-31")
  )

ALLN_residential <- ALLN_residential |>
  mutate(
    
    location = str_extract(
      property_text,
      "^[^ ]+"
    ),
    
    address = str_remove(
      property_text,
      "^[^ ]+\\s+"
    ) |>
      str_squish()
  )

# ============================================================
# 4. CLEAN ALLN RESIDENTIAL PROPERTY ROWS
# ============================================================

ALLN_residential <- ALLN_residential |>
  
  # Remove the two footnote rows
  filter(
    !(location == "1" & address == "1")
  ) |>
  
  # Remove PDF footnote numbers attached to addresses
  mutate(
    address = str_remove(
      address,
      "(?<=\\D)[5-7]$"
    )
  ) |>
  
  # Restore multiline addresses from the annual report
  mutate(
    address = case_when(
      
      location == "Zurich" &
        address == "Schürgistrasse 18/20" ~
        "Heerenwiesen 23–41 / Winterthurerstrasse 563 / Schürgistrasse 18/20",
      
      location == "Adliswil" &
        address == "Grütstrasse 33–39" ~
        "Moosstrasse 1–13 / Grütstrasse 33–39",
      
      location == "Bülach" &
        address == "Im Stumpen 2" ~
        "Hohfuristrasse 7–11 / Unterweg 55–59 / Im Stumpen 2",
      
      location == "Fällanden" &
        address == "Unterdorfwäg 2–22" ~
        "Unterdorfstrasse 2/4 / Unterdorfwäg 2–22",
      
      location == "Glattbrugg" &
        address == "1–23, 2–16" ~
        "Hohenstieglen 1–23, 2–16",
      
      location == "Schlieren" &
        address == "Engstringermatte" ~
        "Limmataustrasse 2–8 / Limmatstrasse 9–11 / Engstringermatte",
      
      location == "Schlieren" &
        address == "Flöhrebenstrasse 6" ~
        "Schulstrasse 71–77 / Flöhrebenstrasse 6",
      
      location == "Volketswil" &
        address == "Neufund 1/3" ~
        "Sunnebüelstrasse 1–17 / Ifangstrasse 12–20 / Neufund 1/3",
      
      location == "Wallisellen" &
        address == "(Richti-Areal)" ~
        paste0(
          "Escherweg 2–6 / Favreweg 1–5 / ",
          "Richtiarkade 13–15 / Richtiring 14–16 (Richti-Areal)"
        ),
      
      location == "Carouge" &
        address == "Rue Saint-Nicolas-le-Vieux 9–11" ~
        "Rue Daniel-Gevril 10 / Rue Saint-Nicolas-le-Vieux 9–11",
      
      location == "Gland" &
        address == "Allée Louis Cristin 1" ~
        "Chemin du Molard 10 / Allée Leotherius 2 / Allée Louis Cristin 1",
      
      TRUE ~ address
    )
  )

# ============================================================
# 5. CREATE ALLN RESIDENTIAL IDs
# ============================================================

ALLN_residential <- ALLN_residential |>
  mutate(
    property_id = sprintf(
      "ALLN_R_%03d",
      row_number()
    ),
    
    report_property = paste(
      location,
      address,
      sep = ", "
    )
  ) |>
  relocate(
    property_id,
    ticker,
    company,
    property_type,
    report_date,
    location,
    address,
    report_property
  )

# ============================================================
# 6. HELPER FUNCTIONS FOR ALLN NUMERIC DATA
# ============================================================

clean_alln_number <- function(x) {
  
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
    as.numeric()
}

# ============================================================
# 7. PARSE RESIDENTIAL NUMERIC VARIABLES
# ============================================================

residential_tail_pattern <- paste0(
  # floor space
  "([0-9]+(?:\\s[0-9]{3})*)\\s+",
  
  # 1–1.5 room
  "([0-9]+|–)\\s+",
  
  # 2–2.5 room
  "([0-9]+|–)\\s+",
  
  # 3–3.5 room
  "([0-9]+|–)\\s+",
  
  # 4–4.5 room
  "([0-9]+|–)\\s+",
  
  # 5+ room
  "([0-9]+|–)\\s+",
  
  # total apartments
  "([0-9]+|–)\\s+",
  
  # other uses m2
  "([0-9]+(?:\\s[0-9]{3})*|–)\\s+",
  
  # target rent
  "([0-9]+(?:\\.[0-9]+)?)\\s+",
  
  # vacancy
  "([0-9]+(?:\\.[0-9]+)?)\\s+",
  
  # discount/capitalisation rate
  "([0-9]+\\.[0-9]+/[0-9]+\\.[0-9]+)$"
)

residential_tail <- str_match(
  ALLN_residential$raw_line,
  residential_tail_pattern
)

ALLN_residential <- ALLN_residential |>
  mutate(
    
    floor_space_m2 =
      map_dbl(
        residential_tail[, 2],
        clean_alln_number
      ),
    
    apartments_1_1_5 =
      map_dbl(
        residential_tail[, 3],
        clean_alln_number
      ),
    
    apartments_2_2_5 =
      map_dbl(
        residential_tail[, 4],
        clean_alln_number
      ),
    
    apartments_3_3_5 =
      map_dbl(
        residential_tail[, 5],
        clean_alln_number
      ),
    
    apartments_4_4_5 =
      map_dbl(
        residential_tail[, 6],
        clean_alln_number
      ),
    
    apartments_5plus =
      map_dbl(
        residential_tail[, 7],
        clean_alln_number
      ),
    
    total_apartments =
      map_dbl(
        residential_tail[, 8],
        clean_alln_number
      ),
    
    other_uses_m2 =
      map_dbl(
        residential_tail[, 9],
        clean_alln_number
      ),
    
    target_rental_income_chf_m =
      map_dbl(
        residential_tail[, 10],
        clean_alln_number
      ),
    
    vacancy_rate_pct =
      map_dbl(
        residential_tail[, 11],
        clean_alln_number
      ),
    
    discount_cap_rate =
      residential_tail[, 12]
  )

# ============================================================
# 8. COMMERCIAL PROPERTY TEXT
# ============================================================

commercial_text <- paste(
  ALLN_pdf[3],
  ALLN_pdf[4],
  sep = "\n"
)

commercial_lines <- commercial_text |>
  str_split("\n") |>
  unlist() |>
  str_squish()

commercial_lines <- commercial_lines[
  commercial_lines != ""
]
# ============================================================
# 9. EXTRACT COMMERCIAL PROPERTY ROWS
# ============================================================

ALLN_commercial_raw <- tibble(
  raw_line = commercial_lines
) |>
  filter(
    str_detect(
      raw_line,
      "\\s(SO|LO)\\s"
    )
  )
# ============================================================
# 10. PARSE COMMERCIAL PROPERTY IDENTIFICATION
# ============================================================

ALLN_commercial <- ALLN_commercial_raw |>
  mutate(
    
    ownership_status = str_match(
      raw_line,
      "\\s(SO|LO)\\s"
    )[, 2],
    
    year_acquired = str_match(
      raw_line,
      "\\s(?:SO|LO)\\s+(\\d{4}(?:/\\d{4})?)"
    )[, 2],
    
    year_construction = str_match(
      raw_line,
      "\\s(?:SO|LO)\\s+\\d{4}(?:/\\d{4})?\\s+(\\d{4}(?:/\\d{2,4})*)"
    )[, 2],
    
    property_text = str_remove(
      raw_line,
      "\\s(?:SO|LO)\\s.*$"
    ) |>
      str_squish(),
    
    ticker = "ALLN",
    company = "Allreal Holding AG",
    property_type = "Commercial",
    report_date = as.Date("2025-12-31")
  )
ALLN_commercial <- ALLN_commercial |>
  mutate(
    
    location = str_extract(
      property_text,
      "^[^ ]+"
    ),
    
    address = str_remove(
      property_text,
      "^[^ ]+\\s+"
    ) |>
      str_squish()
  )
# ============================================================
# 11. PARSE COMMERCIAL USE + FINANCIAL VARIABLES
# ============================================================

commercial_tail_pattern <- paste0(
  
  # floor space
  "([0-9]+(?:\\s[0-9]{3})*)\\s+",
  
  # office %
  "([0-9]+(?:\\.[0-9]+)?)\\s+",
  
  # retail %
  "([0-9]+(?:\\.[0-9]+)?)\\s+",
  
  # residential %
  "([0-9]+(?:\\.[0-9]+)?)\\s+",
  
  # other %
  "([0-9]+(?:\\.[0-9]+)?)\\s+",
  
  # target rent
  "([0-9]+(?:\\.[0-9]+)?)\\s+",
  
  # vacancy
  "([0-9]+(?:\\.[0-9]+)?)\\s+",
  
  # discount / capitalisation
  "([0-9]+\\.[0-9]+/[0-9]+\\.[0-9]+)$"
)

commercial_tail <- str_match(
  ALLN_commercial$raw_line,
  commercial_tail_pattern
)

ALLN_commercial <- ALLN_commercial |>
  mutate(
    
    floor_space_m2 =
      map_dbl(
        commercial_tail[, 2],
        clean_alln_number
      ),
    
    office_pct =
      map_dbl(
        commercial_tail[, 3],
        clean_alln_number
      ),
    
    retail_pct =
      map_dbl(
        commercial_tail[, 4],
        clean_alln_number
      ),
    
    residential_pct =
      map_dbl(
        commercial_tail[, 5],
        clean_alln_number
      ),
    
    other_pct =
      map_dbl(
        commercial_tail[, 6],
        clean_alln_number
      ),
    
    target_rental_income_chf_m =
      map_dbl(
        commercial_tail[, 7],
        clean_alln_number
      ),
    
    vacancy_rate_pct =
      map_dbl(
        commercial_tail[, 8],
        clean_alln_number
      ),
    
    discount_cap_rate =
      commercial_tail[, 9]
  )

# ============================================================
# 12. CLEAN COMMERCIAL PROPERTY TABLE
# ============================================================

ALLN_commercial <- ALLN_commercial |>
  filter(
    !(location == "1" & address == "1")
  ) |>
  mutate(
    
    # Structural correction from PDF line parsing
    location = case_when(
      location == "Le" &
        str_detect(address, "^Grand-Saconnex") ~
        "Le Grand-Saconnex",
      
      TRUE ~ location
    ),
    
    address = case_when(
      location == "Le Grand-Saconnex" ~
        str_remove(address, "^Grand-Saconnex\\s+"),
      
      TRUE ~ address
    )
  )

# ============================================================
# 13. FUNCTION TO EXTRACT SITE AREA
# ============================================================

extract_site_area <- function(x) {
  
  extracted <- str_match(
    x,
    "([0-9]+(?:\\s[0-9]{3})*)\\s+(?:yes|no)\\b"
  )[, 2]
  
  extracted |>
    str_replace_all(" ", "") |>
    as.numeric()
}
ALLN_residential <- ALLN_residential |>
  mutate(
    site_area_m2 = extract_site_area(raw_line)
  )
ALLN_commercial <- ALLN_commercial |>
  mutate(
    site_area_m2 = extract_site_area(raw_line)
  )

# ============================================================
# 14. CORRECT SITE AREA VALUES AFFECTED BY PDF PARSING
# ============================================================

ALLN_residential <- ALLN_residential |>
  mutate(
    site_area_m2 = case_when(
      
      location == "Zurich" &
        str_detect(address, "Josefstrasse 137") ~
        903,
      
      location == "Genf" &
        str_detect(address, "Rue Edouard-Rod 10") ~
        248,
      
      location == "Grand-Lancy" &
        str_detect(address, "Chemin des Semailles 19") ~
        225,
      
      location == "Petit-Lancy" &
        str_detect(address, "Avenue du Cimetière") ~
        280,
      
      location == "Petit-Lancy" &
        str_detect(address, "Chemin des Pâquerettes 20") ~
        446,
      
      TRUE ~ site_area_m2
    )
  )
ALLN_commercial <- ALLN_commercial |>
  mutate(
    site_area_m2 = case_when(
      
      location == "Petit-Lancy" &
        str_detect(address, "Chemin des Olliquettes 4") ~
        963,
      
      TRUE ~ site_area_m2
    )
  )

# ============================================================
# 15. COMBINE ALLN RESIDENTIAL + COMMERCIAL PROPERTIES
# ============================================================

# Add IDs to commercial properties first
ALLN_commercial <- ALLN_commercial |>
  mutate(
    property_id = sprintf(
      "ALLN_C_%03d",
      row_number()
    ),
    report_property = paste(
      location,
      address,
      sep = ", "
    )
  )

# Combine both portfolios
ALLN_properties_2025 <- bind_rows(
  ALLN_residential,
  ALLN_commercial
)

# Check
ALLN_properties_2025 |>
  count(property_type)

nrow(ALLN_properties_2025)
# ============================================================
# 16. STANDARDIZE LOCATION NAMES
# ============================================================

ALLN_properties_2025 <- ALLN_properties_2025 |>
  mutate(
    city = case_when(
      location == "Genf" ~ "Geneva",
      TRUE ~ location
    )
  )
# ============================================================
# 17. CREATE SIMPLIFIED ADDRESS FOR GEOCODING
# ============================================================

ALLN_properties_2025 <- ALLN_properties_2025 |>
  mutate(
    
    # Keep only the first address before "/"
    simplified_address = str_extract(
      address,
      "^[^/]+"
    ),
    
    # Remove text in parentheses
    simplified_address = str_remove(
      simplified_address,
      "\\s*\\([^)]*\\)"
    ),
    
    # Clean spaces
    simplified_address = str_squish(
      simplified_address
    ),
    
    # Full address for geocoding
    simplified_full_address = paste(
      simplified_address,
      city,
      "Switzerland",
      sep = ", "
    )
  )


##### =========================================
# GEOCODING
##### =========================================

library(tidygeocoder)

ALLN_properties_geo <- ALLN_properties_2025 |>
  geocode(
    address = simplified_full_address,
    method = "osm",
    lat = latitude,
    long = longitude
  )

ALLN_properties_geo |>
  summarise(
    total_properties = n(),
    successfully_geocoded =
      sum(!is.na(latitude) & !is.na(longitude)),
    missing_coordinates =
      sum(is.na(latitude) | is.na(longitude))
  )
ALLN_properties_geo |>
  filter(
    is.na(latitude) |
      is.na(longitude)
  ) |>
  select(
    property_id,
    property_type,
    report_property,
    simplified_full_address
  ) |>
  print(n = 100)

# ============================================================
# 18. SECOND-PASS GEOCODING FOR FAILED ALLN PROPERTIES
# ============================================================

ALLN_properties_geo <- ALLN_properties_geo |>
  mutate(
    second_pass_address = case_when(
      
      property_id == "ALLN_R_001" ~
        "Ankerstrasse 23, Zurich, Switzerland",
      
      property_id == "ALLN_R_012" ~
        "Hohenstieglen 1, Glattbrugg, Switzerland",
      
      property_id == "ALLN_C_026" ~
        "Richtiplatz, Wallisellen, Switzerland",
      
      property_id == "ALLN_C_027" ~
        "Richtiring, Wallisellen, Switzerland",
      
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 19. GEOCODE SECOND-PASS ADDRESSES
# ============================================================

ALLN_second_pass <- ALLN_properties_geo |>
  filter(
    is.na(latitude) |
      is.na(longitude)
  ) |>
  select(
    property_id,
    second_pass_address
  ) |>
  geocode(
    address = second_pass_address,
    method = "osm",
    lat = latitude_new,
    long = longitude_new
  )

# ============================================================
# 20. MERGE SECOND-PASS COORDINATES
# ============================================================

ALLN_properties_geo <- ALLN_properties_geo |>
  left_join(
    ALLN_second_pass |>
      select(
        property_id,
        latitude_new,
        longitude_new
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
# 21. SAVE FINAL ALLN PROPERTY DATASET
# ============================================================

write_csv(
  ALLN_properties_geo,
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/DataTop3Properties/ALLN_properties_2025_final.csv"
)