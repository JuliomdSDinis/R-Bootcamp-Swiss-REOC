# ============================================================
# TOP 3 PROPERTY MAPPING
# Interactive Leaflet Map
# PSPN + SPSN
# ============================================================

library(tidyverse)
library(leaflet)

# ============================================================
# 1. IMPORT GEOCODED PROPERTY DATA
# ============================================================

# PSPN
PSPN_properties_geo <- read_csv(
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/DataTop3Properties/PSPN_properties_2025_final.csv",
  show_col_types = FALSE
)

# SPSN
SPSN_properties_geo <- read_csv(
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/DataTop3Properties/SPSN_properties_2025_final.csv",
  show_col_types = FALSE
)

# ALLN
ALLN_properties_geo <- read_csv(
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/DataTop3Properties/ALLN_properties_2025_final.csv",
)
# ============================================================
# 2. QUICK CHECKS
# ============================================================

PSPN_properties_geo |>
  summarise(
    company = "PSPN",
    properties = n(),
    missing_coordinates =
      sum(is.na(latitude) | is.na(longitude))
  )

SPSN_properties_geo |>
  summarise(
    company = "SPSN",
    properties = n(),
    missing_coordinates =
      sum(is.na(latitude) | is.na(longitude))
  )

ALLN_properties_geo |>
  summarise(
    company = "ALLN",
    properties = n(),
    missing_coordinates =
      sum(is.na(latitude) | is.na(longitude))
  )
# ============================================================
# 3. INTERACTIVE PROPERTY MAP
# ============================================================

Top3_leaflet <- leaflet() |>
  
  addProviderTiles(
    providers$CartoDB.Positron
  ) |>
  
  
  # ----------------------------------------------------------
# PSPN - BLUE
# ----------------------------------------------------------

addCircleMarkers(
  data = PSPN_properties_geo,
  
  lng = ~longitude,
  lat = ~latitude,
  
  radius = 5,
  color = "blue",
  fillColor = "blue",
  stroke = TRUE,
  weight = 1,
  fillOpacity = 0.75,
  
  group = "PSP Swiss Property",
  
  label = ~paste0(
    address,
    " — ",
    city
  ),
  
  popup = ~paste0(
    "<b>", address, "</b><br>",
    city, "<br><br>",
    "<b>Company:</b> ", company, "<br>",
    "<b>Property ID:</b> ", property_id
  )
) |>
  
  
  # ----------------------------------------------------------
# SPSN - RED
# ----------------------------------------------------------

addCircleMarkers(
  data = SPSN_properties_geo,
  
  lng = ~longitude,
  lat = ~latitude,
  
  radius = 5,
  color = "red",
  fillColor = "red",
  stroke = TRUE,
  weight = 1,
  fillOpacity = 0.75,
  
  group = "Swiss Prime Site",
  
  label = ~paste0(
    address,
    " — ",
    city
  ),
  
  popup = ~paste0(
    "<b>", address, "</b><br>",
    city, "<br><br>",
    "<b>Company:</b> ", company, "<br>",
    "<b>Property ID:</b> ", property_id
  )
) |>
  
  # ----------------------------------------------------------
# ALLN - GREEN
# ----------------------------------------------------------

addCircleMarkers(
  data = ALLN_properties_geo,
  
  lng = ~longitude,
  lat = ~latitude,
  
  radius = 5,
  color = "green",
  fillColor = "green",
  stroke = TRUE,
  weight = 1,
  fillOpacity = 0.75,
  
  group = "Allreal",
  
  label = ~paste0(
    address,
    " — ",
    city
  ),
  
  popup = ~paste0(
    "<b>", address, "</b><br>",
    city, "<br><br>",
    "<b>Company:</b> ", company, "<br>",
    "<b>Property ID:</b> ", property_id, "<br>",
    "<b>Property type:</b> ", property_type
  )
) |>

  # ==========================================================
# 4. LAYER CONTROL
# ==========================================================

addLayersControl(
  overlayGroups = c(
    "PSP Swiss Property",
    "Swiss Prime Site",
    "Allreal"
  ),
  
  options = layersControlOptions(
    collapsed = FALSE
  )
)


# ============================================================
# 5. DISPLAY MAP
# ============================================================

Top3_leaflet