# ============================================================
# PSPN PROPERTY MAPPING
# Interactive Leaflet Map
# ============================================================

library(tidyverse)
library(leaflet)

# ============================================================
# 1. IMPORT GEOCODED PSP PROPERTY DATA
# ============================================================

PSPN_properties_geo <- read_csv(
  "Groupwork dataset script/Data cleaning/Geo_Firms_Portfolios/DataTop3Properties/PSPN_properties_2025_final.csv",
  show_col_types = FALSE
)

# Quick checks
glimpse(PSPN_properties_geo)

PSPN_properties_geo |>
  summarise(
    properties = n(),
    missing_latitude = sum(is.na(latitude)),
    missing_longitude = sum(is.na(longitude))
  )

# ============================================================
# 2. INTERACTIVE PSP PORTFOLIO MAP
# ============================================================

PSPN_leaflet <- leaflet(
  data = PSPN_properties_geo
) |>
  
  addProviderTiles(
    providers$CartoDB.Positron
  ) |>
  
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    
    radius = 5,
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.75,
    
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
  )

PSPN_leaflet