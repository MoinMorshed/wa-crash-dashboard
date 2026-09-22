# Washington State Crash Analysis Dashboard

An interactive R Shiny dashboard for exploring statewide crash patterns in Washington using 2002 HSIS data (~50K crashes, ~85K vehicle records, ~45K roadway segments). Built as the term project for CE 465/565 at the University of Arizona.

**Live app:** https://moin1928.shinyapps.io/Crash_Dashboard/

![Dashboard screenshot](figures/dashboard.png)

## Features
- **Overview:** rear-end crash summary for the I-5, I-90, I-405, and SR-520 study corridors
- **Map:** geocoded crashes on the state route network with filters and clustering
- **Temporal Patterns:** crashes by month, day of week, and hour
- **Roadway Factors:** crash distribution by AADT, lanes, speed limit, and roadway type
- **Driver & Vehicle:** driver age, vehicle type, and vehicle condition breakdowns
- **Hotspots:** EPDO-ranked 0.5-mile segments with a profile panel linked to the map
- **About:** data sources and methodology

## Methods
- **Data pipeline:** accident, vehicle, and roadway tables queried from SQL Server and cached locally as RDS files
- **Linear referencing:** ~99.95% of crashes geocoded to the WSDOT 2002 state route network using route milepost (ARM); ramp and other non-mainline crashes are snapped to their parent route
- **Roadway enrichment:** non-equi range join attaches AADT and roadway attributes to ~92% of crashes
- **Hotspot screening:** 0.5-mile sliding windows (0.1-mile step) ranked by Equivalent Property Damage Only (EPDO) score, with crash rates per million vehicle-miles

EPDO weights: fatal or disabling injury = 542, non-disabling or possible injury = 11, no injury = 1.

## Tech stack
R · Shiny · bslib · leaflet · plotly · DT · sf · data.table · dplyr · SQL Server (RODBC)

## Project structure
```
app.R                  Shiny app (UI + server)
global.R               loads data, label lookups, and filter defaults
R/                     helper functions
  linear_reference.R     geocoding by route milepost
  hotspot_analysis.R     EPDO and sliding-window hotspot functions
  ui_components.R        reusable filter strips
  databaseConnector_v2.R SQL Server connection (credentials from .Renviron)
scripts/               data pipeline, run in order
  preprocess_01_pull.R     pull raw tables and cache as RDS
  preprocess_02_geocode.R  geocode crashes to the route network
  preprocess_03_enrich.R   attach roadway attributes
  preprocess_04_hotspots.R compute hotspot windows
data/                  raw and processed data (not included, see data/README.md)
.Renviron.example      template for database settings
```

## Running locally
1. Request the HSIS data (see [data/README.md](data/README.md)).
2. Copy `.Renviron.example` to `.Renviron` and fill in your own database settings.
3. Run the four scripts in `scripts/` in order from the project root.
4. Start the app:

```r
install.packages(c("shiny", "bslib", "leaflet", "leaflet.extras", "plotly", "DT", "sf", "data.table", "dplyr", "tidyr", "lubridate", "stringr", "shinyWidgets", "RODBC"))
shiny::runApp()
```

## Team
Developed by Moin Morshed and Yeji Jeon for CE 465/565 (instructors: Dr. Pramesh Pudasaini and Dr. Yao-Jan Wu), University of Arizona.
