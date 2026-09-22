library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)
library(plotly)
library(DT)
library(lubridate)
library(data.table)
library(tidyr)
library(shinyWidgets)
library(leaflet.extras)
library(stringr)

# Source helper modules 
cat("[STARTUP] sourcing helpers...\n")
source("R/linear_reference.R", local = TRUE)
source("R/hotspot_analysis.R", local = TRUE)
source("R/ui_components.R", local = TRUE)
cat("[STARTUP] helpers OK\n")

# Load processed data 
cat("[STARTUP] loading crashes_enriched.rds...\n")
crashes           <- readRDS("data/processed/crashes_enriched.rds")
cat("[STARTUP] loading road_attrs.rds...\n")
roads             <- readRDS("data/processed/road_attrs.rds")
cat("[STARTUP] loading hotspots.rds...\n")
hotspots_baseline <- readRDS("data/processed/hotspots.rds")
cat("[STARTUP] load_boundary()...\n")
wa_boundary       <- load_boundary()
cat("[STARTUP] load_routes()...\n")
routes_sf         <- load_routes()
cat("[STARTUP] data files loaded\n")

# Geometry-free flat frame used in every reactive (avoids repeated st_drop)
crashes_flat  <- sf::st_drop_geometry(crashes)
TOTAL_CRASHES <- nrow(crashes_flat)

# Join vehicle-level trf_cntl (integer HSIS codes) — one row per crash
cat("[STARTUP] loading vehicles_raw.rds...\n")
veh_trf_lookup <- readRDS("data/raw/vehicles_raw.rds") %>%
  dplyr::select(caseno, trf_cntl) %>%
  dplyr::group_by(caseno) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(caseno,
                   veh_trf_cntl = suppressWarnings(as.integer(trf_cntl)))

crashes_flat <- crashes_flat %>%
  dplyr::left_join(veh_trf_lookup, by = "caseno")

# Full vehicle table for driver/vehicle tab reactive join
vehicles <- readRDS("data/raw/vehicles_raw.rds")
cat("[STARTUP] vehicles OK\n")

# Label lookups (HSIS WA data dictionary) 
severity_labels <- c(
  "0" = "Not stated",
  "1" = "No injury",
  "2" = "Fatal",
  "3" = "Unknown/Other",
  "4" = "Unknown/Other",
  "5" = "Disabling injury",
  "6" = "Non-disabling injury",
  "7" = "Possible injury"
)

# Global severity color palette — numeric-keyed, used by map markers & legend.
severity_colors <- c(
  "0" = "#c7c7c7",  # Not stated
  "1" = "#2ca02c",  # No injury (PDO)
  "2" = "#d62728",  # Fatal
  "3" = "#7f7f7f",  # Unknown/Other
  "4" = "#7f7f7f",  # Unknown/Other
  "5" = "#ff7f0e",  # Disabling injury
  "6" = "#fbc6a9",  # Non-disabling injury
  "7" = "#98df8a"   # Possible injury
)

# "0" added — appears in data as "not stated" weather code
weather_labels <- c(
  "0" = "Not stated",
  "1" = "Clear",
  "2" = "Overcast",
  "3" = "Raining",
  "4" = "Snowing",
  "5" = "Fog / smog",
  "6" = "Sleet / freezing rain",
  "7" = "Severe crosswind",
  "8" = "Blowing sand / dirt",
  "9" = "Other"
)

# "7" added — appears in data but was missing from previous version
light_labels <- c(
  "1" = "Daylight",
  "2" = "Dawn",
  "3" = "Dusk",
  "4" = "Dark – street lights on",
  "5" = "Dark – no street lights",
  "6" = "Dark – unknown lighting",
  "7" = "Other",
  "9" = "Unknown"
)

rdsurf_labels <- c(
  "1" = "Dry",
  "2" = "Wet",
  "3" = "Snow / slush",
  "4" = "Ice",
  "5" = "Sand / mud / dirt",
  "6" = "Water (standing)",
  "9" = "Unknown"
)

trf_cntl_labels <- c(
  "0" = "Not stated",
  "1" = "Signals",
  "2" = "Stop sign",
  "3" = "Yield sign",
  "4" = "Flashing red",
  "5" = "Flashing amber",
  "6" = "Railroad signal",
  "7" = "Officer/Flagger",
  "8" = "Other",
  "9" = "No control"
)

# Comprehensive mapping covering all codes in collision_groups plus
# additional codes seen in HSIS WA. make_choices() falls back to
# "Code X" for any value not listed here.
acctype_labels <- c(
  "0"  = "Pedestrian – not stated",
  "1"  = "Ped – vehicle going straight",
  "2"  = "Ped – vehicle turning",
  "3"  = "Ped – backing vehicle",
  "4"  = "Ped – parked vehicle",
  "5"  = "Ped – other",
  "10" = "Angle",
  "11" = "Sideswipe – same dir, overtaking",
  "12" = "Sideswipe – same dir, other",
  "13" = "Rear-end – going straight",
  "14" = "Rear-end – slowing / stopped",
  "15" = "Right turn – same direction",
  "16" = "Left turn – same direction",
  "19" = "Parked vehicle – straight",
  "20" = "Parked vehicle – other",
  "21" = "Driveway – entering",
  "22" = "Driveway – leaving",
  "23" = "Other same direction",
  "24" = "Head-on",
  "25" = "Head-on – other",
  "26" = "Opp dir – left turn",
  "27" = "Opp dir – right turn",
  "28" = "Opp dir – sideswipe",
  "29" = "Opp dir – backing",
  "30" = "Opp dir – other",
  "31" = "Not stated",
  "32" = "Parked vehicle – angle",
  "40" = "Train – at crossing",
  "41" = "Train – other",
  "42" = "Train – equipment",
  "43" = "Train – other (multi-veh)",
  "44" = "Pedalcyclist – vehicle straight",
  "45" = "Pedalcyclist – vehicle turning",
  "46" = "Pedalcyclist – other",
  "47" = "Animal – livestock",
  "48" = "Animal – deer",
  "49" = "Animal – other",
  "50" = "Fixed object",
  "51" = "Other object",
  "52" = "Overturned",
  "53" = "Fire / explosion",
  "54" = "Cargo / equipment loss",
  "55" = "Fell from vehicle",
  "56" = "Ran off road",
  "57" = "Non-collision – other",
  "71" = "Sideswipe same dir (3-veh)",
  "72" = "Sideswipe same dir – other (3-veh)",
  "73" = "Rear-end same dir (3-veh)",
  "74" = "Rear-end same dir – other (3-veh)",
  "81" = "Sideswipe same dir (4+ veh)",
  "82" = "Sideswipe same dir – other (4+ veh)",
  "83" = "Rear-end same dir (4+ veh)",
  "84" = "Rear-end same dir – other (4+ veh)"
)

# HSIS 2002 functional class codes (P2Dictionary)
funccls_labels <- c(
  "1"  = "Rural Interstate",
  "2"  = "Rural Principal Arterial",
  "6"  = "Rural Minor Arterial",
  "7"  = "Rural Collector",
  "9"  = "Rural Unclassified",
  "11" = "Urban Interstate",
  "12" = "Urban Freeway / Expressway",
  "14" = "Urban Principal Arterial",
  "16" = "Urban Minor Arterial",
  "17" = "Urban Collector",
  "19" = "Urban Unclassified"
)

drv_sex_labels <- c(
  "0" = "Not stated",
  "1" = "Male",
  "2" = "Female"
)

contrib_labels <- c(
  "01" = "Under influence of alcohol",
  "02" = "Under influence of drugs",
  "03" = "Exceeded speed limit",
  "04" = "Exceeded safe speed",
  "05" = "Failed to yield ROW",
  "06" = "Improper passing",
  "07" = "Following too closely",
  "08" = "Over centerline",
  "09" = "Failing to signal",
  "10" = "Improper turning",
  "11" = "Disregarded stop/go light",
  "12" = "Disregarded stop sign",
  "13" = "Disregarded warning signal",
  "14" = "Apparently asleep",
  "15" = "Improper parking",
  "16" = "Operating defective equipment",
  "17" = "Other",
  "18" = "No violation",
  "19" = "Improper signal",
  "20" = "Improper U-turn",
  "21" = "Headlight violation",
  "22" = "Failed to yield to ped/cyclist",
  "23" = "Inattention"
)

drv_actn_labels <- c(
  "01" = "Going straight",
  "02" = "Overtaking/passing",
  "03" = "Making right turn",
  "04" = "Making left turn",
  "05" = "Making U-turn",
  "06" = "Slowing",
  "07" = "Stopped for traffic",
  "08" = "Stopped at signal/sign",
  "09" = "Stopped in roadway",
  "10" = "Starting in traffic lane",
  "11" = "Starting from parked",
  "12" = "Merging into traffic",
  "13" = "Legally parked, occupied",
  "14" = "Legally parked, unoccupied",
  "15" = "Backing",
  "16" = "Wrong way on divided hwy",
  "17" = "Wrong way on ramp",
  "18" = "Wrong way on one-way",
  "19" = "Other",
  "20" = "Changing lanes"
)

vehtype_labels <- c(
  "00" = "Not stated",
  "01" = "Passenger car",
  "02" = "Pickup/panel truck",
  "03" = "Van/flatbed",
  "04" = "Truck >10K lbs",
  "05" = "Truck tractor",
  "06" = "Truck tractor + semi",
  "07" = "Other truck combination",
  "08" = "Farm equipment",
  "09" = "Taxi",
  "10" = "Bus/motor stage",
  "11" = "School bus",
  "12" = "Motorcycle",
  "13" = "Scooter",
  "14" = "Other",
  "15" = "Moped"
)

vehcond_labels <- c(
  "01" = "Defective brakes",
  "02" = "Defective headlights",
  "03" = "Defective rear lights",
  "04" = "Tires worn/smooth",
  "05" = "Tires punctured/blown",
  "06" = "Lost a wheel",
  "07" = "Defective steering",
  "08" = "Power failure",
  "09" = "Headlights glaring",
  "10" = "Other lights/reflectors insufficient",
  "11" = "Other defects",
  "12" = "No defects",
  "13" = "Motorcycle lights off",
  "14" = "Studded tires",
  "15" = "Motorcycle windshield installed",
  "16" = "Truck safety inspection"
)

# Collision type groups
# Group names shown in sidebar; server expands selected names to integer codes
collision_groups <- list(
  "Pedestrian/Vehicle"        = c("0","1","2","3","4","5"),
  "Angle"                     = c("10"),
  "Rear-end, same direction"  = c("13","14","73","74","83","84"),
  "Sideswipe, same direction" = c("11","12","71","72","81","82"),
  "Turning, same direction"   = c("15","16"),
  "Parked vehicle"            = c("19","20","32"),
  "Driveway"                  = c("21","22"),
  "Other same direction"      = c("23"),
  "Head-on"                   = c("24","25"),
  "Opposite direction, other" = c("26","27","28","29","30"),
  "Train"                     = c("40","41","42","43"),
  "Pedalcyclist"              = c("44","45","46"),
  "Animal"                    = c("47","48","49"),
  "Object"                    = c("50","51"),
  "Non-collision"             = c("52","53","54","55","56","57"),
  "Not stated"                = c("31")
)

# Per-tab severity choices & filter helper 
severity_tab_choices <- c(
  "All severities"      = "all",
  "Fatal (K)"           = "2",
  "Disabling (A)"       = "5",
  "Non-disabling (B)"   = "6",
  "Possible injury (C)" = "7",
  "No injury (PDO)"     = "1",
  "Injury crashes"      = "injury"
)

apply_severity_filter <- function(data, severity_input) {
  if (is.null(severity_input) || length(severity_input) == 0 || severity_input == "all") {
    return(data)
  } else if (severity_input == "injury") {
    return(data %>% dplyr::filter(severity %in% c(2L, 5L, 6L, 7L)))
  } else {
    sev_code <- as.integer(severity_input)
    return(data %>% dplyr::filter(severity == sev_code))
  }
}

# Sidebar dropdown choices
# Returns setNames(codes, labels); codes not in labels_vec get "Code X" name
make_choices <- function(col_vals, labels_vec) {
  vals <- sort(unique(col_vals[!is.na(col_vals)]))
  keys <- as.character(vals)
  nms  <- ifelse(keys %in% names(labels_vec), labels_vec[keys], paste("Code", keys))
  setNames(keys, nms)
}

severity_choices <- local({
  raw         <- make_choices(crashes_flat$severity, severity_labels)
  unk_present <- any(raw %in% c("3", "4"))
  if (!unk_present) return(raw)
  core   <- raw[!raw %in% c("3", "4")]
  merged <- setNames("3_4", "Unknown/Other")
  c(core, merged)
})
weather_choices  <- make_choices(crashes_flat$weather,  weather_labels)
light_choices    <- make_choices(crashes_flat$light,    light_labels)
funccls_choices  <- make_choices(crashes_flat$func_cls, funccls_labels)

# Washington State official route designation helper
wa_interstates <- c(5, 82, 90, 182, 405, 509, 705)
wa_us_routes   <- c(2, 12, 20, 26, 30, 97, 101, 195, 197, 395, 730)

route_label <- function(rte_nbr) {
  rte_num <- suppressWarnings(as.integer(rte_nbr))
  dplyr::case_when(
    rte_num %in% wa_interstates ~ paste0("I-",  rte_num),
    rte_num %in% wa_us_routes   ~ paste0("US-", rte_num),
    TRUE                         ~ paste0("SR-", rte_num)
  )
}

make_route_choices <- function(data) {
  data %>%
    dplyr::count(rte_nbr, sort = TRUE) %>%
    dplyr::filter(!is.na(rte_nbr)) %>%
    dplyr::mutate(label = paste0(
      route_label(rte_nbr),
      " (", formatC(n, format = "d", big.mark = ","), " crashes)"
    )) %>%
    { stats::setNames(as.character(.$rte_nbr), .$label) }
}

# Routes sorted by crash count DESCENDING (heaviest first)
route_choices <- make_route_choices(crashes_flat)

# Project 2 rear-end crash study corridors. These are used only as the
# Overview tab defaults; the other dashboard tabs keep their own independent filters
project2_route_numbers <- c("5", "90", "405", "520")
project2_route_default <- intersect(project2_route_numbers, unname(route_choices))
project2_collision_default <- "Rear-end, same direction"

# Sensible default selections 
severity_default <- character(0)               
route_default    <- project2_route_default
funccls_default  <- unname(funccls_choices)          
#weather_default  <- c("1", "2", "3", "4")           
#light_default    <- c("1", "4", "5")                
#colltype_default <- c("Angle", "Rear-end, same direction",
                      #"Head-on", "Sideswipe, same direction")
weather_default  <- unname(weather_choices)
light_default    <- unname(light_choices)
colltype_default <- project2_collision_default


# Startup  
cat(" WA Crash Dashboard — startup complete \n")
cat(sprintf("  Crashes loaded    : %6d    \n", TOTAL_CRASHES))
cat(sprintf("  Road segments     : %6d    \n", nrow(roads)))
cat(sprintf("  Hotspot windows   : %6d    \n", nrow(hotspots_baseline)))

