if (!nzchar(Sys.getenv("SHINY_PORT"))) {

library(RODBC)
library(dplyr)
library(lubridate)

source("R/databaseConnector_v2.R")

stage <- "initializing"

tryCatch({

  # 1. Connect 
  stage <- "connecting to SQL"
  cat("Phase 2: Pull & Cache Raw Data \n\n")
  cat("[1] Connecting to SQL...\n")

  conn <- getSQLConnection(
    UserName = Sys.getenv("SQL_USER"),
    Password = Sys.getenv("SQL_PWD"),
    Database = Sys.getenv("SQL_DB")
  )
  on.exit(try(odbcClose(conn), silent = TRUE), add = TRUE)
  cat("    Connected OK\n\n")

  # 2. Pull raw tables 
  stage <- "pulling Accident_raw$"
  cat("[2] Pulling Accident_raw$...\n")
  accidents_raw <- sqlQuery(conn,
                            "SELECT * FROM [dbo].[Accident_raw$]",
                            stringsAsFactors = FALSE)
  cat(sprintf("    %d rows pulled\n\n", nrow(accidents_raw)))

  stage <- "pulling Vehicle UNION ALL"
  cat("[3] Pulling Vehicle_raw$ UNION ALL Vehicle2_raw$...\n")
  vehicles_raw <- sqlQuery(conn,
    "SELECT * FROM [dbo].[Vehicle_raw$]
     UNION ALL
     SELECT * FROM [dbo].[Vehicle2_raw$]",
    stringsAsFactors = FALSE)
  cat(sprintf("    %d rows pulled\n\n", nrow(vehicles_raw)))

  stage <- "pulling Road_raw$"
  cat("[4] Pulling Road_raw$...\n")
  roads_raw <- sqlQuery(conn,
                        "SELECT * FROM [dbo].[Road_raw$]",
                        stringsAsFactors = FALSE)
  cat(sprintf("    %d rows pulled\n\n", nrow(roads_raw)))

  # Connection released via on.exit; close explicitly here for clarity
  odbcClose(conn)
  cat("[5] SQL connection closed\n\n")

  # 3. Lowercase column names 
  stage <- "lowercasing column names"
  cat("[6] Standardizing column names (lowercase)...\n")
  names(accidents_raw) <- tolower(names(accidents_raw))
  names(vehicles_raw)  <- tolower(names(vehicles_raw))
  names(roads_raw)     <- tolower(names(roads_raw))
  cat("    Done\n\n")

  # 4. Datetime fields 
  stage <- "constructing datetime fields"
  cat("[7] Constructing crash_date / crash_hour / crash_datetime...\n")

  accidents_raw <- accidents_raw %>%
    mutate(
      crash_date = make_date(
        year  = as.integer(accyr),
        month = as.integer(month),
        day   = as.integer(daymth)
      ),

      # TIME is HHMM integer; guard against NA / 0 / 2400+
      time_int = suppressWarnings(as.integer(time)),
      time_int = if_else(!is.na(time_int) & time_int >= 0 & time_int < 2400,
                         time_int, NA_integer_),

      crash_hour   = as.integer(time_int %/% 100),
      crash_minute = as.integer(time_int %%  100),

      # Clamp any impossible minute values (e.g. 99)
      crash_minute = if_else(!is.na(crash_minute) & crash_minute < 60,
                             crash_minute, NA_integer_),

      crash_datetime = if_else(
        !is.na(crash_date) & !is.na(crash_hour) & !is.na(crash_minute),
        as.POSIXct(crash_date) +
          as.difftime(crash_hour,   units = "hours") +
          as.difftime(crash_minute, units = "mins"),
        as.POSIXct(NA)
      )
    ) %>%
    select(-time_int)   # drop scratch column

  cat(sprintf("    crash_date range: %s to %s\n",
              min(accidents_raw$crash_date, na.rm = TRUE),
              max(accidents_raw$crash_date, na.rm = TRUE)))
  cat(sprintf("    NA crash_datetime: %d rows\n\n",
              sum(is.na(accidents_raw$crash_datetime))))

  # 5. Weekday name
  stage <- "adding weekday_name"
  cat("[8] Adding weekday_name factor...\n")
  accidents_raw <- accidents_raw %>%
    mutate(weekday_name = lubridate::wday(crash_date, label = TRUE, week_start = 1))
  cat("    Done\n\n")

  # 6. Type cleanup
  stage <- "type cleanup"
  cat("[9] Cleaning column types...\n")

  int_cols_acc <- c("severity", "acctype1", "rdsurf", "weather",
                    "light", "func_cls", "loc_type", "numvehs")
  num_cols_acc <- c("milepost", "ac_srmp")

  for (col in int_cols_acc) {
    if (col %in% names(accidents_raw))
      accidents_raw[[col]] <- suppressWarnings(as.integer(accidents_raw[[col]]))
  }
  for (col in num_cols_acc) {
    if (col %in% names(accidents_raw))
      accidents_raw[[col]] <- suppressWarnings(as.numeric(accidents_raw[[col]]))
  }

  int_cols_road <- c("no_lanes")
  num_cols_road <- c("begmp", "endmp", "aadt", "spd_limt")

  for (col in int_cols_road) {
    if (col %in% names(roads_raw))
      roads_raw[[col]] <- suppressWarnings(as.integer(roads_raw[[col]]))
  }
  for (col in num_cols_road) {
    if (col %in% names(roads_raw))
      roads_raw[[col]] <- suppressWarnings(as.numeric(roads_raw[[col]]))
  }

  cat("    Done\n\n")

  # 7. Save RDS 
  stage <- "saving RDS files"
  cat("[10] Saving RDS files to data/raw/...\n")
  saveRDS(accidents_raw, "data/raw/accidents_raw.rds")
  saveRDS(vehicles_raw,  "data/raw/vehicles_raw.rds")
  saveRDS(roads_raw,     "data/raw/roads_raw.rds")
  cat("     Saved accidents_raw.rds, vehicles_raw.rds, roads_raw.rds\n\n")

  # 8. Summary 
  cat("PULL SUMMARY \n")

  # Row counts
  cat(sprintf("  accidents_raw   : %d rows\n",   nrow(accidents_raw)))
  cat(sprintf("  vehicles_raw    : %d rows\n",   nrow(vehicles_raw)))
  cat(sprintf("  roads_raw       : %d rows\n\n", nrow(roads_raw)))

  # Date range
  cat(sprintf("  crash_date range: %s  to  %s\n\n",
              min(accidents_raw$crash_date, na.rm = TRUE),
              max(accidents_raw$crash_date, na.rm = TRUE)))

  # Unique CASENOs
  acc_cases <- length(unique(accidents_raw$caseno))
  veh_cases <- length(unique(vehicles_raw$caseno))
  matched   <- sum(unique(accidents_raw$caseno) %in% unique(vehicles_raw$caseno))
  cat(sprintf("  Unique CASENOs — accidents : %d\n", acc_cases))
  cat(sprintf("  Unique CASENOs — vehicles  : %d\n", veh_cases))
  cat(sprintf("  Accidents with vehicle match: %d  (%.1f%%)\n\n",
              matched, 100 * matched / acc_cases))

  # Unique routes
  cat(sprintf("  Unique RTE_NBR — accidents : %d\n",
              length(unique(accidents_raw$rte_nbr))))
  if ("road_inv" %in% names(roads_raw))
    cat(sprintf("  Unique ROAD_INV — roads    : %d\n\n",
                length(unique(roads_raw$road_inv))))

  # Top 10 routes by crash count
  cat("  Top 10 routes by crash count:\n")
  top_routes <- accidents_raw %>%
    count(rte_nbr, sort = TRUE) %>%
    slice_head(n = 10)
  print(as.data.frame(top_routes), row.names = FALSE)
  cat("\n")

  # File sizes
  sizes <- file.info(c("data/raw/accidents_raw.rds",
                        "data/raw/vehicles_raw.rds",
                        "data/raw/roads_raw.rds"))$size
  cat(sprintf("  File sizes:\n"))
  cat(sprintf("    accidents_raw.rds : %.1f MB\n", sizes[1] / 1e6))
  cat(sprintf("    vehicles_raw.rds  : %.1f MB\n", sizes[2] / 1e6))
  cat(sprintf("    roads_raw.rds     : %.1f MB\n", sizes[3] / 1e6))

  cat("Phase 2 complete. RDS files ready for Phase 3.\n")

}, error = function(e) {
  cat(sprintf("\n[FAIL] Error during stage '%s':\n  %s\n", stage, conditionMessage(e)))
})

}
