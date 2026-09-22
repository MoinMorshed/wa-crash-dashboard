if (!nzchar(Sys.getenv("SHINY_PORT"))) {

library(sf)
library(dplyr)

source("R/linear_reference.R")

SHAPEFILE <- "data/shapefile/WSDOT_-_State_Route_Line_(1_500K)_2002.shp"

cat("Phase 3: Geocode Crashes\n\n")

# 1. Load accidents 
cat("[1] Loading accidents_raw.rds...\n")
accidents <- readRDS("data/raw/accidents_raw.rds")
cat(sprintf("    %d accidents loaded\n\n", nrow(accidents)))

# 2. Load shapefile 
cat("[2] Loading shapefile...\n")
routes_sf <- load_routes(SHAPEFILE)
cat(sprintf("    %d total segments  |  %d mainline (RelRouteTy is NA)\n\n",
            nrow(routes_sf),
            sum(is.na(routes_sf$RelRouteTy))))

# 3. Split mainline / non-mainline
cat("[3] Splitting accidents...\n")
mainline    <- accidents %>% filter(rd_type == "ML")
nonmainline <- accidents %>% filter(rd_type != "ML")
cat(sprintf("    Mainline: %d  |  Non-mainline: %d\n\n",
            nrow(mainline), nrow(nonmainline)))

#4. Geocode mainline 
cat("[4] Geocoding mainline crashes...\n")
mainline_sf <- geocode_crashes(mainline, routes_sf)
cat("\n")

# 5. Snap non-mainline 
cat("[5] Snapping non-mainline crashes to parent route...\n")
nonmainline_sf <- snap_nonmainline_crashes(nonmainline, routes_sf)
cat("\n")

# 6. Combine 
cat("[6] Combining results...\n")
# Verify CRS matches before rbind; transform nonmainline if not
if (!identical(st_crs(mainline_sf), st_crs(nonmainline_sf))) {
  warning("CRS mismatch — transforming nonmainline_sf to match mainline_sf")
  nonmainline_sf <- st_transform(nonmainline_sf, st_crs(mainline_sf))
}
crashes_geocoded <- rbind(mainline_sf, nonmainline_sf)   # rbind preserves sf class
cat(sprintf("    %d total geocoded\n\n", nrow(crashes_geocoded)))

# 7. Diagnose unmatched 
cat("[7] Diagnosing unmatched accidents...\n")
unmatched_summary <- diagnose_unmatched(accidents, crashes_geocoded)
if (nrow(unmatched_summary) == 0) {
  cat("    All accidents geocoded successfully!\n\n")
} else {
  cat(sprintf("    %d accidents unmatched — breakdown:\n",
              sum(unmatched_summary$count)))
  print(as.data.frame(unmatched_summary), row.names = FALSE)
  cat("\n")
}

# 8. Save
cat("[8] Saving crashes_geocoded.rds...\n")
saveRDS(crashes_geocoded, "data/processed/crashes_geocoded.rds")
cat("\n")

#  9. Summary 
total_in  <- nrow(accidents)
total_out <- nrow(crashes_geocoded)
bb        <- st_bbox(crashes_geocoded)
fsize     <- file.info("data/processed/crashes_geocoded.rds")$size

cat("GEOCODE SUMMARY \n")
cat(sprintf("  Input accidents            : %d\n", total_in))
cat(sprintf("  Successfully geocoded      : %d  (%.1f%%)\n",
            total_out, 100 * total_out / total_in))
cat(sprintf("  Unmatched                  : %d  (%.1f%%)\n",
            total_in - total_out,
            100 * (total_in - total_out) / total_in))
cat("\n  By geocode_method:\n")
method_tbl <- crashes_geocoded %>%
  st_drop_geometry() %>%
  count(geocode_method, sort = TRUE)
print(as.data.frame(method_tbl), row.names = FALSE)
cat(sprintf("\n  Bounding box:\n"))
cat(sprintf("    lon : %.4f  to  %.4f\n", bb["xmin"], bb["xmax"]))
cat(sprintf("    lat : %.4f  to  %.4f\n", bb["ymin"], bb["ymax"]))
cat(sprintf("\n  crashes_geocoded.rds : %.1f MB\n", fsize / 1e6))

cat("Phase 3 complete. Ready for Phase 4 (road attribute enrichment).\n")

} 
