if (!nzchar(Sys.getenv("SHINY_PORT"))) {

library(sf)
library(dplyr)
library(data.table)

cat("Phase 4: Road Attribute Enrichment \n\n")

# 1. Read inputs 
cat("[1] Reading inputs...\n")
crashes <- readRDS("data/processed/crashes_geocoded.rds")
roads   <- readRDS("data/raw/roads_raw.rds")
cat(sprintf("    crashes : %d rows\n",   nrow(crashes)))
cat(sprintf("    roads   : %d rows\n\n", nrow(roads)))

# 2. Prepare road attribute table 
cat("[2] Preparing road attribute table...\n")

# Check which optional columns exist before selecting
optional_cols <- c("no_lane1", "no_lane2")
present_cols  <- intersect(optional_cols, names(roads))
core_cols     <- c("road_inv", "begmp", "endmp",
                   "aadt", "spd_limt", "no_lanes",
                   "surf_typ", "terrain", "func_cls", "trf_cntl",
                   "rd_type", "rte_nbr")
select_cols   <- c(core_cols, present_cols)

road_attrs <- roads %>%
  select(all_of(select_cols)) %>%
  rename_with(
    ~ paste0("road_", .x),
    .cols = intersect(c("aadt", "spd_limt", "no_lanes", "no_lane1", "no_lane2",
                        "surf_typ", "terrain", "func_cls", "trf_cntl", "rd_type"),
                      names(.))
  )

cat(sprintf("    %d segments, %d columns: %s\n\n",
            nrow(road_attrs), ncol(road_attrs),
            paste(names(road_attrs), collapse = ", ")))

# 3. Range join via data.table
cat("[3] Performing non-equi range join (data.table)...\n")

crashes_dt <- as.data.table(st_drop_geometry(crashes))
roads_dt   <- as.data.table(road_attrs)

# Pre-compute segment length and preserve real begmp/endmp as named columns
# BEFORE the join — data.table replaces inequality key columns with i-side values, so begmp/endmp in the result would both equal milepost otherwise.
roads_dt[, `:=`(
  road_begmp        = begmp,
  road_endmp        = endmp,
  segment_length_mi = endmp - begmp
)]

# i-join: for every crash, find the first matching road segment
# roads_dt$road_inv == crashes_dt$road_inv
# AND roads_dt$begmp <= crashes_dt$milepost
# AND roads_dt$endmp  > crashes_dt$milepost
enriched_dt <- roads_dt[
  crashes_dt,
  on           = .(road_inv, begmp <= milepost, endmp > milepost),
  allow.cartesian = TRUE,
  mult         = "first"
]

cat(sprintf("    %d rows returned\n", nrow(enriched_dt)))
cat(sprintf("    Columns after join:\n      %s\n\n",
            paste(names(enriched_dt), collapse = ", ")))

# Drop the join-key artefact columns (begmp/endmp now hold milepost values)
# Keep road_begmp / road_endmp which hold the real segment bounds
cols_to_drop <- intersect(c("begmp", "endmp"), names(enriched_dt))
if (length(cols_to_drop)) enriched_dt[, (cols_to_drop) := NULL]

# 4. Re-attach geometry 
# milepost and geocode_method are carried from Phase 3 directly — data.table's
# non-equi join drops the i-side inequality column (milepost), so it must be
# re-sourced here rather than relying on enriched_dt.
cat("[4] Re-attaching geometry...\n")
enriched_sf <- crashes %>%
  select(caseno, milepost, geocode_method, geometry) %>%
  left_join(as_tibble(enriched_dt), by = "caseno") %>%
  # Guard: drop .y duplicates if data.table left any behind
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
  select(-ends_with(".y")) %>%
  st_as_sf(crs = st_crs(crashes))

cat(sprintf("    %d rows, %d columns (incl. geometry)\n\n",
            nrow(enriched_sf), ncol(enriched_sf)))

# 5. Diagnostics 
cat("[5] Diagnostics...\n")

n_in       <- nrow(crashes)
n_out      <- nrow(enriched_sf)
n_matched  <- sum(!is.na(enriched_sf$road_aadt))
n_missing  <- sum(is.na(enriched_sf$road_aadt))

cat(sprintf("    Total crashes in       : %d\n", n_in))
cat(sprintf("    Total crashes out      : %d\n", n_out))
cat(sprintf("    Road segment matched   : %d  (%.1f%%)\n",
            n_matched, 100 * n_matched / n_out))
cat(sprintf("    No road match (NA aadt): %d  (%.1f%%)\n\n",
            n_missing, 100 * n_missing / n_out))

# NA breakdown by geocode_method
cat("    NA road_aadt by geocode_method:\n")
na_by_method <- enriched_sf %>%
  st_drop_geometry() %>%
  group_by(geocode_method) %>%
  summarise(
    total    = n(),
    na_aadt  = sum(is.na(road_aadt)),
    pct_na   = round(100 * na_aadt / total, 1),
    .groups  = "drop"
  )
print(as.data.frame(na_by_method), row.names = FALSE)
cat("\n")

# AADT distribution
cat("    AADT distribution (road_aadt):\n")
print(summary(enriched_sf$road_aadt))
cat("\n")

# Top 5 segments by AADT
cat("    Top 5 segments by road_aadt:\n")
top_aadt <- enriched_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(road_aadt)) %>%
  distinct(road_inv, road_begmp, road_endmp, road_aadt, rte_nbr) %>%
  arrange(desc(road_aadt)) %>%
  slice_head(n = 5)
print(as.data.frame(top_aadt), row.names = FALSE)
cat("\n")

cat(sprintf("    Segments with NA road_aadt: %d\n\n",
            roads %>% filter(is.na(aadt)) %>% nrow()))

# 6. Save outputs 
cat("[6] Saving outputs...\n")
saveRDS(enriched_sf, "data/processed/crashes_enriched.rds")
saveRDS(road_attrs,  "data/processed/road_attrs.rds")

size_enriched <- file.info("data/processed/crashes_enriched.rds")$size / 1e6
size_roads    <- file.info("data/processed/road_attrs.rds")$size    / 1e6
cat(sprintf("    crashes_enriched.rds : %.1f MB\n", size_enriched))
cat(sprintf("    road_attrs.rds       : %.1f MB\n\n", size_roads))

#7. PASS / FAIL summary 
match_pct <- 100 * n_matched / n_out
status    <- if (match_pct >= 95) "PASS" else "WARN"

cat("ENRICHMENT SUMMARY \n")
cat(sprintf("  [%s] Road match rate  : %.1f%%  (expected >= 95%%)\n",
            status, match_pct))
cat(sprintf("  [%s] Row count        : %d in → %d out\n",
            if (n_in == n_out) "PASS" else "FAIL", n_in, n_out))
cat(sprintf("  [%s] crashes_enriched.rds : %.1f MB  (expected < 60 MB)\n",
            if (size_enriched < 60) "PASS" else "WARN", size_enriched))

cat("Phase 4 complete. Ready for Phase 5 (crash-rate calculations).\n")

} 
