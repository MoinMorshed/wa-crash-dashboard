if (!nzchar(Sys.getenv("SHINY_PORT"))) {

library(sf)
library(dplyr)
library(data.table)

source("R/hotspot_analysis.R")

cat("Phase 5: Hotspot Analysis \n\n")

# 1. Load enriched crashes
cat("[1] Loading crashes_enriched.rds...\n")
crashes <- readRDS("data/processed/crashes_enriched.rds")
cat(sprintf("    %d crashes loaded\n\n", nrow(crashes)))

# 2. Compute sliding-window hotspots 
cat("[2] Computing sliding-window hotspots (0.5 mi window, 0.1 mi step)...\n")
t_start <- proc.time()

hotspots <- sliding_window_hotspots(
  crashes_sf   = crashes,
  window_miles = 0.5,
  step_miles   = 0.1,
  min_crashes  = 3
)

elapsed <- round((proc.time() - t_start)[["elapsed"]], 1)
cat(sprintf("    Done in %.1f seconds\n", elapsed))
cat(sprintf("    %d hotspot windows generated\n\n", nrow(hotspots)))

# 3. Sanity checks 
cat("[3] Top 10 hotspots by EPDO total:\n")
top_epdo <- top_n_hotspots(hotspots, n = 10, metric = "epdo_total")
print(as.data.frame(top_epdo), row.names = FALSE)
cat("\n")

cat("[4] Top 10 hotspots by crash rate (mean_aadt >= 5000):\n")
top_rate <- top_n_hotspots(hotspots, n = 10, metric = "crash_rate_mvmt", min_aadt = 5000)
print(as.data.frame(top_rate), row.names = FALSE)
cat("\n")

cat("[5] Crash count distribution across hotspot windows:\n")
breaks <- c(3, 5, 10, 20, 50, Inf)
labels <- c("3-4", "5-9", "10-19", "20-49", "50+")
count_tbl <- table(cut(hotspots$crash_count,
                       breaks = breaks, labels = labels,
                       right  = FALSE, include.lowest = TRUE))
print(count_tbl)
cat(sprintf("\n    Median crashes/window : %.0f\n",   median(hotspots$crash_count)))
cat(sprintf("    Max crashes/window    : %d\n\n",    max(hotspots$crash_count)))

# 4. Save 
cat("[6] Saving hotspots.rds...\n")
saveRDS(hotspots, "data/processed/hotspots.rds")
fsize <- file.info("data/processed/hotspots.rds")$size / 1e6
cat(sprintf("    hotspots.rds : %.2f MB\n\n", fsize))

# 5. Summary 
cat("HOTSPOT SUMMARY \n")
cat(sprintf("  [%s] Compute time  : %.1f s  (target < 60 s)\n",
            if (elapsed < 60) "PASS" else "WARN", elapsed))
cat(sprintf("  [%s] Windows total : %d\n",
            if (nrow(hotspots) > 0) "PASS" else "FAIL", nrow(hotspots)))
cat(sprintf("  [%s] File size     : %.2f MB  (target < 5 MB)\n",
            if (fsize < 5) "PASS" else "WARN", fsize))
cat(sprintf("  Top EPDO route    : %s  (expected I-5/I-405/I-90)\n",
            paste0("SR-", top_epdo$rte_nbr[1])))

cat("Phase 5 complete. Ready for Phase 6 (Shiny dashboard).\n")

} 

