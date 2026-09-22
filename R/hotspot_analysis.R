library(dplyr)
library(data.table)

# FHWA EPDO weights:
#   Fatal / disabling injury (severity 2, 5) → 542
#   Non-disabling / possible injury (6, 7)   → 11
#   No injury / not stated / NA (0, 1, NA)   → 1
compute_epdo <- function(severity_vec) {
  dplyr::case_when(
    is.na(severity_vec)          ~   1,
    severity_vec %in% c(2L, 5L) ~ 542,
    severity_vec %in% c(6L, 7L) ~  11,
    TRUE                        ~   1   # 0, 1, 3, 4 all treated as PDO
  )
}

# Sliding-window hotspot analysis
# Each crash falls in at most (window_miles / step_miles) windows.
# Crash-window pairs are expanded in data.table and then aggregated — no nested loops over routes or windows
sliding_window_hotspots <- function(crashes_sf,
                                    window_miles = 0.5,
                                    step_miles   = 0.1,
                                    min_crashes  = 3) {

  # 1. Prepare flat table 
  dt <- crashes_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(
      geocode_method == "mainline",
      !is.na(milepost),
      !is.na(road_aadt)
    ) %>%
    dplyr::select(caseno, rte_nbr, milepost, severity, road_aadt) %>%
    dplyr::mutate(epdo = compute_epdo(severity)) %>%
    data.table::as.data.table()

  if (nrow(dt) == 0L) {
    warning("No mainline crashes with valid milepost and road_aadt found.")
    return(data.frame())
  }

  # 2. Route-relative milepost 
  dt[, route_min_mp := floor(min(milepost)), by = rte_nbr]
  dt[, rel_mp       := milepost - route_min_mp]

  # Crash at rel_mp r is in window k when:  k*s <= r < k*s + w
  # Solving for integer k: floor((r-w)/s) + 1  <=  k  <=  floor(r/s)
  # Clamp k_min to 0 (no negative windows).
  # Round to 10 sig figs to neutralise floating-point drift (0.1 + 0.4 ≠ 0.5).
  dt[, `:=`(
    k_min = pmax(0L, as.integer(floor(round((rel_mp - window_miles) / step_miles, 10))) + 1L),
    k_max = as.integer(floor(round(rel_mp / step_miles, 10)))
  )]

  # Drop crashes that fall in no window (can happen at route start < window_miles)
  dt <- dt[k_min <= k_max]

  # 3. Expand to one row per (crash × window) 
  crash_windows <- dt[,
    .(window_k = seq.int(k_min, k_max)),
    by = .(caseno, rte_nbr, milepost, severity, road_aadt, epdo, route_min_mp)
  ]

  # 4. Aggregate by (route, window)
  hotspots <- crash_windows[,
    .(
      crash_count   = .N,
      fatal_count   = sum(severity == 2L, na.rm = TRUE),
      serious_count = sum(severity == 5L, na.rm = TRUE),
      epdo_total    = sum(epdo),
      mean_aadt     = mean(road_aadt, na.rm = TRUE)
    ),
    by = .(rte_nbr, window_k, route_min_mp)
  ]

  # 5. Derived metrics
  hotspots[, window_start_mp := round(route_min_mp + window_k * step_miles, 6)]
  hotspots[, window_end_mp   := round(window_start_mp + window_miles, 6)]

  # Crash rate: crashes per million vehicle-miles traveled
  # MVMT for one year = AADT × 365 × window_miles / 1e6
  hotspots[mean_aadt > 0,
    crash_rate_mvmt := (crash_count * 1e6) / (mean_aadt * 365 * window_miles)
  ]
  hotspots[is.na(mean_aadt) | mean_aadt == 0, crash_rate_mvmt := NA_real_]

  hotspots[, epdo_per_mile := epdo_total / window_miles]

  # 6. Filter, clean, return
  hotspots <- hotspots[crash_count >= min_crashes]
  hotspots[, `:=`(window_k = NULL, route_min_mp = NULL)]

  setcolorder(hotspots, c("rte_nbr", "window_start_mp", "window_end_mp",
                           "crash_count", "fatal_count", "serious_count",
                           "epdo_total", "mean_aadt",
                           "crash_rate_mvmt", "epdo_per_mile"))
  setorder(hotspots, rte_nbr, window_start_mp)

  as.data.frame(hotspots)
}

# Rank hotspots by chosen metric, optionally filtering low-volume segments
top_n_hotspots <- function(hotspots_df,
                           n      = 20,
                           metric = c("epdo_total", "crash_count",
                                      "crash_rate_mvmt", "epdo_per_mile"),
                           min_aadt = 0) {
  metric <- match.arg(metric)

  hotspots_df %>%
    dplyr::filter(mean_aadt >= min_aadt) %>%
    dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::relocate(rank)
}
