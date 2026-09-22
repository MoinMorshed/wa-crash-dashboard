library(sf)
library(dplyr)

# Load Washington State boundary polygon, transform to 4326 for Leaflet
load_boundary <- function(boundary_path = "data/shapefile/WA_State_Boundary.shp") {
  boundary <- st_read(boundary_path, quiet = TRUE)

  if (is.na(st_crs(boundary))) {
    st_crs(boundary) <- 3857
  }

  boundary <- st_transform(boundary, 4326)
  return(boundary)
}

# Load the WSDOT shapefile, assign CRS 3857, transform to 4326 for Leaflet
load_routes <- function(shapefile_path =
    "data/shapefile/WSDOT_-_State_Route_Line_(1_500K)_2002.shp") {
  Sys.setenv(SHAPE_RESTORE_SHX = "YES")
  routes_sf <- st_read(shapefile_path, quiet = TRUE)
  st_crs(routes_sf) <- 3857
  routes_sf <- st_transform(routes_sf, 4326)
  routes_sf
}

# Geocode mainline accidents (rd_type == "ML") via linear referencing
geocode_crashes <- function(accidents_df, routes_sf) {

  # st_line_interpolate requires planar (projected) coordinates — work in 3857,
  # transform result to 4326 at the end
  routes_3857 <- st_transform(routes_sf, 3857)

  routes_main <- routes_3857 %>%
    filter(is.na(RelRouteTy)) %>%
    mutate(
      route_num = as.integer(StateRoute),
      seg_idx   = row_number()
    )

  # Attribute-only lookup — drop geometry so dplyr join never touches CRS
  routes_lookup <- routes_main %>%
    st_drop_geometry() %>%
    select(seg_idx, route_num, BARM, EARM, RouteID)

  mainline_acc <- accidents_df %>%
    filter(rd_type == "ML")

  cat(sprintf("    %d mainline accidents\n", nrow(mainline_acc)))

  # Pure data frame range join — no sf class involved
  matched <- mainline_acc %>%
    inner_join(routes_lookup,
               by           = c("rte_nbr" = "route_num"),
               relationship = "many-to-many") %>%
    filter(milepost >= BARM & milepost <= EARM) %>%
    group_by(caseno) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(fraction = pmax(0, pmin(1, (milepost - BARM) / (EARM - BARM))))

  cat(sprintf("    %d matched  |  %d unmatched\n",
              nrow(matched), nrow(mainline_acc) - nrow(matched)))

  # Pull projected geometries by index, interpolate in planar space
  seg_geoms   <- routes_main$geometry[matched$seg_idx]
  point_geoms <- st_line_interpolate(seg_geoms, matched$fraction, normalized = TRUE)

  # Build sf in 3857, then transform to 4326 for Leaflet
  matched$geometry <- point_geoms
  result_sf <- st_as_sf(matched, sf_column_name = "geometry", crs = 3857)
  result_sf <- st_transform(result_sf, 4326)
  result_sf$geocode_method <- "mainline"

  cat(sprintf("    %d mainline accidents geocoded\n", nrow(result_sf)))
  result_sf
}

# Snap non-mainline accidents (ramps, couplets, etc.) to their parent mainline
snap_nonmainline_crashes <- function(nonmainline_df, routes_sf) {

  routes_3857 <- st_transform(routes_sf, 3857)

  routes_main <- routes_3857 %>%
    filter(is.na(RelRouteTy)) %>%
    mutate(
      route_num = as.integer(StateRoute),
      seg_idx   = row_number()
    )

  routes_lookup <- routes_main %>%
    st_drop_geometry() %>%
    select(seg_idx, route_num, BARM, EARM, RouteID)

  cat(sprintf("    %d non-mainline accidents\n", nrow(nonmainline_df)))

  matched <- nonmainline_df %>%
    inner_join(routes_lookup,
               by           = c("rte_nbr" = "route_num"),
               relationship = "many-to-many") %>%
    filter(milepost >= BARM & milepost <= EARM) %>%
    group_by(caseno) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(fraction = pmax(0, pmin(1, (milepost - BARM) / (EARM - BARM))))

  cat(sprintf("    %d snapped  |  %d unmatched\n",
              nrow(matched), nrow(nonmainline_df) - nrow(matched)))

  seg_geoms   <- routes_main$geometry[matched$seg_idx]
  point_geoms <- st_line_interpolate(seg_geoms, matched$fraction, normalized = TRUE)

  matched$geometry <- point_geoms
  result_sf <- st_as_sf(matched, sf_column_name = "geometry", crs = 3857)
  result_sf <- st_transform(result_sf, 4326)
  result_sf$geocode_method <- paste0("snapped_", result_sf$rd_type)

  cat(sprintf("    %d non-mainline accidents geocoded\n", nrow(result_sf)))
  result_sf
}

# Return summary of accidents that failed to geocode
diagnose_unmatched <- function(accidents_df, geocoded_sf) {
  geocoded_casenos <- geocoded_sf$caseno

  accidents_df %>%
    filter(!caseno %in% geocoded_casenos) %>%
    group_by(rd_type, rte_nbr) %>%
    summarise(
      count         = n(),
      sample_caseno = first(caseno),
      .groups       = "drop"
    ) %>%
    arrange(desc(count))
}

