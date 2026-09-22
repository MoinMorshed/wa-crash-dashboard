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

source("global.R", local = TRUE)



# Sidebar 
filter_sidebar <- sidebar(
  width = 300,

  # Blue header bar
  tags$div(
    style = paste(
      "background:#1a5276; color:white; font-weight:600; font-size:1rem;",
      "padding:0.6rem 1rem; margin:-1rem -1rem 0.75rem -1rem;"
    ),
    icon("filter", class = "me-2"), "Filters"
  ),

  # Select All + Reset buttons
  tags$div(
    class = "d-flex gap-2 mb-3",
    actionButton("select_all",    "Select All",
                 class = "btn-sm btn-success flex-fill"),
    actionButton("reset_filters", "Reset to Defaults",
                 class = "btn-sm btn-secondary flex-fill")
  ),
  
  tags$div(
    class = "d-grid mb-3",
    downloadButton(
      "download_filtered_raw",
      "Download Filtered Raw Data",
      class = "btn-sm btn-outline-primary"
    )
  ),

  pickerInput(
    "sev_filter", "Severity:",
    choices  = severity_choices,
    selected = severity_default,
    multiple = TRUE,
    options  = pickerOptions(
      actionsBox          = TRUE,
      liveSearch          = FALSE,
      selectedTextFormat  = "count > 1",
      countSelectedText   = "{0} of {1} selected"
    )
  ),

  pickerInput(
    "route", "Route (SR number):",
    choices  = route_choices,
    selected = route_default,
    multiple = TRUE,
    options  = pickerOptions(
      actionsBox           = TRUE,
      liveSearch           = TRUE,
      liveSearchPlaceholder = "Search routes...",
      selectedTextFormat   = "count > 1",
      countSelectedText    = "{0} routes selected"
    )
  ),

  pickerInput(
    "func_cls", "Functional class:",
    choices  = funccls_choices,
    selected = funccls_default,
    multiple = TRUE,
    options  = pickerOptions(
      actionsBox         = TRUE,
      selectedTextFormat = "count > 1",
      countSelectedText  = "{0} of {1} selected"
    )
  ),

  sliderInput(
    "month_range", "Month range",
    min = 1, max = 12, value = c(1, 12), step = 1, ticks = FALSE
  ),

  pickerInput(
    "weather_filter", "Weather:",
    choices  = weather_choices,
    selected = weather_default,
    multiple = TRUE,
    options  = pickerOptions(
      actionsBox         = TRUE,
      selectedTextFormat = "count > 1",
      countSelectedText  = "{0} of {1} selected"
    )
  ),

  pickerInput(
    "light_filter", "Lighting:",
    choices  = light_choices,
    selected = light_default,
    multiple = TRUE,
    options  = pickerOptions(
      actionsBox         = TRUE,
      selectedTextFormat = "count > 1",
      countSelectedText  = "{0} of {1} selected"
    )
  ),

  pickerInput(
    "acctype_filter", "Collision type:",
    choices  = names(collision_groups),
    selected = colltype_default,
    multiple = TRUE,
    options  = pickerOptions(
      actionsBox           = TRUE,
      liveSearch           = TRUE,
      liveSearchPlaceholder = "Search types...",
      selectedTextFormat   = "count > 1",
      countSelectedText    = "{0} types selected"
    )
  ),

  hr(),
  uiOutput("filter_count")
)

# Distribution chart card helper 
dist_card <- function(header, output_id) {
  card(
    card_header(header),
    plotlyOutput(output_id, height = "300px")
  )
}

# Overview tab 
overview_tab <- nav_panel(
  "Overview",
  icon = icon("gauge"),
  layout_sidebar(
    sidebar  = filter_sidebar,
    fillable = FALSE,

    # Active filter summary
    uiOutput("filter_summary"),

    # Row 1: 5 KPI value boxes
    layout_columns(
      col_widths = c(2, 2, 2, 3, 3),
      fill = FALSE,
      value_box(
        title    = "Total Crashes",
        value    = textOutput("kpi_total"),
        showcase = icon("car-burst"),
        theme    = "primary"
      ),
      value_box(
        title    = "Fatalities",
        value    = textOutput("kpi_fatal"),
        showcase = icon("skull"),
        theme    = "danger"
      ),
      value_box(
        title    = "Serious Injuries",
        value    = textOutput("kpi_serious"),
        showcase = icon("ambulance"),
        theme    = "warning"
      ),
      value_box(
        title    = "Adverse Weather",
        value    = textOutput("kpi_adv_wx"),
        showcase = icon("cloud-rain"),
        theme    = "secondary"
      ),
      value_box(
        title    = "Nighttime Crashes",
        value    = textOutput("kpi_night"),
        showcase = icon("moon"),
        theme    = "dark"
      )
    ),

    # Row 2: Top 10 routes bar chart (40%) + severity donut (60%)
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Routes by Crash Count — Top 10"),
        plotlyOutput("top10_routes_chart", height = "350px")
      ),
      card(
        card_header("Severity Mix"),
        plotlyOutput("severity_donut", height = "350px")
      )
    ),

    # Row 3: Distribution bar charts
    layout_columns(
      col_widths = c(4, 4, 4),
      dist_card("By Weather Condition",  "dist_weather"),
      dist_card("By Lighting Condition", "dist_light"),
      dist_card("By Collision Type",     "dist_acctype")
    )
  )
)

# Temporal tab 
temporal_tab <- nav_panel(
  "Temporal Patterns",
  icon = icon("clock"),
  compact_filter_strip(
    "temporal",
    extra_inputs = tagList(
      column(3,
             selectizeInput(
               "temporal_route",
               label    = "Route:",
               choices  = c("All routes" = "all", route_choices),
               selected = "all",
               multiple = FALSE,
               width    = "100%"
             )
      ),
      column(2,
             selectizeInput(
               "temporal_day_type",
               label    = "Day type:",
               choices  = c("All days" = "all", "Weekdays" = "weekday", "Weekends" = "weekend"),
               selected = "all",
               multiple = FALSE,
               width    = "100%"
             )
      )
    )
  ),
  uiOutput("temporal_insights"),
  
  card(
    card_header("Crash Frequency by Hour and Day of Week"),
    plotlyOutput("temporal_heatmap", height = "320px")
  ),

  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("Crashes by Hour of Day and Severity"),
      plotlyOutput("temporal_hourly_severity", height = "320px")
    ),
    card(
      card_header("Crashes by Day of Week"),
      plotlyOutput("temporal_dow", height = "320px")
    )
  ),

  card(
    card_header("Monthly Crash Distribution"),
    plotlyOutput("temporal_monthly", height = "280px")
  )
)

# Roadway tab 
roadway_tab <- nav_panel(
  "Roadway Factors",
  icon = icon("road"),
  compact_filter_strip(
    "roadway",
    extra_inputs = column(2,
      selectizeInput(
        "roadway_func_focus",
        label    = "Road class:",
        choices  = c("All classes" = "all", "Interstate" = "interstate",
                     "Urban"       = "urban", "Rural"     = "rural"),
        selected = "all",
        multiple = FALSE,
        width    = "100%"
      )
    )
  ),
  uiOutput("roadway_insights"),

  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("Crash Frequency by Collision Type"),
      plotlyOutput("roadway_collision_type", height = "380px")
    ),
    card(
      card_header("Road Surface Condition at Time of Crash"),
      plotlyOutput("roadway_surface", height = "380px")
    )
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Crashes by Traffic Control Type"),
      plotlyOutput("roadway_traffic_control", height = "320px")
    ),
    card(
      card_header("Crash Distribution by Speed Limit"),
      plotlyOutput("roadway_speed_dist", height = "320px")
    )
  ),

  card(
    card_header("Crash Severity by Functional Class"),
    plotlyOutput("roadway_funcls_heatmap", height = "350px")
  )
)

# Driver tab 
driver_tab <- nav_panel(
  "Driver & Vehicle",
  icon = icon("person"),
  compact_filter_strip(
    "driver",
    extra_inputs = column(2,
      selectizeInput(
        "driver_veh_type",
        label    = "Vehicle type:",
        choices  = c("All vehicles"   = "all",   "Passenger cars" = "01",
                     "Trucks/pickups" = "02",     "Motorcycles"    = "12",
                     "Heavy trucks"   = "heavy"),
        selected = "all",
        multiple = FALSE,
        width    = "100%"
      )
    )
  ),
  uiOutput("driver_insights"),

  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("Driver Contributing Circumstances"),
      plotlyOutput("driver_contrib", height = "380px")
    ),
    card(
      card_header("At-Fault Driver Age Distribution"),
      plotlyOutput("driver_age", height = "380px")
    )
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Vehicle Condition at Time of Crash"),
      plotlyOutput("driver_vehcond", height = "320px")
    ),
    card(
      card_header("Driver Action Before Crash (Top 10)"),
      plotlyOutput("driver_action", height = "320px")
    )
  ),

  card(
    card_header("Vehicle Type: Crash Volume vs. Fatal Rate"),
    plotlyOutput("driver_vehtype", height = "370px")
  )
)

# Hotspot tab
hotspot_tab <- nav_panel(
  "Hotspots",
  icon = icon("fire"),
  compact_filter_strip(
    "hotspot",
    extra_inputs = tagList(
      column(2,
        numericInput(
          "hotspot_min_crashes",
          label = "Min crashes:",
          value = 5, min = 1, max = 50, step = 1,
          width = "100%"
        )
      ),
      column(2,
        selectizeInput(
          "hotspot_metric",
          label    = "Rank by:",
          choices  = c("EPDO total"      = "epdo_total",
                       "Crash count"     = "crash_count",
                       "Crash rate/MVMT" = "crash_rate_mvmt"),
          selected = "epdo_total",
          multiple = FALSE,
          width    = "100%"
        )
      )
    )
  ),
  uiOutput("hotspot_insights"),

  layout_columns(
    col_widths = c(5, 7),
    card(
      card_header(
        "Top 20 Crash Hotspots",
        tags$small(
          class = "text-muted ms-2",
          textOutput("hotspot_table_subtitle", inline = TRUE)
        )
      ),
      DT::dataTableOutput("hotspot_table")
    ),
    card(
      card_header("Hotspot Locations"),
      tags$p(
        class = "text-muted px-3",
        style = "font-size:0.8rem; margin-bottom:4px;",
        "Colored segments = top 20 windows. Darker/warmer = higher ranked. Click a segment for details."
      ),
      leafletOutput("hotspot_map", height = "450px")
    )
  ),

  card(
    card_header("Hotspot Metric Comparison — Top Segments"),
    tags$p(
      class = "text-muted px-3",
      style = "font-size:0.8rem; margin-bottom:0;",
      paste0(
        "Same segments ranked by different metrics. A segment high on EPDO but low on crash count ",
        "has fewer but more severe crashes — different safety concerns, different countermeasures."
      )
    ),
    plotlyOutput("hotspot_comparison", height = "420px")
  ),

  uiOutput("hotspot_profile")
)

# Map tab 
map_tab <- nav_panel(
  "Map",
  icon = icon("map"),
  map_filter_strip(),
  div(
    style = "position:relative;",
    leafletOutput("crash_map", width = "100%",
                  height = "calc(100vh - 280px)"),
    absolutePanel(
      top      = 10, right = 60,
      width    = 190,
      draggable = FALSE,
      style    = paste(
        "background:rgba(255,255,255,0.95);",
        "padding:12px 14px; border-radius:6px;",
        "box-shadow:0 2px 8px rgba(0,0,0,0.25);",
        "z-index:1001;"
      ),
      tags$div(
        style = "font-weight:600; font-size:0.9rem; margin-bottom:8px;",
        icon("sliders", class = "me-1"), " Map Options"
      ),
      checkboxInput("show_clusters", "Cluster markers", value = TRUE),
      checkboxInput("show_heatmap",  "Heatmap (EPDO)",  value = FALSE),
      checkboxInput("show_boundary", "State boundary",  value = TRUE),
      checkboxInput("show_routes",   "Route network",   value = TRUE)
    )
  )
)

# About tab 
about_tab <- nav_panel(
  "About",
  icon = icon("circle-info"),
  card(
    max_height = "90vh",
    card_header("Project Methodology"),
    card_body(
      h4("CE 565 — Transportation Data Management and Analysis (Crash Analysis)"),
      p("This dashboard analyzes 50,052 crashes recorded on Washington State
         highways in 2002, sourced from the Highway Safety Information System
         (HSIS) maintained by FHWA."),
      hr(),
      h5("Data Pipeline"),
      tags$ol(
        tags$li("Raw crash, vehicle, and road tables pulled from a SQL Server database."),
        tags$li("Crashes geocoded to (lat, lon) using WSDOT 2002 500K route shapefile via ARM linear referencing."),
        tags$li("Each crash enriched with road-segment attributes (AADT, speed limit, lane count, terrain) via milepost range-join."),
        tags$li("Sliding-window hotspot analysis (0.5-mi window, 0.1-mi step) computes EPDO and crash rates for every route segment.")
      ),
      hr(),
      h5("EPDO Weights (FHWA standard)"),
      tags$ul(
        tags$li("Fatal / Disabling injury: 542"),
        tags$li("Non-disabling / Possible injury: 11"),
        tags$li("PDO / No injury / Not stated: 1")
      ),
      hr(),
      h5("Limitations"),
      tags$ul(
        tags$li("Non-mainline crashes (~15%) snapped to parent route — exact location approximate."),
        tags$li("AADT values missing for some rural segments (~8%); crash-rate calculations exclude those windows."),
        tags$li("Analysis limited to state-route network in the WSDOT 500K shapefile.")
      ),
      hr(),
      p(tags$em("University of Arizona — CE 565 Spring 2026"))
    )
  )
)

# Page layout
ui <- page_navbar(
  id = "main_nav",
  tags$head(
    tags$style(HTML("
      .filter-strip,
      .filter-strip .card-body,
      .filter-strip .row,
      .filter-strip [class*='col-'],
      .filter-strip .form-group,
      .filter-strip .shiny-input-container {
        overflow: visible !important;
      }
      .filter-strip {
        position: relative;
        z-index: 5000;
      }
      .map-filter-grid {
        display: grid;
        grid-template-columns:
          minmax(150px, 0.85fr)
          minmax(260px, 1.35fr)
          minmax(260px, 1.35fr)
          minmax(220px, 1.25fr)
          minmax(260px, 1.35fr)
          minmax(180px, 0.95fr)
          minmax(54px, 0.25fr);
        gap: 12px 14px;
        align-items: end;
      }
      .map-filter-control .form-group,
      .map-filter-control .shiny-input-container {
        margin-bottom: 0;
      }
      .map-filter-reset {
        padding-bottom: 11px;
      }
      .map-filter-status {
        margin-top: 4px;
      }
      @media (max-width: 1400px) {
        .map-filter-grid {
          grid-template-columns: repeat(auto-fit, minmax(190px, 1fr));
        }
        .map-filter-reset {
          padding-bottom: 0;
        }
      }
      #crash_map .leaflet-top.leaflet-left {
        position: absolute;
      }
      #crash_map .leaflet-top.leaflet-left > .leaflet-control {
        width: max-content;
      }
      #crash_map .leaflet-control-zoom {
        margin: 10px 0 0 10px;
      }
      #crash_map .leaflet-control-layers {
        position: absolute;
        top: 0;
        left: 44px;
        margin: 10px 0 0 10px;
      }
      #crash_map .leaflet-top.leaflet-left > .leaflet-control:not(.leaflet-control-zoom):not(.leaflet-control-layers) {
        clear: both;
        margin: 10px 0 0 10px;
      }
      .leaflet,
      .leaflet-container {
        z-index: 1;
      }
      .selectize-control {
        z-index: 5001;
      }
      .selectize-dropdown {
        z-index: 10000 !important;
      }
      .selectize-dropdown-content {
        max-height: 320px;
      }
      .bootstrap-select .dropdown-menu {
        z-index: 10000 !important;
      }
    "))
  ),
  title    = tags$span(icon("road"), " Washington State Crash Dashboard — 2002"),
  theme    = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  overview_tab,
  map_tab,
  temporal_tab,
  roadway_tab,
  driver_tab,
  hotspot_tab,
  about_tab
)

# Server 
server <- function(input, output, session) {
  session$onFlushed(function() {
    updatePickerInput(session, "sev_filter",     selected = severity_default)
    updatePickerInput(session, "route",          selected = route_default)
    updatePickerInput(session, "func_cls",       selected = funccls_default)
    updateSliderInput(session, "month_range",    value    = c(1, 12))
    updatePickerInput(session, "weather_filter", selected = weather_default)
    updatePickerInput(session, "light_filter",   selected = light_default)
    updatePickerInput(session, "acctype_filter", selected = colltype_default)
  }, once = TRUE)
  

  # Distribution bar chart helper 
  dist_bar_chart <- function(df, col, labels_vec, top_n = NULL,
                             bar_color = "steelblue") {
    plot_data <- df %>%
      dplyr::mutate(
        label = {
          k <- as.character(.data[[col]])
          dplyr::if_else(k %in% names(labels_vec), labels_vec[k], k)
        }
      ) %>%
      dplyr::filter(!is.na(.data[[col]])) %>%
      dplyr::count(label, sort = TRUE)

    if (!is.null(top_n)) plot_data <- head(plot_data, top_n)
    if (nrow(plot_data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    tot       <- sum(plot_data$n)
    plot_data <- dplyr::arrange(plot_data, n)   # ascending → highest bar at top

    plot_ly(
      plot_data,
      x           = ~n,
      y           = ~factor(label, levels = unique(label)),
      type        = "bar",
      orientation = "h",
      marker      = list(color = bar_color),
      text        = ~paste0(format(n, big.mark = ","),
                             " (", round(100 * n / tot, 1), "%)"),
      hoverinfo   = "text"
    ) %>%
      layout(
        xaxis = list(title = "Crashes", tickformat = ",d"),
        yaxis = list(title = "", tickfont = list(size = 11), automargin = TRUE),
        showlegend    = FALSE,
        margin        = list(l = 5, r = 10, t = 5, b = 35),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  }

  overview_route_base <- reactive({
    df <- crashes_flat

    sel_sev_raw <- input$sev_filter
    sel_sev     <- as.integer(sel_sev_raw[sel_sev_raw != "3_4"])
    if ("3_4" %in% sel_sev_raw) sel_sev <- c(sel_sev, 3L, 4L)
    if (length(sel_sev) > 0)
      df <- df[!is.na(df$severity) & df$severity %in% sel_sev, ]

    sel_fc <- input$func_cls
    if (length(sel_fc) > 0 && length(sel_fc) < length(funccls_choices))
      df <- df[!is.na(df$func_cls) & as.character(df$func_cls) %in% sel_fc, ]

    mr <- input$month_range
    if (!is.null(mr))
      df <- df[!is.na(df$month) & df$month >= mr[1] & df$month <= mr[2], ]

    sel_wx_raw <- input$weather_filter
    sel_wx     <- as.integer(sel_wx_raw)
    if (length(sel_wx) > 0 && length(sel_wx_raw) < length(weather_choices))
      df <- df[!is.na(df$weather) & df$weather %in% sel_wx, ]

    sel_lt_raw <- input$light_filter
    sel_lt     <- as.integer(sel_lt_raw)
    if (length(sel_lt) > 0 && length(sel_lt_raw) < length(light_choices))
      df <- df[!is.na(df$light) & df$light %in% sel_lt, ]

    sel_grp <- input$acctype_filter
    if (length(sel_grp) > 0 && length(sel_grp) < length(collision_groups)) {
      sel_codes <- as.integer(unlist(collision_groups[sel_grp]))
      df <- df[!is.na(df$acctype1) & df$acctype1 %in% sel_codes, ]
    }

    df
  })

  overview_route_choices <- reactive({
    make_route_choices(overview_route_base())
  })

  current_overview_route_choices <- reactiveVal(route_choices)

  observeEvent(overview_route_choices(), {
    old_values  <- unname(current_overview_route_choices())
    new_choices <- overview_route_choices()
    new_values  <- unname(new_choices)
    selected    <- input$route

    keep_all <- is.null(selected) || setequal(selected, old_values)
    new_selected <- if (keep_all) new_values else intersect(selected, new_values)

    updatePickerInput(
      session,
      "route",
      choices  = new_choices,
      selected = new_selected
    )
    current_overview_route_choices(new_choices)
  }, ignoreInit = FALSE)

  # Filtered data frame 
  filtered_crashes <- reactive({
    df <- crashes_flat

    # Severity — "3_4" token collapses codes 3 and 4 into one picker entry
    sel_sev_raw <- input$sev_filter
    sel_sev     <- as.integer(sel_sev_raw[sel_sev_raw != "3_4"])
    if ("3_4" %in% sel_sev_raw) sel_sev <- c(sel_sev, 3L, 4L)
    if (length(sel_sev) > 0)
      df <- df[!is.na(df$severity) & df$severity %in% sel_sev, ]

    # Route: skip filter when all routes selected (minor-route crashes would
    # otherwise be hidden since route_choices only covers routes >= 100 crashes)
    sel_rte <- input$route
    if (length(sel_rte) > 0 && length(sel_rte) < length(overview_route_choices()))
      df <- df[!is.na(df$rte_nbr) & as.character(df$rte_nbr) %in% sel_rte, ]

    # Functional class: skip when all classes selected
    sel_fc <- input$func_cls
    if (length(sel_fc) > 0 && length(sel_fc) < length(funccls_choices))
      df <- df[!is.na(df$func_cls) & as.character(df$func_cls) %in% sel_fc, ]

    # Month range
    mr <- input$month_range
    if (!is.null(mr))
      df <- df[!is.na(df$month) & df$month >= mr[1] & df$month <= mr[2], ]

    # Weather
    #sel_wx <- as.integer(input$weather_filter)
    #if (length(sel_wx) > 0)
      #df <- df[!is.na(df$weather) & df$weather %in% sel_wx, ]

    # Lighting
    #sel_lt <- as.integer(input$light_filter)
    #if (length(sel_lt) > 0)
      #df <- df[!is.na(df$light) & df$light %in% sel_lt, ]
    # Weather: skip filter when all weather codes selected
    sel_wx_raw <- input$weather_filter
    sel_wx     <- as.integer(sel_wx_raw)
    if (length(sel_wx) > 0 && length(sel_wx_raw) < length(weather_choices))
      df <- df[!is.na(df$weather) & df$weather %in% sel_wx, ]
    
    # Lighting: skip filter when all lighting codes selected
    sel_lt_raw <- input$light_filter
    sel_lt     <- as.integer(sel_lt_raw)
    if (length(sel_lt) > 0 && length(sel_lt_raw) < length(light_choices))
      df <- df[!is.na(df$light) & df$light %in% sel_lt, ]
    

    # Collision type: expand selected group names to their integer codes
    sel_grp <- input$acctype_filter
    if (length(sel_grp) > 0 && length(sel_grp) < length(collision_groups)) {
      sel_codes <- as.integer(unlist(collision_groups[sel_grp]))
      df <- df[!is.na(df$acctype1) & df$acctype1 %in% sel_codes, ]
    }

    df
  })
  
  output$download_filtered_raw <- downloadHandler(
    filename = function() {
      paste0("wa_crashes_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(
        filtered_crashes(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
  

  # Filter count badge (sidebar footer) 
  output$filter_count <- renderUI({
    n <- nrow(filtered_crashes())
    tags$small(
      class = "text-muted d-block text-center",
      sprintf("%s of %s crashes selected",
              format(n, big.mark = ","),
              format(TOTAL_CRASHES, big.mark = ","))
    )
  })

  # Filter summary bar (above KPI row) 
  output$filter_summary <- renderUI({
    n_shown <- nrow(filtered_crashes())

    sev_sel <- input$sev_filter
    sev_lbl <- if (length(sev_sel) == 0) {
      "All"
    } else if (length(sev_sel) >= length(severity_choices)) {
      "All"
    } else {
      labs <- names(severity_choices)[unname(severity_choices) %in% sev_sel]
      if (length(labs) == 1) labs[[1]]
      else paste0(labs[[1]], " + ", length(labs) - 1L, " more")
    }

    rte_sel <- input$route
    route_labs <- names(overview_route_choices())[
      unname(overview_route_choices()) %in% rte_sel
    ]
    rte_lbl <- if (length(rte_sel) >= length(overview_route_choices())) {
      "All routes"
    } else if (length(route_labs) == 0) {
      "none"
    } else if (length(route_labs) <= 4) {
      paste(route_labs, collapse = ", ")
    } else {
      paste0(length(route_labs), " routes")
    }

    fc_sel <- input$func_cls
    fc_lbl <- if (length(fc_sel) >= length(funccls_choices)) "All"
              else paste0(length(fc_sel), " classes")

    wx_sel <- input$weather_filter
    wx_lbl <- if (length(wx_sel) >= length(weather_choices)) "All"
              else paste0(length(wx_sel), " conditions")

    lt_sel <- input$light_filter
    lt_lbl <- if (length(lt_sel) >= length(light_choices)) "All"
              else paste0(length(lt_sel), " conditions")

    ac_sel <- input$acctype_filter
    ac_lbl <- if (length(ac_sel) >= length(collision_groups)) {
      "All"
    } else if (length(ac_sel) == 0) {
      "none"
    } else if (length(ac_sel) == 1) {
      ac_sel[[1]]
    } else {
      paste0(length(ac_sel), " types")
    }

    tags$div(
      class = "alert alert-info py-1 px-3 mb-2",
      style = "font-size:0.82rem; border-radius:4px;",
      tags$strong(
        sprintf("Showing %s of %s crashes",
                format(n_shown, big.mark = ","),
                format(TOTAL_CRASHES, big.mark = ","))
      ),
      tags$span(
        class = "ms-2 text-muted",
        sprintf("| Severity: %s | Routes: %s | Func class: %s | Weather: %s | Lighting: %s | Collision: %s",
                sev_lbl, rte_lbl, fc_lbl, wx_lbl, lt_lbl, ac_lbl)
      )
    )
  })

  # KPIs
  output$kpi_total <- renderText(
    format(nrow(filtered_crashes()), big.mark = ",")
  )
  output$kpi_fatal <- renderText(
    format(sum(filtered_crashes()$severity == 2L, na.rm = TRUE), big.mark = ",")
  )
  output$kpi_serious <- renderText(
    format(sum(filtered_crashes()$severity == 5L, na.rm = TRUE), big.mark = ",")
  )
  output$kpi_adv_wx <- renderText({
    df <- filtered_crashes()
    n  <- nrow(df)
    if (n == 0) return("—")
    sprintf("%.1f%%", 100 * sum(df$weather != 1L, na.rm = TRUE) / n)
  })
  output$kpi_night <- renderText({
    df <- filtered_crashes()
    n  <- nrow(df)
    if (n == 0) return("—")
    sprintf("%.1f%%", 100 * sum(df$light %in% c(4L, 5L, 6L), na.rm = TRUE) / n)
  })

  # Top 10 routes bar chart 
  output$top10_routes_chart <- renderPlotly({
    route_data <- filtered_crashes() %>%
      count(rte_nbr, sort = TRUE) %>%
      head(10) %>%
      mutate(label = route_label(rte_nbr)) %>%
      arrange(n)   # ascending → highest bar at top

    if (nrow(route_data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    tot <- sum(route_data$n)

    plot_ly(
      route_data,
      x           = ~n,
      y           = ~factor(label, levels = unique(label)),
      type        = "bar",
      orientation = "h",
      marker      = list(color = "#1a5276"),
      text        = ~paste0(format(n, big.mark = ","),
                             " (", round(100 * n / tot, 1), "%)"),
      hoverinfo   = "text"
    ) %>%
      layout(
        xaxis = list(title = "Crashes", tickformat = ",d"),
        yaxis = list(title = "", tickfont = list(size = 12), automargin = TRUE),
        showlegend    = FALSE,
        margin        = list(l = 5, r = 15, t = 5, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Severity donut 
  # Donut occupies left 55% of plot area; legend sits cleanly on the right.
  output$severity_donut <- renderPlotly({
    sev_data <- filtered_crashes() %>%
      mutate(label = dplyr::case_when(
        severity == 2L           ~ "Fatal",
        severity == 5L           ~ "Disabling injury",
        severity == 6L           ~ "Non-disabling injury",
        severity == 7L           ~ "Possible injury",
        severity == 1L           ~ "No injury (PDO)",
        severity == 0L           ~ "Not stated",
        severity %in% c(3L, 4L) ~ "Unknown/Other",
        TRUE                     ~ "Unknown/Other"
      )) %>%
      count(label) %>%
      arrange(desc(n))
    
    severity_colors <- c(
      "Fatal"               = "#d62728",
      "Disabling injury"    = "#ff7f0e",
      "Non-disabling injury"= "#fbc6a9",
      "Possible injury"     = "#98df8a",
      "No injury (PDO)"     = "#2ca02c",
      "Not stated"          = "#c7c7c7",
      "Unknown/Other"       = "#7f7f7f"
    )

    plot_ly(
      sev_data,
      labels       = ~label,
      values       = ~n,
      type         = "pie",
      hole         = 0.55,
      textinfo     = "percent",
      textposition = "inside",
      hoverinfo    = "label+value+percent",
      domain       = list(x = c(0, 0.58), y = c(0, 1)),
      marker       = list(
        colors = unname(severity_colors[sev_data$label])
      ),
      sort = FALSE
    ) %>%
      layout(
        showlegend = TRUE,
        legend     = list(
          orientation = "v",
          x           = 0.62,
          y           = 0.5,
          xanchor     = "left",
          yanchor     = "middle",
          font        = list(size = 12)
        ),
        margin        = list(t = 10, b = 10, l = 10, r = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Distribution bar charts 
  output$dist_weather <- renderPlotly(
    dist_bar_chart(filtered_crashes(), "weather",  weather_labels)
  )
  output$dist_light <- renderPlotly(
    dist_bar_chart(filtered_crashes(), "light",    light_labels)
  )
  output$dist_acctype <- renderPlotly(
    dist_bar_chart(filtered_crashes(), "acctype1", acctype_labels, top_n = 10)
  )

  # Select All 
  observeEvent(input$select_all, {
    updatePickerInput(session, "sev_filter",    selected = unname(severity_choices))
    updatePickerInput(session, "route",         selected = unname(overview_route_choices()))
    updatePickerInput(session, "func_cls",      selected = unname(funccls_choices))
    updateSliderInput(session, "month_range",   value    = c(1, 12))
    updatePickerInput(session, "weather_filter",selected = unname(weather_choices))
    updatePickerInput(session, "light_filter",  selected = unname(light_choices))
    updatePickerInput(session, "acctype_filter",selected = names(collision_groups))
  })

  # Reset to Defaults 
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "sev_filter",    selected = severity_default)
    updatePickerInput(session, "route",         selected = route_default)
    updatePickerInput(session, "func_cls",      selected = funccls_default)
    updateSliderInput(session, "month_range",   value    = c(1, 12))
    updatePickerInput(session, "weather_filter",selected = weather_default)
    updatePickerInput(session, "light_filter",  selected = light_default)
    updatePickerInput(session, "acctype_filter",selected = colltype_default)
  })

  # Map: popup HTML builder (vectorised, pure function)
  build_popups <- function(df) {
    look <- function(x, lbls) {
      k <- as.character(x)
      ifelse(!is.na(x) & k %in% names(lbls), lbls[k], "Unknown")
    }

    # road surface may be in rdsurf or surf_typ depending on data version
    rdsurf_col <- if ("rdsurf"   %in% names(df)) df$rdsurf   else
                  if ("surf_typ" %in% names(df)) df$surf_typ else
                  rep(NA_integer_, nrow(df))

    funccls_col <- if ("func_cls"      %in% names(df)) df$func_cls      else
                   if ("road_func_cls" %in% names(df)) df$road_func_cls else
                   rep(NA_integer_, nrow(df))

    # time: use crash_datetime minutes if available, else show HH:00
    time_str <- if ("crash_datetime" %in% names(df) &&
                    !all(is.na(df$crash_datetime))) {
      format(df$crash_datetime, "%H:%M")
    } else {
      ifelse(!is.na(df$crash_hour),
             sprintf("%02d:00", as.integer(df$crash_hour)), "Unknown")
    }

    date_str  <- ifelse(!is.na(df$crash_date),
                        format(df$crash_date, "%b %d, %Y"), "Unknown")
    route_str <- ifelse(!is.na(df$rte_nbr),
                        route_label(df$rte_nbr), "Unknown")
    mp_str    <- ifelse(!is.na(df$milepost),
                        sprintf("%.2f", df$milepost), "?")
    vehs_str  <- ifelse(!is.na(df$numvehs),
                        as.character(df$numvehs), "Unknown")

    aadt_str <- ifelse(
      !is.na(df$road_aadt),
      paste0("<b>AADT:</b> ",
             formatC(df$road_aadt, format = "d", big.mark = ","), "<br>"),
      ""
    )
    spd_str  <- ifelse(
      !is.na(df$road_spd_limt),
      paste0("<b>Speed limit:</b> ", df$road_spd_limt, " mph<br>"),
      ""
    )

    paste0(
      "<b>Crash #", df$caseno, "</b><br>",
      "<b>Date:</b> ", date_str, " at ", time_str, "<br>",
      "<b>Route:</b> ", route_str, " at MP ", mp_str, "<br>",
      "<b>Severity:</b> ", look(df$severity, severity_labels), "<br>",
      "<b>Collision:</b> ", look(df$acctype1, acctype_labels), "<br>",
      "<b>Weather:</b> ",   look(df$weather,  weather_labels),
      " | <b>Lighting:</b> ", look(df$light,  light_labels), "<br>",
      "<b>Surface:</b> ",   look(rdsurf_col,  rdsurf_labels), "<br>",
      "<b>Functional class:</b> ", look(funccls_col, funccls_labels), "<br>",
      "<b>Vehicles:</b> ", vehs_str, "<br>",
      aadt_str, spd_str
    )
  }

  # Map: base map — rendered ONCE when tab first becomes visible
  output$crash_map <- renderLeaflet({
    help_html <- paste0(
      '<details style="background:white;padding:8px 10px;border-radius:5px;',
      'box-shadow:0 2px 6px rgba(0,0,0,0.2);max-width:230px;font-size:0.82rem;">',
      '<summary style="cursor:pointer;font-weight:600;">&#9432; What am I looking at?</summary>',
      '<ul style="margin:6px 0 0 0;padding-left:16px;">',
      '<li><b>Markers</b>: each dot is one crash, colored by severity</li>',
      '<li><b>Heatmap</b>: density weighted by EPDO (fatal=542, injury=11, PDO=1)</li>',
      '<li><b>Clusters</b>: nearby markers grouped — click to expand</li>',
      '<li>Map filters narrow crashes; map updates instantly</li>',
      '</ul></details>'
    )

    legend_colors <- unname(severity_colors[c("2","5","6","7","1","3","0")])
    legend_labels <- c("Fatal", "Disabling injury", "Non-disabling injury",
                       "Possible injury", "No injury", "Unknown/Other", "Not stated")
    routes_ml <- routes_sf[is.na(routes_sf$RelRouteTy), ]

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,  group = "Light") %>%
      addProviderTiles(providers$OpenStreetMap,     group = "Street") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Light", "Street", "Satellite"),
        position   = "topleft",
        options    = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = -120.7, lat = 47.4, zoom = 7) %>%
      addControl(html = help_html, position = "topleft") %>%
      addPolygons(
        data    = wa_boundary,
        fill    = FALSE,
        color   = "#2c3e50",
        weight  = 2,
        opacity = 0.6,
        group   = "Boundary"
      ) %>%
      addPolylines(
        data    = routes_ml,
        color   = "#333333",
        weight  = 1.5,
        opacity = 0.5,
        group   = "Routes"
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = legend_colors,
        labels   = legend_labels,
        title    = "Severity",
        opacity  = 0.9,
        layerId  = "map_legend"
      )
  })

  # Map: crash points / heatmap — updates on every filter or toggle change 
  observe({
    req(!is.null(input$crash_map_zoom))  # wait for Leaflet to initialise in browser
    df_sf   <- tab_map_sf()
    n_total <- nrow(df_sf)

    # 20 K cap — sample for performance
    if (n_total > 20000) {
      showNotification(
        paste0("Showing random sample of 20,000 of ",
               format(n_total, big.mark = ","),
               " filtered crashes for performance. Narrow your filters."),
        type = "warning", duration = 5
      )
      set.seed(42)
      df_sf <- dplyr::slice_sample(df_sf, n = 20000)
    }

    proxy <- leafletProxy("crash_map")
    proxy %>% clearGroup("Crashes") %>% clearGroup("Heatmap")

    if (isTRUE(input$show_heatmap)) {
      proxy %>% leaflet.extras::addHeatmap(
        data      = df_sf,
        intensity = ~compute_epdo(severity),
        blur      = 20,
        max       = 0.05,
        radius    = 15,
        group     = "Heatmap"
      )
    } else {
      coords <- sf::st_coordinates(df_sf)
      keep   <- is.finite(coords[, 1]) & is.finite(coords[, 2])
      req(any(keep))
      
      coords   <- coords[keep, , drop = FALSE]
      df_plain <- sf::st_drop_geometry(df_sf)[keep, , drop = FALSE]

      sev_fill <- unname(severity_colors[as.character(df_plain$severity)])
      sev_fill[is.na(sev_fill)] <- "#999999"

      proxy %>% addCircleMarkers(
        lng            = coords[, 1],
        lat            = coords[, 2],
        radius         = 6,
        color          = "white",
        fillColor      = sev_fill,
        fillOpacity    = 0.85,
        weight         = 1,
        stroke         = TRUE,
        popup          = build_popups(df_plain),
        clusterOptions = if (isTRUE(input$show_clusters))
          markerClusterOptions(maxClusterRadius = 50,
                               spiderfyOnMaxZoom = TRUE)
        else NULL,
        group = "Crashes"
      )
    }
  })

  # Map: WA state boundary overlay 
  observeEvent(input$show_boundary, {
    proxy <- leafletProxy("crash_map")
    proxy %>% clearGroup("Boundary")
    if (isTRUE(input$show_boundary)) {
      proxy %>% addPolygons(
        data    = wa_boundary,
        fill    = FALSE,
        color   = "#2c3e50",
        weight  = 2,
        opacity = 0.6,
        group   = "Boundary"
      )
    }
  }, ignoreInit = TRUE)

  # Map: state route network overlay 
  observeEvent(input$show_routes, {
    proxy <- leafletProxy("crash_map")
    proxy %>% clearGroup("Routes")
    if (isTRUE(input$show_routes)) {
      routes_ml <- routes_sf[is.na(routes_sf$RelRouteTy), ]
      proxy %>% addPolylines(
        data    = routes_ml,
        color   = "#333333",
        weight  = 1.5,
        opacity = 0.5,
        group   = "Routes"
      )
    }
  }, ignoreInit = TRUE)

  # Map: mutual exclusivity — heatmap ↔ markers 
  observeEvent(input$show_heatmap, {
    if (isTRUE(input$show_heatmap) && isTRUE(input$show_clusters))
      updateCheckboxInput(session, "show_clusters", value = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$show_clusters, {
    if (isTRUE(input$show_clusters) && isTRUE(input$show_heatmap))
      updateCheckboxInput(session, "show_heatmap", value = FALSE)
  }, ignoreInit = TRUE)

  # Map: context-aware legend — severity (markers) or EPDO gradient (heatmap)
  observe({
    req(!is.null(input$crash_map_zoom))  # wait for Leaflet to initialise in browser
    input$show_heatmap  # reactive dependency
    proxy <- leafletProxy("crash_map")
    proxy %>% removeControl("map_legend")

    if (isTRUE(input$show_heatmap)) {
      gradient_html <- paste0(
        '<div style="background:white;padding:8px 10px;border-radius:5px;',
        'box-shadow:0 2px 6px rgba(0,0,0,0.2);font-size:0.82rem;">',
        '<b>EPDO Density</b><br>',
        '<div style="height:12px;width:120px;margin:4px 0;',
        'background:linear-gradient(to right,#313695,#4575b4,#74add1,',
        '#abd9e9,#fdae61,#f46d43,#d73027);border-radius:2px;"></div>',
        '<span style="float:left;">Low</span>',
        '<span style="float:right;">High</span>',
        '<div style="clear:both;font-size:0.75rem;color:#666;margin-top:4px;">',
        'Fatal×542 &nbsp; Injury×11 &nbsp; PDO×1</div>',
        '</div>'
      )
      proxy %>% addControl(
        html     = gradient_html,
        position = "bottomright",
        layerId  = "map_legend"
      )
    } else {
      legend_colors <- unname(severity_colors[c("2","5","6","7","1","3","0")])
      legend_labels <- c("Fatal", "Disabling injury", "Non-disabling injury",
                         "Possible injury", "No injury", "Unknown/Other",
                         "Not stated")
      proxy %>% addLegend(
        position = "bottomright",
        colors   = legend_colors,
        labels   = legend_labels,
        title    = "Severity",
        opacity  = 0.9,
        layerId  = "map_legend"
      )
    }
  })

  # Tab-level reactive: Map 
  map_route_choice_base <- reactive({
    month_start <- as.integer(input$map_month_start)
    month_end   <- as.integer(input$map_month_end)
    hour_start  <- as.integer(input$map_hour_start)
    hour_end    <- as.integer(input$map_hour_end)
    
    month_lo <- min(month_start, month_end, na.rm = TRUE)
    month_hi <- max(month_start, month_end, na.rm = TRUE)
    hour_lo  <- min(hour_start, hour_end, na.rm = TRUE)
    hour_hi  <- max(hour_start, hour_end, na.rm = TRUE)
    
    base <- crashes_flat %>%
      dplyr::filter(
        month      >= month_lo & month      <= month_hi,
        crash_hour >= hour_lo  & crash_hour <= hour_hi
      )
    
    base <- apply_severity_filter(base, input$map_severity)

    sel_map_grp <- input$map_acctype_filter
    if (is.null(sel_map_grp)) sel_map_grp <- names(collision_groups)
    if (length(sel_map_grp) > 0 && length(sel_map_grp) < length(collision_groups)) {
      sel_map_codes <- as.integer(unlist(collision_groups[sel_map_grp]))
      base <- base %>%
        dplyr::filter(!is.na(acctype1) & acctype1 %in% sel_map_codes)
    }
    
    map_road_type <- input$map_road_type
    if (is.null(map_road_type) || length(map_road_type) == 0) {
      map_road_type <- "mainline"
    }
    
    if (map_road_type == "mainline")
      base <- base %>% dplyr::filter(geocode_method == "mainline")
    
    base
  })

  map_route_choices <- reactive({
    c("All routes" = "all", make_route_choices(map_route_choice_base()))
  })

  observeEvent(map_route_choices(), {
    new_choices <- map_route_choices()
    new_values  <- unname(new_choices)
    selected    <- input$map_route
    new_selected <- if (is.null(selected) || !(selected %in% new_values)) {
      "all"
    } else {
      selected
    }

    updateSelectizeInput(
      session,
      "map_route",
      choices  = new_choices,
      selected = new_selected,
      server   = TRUE
    )
  }, ignoreInit = FALSE)

  tab_filtered_map <- reactive({
    base <- map_route_choice_base()

    if (!is.null(input$map_route) && input$map_route != "all")
      base <- base %>%
      dplyr::filter(!is.na(rte_nbr) & as.character(rte_nbr) == input$map_route)

    base
  })

  tab_map_sf <- reactive({
    ids    <- tab_filtered_map()$caseno
    sf_out <- crashes[crashes$caseno %in% ids, ]
    sf_out[!sf::st_is_empty(sf_out), ]
  })
  
  output$map_filter_status <- renderText({
    n_tab    <- nrow(tab_filtered_map())
    n_global <- TOTAL_CRASHES
    map_acctype <- input$map_acctype_filter
    if (is.null(map_acctype)) map_acctype <- names(collision_groups)
    active   <- input$map_severity    != "all"       ||
                input$map_month_start != "1"         ||
                input$map_month_end   != "12"        ||
                input$map_hour_start  != "0"         ||
                input$map_hour_end    != "23"        ||
                input$map_route       != "all"       ||
                input$map_road_type   != "mainline"  ||
                (length(map_acctype) > 0 &&
                 length(map_acctype) < length(collision_groups))
    

    if (active)
      paste0("⚠ Tab filter active: showing ",
             format(n_tab, big.mark = ","), " of ",
             format(n_global, big.mark = ","), " crashes")
    else
      paste0("Showing all ", format(n_tab, big.mark = ","),
             " crashes")
  })

  observeEvent(input$map_reset, {
    updateSelectizeInput(session, "map_severity",  selected = "all")
    updateSelectInput(   session, "map_month_start", selected = "1")
    updateSelectInput(   session, "map_month_end",   selected = "12")
    updateSelectInput(   session, "map_hour_start",  selected = "0")
    updateSelectInput(   session, "map_hour_end",    selected = "23")
    updateSelectizeInput(session, "map_route",     selected = "all")
    updateSelectizeInput(session, "map_road_type", selected = "mainline")
    shinyWidgets::updatePickerInput(
      session,
      "map_acctype_filter",
      selected = names(collision_groups)
    )
  })
  

  # Tab-level reactive: Temporal 
  temporal_route_choice_base <- reactive({
    month_start <- as.integer(input$temporal_month_start)
    month_end   <- as.integer(input$temporal_month_end)
    hour_start  <- as.integer(input$temporal_hour_start)
    hour_end    <- as.integer(input$temporal_hour_end)
    
    month_lo <- min(month_start, month_end, na.rm = TRUE)
    month_hi <- max(month_start, month_end, na.rm = TRUE)
    hour_lo  <- min(hour_start, hour_end, na.rm = TRUE)
    hour_hi  <- max(hour_start, hour_end, na.rm = TRUE)
    
    base <- crashes_flat %>%
      dplyr::filter(
        month      >= month_lo & month      <= month_hi,
        crash_hour >= hour_lo  & crash_hour <= hour_hi
      )
    
    base <- apply_severity_filter(base, input$temporal_severity)

    temporal_day_type <- input$temporal_day_type
    if (is.null(temporal_day_type) || length(temporal_day_type) == 0) {
      temporal_day_type <- "all"
    }
    
    if (temporal_day_type == "weekday")
      base <- base %>% dplyr::filter(as.integer(weekday_name) %in% 1:5)
    else if (temporal_day_type == "weekend")
      base <- base %>% dplyr::filter(as.integer(weekday_name) %in% 6:7)
    
    base
  })

  temporal_route_choices <- reactive({
    c("All routes" = "all", make_route_choices(temporal_route_choice_base()))
  })

  observeEvent(temporal_route_choices(), {
    new_choices <- temporal_route_choices()
    new_values  <- unname(new_choices)
    selected    <- input$temporal_route
    new_selected <- if (is.null(selected) || !(selected %in% new_values)) {
      "all"
    } else {
      selected
    }

    updateSelectizeInput(
      session,
      "temporal_route",
      choices  = new_choices,
      selected = new_selected,
      server   = TRUE
    )
  }, ignoreInit = FALSE)

  tab_filtered_temporal <- reactive({
    base <- temporal_route_choice_base()

    if (!is.null(input$temporal_route) && input$temporal_route != "all")
      base <- base %>%
      dplyr::filter(!is.na(rte_nbr) & as.character(rte_nbr) == input$temporal_route)

    base
  })
  

  output$temporal_filter_status <- renderText({
    n_tab    <- nrow(tab_filtered_temporal())
    n_global <- TOTAL_CRASHES
    active   <- input$temporal_severity    != "all" ||
                input$temporal_month_start != "1"   ||
                input$temporal_month_end   != "12"  ||
                input$temporal_hour_start  != "0"   ||
                input$temporal_hour_end    != "23"  ||
                input$temporal_route       != "all" ||
                input$temporal_day_type    != "all"
    
    
    if (active)
      paste0("⚠ Tab filter active: showing ",
             format(n_tab, big.mark = ","), " of ",
             format(n_global, big.mark = ","), " crashes")
    else
      paste0("Showing all ", format(n_tab, big.mark = ","),
             " crashes")
  })

  observeEvent(input$temporal_reset, {
    updateSelectizeInput(session, "temporal_severity", selected = "all")
    updateSelectInput(session, "temporal_month_start", selected = "1")
    updateSelectInput(session, "temporal_month_end",   selected = "12")
    updateSelectInput(session, "temporal_hour_start",  selected = "0")
    updateSelectInput(session, "temporal_hour_end",    selected = "23")
    updateSelectizeInput(session, "temporal_route",    selected = "all")
    updateSelectizeInput(session, "temporal_day_type", selected = "all")
  })
  
  

  #  Temporal: Key Insight Card 
  output$temporal_insights <- renderUI({
    data <- tab_filtered_temporal() %>% sf::st_drop_geometry()

    if (nrow(data) == 0) {
      return(bslib::card(
        bslib::card_body("No crashes match current filters.")
      ))
    }

    peak_hour <- data %>%
      dplyr::count(crash_hour) %>%
      dplyr::slice_max(n, n = 1) %>%
      dplyr::pull(crash_hour)

    peak_day <- data %>%
      dplyr::count(weekday_name) %>%
      dplyr::slice_max(n, n = 1) %>%
      dplyr::pull(weekday_name)

    night_pct <- round(
      100 * sum(data$crash_hour %in% c(21:23, 0:5)) / nrow(data), 1
    )

    fatal_night <- data %>%
      dplyr::filter(severity == 2L) %>%
      dplyr::summarise(
        pct_night = round(
          100 * sum(crash_hour %in% c(21:23, 0:5)) / n(), 1
        )
      ) %>%
      dplyr::pull(pct_night)

    day_full_names <- c(
      Mon = "Mondays", Tue = "Tuesdays", Wed = "Wednesdays",
      Thu = "Thursdays", Fri = "Fridays", Sat = "Saturdays", Sun = "Sundays"
    )
    peak_day_full <- unname(day_full_names[as.character(peak_day)])
    if (is.na(peak_day_full)) peak_day_full <- as.character(peak_day)

    insight_text <- paste0(
      "Peak crash hour is ", sprintf("%02d:00", peak_hour),
      " with the highest volume on ", peak_day_full, ". ",
      night_pct, "% of all crashes in this filter occur ",
      "between 9 PM and 5 AM. ",
      if (!is.na(fatal_night) && fatal_night > 30)
        paste0("Notably, ", fatal_night,
               "% of fatal crashes are nighttime events — ",
               "disproportionately higher than the overall rate.")
      else
        "Fatal crashes follow a similar time distribution to all crashes."
    )

    bslib::card(
      style = "background:#e8f4f8; border-left: 4px solid #2c7bb6;",
      bslib::card_body(
        tags$p(
          tags$strong("\U0001F4CA Key Insight: "),
          insight_text,
          style = "margin:0; font-size:0.9em;"
        )
      )
    )
  })

  # Temporal: Hour x Day heatmap
  output$temporal_heatmap <- renderPlotly({
    data <- tab_filtered_temporal() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    day_lvls <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

    # Count per (day, hour); coerce weekday_name to plain character to
    # avoid lubridate ordered-factor issues, then discard any NA rows
    counts <- data %>%
      dplyr::filter(!is.na(crash_hour)) %>%
      dplyr::mutate(day_chr = as.character(weekday_name)) %>%
      dplyr::filter(day_chr %in% day_lvls) %>%
      dplyr::count(day_chr, crash_hour)

    # Build 7 x 24 integer matrix — no dimnames so plotly JSON stays clean
    z_mat <- matrix(0L, nrow = 7L, ncol = 24L)
    for (i in seq_along(day_lvls)) {
      rows <- counts[counts$day_chr == day_lvls[i], ]
      for (h in rows$crash_hour) {
        z_mat[i, h + 1L] <- rows$n[rows$crash_hour == h]
      }
    }

    x_labels <- sprintf("%02d:00", 0:23)

    plot_ly() %>%
      add_heatmap(
        x            = x_labels,
        y            = day_lvls,
        z            = z_mat,
        colorscale   = "YlOrRd",
        reversescale = TRUE,
        hoverinfo    = "x+y+z"
      ) %>%
      layout(
        xaxis  = list(title = "Hour of day", tickangle = -45),
        yaxis  = list(title = "", autorange = "reversed"),
        margin = list(l = 60, r = 20, t = 20, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Temporal: Hourly severity stacked bar
  output$temporal_hourly_severity <- renderPlotly({
    data <- tab_filtered_temporal() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    sev_lvls <- c("Fatal","Disabling injury","Non-disabling injury",
                  "Possible injury","No injury","Unknown/Other","Not stated")

    local_sev_colors <- c(
      "Fatal"                = "#d62728",
      "Disabling injury"     = "#ff7f0e",
      "Non-disabling injury" = "#fbc6a9",
      "Possible injury"      = "#98df8a",
      "No injury"            = "#2ca02c",
      "Unknown/Other"        = "#7f7f7f",
      "Not stated"           = "#c7c7c7"
    )

    hourly_sev <- data %>%
      dplyr::mutate(
        sev_label = factor(
          severity_labels[as.character(severity)],
          levels = sev_lvls
        )
      ) %>%
      dplyr::count(crash_hour, sev_label) %>%
      tidyr::complete(crash_hour = 0:23, sev_label, fill = list(n = 0))

    p <- plot_ly()
    for (sev in sev_lvls) {
      trace_data <- hourly_sev %>% dplyr::filter(sev_label == sev)
      if (sum(trace_data$n) == 0) next
      p <- p %>% add_bars(
        data          = trace_data,
        x             = ~crash_hour,
        y             = ~n,
        name          = sev,
        marker        = list(color = unname(local_sev_colors[sev])),
        hovertemplate = paste0("<b>", sev, "</b><br>",
                               "Hour: %{x}:00<br>Crashes: %{y}<extra></extra>")
      )
    }

    p %>%
      layout(
        barmode = "stack",
        xaxis   = list(title     = "Hour of day",
                       tickvals  = 0:23,
                       ticktext  = sprintf("%02d:00", 0:23),
                       tickangle = -45),
        yaxis   = list(title = "Crash count"),
        legend  = list(orientation = "h", y = -0.35, x = 0),
        margin  = list(l = 50, r = 20, t = 20, b = 100),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Temporal: Monthly trend bar
  output$temporal_monthly <- renderPlotly({
    data <- tab_filtered_temporal() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    sev_lvls_mo <- c("Fatal", "Disabling injury", "Non-disabling injury",
                     "Possible injury", "No injury")
    sev_cols_mo <- c(
      "Fatal"                = "#d62728",
      "Disabling injury"     = "#ff7f0e",
      "Non-disabling injury" = "#fbc6a9",
      "Possible injury"      = "#98df8a",
      "No injury"            = "#2ca02c"
    )

    monthly_data <- data %>%
      dplyr::mutate(
        month_name = factor(month.abb[month], levels = month.abb),
        sev_label  = factor(dplyr::case_when(
          severity == 2L ~ "Fatal",
          severity == 5L ~ "Disabling injury",
          severity == 6L ~ "Non-disabling injury",
          severity == 7L ~ "Possible injury",
          severity == 1L ~ "No injury",
          TRUE           ~ NA_character_
        ), levels = sev_lvls_mo)
      ) %>%
      dplyr::filter(!is.na(sev_label)) %>%
      dplyr::count(month_name, sev_label)

    p_mo <- plot_ly()
    for (sev in sev_lvls_mo) {
      td <- monthly_data %>% dplyr::filter(sev_label == sev)
      if (nrow(td) == 0 || sum(td$n) == 0) next
      p_mo <- p_mo %>% add_bars(
        data          = td,
        x             = ~month_name,
        y             = ~n,
        name          = sev,
        marker        = list(color = unname(sev_cols_mo[sev])),
        hovertemplate = paste0("<b>%{x}</b><br>", sev, ": %{y:,}<extra></extra>")
      )
    }
    p_mo %>%
      layout(
        barmode = "stack",
        xaxis   = list(title = ""),
        yaxis   = list(title = "Crash count"),
        legend  = list(orientation = "h", y = -0.25),
        margin  = list(l = 50, r = 20, t = 20, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Temporal: Day-of-week bar
  output$temporal_dow <- renderPlotly({
    data <- tab_filtered_temporal() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    day_lvls <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

    sev_lvls_dow <- c("Fatal", "Disabling injury", "Non-disabling injury",
                      "Possible injury", "No injury")
    sev_colors_dow <- c(
      "Fatal"                = "#d62728",
      "Disabling injury"     = "#ff7f0e",
      "Non-disabling injury" = "#fbc6a9",
      "Possible injury"      = "#98df8a",
      "No injury"            = "#2ca02c"
    )

    dow_data <- data %>%
      dplyr::mutate(
        day_name  = factor(weekday_name, levels = day_lvls),
        sev_label = factor(
          dplyr::case_when(
            severity == 2L ~ "Fatal",
            severity == 5L ~ "Disabling injury",
            severity == 6L ~ "Non-disabling injury",
            severity == 7L ~ "Possible injury",
            severity == 1L ~ "No injury",
            TRUE           ~ NA_character_
          ),
          levels = sev_lvls_dow
        )
      ) %>%
      dplyr::filter(!is.na(sev_label)) %>%
      dplyr::count(day_name, sev_label)

    p <- plot_ly()
    for (sev in sev_lvls_dow) {
      trace_data <- dow_data %>% dplyr::filter(sev_label == sev)
      if (nrow(trace_data) == 0 || sum(trace_data$n) == 0) next
      p <- p %>% add_bars(
        data          = trace_data,
        x             = ~day_name,
        y             = ~n,
        name          = sev,
        marker        = list(color = unname(sev_colors_dow[sev])),
        hovertemplate = paste0("<b>%{x}</b><br>", sev, ": %{y}<extra></extra>")
      )
    }

    p %>%
      layout(
        barmode = "stack",
        xaxis   = list(title = ""),
        yaxis   = list(title = "Crash count"),
        legend  = list(orientation = "h", y = -0.2),
        margin  = list(l = 50, r = 20, t = 20, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Tab-level reactive: Roadway
  tab_filtered_roadway <- reactive({
    month_start <- as.integer(input$roadway_month_start)
    month_end   <- as.integer(input$roadway_month_end)
    hour_start  <- as.integer(input$roadway_hour_start)
    hour_end    <- as.integer(input$roadway_hour_end)
    
    month_lo <- min(month_start, month_end, na.rm = TRUE)
    month_hi <- max(month_start, month_end, na.rm = TRUE)
    hour_lo  <- min(hour_start, hour_end, na.rm = TRUE)
    hour_hi  <- max(hour_start, hour_end, na.rm = TRUE)
    
    base <- crashes_flat %>%
      dplyr::filter(
        month      >= month_lo & month      <= month_hi,
        crash_hour >= hour_lo  & crash_hour <= hour_hi
      )
    
    base <- apply_severity_filter(base, input$roadway_severity)
    
    if (input$roadway_func_focus == "interstate")
      base <- base %>% dplyr::filter(func_cls %in% c(1L, 11L))
    else if (input$roadway_func_focus == "urban")
      base <- base %>% dplyr::filter(func_cls %in% c(11L, 12L, 14L, 16L, 17L, 19L))
    else if (input$roadway_func_focus == "rural")
      base <- base %>% dplyr::filter(func_cls %in% c(1L, 2L, 6L, 7L, 9L))
    
    base
  })
  

  output$roadway_filter_status <- renderText({
    n_tab    <- nrow(tab_filtered_roadway())
    n_global <- TOTAL_CRASHES
    active   <- input$roadway_severity   != "all" ||
                input$roadway_month_start != "1"  ||
                input$roadway_month_end   != "12" ||
                input$roadway_hour_start  != "0"  ||
                input$roadway_hour_end    != "23" ||
                input$roadway_func_focus  != "all"
    
    if (active)
      paste0("⚠ Tab filter active: showing ",
             format(n_tab, big.mark = ","), " of ",
             format(n_global, big.mark = ","), " crashes")
    else
      paste0("Showing all ", format(n_tab, big.mark = ","),
             " crashes")
  })

  observeEvent(input$roadway_reset, {
    updateSelectizeInput(session, "roadway_severity",   selected = "all")
    updateSelectInput(session, "roadway_month_start",   selected = "1")
    updateSelectInput(session, "roadway_month_end",     selected = "12")
    updateSelectInput(session, "roadway_hour_start",    selected = "0")
    updateSelectInput(session, "roadway_hour_end",      selected = "23")
    updateSelectizeInput(session, "roadway_func_focus", selected = "all")
  })

  # Roadway: Key Insight Card
  output$roadway_insights <- renderUI({
    data <- tab_filtered_roadway() %>% sf::st_drop_geometry()

    if (nrow(data) == 0) {
      return(bslib::card(bslib::card_body("No crashes match current filters.")))
    }

    code_to_group <- utils::stack(collision_groups) %>%
      dplyr::rename(code = values, group = ind) %>%
      dplyr::mutate(code = as.character(code))

    top_collision <- data %>%
      dplyr::mutate(code = as.character(acctype1)) %>%
      dplyr::left_join(code_to_group, by = "code") %>%
      dplyr::mutate(group = dplyr::coalesce(group, "Other/Unknown")) %>%
      dplyr::count(group, sort = TRUE) %>%
      dplyr::slice(1) %>%
      dplyr::pull(group)

    adverse_pct <- round(
      100 * sum(data$rdsurf %in% c(2L, 3L, 4L), na.rm = TRUE) / nrow(data), 1
    )

    hs_fatal_pct <- data %>%
      dplyr::filter(!is.na(road_spd_limt), road_spd_limt >= 55) %>%
      dplyr::summarise(pct = round(100 * sum(severity == 2L) / dplyr::n(), 2)) %>%
      dplyr::pull(pct)

    insight_text <- paste0(
      top_collision, " is the most common collision type in this filter. ",
      adverse_pct, "% of crashes occurred on adverse road surfaces ",
      "(wet, snow, or ice). ",
      if (!is.na(hs_fatal_pct) && hs_fatal_pct > 0)
        paste0("On segments with speed limits ≥ 55 mph, ",
               hs_fatal_pct, "% of crashes were fatal.")
      else ""
    )

    bslib::card(
      style = "background:#fff3e0; border-left:4px solid #d95f02;",
      bslib::card_body(
        tags$p(
          tags$strong("\U0001F6E3 Key Insight: "),
          insight_text,
          style = "margin:0; font-size:0.9em;"
        )
      )
    )
  })

  # Roadway: Collision type horizontal bar
  output$roadway_collision_type <- renderPlotly({
    data <- tab_filtered_roadway() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    code_to_group <- utils::stack(collision_groups) %>%
      dplyr::rename(code = values, group = ind) %>%
      dplyr::mutate(code = as.character(code))

    collision_data <- data %>%
      dplyr::mutate(code = as.character(acctype1)) %>%
      dplyr::left_join(code_to_group, by = "code") %>%
      dplyr::mutate(group = dplyr::coalesce(group, "Other/Unknown")) %>%
      dplyr::count(group, name = "crashes") %>%
      dplyr::arrange(crashes) %>%
      dplyr::mutate(group = factor(group, levels = group))

    plot_ly(
      collision_data,
      x           = ~crashes,
      y           = ~group,
      type        = "bar",
      orientation = "h",
      marker      = list(color = "#2166ac",
                         line  = list(color = "#1a4f7a", width = 0.5)),
      customdata  = ~round(100 * crashes / sum(crashes), 1),
      hovertemplate = "<b>%{y}</b><br>Crashes: %{x:,}<br>Share: %{customdata:.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis  = list(title = "Crash count"),
        yaxis  = list(title = ""),
        margin = list(l = 180, r = 20, t = 20, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  #  Roadway: Surface condition donut 
  output$roadway_surface <- renderPlotly({
    data <- tab_filtered_roadway() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    surf_labels_local <- c(
      "0" = "Not stated", "1" = "Dry", "2" = "Wet",
      "3" = "Snow",       "4" = "Ice"
    )
    surf_colors_local <- c(
      "Dry"        = "#4daf4a",
      "Wet"        = "#377eb8",
      "Snow"       = "#a6cee3",
      "Ice"        = "#cab2d6",
      "Not stated" = "#999999"
    )

    surface_data <- data %>%
      dplyr::mutate(
        surface = surf_labels_local[as.character(rdsurf)],
        surface = dplyr::coalesce(surface, "Not stated")
      ) %>%
      dplyr::count(surface) %>%
      dplyr::mutate(pct = round(100 * n / sum(n), 1))

    cols <- unname(surf_colors_local[surface_data$surface])
    cols[is.na(cols)] <- "#bbbbbb"

    plot_ly(
      surface_data,
      labels       = ~surface,
      values       = ~n,
      type         = "pie",
      hole         = 0.5,
      textinfo     = "percent",
      textposition = "inside",
      hoverinfo    = "label+value+percent",
      marker       = list(colors = cols),
      sort         = FALSE
    ) %>%
      layout(
        showlegend    = TRUE,
        legend        = list(orientation = "v", x = 1, y = 0.5),
        margin        = list(t = 10, b = 10, l = 10, r = 100),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Roadway: Traffic control bar
  output$roadway_traffic_control <- renderPlotly({
    data <- tab_filtered_roadway() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    # Use vehicle-level trf_cntl (HSIS integer codes joined at startup)
    trf_col <- if ("veh_trf_cntl" %in% names(data)) data$veh_trf_cntl else
               rep(NA_integer_, nrow(data))

    trf_data <- data.frame(
      raw = as.character(trf_col),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::mutate(
        control = trf_cntl_labels[raw]
      ) %>%
      dplyr::filter(!is.na(control), control != "Not stated") %>%
      dplyr::count(control, name = "crashes") %>%
      dplyr::arrange(dplyr::desc(crashes))

    if (nrow(trf_data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No traffic control data available")))

    plot_ly(
      trf_data,
      x             = ~reorder(control, crashes),
      y             = ~crashes,
      type          = "bar",
      marker        = list(color = "#d95f02",
                           line  = list(color = "#a84c02", width = 0.5)),
      hovertemplate = "<b>%{x}</b><br>Crashes: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis  = list(title = "", tickangle = -35),
        yaxis  = list(title = "Crash count"),
        margin = list(l = 50, r = 20, t = 20, b = 100),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Roadway: Speed limit grouped bar 
  output$roadway_speed_dist <- renderPlotly({
    data <- tab_filtered_roadway() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    sev_lvls_spd <- c("Fatal", "Disabling injury", "Non-disabling injury",
                      "Possible injury", "No injury")
    sev_cols_spd <- c(
      "Fatal"                = "#d62728",
      "Disabling injury"     = "#ff7f0e",
      "Non-disabling injury" = "#fbc6a9",
      "Possible injury"      = "#98df8a",
      "No injury"            = "#2ca02c"
    )

    speed_data <- data %>%
      dplyr::filter(!is.na(road_spd_limt), road_spd_limt > 0) %>%
      dplyr::mutate(
        speed_bin = cut(road_spd_limt,
                        breaks = c(0, 25, 35, 45, 55, 65, 75, Inf),
                        labels = c("≤25","26-35","36-45",
                                   "46-55","56-65","66-75",">75"),
                        right  = TRUE),
        sev_label = factor(
          dplyr::case_when(
            severity == 2L ~ "Fatal",
            severity == 5L ~ "Disabling injury",
            severity == 6L ~ "Non-disabling injury",
            severity == 7L ~ "Possible injury",
            severity == 1L ~ "No injury",
            TRUE           ~ NA_character_
          ), levels = sev_lvls_spd)
      ) %>%
      dplyr::filter(!is.na(sev_label)) %>%
      dplyr::count(speed_bin, sev_label)

    if (nrow(speed_data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No speed data")))

    p_spd <- plot_ly()
    for (sev in sev_lvls_spd) {
      td <- speed_data %>% dplyr::filter(sev_label == sev)
      if (nrow(td) == 0 || sum(td$n) == 0) next
      p_spd <- p_spd %>% add_bars(
        data          = td,
        x             = ~speed_bin,
        y             = ~n,
        name          = sev,
        marker        = list(color = unname(sev_cols_spd[sev])),
        hovertemplate = paste0("<b>%{x} mph</b><br>", sev,
                               ": %{y:,}<extra></extra>")
      )
    }
    p_spd %>%
      layout(
        barmode = "stack",
        xaxis   = list(title = "Posted speed limit (mph)"),
        yaxis   = list(title = "Crash count"),
        legend  = list(orientation = "h", y = -0.25),
        margin  = list(l = 50, r = 20, t = 20, b = 80),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Roadway: Functional class × severity heatmap
  output$roadway_funcls_heatmap <- renderPlotly({
    data <- tab_filtered_roadway() %>% sf::st_drop_geometry()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    sev_order_fc <- c("Fatal", "Disabling injury", "Non-disabling injury",
                      "Possible injury", "No injury")
    sev_cols_fc <- c(
      "Fatal"                = "#d62728",
      "Disabling injury"     = "#ff7f0e",
      "Non-disabling injury" = "#fbc6a9",
      "Possible injury"      = "#98df8a",
      "No injury"            = "#2ca02c"
    )

    fc_sev <- data %>%
      dplyr::filter(!is.na(func_cls), severity %in% c(1L, 2L, 5L, 6L, 7L)) %>%
      dplyr::mutate(
        fc_label  = funccls_labels[as.character(func_cls)],
        sev_label = factor(
          dplyr::case_when(
            severity == 2L ~ "Fatal",
            severity == 5L ~ "Disabling injury",
            severity == 6L ~ "Non-disabling injury",
            severity == 7L ~ "Possible injury",
            severity == 1L ~ "No injury",
            TRUE           ~ NA_character_
          ), levels = sev_order_fc)
      ) %>%
      dplyr::filter(!is.na(fc_label), !is.na(sev_label)) %>%
      dplyr::count(fc_label, sev_label)

    if (nrow(fc_sev) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    p_fc <- plot_ly()
    for (sev in sev_order_fc) {
      td <- fc_sev %>% dplyr::filter(sev_label == sev)
      if (nrow(td) == 0 || sum(td$n) == 0) next
      p_fc <- p_fc %>% add_bars(
        data          = td,
        x             = ~fc_label,
        y             = ~n,
        name          = sev,
        marker        = list(color = unname(sev_cols_fc[sev])),
        hovertemplate = paste0("<b>%{x}</b><br>", sev,
                               ": %{y:,}<extra></extra>")
      )
    }
    p_fc %>%
      layout(
        barmode = "stack",
        xaxis   = list(title = "Functional Class", tickangle = -35),
        yaxis   = list(title = "Crash count"),
        legend  = list(orientation = "v", x = 1.02, y = 0.5,
                       xanchor = "left", yanchor = "middle"),
        margin  = list(l = 50, r = 140, t = 20, b = 120),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Tab-level reactive: Driver 
  tab_filtered_driver <- reactive({
    month_start <- as.integer(input$driver_month_start)
    month_end   <- as.integer(input$driver_month_end)
    hour_start  <- as.integer(input$driver_hour_start)
    hour_end    <- as.integer(input$driver_hour_end)
    
    month_lo <- min(month_start, month_end, na.rm = TRUE)
    month_hi <- max(month_start, month_end, na.rm = TRUE)
    hour_lo  <- min(hour_start, hour_end, na.rm = TRUE)
    hour_hi  <- max(hour_start, hour_end, na.rm = TRUE)
    
    base <- crashes_flat %>%
      dplyr::filter(
        month      >= month_lo & month      <= month_hi,
        crash_hour >= hour_lo  & crash_hour <= hour_hi
      )
    
    base <- apply_severity_filter(base, input$driver_severity)
    base
  })
  

  output$driver_filter_status <- renderText({
    n_tab    <- nrow(tab_filtered_driver())
    n_global <- TOTAL_CRASHES
    active   <- input$driver_severity   != "all" ||
                input$driver_month_start != "1"  ||
                input$driver_month_end   != "12" ||
                input$driver_hour_start  != "0"  ||
                input$driver_hour_end    != "23"
    
    if (active)
      paste0("⚠ Tab filter active: showing ",
             format(n_tab, big.mark = ","), " of ",
             format(n_global, big.mark = ","), " crashes")
    else
      paste0("Showing all ", format(n_tab, big.mark = ","),
             " crashes")
  })

  observeEvent(input$driver_reset, {
    updateSelectizeInput(session, "driver_severity", selected = "all")
    updateSelectInput(session, "driver_month_start", selected = "1")
    updateSelectInput(session, "driver_month_end",   selected = "12")
    updateSelectInput(session, "driver_hour_start",  selected = "0")
    updateSelectInput(session, "driver_hour_end",    selected = "23")
    updateSelectizeInput(session, "driver_veh_type", selected = "all")
  })

  # Driver: joined reactive (crashes + vehicle rows) 
  tab_driver_joined <- reactive({
    crash_subset <- tab_filtered_driver() %>%
      sf::st_drop_geometry() %>%
      dplyr::select(caseno, severity, rte_nbr, month, crash_hour,
                    geocode_method)

    veh_type_filter <- input$driver_veh_type

    result <- vehicles %>%
      dplyr::inner_join(crash_subset, by = "caseno")

    if (!is.null(veh_type_filter) && veh_type_filter != "all") {
      vtype_codes <- if (veh_type_filter == "heavy") c(4L, 5L, 6L, 7L) else
                     as.integer(veh_type_filter)
      vtype_int <- suppressWarnings(as.integer(vehicles$vehtype[1]))
      result <- result %>%
        dplyr::filter(suppressWarnings(as.integer(vehtype)) %in% vtype_codes)
    }

    result
  })

  # Driver: Key Insight card 
  output$driver_insights <- renderUI({
    data <- tab_driver_joined() %>% dplyr::filter(vehno == 1)

    if (nrow(data) == 0)
      return(bslib::card(bslib::card_body("No crashes match current filters.")))

    top_contrib <- data %>%
      dplyr::mutate(
        code        = stringr::str_pad(
                        as.character(suppressWarnings(as.integer(contrib1))),
                        2, pad = "0"),
        contrib_lbl = contrib_labels[code]
      ) %>%
      dplyr::filter(!is.na(contrib_lbl), contrib_lbl != "No violation") %>%
      dplyr::count(contrib_lbl, sort = TRUE) %>%
      dplyr::slice(1) %>%
      dplyr::pull(contrib_lbl)

    defect_pct <- round(
      100 * sum(!is.na(data$vehcond1) & data$vehcond1 != 12, na.rm = TRUE) /
        nrow(data), 1)

    young_pct <- round(
      100 * sum(!is.na(data$drv_age) & data$drv_age > 0 &
                  data$drv_age <= 25) / nrow(data), 1)

    alcohol_pct <- round(
      100 * sum(!is.na(data$contrib1) & data$contrib1 == 1,
                na.rm = TRUE) / nrow(data), 1)

    insight_text <- paste0(
      if (length(top_contrib) > 0 && !is.na(top_contrib))
        paste0("The most cited driver violation is '", top_contrib, "'. ")
      else "",
      "Drivers aged 25 or younger account for ", young_pct,
      "% of at-fault drivers. ",
      alcohol_pct, "% of at-fault drivers were cited for alcohol influence. ",
      if (defect_pct > 5)
        paste0(defect_pct, "% of involved vehicles had a mechanical defect.")
      else
        paste0("Only ", defect_pct,
               "% of involved vehicles had a mechanical defect, ",
               "confirming driver behavior as the dominant factor.")
    )

    bslib::card(
      style = "background:#f3e8f9; border-left:4px solid #7b2d8b;",
      bslib::card_body(
        tags$p(
          tags$strong("\U0001F464 Key Insight: "),
          insight_text,
          style = "margin:0; font-size:0.9em;"
        )
      )
    )
  })

  # Driver: Contributing circumstances horizontal bar
  output$driver_contrib <- renderPlotly({
    data <- tab_driver_joined() %>% dplyr::filter(vehno == 1)

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    contrib_data <- data %>%
      dplyr::mutate(
        code        = stringr::str_pad(
                        as.character(suppressWarnings(as.integer(contrib1))),
                        2, pad = "0"),
        contrib_lbl = contrib_labels[code],
        contrib_lbl = ifelse(is.na(contrib_lbl), "Not stated", contrib_lbl)
      ) %>%
      dplyr::filter(contrib_lbl != "Not stated", contrib_lbl != "No violation") %>%
      dplyr::count(contrib_lbl, name = "crashes") %>%
      dplyr::arrange(crashes) %>%
      dplyr::slice_tail(n = 12) %>%
      dplyr::mutate(contrib_lbl = factor(contrib_lbl, levels = contrib_lbl))

    if (nrow(contrib_data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No violation data")))

    plot_ly(
      contrib_data,
      x             = ~crashes,
      y             = ~contrib_lbl,
      type          = "bar",
      orientation   = "h",
      marker        = list(color = "#7b2d8b",
                           line  = list(color = "#5a1f67", width = 0.5)),
      hovertemplate = "<b>%{y}</b><br>Crashes: %{x:,}<br>%{customdata:.1f}% of violations<extra></extra>",
      customdata    = ~round(100 * crashes / sum(crashes), 1)
    ) %>%
      layout(
        xaxis  = list(title = "Crash count"),
        yaxis  = list(title = ""),
        margin = list(l = 240, r = 20, t = 20, b = 40),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Driver: Age group bar 
  output$driver_age <- renderPlotly({
    data <- tab_driver_joined() %>% dplyr::filter(vehno == 1)

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    age_lvls <- c("<16", "16-20", "21-25", "26-35",
                  "36-45", "46-55", "56-65", "66-75", "76+")
    sev_lvls_age <- c("Fatal", "Disabling injury", "Non-disabling injury",
                      "Possible injury", "No injury")
    sev_cols_age <- c(
      "Fatal"                = "#d62728",
      "Disabling injury"     = "#ff7f0e",
      "Non-disabling injury" = "#fbc6a9",
      "Possible injury"      = "#98df8a",
      "No injury"            = "#2ca02c"
    )

    age_data <- data %>%
      dplyr::filter(!is.na(drv_age), drv_age > 0) %>%
      dplyr::mutate(
        age_group = cut(
          suppressWarnings(as.integer(drv_age)),
          breaks = c(0, 16, 21, 26, 36, 46, 56, 66, 76, Inf),
          labels = age_lvls,
          right  = FALSE
        ),
        age_group = factor(age_group, levels = age_lvls),
        sev_label = factor(dplyr::case_when(
          severity == 2L ~ "Fatal",
          severity == 5L ~ "Disabling injury",
          severity == 6L ~ "Non-disabling injury",
          severity == 7L ~ "Possible injury",
          severity == 1L ~ "No injury",
          TRUE           ~ NA_character_
        ), levels = sev_lvls_age)
      ) %>%
      dplyr::filter(!is.na(age_group), !is.na(sev_label)) %>%
      dplyr::count(age_group, sev_label, name = "crashes") %>%
      tidyr::complete(
        age_group = factor(age_lvls, levels = age_lvls),
        sev_label = factor(sev_lvls_age, levels = sev_lvls_age),
        fill = list(crashes = 0)
      )

    if (nrow(age_data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No age data")))

    p_age <- plot_ly()
    for (sev in sev_lvls_age) {
      trace_data <- age_data %>% dplyr::filter(sev_label == sev)
      if (nrow(trace_data) == 0 || sum(trace_data$crashes) == 0) next
      p_age <- p_age %>% add_bars(
        data          = trace_data,
        x             = ~age_group,
        y             = ~crashes,
        name          = sev,
        marker        = list(color = unname(sev_cols_age[sev])),
        hovertemplate = paste0("<b>Age %{x}</b><br>", sev,
                               ": %{y:,}<extra></extra>")
      )
    }

    p_age %>%
      layout(
        barmode = "stack",
        xaxis  = list(
          title         = "Driver age group",
          categoryorder = "array",
          categoryarray = age_lvls
        ),
        yaxis  = list(title = "Crash count"),
        legend = list(orientation = "h", y = -0.25),
        margin = list(l = 50, r = 20, t = 20, b = 80),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Driver: Vehicle condition donut 
  output$driver_vehcond <- renderPlotly({
    data <- tab_driver_joined()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    vehcond_data <- data %>%
      dplyr::mutate(
        code     = stringr::str_pad(
                     as.character(suppressWarnings(as.integer(vehcond1))),
                     2, pad = "0"),
        cond_lbl = vehcond_labels[code],
        cond_lbl = ifelse(is.na(cond_lbl), "Not stated", cond_lbl)
      ) %>%
      dplyr::count(cond_lbl, name = "n") %>%
      dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
      dplyr::arrange(dplyr::desc(n))

    vehcond_colors <- c(
      "No defects"                            = "#4daf4a",
      "Not stated"                            = "#999999",
      "Defective brakes"                      = "#d62728",
      "Tires worn/smooth"                     = "#ff7f0e",
      "Tires punctured/blown"                 = "#e7969c",
      "Other defects"                         = "#c49c94",
      "Defective steering"                    = "#9467bd",
      "Power failure"                         = "#8c564b",
      "Defective headlights"                  = "#1f77b4",
      "Defective rear lights"                 = "#aec7e8",
      "Lost a wheel"                          = "#ffbb78",
      "Headlights glaring"                    = "#f7b6d2",
      "Other lights/reflectors insufficient"  = "#dbdb8d",
      "Motorcycle lights off"                 = "#bcbd22",
      "Studded tires"                         = "#17becf",
      "Motorcycle windshield installed"       = "#9edae5",
      "Truck safety inspection"               = "#7f7f7f"
    )

    plot_ly(
      vehcond_data,
      labels       = ~cond_lbl,
      values       = ~n,
      type         = "pie",
      hole         = 0.5,
      textinfo     = "percent",
      textposition = "inside",
      hoverinfo    = "label+value+percent",
      marker       = list(colors = unname(vehcond_colors[vehcond_data$cond_lbl])),
      sort         = FALSE
    ) %>%
      layout(
        showlegend    = TRUE,
        legend        = list(orientation = "v", x = 1, y = 0.5,
                             font = list(size = 10)),
        margin        = list(t = 10, b = 10, l = 10, r = 180),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Driver: Action before crash bar 
  output$driver_action <- renderPlotly({
    data <- tab_driver_joined() %>% dplyr::filter(vehno == 1)

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    action_data <- data %>%
      dplyr::mutate(
        code       = stringr::str_pad(
                       as.character(suppressWarnings(as.integer(drv_actn))),
                       2, pad = "0"),
        action_lbl = drv_actn_labels[code],
        action_lbl = ifelse(is.na(action_lbl), "Not stated", action_lbl)
      ) %>%
      dplyr::filter(action_lbl != "Not stated") %>%
      dplyr::count(action_lbl, name = "crashes") %>%
      dplyr::arrange(dplyr::desc(crashes)) %>%
      dplyr::slice_head(n = 10)

    if (nrow(action_data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No action data")))

    plot_ly(
      action_data,
      x    = ~reorder(action_lbl, crashes),
      y    = ~crashes,
      type = "bar",
      marker = list(color = "#1f78b4",
                    line  = list(color = "#145a8a", width = 0.5)),
      hovertemplate = "<b>%{x}</b><br>Crashes: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis  = list(title = "", tickangle = -35),
        yaxis  = list(title = "Crash count"),
        margin = list(l = 50, r = 20, t = 20, b = 130),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Driver: Vehicle type — crash volume vs fatal rate dual axis 
  output$driver_vehtype <- renderPlotly({
    data <- tab_driver_joined()

    if (nrow(data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    vehtype_data <- data %>%
      dplyr::mutate(
        code  = stringr::str_pad(
                  as.character(suppressWarnings(as.integer(vehtype))),
                  2, pad = "0"),
        vtype = vehtype_labels[code],
        vtype = ifelse(is.na(vtype), "Not stated", vtype)
      ) %>%
      dplyr::filter(vtype != "Not stated") %>%
      dplyr::group_by(vtype) %>%
      dplyr::summarise(
        total_crashes = dplyr::n_distinct(caseno),
        fatal_crashes = dplyr::n_distinct(caseno[severity == 2L]),
        fatal_rate_pct = round(100 * fatal_crashes / total_crashes, 2),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(total_crashes)) %>%
      dplyr::slice_head(n = 10)

    if (nrow(vehtype_data) == 0)
      return(plot_ly() %>% layout(title = list(text = "No vehicle type data")))

    plot_ly() %>%
      add_bars(
        data          = vehtype_data,
        x             = ~reorder(vtype, -total_crashes),
        y             = ~total_crashes,
        name          = "Total crashes",
        marker        = list(color = "#2166ac"),
        hovertemplate = "<b>%{x}</b><br>Total: %{y:,}<extra></extra>"
      ) %>%
      add_lines(
        data          = vehtype_data,
        x             = ~reorder(vtype, -total_crashes),
        y             = ~fatal_rate_pct,
        name          = "Fatal rate (%)",
        yaxis         = "y2",
        line          = list(color = "#d62728", width = 2.5),
        marker        = list(color = "#d62728", size = 8),
        hovertemplate = "<b>%{x}</b><br>Fatal rate: %{y:.2f}%<extra></extra>"
      ) %>%
      layout(
        xaxis  = list(title = "", tickangle = -35),
        yaxis  = list(title = "Crash count"),
        yaxis2 = list(title      = "Fatal rate (%)",
                      overlaying = "y",
                      side       = "right",
                      showgrid   = FALSE,
                      rangemode  = "tozero"),
        legend = list(orientation = "h", y = -0.35),
        margin = list(l = 60, r = 70, t = 20, b = 130),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Tab-level reactive: Hotspot
  tab_filtered_hotspot <- reactive({
    month_start <- as.integer(input$hotspot_month_start)
    month_end   <- as.integer(input$hotspot_month_end)
    hour_start  <- as.integer(input$hotspot_hour_start)
    hour_end    <- as.integer(input$hotspot_hour_end)
    
    month_lo <- min(month_start, month_end, na.rm = TRUE)
    month_hi <- max(month_start, month_end, na.rm = TRUE)
    hour_lo  <- min(hour_start, hour_end, na.rm = TRUE)
    hour_hi  <- max(hour_start, hour_end, na.rm = TRUE)
    
    base <- crashes_flat %>%
      dplyr::filter(
        month      >= month_lo & month      <= month_hi,
        crash_hour >= hour_lo  & crash_hour <= hour_hi
      )
    
    base <- apply_severity_filter(base, input$hotspot_severity)
    base
  })
  
  output$hotspot_filter_status <- renderText({
    n_tab    <- nrow(tab_filtered_hotspot())
    n_global <- TOTAL_CRASHES
    active   <- input$hotspot_severity   != "all" ||
                input$hotspot_month_start != "1"  ||
                input$hotspot_month_end   != "12" ||
                input$hotspot_hour_start  != "0"  ||
                input$hotspot_hour_end    != "23"
    
    if (active)
      paste0("⚠ Tab filter active: showing ",
             format(n_tab, big.mark = ","), " of ",
             format(n_global, big.mark = ","), " crashes")
    else
      paste0("Showing all ", format(n_tab, big.mark = ","),
             " crashes")
  })

  observeEvent(input$hotspot_reset, {
    updateSelectizeInput(session, "hotspot_severity",   selected = "all")
    updateSelectInput(session, "hotspot_month_start",   selected = "1")
    updateSelectInput(session, "hotspot_month_end",     selected = "12")
    updateSelectInput(session, "hotspot_hour_start",    selected = "0")
    updateSelectInput(session, "hotspot_hour_end",      selected = "23")
    updateNumericInput(session,  "hotspot_min_crashes", value = 5)
    updateSelectizeInput(session, "hotspot_metric",     selected = "epdo_total")
  })

  # Hotspot: Data reactives
  hotspot_data <- reactive({
    sev_changed   <- input$hotspot_severity != "all"
    month_changed <- as.integer(input$hotspot_month_start) != 1L ||
                     as.integer(input$hotspot_month_end)   != 12L
    hour_changed  <- as.integer(input$hotspot_hour_start)  != 0L  ||
                     as.integer(input$hotspot_hour_end)    != 23L

    if (!sev_changed && !month_changed && !hour_changed) {
      return(hotspots_baseline)
    }

    filtered <- tab_filtered_hotspot()
    if (nrow(filtered) < 50) return(hotspots_baseline)

    filtered_sf <- crashes[crashes$caseno %in% filtered$caseno, ]

    sliding_window_hotspots(
      crashes_sf   = filtered_sf,
      window_miles = 0.5,
      step_miles   = 0.1,
      min_crashes  = max(3L, as.integer(input$hotspot_min_crashes))
    )
  }) %>% debounce(800)

  hotspot_ranked <- reactive({
    hs_df <- hotspot_data()
    if (is.null(hs_df) || nrow(hs_df) == 0) return(data.frame())

    top_n_hotspots(
      hotspots_df = hs_df,
      n           = 20,
      metric      = input$hotspot_metric,
      min_aadt    = 0
    ) %>%
      dplyr::mutate(
        rank         = dplyr::row_number(),
        route_lbl    = route_label(rte_nbr),
        segment_desc = paste0(
          route_lbl, " MP ",
          round(window_start_mp, 1), "–",
          round(window_end_mp, 1)
        )
      )
  })

  # Hotspot: Key insight card 
  output$hotspot_insights <- renderUI({
    hs <- hotspot_ranked()
    if (nrow(hs) == 0) {
      return(bslib::card(
        bslib::card_body("No hotspots match current filters.")
      ))
    }

    top1     <- hs[1, ]
    n_fatal  <- sum(hs$fatal_count > 0, na.rm = TRUE)
    max_rate <- hs %>%
      dplyr::filter(!is.na(mean_aadt), mean_aadt >= 5000,
                    !is.na(crash_rate_mvmt)) %>%
      dplyr::slice_max(crash_rate_mvmt, n = 1, with_ties = FALSE)

    insight_text <- paste0(
      "The top-ranked segment is ", top1$segment_desc,
      " with an EPDO of ", round(top1$epdo_total),
      " and ", top1$crash_count, " crashes in 2002. ",
      n_fatal, " of the top 20 hotspot windows contain at least one fatal crash. ",
      if (nrow(max_rate) > 0)
        paste0(
          "The highest crash rate on roads with AADT ≥ 5,000 is ",
          round(max_rate$crash_rate_mvmt[1], 2),
          " crashes per million VMT at ",
          max_rate$segment_desc[1], "."
        )
      else ""
    )

    bslib::card(
      style = "background:#fef9e7; border-left:4px solid #f39c12;",
      bslib::card_body(
        tags$p(
          tags$strong("⚠️ Key Insight: "),
          insight_text,
          style = "margin:0; font-size:0.9em;"
        )
      )
    )
  })

  # Hotspot: Table subtitle
  output$hotspot_table_subtitle <- renderText({
    switch(input$hotspot_metric,
      "epdo_total"      = "Ranked by: EPDO Total (severity-weighted)",
      "crash_count"     = "Ranked by: Raw Crash Count",
      "crash_rate_mvmt" = "Ranked by: Crash Rate per Million VMT"
    )
  })

  # Hotspot: Ranked table
  output$hotspot_table <- DT::renderDataTable({
    hs <- hotspot_ranked()
    if (nrow(hs) == 0) {
      return(DT::datatable(
        data.frame(Message = "No hotspots found for current filters"),
        rownames = FALSE,
        options  = list(dom = "t")
      ))
    }

    display <- hs %>%
      dplyr::select(
        Rank        = rank,
        Segment     = segment_desc,
        Crashes     = crash_count,
        Fatal       = fatal_count,
        Serious     = serious_count,
        EPDO        = epdo_total,
        `Rate/MVMT` = crash_rate_mvmt,
        AADT        = mean_aadt
      ) %>%
      dplyr::mutate(
        EPDO        = round(EPDO, 0),
        `Rate/MVMT` = round(`Rate/MVMT`, 2),
        AADT        = format(round(AADT), big.mark = ",")
      )

    DT::datatable(
      display,
      selection  = "single",
      rownames   = FALSE,
      options    = list(
        pageLength     = 20,
        dom            = "t",
        scrollY        = "400px",
        scrollCollapse = TRUE,
        columnDefs     = list(
          list(className = "dt-center", targets = c(0, 2:7))
        )
      ),
      class = "compact stripe hover"
    ) %>%
      DT::formatStyle(
        "Fatal",
        backgroundColor = DT::styleInterval(
          c(0, 1),
          c("white", "#fff3cd", "#f8d7da")
        )
      ) %>%
      DT::formatStyle(
        "EPDO",
        background         = DT::styleColorBar(hs$epdo_total, "#b3d1f5"),
        backgroundSize     = "100% 90%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # Hotspot: Map 
  output$hotspot_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -120.7, lat = 47.4, zoom = 7) %>%
      addPolygons(
        data    = wa_boundary,
        fill    = FALSE,
        color   = "#2c3e50",
        weight  = 1.5,
        opacity = 0.5
      )
  })

  observe({
    req(!is.null(input$hotspot_map_zoom))  # wait for Leaflet to initialise in browser
    hs <- hotspot_ranked()
    if (nrow(hs) == 0) {
      leafletProxy("hotspot_map") %>%
        clearGroup("Hotspots") %>%
        clearGroup("Routes") %>%
        clearControls()
      return()
    }


    routes_main_3857 <- routes_sf %>%
      dplyr::filter(is.na(RelRouteTy)) %>%
      dplyr::mutate(route_num = as.integer(StateRoute)) %>%
      sf::st_transform(3857)

    hs_matched <- routes_main_3857 %>%
      dplyr::inner_join(
        hs %>% dplyr::mutate(
          rte_nbr_int    = as.integer(rte_nbr),
          window_mid_mp  = (window_start_mp + window_end_mp) / 2
        ),
        by           = c("route_num" = "rte_nbr_int"),
        relationship = "many-to-many"
      ) %>%
      dplyr::filter(window_mid_mp >= BARM, window_mid_mp <= EARM) %>%
      dplyr::group_by(rank) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    proxy <- leafletProxy("hotspot_map") %>%
      clearGroup("Hotspots") %>%
      clearGroup("Routes") %>%
      addPolylines(
        data    = routes_sf %>% dplyr::filter(is.na(RelRouteTy)),
        color   = "#cccccc",
        weight  = 1,
        opacity = 0.4,
        group   = "Routes"
      )

    if (nrow(hs_matched) == 0) {
      proxy %>% clearControls()
      return()
    }

    frac_start <- pmax(0, pmin(1,
      (hs_matched$window_start_mp - hs_matched$BARM) /
      (hs_matched$EARM - hs_matched$BARM)
    ))
    frac_end <- pmax(0, pmin(1,
      (hs_matched$window_end_mp - hs_matched$BARM) /
      (hs_matched$EARM - hs_matched$BARM)
    ))

    metric_vals <- hs_matched[[input$hotspot_metric]]
    metric_safe <- dplyr::if_else(is.na(metric_vals), 0, metric_vals)

    pal <- leaflet::colorNumeric(
      palette = "YlOrRd",
      domain  = metric_safe,
      reverse = FALSE
    )

    hs_matched <- hs_matched %>%
      dplyr::mutate(
        popup_txt = paste0(
          "<b>Rank #", rank, " — ", segment_desc, "</b><br>",
          "Crashes: ", crash_count,
          " | Fatal: ", fatal_count,
          " | Serious: ", serious_count, "<br>",
          "EPDO: ", round(epdo_total),
          " | Rate/MVMT: ",
          dplyr::if_else(is.na(crash_rate_mvmt), "N/A",
                         as.character(round(crash_rate_mvmt, 2))), "<br>",
          "Mean AADT: ", format(round(mean_aadt), big.mark = ",")
        )
      )

    geoms_3857 <- sf::st_geometry(hs_matched)

    for (i in seq_len(nrow(hs_matched))) {
      tryCatch({
        pt_s    <- sf::st_line_interpolate(geoms_3857[i], frac_start[i], normalized = TRUE)
        pt_e    <- sf::st_line_interpolate(geoms_3857[i], frac_end[i],   normalized = TRUE)
        coords  <- rbind(sf::st_coordinates(pt_s), sf::st_coordinates(pt_e))
        seg_sf  <- sf::st_sf(
          geometry = sf::st_sfc(sf::st_linestring(coords[, 1:2]), crs = 3857)
        )
        seg_sf  <- sf::st_transform(seg_sf, 4326)

        proxy <<- proxy %>%
          addPolylines(
            data    = seg_sf,
            color   = pal(metric_safe[i]),
            weight  = 6,
            opacity = 0.9,
            popup   = hs_matched$popup_txt[i],
            group   = "Hotspots"
          )
      }, error = function(e) message("Hotspot seg ", i, ": ", conditionMessage(e)))
    }

    proxy %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = metric_safe,
        title    = switch(input$hotspot_metric,
          "epdo_total"      = "EPDO Total",
          "crash_count"     = "Crash Count",
          "crash_rate_mvmt" = "Rate/MVMT"
        ),
        opacity = 0.9
      )
  })

  # Hotspot: Metric comparison chart
  output$hotspot_comparison <- renderPlotly({
    hs_df <- hotspot_data()
    if (is.null(hs_df) || nrow(hs_df) == 0)
      return(plot_ly() %>% layout(title = list(text = "No hotspot data")))

    make_top <- function(metric_name) {
      df <- top_n_hotspots(hs_df, 10, metric_name) %>%
        dplyr::mutate(
          seg = paste0(
            route_label(rte_nbr), " MP ",
            round(window_start_mp, 1), "-",
            round(window_end_mp, 1)
          )
        )
      if (metric_name == "crash_rate_mvmt")
        df <- df %>% dplyr::filter(!is.na(mean_aadt), mean_aadt >= 5000)
      df
    }

    top_epdo  <- make_top("epdo_total")
    top_count <- make_top("crash_count")
    top_rate  <- make_top("crash_rate_mvmt")

    all_segs <- dplyr::bind_rows(
      top_epdo  %>% dplyr::select(seg, epdo_total, crash_count, crash_rate_mvmt),
      top_count %>% dplyr::select(seg, epdo_total, crash_count, crash_rate_mvmt),
      top_rate  %>% dplyr::select(seg, epdo_total, crash_count, crash_rate_mvmt)
    ) %>%
      dplyr::distinct(seg, .keep_all = TRUE) %>%
      dplyr::arrange(dplyr::desc(epdo_total)) %>%
      dplyr::slice_head(n = 15)

    if (nrow(all_segs) == 0)
      return(plot_ly() %>% layout(title = list(text = "No data")))

    plot_ly(all_segs) %>%
      add_bars(
        x             = ~seg,
        y             = ~epdo_total,
        name          = "EPDO Total",
        marker        = list(color = "#d62728"),
        hovertemplate = "<b>%{x}</b><br>EPDO: %{y:,}<extra></extra>"
      ) %>%
      add_bars(
        x             = ~seg,
        y             = ~crash_count,
        name          = "Crash Count",
        marker        = list(color = "#2166ac"),
        hovertemplate = "<b>%{x}</b><br>Crashes: %{y:,}<extra></extra>"
      ) %>%
      layout(
        barmode = "group",
        xaxis   = list(title = "", tickangle = -40),
        yaxis   = list(title = "Value"),
        legend  = list(orientation = "h", y = -0.35),
        margin  = list(l = 60, r = 20, t = 20, b = 160),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Hotspot: Segment profile panel 
  output$hotspot_profile <- renderUI({
    sel <- input$hotspot_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(
        bslib::card(
          style = "background:#f8f9fa; border:1px dashed #dee2e6;",
          bslib::card_body(
            tags$p(
              style = "color:#6c757d; text-align:center; margin:20px 0;",
              "\U0001f447 Click a row in the table above to see the segment profile"
            )
          )
        )
      )
    }

    hs <- hotspot_ranked()
    if (nrow(hs) < sel) return(NULL)
    seg <- hs[sel, ]

    seg_crashes <- tab_filtered_hotspot() %>%
      dplyr::filter(
        geocode_method == "mainline",
        rte_nbr  == seg$rte_nbr,
        milepost >= seg$window_start_mp,
        milepost <  seg$window_end_mp
      )

    code_to_group <- utils::stack(collision_groups) %>%
      dplyr::rename(code = values, group = ind) %>%
      dplyr::mutate(code = as.character(code))

    top_collisions <- if (nrow(seg_crashes) > 0) {
      seg_crashes %>%
        dplyr::mutate(code = as.character(acctype1)) %>%
        dplyr::left_join(code_to_group, by = "code") %>%
        dplyr::mutate(col_grp = dplyr::coalesce(as.character(group), "Other")) %>%
        dplyr::count(col_grp, sort = TRUE) %>%
        dplyr::slice_head(n = 3) %>%
        dplyr::pull(col_grp) %>%
        paste(collapse = ", ")
    } else "None"

    sev_summary <- if (nrow(seg_crashes) > 0) {
      seg_crashes %>%
        dplyr::count(severity) %>%
        dplyr::mutate(
          label = severity_labels[as.character(severity)],
          label = dplyr::if_else(is.na(label), "Unknown", label)
        ) %>%
        dplyr::arrange(severity) %>%
        dplyr::mutate(text = paste0(label, ": ", n)) %>%
        dplyr::pull(text) %>%
        paste(collapse = " | ")
    } else "No crashes found"

    bslib::card(
      style = "background:#f0f7ff; border-left:4px solid #2166ac;",
      bslib::card_header(
        paste0("Segment Profile: ", seg$segment_desc, " (Rank #", seg$rank, ")")
      ),
      bslib::card_body(
        fluidRow(
          column(3,
            tags$h5("Crash Summary"),
            tags$p(paste0("Total crashes: ", seg$crash_count)),
            tags$p(paste0("Fatal: ",   seg$fatal_count)),
            tags$p(paste0("Serious: ", seg$serious_count)),
            tags$p(paste0("EPDO: ",    round(seg$epdo_total)))
          ),
          column(3,
            tags$h5("Road Context"),
            tags$p(paste0("Mean AADT: ",
              format(round(seg$mean_aadt), big.mark = ","))),
            tags$p(paste0("Rate/MVMT: ",
              dplyr::if_else(is.na(seg$crash_rate_mvmt), "N/A",
                             as.character(round(seg$crash_rate_mvmt, 2))))),
            tags$p(paste0("Window: ",
              round(seg$window_start_mp, 1), " – ",
              round(seg$window_end_mp, 1), " mi"))
          ),
          column(3,
            tags$h5("Severity Mix"),
            tags$p(sev_summary, style = "font-size:0.85em;")
          ),
          column(3,
            tags$h5("Top Collision Types"),
            tags$p(top_collisions, style = "font-size:0.85em;"),
            tags$br(),
            actionButton(
              "zoom_to_segment",
              "Zoom map to segment",
              class = "btn-sm btn-primary",
              icon  = icon("map-marker-alt")
            )
          )
        )
      )
    )
  })

  # Observer: zoom the hotspot map when Zoom button clicked
  observeEvent(input$zoom_to_segment, {
    sel <- input$hotspot_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return()

    hs <- hotspot_ranked()
    if (nrow(hs) < sel) return()
    seg <- hs[sel, ]

    # Use the actual geocoded crash points in the window as the zoom target
    seg_pts <- crashes %>%
      dplyr::filter(
        geocode_method == "mainline",
        rte_nbr  == seg$rte_nbr,
        milepost >= seg$window_start_mp,
        milepost <  seg$window_end_mp
      )

    if (nrow(seg_pts) == 0) return()

    coords_mat <- sf::st_coordinates(seg_pts)

    leafletProxy("hotspot_map") %>%
      setView(
        lng  = mean(coords_mat[, 1]),
        lat  = mean(coords_mat[, 2]),
        zoom = 14
      )
  })

}

shinyApp(ui, server)
