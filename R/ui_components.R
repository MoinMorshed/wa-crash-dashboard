library(shiny)
library(bslib)

# Compact secondary filter strip rendered at the top of each data tab.
# tab_id       — unique prefix (e.g. "map", "temporal") used for input IDs
# extra_inputs — additional column() calls for tab-specific controls; can be
#                a single column() or tagList() of columns
compact_filter_strip <- function(tab_id, extra_inputs = NULL) {
  month_choices <- setNames(as.character(1:12), month.abb)
  hour_choices  <- setNames(as.character(0:23), sprintf("%02d:00", 0:23))
  
  bslib::card(
    class = "mb-2 filter-strip",
    style = "background:#f8f9fa; border:1px solid #dee2e6; overflow:visible;",
    bslib::card_body(
      class = "py-2",
      style = "padding:10px 14px; min-height:150px; overflow:visible;",
      fluidRow(
        column(
          2,
          selectizeInput(
            paste0(tab_id, "_severity"),
            label    = "Severity:",
            choices  = severity_tab_choices,
            selected = "all",
            multiple = FALSE,
            options  = list(dropdownParent = "body"),
            width    = "100%"
          )
        ),
        column(
          2,
          tags$label("Month range:", class = "form-label"),
          div(
            style = "display:flex; gap:8px;",
            div(
              style = "flex:1;",
              selectInput(
                paste0(tab_id, "_month_start"),
                label    = NULL,
                choices  = month_choices,
                selected = "1",
                width    = "100%"
              )
            ),
            div(
              style = "flex:1;",
              selectInput(
                paste0(tab_id, "_month_end"),
                label    = NULL,
                choices  = month_choices,
                selected = "12",
                width    = "100%"
              )
            )
          )
        ),
        column(
          2,
          tags$label("Hour range:", class = "form-label"),
          div(
            style = "display:flex; gap:8px;",
            div(
              style = "flex:1;",
              selectInput(
                paste0(tab_id, "_hour_start"),
                label    = NULL,
                choices  = hour_choices,
                selected = "0",
                width    = "100%"
              )
            ),
            div(
              style = "flex:1;",
              selectInput(
                paste0(tab_id, "_hour_end"),
                label    = NULL,
                choices  = hour_choices,
                selected = "23",
                width    = "100%"
              )
            )
          )
        ),
        if (!is.null(extra_inputs)) extra_inputs,
        column(
          1,
          br(),
          actionButton(
            paste0(tab_id, "_reset"),
            "Reset",
            class = "btn-sm btn-outline-secondary",
            style = "margin-top:4px;"
          )
        )
      ),
      fluidRow(
        column(12,
               textOutput(paste0(tab_id, "_filter_status")),
               tags$style(paste0(
                 "#", tab_id, "_filter_status {",
                 "font-size:0.8em; color:#6c757d; padding:2px 0;",
                 "}"
               ))
        )
      )
    )
  )
}


map_filter_strip <- function() {
  month_choices <- setNames(as.character(1:12), month.abb)
  hour_choices  <- setNames(as.character(0:23), sprintf("%02d:00", 0:23))
  
  bslib::card(
    class = "mb-2 filter-strip map-filter-strip",
    style = "background:#f8f9fa; border:1px solid #dee2e6; overflow:visible;",
    bslib::card_body(
      class = "py-2",
      style = "padding:10px 14px; min-height:118px; overflow:visible;",
      tags$div(
        class = "map-filter-grid",
        tags$div(
          class = "map-filter-control",
          selectizeInput(
            "map_severity",
            label    = "Severity:",
            choices  = severity_tab_choices,
            selected = "all",
            multiple = FALSE,
            options  = list(dropdownParent = "body"),
            width    = "100%"
          )
        ),
        tags$div(
          class = "map-filter-control",
          tags$label("Month range:", class = "form-label"),
          div(
            style = "display:flex; gap:8px;",
            div(
              style = "flex:1;",
              selectInput(
                "map_month_start",
                label    = NULL,
                choices  = month_choices,
                selected = "1",
                width    = "100%"
              )
            ),
            div(
              style = "flex:1;",
              selectInput(
                "map_month_end",
                label    = NULL,
                choices  = month_choices,
                selected = "12",
                width    = "100%"
              )
            )
          )
        ),
        tags$div(
          class = "map-filter-control",
          tags$label("Hour range:", class = "form-label"),
          div(
            style = "display:flex; gap:8px;",
            div(
              style = "flex:1;",
              selectInput(
                "map_hour_start",
                label    = NULL,
                choices  = hour_choices,
                selected = "0",
                width    = "100%"
              )
            ),
            div(
              style = "flex:1;",
              selectInput(
                "map_hour_end",
                label    = NULL,
                choices  = hour_choices,
                selected = "23",
                width    = "100%"
              )
            )
          )
        ),
        tags$div(
          class = "map-filter-control map-filter-route",
          selectizeInput(
            "map_route",
            label    = "Route:",
            choices  = c("All routes" = "all", route_choices),
            selected = "all",
            multiple = FALSE,
            options  = list(dropdownParent = "body"),
            width    = "100%"
          )
        ),
        tags$div(
          class = "map-filter-control map-filter-collision",
          shinyWidgets::pickerInput(
            "map_acctype_filter",
            label    = "Collision type:",
            choices  = names(collision_groups),
            selected = names(collision_groups),
            multiple = TRUE,
            options  = shinyWidgets::pickerOptions(
              actionsBox            = TRUE,
              liveSearch            = TRUE,
              liveSearchPlaceholder = "Search types...",
              selectedTextFormat    = "count > 1",
              countSelectedText     = "{0} types selected",
              container             = "body"
            ),
            width = "100%"
          )
        ),
        tags$div(
          class = "map-filter-control",
          selectizeInput(
            "map_road_type",
            label    = "Road type:",
            choices  = c("Mainline only" = "mainline",
                         "All (incl. ramps)" = "all"),
            selected = "mainline",
            multiple = FALSE,
            options  = list(dropdownParent = "body"),
            width    = "100%"
          )
        ),
        tags$div(
          class = "map-filter-control map-filter-reset",
          actionButton(
            "map_reset",
            "Reset",
            class = "btn-sm btn-outline-secondary",
            style = "width:100%;"
          )
        )
      ),
      tags$div(
        class = "map-filter-status",
        textOutput("map_filter_status"),
        tags$style(
          "#map_filter_status {font-size:0.8em; color:#6c757d; padding:2px 0;}"
        )
      )
    )
  )
}
