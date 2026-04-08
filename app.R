# This is a draft of the Coffee dashboard.
# The focus of this dashboard is to allow managers to track KPIs of field teams
# on extension activities (i.e. enrollment and training of farmers).
# An emphasis will be put on presenting the data visually on a map.
# The dashboard is built using the Shiny web framework. Currently, the app is developed in R
# for quick prototyping by it will be converted to Python (still using Shiny) for deployment on Dataiku
# In case more functionality is requested by the coffee team, the app could eventually
# be migrated to ArcGIS dashboard or a dedicated R-Shiny server could be set up for it.
#===============================================================================
# Load the required packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(magrittr, dplyr, readr, stringr, tidyr, lubridate,
               shiny, shinyjs, shinycssloaders, plotly,bslib,
               leaflet, sf, nngeo, tmap, viridis,
               googlesheets4, jsonlite, openssl, janitor, classInt)

# Load coffee kpi data
#=========================================
# Authenticate google sheets with a service account
#---------------------------------------------------
# In Posit Connect Cloud, store the base64-encoded service account JSON in the
# "GSHEET_SERVICE_JSON_BASE64" variable. For local development, fall back to the
# JSON file in the project directory when the variable is not set.
json_path <- "shiny-gsheets-service-account-file.json"
b64 <- Sys.getenv("GSHEET_SERVICE_JSON_BASE64")

if (nzchar(b64)) {
  decoded_raw <- base64_decode(b64)
  tmp <- tempfile(fileext = ".json")
  writeBin(decoded_raw, tmp)
  on.exit(unlink(tmp), add = TRUE)
  gs4_auth(path = tmp)
} else if (file.exists(json_path)) {
  gs4_auth(path = json_path)
} else {
  stop(
    paste(
      "Google Sheets authentication is not configured.",
      "Set GSHEET_SERVICE_JSON_BASE64 in Posit Connect Cloud or provide",
      shQuote(json_path),
      "for local development."
    )
  )
}

# Load the input datasets from Google Sheets
url <- "1S2tvQ2S2GBQffGXAxLTExDu0i24jHxj7NwG-gWPahD4"
data_farms <- range_read(url, sheet = "Coffee_farms", range = "A1:AE")
data_cws <- range_read(url, sheet = "Coffee Washing Stations", range = "A1:Z")
data_coops <- range_read(url, sheet = "Cooperatives", range = "A1:R")
# Given farmers dataset is too big to read in one go, use the national_id column
# to determine the number of data rows, then read the sheet in row chunks so that
# blank cells in other columns are preserved as NA within each chunk.
national_id_col <- range_read(url, sheet = "Coffee farmers", range = "A:A")
n_data_rows <- nrow(national_id_col)
chunk_size <- 5000
farmer_col_names <- c(
  "national_id", "farmer_col_b", "district", "gender", "age", "farmer_col_f",
  "farmer_col_g", "young_in_hh", "farmer_col_i", "farmer_col_j", "farmer_col_k", "farmer_col_l",
  "farmer_col_m", "farmer_col_n", "farmer_col_o", "farmer_col_p", "farmer_col_q", "farmer_col_r",
  "training_topics", "farmer_col_t", "farmer_col_u", "farmer_col_v", "farmer_cws", "cooperative",
  "farmer_col_y", "farmer_col_z"
)
farmer_col_types <- paste(rep("c", length(farmer_col_names)), collapse = "")

# Read all chunks with explicit names and character types. This avoids name
# repair warnings from blank or duplicated sheet headers and keeps empty cells as NA.
first_chunk_end <- min(chunk_size + 1, n_data_rows + 1)
first_chunk <- range_read(
  url,
  sheet = "Coffee farmers",
  range = paste0("A1:Z", first_chunk_end),
  col_names = FALSE,
  col_types = farmer_col_types,
  .name_repair = "minimal"
)
names(first_chunk) <- farmer_col_names
first_chunk <- first_chunk %>% slice(-1)

starts <- seq(first_chunk_end + 1, n_data_rows + 1, by = chunk_size)
ends <- pmin(starts + chunk_size - 1, n_data_rows + 1)
row_ranges <- paste0("A", starts, ":Z", ends)

chunk_list <- lapply(row_ranges, function(r) {
  chunk <- range_read(
    url,
    sheet = "Coffee farmers",
    range = r,
    col_names = FALSE,
    col_types = farmer_col_types,
    .name_repair = "minimal"
  )
  names(chunk) <- farmer_col_names
  chunk
})

data_farmers <- bind_rows(c(list(first_chunk), chunk_list))

# Some Google Sheets reads can produce list columns when cell types vary.
# Normalize join keys to plain character vectors before downstream joins.
normalize_sheet_id <- function(x) {
  if (is.list(x)) {
    vapply(
      x,
      function(value) {
        if (length(value) == 0 || all(is.na(value))) NA_character_ else as.character(value[[1]])
      },
      character(1)
    )
  } else {
    as.character(x)
  }
}

data_farmers$national_id <- normalize_sheet_id(data_farmers$national_id)
data_farms$national_id <- normalize_sheet_id(data_farms$national_id)

# convert coops and CWS data to sf
data_coops %<>% st_as_sf(coords = c("longitude", "latitude"), sf_column_name = "geom", crs = 4326, remove = T, na.fail = F) %>%
  filter(!st_is_empty(geom)) %>% st_transform(crs = 32736)
data_cws %<>% st_as_sf(coords = c("longitude", "latitude"), sf_column_name = "geom", crs = 4326, remove = T, na.fail = F) %>%
  filter(!st_is_empty(geom)) %>% st_transform(crs = 32736)
data_farms %<>% st_as_sf(coords = c("longitude", "latitude"), sf_column_name = "geom", crs = 4326, remove = T, na.fail = F) %>%
  filter(!st_is_empty(geom)) %>% st_transform(crs = 32736)


# since one farmer (national id) can have multiple farms, we need to aggregate the data
# to get the total area and number of coffee trees per farmer (per age of trees)
data_farms_stats <- data_farms %>% st_drop_geometry() %>% group_by(national_id, age_range_coffee_trees) %>%
  summarise(area = sum(area_ares, na.rm = T),
            nbr_coffee_trees = sum(as.integer(nbr_coffee_trees), na.rm = T),
            .groups = "drop")

# join farmers IDs to their corresponding farms
data_farmers %<>% mutate(cws_id = str_replace_all(str_squish(str_to_lower(farmer_cws)), " ", "_"))
data_farmers %<>% mutate(cooperative_id = str_replace_all(str_squish(str_to_lower(cooperative)), " ", "_"))
data_farmers_full <- data_farmers %>% select(national_id, district, training_topics, cooperative_id, cws_id) %>%
  left_join(data_farms_stats, by = "national_id")

# load geospatial data
country <- st_read(paste(getwd(),"data_wgs84", "RW_country.gpkg", sep = "/"), layer = "country")
lakes <- st_read(paste(getwd(),"data_wgs84", "RW_lakes.gpkg", sep = "/"), layer = "lakes")
np <- st_read(paste(getwd(),"data_wgs84", "RW_national_parks.gpkg", sep = "/"), layer = "np")
districts <- st_read(paste(getwd(),"data_wgs84", "RW_districts.gpkg", sep = "/"), layer = "districts")

# clean and prepare the geospatial data
country %<>% st_zm(drop = T, what = "ZM") %>%  st_make_valid(.) %>% st_transform(crs = 32736)
lakes %<>% st_zm(drop = T, what = "ZM") %>%  st_make_valid(.) %>% st_transform(crs = 32736)
np %<>% st_zm(drop = T, what = "ZM") %>%  st_make_valid(.) %>% st_transform(crs = 32736)
districts %<>% st_zm(drop = T, what = "ZM") %>%  st_make_valid(.) %>% st_transform(crs = 32736)

# harmonize district names with the names in the farmers dataset
districts %<>% mutate(district = str_to_lower(district))

# BUILD THE DASHBOARD USING SHINY WEB FRAMEWORK
#=================================================
useShinyjs() # enable the rendering of the UI only when "Go" button is clicked

# UI definition ----
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  # CSS definition for various page components
  tags$head(
    tags$style(HTML("
      body{
        padding: 0;
      }
      .title-panel{
        background-color: #52876e;
        color: white;
        padding: 10px 5px;
        margin-bottom: 20px;
        width: 100vw;
        position: relative;
        left: 50%;
        right: 50%;
        margin-left: -50vw;
        margin-right: -50vw;
      }
      .highlighted-district {
        fill-opacity: 0.7 !important;
        fill: #FF9933 !important;
        stroke-width: 3;
        stroke: #FF6600;
      }
      .card-body {
        height: 100%;
        width: 100%;
      }
      /* Styles for text output boxes (both large and small screens) */
      .text-output-box .value {
        font-size: 1em; /* Default smaller font size for the main number */
        font-weight: bold;
        margin-bottom: 5px;
      }
      .text-output-box .label {
        font-size: 0.8em; /* Default smaller font size for the label */
        color: #555;
      }
      .text-output-content {
        display: flex;
        flex-direction: column; /* Stack value and label vertically */
        justify-content: center;
        align-items: center;
        height: 100%;
        text-align: center;
      }
      /* Adjust for large screens if needed for overall text, not for value/label specifically */
      @media (min-width: 768px) {
        /* You can add more specific large screen adjustments here if necessary,
           but the .text-output-box .value/.label styles will apply by default
           unless overridden for h1/p elements */
      }
    "))
  ),
  useShinyjs(),  # Enable shinyjs
  
  # Title panel
  fluidRow(
    column(12,
           div(class = "title-panel",
               h3("Coffee Extension Activities Dashboard")
           )
    )
  ),
  
  # Main content area with three columns
  fluidRow(
    # National statistics sidebar
    column(3,
           fluidRow(
             card(
               style = "height: 20vh;",
               full_screen = TRUE,
               # Always include icon in header for all screens
               card_header(span(img(src = "farmer.png", height = "25px", style = "vertical-align: middle;"), "Total # Farmers")),
               card_body(
                 uiOutput("nbr_farmers_content") %>% # Content rendered directly
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE, size = .5) %>% as_fill_carrier()
               )
             )
           ),
           fluidRow(
             card(
               style = "height: 20vh;",
               full_screen = TRUE,
               # Always include icon in header for all screens
               card_header(span(img(src = "female_farmer.png", height = "25px", style = "vertical-align: middle;"), "% Women Farmers")),
               card_body(
                 uiOutput("nbr_farmers_women_content") %>% # Content rendered directly
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE, size = .5) %>% as_fill_carrier()
               )
             )
           ),
           fluidRow(
             card(
               style = "height: 20vh;",
               full_screen = TRUE,
               # Always include icon in header for all screens
               card_header(span(img(src = "young_farmer.png", height = "25px", style = "vertical-align: middle;"), "% Youth Farmers")),
               card_body(
                 uiOutput("nbr_farmers_young_content") %>% # Content rendered directly
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE, size = .5) %>% as_fill_carrier()
               )
             )
           ),
           fluidRow(
             card(
               style = "height: 20vh;",
               full_screen = TRUE,
               # Always include icon in header for all screens
               card_header(span(img(src = "family_farmers.png", height = "25px", style = "vertical-align: middle;"), "Youth in HH")),
               card_body(
                 uiOutput("youth_in_hh_content") %>% # Content rendered directly
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE, size = .5) %>% as_fill_carrier()
               )
             )
           )
    ),
    # Maps visualization area
    column(6,
           # Maps tabs
           tabsetPanel(id = "mapTabs",
                       tabPanel("Cooperatives/CWS View",
                                card(
                                  style = "height: 80vh;",
                                  full_screen = TRUE,
                                  #card_header("Map 1"),
                                  card_body(
                                    leafletOutput("map_cws") %>%
                                      withSpinner(type = 6, color = "#30804e") %>% as_fill_carrier()
                                  )
                                )
                       ),
                       # Data table tab
                       tabPanel("Coffee Farms View",
                                card(
                                  style = "height: 80vh;",
                                  full_screen = TRUE,
                                  #card_header("Map 2"),
                                  card_body(
                                    leafletOutput("map_farms") %>%
                                      withSpinner(type = 5, color = "#30804e") %>% as_fill_carrier()
                                  )
                                )
                       )
           )
    ),
    # Disaggregated statistics area
    column(3,
           # Farm area panel
           fluidRow(
             card(
               style = "height: 16vh;",
               full_screen = TRUE,
               # Always include icon in header for all screens
               card_header(span(img(src = "farm_area.png", height = "25px", style = "vertical-align: middle;"), "Total Area in Ha")),
               card_body(
                 uiOutput("farm_area_content") %>% # Content rendered directly
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE, size = .5) %>% as_fill_carrier()
               )
             )
           ),
           # Coffee trees panel
           fluidRow(
             card(
               style = "height: 32vh;",
               full_screen = TRUE,
               # Add coffee tree icon to header and ensure text is on the same line
               card_header(span(img(src = "coffee_tree.png", height = "25px", style = "vertical-align: middle; margin-right: 5px;"), htmlOutput("coffee_trees_chart_title"))),
               card_body(
                 uiOutput("coffee_trees_chart") %>%
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE) %>% as_fill_carrier()
               )
             )
           ),
           # Training topics panel
           fluidRow(
             card(
               style = "height: 33vh;",
               full_screen = TRUE,
               # Add farmer training icon to header and ensure text is on the same line
               card_header(span(img(src = "farmer_training.png", height = "25px", style = "vertical-align: middle; margin-right: 5px;"), htmlOutput("touch_points_chart_title"))),
               card_body(
                 uiOutput("touch_points_chart") %>%
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE) %>% as_fill_carrier()
               )
             )
           )
    )
  )
)

# Server processing
server <- function(input, output, session) {
  # initialize some reactive variables
  rv <- reactiveValues(
    current_tab = "Cooperatives/CWS View",
    clicked_point = NULL,
    clicked_district = NULL
  )
  
  # Observe tab changes and update the related variables accordingly
  observe({
    # Reset all selections when tab changes
    if(rv$current_tab != input$mapTabs) {
      rv$current_tab <- input$mapTabs
      rv$clicked_point <- NULL
      rv$clicked_district <- NULL
      
      # Clear map highlights based on which tab we're switching to
      if(input$mapTabs == "Cooperatives/CWS View") {
        leafletProxy("map_cws") %>% clearGroup("clicked_points")
      } else {
        leafletProxy("map_farms") %>% clearGroup("highlighted_district")
      }
    }
  }) %>%
    bindEvent(input$mapTabs)
  
  # Calculate  statistics at the national level
  #============================================
  nbr_farmers_country <- reactive({
    data_farmers %>%
      summarize(nbr_farmers = n()) %>% pull(nbr_farmers)
  })
  
  nbr_farmers_women_country <- reactive({
    data_farmers %>% filter(gender == "female") %>%
      summarize(nbr_farmers_women = n()) %>% pull(nbr_farmers_women)
  })
  
  nbr_farmers_young_country <- reactive({
    data_farmers %>% mutate(age = as.integer(age)) %>% filter(age < 30) %>%
      summarize(nbr_farmers_young = n()) %>% pull(nbr_farmers_young)
  })
  
  nbr_youth_hh_country <- reactive({
    data_farmers %>%
      summarize(young_in_hh = sum(as.integer(young_in_hh), na.rm = T)) %>% pull(young_in_hh)
  })
  
  # Define common style for centering content in text output boxes
  center_style <- "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; text-align: center;"
  
  # Simplified UI for National Statistics cards (Content only)
  output$nbr_farmers_content <- renderUI({
    div(class = "text-output-box",
        div(class = "text-output-content",
            div(class = "value", format(round(nbr_farmers_country()), big.mark = ",")),
            div(class = "label", "Total # of farmers per country")
        )
    )
  })
  
  output$nbr_farmers_women_content <- renderUI({
    div(class = "text-output-box",
        div(class = "text-output-content",
            div(class = "value", paste0(format(round((nbr_farmers_women_country() * 100)/nbr_farmers_country()), big.mark = ","), "%")),
            div(class = "label", "Female Farmers at the National Level")
        )
    )
  })
  
  output$nbr_farmers_young_content <- renderUI({
    div(class = "text-output-box",
        div(class = "text-output-content",
            div(class = "value", paste0(format(round((nbr_farmers_young_country() * 100)/nbr_farmers_country()), big.mark = ","), "%")),
            div(class = "label", "Young Farmers at the National Level")
        )
    )
  })
  
  output$youth_in_hh_content <- renderUI({
    div(class = "text-output-box",
        div(class = "text-output-content",
            div(class = "value", format(round(nbr_youth_hh_country()), big.mark = ",")),
            div(class = "label", "Total # of Youth involved in coffee farming")
        )
    )
  })
  
  # render the cws/coops map
  output$map_cws <- renderLeaflet({
    # Define symbol sizes to use in both map and legend
    symbol_sizes <- c(10, 16, 22)
    
    # Calculate Jenks natural breaks for three classes
    cws_breaks <- classInt::classIntervals(data_cws$actual_capacity, n = 3, style = "jenks")$brks
    coop_breaks <- classInt::classIntervals(data_coops$nbr_cooperative_members, n = 3, style = "jenks")$brks
    
    # Create category column based on Jenks breaks. include.lowest=T ensures the lowest values are included.
    data_cws$category <- cut(data_cws$actual_capacity, breaks = cws_breaks,
                             labels = FALSE, include.lowest = TRUE)
    data_coops$category <- cut(data_coops$nbr_cooperative_members,
                               breaks = coop_breaks,
                               labels = FALSE, include.lowest = TRUE)
    
    # add a size_px column to hold the symbol size values in pixels
    data_coops$coop_size_px <- symbol_sizes[data_coops$category]
    data_cws$cws_size_px <- symbol_sizes[data_cws$category]
    
    # define pop-up variables
    data_coops$coop_size_var <- paste(format(data_coops$nbr_cooperative_members, 
                                             big.mark = ",", scientific = FALSE),"farmers", sep = " ")
    data_cws %<>% mutate(cws_size_var = round(actual_capacity/1000, 1)) %>%
      mutate(cws_size_var = paste(format(cws_size_var, big.mark = ",", scientific = FALSE), "tonnes", sep = " "))
    
    # Build the tmap object
    tmap_object <- tmap_mode("view") +
      tm_basemap("Esri.WorldTopoMap") +
      tm_shape(districts) +
      tm_borders(col = "#A76948", fill_alpha = .8) +
      tm_shape(lakes) +
      tm_polygons(fill = "#2CA2E6", fill_alpha = .2,
                  popup.vars = c("Lake" = "name"), id = "name") +
      tm_shape(np) +
      tm_polygons(fill = "#158849", fill_alpha = .2,
                  popup.vars = c("National Park" = "name"), id = "name") +
      tm_shape(country) +
      tm_borders(col = "#A76948", lwd = 2, fill_alpha = .6) +
      
      tm_shape(data_coops) +
      tm_dots(fill = "#063b57",
              size       = "coop_size_px",
              size.scale = tm_scale_continuous(values.scale = 1),
              size.legend = tm_legend_hide(),
              popup.vars = c("Name" = "cooperative_name",
                             "Members" = "coop_size_var"),
              group = "Cooperatives") +
      
      tm_shape(data_cws) +
      tm_dots(fill = "#adcb17",
              size       = "cws_size_px",
              size.scale = tm_scale_continuous(values.scale = 1),
              size.legend = tm_legend_hide(),
              popup.vars = c("Name" = "cws_name",
                             "Capacity" = "cws_size_var"),
              group = "CWS") +
      
      tm_view(bbox = st_bbox(country)) +
      tm_layout(frame = FALSE) +
      tm_layout(legend.show = FALSE)
    
    # Convert to leaflet
    leaflet_map <- tmap_leaflet(tmap_object)
    
    # Create legend labels from the Jenks break values
    coop_labels <- c(
      paste(round(coop_breaks[1]), "-", round(coop_breaks[2]), "members"),
      paste(round(coop_breaks[2]) + 1, "-", round(coop_breaks[3]), "members"),
      paste(round(coop_breaks[3]) + 1, "-", round(coop_breaks[4]), "members")
    )
    cws_labels <- c(
      paste(round(cws_breaks[1]/1000), "-", round(cws_breaks[2]/1000), "Tonnes"),
      paste(round(cws_breaks[2]/1000), "-", round(cws_breaks[3]/1000), "Tonnes"),
      paste(round(cws_breaks[3]/1000), "-", round(cws_breaks[4]/1000), "Tonnes")
    )
    
    # Use the same symbol sizes as defined in the scale
    coop_legend <- addLegendCustom(
      map = NULL, position = NULL,
      size_values = symbol_sizes,
      labels = coop_labels,
      color = "#063b57",
      title = "Cooperatives"
    )
    cws_legend <- addLegendCustom(
      map = NULL, position = NULL,
      size_values = symbol_sizes,
      labels = cws_labels,
      color = "#adcb17",
      title = "CWS Capacity"
    )
    combined_legend_html <- paste0(
      "<div style='display: flex; justify-content: center; align-items: flex-start;
              background: rgba(255, 255, 255, 0.2); padding: 5px; border-radius: 5px;'>",
      coop_legend, cws_legend, "</div>"
    )
    
    # add a layer control widget but remove the base layers from the list of toggleble layers
    leaflet_map %>%
      removeLayersControl() %>%  # Remove the existing layer control first
      addLayersControl(overlayGroups = c("Cooperatives", "CWS"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addControl(html = combined_legend_html, position = "bottomright")
  })
  
  # Helper function to generate the HTML for the custom legend
  #-------------------------------------------------------------------------------
  addLegendCustom <- function(map, position, size_values, labels, color, title) {
    # Use size_values as radius by doubling for width/height (diameter) ***
    symbol_diameters <- 4/7 * size_values # (the 4/7 value was determined empirically)
    
    legend_items <- paste0(
      "<div style='display: flex; align-items: center; margin-bottom: 2px;'>",
      "<div style='width:", symbol_diameters, "px; height:", symbol_diameters, "px; background-color:", color,
      "; border-radius:50%; margin-right:5px; border: 1px solid #333;'></div>",
      "<span style='font-size: 12px; line-height: 1.2;'>", labels, "</span>",
      "</div>",
      collapse = ""
    )
    
    html_legend <- paste0(
      "<div style='padding: 2px; margin: 3px; text-align: left;'>",
      "<h4 style='margin-top:0; margin-bottom: 5px; font-weight: bold; text-align: center; font-size: 14px;'>", title, "</h4>",
      legend_items,
      "</div>"
    )
    
    # if a map object is passed, add the legend to the map
    if(!is.null(map))
      map %>% addControl(html = html_legend, position = position)
    # else return the legend html code
    else
      return(html_legend)
  }
  
  # Get the clicked cooperative or CWS
  clicked_cws_coop <- reactive({
    req(rv$current_tab == "Cooperatives/CWS View")
    req(input$map_cws_click)
    
    click <- input$map_cws_click
    
    # Create point from click
    pt <- st_point(c(click$lng, click$lat)) %>%
      st_sfc(crs = 4326) %>%
      st_transform(crs = st_crs(districts))
    
    # find nearest neighbor in each layer using st_nn
    nn_coops <- st_nn(pt, data_coops, k = 1, returnDist = TRUE)
    nn_cws <- st_nn(pt, data_cws, k = 1, returnDist = TRUE)
    
    # compare distances and select the nearest point
    if (nn_coops[[2]][[1]] < nn_cws[[2]][[1]]) {
      nearest_idx <- nn_coops[[1]][[1]]
      return(list(
        dataset = "data_coops",
        row = data_coops[nearest_idx, ]
      ))
    } else {
      nearest_idx <- nn_cws[[1]][[1]]
      return(list(
        dataset = "data_cws",
        row = data_cws[nearest_idx, ]
      ))
    }
  })
  
  # render the farms map
  output$map_farms <- renderLeaflet({
    tmap_object <- tm_shape(districts) +
      tm_borders(col = "#A76948", fill_alpha = .6) +
      tm_fill(col = "#A76948",
              fill_alpha = .2,
              id = "district") +
      
      tm_shape(lakes) +
      tm_polygons(col = "#2CA2E6",
                  fill_alpha = .6,
                  popup.vars = c("Lake" = "name")) +
      
      tm_shape(np) +
      tm_polygons(col = "#085e27",
                  fill_alpha = .6,
                  popup.vars = c("National Park" = "name")) +
      
      tm_shape(country) +
      tm_borders(lwd = 2) +
      
      tm_shape(data_farms) +
      tm_dots(col = "#011e0b",
              fill_alpha = .6,
              size = 0.1) +
      
      tm_view(bbox = st_bbox(country)) +
      tm_basemap("Esri.WorldTopoMap")
    
    leaflet_map <- tmap_leaflet(tmap_object)
  })
  
  # Get the clicked district name
  clicked_district <- reactive({
    req(rv$current_tab == "Coffee Farms View")
    req(input$map_farms_click)
    
    click <- input$map_farms_click
    
    # Create point from click
    click_point <- st_point(c(click$lng, click$lat)) %>%
      st_sfc(crs = 4326) %>%
      st_transform(crs = st_crs(districts))
    
    # Find which district was clicked
    districts %>%
      st_filter(click_point, .predicate = st_intersects) %>%
      pull(district)
  })
  
  # Update the reactive expressions for filtering
  farm_area <- reactive({
    if (rv$current_tab == "Cooperatives/CWS View" && !is.null(rv$clicked_point)) {
      if(rv$clicked_point$dataset == "data_coops") {
        data_farmers_full %>%
          filter(cooperative_id == rv$clicked_point$row$cooperative_id) %>%
          summarize(farm_area_ha = sum(area, na.rm = T)/100) %>%
          pull(farm_area_ha)
      } else {
        data_farmers_full %>%
          filter(cws_id == rv$clicked_point$row$cws_id) %>%
          summarize(farm_area_ha = sum(area, na.rm = T)/100) %>%
          pull(farm_area_ha)
      }
    } else if (rv$current_tab == "Coffee Farms View" && !is.null(rv$clicked_district)) {
      data_farmers_full %>%
        filter(district == rv$clicked_district) %>%
        summarize(farm_area_ha = sum(area, na.rm = T)/100) %>%
        pull(farm_area_ha)
    } else {
      # Default view - show all data
      data_farmers_full %>%
        summarize(farm_area_ha = sum(area, na.rm = T)/100) %>%
        pull(farm_area_ha)
    }
  })
  
  # Simplified UI for Farm Area content
  output$farm_area_content <- renderUI({
    # clean the result of farm_area variable
    area <- farm_area() %>% (function(x) {
      if (length(x) == 0 || is.na(x)) 0 else x
    })()
    
    # Display icon in content area only when there is no data.
    if (area <= 0) {
      div(class = "text-output-box",
          div(class = "text-output-content",
              #img(src = "farm_area.png", height = "35px", style = "margin-bottom: 5px;"), # Icon when no data
              div(class = "label", paste("No farm area data available", subtitle_text()))
          )
      )
    } else {
      div(class = "text-output-box",
          div(class = "text-output-content",
              div(class = "value", format(round(area, 1), big.mark = ",")),
              div(class = "label", paste("Farm area in Ha", subtitle_text()))
          )
      )
    }
  })
  
  coffee_trees <- reactive({
    if (rv$current_tab == "Cooperatives/CWS View" && !is.null(rv$clicked_point)) {
      if(rv$clicked_point$dataset == "data_coops") {
        data_farmers_full %>%
          filter(cooperative_id == rv$clicked_point$row$cooperative_id) %>%
          group_by(age_range_coffee_trees) %>%
          summarise(nbr_coffee_trees = sum(nbr_coffee_trees, na.rm = T)) %>%
          arrange(desc(nbr_coffee_trees))
      } else {
        data_farmers_full %>%
          filter(cws_id == rv$clicked_point$row$cws_id) %>%
          group_by(age_range_coffee_trees) %>%
          summarise(nbr_coffee_trees = sum(nbr_coffee_trees, na.rm = T)) %>%
          arrange(desc(nbr_coffee_trees))
      }
    } else if (rv$current_tab == "Coffee Farms View" && !is.null(rv$clicked_district)) {
      data_farmers_full %>%
        filter(district == rv$clicked_district) %>%
        group_by(age_range_coffee_trees) %>%
        summarise(nbr_coffee_trees = sum(nbr_coffee_trees, na.rm = T)) %>%
        arrange(desc(nbr_coffee_trees))
    } else {
      # Default view - show all data
      data_farmers_full %>%
        group_by(age_range_coffee_trees) %>%
        summarise(nbr_coffee_trees = sum(nbr_coffee_trees, na.rm = T)) %>%
        arrange(desc(nbr_coffee_trees))
    }
  })
  
  touch_points <- reactive({
    if (rv$current_tab == "Cooperatives/CWS View" && !is.null(rv$clicked_point)) {
      if(rv$clicked_point$dataset == "data_coops") {
        data_farmers_full %>%
          filter(cooperative_id == rv$clicked_point$row$cooperative_id) %>%
          separate_rows(training_topics, sep = " ") %>%
          count(training_topics, name = "frequency", sort = TRUE)
      } else {
        data_farmers_full %>%
          filter(cws_id == rv$clicked_point$row$cws_id) %>%
          separate_rows(training_topics, sep = " ") %>%
          count(training_topics, name = "frequency", sort = TRUE)
      }
    } else if (rv$current_tab == "Coffee Farms View" && !is.null(rv$clicked_district)) {
      data_farmers_full %>%
        filter(district == rv$clicked_district) %>%
        separate_rows(training_topics, sep = " ") %>%
        count(training_topics, name = "frequency", sort = TRUE)
    } else {
      # Default view - show all data
      data_farmers_full %>%
        separate_rows(training_topics, sep = " ") %>%
        count(training_topics, name = "frequency", sort = TRUE)
    }
  })
  
  # Observe clicks observers
  #-----------------------------
  # 1. Coops/CWS map 
  observe({
    req(rv$current_tab == "Cooperatives/CWS View")
    click <- input$map_cws_click
    
    # Check which layers are currently active
    active_layers <- input$map_cws_groups
    coops_active <- "Cooperatives" %in% active_layers
    cws_active <- "CWS" %in% active_layers
    
    # If no layers are active, don't process the click
    if (!coops_active && !cws_active) {
      return()
    }
    
    # Create point from click
    pt <- st_point(c(click$lng, click$lat)) %>%
      st_sfc(crs = 4326) %>%
      st_transform(crs = st_crs(districts))
    
    # Calculate distances only for active layers
    distances <- list()
    nearest_points <- list()
    
    if (coops_active) {
      nn_coops <- st_nn(pt, data_coops, k = 1, returnDist = TRUE)
      distances$coops <- nn_coops[[2]][[1]]
      nearest_points$coops <- list(
        dataset = "data_coops",
        row = data_coops[nn_coops[[1]][[1]], ]
      )
    }
    
    if (cws_active) {
      nn_cws <- st_nn(pt, data_cws, k = 1, returnDist = TRUE)
      distances$cws <- nn_cws[[2]][[1]]
      nearest_points$cws <- list(
        dataset = "data_cws",
        row = data_cws[nn_cws[[1]][[1]], ]
      )
    }
    
    # Select the nearest point from active layers only
    if (length(distances) == 1) {
      # Only one layer is active
      rv$clicked_point <- nearest_points[[1]]
    } else if (length(distances) == 2) {
      # Both layers are active, choose the nearest
      if (distances$coops < distances$cws) {
        rv$clicked_point <- nearest_points$coops
      } else {
        rv$clicked_point <- nearest_points$cws
      }
    }
  }) %>%
    bindEvent(input$map_cws_click)
  
  # 2. Farms map
  observe({
    req(rv$current_tab == "Coffee Farms View")
    click <- input$map_farms_click
    
    # Create point from click
    click_point <- st_point(c(click$lng, click$lat)) %>%
      st_sfc(crs = 4326) %>%
      st_transform(crs = st_crs(districts))
    
    # Find which district was clicked
    clicked <- districts %>%
      st_filter(click_point, .predicate = st_intersects) %>%
      pull(district)
    
    if(length(clicked) > 0) {
      rv$clicked_district <- clicked
    }
  }) %>%
    bindEvent(input$map_farms_click)
  
  # Map tab observers for highlighting
  observe({
    req(rv$current_tab == "Cooperatives/CWS View")
    if(!is.null(rv$clicked_point)) {
      leafletProxy("map_cws") %>%
        clearGroup("clicked_points") %>%
        addMarkers(
          data = if(rv$clicked_point$dataset == "data_coops")
            rv$clicked_point$row %>% st_transform(crs = 4326) else rv$clicked_point$row %>% st_transform(crs = 4326),
          group = "clicked_points"
        )
    }
  })
  
  observe({
    req(rv$current_tab == "Coffee Farms View")
    if(!is.null(rv$clicked_district)) {
      leafletProxy("map_farms") %>%
        clearGroup("highlighted_district") %>%
        addPolygons(
          data = districts %>%
            filter(district == rv$clicked_district) %>%
            st_transform(crs = 4326),
          fillColor = "#FF9933",
          fillOpacity = 0.7,
          weight = 3,
          color = "#FF6600",
          group = "highlighted_district"
        )
    }
  })
  
  # Create a reactive expression for the subtitle based on selection
  subtitle_text <- reactive({
    if (rv$current_tab == "Cooperatives/CWS View" && !is.null(rv$clicked_point)) {
      name <- if(rv$clicked_point$dataset == "data_coops") {
        str_to_title(rv$clicked_point$row$cooperative_name)
      } else {
        str_to_title(rv$clicked_point$row$cws_name)
      }
      paste("for", name)
    } else if (rv$current_tab == "Coffee Farms View" && !is.null(rv$clicked_district)) {
      paste("for", str_to_title(rv$clicked_district), "District")
    } else {
      "at the National Level"
    }
  })
  
  # Helper function to set the correctly sort the tree age categories
  tree_age_order <- function(x) {
    factor(x, levels = c("less_3", "3_to_7", "8_to_15", "16_to_30", "more_30"))
  }
  
  total_trees <- reactive({
    sum(coffee_trees()$nbr_coffee_trees, na.rm = TRUE)
  })
  
  # Chart title for Coffee Trees chart
  output$coffee_trees_chart_title <- renderUI({
    total_formatted <- format(total_trees(), big.mark = ",")
    chart_subtitle_text <- paste(total_formatted, "trees", subtitle_text())
    span(style="font-size: 0.9em;", paste("Coffee trees:", chart_subtitle_text))
  })
  
  # Display plot or a styled message
  output$coffee_trees_chart <- renderUI({
    total <- total_trees()
    
    if (nrow(coffee_trees()) == 0 || total == 0) {
      # Display message if no data
      div(
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; text-align: center;",
        img(src = "coffee_trees.png", height = "50px", style="margin-bottom: 15px;"),
        p(style = "font-size: .8em;", paste("No coffee trees data available", subtitle_text()))
      )
    } else {
      # Display the plot if data is available
      plotlyOutput("coffee_trees_plot", height = "100%")
    }
  })
  
  # This renders the plot to be displayed by the UI above
  output$coffee_trees_plot <- renderPlotly({
    req(nrow(coffee_trees()) > 0, total_trees() > 0) 
    
    plot_data <- coffee_trees() %>%
      mutate(age_range_coffee_trees = tree_age_order(age_range_coffee_trees))
    
    plot_ly(plot_data,
            x = ~age_range_coffee_trees,
            y = ~nbr_coffee_trees,
            type = "bar",
            marker = list(color = "#3ea363")
    ) %>%
      layout(
        xaxis = list(title = "", # Remove x-axis title
                     categoryorder = "array",
                     categoryarray = levels(tree_age_order(""))),
        yaxis = list(title = "", # Remove y-axis title
                     tickmode = "array",
                     tickvals = c(min(plot_data$nbr_coffee_trees), max(plot_data$nbr_coffee_trees)),
                     ticktext = c(
                       paste0(round(min(plot_data$nbr_coffee_trees)/1000, 1), "k"),
                       paste0(round(max(plot_data$nbr_coffee_trees)/1000, 1), "k")
                     )),
        margin = list(t = 20, l = 20) # Adjusted top margin
      )
  })
  
  # Chart title for Touch Points 
  output$touch_points_chart_title <- renderUI({
    chart_subtitle_text <- subtitle_text()
    span(style="font-size: 0.9em;", paste("Touch points", chart_subtitle_text))
  })
  
  # Display plot or a data missing message
  output$touch_points_chart <- renderUI({
    if (nrow(touch_points()) == 0) {
      div(
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; text-align: center;",
        img(src = "extension.png", height = "50px", style="margin-bottom: 15px;"),
        p(style = "font-size: .8em;", paste("No training data available", subtitle_text()))
      )
    } else {
      plotlyOutput("touch_points_plot", height = "100%")
    }
  })
  
  # This renders the plot to be displayed by the UI above
  output$touch_points_plot <- renderPlotly({
    req(nrow(touch_points()) > 0) 
    
    sorted_data <- touch_points() %>%
      arrange(desc(frequency)) %>%
      mutate(color_index = row_number())
    
    plot_ly(sorted_data,
            x = ~reorder(training_topics, -frequency),
            y = ~frequency,
            type = "bar",
            marker = list(
              color = ~color_index,
              colorscale = "Viridis",
              showscale = FALSE
            )) %>%
      layout(xaxis = list(title = "", # Remove x-axis title
                          categoryorder = "array",
                          categoryarray = ~reorder(training_topics, -frequency)),
             yaxis = list(title = "", # Remove y-axis title
                          tickmode = "array",
                          tickvals = c(min(sorted_data$frequency), max(sorted_data$frequency)),
                          ticktext = c(
                            paste0(round(min(sorted_data$frequency)/1000, 1), "k"),
                            paste0(round(max(sorted_data$frequency)/1000, 1), "k")
                          )),
             margin = list(t = 20, l = 20) # Adjusted top margin
      )
  })
}

# Run the app
shinyApp(ui, server)
