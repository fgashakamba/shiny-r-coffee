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
# The JSON service account file has been encoded to base64 and stored in the
# environment variable called "GSHEET_SERVICE_JSON_BASE64" like this:
#cat(openssl::base64_encode(readChar("shiny-gsheets-service-account-file.json", file.info(json_path)$size)))
b64 <- Sys.getenv("GSHEET_SERVICE_JSON_BASE64") # Read the encoded service account json file
decoded_raw <- base64_decode(b64) # Decode the JSON string
tmp <- tempfile(fileext = ".json") # Create a temporary file
writeBin(decoded_raw, tmp) # Write decoded text to the created temporary file as binary
gs4_auth(path = tmp)# Authenticate with the service account

# Load the input datasets from Google Sheets
url <- "https://docs.google.com/spreadsheets/d/1S2tvQ2S2GBQffGXAxLTExDu0i24jHxj7NwG-gWPahD4"
data_farms <- range_read(url, sheet = "Coffee_farms", range = "A1:AE")
data_cws <- range_read(url, sheet = "Coffee Washing Stations", range = "A1:Y")
data_coops <- range_read(url, sheet = "Cooperatives", range = "A1:R")
# Given farmers dataset is too big, use named ranges to avoid time-out in production
ranges <- c("A:D", "H:I", "K:K", "S:T", "Y:Z")

# Read each range separately
range_list <- lapply(ranges, function(r) {
  range_read(url, sheet = "Coffee farmers", range = r)
})

# Combine by columns (assuming same number of rows)
data_farmers <- do.call(cbind, range_list)

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
        background-color: #473E2C;
        color: white;
        padding: 20px 15px;
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
    "))
  ),
  useShinyjs(),  # Enable shinyjs
  
  # Title panel
  fluidRow(
    column(12,
           div(class = "title-panel",
               h1("Coffee Extension Activities Dashboard"),
               h3("A dashboard to track key extension-related KPIs for Rwanda's coffee program")
           )
    )
  ),
  
  # Main content area with three columns
  fluidRow(
    # National statistics sidebar
    column(3,
           fluidRow(
             card(
               style = "height: 16vh;",
               full_screen = TRUE,
               card_header("# Farmers"),
               card_body(
                 htmlOutput("nbr_farmers") %>%
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE, size = .5) %>% as_fill_carrier()
               )
             )
           ),
           fluidRow(
             card(
               style = "height: 16vh;",
               full_screen = TRUE,
               card_header("% Women"),
               card_body(
                 htmlOutput("nbr_farmers_women") %>%
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE, size = .5) %>% as_fill_carrier()
               )
             )
           ),
           fluidRow(
             card(
               style = "height: 16vh;",
               full_screen = TRUE,
               card_header("% Youth"),
               card_body(
                 htmlOutput("nbr_farmers_young") %>%
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE, size = .5) %>% as_fill_carrier()
               )
             )
           ),
           fluidRow(
             card(
               style = "height: 16vh;",
               full_screen = TRUE,
               card_header("Young in HH"),
               card_body(
                 htmlOutput("youth_in_hh") %>%
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
                                    leafletOutput("map_coops") %>%
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
               card_header("Total area in Ha"),
               card_body(
                 uiOutput("farm_area") %>%
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
               card_header("# Coffee trees per age"),
               card_body(
                 # MODIFIED: Using uiOutput for dynamic content (plot or message)
                 uiOutput("coffee_trees_chart") %>%
                   withSpinner(type = 6, color = "#30804e",
                               hide.ui = FALSE) %>% as_fill_carrier()
               )
             )
           ),
           # Training topics panel
           fluidRow(
             card(
               style = "height: 32vh;",
               full_screen = TRUE,
               card_header("# Farmers per training touch points"),
               card_body(
                 # MODIFIED: Using uiOutput for dynamic content (plot or message)
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
        leafletProxy("map_coops") %>% clearGroup("clicked_points")
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
    data_farmers %>% filter(age < 30) %>%
      summarize(nbr_farmers_young = n()) %>% pull(nbr_farmers_young)
  })
  
  nbr_youth_hh_country <- reactive({
    data_farmers %>%
      summarize(young_in_hh = sum(as.integer(young_in_hh), na.rm = T)) %>% pull(young_in_hh)
  })
  
  # Display the national summary cards
  # =======================================
  output$nbr_farmers <- renderUI({
    tagList(
      h1(format(round(nbr_farmers_country()), big.mark = ",")),
      p("Total # per country")
    )
  })
  
  output$nbr_farmers_women <- renderUI({
    tagList(
      h1(format(round((nbr_farmers_women_country() * 100)/nbr_farmers_country()), big.mark = ",")),
      p("Total # per country")
    )
  })
  
  output$nbr_farmers_young <- renderUI({
    tagList(
      h1(format(round((nbr_farmers_young_country() * 100)/nbr_farmers_country()), big.mark = ",")),
      p("Total # per country")
    )
  })
  
  output$youth_in_hh <- renderUI({
    tagList(
      h1(format(round(nbr_youth_hh_country()), big.mark = ",")),
      p("Total # per country")
    )
  })
  
  # render the cws/coops map
  output$map_coops <- renderLeaflet({
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
                             "Members" = "nbr_cooperative_members"),
              group = "Cooperatives") +
      
      tm_shape(data_cws) +
      tm_dots(fill = "#adcb17",
              size       = "cws_size_px",
              size.scale = tm_scale_continuous(values.scale = 1),
              size.legend = tm_legend_hide(),
              popup.vars = c("Name" = "cws_name",
                             "Capacity" = "actual_capacity"),
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
    req(input$map_coops_click)
    
    click <- input$map_coops_click
    
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
  # 1. Coops/CWS map
  observe({
    req(rv$current_tab == "Cooperatives/CWS View")
    click <- input$map_coops_click
    
    # Create point from click
    pt <- st_point(c(click$lng, click$lat)) %>%
      st_sfc(crs = 4326) %>%
      st_transform(crs = st_crs(districts))
    
    # find nearest neighbor in each layer using st_nn
    nn_coops <- st_nn(pt, data_coops, k = 1, returnDist = TRUE)
    nn_cws <- st_nn(pt, data_cws, k = 1, returnDist = TRUE)
    
    if (nn_coops[[2]][[1]] < nn_cws[[2]][[1]]) {
      nearest_idx <- nn_coops[[1]][[1]]
      rv$clicked_point <- list(
        dataset = "data_coops",
        row = data_coops[nearest_idx, ]
      )
    } else {
      nearest_idx <- nn_cws[[1]][[1]]
      rv$clicked_point <- list(
        dataset = "data_cws",
        row = data_cws[nearest_idx, ]
      )
    }
  }) %>%
    bindEvent(input$map_coops_click)
  
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
      leafletProxy("map_coops") %>%
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
  
  # MODIFIED: Logic for farm_area widget updated
  output$farm_area <- renderUI({
    # clean the result of farm_area variable
    area <- farm_area() %>% (function(x) {
      if (length(x) == 0 || is.na(x)) 0 else x
    })()
    
    center_style <- "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; text-align: center;"
    
    # Check if area is empty, NA, or zero. Show icon only in this case.
    if (area <= 0) {
      div(
        style = center_style,
        # IMPORTANT: Replace 'area_icon.jpg' with your icon file in the www/ folder
        img(src = "area_icon.jpg", height = "35px", style = "margin-bottom: 5px;"),
        p(style = "font-size: 1.1em;", paste("No farm area data available", subtitle_text()))
      )
    } else {
      # When data is available, do NOT show the icon.
      div(
        style = center_style,
        h1(style="margin-bottom: 0;", format(round(area, 1), big.mark = ",")),
        p(style = "font-size: 1.1em;", paste("Farm area in Ha", subtitle_text()))
      )
    }
  })
  
  # Helper function to set the correctly sort the tree age categories
  tree_age_order <- function(x) {
    factor(x, levels = c("less_3", "3_to_7", "8_to_15", "16_to_30", "more_30"))
  }
  
  total_trees <- reactive({
    sum(coffee_trees()$nbr_coffee_trees, na.rm = TRUE)
  })
  
  # Use renderUI to conditionally display plot or a styled message
  output$coffee_trees_chart <- renderUI({
    total <- total_trees()
    
    if (nrow(coffee_trees()) == 0 || total == 0) {
      # Display message if no data
      div(
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; text-align: center;",
        # IMPORTANT: Replace 'trees_icon.jpg' with your icon file in the www/ folder
        img(src = "trees_icon.jpg", height = "50px", style="margin-bottom: 15px;"),
        p(style = "font-size: 1.2em;", paste("No coffee tree data available", subtitle_text()))
      )
    } else {
      # Display the plot if data is available
      plotlyOutput("coffee_trees_plot", height = "100%")
    }
  })
  
  # This now renders the plot to be displayed by the UI above
  output$coffee_trees_plot <- renderPlotly({
    req(nrow(coffee_trees()) > 0, total_trees() > 0) # Ensure data exists before rendering
    
    plot_data <- coffee_trees() %>%
      mutate(age_range_coffee_trees = tree_age_order(age_range_coffee_trees))
    
    total_formatted <- format(total_trees(), big.mark = ",")
    chart_title <- paste("Total:", total_formatted, "trees", subtitle_text())
    
    plot_ly(plot_data,
            x = ~age_range_coffee_trees,
            y = ~nbr_coffee_trees,
            type = "bar",
            marker = list(color = "#3ea363")
    ) %>%
      layout(title = list(text = chart_title, font = list(size = 14)),
             xaxis = list(categoryorder = "array", categoryarray = levels(tree_age_order(""))),
             yaxis = list(title = "# Coffee trees"),
             margin = list(t = 50) # Add top margin to avoid title collision
      )
  })
  
  # Use renderUI to conditionally display plot or a styled message
  output$touch_points_chart <- renderUI({
    if (nrow(touch_points()) == 0) {
      div(
        style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100%; text-align: center;",
        # IMPORTANT: Replace 'training_icon.jpg' with your icon file in the www/ folder
        img(src = "training_icon.jpg", height = "50px", style="margin-bottom: 15px;"),
        p(style = "font-size: 1.2em;", paste("No training data available", subtitle_text()))
      )
    } else {
      plotlyOutput("touch_points_plot", height = "100%")
    }
  })
  
  # This now renders the plot to be displayed by the UI above
  output$touch_points_plot <- renderPlotly({
    req(nrow(touch_points()) > 0) # Ensure data exists
    
    sorted_data <- touch_points() %>%
      arrange(desc(frequency)) %>%
      mutate(color_index = row_number())
    
    chart_title <- paste("# Farmers per touch point", subtitle_text())
    
    plot_ly(sorted_data,
            x = ~reorder(training_topics, -frequency),
            y = ~frequency,
            type = "bar",
            marker = list(
              color = ~color_index,
              colorscale = "Viridis",
              showscale = FALSE
            )) %>%
      layout(title = list(text = chart_title, font = list(size = 14)),
             xaxis = list(title = "", categoryorder = "array", categoryarray = ~reorder(training_topics, -frequency)),
             yaxis = list(title = "# Farmers"),
             margin = list(t = 50) # Add top margin
      )
  })
}

# Run the app
shinyApp(ui, server)