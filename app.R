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
pacman::p_load(magrittr, dplyr, readr, stringr, tidyr, shiny, lubridate, nngeo,
                  bslib, shinyjs, shinycssloaders,
                  plotly, leaflet, sf, tmap, viridis, janitor)

# Load kpi data
path <- getwd()
data_cws <- read_csv(paste(path, "data/Coffee_Washing_Stations.csv", sep = "/")) %>% clean_names(.)
data_coops <- read_csv(paste(path, "data/Cooperatives.csv", sep = "/")) %>% clean_names(.) 
data_farmers <- read_csv(paste(path, "data/Coffee_farmers.csv", sep = "/"), col_types = cols(.default = col_character())) %>% clean_names(.)
data_farms <- read_csv(paste(path, "data/Coffee_farms.csv", sep = "/"), col_types = cols(.default = col_character())) %>% clean_names(.)

# convert coops and CWS data to sf
data_coops %<>% st_as_sf(wkt = "geom", crs = 4326, remove = T) %>% st_transform(crs = 32736) 
data_cws %<>% st_as_sf(wkt = "geom", crs = 4326, remove = T) %>% st_transform(crs = 32736) 

# for farms, there are some invalid WKT strings and we are going to remove these rows first
#-------------------------------------------------
# Create a function to check if a WKT string is valid
check_valid_wkt <- function(wkt) {
  result <- tryCatch(
    {
      st_as_sfc(wkt, crs = 4326)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
  return(result)
}

# Identify valid geometries
data_farms$valid_geom <- sapply(data_farms$geom, check_valid_wkt)

# Display the count of invalid geometries
num_invalid <- sum(!data_farms$valid_geom)
cat("Number of invalid geometries:", num_invalid, "\n")

# Filter to keep only valid geometries and convert to sf
data_farms %<>% filter(valid_geom) %>% st_as_sf(wkt = "geom", crs = 4326, remove = TRUE)
data_farms %<>%  select(-valid_geom) %>% st_transform(crs = 32736) 

# get farms centroids
data_farms_centroids <- data_farms %>% st_centroid()

# convert some columns to numbers
data_coops %<>% mutate(nbr_cooperative_members = as.numeric(nbr_cooperative_members))
data_cws %<>% mutate(actual_capacity = as.numeric(actual_capacity))

# calculate area of farms
data_farms %<>% mutate(area = st_area(.)) %>% mutate(area = as.numeric(area)/100)

# since one farmer (national id) can have multiple farms, we need to aggregate the data
# to get the total area and number of coffee trees per farmer (per age of trees)
data_farms_stats <- data_farms %>% st_drop_geometry() %>% group_by(national_id, age_range_coffee_trees) %>% 
  summarise(area = sum(area, na.rm = T),
            nbr_coffee_trees = sum(as.integer(nbr_coffee_trees), na.rm = T),
            .groups = "drop")

#-------------------------------------------------------
# ensure we have farmers in both the farms and farmers datasets.
# This is to be removed when we have full data
data_farmers %<>% mutate(national_id = sample(data_farms_stats$national_id, n(), replace = TRUE))
#-------------------------------------------------------------

# join farmers IDs to their corresponding farms
data_farmers_full <- data_farmers %>% select(national_id, district, training_topics, cooperative, farmer_cws_id) %>%
  left_join(data_farms_stats, by = "national_id")

#-------------------------------------------------------
# ensure we have many cooperatives in the farmers datasets that match those in the coops dataset.
# we do the same for CWS data as well
# This is to be removed when we have full data
data_farmers_full %<>% mutate(cooperative_id = sample(data_coops$cooperative_id, n(), replace = TRUE))
data_farmers_full %<>% mutate(cws_id = sample(data_cws$cws_id, n(), replace = TRUE))
#-------------------------------------------------------------

# load geospatial data
country <- st_read(paste(path,"data_wgs84", "RW_country.gpkg", sep = "/"), layer = "country")
lakes <- st_read(paste(path,"data_wgs84", "RW_lakes.gpkg", sep = "/"), layer = "lakes")
np <- st_read(paste(path,"data_wgs84", "RW_national_parks.gpkg", sep = "/"), layer = "np")
districts <- st_read(paste(path,"data_wgs84", "RW_districts.gpkg", sep = "/"), layer = "districts")

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
              htmlOutput("farm_area") %>%
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
            plotlyOutput("coffee_trees_chart") %>%
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
            plotlyOutput("touch_points_chart") %>%
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
    # Dynamic size values for the legends
    coop_sizes <- range(data_coops$nbr_cooperative_members, na.rm = TRUE)
    cws_sizes <- range(data_cws$actual_capacity, na.rm = TRUE)
    
    coop_breaks <- round(seq(coop_sizes[1], coop_sizes[2], length.out = 3))  
    cws_breaks <- round(seq(cws_sizes[1], cws_sizes[2], length.out = 3))
    
    # Create the tmap object
    tmap_object <- tm_shape(districts) + 
      tm_borders(col = "#A76948", alpha = .6) +
      # Add tooltip and popup for districts
      tm_text("district", size = 0) +  # This adds labels but makes them invisible
      
      tm_shape(lakes) +
      tm_polygons(col = "#2CA2E6", 
                  alpha = .6, 
                  popup.vars = c("Lake" = "name"),
                  id = "name") +  # Add lake names as tooltips
      
      tm_shape(np) + 
      tm_polygons(col = "#158849", 
                  alpha = .6,
                  popup.vars = c("National Park" = "name"),
                  id = "name") +  # Add national park names as tooltips
      
      tm_shape(country) + 
      tm_borders(lwd = 2) +
      
      tm_shape(data_coops) + 
      tm_dots(col = "#063b57", 
              size = "nbr_cooperative_members",
              title = "Cooperatives",
              popup.vars = c("Name" = "cooperative_name",
                             "Members" = "nbr_cooperative_members"),
              id = "cooperative_name",  # Use cooperative name as tooltip
              legend.show = TRUE) +
      
      tm_shape(data_cws) + 
      tm_dots(col = "#440d4a",  
              size = "actual_capacity",
              title = "Coffee Washing Stations",
              popup.vars = c("Name" = "cws_name",
                             "Capacity" = "actual_capacity"),
              id = "cws_name",  # Use CWS name as tooltip
              legend.show = TRUE) +
      
      tm_view(bbox = st_bbox(country))
    # Convert the tmap object to a Leaflet map
    leaflet_map <- tmap_leaflet(tmap_object)
    
    # Create legends for both layers
    coop_legend <- addLegendCustom(
      map = NULL, 
      position = NULL,
      size_values = coop_breaks,
      labels = paste(coop_breaks, "members"),
      color = "#063b57",
      title = "Cooperatives"
    )
    
    cws_legend <- addLegendCustom(
      map = NULL, 
      position = NULL,
      size_values = cws_breaks,
      labels = paste(cws_breaks, "capacity"),
      color = "#440d4a",
      title = "CWS"
    )
    
    # Combine legends into a single container with horizontal layout
    combined_legend_html <- paste0(
      "<div style='display: flex; justify-content: center; align-items: flex-start; 
                background: rgba(255, 255, 255, 0.2); padding: 5px; border-radius: 5px;'>",
      coop_legend,
      cws_legend,
      "</div>"
    )
    
    # Add combined legend to the map
    leaflet_map %>% addControl(html = combined_legend_html, position = "bottomright")
  })
  
  # Helper function to create a dynamic size legend
  addLegendCustom <- function(map, position, size_values, labels, color, title) {
    legend_html <- paste0(
      "<div style='padding: 10px; background: white; border: 1px solid #ccc; 
                  border-radius: 5px; display: inline-block; margin-right: 10px;'>",
      "<strong>", title, "</strong><br>"
    )
    
    for (i in seq_along(size_values)) {
      legend_html <- paste0(
        legend_html,
        "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
        "<div style='width:", size_values[i] / max(size_values) * 20, "px; height:", 
        size_values[i] / max(size_values) * 20, 
        "px; background:", color, "; border-radius:50%; margin-right: 10px;'></div>",
        labels[i], "</div>"
      )
    }
    
    legend_html <- paste0(legend_html, "</div>")
    return(legend_html)
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
      tm_borders(col = "#A76948", alpha = .6) +
      tm_fill(col = "#A76948", 
              alpha = .2,
              id = "district") +
      
      tm_shape(lakes) +
      tm_polygons(col = "#2CA2E6", 
                  alpha = .6, 
                  popup.vars = c("Lake" = "name")) +
      
      tm_shape(np) + 
      tm_polygons(col = "#085e27", 
                  alpha = .6,
                  popup.vars = c("National Park" = "name")) +
      
      tm_shape(country) + 
      tm_borders(lwd = 2) +
      
      tm_shape(data_farms_centroids) + 
      tm_dots(col = "#011e0b",
              alpha = .6,
              size = 0.1) +
      
      tm_view(bbox = st_bbox(country))
    
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
          summarize(farm_area_ha = sum(area)/100) %>% 
          pull(farm_area_ha)
      } else {
        data_farmers_full %>% 
          filter(cws_id == rv$clicked_point$row$cws_id) %>%
          summarize(farm_area_ha = sum(area)/100) %>% 
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
  
  # Display the statistics at the CWS/Cooperative or district level
  output$farm_area <- renderUI({
    tagList(
      h1(format(round(farm_area()), big.mark = ",")),  
      p("Farm area in Ha")  
    )
  })
  
  # Display the tree numbers chart card
  # First, we will create a helper function to set the correctly sort the tree age  categories
  tree_age_order <- function(x) {
    factor(x, levels = c("less_3", "3_to_7", "8_to_15", "16_to_30", "more_30"))
  }
  
  # Now, let's get the total number of trees
  total_trees <- reactive({
    coffee_trees() %>% 
      summarize(nbr_coffee_trees = sum(nbr_coffee_trees, na.rm = T)) %>% 
      pull(nbr_coffee_trees)
  })
  
  # finally, render the coffee trees chart
  output$coffee_trees_chart <- renderPlotly({
    plot_data <- coffee_trees() %>%
      mutate(age_range_coffee_trees = tree_age_order(age_range_coffee_trees))
    
    # Format the total trees number with commas
    total_formatted <- format(total_trees(), big.mark = ",")
    
    plot_ly(plot_data, 
            x = ~age_range_coffee_trees, 
            y = ~nbr_coffee_trees, 
            type = "bar",
            marker = list(
              color = "#3ea363")
    ) %>%
      layout(title = list(
        text = paste("Total:", total_formatted, "trees")
      ),
      xaxis = list(# title = "Age range of coffee trees",
        categoryorder = "array",
        categoryarray = levels(tree_age_order(""))),
      yaxis = list(title = "# Coffee trees")
      )
  })
  
  # Display the training chart card
  output$touch_points_chart <- renderPlotly({
    # Sort the data and add color mapping
    sorted_data <- touch_points() %>% 
      arrange(desc(frequency)) %>%
      mutate(color_index = row_number())
    
    # Create plot using the viridis color ramp
    plot_ly(sorted_data, 
            x = ~reorder(training_topics, -frequency),
            y = ~frequency, 
            type = "bar",
            marker = list(
              color = ~color_index,
              colorscale = "Viridis",
              showscale = FALSE  # Don't show the color scale
            )) %>%
      layout(title = "",
             xaxis = list(title = "",
                          categoryorder = "array",
                          categoryarray = ~reorder(training_topics, -frequency)),
             yaxis = list(title = "# Farmers")
      )
  })
}

# Run the app
shinyApp(ui, server)
