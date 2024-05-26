# Script Name: app.R
# Description: This script taps into the Zurich Open Data API to extract data on the status of all public outdoor pools in Zurich.
# Author: Kerim Lengwiler
# Date Created: 2024_05_04
# Date Updated: 2024_05_26

# Loading packages
library(remotes)
library(openrouteservice)
library(shiny)
library(httr)
library(XML)
library(bslib)
library(leaflet)
library(tidyverse)
library(sf)
library(shinydashboard)
library(fresh)

# API Key for OpenRouteService
ors_api_key <- Sys.getenv("ORS_API_KEY")

# URL for pool data
url <- "https://www.stadt-zuerich.ch/stzh/bathdatadownload"

# Make the request for pool data
pool_response <- GET(url = url, query = list()) 
http_status(pool_response)

xml_data <- xmlParse(rawToChar(pool_response$content))
pool <- getNodeSet(xml_data, "//bath")

# Extract data for each pool
pool_details <- lapply(pool, function(pool) {
  title <- xmlValue(getNodeSet(pool, "./title")[[1]])
  temperatureWater <- xmlValue(getNodeSet(pool, "./temperatureWater")[[1]])
  poiid <- xmlValue(getNodeSet(pool, "./poiid")[[1]])
  dateModified <- xmlValue(getNodeSet(pool, "./dateModified")[[1]])
  openClosedTextPlain <- xmlValue(getNodeSet(pool, "./openClosedTextPlain")[[1]])
  urlPage <- xmlValue(getNodeSet(pool, "./urlPage")[[1]])
  pathPage <- xmlValue(getNodeSet(pool, "./pathPage")[[1]])
  
  # Return a list of pool details
  list(Title = title, Wassertemperatur = temperatureWater, ID = poiid, 
       Update = dateModified, Status = openClosedTextPlain, 
       URL_Page = urlPage, Path_Page = pathPage)
})

# Convert the list to a data frame for easier manipulation and viewing
df_pools <- do.call(rbind.data.frame, pool_details)

if (class(df_pools$Wassertemperatur) %in% c("factor", "character")) {
  df_pools$Wassertemperatur <- as.numeric(as.character(df_pools$Wassertemperatur))
}

# Coordinates for the pools
df_pools_coordinates <- data.frame(
  Title = c("Flussbad Au-Höngg","Flussbad Oberer Letten","Flussbad Unterer Letten", "Flussbad Unterer Letten Flussteil", "Frauenbad Stadthausquai", "Freibad Allenmoos","Freibad Auhof",
            "Freibad Dolder", "Freibad Heuried", "Freibad Letzigraben", "Freibad Seebach", "Freibad Zwischen den Hölzern", "Hallenbad Bläsi", "Hallenbad Bungertwies", "Hallenbad City",
            "Hallenbad Leimbach", "Hallenbad Oerlikon", "Männerbad Schanzengraben ", "Seebad Enge", "Seebad Katzensee", "Seebad Utoquai", "Strandbad Mythenquai",
            "Strandbad Tiefenbrunnen","Strandbad Wollishofen ", "Wärmebad Käferberg" ),
  Latitude = c(47.39910, 47.38532, 47.39005, 47.38895, 47.36839, 47.40549, 47.40876, 47.37548, 47.36778, 47.37851, 47.42352, 47.40904, 47.40133, 47.37201, 47.37209, 47.32652,
               47.41023, 47.37122, 47.36170, 47.42844, 47.36176, 47.35451, 47.35239, 47.34112, 47.39942),
  Longitude = c(8.489321, 8.534956, 8.528608, 8.529391, 8.542030, 8.539013, 8.571462, 8.576269, 8.505399, 8.498366, 8.548195, 8.469622, 8.501876, 8.560206, 8.532868, 8.513683, 8.556734, 
                8.532703, 8.536701, 8.495608, 8.547013, 8.534713, 8.557461, 8.537336, 8.518000))


# Merging
df_pools <- merge(df_pools, df_pools_coordinates, by = "Title")

### SHINY APP

# Creating Theme
capstone_theme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

# Route Function (Directions)
get_route <- function(from_coords, to_coords, transport_type) {
  req(from_coords, to_coords, transport_type)
  tryCatch({
    route <- ors_directions(api_key = ors_api_key,
                            coordinates = list(from_coords, to_coords),
                            profile = transport_type,  # Use the selected transportation type
                            format = "geojson")
    print(paste("Route fetched from", from_coords, "to", to_coords, "using", transport_type))
    return(route)
  }, error = function(e) {
    print(paste("Error fetching route:", e$message))
    return(NULL)
  })
}

## Setting up User Interface (ui)
ui <- dashboardPage(
  dashboardHeader(title = tags$span("Freibäder Stadt Zürich", style = "font-weight: bold;")),
  dashboardSidebar(
    width = 250, # Customize sidebar width as needed
    sliderInput("temperatur", "Wassertemperatur", 
                min = min(df_pools$Wassertemperatur, na.rm = TRUE), 
                max = max(df_pools$Wassertemperatur, na.rm = TRUE), 
                value = c(min(df_pools$Wassertemperatur, na.rm = TRUE), 
                          max(df_pools$Wassertemperatur, na.rm = TRUE))),
    selectInput("status", "Status", choices = c("Alle", "offen", "geschlossen")),
    selectInput("title", "Name des Bades", choices = c("Alle", "Please select")),
    selectInput("transport_type", "Transportart", 
                choices = c("Fussweg" = "foot-walking", "Fahrrad" = "cycling-regular", "Auto" = "driving-car")),
    
    # Button group, positioned below the input selectors
    div(
      style = "display: flex; justify-content: flex-start; margin-top: 20px; margin-bottom: 20px;", # Added margin-bottom
      actionButton("show_route", "Route anzeigen", style = "margin-right: 5px;", icon = icon("route")),
      actionButton("clear_routes", "Route löschen", icon = icon("eraser"))
    ),
    actionButton("update_location", "Standort aktualisieren", icon = icon("location-arrow"))
  ),
  dashboardBody(
    use_theme(capstone_theme),
    tags$head(
      tags$style(HTML("
        .leaflet-container {
          height: calc(100vh - 80px) !important; 
        }
      ")),
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          // Geolocation setup
          if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(showPosition, showError);
          } else {
            Shiny.setInputValue('geolocation_error', 'Geolocation is not supported by this browser.');
          }

          function showPosition(position) {
            Shiny.setInputValue('user_lat', position.coords.latitude);
            Shiny.setInputValue('user_lon', position.coords.longitude);
            Shiny.setInputValue('geolocation', 'Lat: ' + position.coords.latitude + ', Lon: ' + position.coords.longitude);
          }

          function showError(error) {
            switch(error.code) {
              case error.PERMISSION_DENIED:
                Shiny.setInputValue('geolocation_error', 'User denied the request for Geolocation.');
                break;
              case error.POSITION_UNAVAILABLE:
                Shiny.setInputValue('geolocation_error', 'Location information is unavailable.');
                break;
              case error.TIMEOUT:
                Shiny.setInputValue('geolocation_error', 'The request to get user location timed out.');
                break;
              case error.UNKNOWN_ERROR:
                Shiny.setInputValue('geolocation_error', 'An unknown error occurred.');
                break;
            }
          }

          // Sidebar hiding functionality on 'Route anzeigen' button click
          $(document).on('shiny:inputchanged', function(event) {
            if (event.name === 'show_route') {
              $('#sidebarID').css('width', '0px');
              $('#sidebarID').css('visibility', 'hidden');
              $('.content-wrapper').css('margin-left', '0px');
            }
          });
        });
      "))
    ),
    leafletOutput("map", width = "100%", height = "100%")
  )
)


## Server
server <- function(input, output, session) {
  session$userData <- reactiveValues(selected_pool = NULL, user_location = NULL)
  
  # Observe the map marker click event  
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click)
    session$userData$selected_pool <- c(input$map_marker_click$lng, input$map_marker_click$lat)  
    showNotification("Standort Freibad ausgewählt.", type = "message")
  })
  
  observeEvent(input$show_route, {
    req(session$userData$selected_pool, input$user_lat, input$user_lon, input$transport_type)
    session$userData$user_location <- c(input$user_lon, input$user_lat)  
    
    from_coords <- session$userData$user_location
    to_coords <- session$userData$selected_pool
    
    # Validate coordinates to ensure they are within Zurich area
    if (from_coords[1] < 8 || from_coords[1] > 9 || from_coords[2] < 47 || from_coords[2] > 48 ||
        to_coords[1] < 8 || to_coords[1] > 9 || to_coords[2] < 47 || to_coords[2] > 48) {
      showNotification("Coordinates are out of expected range.", type = "error")
      return()
    }
    
    # Retrieve route from OpenRouteService using the selected transport type
    route <- get_route(from_coords, to_coords, input$transport_type)
    
    if (is.null(route)) {
      showNotification("Route not found.", type = "error")
      return()
    }
    
    leafletProxy("map") %>% 
      clearGroup("route") %>%  # Clear previous routes
      addGeoJSON(route, color = "blue", weight = 5, opacity = 0.7, group = "route")  
    showNotification("Route angezeigt.", type = "message")
  })
  
  # Clearing routes
  observeEvent(input$clear_routes, {
    leafletProxy("map") %>% 
      clearGroup("route")
    showNotification("Routen gelöscht.", type = "message")
  })
  
  # Printing latitude, longitude, and geolocation info
  output$lat <- renderPrint({
    req(input$user_lat)  
    input$user_lat
  })
  
  output$long <- renderPrint({
    req(input$user_lon)
    input$user_lon
  })
  
  output$geolocation <- renderPrint({
    req(input$geolocation)
    input$geolocation
  })
  
  output$geo_error <- renderPrint({
    input$geolocation_error
  })
  
  # Observe the 'Update My Location' button click
  observeEvent(input$update_location, {
    session$sendCustomMessage(type = 'getLocation', message = 'update')
    showNotification("Standort aktualisiert", type = "message")
  })
  
  # Custom message handler for initiating geolocation
  tags$script(HTML("
Shiny.addCustomMessageHandler('getLocation', function(message) {
  if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(showPosition, showError, {enableHighAccuracy: true, timeout: 5000, maximumAge: 0});
  } else {
    Shiny.setInputValue('geolocation_error', 'Geolocation is not supported by this browser.');
  }
});
"))
  
  # Updating the title select input
  observe({
    sorted_titles <- sort(unique(df_pools$Title))
    updateSelectInput(session, "title", choices = c("Alle", sorted_titles))
  })
  
  # Reactive expression for filtered data
  map_df = reactive({
    temp_filtered <- df_pools %>%
      filter(Wassertemperatur >= input$temperatur[1], Wassertemperatur <= input$temperatur[2])
    
    status_filtered <- if (input$status != "Alle") {
      temp_filtered %>%
        filter(Status == input$status)
    } else {
      temp_filtered
    }
    
    title_filtered <- if (input$title != "Alle") {
      status_filtered %>%
        filter(Title == input$title)
    } else {
      status_filtered
    }
    
    title_filtered %>%
      filter(!is.na(Longitude), !is.na(Latitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude")) %>%
      st_set_crs(4326)
  })
  
  # Initial map rendering
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 8.5417, lat = 47.3769, zoom = 12) 
  })
  
  # Updating the map with user location and pool markers
  observe({
    leafletProxy("map", data = NULL) %>%
      clearMarkers()
    
    # Add user location marker if available
    if (!is.null(input$user_lat) && !is.null(input$user_lon)) {
      leafletProxy("map") %>%
        addMarkers(lng = input$user_lon, lat = input$user_lat, popup = "Your location")
    }
    
    # Add pool markers
    leafletProxy("map") %>%
      addCircleMarkers(
        data = map_df(),
        popup = ~paste("<b>Bad:</b>", Title, "<br><strong>Wassertemperatur:</strong>", Wassertemperatur, "°C",
                       "<br><b>Status</b>", Status, "<br>Zuletzt aktualisiert:", Update),
        radius = 8,
        color = '#007BFF',
        fillOpacity = 0.7
      )
  })
}

shinyApp(ui, server)