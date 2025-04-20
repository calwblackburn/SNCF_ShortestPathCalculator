## Install packages
library(sf)
library(leaflet)
library(osmdata)
library(dplyr)
library(shiny)
library(rsconnect)
library(jsonlite)
library(tidytransit)
library(hms)
library(igraph)
library(tidyr)
library(stringr)

## Create cached file
cache_file = "cached_gtfs_data.rds"

## Load cache if it exists
if (file.exists(cache_file)) {
  cached_data = readRDS(cache_file)
  combined_data = cached_data$combined_data
  filtered_travel_times = cached_data$filtered_travel_times
  edges = cached_data$edges
  g = cached_data$g
  centrality_stats = cached_data$centrality_stats
  ridership_df = cached_data$ridership_df
  ## Otherwise, run script
} else {
  ## Import relevant data for stations, lines, ridership
  stations = read_sf(
    "https://ressources.data.sncf.com/api/explore/v2.1/catalog/datasets/gares-de-voyageurs/exports/geojson"
  )
  lines = read_sf(
    "https://ressources.data.sncf.com/api/explore/v2.1/catalog/datasets/formes-des-lignes-du-rfn/exports/geojson"
  )
  network = read_sf(
    "https://ressources.data.sncf.com/api/explore/v2.1/catalog/datasets/liste-des-gares/exports/geojson"
  )
  ridership_raw = jsonlite::fromJSON(
    "https://data.sncf.com/api/explore/v2.1/catalog/datasets/frequentation-gares/exports/json",
    flatten = TRUE
  )
  ## Filter out freight rail stations
  passengerstations = network %>% filter(voyageurs == "O")
  
  ## Create ridership dataframe, filter out unneeded columns
  ridership_df = ridership_raw
  ridership_df = ridership_df %>%
    dplyr::select(
      code_uic_complet,
      dplyr::matches("^(total_voyageurs_[0-9]{4}|totalvoyageurs[0-9]{4})$")
    )
  
  ## Rename any remaining columns that are not under uniform name
  ridership_df = ridership_df %>%
    dplyr::rename_with(
      ~ gsub("^totalvoyageurs", "total_voyageurs_", .x),
      dplyr::matches("^totalvoyageurs")
    )
  
  ## Calculate average of all year values, return whole number result
  cols_to_convert = setdiff(names(ridership_df), "code_uic_complet")
  ridership_df[cols_to_convert] = lapply(ridership_df[cols_to_convert], as.numeric)
  ridership_df$avg_ridership = round(rowMeans(ridership_df[, cols_to_convert], na.rm = TRUE), 0)
  
  
  ## Function that takes a gtfs web link (.zip form) and finds quickest travel times between nodes
  process_gtfs = function(gtfs_path, train_type) {
    ## Load data from source (must be a .zip link)
    gtfs_data = read_gtfs(gtfs_path)
    
    ## Retrieve necessary data tables from GTFS file
    stop_times = gtfs_data$stop_times
    trips = gtfs_data$trips
    routes = gtfs_data$routes
    stops = gtfs_data$stops
    
    ## Join trip, route, stop tables to construct trip information
    trip_info = stop_times %>%
      left_join(trips, by = "trip_id") %>%
      left_join(routes, by = "route_id") %>%
      arrange(route_id, trip_id, stop_sequence)
    
    ## Calculate transit times with times in minutes, handling overnight trips (trips where arrival time is earlier than departure time)
    fastest_transit_times = trip_info %>%
      group_by(route_id, trip_id) %>%
      mutate(
        arrival_time_minutes = as.numeric(substr(arrival_time, 1, 2)) * 60 + as.numeric(substr(arrival_time, 4, 5)),
        departure_time_minutes = as.numeric(substr(departure_time, 1, 2)) * 60 + as.numeric(substr(departure_time, 4, 5)),
        next_arrival_time_minutes = lead(arrival_time_minutes),
        next_stop_id = lead(stop_id)
      ) %>%
      filter(!is.na(next_arrival_time_minutes)) %>%
      mutate(
        transit_time = if_else(
          next_arrival_time_minutes < departure_time_minutes,
          (next_arrival_time_minutes + 1440 - departure_time_minutes),
          next_arrival_time_minutes - departure_time_minutes
        ),
        stop_uic = str_extract(stop_id, "[0-9]+$"),
        next_stop_uic = str_extract(next_stop_id, "[0-9]+$")
      ) %>%
      ungroup() %>%
      select(stop_uic, next_stop_uic, transit_time)
    
    ## Map UIC codes to stop names
    stop_info = stops %>%
      mutate(uic_code = str_extract(stop_id, "[0-9]+$")) %>%
      distinct(uic_code, .keep_all = TRUE) %>%
      select(uic_code, stop_name)
    
    ## Add O-D UIC code, station name to fastest transit times dataframe
    fastest_transit_times = fastest_transit_times %>%
      left_join(stop_info, by = c("stop_uic" = "uic_code")) %>%
      rename(origin_stop_name = stop_name) %>%
      left_join(stop_info, by = c("next_stop_uic" = "uic_code")) %>%
      rename(destination_stop_name = stop_name) %>%
      mutate(train_type = train_type)
    
    ## Return completed dataset
    return(fastest_transit_times)
  }
  
  ## Process SNCF Intercités (Local), TER (Express), and TGV (High-Speed) GTFS data
  intercity_data = process_gtfs(
    "https://eu.ftp.opendatasoft.com/sncf/plandata/export-intercites-gtfs-last.zip",
    "Intercités"
  )
  ter_data = process_gtfs(
    "https://eu.ftp.opendatasoft.com/sncf/plandata/export-ter-gtfs-last.zip",
    "TER"
  )
  tgv_data = process_gtfs(
    "https://eu.ftp.opendatasoft.com/sncf/plandata/export_gtfs_voyages.zip",
    "TGV"
  )
  
  ## Merge all results
  combined_data = bind_rows(intercity_data, ter_data, tgv_data)
  
  ## Find shortest transit time pairing for each origin-destination pairing from local, express, and high-speed
  shortest_transit_times = combined_data %>%
    group_by(stop_uic, next_stop_uic) %>%
    arrange(transit_time) %>%
    slice(1) %>%
    ungroup()
  
  ## Since the network contains duplicate stations, remove all but one
  network = network %>%
    mutate(uic_code = ifelse(
      grepl("-", code_uic),
      str_extract(code_uic, "[0-9]+$"),
      code_uic
    )) %>%
    separate_rows(uic_code, sep = ";") %>%
    distinct(uic_code, .keep_all = TRUE)
  
  ## Since travel times include trips outside of France, filter these out
  filtered_travel_times = shortest_transit_times %>%
    filter(stop_uic %in% network$uic_code,
           next_stop_uic %in% network$uic_code)
  
  ## Create new dataframe of pairings, but only use unique pairings
  edges = filtered_travel_times %>%
    select(stop_uic, next_stop_uic, transit_time) %>%
    distinct()
  
  ## Create list of all station UIC codes as comparison for graph generation
  all_station_codes = unique(network$uic_code)
  
  ## Generate graph from edge data to create metrics, using station list to ensure no station is left out
  g = graph_from_data_frame(edges,
                            directed = FALSE,
                            vertices = data.frame(name = all_station_codes))
  
  ## Create degree and betweenness metrics for data
  degree_centrality = degree(g, mode = "all")
  betweenness_centrality = betweenness(g,
                                       directed = FALSE,
                                       weights = 1 / E(g)$transit_time)
  
  ## Create data frame to store statistics
  centrality_stats = data.frame(
    stop_uic = V(g)$name,
    degree = degree_centrality,
    betweenness = betweenness_centrality
  )
  
  ## Fix rail line color/label values based on whether they are part of passenger or freight network
  lines = lines %>%
    mutate(
      color = ifelse(
        code_ligne %in% passengerstations$code_ligne,
        "darkblue",
        "darkred"
      ),
      class = ifelse(
        code_ligne %in% passengerstations$code_ligne,
        "Passenger Rail",
        "Freight Rail"
      )
    )
  
  ## Create and store list of cached data
  cached_data = list(
    combined_data = combined_data,
    filtered_travel_times = filtered_travel_times,
    edges = edges,
    g = g,
    centrality_stats = centrality_stats,
    network = network,
    stations = stations,
    lines = lines,
    passengerstations = passengerstations,
    ridership_df = ridership_df
  )
  saveRDS(cached_data, file = cache_file)
  cat("GTFS data processed and cached.\n")
}

## Import icon for stations, set dimensions
station_icon = icons(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/9/96/Paris_transit_icons_-_Train.svg",
                     iconWidth = 9,
                     iconHeight = 9)


## Function to calculate shortest path between two points in network
calculate_shortest_path = function(origin, destination) {
  ## Check to make sure it is in the network
  if (origin %in% V(g)$name && destination %in% V(g)$name) {
    ## Use network to determine shortest path between points, using transit time as weight
    shortest_path = shortest_paths(
      g,
      from = origin,
      to = destination,
      weights = E(g)$transit_time,
      output = "vpath"
    )
    ## Create a string with all associated UICs
    path_uics = V(g)$name[unlist(shortest_path$vpath)]
    path_segments = c()
    ## Start total time accumulator
    total_time = 0
    ## Loop through UIC string, pull stop origin, train type, and transit time
    for (i in 1:(length(path_uics) - 1)) {
      start_uic = path_uics[i]
      next_uic = path_uics[i + 1]
      segment = filtered_travel_times %>%
        filter(stop_uic == start_uic, next_stop_uic == next_uic) %>%
        select(origin_stop_name, train_type, transit_time) %>%
        distinct() %>%
        slice(1)
      ## If path is possible, generate string from each stop on the way with name + train type, add transit time to accumulator
      if (nrow(segment) > 0) {
        path_segment = paste0(segment$origin_stop_name,
                              " (",
                              segment$train_type,
                              ")")
        path_segments = c(path_segments, path_segment)
        total_time = total_time + segment$transit_time
        ## Otherwise, tell user
      } else {
        return(paste("Path not possible in network."))
      }
    }
    ## Pull value for ultimate destination stop
    final_stop_name = filtered_travel_times %>%
      filter(stop_uic == path_uics[length(path_uics)]) %>%
      select(origin_stop_name) %>%
      distinct() %>%
      slice(1) %>%
      pull()
    ## Generate final string in format: "Stop -> Stop -> ... -> Final Stop"
    path_segments = c(path_segments, final_stop_name)
    path_string = paste(path_segments, collapse = " -> ")
    ## Calculate hour, minute values for travel time from accumulator
    hours = total_time %/% 60
    minutes = total_time %% 60
    ## Only show hours/minutes if necessary
    if (hours > 0) {
      if (minutes > 0) {
        time_string = paste(hours, "hour(s)", minutes, "minute(s)")
      } else {
        time_string = paste("Hours:", hours)
      }
    } else {
      time_string = paste("Minutes:", minutes)
    }
    ## Add total time to path string
    path_string = paste0(path_string, " | Total time: ", time_string)
    ## Finally, return it
    return(path_string)
    ## If bad input, inform user
  } else {
    return("Origin or destination stop is not in network.")
  }
}

## Make sure that UIC codes are good for processing
filtered_travel_times = filtered_travel_times %>%
  mutate(stop_uic = as.character(stop_uic),
         next_stop_uic = as.character(next_stop_uic))

## Create bounding box with dimensions of France
france_bbox = st_bbox(c(
  xmin = -5.0,
  ymin = 41.0,
  xmax = 10.0,
  ymax = 51.0
), crs = st_crs(4326))

## Exclude all stations that are outside of this bounding box
stations_in_france = cached_data$network %>%
  mutate(
    uic_code = as.character(uic_code),
    coords = st_coordinates(geometry),
    longitude = coords[, 1],
    latitude = coords[, 2]
  ) %>%
  filter(longitude >= -5.0,
         longitude <= 10.0,
         latitude >= 41.0,
         latitude <= 51.0) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    agr = "constant"
  ) %>%
  ## Add centrality statistics to these stations for use in map
  left_join(centrality_stats, by = c("uic_code" = "stop_uic")) %>%
  left_join(
    ridership_df %>% dplyr::select(code_uic_complet, avg_ridership),
    by = c("uic_code" = "code_uic_complet")
  )

## Generate labels for stations
station_labels = paste(
  "<strong>Station Name:</strong>",
  stations_in_france$libelle,
  "<br><strong>Degree Centrality (non-stop connections to other stations):</strong>",
  stations_in_france$degree,
  "<br><strong>Betweenness Centrality (paths in network that use this station):</strong>",
  format(
    round(stations_in_france$betweenness, 0),
    big.mark = ",",
    scientific = FALSE
  ),
  "<br><strong>Average Annual Ridership (2015-2023):</strong>",
  format(
    stations_in_france$avg_ridership,
    big.mark = ",",
    scientific = FALSE
  )
)

## Create UI for app
ui = fluidPage(
  ## Set size dynamically according to device
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  ## Set title, selector values (selector is empty with prompt for user to search)
  titlePanel(
    "SNCF Shortest Path Finder: Find the Shortest Possible Travel Time Between Two Stations in the French Rail Network"
  ),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "origin",
        "Select Origin Station:",
        choices = NULL,
        selected = NULL,
        options = list(placeholder = 'Start typing to search...', maxOptions = 4000)
      ),
      
      selectizeInput(
        "destination",
        "Select Destination Station:",
        choices = NULL,
        selected = NULL,
        options = list(placeholder = 'Start typing to search...', maxOptions = 4000)
      ),
      
      actionButton("findPath", "Find Shortest Path"),
      width = 3
    ),
    ## Create space for shortest path output
    mainPanel(
      leafletOutput("map", height = "80vh"),
      verbatimTextOutput("pathOutput"),
      width = 9
    )
  ),
  tags$style(
    type = "text/css",
    "#pathOutput {white-space: pre-wrap; word-wrap: break-word; overflow-x: hidden; overflow-y: auto; height: 150px; border: 1px solid #ccc; padding: 10px;}"
  )
)

## Define server for Shiny
server = function(input, output, session) {
  ## Generate a leaflet map with predefined station, marker, rail line values
  output$map = renderLeaflet({
    leaflet(stations_in_france) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        icon = station_icon,
        lng = ~ st_coordinates(geometry)[, 1],
        lat = ~ st_coordinates(geometry)[, 2],
        popup = station_labels
      ) %>%
      addPolylines(
        data = cached_data$lines,
        color = ~ color,
        label = ~ class
      )
  })
  
  ## Populate available origins from filtered travel times list, sort alphabetically
  observe({
    available_origins = filtered_travel_times %>%
      distinct(origin_stop_name) %>%
      pull(origin_stop_name) %>%
      sort()
    
    if (!is.null(input$origin) &&
        input$origin %in% available_origins) {
      return()
    }
    
    ## Ensure that refreshing the origin list at start does not populate the selector
    updateSelectizeInput(
      session,
      "origin",
      choices = available_origins,
      selected = character(0),
      server = TRUE
    )
  })
  
  ## Sort destination selector list
  observeEvent(input$origin, {
    origin_uic = filtered_travel_times %>%
      filter(origin_stop_name == input$origin) %>%
      pull(stop_uic) %>%
      unique()
    
    ## Only allow destination selection when origin is populated
    if (!is.null(origin_uic) && any(origin_uic %in% V(g)$name)) {
      reachable_stations = V(g)$name[distances(g, v = origin_uic, weights = E(g)$transit_time) < Inf]
      reachable_stop_names = filtered_travel_times %>%
        filter(stop_uic %in% reachable_stations) %>%
        distinct(destination_stop_name) %>%
        pull(destination_stop_name) %>%
        sort()
      
      if (!is.null(input$destination) &&
          input$destination %in% reachable_stop_names) {
        return()
      }
      
      ## Ensure that refreshing the destination list at start does not populate the selector
      updateSelectizeInput(
        session,
        "destination",
        choices = reachable_stop_names,
        selected = character(0),
        server = TRUE
      )
    }
  })
  
  ## Ensure that path is only calculated when origin and destination are provided
  observeEvent(input$findPath, {
    req(input$origin, input$destination)
    
    ## Convert station names to UICs for path calculation
    origin_uic = filtered_travel_times %>%
      filter(origin_stop_name == input$origin) %>%
      pull(stop_uic) %>%
      unique()
    destination_uic = filtered_travel_times %>%
      filter(destination_stop_name == input$destination) %>%
      pull(next_stop_uic) %>%
      unique()
    
    ## Calculate shortest path unless path not possible
    path_result = tryCatch({
      calculate_shortest_path(origin_uic, destination_uic)
    }, error = function(e) {
      "Path not possible in network."
    })
    
    ## Return path text when necessary
    output$pathOutput = renderText({
      path_result
    })
    
  })
}

shinyApp(ui, server)