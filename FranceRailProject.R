## Dynamically set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
library(ggplot2)
library(rlang)
library(scales)

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
passengerstations = network %>% filter(voyageurs == "O")

### Create ridership dataframe, filter out unneeded columns
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

## Filter to only include station data that falls inside French borders
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

## Convert filtered travel times into edge values, but only use unique pairings
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

## Create new dataframe to store station stats: starting with centrality stats as base, merge ridership according to UIC code match
station_stats = centrality_stats %>%
  inner_join(ridership_df, by = c("stop_uic" = "code_uic_complet"))

## Generate list of station names, rename column value for easier use later on
station_names = network %>%
  st_drop_geometry() %>%
  dplyr::select(uic_code, libelle) %>%
  rename(station_name = libelle)

## Add station names to station stats according to UIC code match
station_stats = station_stats %>%
  left_join(station_names, by = c("stop_uic" = "uic_code"))

## Function to generate box plot
plot_boxplot = function(data, metric, metric_label) {
  ## Remove null values
  valid_data = data %>% filter(!is.na(.data[[metric]]), .data[[metric]] != 0)
  ## Generate lower and upper boundaries based on IQR
  q25 = quantile(valid_data[[metric]], 0.25)
  q75 = quantile(valid_data[[metric]], 0.75)
  iqr = q75 - q25
  lower_bound = q25 - 1.5 * iqr
  upper_bound = q75 + 1.5 * iqr
  ## Remove outliers for graph
  trimmed_data = valid_data %>%
    filter(.data[[metric]] >= lower_bound, .data[[metric]] <= upper_bound)
  ## Pull mean, median values from non-trimmed data for the input metric
  mean_val = mean(valid_data[[metric]], na.rm = TRUE)
  median_val = median(valid_data[[metric]], na.rm = TRUE)
  ## Create ggplot boxplot based on outlier-less data, using input metric as data
  p = ggplot(trimmed_data, aes(x = .data[[metric]], y = factor(1))) +
    geom_boxplot(width = 0.2, fill = "lightblue", outlier.shape = NA) +
    ## Add points for outlier-included mean/median values
    stat_summary(data = valid_data, fun = mean, geom = "point", color = "red", size = 3) +
    stat_summary(data = valid_data, fun = median, geom = "point", color = "blue", size = 3) +
    ## Create mean/median labels, offset their y value so they don't overlap
    annotate("text",
             x = mean_val,
             y = 1.25,
             label = paste("Mean (with outliers) =", round(mean_val, 2)),
             color = "red", hjust = 0) +
    annotate("text",
             x = median_val,
             y = 1.15,
             label = paste("Median (with outliers) =", round(median_val, 2)),
             color = "blue", hjust = 0) +
    ## Create title and axis labels based on input
    labs(title = paste("Boxplot of", metric_label),
         x = metric_label,
         y = "") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  ## Return finished boxplot
  return(p)
}

## Call function to generate boxplots
degree_boxplot = plot_boxplot(station_stats, "degree", "Degree Centrality for SNCF Stations")
betweenness_boxplot = plot_boxplot(station_stats, "betweenness", "Betweenness Centrality for SNCF Stations")

## Function to generate top/bottom 25 charts for input metrics
plot_top_bottom = function(data, metric, ylabel, title_prefix) {
  ## Create list of values associated with metric
  metric_sym = sym(metric)
  ## Exclude null and 0 values for that metric
  valid_data = data %>%
    filter(!is.na(!!metric_sym), !!metric_sym > 0)
  ## Create top/bottom 25 lists by slicing first/last 25 values
  top25 = valid_data %>% arrange(desc(!!metric_sym)) %>% slice_head(n = 25)
  bottom25 = valid_data %>% arrange(!!metric_sym) %>% slice_head(n = 25)
  ## Set unique label values if input metric is average ridership, but only for top 25, all other labels are in comma format (1000 = 1,000)
  if(metric == "avg_ridership"){
    top_axis_function = label_number(scale = 1/1e6, suffix = " million", accuracy = 1)
    top_text_function = comma
    bottom_axis_function = comma
    bottom_text_function = comma
  } else {
    top_axis_function = comma
    top_text_function = comma
    bottom_axis_function = comma
    bottom_text_function = comma
  }
  
  ## Create ggplot chart for top 25, using sliced data
  p_top = ggplot(top25, aes(x = reorder(station_name, !!metric_sym), y = !!metric_sym)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    ## Define axis 'ticks' for metric
    geom_text(aes(label = top_text_function(!!metric_sym)), hjust = -0.1, size = 3) +
    ## Flip orientation so chart lists stations from top to bottom rather than right to left
    coord_flip() +
    ## Create title, axis labels
    labs(title = paste(title_prefix, ": Top 25 Stations"),
         x = "Station",
         y = ylabel) +
    theme_minimal() +
    ## Ensure that labels metric ticks are in numeric (non-exponential) format
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       labels = top_axis_function)
  
  ## Create ggplot chart for bottom 25, using sliced data (identical to above)
  p_bottom = ggplot(bottom25, aes(x = reorder(station_name, !!metric_sym), y = !!metric_sym)) +
    geom_bar(stat = "identity", fill = "tomato") +
    geom_text(aes(label = bottom_text_function(!!metric_sym)), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(title = paste(title_prefix, ": Bottom 25 Stations"),
         x = "Station",
         y = ylabel) +
    theme_minimal() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       labels = bottom_axis_function)
  
  ## Return finished charts for top, bottom stats
  return(list(top = p_top, bottom = p_bottom))
}

## Generate boxplots for plots, betweenness, ridership
degree_charts = plot_top_bottom(station_stats, "degree", "Degree", "Degree Centrality")
betweenness_charts = plot_top_bottom(station_stats, "betweenness", "Betweenness", "Betweenness Centrality")
ridership_charts = plot_top_bottom(station_stats, "avg_ridership", "Average Ridership", "Average Annual Ridership")

## Generate folder to save boxplots, charts
output_folder = "Statistics"
if(!dir.exists(output_folder)){
  dir.create(output_folder)
}

## Save local versions of boxplots, charts in PNG format
ggsave("DegreeCentralityBoxplot.png", plot=degree_boxplot, bg="white", path=output_folder)
ggsave("BetweenessCentralityBoxplot.png", plot=betweenness_boxplot, bg="white", path=output_folder)
ggsave("DegreeCentralityTop25.png", plot=degree_charts$top, bg="white", path=output_folder)
ggsave("DegreeCentralityBottom25.png", plot=degree_charts$bottom, bg="white", path=output_folder)
ggsave("BetweennessCentralityTop25.png", plot=betweenness_charts$top, bg="white", path=output_folder)
ggsave("BetweennessCentralityBottom25.png", plot=betweenness_charts$bottom, bg="white", path=output_folder)
ggsave("RidershipTop25.png", plot=ridership_charts$top, bg="white", path=output_folder)
ggsave("RidershipBottom25.png", plot=ridership_charts$bottom, bg="white", path=output_folder)
