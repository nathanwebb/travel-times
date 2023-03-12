# Installs the required packages
#install.packages("dplyr","ggplot2","rgdal","tmap","ggmap")

# Loads the required required packages
library(dplyr)
#>
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#>
#>     filter, lag
#> The following objects are masked from 'package:base':
#>
#>     intersect, setdiff, setequal, union
library(ggplot2)

#> Loading required package: sp
#> rgdal: version: 1.3-4, (SVN revision 766)
#>  Geospatial Data Abstraction Library extensions to R successfully loaded
#>  Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
#>  Path to GDAL shared files: /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rgdal/gdal
#>  GDAL binary built with GEOS: FALSE
#>  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
#>  Path to PROJ.4 shared files: /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rgdal/proj
#>  Linking to sp version: 1.3-1
#library(tmap)
#library(ggmap)
library(sf)
library(googleway)
library(assertthat)
library(leaflet)

targets <- list(
  odi = list(
    latlng = c(-33.86516, 151.19618),
    arrival = as.POSIXct("2023-03-06 09:10:00", tz = "Australia/Sydney"),
    mode = "transit"
  ),
  odi_drive = list(
    latlng = c(-33.86516, 151.19618),
    arrival = as.POSIXct("2023-03-06 09:10:00", tz = "Australia/Sydney"),
    mode = "driving"
  ),
  llv = list(
    latlng = c(-33.78824, 151.16135),
    arrival = as.POSIXct("2023-03-06 08:45:00", tz = "Australia/Sydney"),
    mode = "transit"
  ),
  llv_drive = list(
    latlng = c(-33.78824, 151.16135),
    arrival = as.POSIXct("2023-03-06 08:45:00", tz = "Australia/Sydney"),
    mode = "driving"
  ),
  pe = list(
    latlng = c(-33.87283, 151.22373),
    arrival = as.POSIXct("2023-03-06 10:00:00", tz = "Australia/Sydney"),
    mode = "transit"
  ),
  crowie = list(
    latlng = c(-33.82743, 151.20097),
    arrival = as.POSIXct("2023-03-11 17:00:00", tz = "Australia/Sydney"),
    mode = "transit"
  ),
  chatswood = list(
    latlng = c(-33.79799, 151.18093),
    arrival = as.POSIXct("2023-03-11 17:00:00", tz = "Australia/Sydney"),
    mode = "transit"
  )
)

metrocouncils <- c("Ku-ring-gai", "Lane Cove", "Mosman", "North Sydney",
                   "Northern Beaches", "Ryde", "Hunters Hill", "Willoughby", "Sydney", "Inner West", "Canada Bay", "Woollahra")
northshore <- c("Ku-ring-gai", "Lane Cove", "Mosman", "North Sydney",
                "Ryde", "Willoughby")
keysuburbs <- c("Ku-ring-gai", "Lane Cove", "Ryde", "Willoughby")
councils <- northshore


polygon_centroids <- function(polygons) {
  polygons %>% sf::st_centroid() %>% sf::st_coordinates()
}

query_distance_matrix <- function(
    origin,
    destination,
    arrival_time,
    mode = "transit"
) {
  rev_origin <- if (is.matrix(origin) && nrow(origin) == 1) {
    c(origin[1,2], origin[1,1])
  } else if (is.matrix(origin)) {
    as.data.frame(origin[, c(2, 1)])
  } else {
    rev(origin)
  }

  response <- googleway::google_distance(
    origins = rev_origin,
    destinations = destination,
    mode = mode,
    arrival_time = arrival_time,
    units = "metric",
    key = "AIzaSyBTF-FDG4a5R8QJMhwbwuTZb6pVBwVB_kw"
  )
  if (response$status != "OK") {
    stop(response$error_message)
  }
  response
}

dm_origin_address <- function(response) response$origin_addresses
dm_matrix_response <- function(response) response$rows$elements
dm_distance <- function(response) {
  response %>% dm_matrix_response() %>% purrr::map_int(
    function(x) ifelse(x$status == "OK", x$distance$value, NA_integer_)
  )
}
dm_time <- function(response) {
  response %>% dm_matrix_response() %>% purrr::map_int(
    function(x) ifelse(x$status == "OK", x$duration$value, NA_integer_)
  )
}

commute_facts <- function(polygons, destination, arrival_time, mode = "transit") {
  batch_size <- 25
  n_polys <- polygons %>% length()
  batches <- ceiling(n_polys / batch_size)

  query_batch_number <- function(batch_number) {
    batch_start <- batch_size * (batch_number - 1) + 1
    batch_end <- min(batch_start + batch_size - 1, n_polys)
    polygons_in_batch <- polygons[batch_start:batch_end]
    coords_in_batch <- polygons_in_batch %>% polygon_centroids()
    response <- query_distance_matrix(coords_in_batch, destination = destination, arrival_time = arrival_time, mode = mode)

    dplyr::as_tibble(polygons_in_batch) %>%
      cbind(coords_in_batch) %>%
      dplyr::mutate(
        origin = dm_origin_address(response),
        commute_distance_m = dm_distance(response),
        commute_time_s = dm_time(response),
        commute_time = paste(round(commute_time_s / 60, 1), "minutes")
      ) %>%
      dplyr::as_tibble()
  }

  purrr::map_dfr(seq(batches), query_batch_number) %>% dplyr::distinct()
}

suburb_grid <- function(metro_grid, localities, suburb) {
  assertthat::assert_that(suburb %in% localities$ABB_NAME)
  suburb_shp <- localities %>% dplyr::filter(ABB_NAME == suburb)
  grid_in_suburb <- metro_grid[suburb_shp]
  assertthat::assert_that(length(grid_in_suburb) > 0)
  grid_in_suburb
}

clamp_values <- function(values, min_value = NULL, max_value = NULL) {
  if (is.null(min_value)) min_value <- min(values, na.rm = TRUE)
  if (is.null(max_value)) max_value <- max(values, na.rm = TRUE)

  clamped_values <- values
  clamped_values[values > max_value] <- max_value
  clamped_values[values < min_value] <- min_value

  clamped_values
}

clamped_palette_function <- function(palette, values, min_value = NULL, max_value = NULL, reverse_palette = TRUE) {
  clamped_values <- clamp_values(values, min_value, max_value)

  leaflet::colorNumeric(
    palette,
    reverse = reverse_palette,
    domain = min(clamped_values):max(clamped_values)
  )
}

clamped_palette <- function(palette, values, min_value = NULL, max_value = NULL, reverse_palette = TRUE) {
  fun <- clamped_palette_function(palette, values, min_value, max_value, reverse_palette)
  clamped_values <- clamp_values(values, min_value, max_value)
  return(fun(clamped_values))
}

leaflet_title_class <- htmltools::tags$style(htmltools::HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 38%;
    max-width: 50%;
    text-align: center;
    padding-left: 5px;
    padding-right: 5px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 1.0em;
  }
"))

leaflet_title <- htmltools::tags$div(
  leaflet_title_class,
  htmltools::HTML("Public transport commute time to Google Office")
)

plot_commutes <- function(
    polygons,
    colour_by,
    destination,
    min_value = 10 * 60,
    max_value = 90 * 60
) {
  non_na_polygons <- dplyr::filter(polygons, !is.na(commute_time_s))

  leaflet::leaflet() %>%
    leaflet::addControl(leaflet_title, position = "topleft", className = "map-title") %>%
    leaflet::addProviderTiles("OpenStreetMap") %>%
    leaflet::setView(lng = destination[2], lat = destination[1], zoom = 12) %>%
    leaflet::addPolygons(
      data = sf::st_transform(non_na_polygons$geometry, "+proj=longlat +datum=WGS84"),
      fillColor = clamped_palette(
        "Spectral",
        non_na_polygons$commute_time_s,
        min_value = min_value,
        max_value = max_value
      ),
      fillOpacity = 0.4,
      weight = 0,
      label = non_na_polygons$commute_time
    ) %>%
    leaflet::addLegend(
      title = "commute time",
      pal = clamped_palette_function(
        "Spectral",
        non_na_polygons$commute_time_s,
        min_value = min_value,
        max_value = max_value
      ),
      values = clamp_values(non_na_polygons$commute_time_s, min_value, max_value),
      labFormat = leaflet::labelFormat(
        suffix = " min",
        transform = function(x) round(x / 60)
      )
    )
}

viewport <- htmltools::tags$meta(
  name = "viewport",
  content = "width=device-width, initial-scale=1.0"
)

save_commute_plot <- function(commute_plot, file_path) {
  commute_plot %>%
    htmlwidgets::prependContent(viewport) %>%
    htmlwidgets::saveWidget(file_path)
}

nsw <- st_read("nsw_lga.shp")
localities <- nsw[nsw$ABB_NAME %in% councils,]
metro <- localities$geometry
#plot(metro, main = "Sydney")


#metro_grid <- sf::st_make_grid(metro, cellsize = 0.1, square = FALSE)[metro] %>% plot()
metro_grid <- sf::st_make_grid(metro, cellsize = 0.0025, square = FALSE)[metro]
#plot(metro_grid)

target <- targets$chatswood
chatswood_commute <- commute_facts(metro_grid, target$latlng, target$arrival, target$mode)
save(chatswood_commute, file = "chatswood.RData")

plot <- plot_commutes(chatswood_commute, NULL, target$latlng, min_value = NULL, max_value = 15 * 60)
save_commute_plot(plot, "chatswood.html")
