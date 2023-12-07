library(tidyverse)
library(usmap)
library(sf)
library(tidycensus) # For FIPS codes
#library(esquisse)

aliens_raw <- read.csv("~/Datasets/aliens_scrubbed.csv")

## Clean Data
which(!grepl('^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$', 
             aliens_raw[["duration..seconds."]]))
# aliens_raw[c(27823, 35693, 41202, 58592),]
# Fix seconds row in the above columns
aliens_raw[["duration..seconds."]] <- trimws(gsub("`", "", 
                                        aliens_raw[["duration..seconds."]]))
# Remove extra character from latitude
# aliens_raw[43783,]
aliens_raw[["latitude"]] <- trimws(gsub("[^0-9.-]", "", 
                                        aliens_raw[["latitude"]]))
aliens <- aliens_raw |> mutate(datetime = as_datetime(datetime, 
                                                      format = "%m/%d/%Y %H:%M"),
                               duration_sec = as.numeric(duration..seconds.),
                               date.posted = as.Date(date.posted, 
                                                     format = "%m/%d/%Y"),
                               latitude = as.numeric(latitude)) |>
    rename(duration_min=duration..hours.min.,'date_posted'='date.posted') |>
    select(-duration..seconds.) |>
    relocate(duration_sec, .before = duration_min)

## Data exploration
#glimpse(aliens)
#esquisse::esquisser(aliens)

# Just the 'merician aliens
aliens_us <- aliens |> filter(country == 'us')

## Map by state
aliens_by_state <- aliens_us |>
    group_by(state) |> 
    count(state)

plot_usmap(data = aliens_by_state, values = "n") +
    scale_fill_continuous(name = "UFO Sightings Between 1910-2015") +
    theme(legend.position = "right")

## Map by county
## Return a dictionary of lat/long coordinates and corresponding county
coords_to_county <- function(pointsDF, states) {
    # Transform coordinates to account for Alaska/Hawaii
    pts <- usmap_transform(pointsDF, input_names = c("longitude", "latitude"))
    ## Convert points data.frame to an sf POINTS object
    pts <- st_as_sf(pts, coords = 3:4, crs = usmap_crs())
    states <- st_as_sf(states)
    ## Transform spatial data to Mercator coordinate system
    pts <- st_transform(pts, crs = 3857)
    states <- st_transform(states, crs = 3857)
    ## Find names of county intersected by each point
    county_names <- states[["county"]]
    # Sometimes a coordinate will return multiple counties or nothing
    ii <- st_intersects(pts, states)
    ii[lengths(ii) == 0] <- NA
    ii <- asplit(do.call(rbind, ii), 2)
    ii <- as.integer(ii[[1]])
    # Return a the final dictionary of county and coords
    coords <- st_drop_geometry(pts)
    cbind(coords, county_names[ii]) |>
        rename(county = "county_names[ii]") |>
        mutate(county = str_remove_all(tolower(county), "\\b (?:county|parish)$"))
}

# Get county list and create geometry map
state_list <- usmap::us_map('counties') |>
    rename(state = abbr)
state_list <- do.call('rbind', split(state_list, 
                                     paste(state_list$state, 
                                           state_list$county, 
                                           state_list$piece)) |>
                          lapply(function(x) st_sf(county = x$county[1], 
                                                   state = x$state[1],
                                                   st_sfc(st_polygon(
                                                       list(cbind(x$x, x$y)))),
                                                   crs = usmap::usmap_crs())))

# Transform spatial data to Mercator coordinate system and get county
county_dict <- coords_to_county(aliens_us[,c("longitude","latitude")], state_list)
# Merge and append state name to county
aliens_w_county <- merge(aliens_us, county_dict, by = c("longitude","latitude")) 

aliens_w_county <- aliens_w_county |> 
    mutate(full_county = 
        paste(
            tolower(state.name[
                match(
                    toupper(aliens_w_county$state), state.abb)]
            ),
            aliens_w_county$county, sep=",")
        )

# Get count by county
aliens_by_county <- aliens_w_county |>
    group_by(full_county) |> 
    count(full_county) |>
    filter(!grepl('NA', full_county)) # Dropping 144 rows with no matching county

# Get FIPS from tidycensus
data("fips_codes")
fips <- fips_codes |>
    mutate(fips = paste(state_code, county_code, sep = ""),
           full_county = str_remove_all(
               tolower(
                   paste(state_name, county, sep = ",")), 
                                        "\\b (?:county|parish)$")) |>
    select(c(fips, full_county))


# Join fips code to county name
aliens_by_county <- aliens_by_county |>
    left_join(fips, by = "full_county")

# Create map
plot_usmap(regions = "counties", data = aliens_by_county, 
           values = "n") +
    scale_fill_continuous(name = "UFO Sightings Between 1998-2015") +
    theme(legend.position = "right")

## Trying something interactive with a slider for date
library(plotly)
library(rjson)

# Get count by year and county
aliens_by_yearCounty <- aliens_w_county |>
    mutate(year = as.numeric(format(datetime, '%Y'))) |>
    group_by(year, full_county) |> 
    count(year, full_county) |>
    filter(!grepl('NA', full_county)) |>
    left_join(fips, by = "full_county")


url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)
plot_ly() |>
    add_trace(
        type = "choropleth",
        geojson = counties,
        locations = aliens_by_yearCounty$fips,
        z = aliens_by_yearCounty$n,
        zmin = 0,
        zmax = 150,
        frame = aliens_by_yearCounty$year,
        colors = c("#cffccc", "#70f566", "#18da3b", "#2bdb6b", "#39a78e", "#48b9bf")
    ) |>
    colorbar(title = "Number of UFO Sightings", 
             tickvals = c(10, 25, 50, 100, 150)
        ) |>
    layout(title = 'UFO Sightings 1998-2015', geo = g)
