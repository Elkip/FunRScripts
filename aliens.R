library(tidyverse)
library(usmap)
library(sf)
#library(esquisse)

aliens_raw <- read.csv("~/Datasets/aliens_scrubbed.csv")

## Clean Data
which(!grepl('^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$', aliens_raw[["duration..seconds."]]))
# aliens_raw[c(27823, 35693, 41202, 58592),]
aliens_raw[["duration..seconds."]] <- trimws(gsub("`", "", 
                                                  aliens_raw[["duration..seconds."]]))

aliens <- aliens_raw |> mutate(datetime = as_datetime(datetime, 
                                                      format = "%m/%d/%Y %H:%M"),
                            duration_sec = as.numeric(duration..seconds.),
                            date.posted = as.Date(date.posted, 
                                                  format = "%m/%d/%Y")) |>
    rename(duration_min=duration..hours.min.,'date_posted'='date.posted') |>
    select(-duration..seconds.) |>
    relocate(duration_sec, .before = duration_min)

## Data exploration
#glimpse(aliens)
#esquisse::esquisser(aliens)

## Map by state
aliens_us <- aliens |> filter(country == 'us') 

aliens_by_state <- aliens_us |>
    group_by(state) |> 
    count(state)

plot_usmap(data = aliens_by_state, values = "n") +
    scale_fill_continuous(name = "Alien Sightings Between 1998-2015") +
    theme(legend.position = "right")

## Convert Lat/Long to county using Spatial Points library (sp)
coords_to_county <- function(pointsDF,
                            states = map('county', 
                                         fill=TRUE, 
                                         col="transparent", 
                                         plot=FALSE),
                            name_col = "ID") {
    ## Convert points data.frame to an sf POINTS object
    pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
    states <- st_as_sf(states)
    ## Transform spatial data to Mercator coordinate system
    states <- st_transform(states, crs = 3857)
    pts <- st_transform(pts, crs = 3857)
    ## Find names of county intersected by each point
    state_names <- states[[name_col]]
    ii <- as.integer(st_intersects(pts, states))
    state_names[ii]
}

# Transform spatial data to Mercator coordinate system
aliens_us[["county"]] <- coords_to_county(aliens_us[,c("longitude","latitude")])

# Get count by county
aliens_by_county <- aliens_us |> filter(country == 'us') |>
    group_by(county) |> 
    count(county)

# Get FIPS
fips <- maps::county.fips |>
    as_tibble() |>
    rename(county=polyname)

# Join fips code to county name
aliens_by_county <- aliens_by_county |>
    left_join(fips, by = "county")

# Create map
plot_usmap(regions = "counties", data = aliens_by_county, values = "n") +
    scale_fill_continuous(name = "Alien Sightings Between 1998-2015") +
    theme(legend.position = "right")
