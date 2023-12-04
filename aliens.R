library(tidyverse)
library(usmap)
library(esquisse)


aliens_raw <- read.csv("~/Datasets/aliens_scrubbed.csv")

which(!grepl('^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$',aliens_raw[["duration..seconds."]]))
# aliens_raw[c(27823, 35693, 41202, 58592),]
aliens_raw[["duration..seconds."]] <- trimws(gsub("`", "", aliens_raw[["duration..seconds."]]))


aliens <- aliens_raw |> mutate(datetime = as_datetime(datetime, format = "%m/%d/%Y %H:%M"),
                            duration_sec = as.numeric(duration..seconds.),
                            date.posted = as.Date(date.posted, format = "%m/%d/%Y")) |>
    rename(duration_min=duration..hours.min.,'date_posted'='date.posted') |>
    select(-duration..seconds.) |>
    relocate(duration_sec, .before = duration_min)

#glimpse(aliens)
#esquisse::esquisser(aliens)

aliens_us <- aliens |> filter(country == 'us') |>
    group_by(state) |>
    count(state)

plot_usmap(data = aliens_us, values = "n") +
    scale_fill_continuous(name = "Alien Sightings Between 1998-2015") +
    theme(legend.position = "right")
