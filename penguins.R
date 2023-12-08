library(palmerpenguins)
library(data.table)
library(dplyr)
library(dtplyr)
library(networkD3)

data("penguins")
penguins <- data.table(penguins)
glimpse(penguins)
unique(penguins$species)

penguins[species == "Chinstrap" & sex == "male", .(island, year)]

cols <- c("island", "year")
penguins[species == "Chinstrap" & sex == "male", ..cols]

penguins[species == "Chinstrap" & year == 2007, table(island)]

penguins[, sum(body_mass_g > 5000, na.rm = T)]
penguins[body_mass_g > 5000, .(count = .N, mean_g = mean(body_mass_g))]

## Create a Sankey Diagram of the species living on each Island
links <- penguins[, .N, by = .(island, species)]
nodes <- data.frame(
    name=c(as.character(penguins$island), 
           as.character(penguins$species)) %>% unique()
)
links$IDsource <- match(links$island, nodes$name)-1 
links$IDtarget <- match(links$species, nodes$name)-1
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "N", NodeID = "name", 
              sinksRight=FALSE)
