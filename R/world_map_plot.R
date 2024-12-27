# install.packages("ggplot2")
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")

# library(ggplot2)
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
#
# # Example dataframe
# data <- data.frame(
#   country_code = c("USA", "CAN", "BRA", "FRA", "DEU", "IND", "CHN"),
#   score = c(1, 2, 3, 4, 5, 6, 7)
# )
#
# # Load world map data
# world <- ne_countries(scale = "medium", returnclass = "sf")
#
# # Merge the data with the world map
# world <- merge(world, data, by.x = "iso_a3", by.y = "country_code", all.x = TRUE)
#
# # Define the color scale
# color_scale <- c("green", "lightgreen", "yellow", "orange", "orangered", "red", "darkred")
#
# # Plot the map (NA = grey)
# ggplot(data = world) +
#   geom_sf(aes(fill = score), color = "black") +
#   scale_fill_gradientn(colors = color_scale, na.value = "grey50", guide = "legend") +
#   theme_void() +
#   labs(fill = "Score")
#
# # Plot the map (NA = white)
# ggplot(data = world) +
#   geom_sf(aes(fill = score), color = "black") +
#   scale_fill_gradientn(colors = color_scale, na.value = "white", guide = "legend") +
#   theme_void() +
#   labs(fill = "Score")

# Load the map plot function
# source("world_map_plot.R")
