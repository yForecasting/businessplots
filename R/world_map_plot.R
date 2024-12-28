#' world map plot
#'
#' Creates a plot of world with color coding.
#'
#' This function creates a plot of a map with different colors for different countries.
#'
#'
#'
#' @param data A dataframe with at least 2 columns: ISO code of the country and
#' a color for the country.
#' @param modelname Label for the plot of the world map
#' @param white Boolean by default set on 'TRUE' to indicate the color of
#' countries with value NA, or not included in data for the score.
#'
#' @author Yves R. Sagaert
#'
#'
#' @return A plot
#' @export
#'
#' @examples
#'   \dontrun{
#'     data <- data.frame(
#'      country_code = c("USA", "CAN", "BRA", "FRA", "DEU", "IND", "CHN"),
#'      score = c(1, 2, 3, 4, 5, 6, 7))
#'     world_map_plot(data)
#'   }
#'

world_map_plot <- function(data, modelname="Model X", white=TRUE){
  # Catch different data formats
  if (ncol(data)==2){
    colnames(data) = c("country_code","score")
  } else if (ncol(data)==3 & colnames(data)[1]=="ISO3" & colnames(data)[2]=="Year"){
    data = data[,c(1,3)]
    colnames(data) = c("country_code","score")
  }

  if (class(data$score)=="character"){
    data <- replace_letters_with_numbers(data, "score")
  }

  # Load world map data
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Merge the data with the world map
  world <- merge(world, data, by.x = "iso_a3", by.y = "country_code", all.x = TRUE)

  # Convert score to factor
  world$score <- factor(world$score, levels = 1:7) # only for scale_fill_manual and does not has desired effect

  # Define the color scale
  color_scale <- c("green", "lightgreen", "yellow", "orange", "orangered", "red", "darkred")

  # Use only the colors present in the country scores
  # color_scale <- color_scale[(unique(df$Finalcategory)[order(unique(df$Finalcategory))])] # only when using scale_fill_gradientn

  if (white){
    # Plot the map (NA = white)
    ggplot2::ggplot(data = world) +
      ggplot2::geom_sf(aes(fill = score), color = "black") +
      # ggplot2::scale_fill_gradientn(colors = color_scale, na.value = "white", guide = "legend") +
      ggplot2::scale_fill_manual(
        values = setNames(color_scale, 1:7),
        breaks = 1:7,
        labels = 1:7,
        na.value = "white",
        guide = ggplot2::guide_legend(override.aes = list(color = "black"))
      ) +
      ggplot2::theme_void() +
      ggplot2::ggtitle(paste("Country risk scores of",modelname)) +
      ggplot2::labs(fill = "Score")
  } else {
    # Plot the map (NA = grey)
    ggplot2::ggplot(data = world) +
      ggplot2::geom_sf(aes(fill = score), color = "black") +
      # ggplot2::scale_fill_gradientn(colors = color_scale, na.value = "grey50", guide = "legend") +
      ggplot2::scale_fill_manual(
        values = setNames(color_scale, 1:7),
        breaks = 1:7,
        labels = 1:7,
        na.value = "grey50",
        guide = ggplot2::guide_legend(override.aes = list(color = "black"))
      ) +
      ggplot2::theme_void() +
      ggplot2::ggtitle(paste("Country risk scores of",modelname)) +
      ggplot2::labs(fill = "Score")
  }
}
