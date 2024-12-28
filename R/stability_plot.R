#' stability plot function
#'
#' Creates a custom designed visualisation of different metrics.
#'
#' This function creates a custom designed visualisation of the metric on
#' stabilisation of the model.
#'
#'
#'
#' @param a Metric on top of the central circle. For the stability plot this
#' represents the overall upwards lift.
#' @param b Metric below the central circle. For the stability plot this
#' represents the overall downwards lift.
#' @param c Metric on the horizontal axis (+/-). For the stability plot this
#' represents the overall bias.
#' @param percentage Percent stability, noted as mark in the circle.

#'
#' @author Yves R. Sagaert
#'
#'
#' @return A visualisation plot
#' @export
#'
#' @examples
#'   \dontrun{
#'      stability_plot(a=3, b=6, c=10, percentage=25)
#'   }
#'
#'

stability_plot <- function(a, b, c, percentage) {
  # Create a blank ggplot
  p <- ggplot2::ggplot() +

    # Draw the T on top with length a
    ggplot2::geom_segment(aes(x = 0, y = 0.5, xend = 0, yend = 0.5 + a), color = "black") +
    ggplot2::geom_segment(aes(x = -0.1 * max(a, b) / 50, y = 0.5 + a, xend = 0.1 * max(a, b) / 50, yend = 0.5 + a), color = "black") +
    # Draw the upside-down T with length b
    ggplot2::geom_segment(aes(x = 0, y = -0.5, xend = 0, yend = -0.5 - b), color = "black") +
    ggplot2::geom_segment(aes(x = -0.1 * max(a, b) / 50, y = -0.5 - b, xend = 0.1 * max(a, b) / 50, yend = -0.5 - b), color = "black") +
    # Draw the bar to the right or left of the circle with length c
    ggplot2::geom_segment(aes(x = 0.5, y = 0, xend = 0.5 + c, yend = 0), color = "red", data = data.frame(c = c[c >= 0])) +
    ggplot2::geom_segment(aes(x = -0.5, y = 0, xend = -0.5 + c, yend = 0), color = "red", data = data.frame(c = c[c < 0])) +
    # Set limits and aspect ratio
    ggplot2::coord_fixed(xlim = c(-max(abs(c), 1), max(abs(c), 1)), ylim = c(-max(a, b, 1), max(a, b, 1))) +
    # geom_point(aes(x = 0, y = 0), shape = 21, size = 50, color = "blue", fill = "white") +
    # Draw the circle
    ggplot2::geom_point(aes(x = 0, y = 0), shape = 21, size = 50, color = "blue", fill = "white") +
    # Add the percentage text
    ggplot2::annotate("text", x = 0, y = 0, label = paste0(percentage, "%"), size = 6, color = "black") +
    ggplot2::annotate("text", x = 0.1*c, y =  a / 2, label = a, size = 4, hjust = 0) +
    ggplot2::annotate("text", x = 0.1*c, y = - b / 2, label = b, size = 4, hjust = 0) +
    ggplot2::annotate("text", x =  0.25*c + c / 2, y = 0.03*a, label = c, size = 4, vjust = 0, data = data.frame(c = c[c >= 0])) +
    # ggplot2::annotate("text", x =  c / 2, y = 0.1, label = c, size = 4, vjust = 0, data = data.frame(c = c[c < 0])) +
    # Hide axes
    ggplot2::theme_void()

  # Print the plot
  print(p)
}
