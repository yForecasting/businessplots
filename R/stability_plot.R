
library(ggplot2)
library(grid)

# Function to create the stability plot
stability_plot <- function(a, b, c, percentage) {
  # Create a blank ggplot
  p <- ggplot() +
   
    # Draw the T on top with length a
    geom_segment(aes(x = 0, y = 0.5, xend = 0, yend = 0.5 + a), color = "black") +
    geom_segment(aes(x = -0.1 * max(a, b) / 50, y = 0.5 + a, xend = 0.1 * max(a, b) / 50, yend = 0.5 + a), color = "black") +
    # Draw the upside-down T with length b
    geom_segment(aes(x = 0, y = -0.5, xend = 0, yend = -0.5 - b), color = "black") +
    geom_segment(aes(x = -0.1 * max(a, b) / 50, y = -0.5 - b, xend = 0.1 * max(a, b) / 50, yend = -0.5 - b), color = "black") +
    # Draw the bar to the right or left of the circle with length c
    geom_segment(aes(x = 0.5, y = 0, xend = 0.5 + c, yend = 0), color = "red", data = data.frame(c = c[c >= 0])) +
    geom_segment(aes(x = -0.5, y = 0, xend = -0.5 + c, yend = 0), color = "red", data = data.frame(c = c[c < 0])) +
    # Set limits and aspect ratio
    coord_fixed(xlim = c(-max(abs(c), 1), max(abs(c), 1)), ylim = c(-max(a, b, 1), max(a, b, 1))) +
    # geom_point(aes(x = 0, y = 0), shape = 21, size = 50, color = "blue", fill = "white") +
    # Draw the circle
    geom_point(aes(x = 0, y = 0), shape = 21, size = 50, color = "blue", fill = "white") +
    # Add the percentage text
    annotate("text", x = 0, y = 0, label = paste0(percentage, "%"), size = 6, color = "black") +
    # Hide axes
    theme_void()
 
  # Print the plot
  print(p)
}

# Generate the plot
stability_plot(stability_differences_up, stability_differences_down, stability_differences_size,
            round(stability_differences/stability_differences_total*100,0))
