
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

# Calculate stability of model year-over-year
m_last_year <- sqldf("SELECT ISO3,
      Year+1 AS Year, Finalcategory
      FROM test_model")
ml=replace_letters_with_numbers(m_last_year,"Finalcategory")
m_stability <- sqldf("SELECT m.ISO3 AS ISO3, m.Year AS Year, m.Finalcategory AS Category_model,
                ml.ISO3 AS mlISO3, ml.Year AS mlYear, ml.Finalcategory AS Category_model2
         FROM m LEFT JOIN ml on m.ISO3=ml.ISO3 and m.Year=ml.Year
                     wHERE m.Year > 2015")
# tail(m_stability)
# stability measures

stability_differences=sum(abs(m_stability$Category_model - m_stability$Category_model2)>0,na.rm=TRUE)
stability_differences_up=sum((m_stability$Category_model - m_stability$Category_model2)>0,na.rm=TRUE)
stability_differences_down=sum((m_stability$Category_model - m_stability$Category_model2)<0,na.rm=TRUE)
# Total scores to compare:
stability_differences_total=sum(!(is.na(m_stability$Category_model - m_stability$Category_model2)),na.rm=TRUE)
# Size of different scores in total:
stability_differences_size=sum(m_stability$Category_model - m_stability$Category_model2,na.rm=TRUE)


# Generate the plot
stability_plot(stability_differences_up, stability_differences_down, stability_differences_size,
            round(stability_differences/stability_differences_total*100,0))
