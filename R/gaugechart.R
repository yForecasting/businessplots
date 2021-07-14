  #' gauge chart function
  #'
  #' Creates a gauge chart of an integer or a numeric value.
  #'
  #' This function creates a gauge chart based on an integer or a numeric value.
  #'
  #' @param gauge_value An integer or a numeric value
  #' @param max_value An integer or a numeric value
  #' @param type The type of the gauge chart
  #' @param mode The mode of the gauge chart
  #' @param title The title of the gauge chart
  #' @param title_size The size of the title of the gauge chart
  #' @param axis_tick_width The tick width of the axis of the gauge chart
  #' @param border_width The border width of the gauge chart
  #' @param threshold_width The threshold width
  #' @param threshold_thickness The thickness of the threshold
  #' @param font_family The font family of the text in the gauge chart
  #' @param primary_color The primary color
  #' @param secondary_color The secondary color
  #' @param tertiary_color The tertiary color
  #' @param quaternary_color The quaternary color
  #' @param quinary_color The quinary color
  #' @param senary_color The senary color
  #'
  #' @author Ruben Vanhecke
  #'
  #' at import ggplot2
  #' at import plotly
  #'
  #' @return A gauge chart
  #' @export
  #'
  #' @examples
  #'   \dontrun{
  #'      gaugechart(25, 100, 
  #' "indicator", "gauge+number", 
  #' "Visitors inside", 
  #' 24, 1, 2, 4, 0.75, 
  #' "Arial",
  #'        
  #' "#004D9A", "#002142", "#0069D2", 
  #' "#0180FF", "#4FA7FF", 
  #' "#A7D3FF")
  #'   }
  #'
  #'
  #'

  # plot gauge chart
  gaugechart <- function(gauge_value, max_value, type, mode, title, title_size, axis_tick_width, border_width, threshold_width, threshold_thickness, font_family,
                         primary_color, secondary_color, tertiary_color, quaternary_color, quinary_color, senary_color){
    # gauge_value is an integer or a numeric value
    # max_value is an integer or a numeric value
    # type is the type of the gauge chart
    # mode is the mode of the gauge chart
    # title is the title of the gauge chart
    # title_size is the size of the title of the gauge chart
    # axis_tick_width is the tick width of the axis of the gauge chart
    # border_width is the border width of the gauge chart
    # threshold_width is the threshold width
    # threshold_thickness is the thickness of the threshold
    # font_family is the font family of the text in the gauge chart
    # primary_color is the primary color
    # secondary_color is the secondary color
    # tertiary_color is the tertiary color
    # quaternary_color is the quaternary color
    # quinary_color is the quinary color
    # senary_color is the senary color


    # library
    library(plotly)

    # create gauge chart
    fig <- plotly::plot_ly(
      type = type,
      mode = mode,
      value = gauge_value,
      title = list(text = title, font = list(size = title_size)),
      gauge = list(
        axis = list(range = list(NULL, max_value), tickwidth = axis_tick_width, tickcolor = primary_color),
        bar = list(color = senary_color),
        bgcolor = tertiary_color,
        borderwidth = 2,
        bordercolor = border_width,
        threshold = list(
          line = list(color = quaternary_color, width = threshold_width),
          thickness = threshold_thickness,
          value = max_value / 2)))
    fig <- fig %>%
      layout(
        margin = list(l=20,r=30),
        font = list(color = secondary_color, family = font_family))

    # plot gauge chart
    fig
  }
