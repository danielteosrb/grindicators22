stackedBar <- function(data, x,
                       y1, y2, y3 = NULL,
                       y1Name = y1, y2Name = y2, y3Name = y3,
                       legend = TRUE,
                       title = NA) {
  
  stacked <-
    # Use plot_ly() to create the first bar in each stack
    plot_ly(data, # The data frame
            x = data[[x]],  # x axis value
            y = data[[y1]], # First bar
            type = "bar",  # Set plotly type to bar
            marker = list(color = "#426bba",  # Change the bar colour
                          line = list(color = "#002469", width = 2)), # Set the properties of line around bar
            name = y1Name,  # Name of the first bar
            text = paste0("<b>", round2(data[[y1]]), "</b>"), # Format to display text on each bar
            textposition = "auto",  # Auto position text inside/outside bar
            insidetextanchor = "middle",  # For text positioned inside bar, place it in the middle
            textangle = 0,  # No rotation of text
            showlegend = legend,
            hoverinfo = "x+text",
            hovertext = paste0(y1Name, ": ", data[[y1]], "%")) %>%  # Set legend to TRUE/FALSE as above
    # Use add_trace() to add the second bar
    add_trace(y = data[[y2]],
              marker = list(color = "#c4db0d"),
              name = y2Name,
              text = paste0("<b>", round2(data[[y2]]), "</b>"),
              hovertext = paste0(y2Name, ": ", data[[y2]], "%"))
    
  
  if (!is.null(y3)) {
    
    stacked <- stacked %>%
      add_trace(y = data[[y3]],
                marker = list(color = "#002469",
                              line = list(color = "#426bba")),
                name = y3Name,
                text = paste0("<b>", round2(data[[y3]]), "</b>"),
                hovertext = paste0(y3Name, ": ", data[[y3]], "%"))
    
  }
  
  
  stacked <- stacked %>%
    # Use layout() to style the graph
    layout(barmode = "stack",  # Change mode to stacked bar
           xaxis = list(title = "",  # Remove axis labels
                        categoryorder = "array",  # How to order the categories
                        categoryarray = data[[x]], # Which variable to order them on
                        tickangle = 0), # Make sure text doesn't rotate
           yaxis = list(title = "",  # Remove axis label
                        range = list(0, 100), # Y axis always displays 0 to 100
                        showline = TRUE, # Ensures that a line for the y axis is displayed
                        linewidth = 1), # Sets width of y axis line  
           legend = list(x = 0.5, xanchor = "center",  # position of legend
                         y = -0.1, yanchor = "top",  # position of legend
                         orientation = "h"),  # Legend horizontal
           font = list(family = "Helvetica", size = 14),  # Set font size and type
           bargap = 0.5,  # Set white space between bars
           margin = list(l = 0, r = 0)) %>%  # Change white space around the graph
    config(displaylogo = FALSE)  # Remove plotly watermark
  
  
  # Adds a title if one is declared in functin call
  if (!is.na(title)) {
    
    stacked <- stacked %>%
      layout(annotations = list(text = title, showarrow = FALSE,
                                x = 0.5, xanchor = "center", xref = "paper",
                                y = 95, yanchor = "middle", yref = "y"))
    
  }
  
  stacked
  
}