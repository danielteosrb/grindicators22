stackedBarSplit <- function(data, data2,
                            x,
                            y1, y2,
                            y1Name = y1, y2Name = y2,
                            legend = TRUE,
                            title1, title2) {
  
  # Add an invisble character to the end of the first column in data2 to prevent items being grouped on the x axis.
  data2[[1]] <- paste0(data2[[1]], "\n")
  
  # Bind two data frames together
  data <- rbind(data, data2)
  
  stacked <-
    # Use plot_ly() to create the first bar in each stack
    plot_ly(data, # The data
            x = data[[x]], # x axis value
            y = data[[y1]], # First bar
            type = "bar", # Set plotly type to bar
            marker = list(color = "#426bba",
                          line = list(color = "#002469", width = 2)),  # Set bar colour and line options
            name = y1Name, # Name of first bar
            text = paste0("<b>", round(data[[y1]]), "</b>"), # text to display on bar
            textposition = "auto", # Text position inside/outside bar
            insidetextanchor = "middle",  # Text placed inside bar is middle aligned
            textangle = 0,  # No rotation of text
            showlegend = legend,
            hoverinfo = "x+text",
            hovertext = paste0(y1Name, ": ", data[[y1]], "%")) %>%  # Turn legend on/off
    # Add second bar to stacks using add_trace()
    add_trace(y = data[[y2]], # Second bar
              marker = list(color = "#c4db0d"), # Change bar colour
              name = y2Name, # Name of bar
              text = paste0("<b>", round(data[[y2]]), "</b>"),
              hovertext = paste0(y2Name, ": ", data[[y2]], "%")) %>% # Text to display
    # Set display options using layout()
    layout(barmode = "stack", # Set mode to stacked bar
           xaxis = list(title = "",  # Remove label
                        categoryorder = "array",  # How to order the axis
                        categoryarray = data[[x]], # Based on order of x column
                        tickangle = 0), # No rotation of text
           yaxis = list(title = "", # Remove label
                        range = list(0, 100), # Axis always displays 0-100
                        showline = TRUE, # Ensures that a line for the y axis is displayed 
                        linewidth = 1),  # Sets width of y axis line    
           legend = list(x = 0.5, xanchor = "center", 
                         y = -0.1, yanchor = "top", # Legend position 
                         orientation = "h"),  # Horizontal legend
           font = list(family = "Helvetica", size = 14),  # Text options
           bargap = 0.5,  # Space between bars
           annotations = list(list(text = title1, showarrow = FALSE,
                                   x = 0.25, xanchor = "center", xref = "paper",
                                   y = 95, yanchor = "middle", yref = "y"),
                              list(text = title2, showarrow = FALSE,
                                   x = 0.75, xanchor = "center", xref = "paper",
                                   y = 95, yanchor = "middle", yref = "y")),  # Add individual titles over plot
           shapes = list(type = line,
                         x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1, xref = "paper", yref = "paper",
                         line = list(width = 0.5))) %>%  # Adds line down the middle
    config(displaylogo = FALSE)  # Removes plotly watermark
  
  
  stacked
  
}