# This function will produce a clustered bar chart with two clusters and a line chart on top.
# It takes five required arguments:
#
# data - The data frame to be used to plot the figure
# x - the column from the data frame that will be on the x axis
# y1 - the column from the data frame that will form the first cluster
# y2 - the column from the data frame that will form the second cluster
# line - the column from the data frame that will be plotted as a line
#
# There are also three optional arguments:
# y1Name - If y1 requires a label that is different to the variable name it can be set here
# y2Name - If y2 requires a label that is different to the variable name it can be set here
# lineName - If line requires a label that is different to the variable name it can be set here

clusteredBarWithLine <- function(data, x, y1, y2, line, y1Name = y1, y2Name = y2, lineName = line) {
  
  plot_ly(data,   # The data frame
          x = data[[x]],  # x axis value
          y = data[[y1]],  # The first cluster value on the y axis
          name = y1Name,  # Legend entry for first cluster
          type = "bar",   # Set plotly type to bar
          marker = list(color = "#002469"),   # Colour of bars
          text = round2(data[[y1]]),  # Text to display on bar
          textposition = "inside", # Position of text
          insidetextanchor = "start", # Placement within the bar
          hovertext = paste(y1Name, ":", round2(data[[y1]])),
          hoverinfo = "x+text"
          #width = 1
          ) %>%   # Space between bars
    add_trace(y = data[[y2]],    # The first cluster value on the y axis
              name = y2Name,   # The legend entry
              marker = list(color = "#426bba"),  # Change bar colour
              text = round2(data[[y2]]),
              hovertext = paste(y2Name, ":", round2(data[[y2]]))
              ) %>%   # Text to display on bar
    add_trace(y = data[[line]],   # The data for the line
              name = lineName,   # Legend entry for line
              text = paste0(round2(data[[line]]), "\n"),  # Text to display on line points
              type = "scatter",  # Change type to scatter
              mode = "lines+markers+text",  # Specify we want lines and points
              line = list(color = "#8A9B08", width = 4),  # Line options
              marker = list(color = "#8A9B08", size = 8),  # Marker options
              textfont = list(color = "#8A9B08"),
              hovertext = paste(line, ":", round2(data[[line]])),
              textposition = "top center") %>%  # Position of text labels above points
    layout(legend = list(orientation = "h",
                         x = 0.5,
                         y = 1,
                         xanchor = "center",
                         yanchor = "top"),      # Position legend
           xaxis = list(title = "",  # Remove x axis label
                        tickangle = 0,  # Maintain horizontal text. Accessibility requirement
                        autotick = FALSE),   # Ensure all values on x axis are displayed
           yaxis = list(title = "",   # Remove y axis label
                        range = list(0, 100), # Ensure axis goes from 0% to 100%
                        showline = TRUE, # Ensures that a line for the y axis is displayed
                        linewidth = 1), # Sets width of y axis line
           font = list(family = "Helvetica", size = 14)) %>%  # Match font to rest of report
    config(displaylogo = FALSE)  # Remove plotly logo.
  
}