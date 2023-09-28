# This function will produce a clustered bar chart with two or three clusters and takes four required arguments:
#
# data - The data frame to be used to plot the figure
# x - the column from the data frame that will be on the x axis
# y1 - the column from the data frame that will form the first cluster
# y2 - the column from the data frame that will form the second cluster
#
# There are also six optional arguments:
# y3 - the column from the data frame that will form the third cluster
# y1Name - If y1 requires a label that is different to the variable name it can be set here
# y2Name - If y2 requires a label that is different to the variable name it can be set here
# y3Name - If y3 requires a label that is different to the variable name it can be set here
# title - If graph requires its own title enter here
# legend - default value of "top". Set to "bottom" to move legend position


clusteredBar <- function(data,
                         x,
                         y1,
                         y2,
                         y3 = NA,
                         y1Name = y1,
                         y2Name = y2,
                         y3Name = y3,
                         title = NA,
                         legend = "top") {
  
  
  # Create a bar chart with two clusters y1 and y2
  cluster <-
  plot_ly(data,   # The data
          x = data[[x]],  # The x axis
          y = data[[y1]], # The first cluster, on the y axis
          name = y1Name,  # Legend entry for first cluster
          type = "bar",    # Sets type to bar
          text = paste0("<b>", round2(data[[y1]]), "</b>"),  # Text to appear on bar
          textposition = "inside",   # Text placed inside bar
          insidetextanchor = "middle", # Text centred inside bar
          textangle = 0,   # Text horizontal. Accessibility requirement.
          marker = list(color = "#426bba",
                        line = list(color = "#002469", width = 2)), # Color of bars for first cluster set
          hoverinfo = "x+text",
          hovertext = paste0(y1Name, ": ", data[[y1]], "%")) %>%  
    add_trace(y = data[[y2]],   # Second cluster variable
              name = y2Name,   # Legend entry for second cluster
              marker = list(color = "#c4db0d"),   # Bar colour changed
              text = paste0("<b>", round2(data[[y2]]), "</b>"),
              hovertext = paste0(y2Name, ": ", data[[y2]], "%"))  # Text on bar set
  
  # If y3 is defined it will be added here
  if(!is.na(y3)) {
    
    cluster <- cluster %>%
    add_trace(y = data[[y3]],  # Third cluster variable
              name = y3Name,   # Legend entry for third  cluster
              marker = list(color = "#002469"),  # Bar colour changed
              text = paste0("<b>", round2(data[[y3]]), "</b>"),
              hovertext = paste0(y3Name, ": ", data[[y3]], "%"))  # Text on bar set

  }
  
  # Layout options are set
  cluster <- cluster %>%  
  layout(xaxis = list(title = "",   # Remove label from x axis
                        tickangle = 0,  # Set text horizontal. Accessibility requirement
                        categoryorder = "array",  # Type of sorting to perform on x axis
                        categoryarray = data[[x]]),  # Variable to sort on
           yaxis = list(title = "",   # Remove title from y axis
                        range = list(0, 100),  # Ensure y axis covers 0% to 100%
                        showline = TRUE),  # Axis line is visible
           font = list(family = "Helvetica", size = 14))  # Set font size to match rest of document
  
  
  # Legend placed at top or bottom of plot
  if (legend == "top") {
  
    cluster <- cluster %>%
      layout(legend = list(x = 1, y = 1, xanchor = "right"))
  
  } else if (legend == "bottom") {
    
    cluster <- cluster %>%
      layout(legend = list(orientation = "h",
                          x = 0.5, xanchor = "center",
                          y = -0.25, yanchor = "top"))
    
  }
  
  # Title added and plot area increased if needed
  if (!is.na(title)) {
    cluster <- cluster %>%
      layout(title = title,
             margin = list(t = 50))
  }
  
  # Remove the plotly logo
  cluster %>%
    config(displaylogo = FALSE)
  
}