# This function will produce a standard bar chart and takes three required arguments:
# data - The data frame to be used to plot the figure
# x - the column from the data frame that will be on the x axis
# y - the column from the data frame that will be on the y axis
#
# There are also three optional arguments:
# legendItem - Text to be displayed in the legend

barChart <- function (data, x, y, legendItem = NA, chartTitle = NA) {
  
  bar <- plot_ly(data,   # The data
          x = data[[x]], # x axis
          y = data[[y]], # y axis
          type = "bar",  # sets it to bar chart
          marker = list(color = "#426bba",
                        line = list(color = "#002469", width = 2)),  # Sets the colour of bars and their outline
          name = legendItem,                                       # Gives chart a name if legend required
          text = paste0("<b>", round2(data[[y]]), "</b>"),  # The text to appear on each bar
          textposition = "inside",   # Location of text inside bars
          insidetextanchor = "middle",
          hoverinfo = "x+text",
          hovertext = paste0(data[[y]], "%")) %>% # Centres text on bar
      layout(title = list(text = chartTitle, 
                          font = list(family = "Helvetica", size = 14),
                          x = 0.3, xref = "paper", xanchor = "left",
                          y = 0.98, yref = "paper", yanchor = "middle"), # adds chart title if necessary, in smaller font than rest of chart
           xaxis = list(title = "",     # Removes x axis title as this will be in Markdown document
                        tickangle = 0,  # Keeps text horizontal. Accessibility requirement.
                        categoryorder = "array",  # Type of sorting applied to bars
                        categoryarray = data[[x]]), # Sort bars on their x axis value
           yaxis = list(title = "",         # Removes axis title
                        range = list(0, 100),  # Sets axis to always show 0% to 100%
                        showline = TRUE,  # Ensures that a line for the y axis is displayed
                        linewidth = 1),  # sets y axis line width       
           font = list(family = "Helvetica", size = 14),  # Sets text size and font to match that of rest of report
           bargap = 0.5, # Keeps even spacing between bars
           margin = list(b =0))   
  
  
  if (!is.na(legendItem)) {
    
    bar <- bar %>%
      layout(annotations = list(text = legendItem, showarrow = FALSE,
                                x = 0.3, xref = "paper", xanchor = "left",
                                y = 0.85, yref = "paper", yanchor = "middle"),
             shapes = list(type = "rect",
                           x0 = 0.25, x1 = 0.29, xref = "paper", xanchor = "right",
                           y0 = 0.835, y1 = 0.865, yref = "paper", yanchor = "middle",
                           fillcolor = "#426bba",
                           line = list(color = "#002469", width = 2)))
    
  }

  # Removes the Plotly logo from the plot
  bar %>%
    config(displaylogo = FALSE)
  
}