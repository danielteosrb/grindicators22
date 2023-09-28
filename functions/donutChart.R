# This function produces a donut chart.
# It takes three required arguments:

# data - the data frame in use
# values - the variable which will determine the size of the sections
# sections - the variable which will label the sections

# It also has two optional arguments:

# colours - a vector containing colours for the sections. Generic ones have been set
# title - A title to appear above the chart. By default it is the current year

donutChart <- function(data,
                       values,
                       sections,
                       colours = c("#d0cece", "#002469", "#c4db0d", "#426bba"),
                       title = NILTyear) {
  
  tot <- sum(data[[values]], na.rm = TRUE)
  
  # Create plot:
  plot_ly(data,  # The data
          values = data[[values]],  # The values
          labels = data[[sections]],  # The labels
          type = "pie",  # Set type to pie
          textinfo = "text",   # Set the chart to display the content of "text"
          text = paste0("<b>", round2(data[[values]]), "</b>"), # Define the text
          hovertext = paste0(round2(data[[values]] / tot * 100), "%"),
          hoverinfo = "label+text",
          textfont = list(font = "Helvetica", size = 16), # Font size
          insidetextorientation = "horizontal", # Make text horizontal
          marker = list(colors = colours,  # Set colours of segments
                        line = list(color = "#002469", width = 2)),  # Line colour of segments
          sort = FALSE,  # Don't apply any sorting
          hole = 0.5) %>%  # Convert piechart to donut chart
    layout(legend = list(x = 0.5,
                         xanchor = "center",
                         y = -0.1,
                         yanchor = "bottom",
                         orientation = "h"),   # Position of legend
           margin = list(b = 50),  # Add more white space to bottom of chart to accomodate legend
           font = list(family = "Helvetica", size = 14),  # font properties
           annotations = list(
             list(text = title, showarrow = FALSE,
                  font = list(size = 20),
                  x = 0.5, xref = "paper", xanchor = "center",
                  y = 1, yref = "paper", yanchor = "top")   # add a title
           )) %>%
    config(displaylogo = FALSE)  # Remove plotly logo
  
}