# This function will produce a horizontal bar chart, with up to four clusters.
# It takes three required arguments:
# data - The data frame to be used to plot the figure
# x1 - the column from the data frame that will be on the x axis
# y - the column from the data frame that will be on the y axis
#
# There are also seven optional arguments:
# x2 - the column from the data frame that will form the second cluster
# x3 - the column from the data frame that will form the third cluster
# x4 - the column from the data frame that will form the fourth cluster
# x1Name - If x1 requires a label that is different to the variable name it can be set here
# x2Name - If x2 requires a label that is different to the variable name it can be set here
# x3Name - If x3 requires a label that is different to the variable name it can be set here
# x4Name - If x4 requires a label that is different to the variable name it can be set here


horizontalBar <- function(data,
                          x1, x2 = NA, x3 = NA, x4 = NA,
                          x1Name = x1, x2Name = x2, x3Name = x3, x4Name = x4,
                          y) {
  
  plot_ly(data,  # The data
          x = data[[x1]],  # The first cluster
          y = data[[y]],  # The y axis value
          name = x1Name,  # The first legend entry
          type = "bar",  # Set type to bar
          marker = list(color = "#d0cece",
                        line = list(color = "#002469", width = 1)),  # Set bar colour and line colour
          text = round2(data[[x1]]),
          textposition = "outside",
          textangle = 0,
          hoverinfo = "y+text",
          hovertext = paste0(x1Name, ": ", data[[x1]], "%")) %>%
    add_trace(x = data[[x2]],
              name = x2Name,
              marker = list(color = "#002469",
                            line = list(color = "#426bba")),
              text = round2(data[[x2]]),
              hovertext = paste0(x2Name, ": ", data[[x2]], "%")) %>%
    add_trace(x = data[[x3]],
              name = x3Name,
              marker = list(color = "#c4db0d"),
              text = round2(data[[x3]]),
              hovertext = paste0(x3Name, ": ", data[[x3]], "%")) %>%
    add_trace(x = data[[x4]],
              name = x4Name,
              marker = list(color = "#426bba"),
              text = round2(data[[x4]]),
              hovertext = paste0(x4Name, ": ", data[[x4]], "%")) %>%
    layout(xaxis = list(title = ""),
           yaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray = rev(data[[y]])),
           font = list(family = "Helvetica", size = 14),
           legend = list(orientation = "h",
                         x = 0.5, xanchor =  "center",
                         y = -0.1, yanchor = "top",
                         traceorder = "reversed"),
           hovermode = "y unified") %>%
    config(displaylogo = FALSE)
  
}