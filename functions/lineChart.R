lineChart <- function(data, x,
                      y1, y2 = NA, y3 = NA, y4 = NA, y5 = NA, y6 = NA,
                      y1Name = y1, y2Name = y2, y3Name = y3, y4Name = y4, y5Name = y5, y6Name = y6,
                      title = NA,
                      yHeight = 100) {
  
  
  line <-
  # Plot first line of graph:
  plot_ly(data,       # Data source for figure
          x = data[[x]],   # x axis variable
          y = data[[y1]], # y axis variable
          name = y1Name,   # legend label for this line
          line = list(color = "#426bba", width = 4),  # colour and width of this line
          type = "scatter",   # to create a line graph we first choose scatter as the type
          hovertext = paste(y1Name, ":", round2(data[[y1]])),
          hoverinfo = "x+text",
          connectgaps = TRUE,
          mode = "lines")  # and set the mode to lines
  
  
  if (!is.na(y2)) {
  
    
    if (y2 == "young") {
      
      data <- data %>%
        mutate(dash1 = case_when(year %in% c(2019, 2020, 2021) ~ young,
                                 TRUE ~ as.numeric(NA)), # dash1 is dashed line (between 19-20)
               dash2 = case_when(year > 2019 ~ as.numeric(NA),
                                 TRUE ~ young), # dash2 is first solid line (up to 19)
               dash3 = case_when(year < 2021 ~ as.numeric(NA),
                                 TRUE ~ young), # dash3 is second solid line (21 onwards)
               hover_text = case_when(year == 2020 ~ "No YLT survey ran",
                                      TRUE ~ paste(y2Name, ":", round2(young))))
      
      line <- line %>%
        
      # Add first part of solid line to same graph:
      add_trace(y = data[["dash2"]],   # y axis variable, no need to add new x axis
                name = y2Name,   # legend label
                hovertext = data[["hover_text"]],
                line = list(color = "#8A9B08"))  %>%    # Change colour, line width will be same as above
      # Add second part of solid line to same graph:
      add_trace(y = data[["dash3"]],   # y axis variable, no need to add new x axis
                name = y2Name,   # legend label
                hovertext = data[["hover_text"]],
                line = list(color = "#8A9B08"),
                showlegend = FALSE)  %>%    # Change colour, line width will be same as above
      # Add dashed line to same graph:
      add_trace(y = data[["dash1"]],   # y axis variable, no need to add new x axis
                name = y2Name,   # legend label
                hoverinfo = "none",
                line = list(color = "#8A9B08",
                            dash = "dash"),
                showlegend = FALSE)  # Change colour, line width will be same as above
      
    } else {
    
      line <- line %>%
    
        # Add second line to same graph:
        add_trace(y = data[[y2]],   # y axis variable, no need to add new x axis
                  name = y2Name,   # legend label
                  hovertext = paste(y2Name, ":", round2(data[[y2]])),
                  line = list(color = "#8A9B08"))      # Change colour, line width will be same as above
    
    }
    
  }
  
  if (!is.na(y3)) {
    
    line <- line %>%
      
      # Add third line to same graph:
      add_trace(y = data[[y3]],   # y axis variable, no need to add new x axis
                name = y3Name,   # legend label
                hovertext = paste(y3Name, ":", round2(data[[y3]])),
                line = list(color = "#9A9393",
                            dash = "dash"))      # Change colour, line width will be same as above
    
  }
  
  if (!is.na(y4)) {
    
    line <- line %>%
      
      # Add fourth line to same graph:
      add_trace(y = data[[y4]],   # y axis variable, no need to add new x axis
                name = y4Name,   # legend label
                hovertext = paste(y4Name, ":", round2(data[[y4]])),
                line = list(color = "#002469"))      # Change colour, line width will be same as above
    
  }
  
  if (!is.na(y5)) {
    
    line <- line %>%
      
      # Add fifth line to same graph:
      add_trace(y = data[[y5]],   # y axis variable, no need to add new x axis
                name = y5Name,   # legend label
                hovertext = paste(y5Name, ":", round2(data[[y5]])),
                line = list(color = "#69A343"))      # Change colour, line width will be same as above
    
  }
  
  if (!is.na(y6)) {
    
    line <- line %>%
      
      # Add sixth line to same graph:
      add_trace(y = data[[y6]],   # y axis variable, no need to add new x axis
                name = y6Name,   # legend label
                hovertext = paste(y6Name, ":", round2(data[[y6]])),
                line = list(color = "#5999D4"))      # Change colour, line width will be same as above
    
  }
  
  
  tickVals <- if (yHeight <= 100) {
    seq(20, yHeight, 20)
  } else if (yHeight < 1000) {
    seq(100, yHeight, 100)
  } else {
    seq(200, yHeight, 200)
  }
  
  line <- line %>%
   
    # Start styling the graph:
    layout(legend = list(orientation = "h",    # Here the legend is changed to horizontal 
                         yanchor = "top",     # And placed just under the x axis
                         y = -0.1,
                         xanchor = "center",     # And centred
                         x = 0.5),
           xaxis = list(title = "",               # Remove title and gridlines from x axis
                        showgrid = FALSE,
                        tickvals = data[[x]],
                        tickmode = "array",
                        linewidth = 1,
                        tickangle = 0),
           yaxis = list(title = "",               # Remove title from y axis and extend range from 0 to 100
                        range = list(0, yHeight),
                        tickmode = "array",
                        tickvals = tickVals,
                        showline = TRUE,
                        linewidth = 1,
                        tickformat = ",d"),
           hovermode = "x unified", # Adds hover label
           font = list(family = "Helvetica", size = 14)) %>%
    config(displaylogo = FALSE)
  
  if (!is.na(title)) {
    line <- line %>%
      layout(title = title,
             margin = list(t = 50))
  }
  
  
  line
  
}