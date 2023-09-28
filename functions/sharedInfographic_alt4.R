sharedInfographic_alt4 <- function() {

  leisure <- (fig7$def + fig7$prob)[fig7$area == "Leisure Centres"]
  park <- (fig7$def + fig7$prob)[fig7$area == "Parks"]
  library <- (fig7$def + fig7$prob)[fig7$area == "Libraries"]
  shopping <- (fig7$def + fig7$prob)[fig7$area == "Shopping Centres"]
  
  workplace <- round2(dataNew$f12_workplace)
  neighbourhood <- round2(dataNew$f12_neighbourhood)
  school <- round2(dataNew$f12_school)
    
  plot_ly(fig7,
          type = "scatter",
          mode = "markers") %>%
    layout(
      xaxis = list(showgrid = FALSE,
                   zeroline = FALSE,
                   visbile = FALSE,
                   showticklabels = FALSE,
                   fixedrange = TRUE),
      
      yaxis = list(showgrid = FALSE,
                   zeroline = FALSE,
                   visible = FALSE,
                   fixedrange = TRUE),
      
      showlegend = FALSE,
      
      images = list(
        list(source = dataURI(file = "images/leisure centre.png"),
             x = 0.1, y = 0.96, sizex = 0.16, sizey = 0.16, xanchor = "centre", yanchor = "top"),
        list(source = dataURI(file = "images/park.png"),
             x = 0.1, y = 0.64, sizex = 0.16, sizey = 0.16, xanchor = "centre", yanchor = "middle"),
        list(source = dataURI(file = "images/library.png"),
             x = 0.1, y = 0.39, sizex = 0.16, sizey = 0.16, xanchor = "centre", yanchor = "middle"),
        list(source = dataURI(file = "images/shopping centre.png"),
                   x = 0.1, y = 0.03, sizex = 0.16, sizey = 0.16, xanchor = "left", yanchor = "bottom"),
        list(NA),
        list(NA)
      ),
      
      shapes = list(
        list(type = "rect",
             x0 = 0.3, x1 = 0.3 + leisure * 0.007,
             y0 = 0.84, y1 = 0.94,
             xref = "paper", yref = "paper",
             fillcolor = "#b0deff",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.3 + leisure * 0.007, x1 = 1,
             y0 = 0.84, y1 = 0.94,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.3, x1 = 0.3 + park * 0.007,
             y0 = 0.59, y1 = 0.69,
             xref = "paper", yref = "paper",
             fillcolor = "#999999",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.3 + park * 0.007, x1 = 1,
             y0 = 0.59, y1 = 0.69,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.3, x1 = 0.3 + library * 0.007,
             y0 = 0.35, y1 = 0.45,
             xref = "paper", yref = "paper",
             fillcolor = "#85afd6",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.3 + library * 0.007, x1 = 1,
             y0 = 0.35, y1 = 0.45,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.3, x1 = 0.3 + shopping * 0.007,
             y0 = 0.08, y1 = 0.18,
             xref = "paper", yref = "paper",
             fillcolor = "#2d4673",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.3 + shopping * 0.007, x1 = 1,
             y0 = 0.08, y1 = 0.18,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(NA),
        list(NA),
        list(NA),
        list(NA)
      ),
      
      annotations = list(
        list(text = paste0("<b>", leisure, "%</b>"),
             showarrow = FALSE,
             x = 0.3 + leisure * 0.0035, xref = "paper", xanchor = "center",
             y = 0.89, yref = "paper", yanchor = "middle"),
        list(text = "Think that leisure centres are 'shared and open' to both Catholics and Protestants",
             showarrow = FALSE,
             x = 0.3, xref = "paper", xanchor = "left",
             y = 0.84, yref = "paper", yanchor = "top"),
        list(text = paste0("<b>", park, "%</b>"),
             showarrow = FALSE,
             x = 0.3 + park * 0.0035, xref = "paper", xanchor = "center",
             y = 0.64, yref = "paper", yanchor = "middle"),
        list(text = "Think that parks are 'shared and open' to both Catholics and Protestants",
             showarrow = FALSE,
             x = 0.3, xref = "paper", xanchor = "left",
             y = 0.59, yref = "paper", yanchor = "top"),
        list(text = paste0("<b>", library, "%</b>"),
             showarrow = FALSE,
             x = 0.3 + library * 0.0035, xref = "paper", xanchor = "center",
             y = 0.4, yref = "paper", yanchor = "middle"),
        list(text = "Think that libraries are 'shared and open' to both Catholics and Protestants",
             showarrow = FALSE,
             x = 0.3, xref = "paper", xanchor = "left",
             y = 0.35, yref = "paper", yanchor = "top"),
        list(text = paste0("<b>", shopping, "%</b>"),
             showarrow = FALSE,
             x = 0.3 + shopping * 0.0015, xref = "paper", xanchor = "center",
             y = 0.13, yref = "paper", yanchor = "middle",
             font = list(color = "#ffffff")),
        list(text = "Think that shopping centres are 'shared and open' to both Catholics and Protestants",
             showarrow = FALSE,
             x = 0.3, xref = "paper", xanchor = "left",
             y = 0.08, yref = "paper", yanchor = "top",
             align = "left"),
        list(NA),
        list(NA),
        list(NA),
        list(NA),
        list(NA)
      ),
      
      modebar = list(remove = c("zoom", "pan", "zoomIn2d", "zoomOut2d",
                                "autoscale", "resetScale2d", "hoverCompareCartesian", "hoverClosestCartesian")),
      
      font = list(family = "Helvetica", size = 14)
      
    ) %>%
    
    config(displaylogo = FALSE)
  
}