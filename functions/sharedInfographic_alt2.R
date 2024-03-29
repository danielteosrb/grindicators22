sharedInfographic_alt2 <- function() {

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
             x = 0, y = 0.96, sizex = 0.16, sizey = 0.16, xanchor = "left", yanchor = "top"),
        list(source = dataURI(file = "images/park.png"),
             x = 0, y = 0.47, sizex = 0.16, sizey = 0.16, xanchor = "left", yanchor = "middle"),
        list(source = dataURI(file = "images/library.png"),
             x = 0, y = 0, sizex = 0.16, sizey = 0.16, xanchor = "left", yanchor = "bottom"),
        list(source = dataURI(file = "images/workplace.png"),
             x = 0.53, y = 0.78, sizex = 0.16, sizey = 0.16, xanchor = "left", yanchor = "top"),
        list(source = dataURI(file = "images/neighbourhood.png"),
             x = 0.53, y = 0.28, sizex = 0.16, sizey = 0.16, xanchor = "left", yanchor = "middle"),
        list(NA)
      ),
      
      shapes = list(
        list(type = line,
             x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,
             line = list(color = "#999999", width = 2, dash = "dash")),
        list(type = "rect",
             x0 = 0.17, x1 = 0.17 + leisure * 0.003,
             y0 = 0.85, y1 = 0.95,
             xref = "paper", yref = "paper",
             fillcolor = "#b0deff",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.17 + leisure * 0.003, x1 = 0.47,
             y0 = 0.85, y1 = 0.95,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.17, x1 = 0.17 + park * 0.003,
             y0 = 0.45, y1 = 0.55,
             xref = "paper", yref = "paper",
             fillcolor = "#999999",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.17 + park * 0.003, x1 = 0.47,
             y0 = 0.45, y1 = 0.55,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.17, x1 = 0.17 + library * 0.003,
             y0 = 0.05, y1 = 0.15,
             xref = "paper", yref = "paper",
             fillcolor = "#85afd6",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.17 + library * 0.003, x1 = 0.47,
             y0 = 0.05, y1 = 0.15,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.7, x1 = 0.7 + workplace * 0.003,
             y0 = 0.65, y1 = 0.75,
             xref = "paper", yref = "paper",
             fillcolor = "#b0deff",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.7 + workplace * 0.003, x1 = 1,
             y0 = 0.65, y1 = 0.75,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.7, x1 = 0.7 + neighbourhood * 0.003,
             y0 = 0.23, y1 = 0.33,
             xref = "paper", yref = "paper",
             fillcolor = "#999999",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.7 + neighbourhood * 0.003, x1 = 1,
             y0 = 0.23, y1 = 0.33,
             xref = "paper", yref = "paper",
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(NA),
        list(NA)
      ),
      
      annotations = list(
        list(text = paste0("<b>", leisure, "%</b>"),
             showarrow = FALSE,
             x = 0.17 + leisure * 0.0015, xref = "paper", xanchor = "center",
             y = 0.9, yref = "paper", yanchor = "middle"),
        list(text = "Think that leisure centres are 'shared\nand open' to both Catholics and\nProtestants",
             showarrow = FALSE,
             x = 0.17, xref = "paper", xanchor = "left",
             y = 0.84, yref = "paper", yanchor = "top",
             align = "left"),
        list(text = paste0("<b>", park, "%</b>"),
             showarrow = FALSE,
             x = 0.17 + park * 0.0015, xref = "paper", xanchor = "center",
             y = 0.5, yref = "paper", yanchor = "middle"),
        list(text = "Think that parks are 'shared and\nopen' to both Catholics and\nProtestants",
             showarrow = FALSE,
             x = 0.17, xref = "paper", xanchor = "left",
             y = 0.44, yref = "paper", yanchor = "top",
             align = "left"),
        list(text = paste0("<b>", library, "%</b>"),
             showarrow = FALSE,
             x = 0.17 + library * 0.0015, xref = "paper", xanchor = "center",
             y = 0.1, yref = "paper", yanchor = "middle"),
        list(text = "Think that libraries are 'shared and\nopen' to both Catholics and\nProtestants",
             showarrow = FALSE,
             x = 0.17, xref = "paper", xanchor = "left",
             y = 0.04, yref = "paper", yanchor = "top",
             align = "left"),
        list(text = paste0("<b>", workplace, "%</b>"),
             showarrow = FALSE,
             x = 0.7 + workplace * 0.0015, xref = "paper", xanchor = "center",
             y = 0.7, yref = "paper", yanchor = "middle"),
        list(text = "Prefer a mixed religion workplace",
             showarrow = FALSE,
             x = 0.7, xref = "paper", xanchor = "left",
             y = 0.63, yref = "paper", yanchor = "top",
             align = "left"),
        list(text = paste0("<b>", neighbourhood, "%</b>"),
             showarrow = FALSE,
             x = 0.7 + neighbourhood * 0.0015, xref = "paper", xanchor = "center",
             y = 0.28, yref = "paper", yanchor = "middle"),
        list(text = "Prefer a mixed religion neighbourhood",
             showarrow = FALSE,
             x = 0.7, xref = "paper", xanchor = "left",
             y = 0.21, yref = "paper", yanchor = "top",
             align = "left"),
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