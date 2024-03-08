safeInfographic <- function() {
  
  
  donutValues <- c(dataNew$f17_compare,
                   100 - dataNew$f17_compare)
  
  f17_diff <<- dataNew$f17_compare - dataOld$f17_compare
  
  safeSig <<- significanceTest(p1 = dataNew$f17_compare,
                               n1 = dataNew$NISAFEWL_n,
                               p2 = dataOld$f17_compare,
                               n2 = dataOld$NISAFEWL_n)
  
  if (safeSig == "significant decrease") {
    
    cap <- paste0(abs(round2(f17_diff)), " percentage points lower than in ", NILTyear - 1)
    arrow <- "images/downArrow.png"
    
  } else if (safeSig == "significant increase") {
    
    cap <- paste0(round2(f17_diff), "% higher than in ", NILTyear - 1)
    arrow <- "images/upArrow.png"
    
  } else {
    
    cap <- paste0("No significant change from ", NILTyear - 1)
    arrow <- "images/sideArrow.png"
    
  }
  
  plot_ly(fig17,
          domain = list(x = c(0.05, 0.25), y = c(0.75, 0.95)),
          values = donutValues,
          labels = c("Agree or strongly disagree", "Do not agree"),
          type = "pie",
          marker = list(colors = c("#2d4673", "#e5e5e5")),
          hole = 0.7,
          textinfo = "none",
          hoverinfo = "none",
          showlegend = FALSE) %>%
    
    layout(annotations = list(
      list(text = paste0("<b>", round2(donutValues[1]), "</b>%"),
           showarrow = FALSE,
           x = 0.15, xref = "paper", xanchor = "center",
           y = 0.75, yref = "paper", yanchor = "bottom",
           font = list(color = "#ffffff", size = 13)),
      list(text = paste0(round2(donutValues[1]), "% of people see town centres as safe and welcoming\nplaces for people from all walks of life"),
           showarrow = FALSE,
           x = 0.3, xref = "paper", xanchor = "left",
           y = 0.9, yref = "paper", yanchor = "middle",
           align = "left"),
      list(text = cap,
           showarrow = FALSE,
           x = 0.34, xref = "paper", xanchor = "left",
           y = 0.8, yref = "paper", yanchor = "middle"),
      list(text = paste0("<b>", round2(fig18$All[grepl("GAA", fig18$location)]), "%</b>"),
           showarrow = FALSE,
           x = 0.3 + fig18$All[grepl("GAA", fig18$location)] * 0.0035,
           xref = "paper", xanchor = "center",
           y = 0.64, yref = "paper", yanchor = "middle"),
      list(text = paste0("<b>", round2(fig18$All[grepl("Orange", fig18$location)]), "%</b>"),
           showarrow = FALSE,
           x = 0.3 + fig18$All[grepl("Orange", fig18$location)] * 0.0035,
           xref = "paper", xanchor = "center",
           y = 0.44, yref = "paper", yanchor = "middle",
           font = list(color = "#ffffff")),
      list(text = paste0("<b>", round2(fig18$All[grepl("Protestant", fig18$location)]), "%</b>"),
           showarrow = FALSE,
           x = 0.3 + fig18$All[grepl("Protestant", fig18$location)] * 0.0035,
           xref = "paper", xanchor = "center",
           y = 0.25, yref = "paper", yanchor = "middle"),
      list(text = paste0("<b>", round2(fig18$All[grepl("Catholic", fig18$location)]), "%</b>"),
           showarrow = FALSE,
           x = 0.3 + fig18$All[grepl("Catholic", fig18$location)] * 0.0035,
           xref = "paper", xanchor = "center",
           y = 0.05, yref = "paper", yanchor = "middle"),
      list(text = "Feel safe attending events held in a GAA Club",
           showarrow = FALSE,
           x = 0.3, xref = "paper", xanchor = "left",
           y = 0.59, yref = "paper", yanchor = "top"),
      list(text = "Feel safe attending events held in a Orange Hall",
           showarrow = FALSE,
           x = 0.3, xref = "paper", xanchor = "left",
           y = 0.39, yref = "paper", yanchor = "top"),
      list(text = "Feel safe attending events held in a Protestant Secondary School",
           showarrow = FALSE,
           x = 0.3, xref = "paper", xanchor = "left",
           y = 0.2, yref = "paper", yanchor = "top"),
      list(text = "Feel safe attending events held in an Catholic Secondary School",
           showarrow = FALSE,
           x = 0.3, xref = "paper", xanchor = "left",
           y = 0, yref = "paper", yanchor = "top")
    ),
    
    images = list(
      list(source = dataURI(file = arrow),
           x = 0.31, y = 0.8,
           xanchor = "bottom", yanchor = "middle",
           sizex = 0.05, sizey = 0.05),
      list(source = dataURI(file = "images/town.png"),
           x = 0.15, y = 0.85,
           xanchor = "center", yanchor = "middle",
           sizex = 0.09, sizey = 0.09),
      list(source = dataURI(file = "images/GAAclub.png"),
           x = 0.15, y = 0.71,
           xanchor = "center", yanchor = "top",
           sizex = 0.2, sizey = 0.16),
      list(source = dataURI(file = "images/OrangeHall.png"),
           x = 0.15, y = 0.51,
           xanchor = "center", yanchor = "top",
           sizex = 0.2, sizey = 0.16),
      list(source = dataURI(file = "images/religion.png"),
           x = 0.15, y = 0.17,
           xanchor = "center", yanchor = "bottom",
           sizex = 0.15, sizey = 0.16),
      list(source = dataURI(file = "images/catholic.png"),
           x = 0.15, y = 0.03,
           xanchor = "center", yanchor = "middle",
           sizex = 0.21, sizey = 0.16)),
    
    shapes = list(
      list(type = "rect",
           x0 = 0.3, x1 = 0.3 + fig18$All[grepl("GAA Club", fig18$location)] * 0.007,
           y0 = 0.59, y1 = 0.69,
           fillcolor = "#85afd6",
           line = list(width = 0)),
      list(type = "rect",
           x0 = 0.3 + fig18$All[grepl("GAA Club", fig18$location)] * 0.007, x1 = 1,
           y0 = 0.59, y1 = 0.69,
           fillcolor = "#e8e8e8",
           line = list(width = 0)),
      list(type = "rect",
           x0 = 0.3, x1 = 0.3 + fig18$All[grepl("Orange Hall", fig18$location)] * 0.007,
           y0 = 0.39, y1 = 0.49,
           fillcolor = "#2d4673",
           line = list(width = 0)),
      list(type = "rect",
           x0 = 0.3 + fig18$All[grepl("Orange Hall", fig18$location)] * 0.007, x1 = 1,
           y0 = 0.39, y1 = 0.49,
           fillcolor = "#e8e8e8",
           line = list(width = 0)),
      list(type = "rect",
           x0 = 0.3, x1 = 0.3 + fig18$All[grepl("Protestant", fig18$location)] * 0.007,
           y0 = 0.2, y1 = 0.3,
           fillcolor = "#999999",
           line = list(width = 0)),
      list(type = "rect",
           x0 = 0.3 + fig18$All[grepl("Protestant", fig18$location)] * 0.007, x1 = 1,
           y0 = 0.2, y1 = 0.3,
           fillcolor = "#e8e8e8",
           line = list(width = 0)),
      list(type = "rect",
           x0 = 0.3, x1 = 0.3 + fig18$All[grepl("Catholic", fig18$location)] * 0.007,
           y0 = 0, y1 = 0.1,
           fillcolor = "#b0deff",
           line = list(width = 0)),
      list(type = "rect",
           x0 = 0.3 + fig18$All[grepl("Catholic", fig18$location)] * 0.007, x1 = 1,
           y0 = -0, y1 = 0.1,
           fillcolor = "#e8e8e8",
           line = list(width = 0))
    ),
    
    modebar = list(remove = c("zoom", "pan", "zoomIn2d", "zoomOut2d",
                              "autoscale", "resetScale2d", "hoverCompareCartesian", "hoverClosestCartesian", "hoverClosestPie")),
    
    font = list(family = "Helvetica", size = 14)
    
    ) %>%
    
    config(displaylogo = FALSE)
  
}