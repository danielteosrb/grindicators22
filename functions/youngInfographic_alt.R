youngInfographic_alt <- function() { 

 adultDiff <- round2(dataNew$f1_adults - dataOld$f1_adults)
 
 adultSig <<- significanceTest(p1 = dataNew$f1_adults,
                               n1 = dataNew$RLRELAGO_n,
                               p2 = dataOld$f1_adults,
                               n2 = dataOld$RLRELAGO_n)
  
  if (adultSig == "significant decrease") {
    adultArrow <- "images/downArrow.png"
    adultCap <- paste0(abs(adultDiff), "% lower than in ", NILTyear - 1)
  } else if (adultSig == "significant increase") {
    adultArrow <- "images/upArrow.png"
    adultCap <- paste0(adultDiff, "% higher than in ", NILTyear - 1)
  } else {
    adultArrow <- "images/ArrowSideways.png"
    adultCap <- paste0("No significant change since ", NILTyear - 1)
  }
  
  youngDiff <- round2(dataNew$f1_young - dataOld$f1_young)
  
  youngSig <<- significanceTest(p1 = dataNew$f1_young,
                                n1 = dataNew$RLRELAGO_Yn,
                                p2 = dataOld$f1_young,
                                n2 = dataOld$RLRELAGO_Yn)

  if (youngSig == "significant decrease") {
    youngArrow <- "images/downArrow.png"
    youngCap <- paste0(abs(youngDiff), "% lower than in ", NILTyear - 1)
  } else if (youngDiff == "significant increase") {
    youngArrow <- "images/upArrow.png"
    youngCap <- paste0(youngDiff, "% higher than in ", NILTyear - 1)
  } else {
    youngArrow <- "images/ArrowSideways.png"
    youngCap <- paste0("No significant change since ", NILTyear - 1)
  }
  
  plot_ly(fig1,
          domain = list(x = c(0.06, 0.44), y = c(0.5, 1)),
          value = round2(dataNew$f1_adults),
          number = list(suffix = "%", font = list(size = 55)),
          type = "indicator",
          mode = "gauge+number",
          gauge = list(
            axis = list(
              range = list(NULL,100),
              tickwidth = 1, 
              tickcolor = "black"),
            bar = list(color = "#b0deff"),
            bgcolor = "white", 
            borderwidth = 2,
            bordercolor = "gray",
            steps = list(
              list(range = c(0, 100), color = "#e8e8e8")),
            threshold = list(
              line = list(color = "black", width = 4),
              thickness = 0.75,
              value = round2(dataNew$f1_adults)))) %>%
    
    add_trace(domain = list(y = c(0, 0.5)),
              value = round2(dataNew$f1_young),
              gauge = list(
                bar = list(color = "#85afd6"),
                threshold = list(
                  value = round2(dataNew$f1_young)))) %>%
    
    layout(annotations = list(
      list(text = "<b>Relations between Protestants and Catholics\nare better now than they were five years ago?</b>",
           showarrow = FALSE,
           x = 0.25, xref = "paper", xanchor = "center",
           y = 1, yref = "paper", yanchor = "bottom"),
      list(text = "<b>Adults:</b>",
           showarrow = FALSE,
           x = 0.25, xref = "paper", xanchor = "center",
           y = 0.75, yref = "paper", yanchor = "middle"),
      list(text = adultCap,
           showarrow = FALSE,
           x = 0.25, xref = "paper", xanchor = "center",
           y = 0.53, yref = "paper", yanchor = "middle"),
      list(text = "<b>Young people:</b>",
           showarrow = FALSE,
           x = 0.25, xref = "paper", xanchor = "center",
           y = 0.25, yref = "paper", yanchor = "middle"),
      list(text = youngCap,
           showarrow = FALSE,
           x = 0.25, xref = "paper", xanchor = "center",
           y = 0.03, yref = "paper", yanchor = "middle"),
      list(text = "Done projects with pupils from other schools where the\npupils are from a different religious background",
           showarrow = FALSE,
           x = 0.53, xref = "paper", xanchor = "left",
           y = 0.73, yref = "paper", yanchor = "top",
           align = "left"),
      list(text = "Shared classes with pupils from other schools where the\npupils are from a different religious background",
           showarrow = FALSE,
           x = 0.53, xref = "paper", xanchor = "left",
           y = 0.13, yref = "paper", yanchor = "top",
           align = "left"),
      list(text = paste0("<b>", round2(fig5$young[fig5$activity == "Projects"]), "%</b>"),
           showarrow = FALSE,
           x = 0.7 + fig5$young[fig5$activity == "Projects"] * 0.0015, xref = "paper", xanchor = "center",
           y = 0.85, yref = "paper", yanchor = "middle"),
      list(text = paste0("<b>", round2(fig5$young[fig5$activity == "Classes"]), "%</b>"),
           showarrow = FALSE,
           x = 0.7 + fig5$young[fig5$activity == "Classes"] * 0.0015, xref = "paper", xanchor = "center",
           y = 0.28, yref = "paper", yanchor = "middle")),
      
      images = list(
        list(source = dataURI(file = adultArrow),
             x = 0.09, y = 0.56, sizex = 0.05, sizey = 0.05),
        list(source = dataURI(file = youngArrow),
             x = 0.09, y = 0.06, sizex = 0.05, sizey = 0.05),
        list(source = dataURI(file = "images/book.png"),
             x = 0.6, y = 0.85, sizex = 0.16, sizey = 0.16, xanchor = "center", yanchor = "middle"),
        list(source = dataURI(file = "images/school.png"),
             x = 0.6, y = 0.28, sizex = 0.16, sizey = 0.16, xanchor = "center", yanchor = "middle")),
      
      shapes = list(
        list(type = line,
             x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1,
             line = list(color = "#999999", width = 2, dash = "dash")),
        list(type = "rect",
             x0 = 0.7, x1 = 0.7 + fig5$young[fig5$activity == "Projects"] * 0.003,
             y0 = 0.8, y1 = 0.9,
             fillcolor = "#b0deff",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.7 + fig5$young[fig5$activity == "Projects"] * 0.003, x1 = 1,
             y0 = 0.8, y1 = 0.9,
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.7, x1 = 0.7 + fig5$young[fig5$activity == "Classes"] * 0.003,
             y0 = 0.23, y1 = 0.33,
             fillcolor = "#85afd6",
             line = list(width = 0)),
        list(type = "rect",
             x0 = 0.7 + fig5$young[fig5$activity == "Classes"] * 0.003, x1 = 1,
             y0 = 0.23, y1 = 0.33,
             fillcolor = "#e8e8e8",
             line = list(width = 0))),
      
      
      margin = list(t = 50, b = 60),
      
      font = list(family = "Helvetica", size = 14)) %>% 
    
     config(displaylogo = FALSE)
  
 }