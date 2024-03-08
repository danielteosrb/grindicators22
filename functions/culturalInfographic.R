# The following function is called without arguments. It will produce the Cultural identity infographic

culturalInfographic <- function() {
  
  # The values for the neighbourhood donut charts are written out as vectors
  # Current year:
  nghDonut <- c(dataNew$f21b_neighbourhood,
                   100 - dataNew$f21b_neighbourhood)
  
  # Previous year:
  nghDonutOld <- c(dataOld$f21b_neighbourhood,
                          100 - dataOld$f21b_neighbourhood)
  
  # The difference between years is calculated
  nghDiff <- dataNew$f21b_neighbourhood - dataOld$f21b_neighbourhood
  
  # Test if difference is significant
  nghSig <- significanceTest(p1 = dataNew$f21b_neighbourhood,
                             n1 = dataNew$INFLLOCL_n,
                             p2 = dataOld$f21b_neighbourhood,
                             n2 = dataOld$INFLLOCL_n)
  
  # Depending on significance create a caption and select an arrow for the direction of change
  if (nghSig == "significant decrease") {
    
    nghCap <- paste0(abs(round2(nghDiff)), "% lower than in ", NILTyear - 1)
    nghArrow <- "images/downArrow.png"
    
  } else if (nghSig == "significant increase") {
    
    nghCap <- paste0(round2(nghDiff), "% higher than in ", NILTyear - 1)
    nghArrow <- "images/upArrow.png"
    
  } else {
    
    nghCap <- paste0("No significant change\nsince ", NILTyear - 1)
    nghArrow <- "images/ArrowSideways.png"
    
  }
  
  # The values for the NI donut charts are written out as vectors
  # Current year:
  NIdonut <- c(dataNew$f21b_NI,
               100 - dataNew$f21b_NI)
  
  # Previous year:
  NIdonutOld <- c(dataOld$f21b_NI,
                  100 - dataOld$f21b_NI)
  
  # The difference between years is calculated
  NIDiff <- dataNew$f21b_NI - dataOld$f21b_NI
  
  # Test if difference is significant
  niSig <- significanceTest(p1 = dataNew$f21b_NI,
                            n1 = dataNew$INFLNI_n,
                            p2 = dataOld$f21b_NI,
                            n2 = dataOld$INFLNI_n)
  
  # Depending on significance create a caption and select an arrow for the direction of change
  if (niSig == "significant decrease") {
    
    NICap <- paste0(abs(round2(NIDiff)), "% lower in\n", NILTyear, " than in ", NILTyear - 1)
    NIArrow <- "images/downArrow.png"
    
  } else if (niSig == "significant increase") {
    
    NICap <- paste0(round2(NIDiff), "% higher in\n", NILTyear, " than in ", NILTyear - 1)
    NIArrow <- "images/upArrow.png"
    
  } else {
    
    NICap <- paste0("No significant change\nsince ", NILTyear - 1)
    NIArrow <- "images/ArrowSideways.png"
    
  }
  
  # Calculate values for percentage bars
  protestant <- (fig22$strongly + fig22$agree)[fig22$community == "Protestant\ncommunities"]
  
  catholic <- 71
  
  minority <- 67
  
  # Plotting the infographic:
  # Plots the first donut chart in top right hand corner
  plot_ly(fig21b, # A data frame
          domain = list(x = c(0.83, 0.98), y = c(0.52, 0.98)),  # Position of donut chart on plot
          values = nghDonutOld,  # The vector of values for the donut
          name = paste(NILTyear, "influence on decisions in their neighbourhood"),
          labels = c("Definitely or probably", "Do not agree"),  # Section labels 
          type = "pie",   # Set type to pie
          marker = list(colors = c("#2d4673", "#e5e5e5")),  # Define colours
          hole = 0.7,   # Place hole in pie chart to make it a donut
          textinfo = "none",   # No text to be displayed on donut
          hoverinfo = "none", # No hover labels to be displayed on donut
          sort = FALSE,   # No sorting applied on donut sections
          showlegend = FALSE) %>%  # Remove legend
    
    # Add second donut, changing position and values
    add_trace(domain = list(x = c(0.52, 0.67), y = c(0.52, 0.98)),
              name = paste(NILTyear - 1, "influence on decisions in their neighbourhood"),
              values = nghDonut) %>%
    
    # Add third donut, changing position, values and colours
    add_trace(domain = list(x = c(0.83, 0.98), y = c(0.02, 0.48)),
              values = NIdonutOld,
              name = paste(NILTyear, "influence on Northern Ireland"),
              marker = list(colors = c("#85afd6", "#e5e5e5"))) %>%
    
    # Add fourth donut, changing position, values and colours
    add_trace(domain = list(x = c(0.52, 0.67), y = c(0.02, 0.48)),
              values = NIdonut,
              name = paste(NILTyear - 1, "influence on Northern Ireland"),
              marker = list(colors = c("#85afd6", "#e5e5e5"))) %>%
    
    # Plot some shapes
    layout(
      shapes = list(
        # Dashed vertical line through the middle of the infographic
        list(type = line,
             x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 0.89,
             line = list(color = "#999999", width = 2, dash = "dash")),
        # Blue rectangle showing the proportion who think catholics enrich NI
        list(type = "rect",
             x0 = 0.17, x1 = 0.17 + catholic * 0.003,
             y0 = 0.75, y1 = 0.85,
             fillcolor = "#85afd6",
             line = list(width = 0)),
        # Grey rectangle to fill rest of bar above
        list(type = "rect",
             x0 = 0.17 + catholic * 0.003, x1 = 0.47,
             y0 = 0.75, y1 = 0.85,
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        # Blue rectangle showing the proportion who think protestants enrich NI
        list(type = "rect",
             x0 = 0.17, x1 = 0.17 + protestant * 0.003,
             y0 = 0.45, y1 = 0.55,
             fillcolor = "#2d4673",
             line = list(width = 0)),
        # Grey rectangle to fill rest of bar above
        list(type = "rect",
             x0 = 0.17 + protestant * 0.003, x1 = 0.47,
             y0 = 0.45, y1 = 0.55,
             fillcolor = "#e8e8e8",
             line = list(width = 0)),
        # Blue rectangle showing the proportion who think minority communities enrich NI
        list(type = "rect",
             x0 = 0.17, x1 = 0.17 + minority * 0.003,
             y0 = 0.15, y1 = 0.25,
             fillcolor = "#b0deff",
             line = list(width = 0)),
        # Grey rectangle to fill rest of bar above
        list(type = "rect",
             x0 = 0.17 + minority * 0.003, x1 = 0.47,
             y0 = 0.15, y1 = 0.25,
             fillcolor = "#e8e8e8",
             line = list(width = 0))
      ),
      
      # Insert images
      images = list(
        # First community image
        list(source = dataURI(file = "images/community1.png"),
             x = 0.09, y = 0.75, xanchor = "center", yanchor = "middle",
             sizex = 0.13, sizey = 0.15),
        # Second community image
        list(source = dataURI(file = "images/community2.png"),
             x = 0.09, y = 0.45, xanchor = "center", yanchor = "middle",
             sizex = 0.13, sizey = 0.15),
        # Third community image
        list(source = dataURI(file = "images/community3.png"),
             x = 0.09, y = 0.15, xanchor = "center", yanchor = "middle",
             sizex = 0.13, sizey = 0.15),
        # Local decisions image
        list(source = dataURI(file = "images/local.png"),
             x = 0.595, y = 0.75, xanchor = "center", yanchor = "middle",
             sizex = 0.1, sizey = 0.1),
        list(source = dataURI(file = "images/local.png"),
             x = 0.905, y = 0.75, xanchor = "center", yanchor = "middle",
             sizex = 0.1, sizey = 0.1),
        # NI image
        list(source = dataURI(file = "images/NI.png"),
             x = 0.595, y = 0.25, xanchor = "center", yanchor = "middle",
             sizex = 0.1, sizey = 0.1),
        list(source = dataURI(file = "images/NI.png"),
             x = 0.905, y = 0.25, xanchor = "center", yanchor = "middle",
             sizex = 0.1, sizey = 0.1),
        list(source = dataURI(file = NIArrow),
             x = 0.75, y = 0.24, xanchor = "center", yanchor = "middle",
             sizex = 0.06, sizey = 0.06),
        list(source = dataURI(file = nghArrow),
             x = 0.75, y = 0.74, xanchor = "center", yanchor = "middle",
             sizex = 0.06, sizey = 0.06)
      ),
      
      # Add text:
      annotations = list(
        list(text = paste0("<b>", round2(catholic), "%</b>"),
             showarrow = FALSE,
             x = 0.17 + protestant * 0.0015, y = 0.8,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "middle"),
        list(text = "Think that Catholic Communities\nadd to the richness and diversity\nof Northern Ireland society",
             showarrow = FALSE,
             x = 0.17, y = 0.75,
             xref = "paper", yref = "paper",
             xanchor = "left", yanchor = "top",
             align = "left"),
        list(text = paste0("<b>", round2(protestant), "%</b>"),
             showarrow = FALSE,
             x = 0.17 + catholic * 0.0015, y = 0.5,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "middle",
             font = list(color = "#ffffff")),
        list(text = "Think that Protestant Communities\nadd to the richness and diversity\nof Northern Ireland society",
             showarrow = FALSE,
             x = 0.17, y = 0.45,
             xref = "paper", yref = "paper",
             xanchor = "left", yanchor = "top",
             align = "left"),
        list(text = paste0("<b>", round2(minority), "%</b>"),
             showarrow = FALSE,
             x = 0.17 + minority * 0.0015, y = 0.2,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "middle"),
        list(text = "Think that Minority Ethnic Communities\nadd to the richness and diversity\nof Northern Ireland society",
             showarrow = FALSE,
             x = 0.17, y = 0.15,
             xref = "paper", yref = "paper",
             xanchor = "left", yanchor = "top",
             align = "left"),
        list(text = paste0(round2(dataNew$f21b_neighbourhood), "%"),
             showarrow = FALSE,
             x = 0.595, y = 0.84,
             xref = "paper", yref = "paper",
             xanchor = "left", yanchor = "middle",
             font = list(color = "#ffffff", size = 12)),
        list(text = nghCap,
             showarrow = FALSE,
             x = 0.75, y = 0.69,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "top"),
        list(text = paste0(round2(dataOld$f21b_neighbourhood), "%"),
             showarrow = FALSE,
             x = 0.905, y = 0.84,
             xref = "paper", yref = "paper",
             xanchor = "left", yanchor = "middle",
             font = list(color = "#ffffff", size = 12)),
        list(text = paste0(round2(dataNew$f21b_NI), "%"),
             showarrow = FALSE,
             x = 0.595, y = 0.34,
             xref = "paper", yref = "paper",
             xanchor = "left", yanchor = "middle",
             font = list(color = "#ffffff", size = 12)),
        list(text = paste0(round2(dataOld$f21b_NI), "%"),
             showarrow = FALSE,
             x = 0.905, y = 0.34,
             xref = "paper", yref = "paper",
             xanchor = "left", yanchor = "middle",
             font = list(color = "#ffffff", size = 12)),
        list(text = paste0("<b>", NILTyear, "</b>"),
             showarrow = FALSE,
             x = 0.595, y = 0.13,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "top"),
        list(text = paste0("<b>", NILTyear - 1, "</b>"),
             showarrow = FALSE,
             x = 0.905, y = 0.13,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "top"),
        list(text = paste0("<b>", NILTyear, "</b>"),
             showarrow = FALSE,
             x = 0.595, y = 0.63,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "top"),
        list(text = paste0("<b>", NILTyear - 1, "</b>"),
             showarrow = FALSE,
             x = 0.905, y = 0.63,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "top"),
        list(text = "<b>Feel they have an influence on\ndecisions in their neighbourhood</b>",
             showarrow = FALSE,
             x = 0.75, y = 0.57,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "top"),
        list(text = "<b>Feel they have an influence on\nNorthern Ireland decisions</b>",
             showarrow = FALSE,
             x = 0.75, y = 0.07,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "top"),
        list(text = NICap,
             showarrow = FALSE,
             x = 0.75, y = 0.19,
             xref = "paper", yref = "paper",
             xanchor = "center", yanchor = "top")
      ),
      
      font = list(family = "Helvetica", size = 14),
      
      modebar = list(remove = c("hoverClosestCartesian", "hoverClosestPie"))
      
    ) %>%
    
    config(displaylogo = FALSE)
  
}