# This function will create the Indicator and "Why is this important?" style text boxes
# that heads each section of the report

# It takes three required arguments:
#   ind  - The indicator number. Given inside quotes, eg "1.1a"
#   desc - The description of the indicator. Given inside quotes
#   important - The text to go below "Why is this indicator important?". Given inside quotes.

indicator <- function(ind, desc, important) {
  
  # Extract first character from ind and convert to numeric
  area <- as.numeric(substr(ind, 1, 1))
  
  # if/else logic to set the text colour and background colour of text box
  if (area == 1) {
    
    textColour <- "#ffffff"
    bgColour <- "#426bba"
    
  } else if (area == 2) {
    
    textColour <- "#002469"
    bgColour <- "#c4db0d"
    
  } else if (area == 3) {
    
    textColour <- "#002469"
    bgColour <- "#d0cece"
    
  } else if (area == 4) {
    
    textColour <- "#ffffff"
    bgColour <- "#002469"
    
  }
  
  # Creates a unique id for each text box based on indicator number.
  # This id will be used for internal hyperlinking to specific sections.
  # Since there are two indicator 1.1a, the second one will have a 2 appended.
  id <- if (ind == "1.1a" & grepl("will be better", desc)) {
    "Indicator_11a2"
  } else {
    paste0("Indicator_", sub(".", "", ind, fixed = TRUE))
  }
  
  # The html to create the indicator and "Why is this important?" text boxes using the values above
  div(style = if (grepl(".1a", ind) & id != "Indicator_11a2") {""} else {"page-break-before: always;"},
  
  div(class = "row", style = "display: flex;", id = id,
      div(class = "row-indent"),
      div(style = paste0("width: 5px; background-color: ", bgColour, " !important; border-left: solid #002469 1px; border-top: solid #002469 1px; border-bottom: solid #002469 1px;")),
      div(style = paste0("width: 100%; background-color: ", bgColour, " !important; color: ", textColour," !important; border-right: solid #002469 1px; border-top: solid #002469 1px; border-bottom: solid #002469 1px;"),
          h3(style = paste0("color: ", textColour, " !important;"),
             paste0("Indicator ", ind)),
          p(style = paste0("font-weight: bold; color: ", textColour, " !important;"),
            desc)),
      div(class = "row-indent")),
    br(),
  div(class = "row", style = "display: flex;",
      div(class = "row-indent"),
      div(style = "width : 5px; background-color: #deebf7 !important;"),
    div(style = "width: 100%; background-color: #deebf7 !important; color: #002469;",
        p(style = "font-weight: bold;",
          "Why is this indicator important?"),
        p(important)),
    div(style = "width : 5px; background-color: #deebf7 !important;"),
    div(class = "row-indent"))
  
  )
}