chartTitle <- function(fig, title) {
  
  # Remove italic tags for downloadButtons
  downloadTitle <<- paste0("Figure ", fig, ": ", title) %>%
    gsub("<i>", "", .) %>%
    gsub("</i>", "", .)

  # Output title in columns
  div(class = "row",
    div(class = "col-sm-1", style = "width: 12%; flex: 0 0 12%; max-width: 12%; padding-right: 0px; margin-right: 0px", h4(paste0("Figure ", fig, ":"))),
    div(class = "col-sm-10", style = "padding-left: 0px;", h4(HTML(title))))
  
}