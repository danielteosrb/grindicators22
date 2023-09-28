# This function will output a HTML banner at the top of the report
# The following arguments are required:

# leftImage - The file name of an image to go on left hand side of banner. It needs to be stored in the images sub-folder
# leftAlt - The alt text to go on the left side image
# rightImage - See leftImage
# rightAlt - See leftAlt

banner <- function(leftImage, leftAlt, rightImage, rightAlt, subtitle = NULL) {
  
  if (is.null(subtitle)) {
  
    if (params$pre_release == TRUE) {
      subtitle <- p("Not for general release until: 9.30am", span(style = "text-decoration: underline", pubDate))
    } else {
      subtitle <- paste0("Published on ", pubDate)
    }
      
  }
  
  div(
    div(class = "row", style = "display: flex;",
        div(class = "row-indent"),
        div(class = "left", img(src = paste0("images/", leftImage), alt = leftAlt)),
        div(class = "middle", p(class = "toc-ignore", rmarkdown::metadata$title)),
        div(class = "right", img(src = paste0("images/", rightImage), alt = rightAlt)),
        div(class = "row-indent")),
    
    div(class = "row", style = "display: flex;",
      div(class = "row-indent"),
      div(class = "findings", subtitle),
      div(class = "row-indent", style = "background-color: #08215a !important"),
      div(class = "row-indent")
    ),
    
    div(class = "row", style = "display: flex;",
        div(class = "row-indent"),
        div(style = "background-color: #7eb0d3 !important; height: 9px; width: 100%;"),
        div(class = "row-indent")),
  
    if (params$pre_release == TRUE) {
    
      div(style = "font-weight: bold; text-align: center; color: #B33A3A !important;",
          br(),
          "Official - Sensitive - Statistics",
          br(),
          span(style = "text-decoration: underline", "Do not forward"),
          br(),
          br(),
          "Official statistics – please treat as protected, for named individuals or identified post holders only. Not for sharing with anyone else or to be used in any other documents before publication.",
          br(),
          br(),
          "If you think you need to discuss and share with anyone not on the circulation list, first contact the Department’s senior statistician. Any accidental or wrongful release should be reported immediately and may lead to an inquiry. Wrongful release includes indications of the content or trend of the figures, including description such as ‘favourable’ or ‘unfavourable’.")
    
    },
    br())
  
}