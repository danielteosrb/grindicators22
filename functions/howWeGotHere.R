howWeGotHere <- function(para1, para2 = "") {

  div(div(class = "row", style = "display: flex;",
      div(class = "row-indent"),
      div(style = "width : 5px; background-color: #deebf7 !important;"),
      div(style = "width: 100%; background-color: #deebf7 !important; color: #002469;",
          p(style = "font-weight: bold;",
            "How we got here:"),
          p(para1),
          p(para2)),
      div(style = "width : 5px; background-color: #deebf7 !important;"),
      div(class = "row-indent")),
  br())
  
}