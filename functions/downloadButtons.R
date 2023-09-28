# This function will take in a data frame, convert it to csv and xlsx, embed those files in the document
# and output download buttons that link to those files


downloadButtons <- function(fig, # Data frame to be output to download button. Entered without quotes.
                            indicator, # Indicator number. Entered inside quotes.
                            headers) # A column c() of header rows for accessible table. Each header inside quotes.
                            
  { 

  if (params$pre_release != TRUE) {
  
    # Create downloads folder if not existing
    if (!dir.exists("downloads")) {
      dir.create("downloads")
    }
    
    # Takes a copy of the input data frame
    figDownload <- fig
    
    # Renames the columns according to what has been entered in headers
    names(figDownload) <- headers
    # Creates an excel sheet name based on the figure name
    sheet <- deparse(substitute(fig))
    
    # Flatten first column (ie remove any line breaks, spaces, hyphens that were
    # necessary for chart presentation)
    figDownload[[1]] <- gsub("\n", " ", figDownload[[1]])
    figDownload[[1]] <- gsub("  ", " ", figDownload[[1]])
    figDownload[[1]] <- gsub("- ", "", figDownload[[1]])
    
    # If figure covers a range of years it will name the csv file accordingly and remove slashes
    # If the figure covers a single year it will name the csv file accordingly
    csvName <- if ("year" %in% names(fig)) {
      
      paste0("downloads/",
             sub("fig", "figure-", sheet),
             "-indicator-",
             sub(".", "-", indicator, fixed = TRUE),
             "-",
             sub("/", "-", min(figDownload$Year)),
             "-",
             sub("/", "-", max(figDownload$Year)),
             ".csv")
      
    } else {
      
      paste0("downloads/",
             sub("fig", "figure-", sheet),
             "-indicator-",
             sub(".", "-", indicator, fixed = TRUE),
             "-",
             NILTyear,
             ".csv")
      
    }
    
    # Excel file name is created by substituting the .csv extension for .xlsx
    excelName <- sub(".csv", ".xlsx", csvName, fixed = TRUE)
    
    # Remove commas from column headings for csv formatting
    csvDownload <- figDownload
    names(csvDownload) <- gsub(",", "", names(csvDownload))
    
    # Data frame is written out to the csv file
    write.csv(csvDownload,
              file = csvName,
              append = F,
              row.names = F)
    # Excel:
    # Cell style for column headers
    hs <- createStyle(halign = "right",
                      wrapText = TRUE,
                      textDecoration = "bold")
    
    hs2 <- createStyle(halign = "left",
                       wrapText = TRUE,
                       textDecoration = "bold")
    
    # Cell style for titles
    ts <- createStyle(textDecoration = "bold",
                      fontSize = 14)
    
    # Cell style for figures
    fs <- createStyle(numFmt = "#,##0",
                      halign = "right")
    
    # Cell style for row labels
    rs <- createStyle(halign = "left",
                      wrapText = TRUE)
    
    # Creates a new excel workbook and sets metadata
    wb <- createWorkbook(creator = "The Executive Office",
                         title = downloadTitle,
                         subject = "This publication monitors indicators for the four key priorities of the T:BUC Strategy: our children and young people; our shared community; our safe community; and our cultural expression.",
                         category = "Statistics produced in accordance with departmental requirements")
    
    # Changes font for new workbook
    modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
    
    # Gives the 
    tableName <- sub("fig", "fig_", sheet)
    
    # Adds a sheet
    addWorksheet(wb, sheet)
    
    r <- 1
    
    # Adds a title
    writeData(wb, sheet, downloadTitle, startCol = 1, startRow = r)
    
    addStyle(wb, sheet,
             style = ts,
             rows = r,
             cols = 1)
    
    r <- r + 1
    
    # Adds the dataframe
    writeDataTable(wb, sheet = sheet, figDownload,
                   startCol = 1, startRow = r, colNames = TRUE,
                   tableName = tableName,
                   withFilter = FALSE,
                   bandedRows = FALSE,
                   tableStyle = "none",
                   headerStyle = hs,
                   keepNA = TRUE,
                   na.string = "n/a")
    
    addStyle(wb, sheet = sheet,
             style = hs2,
             rows = r:(r + nrow(data)),
             cols = 1)
    
    addStyle(wb, sheet = sheet,
             style = rs,
             rows = r+1:(r + nrow(data)),
             cols = 1)
    
    addStyle(wb, sheet = sheet,
             style = fs,
             rows = r+1:(r + nrow(data)),
             cols = 2:length(data),
             gridExpand = TRUE)
    
    if ("Year" %in% headers) {
      setColWidths(wb, sheet, cols = 1, widths = 10)
    } else {
      setColWidths(wb, sheet, cols = 1, widths = 50)
    }
    
    if (indicator == "1.1b") {
      setColWidths(wb, sheet, cols = 2:length(data), widths = 24)
    } else {
      setColWidths(wb, sheet, cols = 2:length(data), widths = 14)
    }
    
    # Workbook saved
    saveWorkbook(wb, excelName, overwrite = TRUE)
    
    # Calculate size of csv file
    CSVsize <- round2(file.size(csvName) / 1000)
    
    # If csv size display 0kB round up to 1 anyway
    CSVsize <- if (CSVsize == 0) {
      "(1kB)"
    } else {
      paste0("(", CSVsize, "kB)")
    }
    
    # Calculate size of excel file
    XLsize <- paste0("(", round2(file.size(excelName) / 1000), "kB)")
    
    # Embed CSV and Excel files behind links
    emCSV <- embed_file(csvName, text = paste0(sub("fig", "Figure ", sheet), ".csv"))
    emXL <- embed_file(excelName, text = paste0(sub("fig", "Figure ", sheet), ".xlsx"))
    
    # Output download buttons
    div(
      
      div(class = "row", style = "display: flex;",
          div(class = "row-indent"),
          div(class = "download", "Download data: ")),
      
      div(class = "row", style = "display: flex;",
          div(class = "row-indent"),
          div(class = "xl-button", emXL, span(style = "font-weight: normal; color: #08215a !important; font-size: 12pt", XLsize)),
          div(class = "row-indent"),
          div(class = "csv-button", emCSV, span(style = "font-weight: normal; color: #ffffff !important; font-size: 12pt", CSVsize)))
      
    )
  
  }
  
}

