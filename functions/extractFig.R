# This function will extract particular columns from the main data frame
# It takes one argument f which relates to the figure number
# eg, extractFig("f1") will extract the data for Figure 1

extractFig <- function(f) {

  # Select the year column as well as the selected figure
  df <- data[names(data) == "year" | grepl(paste0(f, "_"), names(data))]
  
  # Drop f##_ from the column names
  names(df) <- gsub(paste0(f, "_"), "", names(df))
  
  # Output the data frame
  df

}