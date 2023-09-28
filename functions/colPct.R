# Function that returns the % of times a value appears in a column

# This function takes three required arguments and four optional arguments.

# Required arguments:
# data - the data frame containing the column we wish to analyse, given without quotes
# col - the column we are analysing
# value - one value, or a list of values, from that column we wish to see the total percentage for.
#         single values are given inside quotes and lists of values are wrapped inside c()
#         eg, c("val1", "val2", "val3"). Using a list of values returns the summed percentage for those values.


# Optional arguments (Any combination of these can be input to the function):
# gender - if used will filter the data and return the percentage of the value for the gender option(s) stated.
#          Must be included in quotes and/or lists inside c(), like value above.
# religion - Like gender, data will be filtered for religion entered. Given in quotes
# resident - If not stated when function is called then defaults to FALSE.
#            If input with resident = TRUE then data is filtered down to those who live
#            "Within a few streets of the nearest Peace Line". TRUE or FALSE are entered without quotes.
# weighted - Currently set to TRUE. By default the function will give the weighted percentage with the NILT data
#            and the unweighted percentage for the YLT data. Change to FALSE only if you require an unweighted
#            percentage from the NILT data.TRUE or FALSE are entered without quotes.


# Example usage:
# colPct(NILT, "RLRELAGO", "Better")
#    - will return weighted percentage of those who answered "Better" to question RLREAGO for all respondents of NILT.
# colPct(NILT, "RLRELAGO", "Better", gender = "Male")
#    - will return weighted percentage of those who answered "Better" to question RLREAGO for Male respondents of NILT.
# colPct(YLT, "FEELCATH", c("Very favourable", "Quite favourable"))
#    - will return unweighted percentage of those who answered "Very favourable" or "Quite favourable" to question FEELCATH
#      for all respondents of YLT
# colPct(NILT, "RLRELAGO", "Better", religion = "Protestant")
#    - will return weighted percentage of those who answered "Better" to question RLREAGO for Protestant respondents of NILT.
# colPct(NILT, "PLINEREM", "I would like the Peace Lines to come down now", resident = TRUE)
#    - will return weighted percentage of those who answered "I would like the Peace Lines to come down now" for those NILT 
#      respondents who live within a few streets of peace lines.
# colPct(NILT, "RLRELAGO", "Better", weighted = FALSE)
#    - will return unweighted percentage of those who answered "Better" to question RLREAGO for all respondents of NILT.

colPct <- function(data, col, value, gender = NULL, religion = NULL, resident = FALSE, weighted = TRUE) {

  column <- deparse(substitute(col))

  # Allow for question names with "a" appended
  column <- if (!column %in% names(data)) {
    paste0(column, "a")
  } else {
    column
  }
  
  # Apply filters on data
  # First filter filters out those who didn't respond to question
  # If the question DONEWHAT is being analysed then extra filtering is needed
  if (column %in% paste0("DONEWHAT", seq(1, 4))) {
    
    df <- data %>%
      # Filter out those who ticked "No"
      filter(.[["DONEWHAT5"]] == "Not ticked") %>%
      # Additional filter needed for those who didn't tick any of the 5 options
      filter(!(.[["DONEWHAT1"]] == "Not ticked" & .[["DONEWHAT2"]] == "Not ticked" & .[["DONEWHAT3"]] == "Not ticked" & .[["DONEWHAT4"]] == "Not ticked"))
    
  } else if (column %in% c("LCOPEN", "PARKOPEN", "LIBOPEN", "SHCNOPEN")) {
    
    df <- data %>%
      filter(!is.na(.[[column]])) %>%
      filter(.[[column]] != "None in this area")
      
  } else {
  
    df <- data %>%
      filter(!is.na(.[[column]]))
    
  }
  
  # If question needs filtered by gender this is first performed
  if(!is.null(gender)) {
    df <- df %>%
      filter(RSEX %in% gender)
  }
  
  # Filtered by religion if necessary
  if(!is.null(religion)) {
    
    df <- df %>%
      filter(toupper(RELIGCAT) == toupper(religion))
    
  }
  
  # Filter by peace wall residents
  if(resident == TRUE) {
    
    df <- df %>%
      filter(PLINELIV == "Within a few streets of the nearest Peace Line")
    
  }
  
  # Calculate weighted (default for NILT) or unweighted value (default for YLT)
  if(weighted == FALSE | deparse(substitute(data)) == "YLT") {
    
    # If only one value entered calculate rounded percentage
    if (length(value) == 1) {
    
      round2(length(which(df[[column]] == value)) / nrow(df) * 100)

    } else {
      
      # If a list of values calculate each percentage, then sum, then round:
      for (i in 1:length(value)) {
        
        if (i == 1) {
          x <- length(which(df[[column]] == value[i])) / nrow(df) * 100
        } else {
          x <- x + (length(which(df[[column]] == value[i])) / nrow(df) * 100)
        }
        
      }
      
      round2(x)
      
    }
    # Calculate weighted version:
  } else if(weighted == TRUE) {
    
    # Calculation for weighted n
    nw <-  df %>%
      group_by(.[[column]]) %>%
      summarise(response = sum(WTFACTOR)) %>%
      pull("response") %>%
      round2() %>%
      sum()

    # If only one value entered calculate rounded percentage
    if (length(value) == 1) {

      pw <- round2(sum(df[["WTFACTOR"]][df[[column]] == value]))

      round2(pw / nw * 100)

    } else {
      # If a list of values calculate each percentage, then sum and then round:
      for (i in 1:length(value)) {

        if (i == 1) {

          pw <- round2(sum(df[["WTFACTOR"]][df[[column]] == value[i]]))

          x <- pw / nw * 100

        } else {

          pw <- round2(sum(df[["WTFACTOR"]][df[[column]] == value[i]]))

          x <- x + (pw / nw * 100)
          
        }

      }

      round2(x)
      
    }
    
  }
  
}