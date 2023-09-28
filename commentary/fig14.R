# Calculate last year of psni data
psniYearLast <- paste0("20", as.numeric(substr(psniYear, 3, 4)) - 1, "/", as.numeric(substr(psniYear, 6, 7)) - 1)
psniOld <- fig14 %>%
  filter(year == psniYearLast)

# Sum of Racial and Sectarian Crimes
rsCrimes <- sum(psniNew[names(psniNew) %in% c("Racial", "Sectarian")])
# Racial crimes
rCrimes <- sum(psniNew[names(psniNew) %in% c("Racial")])
# Sectarian crimes
sCrimes <- sum(psniNew[names(psniNew) %in% c("Sectarian")])
# Sum of all Crimes
allCrimes <- sum(psniNew[!names(psniNew) %in% c("year")])

othersDoubled <- (allCrimes - rsCrimes) * 2
othersDoubledRange <- (othersDoubled - 10):(othersDoubled + 10)

# Proportion of racial and sectarian
propRS <- rsCrimes / allCrimes

otherCrimes <- allCrimes - rsCrimes

# Commentary sentence about all racial/sectarian vs all other crimes
allOthers <- if (psniNew$Racial %in% othersDoubledRange & psniNew$Sectarian %in% othersDoubledRange) {
  "Sectarian and racially motivated hate crimes are each around twice the total number of all other categories of hate crimes combined."
} else if ((rCrimes / 2) > otherCrimes & (sCrimes / 2) > otherCrimes) {
  "All other categories of hate crimes when combined are less than half the number of either sectarian or racially motivated hate crimes."
} else {
  paste0("All other categories of hate crimes when combined accounted for ",
         round2((1 - propRS) * 100), "% of hate crimes.")
}


# Sequence of racial greater than sectarian
rsSequence <- fig14$Racial > fig14$Sectarian
# Which is greater in current year?
if (psniNew$Racial > psniNew$Sectarian) {
  rsGreater <- "racially motivated"
  rsFewer <- "sectarian"
} else {
  rsGreater <- "sectarian"
  rsFewer <- "racially motivated"
}

# Number of consecutive years it's been the same
for (i in 1:length(rsSequence)) {
  if (i == 1) {
    consecutive <- 1
  } else {
    if (rev(rsSequence)[i] == rev(rsSequence)[i - 1]) {
      counsecutive <- consecutive + 1
    } else {
      break
    }
  }
}

f14sentence1 <- 
  if(rsSequence[length(rsSequence)] != rsSequence[length(rsSequence) - 1]) {
  paste0("Unlike last year, the number of ", rsGreater, " hate crimes has surpassed that of ",
         rsFewer," hate crimes.")
  } else {
    paste0("Like last year, the number of ", rsGreater, " hate crimes has surpassed that of ",
           rsFewer," hate crimes. This is the ", ordinal(length(rsSequence[rsSequence == TRUE])), " year in which this has happened since 2013.")
  }

# How has racially motivated changed on last year
racialChange <- if (psniNew$Racial > psniOld$Racial) {
  "shown an increase"
} else if (psniNew$Racial < psniOld$Racial) {
  "shown a decrease"
} else {
  "remained the same"
}

sectarianChange <- if (psniNew$Sectarian > psniOld$Sectarian) {
  "shown an increase"
} else if (psniNew$Sectarian < psniOld$Sectarian) {
  "shown a decrease"
} else {
  "remained the same"
}

f14sentence2 <-
  
  if (racialChange == sectarianChange) {
    paste0("While both have ", racialChange, " since ", psniYearLast, " (", sub("shown ", "", racialChange),
           " of ", abs(psniNew$Racial - psniOld$Racial), " (racial) and ",
           abs(psniNew$Sectarian - psniOld$Sectarian), " (sectarian)),")
    
  } else if (racialChange == "shown a decrease" & sectarianChange == "shown an increase") {
    
    paste0("While racially motivated hate crimes have shown a decrease of ",
           abs(psniNew$Racial - psniOld$Racial), " since ", psniYearLast,
           " and sectarian hate crimes have shown an increase of ",
           abs(psniNew$Sectarian - psniOld$Sectarian), ",")
    
  } else if (racialChange == "shown an increase" & sectarianChange == "shown a decrease") {
    
    paste0("While sectarian hate crimes have shown a decrease of ",
           abs(psniNew$Sectarian - psniOld$Sectarian), " since ", psniYearLast,
           " and racially motivated hate crimes have shown an increase of ",
           abs(psniNew$Racial - psniOld$Racial), ",")
    
  } else if (racialChange == "remained the same" & sectarianChange != "remained the same") {
    
    paste0("While sectarian hate crimes have ", sectarianChange ," of ",
           abs(psniNew$Sectarian - psniOld$Sectarian), " since ", psniYearLast,
           " and racially motivated hate crimes have remained the same,")
    
  } else if (sectarianChange == "remained the same" & racialChange != "remained the same") {
    
    paste0("While racial hate crimes have ", racialChange ," of ",
           abs(psniNew$Racial - psniOld$Racial), " since ", psniYearLast,
           " and sectarian hate crimes have remained the same,")
    
  }


f14para1 <- paste(f14sentence1, f14sentence2,
                  "it should be noted that increases or decreases in the number of crimes may be attributed to changes in reporting; detection; or confidence in the police.")
    