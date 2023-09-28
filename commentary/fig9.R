sharedPct <- function(activity) {
  round2(omniData[[activity]][omniData$year == omniYear])
}

schools <- sharedPct("yes")
projects <- sharedPct("Projects")
resources <- sharedPct("Resources")
classes <- sharedPct("Classes\n(other than\nentitlement\nframework)")
extra <- sharedPct("Extra-\ncurricular\nactivities")
classes2 <- sharedPct("Classes\n(entitlement\nframework*)")
shared <- sharedPct("Development\nof Shared\nPolicies")
teachers <- sharedPct("Teachers")
equipment <- sharedPct("Equipment")
other <- sharedPct("Other")

previousOmni <- tail(omniData$year, 2)[1]

sentence1 <- if (omniData$yes[omniData$year == omniYear] > omniData$yes[omniData$year == previousOmni]) {
  paste0("Since ", previousOmni, " there has been a ",
         round2(omniData$yes[omniData$year == omniYear] - omniData$yes[omniData$year == previousOmni]),
         " percentage point increase in the proportion of schools involved in shared education (",
         omniYear,": ", omniData$yes[omniData$year == omniYear], "%; ", previousOmni, ": ", omniData$yes[omniData$year == previousOmni], "%).")
} else if (omniData$yes[omniData$year == omniYear] < omniData$yes[omniData$year == previousOmni]) {
  paste0("Since ", previousOmni, " there has been a ",
         abs(round2(omniData$yes[omniData$year == omniYear] - omniData$yes[omniData$year == previousOmni])),
         " percentage point decrease in the proportion of schools involved in shared education (",
         omniYear,": ", omniData$yes[omniData$year == omniYear], "%; ", previousOmni, ": ", omniData$yes[omniData$year == previousOmni], "%).")
} else {
  paste0("Since ", previousOmni,
         " there has been no change in the proportion of schools involved in shared education.")
}

mainAreas <- arrange(fig9, desc(as.character(omniYear))) %>%
  head(2) %>%
  mutate(activity2 = case_when(activity == "Projects" ~ "done projects",
                               activity == "Resources" ~ "shared resources"))

main1 <- mainAreas[as.character(omniYear)][1,]
main1Last <- mainAreas[as.character(previousOmni)][1,]

main2 <- mainAreas[as.character(omniYear)][2,]
main2Last <- mainAreas[as.character(previousOmni)][2,]

sentence2 <- paste0("The two main areas in which education is shared is in schools that have ",
                    mainAreas$activity2[1], " with other schools (", omniYear, ": ", main1, "%; ",
                    previousOmni, ": ", main1Last, "%) and ", mainAreas$activity2[2]," (", omniYear, ": ", main2, "%; ",
                    previousOmni, ": ", main2Last, "%).")

# Below is code for an alternate way of phrasing the "How we got here" section - to use, un-comment code below and comment out lines 44-47 above
# Alternate phrasing will read as below:
  
# Since *previous year* there has been a x percentage point increase in the proportion of schools involved in shared education (x% in *previous year* to X% in *current year*). 
# The two main areas in which education is shared is in schools that have done projects with other schools (x%, up x percentage points from *previous year*). 

# sentence2 <- paste0("The two main areas in which education is shared is in schools that have ",
#                     mainAreas$activity2[1], " with other schools (", main1, "%, ",
#                     if(main1 > main1Last) {"up"} else {"down"}, " ", abs(round2(main1 - main1Last)),
#                     " percentage points from ", previousOmni, ") and ", mainAreas$activity2[2]," (", main2, "%, ",
#                     if(main2 > main2Last) {"up"} else {"down"}," ", abs(round2(main2 - main2Last)),
#                     " percentage points from ", previousOmni, ").")


f9para1 <- paste(sentence1, sentence2)