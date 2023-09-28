workplace <- round2(dataNew$f12_workplace)
neighbourhood <- round2(dataNew$f12_neighbourhood)
school <- round2(dataNew$f12_school)

nghworkSig <- significanceTest(p1 = dataNew$f12_neighbourhood,
                               n1 = dataNew$MXRLGNGH_n,
                               p2 = dataNew$f12_workplace,
                               n2 = dataNew$MXRLGWRK_n)

nghschoolSig <- significanceTest(p1 = dataNew$f12_neighbourhood,
                                 n1 = dataNew$MXRLGNGH_n,
                                 p2 = dataNew$f12_school,
                                 n2 = dataNew$OWNMXSCH_n)

workschoolSig <- significanceTest(p1 = dataNew$f12_workplace,
                                  n1 = dataNew$MXRLGWRK_n,
                                  p2 = dataNew$f12_school,
                                  n2 = dataNew$OWNMXSCH_n)

f12cysentence1 <- if (nghworkSig != FALSE & nghschoolSig != FALSE & workschoolSig == nghschoolSig) {
  paste0("Preference for mixed religion workplaces among all survey respondents is currently ", workplace,
         "%. This is significantly ", if (nghworkSig == "significant increase") {"lower"} else {"higher"}, " than the proportion of all survey respondents who prefer to live in a mixed religion neighbourhood (", neighbourhood,
         "%). A significantly ", if (nghschoolSig == "significant increase") {"lower"} else {"higher"}, " proportion of all survey respondents would prefer to send their children to a mixed religion school (", school, "%) than work in a mixed religion workplace or live in a mixed religion neighbourhood.")
}

noSchool <- round2(colPct(NILT, OWNMXSCH, "Mixed religion school", religion = "No religion"))
cathSchool <- round2(colPct(NILT, OWNMXSCH, "Mixed religion school", religion = "Catholic"))
protSchool <- round2(colPct(NILT, OWNMXSCH, "Mixed religion school", religion = "Protestant"))

protcathschoolSig <- significanceTest(p1 = cathSchool,
                                      n1 = unweighted_n(NILT$OWNMXSCH[NILT$RELIGCAT == "Catholic"]),
                                      p2 = protSchool,
                                      n2 = unweighted_n(NILT$OWNMXSCH[NILT$RELIGCAT == "Protestant"]))

protnoschoolSig <- significanceTest(p1 = protSchool,
                                    n1 = unweighted_n(NILT$OWNMXSCH[NILT$RELIGCAT == "Protestant"]),
                                    p2 = noSchool,
                                    n2 = unweighted_n(NILT$OWNMXSCH[NILT$RELIGCAT == "No religion"]))

cathnoschoolSig <- significanceTest(p1 = cathSchool,
                                    n1 = unweighted_n(NILT$OWNMXSCH[NILT$RELIGCAT == "Catholic"]),
                                    p2 = noSchool,
                                    n2 = unweighted_n(NILT$OWNMXSCH[NILT$RELIGCAT == "No religion"]))


f12cysentence2 <- if (protnoschoolSig != FALSE & cathnoschoolSig == protnoschoolSig) {
  paste0("A significantly ", if (protnoschoolSig == "significant increase") {"lower"} else {"higher"}, " proportion of those with no religion (", noSchool,
         "%) would prefer mixed religion schools when compared with Catholic respondents (",
         cathSchool, "%) and Protestant respondents (", protSchool, "%).")
}

# How we got here

nghDiff <- round2(dataNew$f12_neighbourhood - data$f12_neighbourhood[data$year == 2013])
workDiff <- round2(dataNew$f12_workplace - data$f12_workplace[data$year == 2013])
schoolDiff <- round2(dataNew$f12_school - data$f12_school[data$year == 2013])

nghSig <- significanceTest(p1 = dataNew$f12_neighbourhood,
                           n1 = dataNew$MXRLGNGH_n,
                           p2 = dataOld$f12_neighbourhood,
                           n2 = dataOld$MXRLGNGH_n)

workSig <- significanceTest(p1 = dataNew$f12_workplace,
                            n1 = dataNew$MXRLGWRK_n,
                            p2 = dataOld$f12_workplace,
                            n2 = dataOld$MXRLGWRK_n)

schoolSig <- significanceTest(p1 = dataNew$f12_school,
                              n1 = dataNew$OWNMXSCH_n,
                              p2 = dataOld$f12_school,
                              n2 = dataOld$OWNMXSCH_n)

f12sentence1 <- if (nghSig == FALSE & workSig == FALSE & schoolSig == FALSE) {
  paste0("There has been little change in preference for mixed religion neighbourhoods, workplaces, and schools, since ",
         NILTyear - 1, ".")
} else if (nghSig == FALSE & workSig != FALSE & schoolSig == workSig) {
  paste0("There has been a ", workSig,
         " in preference for mixed religion workplaces (", NILTyear, ": ", round2(dataNew$f12_workplace),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_workplace), "%) and schools (", NILTyear, ": ", round2(dataNew$f12_school),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_school), "%) since ", NILTyear - 1,
         " but there has been no significant change in preference for mixed religion neighbourhoods.")
} else if (workSig == FALSE & nghSig != FALSE & nghSig == schoolSig) {
  paste0("There has been a ", nghSig,
         " in preference for mixed religion neighbourhoods (", NILTyear, ": ", round2(dataNew$f12_neighbourhood),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_neighbourhood), "%) and schools (", NILTyear, ": ", round2(dataNew$f12_school),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_school), "%) since ", NILTyear - 1,
         " but there has been no significant change in preference for mixed religion workplaces.")
} else if (schoolSig == FALSE & nghSig != FALSE & nghSig == workSig) {
  paste0("There has been a ", nghSig,
         " in preference for mixed religion neighbourhoods (", NILTyear, ": ", round2(dataNew$f12_neighbourhood),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_neighbourhood), "%) and workplaces (", NILTyear, ": ", round2(dataNew$f12_workplace),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_workplace), "%) since ", NILTyear - 1,
         " but there has been no significant change in preference for mixed religion schools.")
} else if (nghSig != FALSE & nghSig == workSig & nghSig == schoolSig) {
  paste0("There has been a ", nghSig,
         " in preference for mixed religion neighbourhoods (", NILTyear, ": ", round2(dataNew$f12_neighbourhood),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_neighbourhood), "%), workplaces (", NILTyear, ": ", round2(dataNew$f12_workplace),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_workplace), "%) and schools (", NILTyear, ": ", round2(dataNew$f12_school),
         "%; ", NILTyear - 1, ": ", round2(dataOld$f12_school), "%) since ", NILTyear - 1, ".")
}

f12para1 <- paste0(f12sentence1,  " Since 2013, preference for all three has increased significantly – preference for mixed religion neighbourhoods increasing ",
                   abs(nghDiff), " percentage points (", NILTyear, ": ", dataNew$f12_neighbourhood, "%; 2013: ", data$f12_neighbourhood[data$year == 2013], "%), workplaces ", abs(workDiff),
                   " percentage points (", NILTyear, ": ", dataNew$f12_workplace, "%; 2013: ", data$f12_workplace[data$year == 2013], "%), and preferences for mixed religion schools increasing ",
                   abs(schoolDiff), " percentage points (", NILTyear, ": ", dataNew$f12_school, "%; 2013: ", data$f12_school[data$year == 2013], "%).")