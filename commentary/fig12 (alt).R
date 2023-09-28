workplace <- round2(dataNew$f12_workplace)
neighbourhood <- round2(dataNew$f12_neighbourhood)
school <- round2(dataNew$f12_school)

noSchool <- round2(colPct(NILT, OWNMXSCH, "Mixed religion school", religion = "No religion"))
cathSchool <- round2(colPct(NILT, OWNMXSCH, "Mixed religion school", religion = "Catholic"))
protSchool <- round2(colPct(NILT, OWNMXSCH, "Mixed religion school", religion = "Protestant"))

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
         " in preference for mixed religion workplaces (",
         round2(abs(dataNew$f12_workplace - dataOld$f12_workplace)), " percentage points) and schools (",
         round2(abs(dataNew$f12_school - dataOld$f12_school))," percentage points) since ", NILTyear - 1,
         " but there has been no significant change in preference for mixed religion neighbourhoods.")
} else if (workSig == FALSE & nghSig != FALSE & nghSig == schoolSig) {
  paste0("There has been a ", nghSig,
         " in preference for mixed religion neighbourhoods (",
         round2(abs(dataNew$f12_neighbourhood - dataOld$f12_neighbourhood)), " percentage points) and schools (",
         round2(abs(dataNew$f12_school - dataOld$f12_school))," percentage points) since ", NILTyear - 1,
         " but there has been no significant change in preference for mixed religion workplaces.")
} else if (schoolSig == FALSE & nghSig != FALSE & nghSig == workSig) {
  paste0("There has been a ", nghSig,
         " in preference for mixed religion neighbourhoods (",
         round2(abs(dataNew$f12_neighbourhood - dataOld$f12_neighbourhood)), " percentage points) and workplaces (",
         round2(abs(dataNew$f12_work - dataOld$f12_work))," percentage points) since ", NILTyear - 1,
         " but there has been no significant change in preference for mixed religion schools.")
} else if (nghSig != FALSE & nghSig == workSig & nghSig == schoolSig) {
  paste0("There has been a ", nghSig,
         " in preference for mixed religion neighbourhoods (",
         round2(abs(dataNew$f12_neighbourhood - dataOld$f12_neighbourhood)), " percentage points), workplaces (",
         round2(abs(dataNew$f12_work - dataOld$f12_work))," percentage points) and schools (",
         round2(abs(dataNew$f12_school - dataOld$f12_school))," percentage points) since ", NILTyear - 1, ".")
}

f12para1 <- paste0(f12sentence1,  " Since 2013, preference for all three has increased significantly – preference for mixed religion neighbourhoods increasing ",
                   abs(nghDiff), " percentage points, workplaces ", abs(workDiff),
                   " percentage points, and preferences for mixed religion schools increasing ",
                   abs(schoolDiff), " percentage points.")