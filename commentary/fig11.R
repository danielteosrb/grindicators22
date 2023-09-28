neighbourhood <- round2(dataNew$f11_neighbourhood)
workplace <- round2(dataNew$f11_workplace)

f11Sig <- significanceTest(p1 = neighbourhood,
                           n1 = dataNew$NGHCULT2_n,
                           p2 = workplace,
                           n2 = dataNew$WORKCULT2_n)

f11SigStatement <- if (f11Sig == "significant increase") {
  paste0(neighbourhood, 
         "% of respondents define their neighbourhood as somewhere they can be open about their cultural identity, while significantly fewer say this about their workplace (",
         workplace, "%).")
} else if (f11Sig == "significant decrease") {
  paste0(workplace, 
         "% of respondents define their workplace as somewhere they can be open about their cultural identity, while significantly fewer say this about their neighbourhood (",
         neighbourhood, "%).")
} else {
  paste0(neighbourhood, 
         "% of respondents define their neighbourhood as somewhere they can be open about their cultural identity. There is no significant difference in the proportion of people who say this about their workplace (",
         workplace, "%).")
}

workDifflast <- round2(dataNew$f11_workplace - dataOld$f11_workplace)
nghDifflast <- round2(dataNew$f11_neighbourhood - dataOld$f11_neighbourhood)

workSiglast <- significanceTest(p1 = dataNew$f11_workplace,
                            n1 = dataNew$WORKCULT2_n,
                            p2 = dataOld$f11_workplace,
                            n2 = dataOld$WORKCULT2_n)

nghSiglast <- significanceTest(p1 = dataNew$f11_neighbourhood,
                           n1 = dataNew$NGHCULT2_n,
                           p2 = dataOld$f11_neighbourhood,
                           n2 = dataOld$NGHCULT2_n)

f11para1 <-
  # Both significant
  if (workSiglast != FALSE & nghSiglast != FALSE) {
    paste0("Since ", NILTyear - 1, ", the proportion who define their workplace as a shared space has seen a ",
           workSiglast," of ", abs(workDifflast)," percentage points (", NILTyear, ": ", workplace, "%; ", NILTyear - 1, ": ", round2(dataOld$f11_workplace),
           "%). Those who say the same of their neighbourhood has seen a ", nghSiglast, " of ", abs(nghDifflast), " percentage points since ", NILTyear - 1, " (", NILTyear, ": ", neighbourhood, "%; ", NILTyear - 1, ": ", round2(dataOld$f11_neighbourhood), "%).")
    # Only workplace significant
  } else if (workSiglast != FALSE & nghSiglast == FALSE) {
    paste0("Since ", NILTyear - 1, ", the proportion who define their workplace as a shared space has seen a ",
           workSiglast," of ", abs(workDifflast)," percentage points (", NILTyear, ": ", workplace, "%; ", NILTyear - 1, ": ", round2(dataOld$f11_workplace),
           "%). There has been no significant change in those who say the same of their neighbourhood since ", NILTyear - 1, ".")
    # Only neighbourhood significant
  } else if (workSiglast == FALSE & nghSiglast != FALSE) {
    paste0("Since ", NILTyear - 1, ", the proportion who define their neighbourhood as a shared space has seen a ", nghSiglast, " of ", abs(nghDifflast), " percentage points since ", NILTyear - 1,
           " (", NILTyear, ": ", neighbourhood, "%; ", NILTyear - 1, ": ", round2(dataOld$f11_neighbourhood),
           "%). There has been no significant change in those who say the same about their workplace since ", NILTyear - 1, ".")
    # Neither significant
  } else if (workSiglast == FALSE & nghSiglast == FALSE) {
    paste0("Since ", NILTyear - 1, ", the proportions who define their workplace or their neighbourhood as a shared space have seen no significant change.")
  }

workDiff <- round2(dataNew$f11_workplace - data$f11_workplace[data$year == 2014])
nghDiff <- round2(dataNew$f11_neighbourhood - data$f11_neighbourhood[data$year == 2014])

workSig <- significanceTest(p1 = dataNew$f11_workplace,
                            n1 = dataNew$WORKCULT2_n,
                            p2 = data$f11_workplace[data$year == 2014],
                            n2 = data$WORKCULT2_n[data$year == 2014])

nghSig <- significanceTest(p1 = dataNew$f11_neighbourhood,
                           n1 = dataNew$NGHCULT2_n,
                           p2 = data$f11_neighbourhood[data$year == 2014],
                           n2 = data$NGHCULT2_n[data$year == 2014])

f11para2 <-
  # Both significant
  if (workSig != FALSE & nghSig != FALSE) {
    paste0("Since this question was first asked, in 2014, the proportion who define their workplace as a shared space has seen a ",
           workSig," of ", abs(workDiff)," percentage points (", NILTyear, ": ", workplace, "%; 2014: ", round2(data$f11_workplace[data$year == 2014]),
           "%). Those who say the same of their neighbourhood is consistently ",
           if (nghSig == "significant increase") {"higher"} else {"lower"},
           " and has seen a ", abs(nghDiff), " percentage point ",
           sub("significant ", "", nghSig, fixed = TRUE), " (", NILTyear, ": ", neighbourhood, "%; 2014: ", round2(data$f11_neighbourhood[data$year == 2014]),
           "%).")
    # Only workplace significant
  } else if (workSig != FALSE & nghSig == FALSE) {
    paste0("Since this question was first asked, in 2014, the proportion who define their workplace as a shared space has seen a ",
           workSig," of ", abs(workDiff)," percentage points (", NILTyear, ": ", workplace, "%; 2014: ", round2(data$f11_workplace[data$year == 2014]),
           "%). There has been no significant change in those who say the same of their neighbourhood since 2014.")
   # Only neighbourhood significant
  } else if (workSig == FALSE & nghSig != FALSE) {
    paste0("Since this question was first asked, in 2014, the proportion who define their neighbourhood as a shared space is consistently ",
           if (nghSig == "significant increase") {"higher"} else {"lower"},
           " and has seen a ", abs(nghDiff), " percentage point ",
           sub("significant ", "", nghSig, fixed = TRUE), " (", NILTyear, ": ", neighbourhood, "%; 2014: ", round2(data$f11_neighbourhood[data$year == 2014]),
           "%). There has been no significant change in those who say the same about their workplace since 2014.")
    # Neither significant
  } else if (workSig == FALSE & nghSig == FALSE) {
    paste0("Since this question was first asked, in 2014, the proportions who define their workplace or their neighbourhood as a shared space have seen no significant change.")
  }