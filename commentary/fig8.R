f8 <- round2(dataNew$f8_pct)

f8Diff <- abs(round2(dataNew$f8_pct - dataOld$f8_pct))

f8Sig <- significanceTest(p1 = dataNew$f8_pct,
                          n1 = dataNew$SCHLCULT2_n,
                          p2 = dataOld$f8_pct,
                          n2 = dataOld$SCHLCULT2_n)

f8SigCP <- significanceTest(p1 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "Catholic"),
                            n1 = unweighted_n(NILT$SCHLCULT2[NILT$RELIGCAT == "Catholic"]),
                            p2 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "Protestant"),
                            n2 = unweighted_n(NILT$SCHLCULT2[NILT$RELIGCAT == "Protestant"]))

f8SigCNR <- significanceTest(p1 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "Catholic"),
                            n1 = unweighted_n(NILT$SCHLCULT2[NILT$RELIGCAT == "Catholic"]),
                            p2 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "No religion"),
                            n2 = unweighted_n(NILT$SCHLCULT2[NILT$RELIGCAT == "No religion"]))

f8SigPNR <- significanceTest(p1 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "Protestant"),
                             n1 = unweighted_n(NILT$SCHLCULT2[NILT$RELIGCAT == "Protestant"]),
                             p2 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "No religion"),
                             n2 = unweighted_n(NILT$SCHLCULT2[NILT$RELIGCAT == "No religion"]))

f8Siggen <- significanceTest(p1 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), gender = "Male"),
                             n1 = unweighted_n(NILT$SCHLCULT2[NILT$RSEX == "Male"]),
                             p2 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), gender = "Female"),
                             n2 = unweighted_n(NILT$SCHLCULT2[NILT$RSEX == "Female"]))

f8religionsentence <- if (f8SigCP == FALSE & f8SigCNR == f8SigCP & f8SigPNR == f8SigCP) {
  pasteo("There are no differences in responses when analysed by religion.")
} else if (f8SigCNR != FALSE & f8SigPNR == f8SigCNR & f8SigCP == FALSE) {
  paste0("Respondents with no religion ", if (f8SigCNR == "significant increase") {"less"} else {"more"}, " often report that their child's school is somewhere their children can be open about their cultural identity than either Catholic or Protestant respondents. However, there is no significant difference between the proportion of Catholic and Protestant respondents who report this.")
}

f8gendersentence <- if (f8Siggen != FALSE) {
  paste0("Women are significantly ", if (f8Siggen == "significant decrease") {"more"} else {"less"}, " likely than men to report that their child's school is somewhere their children can be open about their cultural identity.")
} else {
  paste0("There are ", if (f8SigCP == FALSE & f8SigCNR == f8SigCP & f8SigPNR == f8SigCP) {"also "}, "no differences in responses when analysed by gender.")
}

f8para1 <- if (f8Sig != FALSE) {
  paste0("Since ", NILTyear - 1, ", there has been a ", f8Sig, " in the proportion of those with children at school who think that their child's school is somewhere their children can be open about their cultural identity (", NILTyear , ": ", round2(dataNew$f8_pct), "%; ", NILTyear - 1, ": ", round2(dataOld$f8_pct), "%).")
} else {
  paste0("Since ", NILTyear - 1, ", there has been no significant change in the proportion of those with children at school who think that their child's school is somewhere their children can be open about their cultural identity.")
}