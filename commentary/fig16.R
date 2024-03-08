annoyedR <- round2(dataNew$f16_aR)
intimidatedR <- round2(dataNew$f16_iR)
annoyedL <- round2(dataNew$f16_aL)
intimidatedL <- round2(dataNew$f16_iL)

f16Sig <- significanceTest(p1 = annoyedR,
                           n1 = dataNew$REPMUR2_n,
                           p2 = annoyedL,
                           n2 = dataNew$LOYMUR2_n)

f16Sig <- if (f16Sig == FALSE) {
  "There was no significant difference in the proportion of"
} else if (f16Sig == "significant decrease") {
  "Significantly more"
} else {
  "Significantly less"
}

loyDiff1 <- abs(dataNew$f16_aL - round2(dataOld$f16_aL))

loyChange1 <- significanceTest(p1 = dataNew$f16_aL,
                               n1 = dataNew$LOYMUR2_n,
                               p2 = round2(dataOld$f16_aL),
                               n2 = dataOld$LOYMUR2_n)

loyChange1 <- if (loyChange1 == FALSE) {
  "no significant change"
} else {
  paste0("a ", loyChange1)
}

repDiff1 <- abs(dataNew$f16_aR - round2(dataOld$f16_aR))

repChange1 <- significanceTest(p1 = dataNew$f16_aR,
                              n1 = dataNew$REPMUR2_n,
                              p2 = round2(dataOld$f16_aR),
                              n2 = dataOld$REPMUR2_n)

repChange1 <- if (repChange1 == FALSE) {
  "no significant change"
} else {
  paste0("a ", repChange1)
}

f16sentence1 <- 
  # Neither significant
  if (repChange1 == loyChange1 & repChange1 == "no significant change") {
    paste0("Since ", NILTyear - 1,
           " there have been no significant changes in the proportions of adults who felt annoyed by either loyalist or republican murals.")
    # Both significant and the same
  } else if (repChange1 == loyChange1 & repChange1 != "no significant change") {
    paste0("Since ", NILTyear - 1, " there has been ", loyChange1, " in the proportion of adults who felt annoyed by loyalist murals (",
           NILTyear, ": ", dataNew$f16_aL, "%; ", NILTyear - 1, ": ", dataOld$f16_aL, "%) and there was also ", repChange1, " in the proportion of adults who felt annoyed by republican murals (",
           NILTyear, ": ", dataNew$f16_aR, "%; ", NILTyear - 1, ": ", dataOld$f16_aR, "%).")
    # Republican not significant and loyalist significant
  } else if (repChange1 == "no significant change" & loyChange1 != "no significant change") {
    paste0("Since ", NILTyear - 1, " there has been ", loyChange1, " in the proportion of adults who felt annoyed by loyalist murals (",
           NILTyear, ": ", dataNew$f16_aL, "%; ", NILTyear - 1, ": ", dataOld$f16_aL, "%), however there was no significant change in the proportion of adults who felt annoyed by republican murals.")
    # Republican significant and loyalist not significant
  } else if (repChange1 != "no significant change" & loyChange1 == "no significant change") {
    paste0("Since ", NILTyear - 1, " there has been ", repChange1, " in the proportion of adults who felt annoyed by republican murals (",
           NILTyear, ": ", dataNew$f16_aR, "%; ", NILTyear - 1, ": ", dataOld$f16_aR, "%) however there was no significant change in the proportion of adults who felt annoyed by loyalist murals.")
    # Both significant but different
  } else if (repChange1 != "no significant change" & loyChange1 != "no significant change" & repChange1 != loyChange1) {
    paste0("Since ", NILTyear - 1, " there has been ", loyChange1, " in the proportion of adults who felt annoyed by loyalist murals (",
           NILTyear, ": ", dataNew$f16_aL, "%; ", NILTyear - 1, ": ", dataOld$f16_aL, "%) however there was ", repChange1, " in the proportion of adults who felt annoyed by republican murals (",
           NILTyear, ": ", dataNew$f16_aR, "%; ", NILTyear - 1, ": ", dataOld$f16_aR, "%).")
  }

loyDiff2 <- abs(round2(dataNew$f16_iL - dataOld$f16_iL))

loyChange2 <- significanceTest(p1 = dataNew$f16_iL,
                               n1 = dataNew$LOYMURAL_n,
                               p2 = round2(dataOld$f16_iL),
                               n2 = dataOld$LOYMURAL_n)

loyChange2 <- if (loyChange2 == FALSE) {
  "no significant change"
} else {
  paste0("a ", loyChange2)
}

repDiff2 <- abs(round2(dataNew$f16_iR - dataOld$f16_iR))

repChange2 <- significanceTest(p1 = dataNew$f16_iR,
                               n1 = dataNew$REPMURAL_n,
                               p2 = round2(dataOld$f16_iR),
                               n2 = dataOld$REPMURAL_n)

repChange2 <- if (repChange2 == FALSE) {
  "no significant change"
} else {
  paste0("a ", repChange2)
}

f16sentence2 <-
  if (repChange2 == loyChange2 & repChange2 == "no significant change") {
    if (repChange1 == loyChange1 & repChange1 == "no significant change") {
      "There have also been no significant changes in the proportions of adults who felt intimidated by either loyalist or republican murals."
    } else {
      "There were no significant changes in the proportions of adults who felt intimidated by either loyalist or republican murals."
    }
    # Both significant and the same
  } else if (repChange2 == loyChange2 & repChange2 != "no significant change") {
    paste0("There has been ", loyChange2, " in the proportion of adults who felt intimidated by loyalist murals (",
           NILTyear, ": ", dataNew$f16_iL, "%; ", NILTyear - 1, ": ", dataOld$f16_iL, "%) and there was also ", repChange2, " in the proportion of adults who felt intimidated by republican murals (",
           NILTyear, ": ", dataNew$f16_iR, "%; ", NILTyear - 1, ": ", dataOld$f16_iR, "%).")
    # Republican not significant and loyalist significant
  } else if (repChange2 == "no significant change" & loyChange2 != "no significant change") {
    paste0("There has been ", loyChange2, " in the proportion of adults who felt intimidated by loyalist murals (",
           NILTyear, ": ", dataNew$f16_iL, "%; ", NILTyear - 1, ": ", dataOld$f16_iL, "%), however there was no significant change in the proportion of adults who felt intimidated by republican murals.")
    # Republican not significant and loyalist significant
  } else if (repChange2 != "no significant change" & loyChange2 == "no significant change") {
    paste0("There has been ", repChange2, " in the proportion of adults who felt intimidated by republican murals (",
           NILTyear, ": ", dataNew$f16_iR, "%; ", NILTyear - 1, ": ", dataOld$f16_iR, "%) however there was no significant change in the proportion of adults who felt intimidated by loyalist murals.")
    # Both significant but different
  } else if (repChange2 != "no significant change" & loyChange2 != "no significant change" & repChange != loyChange2) {
    paste0("There has been ", loyChange2, " in the proportion of adults who felt intimidated by loyalist murals (",
           NILTyear, ": ", dataNew$f16_iL, "%; ", NILTyear - 1, ": ", dataOld$f16_iL, "%) however there was ", repChange2, " in the proportion of adults who felt intimidated by republican murals (",
           NILTyear, ": ", dataNew$f16_iR, "%; ", NILTyear - 1, ": ", dataOld$f16_iR, "%).")
  }

f16para1 <- paste(f16sentence1, f16sentence2)

loyDiff3 <- abs(dataNew$f16_aL - round2(data$f16_aL[data$year == 2013]))

loyChange3 <- significanceTest(p1 = dataNew$f16_aL,
                               n1 = dataNew$LOYMUR2_n,
                               p2 = round2(data$f16_aL[data$year == 2013]),
                               n2 = data$LOYMUR2_n[data$year == 2013])

loyChange3 <- if (loyChange3 == FALSE) {
  "no significant change"
} else {
  paste0("a ", loyChange3)
}

repDiff3 <- abs(dataNew$f16_aR - round2(dataOld$f16_aR))

repChange3 <- significanceTest(p1 = dataNew$f16_aR,
                               n1 = dataNew$REPMUR2_n,
                               p2 = round2(data$f16_aR[data$year == 2013]),
                               n2 = data$REPMUR2_n[data$year == 2013])

repChange3 <- if (repChange3 == FALSE) {
  "no significant change"
} else {
  paste0("a ", repChange3)
}

f16sentence3 <- 
  # Neither significant
  if (repChange3 == loyChange3 & repChange3 == "no significant change") {
    paste0("Since ", NILTyear - 1,
           " there have been no significant changes in the proportions of adults who felt annoyed by either loyalist or republican murals.")
    # Both significant and the same
  } else if (repChange3 == loyChange3 & repChange3 != "no significant change") {
    paste0("Since 2014 there has been ", loyChange3, " in the proportion of adults who felt annoyed by loyalist murals (",
           NILTyear, ": ", dataNew$f16_aL, "%; 2014: ", data$f16_aL[data$year == 2013], "%) and there was also ", repChange3, " in the proportion of adults who felt annoyed by republican murals (",
           NILTyear, ": ", dataNew$f16_aR, "%; 2014: ", data$f16_aR[data$year == 2013], "%).")
    # Republican not significant and loyalist significant
  } else if (repChange3 == "no significant change" & loyChange3 != "no significant change") {
    paste0("Since 2014 there has been ", loyChange3, " in the proportion of adults who felt annoyed by loyalist murals (",
           NILTyear, ": ", dataNew$f16_aL, "%; 2014: ", data$f16_aL[data$year == 2013], "%), however there was no significant change in the proportion of adults who felt annoyed by republican murals.")
    # Republican significant and loyalist not significant
  } else if (repChange3 != "no significant change" & loyChange3 == "no significant change") {
    paste0("Since 2014 there has been ", repChange3, " in the proportion of adults who felt annoyed by republican murals (",
           NILTyear, ": ", dataNew$f16_aR, "%; 2014: ", data$f16_aR[data$year == 2013], "%) however there was no significant change in the proportion of adults who felt annoyed by loyalist murals.")
    # Both significant but different
  } else if (repChange3 != "no significant change" & loyChange3 != "no significant change" & repChange3 != loyChange3) {
    paste0("Since 2014 there has been ", loyChange3, " in the proportion of adults who felt annoyed by loyalist murals (",
           NILTyear, ": ", dataNew$f16_aL, "%; 2014: ", data$f16_aL[data$year == 2013], "%) however there was ", repChange3, " in the proportion of adults who felt annoyed by republican murals (",
           NILTyear, ": ", dataNew$f16_aR, "%; 2014: ", data$f16_aR[data$year == 2013], "%).")
  }

loyDiff4 <- abs(round2(dataNew$f16_iL - data$f16_iL[data$year == 2013]))

loyChange4 <- significanceTest(p1 = dataNew$f16_iL,
                               n1 = dataNew$LOYMURAL_n,
                               p2 = round2(data$f16_iL[data$year == 2013]),
                               n2 = data$LOYMURAL_n[data$year == 2013])

loyChange4 <- if (loyChange4 == FALSE) {
  "no significant change"
} else {
  paste0("a ", loyChange4)
}

repDiff4 <- abs(round2(dataNew$f16_iR - data$f16_iR[data$year == 2013]))

repChange4 <- significanceTest(p1 = dataNew$f16_iR,
                               n1 = dataNew$REPMURAL_n,
                               p2 = round2(data$f16_iR[data$year == 2013]),
                               n2 = data$REPMURAL_n[data$year == 2013])

repChange4 <- if (repChange4 == FALSE) {
  "no significant change"
} else {
  paste0("a ", repChange4)
}

f16sentence4 <-
  if (repChange4 == loyChange4 & repChange4 == "no significant change") {
    if (repChange3 == loyChange3 & repChange3 == "no significant change") {
      "There have also been no significant changes in the proportions of adults who felt intimidated by either loyalist or republican murals."
    } else {
      "There were no significant changes in the proportions of adults who felt intimidated by either loyalist or republican murals."
    }
    # Both significant and the same
  } else if (repChange4 == loyChange4 & repChange4 != "no significant change") {
    paste0("There has been ", loyChange4, " in the proportion of adults who felt intimidated by loyalist murals (",
           NILTyear, ": ", dataNew$f16_iL, "%; ", NILTyear - 1, ": ", data$f16_iL[data$year == 2013], "%) and there was also ", repChange4, " in the proportion of adults who felt intimidated by republican murals (",
           NILTyear, ": ", dataNew$f16_iR, "%; ", NILTyear - 1, ": ", data$f16_iR[data$year == 2013], "%).")
    # Republican not significant and loyalist significant
  } else if (repChange4 == "no significant change" & loyChange4 != "no significant change") {
    paste0("There has been ", loyChange4, " in the proportion of adults who felt intimidated by loyalist murals (",
           NILTyear, ": ", dataNew$f16_iL, "%; ", NILTyear - 1, ": ", data$f16_iL[data$year == 2013], "%), however there was no significant change in the proportion of adults who felt intimidated by republican murals.")
    # Republican not significant and loyalist significant
  } else if (repChange4 != "no significant change" & loyChange4 == "no significant change") {
    paste0("There has been ", repChange4, " in the proportion of adults who felt intimidated by republican murals (",
           NILTyear, ": ", dataNew$f16_iR, "%; ", NILTyear - 1, ": ", data$f16_iR[data$year == 2013], "%) however there was no significant change in the proportion of adults who felt intimidated by loyalist murals.")
    # Both significant but different
  } else if (repChange4 != "no significant change" & loyChange4 != "no significant change" & repChange != loyChange4) {
    paste0("There has been ", loyChange4, " in the proportion of adults who felt intimidated by loyalist murals (",
           NILTyear, ": ", dataNew$f16_iL, "%; ", NILTyear - 1, ": ", data$f16_iL[data$year == 2013], "%) however there was ", repChange4, " in the proportion of adults who felt intimidated by republican murals (",
           NILTyear, ": ", dataNew$f16_iR, "%; ", NILTyear - 1, ": ", data$f16_iR[data$year == 2013], "%).")
  }

f16para2 <- "Since 2013, there has been a significant increase across all categories: adults who felt annoyed by loyalist murals (2022: 55%; 2014: 33%), adults who felt annoyed by republican murals (2022: 40%; 2014: 22%), adults who felt intimidated by loyalist murals (2022: 39%; 2021: 20%) and adults who felt intimidated by republican murals (2022: 28%; 2021: 10%)."