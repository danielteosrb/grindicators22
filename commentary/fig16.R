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