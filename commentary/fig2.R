adultDiff <- dataNew$f2_adults - dataOld$f2_adults

adultSig <- significanceTest(p1 = dataNew$f2_adults,
                             n1 = dataNew$RLRELFUT_n,
                             p2 = dataOld$f2_adults,
                             n2 = dataOld$RLRELFUT_n)



youngDiff <- dataNew$f2_young - dataOld$f2_young

youngSig <- significanceTest(p1 = dataNew$f2_young,
                             n1 = dataNew$RLRELFUT_Yn,
                             p2 = dataOld$f2_young,
                             n2 = dataOld$RLRELFUT_Yn)

# To determine if this is lowest proportion of young people since 2013
youngRank <- if (dataNew$f2_young == min(data$f2_young)) {
  "lowest"
} else {
  paste0(ordinal(rank(data$f2_young)[data$year == YLTyear]), " lowest")
}

# To determine if this is lowest proportion of adults since 2013
adultRank <- if (dataNew$f2_adult == min(data$f2_adult)) {
  "lowest"
} else {
  paste0(ordinal(rank(data$f2_adult)[data$year == NILTyear]), " lowest")
}

rankSentence <- if (youngRank == adultRank) {
  paste0("These are the ", youngRank, " proportions since the launch of the T:BUC strategy in 2013.")
} else if (adultRank == "lowest" & youngRank != "lowest") {
  paste0("For adults this is the lowest proportion since the launch of the T:BUC strategy in 2013, while for young people this is the ", youngRank, ".")
} else if (youngRank == "lowest" & adultRank != "lowest") {
  paste0("For young people this is the lowest proportion since the launch of the T:BUC strategy in 2013, while for adults this is the ", adultRank, ".")
}

f2para1 <- paste0("Since ", NILTyear - 1, ", there has been ",
                  if(adultSig == FALSE) {"no significant change"} else {paste0("a ", adultSig)},
                  if(adultSig != FALSE) {paste0(" (", abs(adultDiff), " percentage points to ", round2(dataNew$f2_adults), "%)")},
                  " in the proportion of adult respondents who think relations will be better in five years' time", if(adultSig !=FALSE) {paste0(" (", NILTyear , ": ", round2(dataNew$f2_adults), "%; ", NILTyear - 1, ": ", round2(dataOld$f2_adults), "%)")},"; the corresponding figure for young people ",
                  if(youngSig == FALSE){if(adultSig ==FALSE){"has also not changed significantly. "} else{"has not changed significantly. "}} else {paste0("has ", sub("significant ", "", youngSig), "d by ", abs(youngDiff), " percentage points", if(youngSig !=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f2_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f2_young))}, "%)")}, ". ",
                  rankSentence)