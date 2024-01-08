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
  paste0("For adults this is the lowest proportion since the launch of the T:BUC strategy in 2013, whilst for young people this is the ", youngRank, ".")
} else if (youngRank == "lowest" & adultRank != "lowest") {
  paste0("For young people this is the lowest proportion since the launch of the T:BUC strategy in 2013, whilst for adults this is the ", adultRank, ".")
}

f2para1 <- paste0("Since ", NILTyear - 1, ", there has been ",
                  if(adultSig == FALSE) {"no significant change"} else {paste0("a ", adultSig)},
                  if(adultSig != FALSE) {paste0(" (", abs(adultDiff), " percentage points to ", round2(dataNew$f2_adults), "%)")},
                  " in the proportion of adult respondents who think relations will be better in five years' time", if(adultSig !=FALSE) {paste0(" (", NILTyear , ": ", round2(dataNew$f2_adults), "%; ", NILTyear - 1, ": ", round2(dataOld$f2_adults), "%)")},"; the corresponding figure for young people ",
                  if(youngSig == FALSE){if(adultSig ==FALSE){"has also not changed significantly. "} else{"has not changed significantly. "}} else {paste0("has ", sub("significant ", "", youngSig), "d by ", abs(youngDiff), " percentage points", if(youngSig !=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f2_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f2_young))}, "%)")}, ". ",
                  rankSentence)

# First sentence of second paragraph:
adultProtSig <- significanceTest(p1 = dataNew$f2a_Prot,
                                 n1 = dataNew$RLRELFUT_nProt,
                                 p2 = dataOld$f2a_Prot,
                                 n2 = dataOld$RLRELFUT_nProt)

youngProtSig <- significanceTest(p1 = dataNew$f2y_Prot,
                                 n1 = dataNew$RLRELFUT_YnProt,
                                 p2 = dataOld$f2y_Prot,
                                 n2 = dataOld$RLRELFUT_YnProt)

protSig <-
  # Both significant and the same
  if(adultProtSig != FALSE & adultProtSig == youngProtSig) {
    paste0("The proportion of both adults and young people identifying as Protestant, who think relations between Protestants and Catholics will be better in five years' time, has ", sub("significant ", "", adultProtSig), "d by ",
           abs(dataNew$f2a_Prot - dataOld$f2a_Prot)," percentage points (", NILTyear, ": ", dataNew$f2a_Prot, "%; ", NILTyear - 1, ": ", dataOld$f2a_Prot, "%) and ", abs(dataNew$f2y_Prot - dataOld$f2y_Prot), " percentage points (",
           YLTyear, ": ", dataNew$f2y_Prot, "%; ", YLTyear - 1, ": ", dataOld$f2y_Prot, "%) respectively, from ", LTyear - 1, " to ", LTyear, ".")
    # Adult not significant and young people significant change
  } else if (adultProtSig == FALSE & youngProtSig != FALSE) {
    paste0("From ", LTyear - 1, " to ", LTyear, " there was no significant change in the proportion of adults identifying as Protestant who think relations between Protestants and Catholics will be better in five years' time, whilst there was a ", youngProtSig, " of ", abs(dataNew$f2y_Prot - dataOld$f2y_Prot), " percentage points among young people who identify as Protestant (", YLTyear, ": ", dataNew$f2y_Prot, "%; ", YLTyear - 1, ": ", dataOld$f2y_Prot, "%).")
    # Adult significant and young people not significant
  } else if (adultProtSig != FALSE & youngProtSig == FALSE) {
    paste0("From ", LTyear - 1, " to ", LTyear, " there was a ", adultProtSig, " of ", abs(dataNew$f2a_Prot - dataOld$f2a_Prot), " percentage points in the proportion of adults identifying as Protestant, who think relations between Protestants and Catholics will be better in five years' time (", LTyear, ": ", dataNew$f2a_Prot, "%; ", LTyear - 1, ": ", dataOld$f2a_Prot, "%),  whilst there was no significant change in this measure among young people who identify as Protestant.")
    # Neither significant
  } else if (adultProtSig == FALSE & youngProtSig == FALSE) {
    paste0("There was no significant change in the proportion of either adults or young people identifying as Protestant, who think relations between Protestants and Catholics will be better in five years' time, from ", LTyear - 1, " to ", LTyear, ".")
    # Both significant but change in different directions
  } else if(adultProtSig != FALSE & youngProtSig != FALSE & adultProtSig != youngProtSig) {
    paste0("From ", LTyear - 1, " to ", LTyear, " there was a ", adultProtSig, " of ", abs(dataNew$f2a_Prot - dataOld$f2a_Prot), " percentage points in the proportion of adults identifying as Protestant, who think relations between Protestants and Catholics will be better in five years' time (", NILTyear, ": ", dataNew$f2a_Prot, "%; ", NILTyear - 1, ": ", dataOld$f2a_Prot, "%), whilst there was ", youngProtSig, " of ", abs(dataNew$f2y_Prot - dataOld$f2y_Prot), " percentage points in this measure among young people who identify as Protestant (", YLTyear, ": ", dataNew$f2y_Prot, "%; ", YLTyear - 1, ": ", dataOld$f2y_Prot, "%).")
  }

# Second sentence of second paragraph:
adultNoRSig <- significanceTest(p1 = dataNew$f2a_NoR,
                                n1 = dataNew$RLRELFUT_nNoR,
                                p2 = dataOld$f2a_NoR,
                                n2 = dataOld$RLRELFUT_nNoR)

youngNoRSig <- significanceTest(p1 = dataNew$f2y_NoR,
                                n1 = dataNew$RLRELFUT_YnNoR,
                                p2 = dataOld$f2y_NoR,
                                n2 = dataOld$RLRELFUT_YnNoR)


noRSig <-
  # Both significant and the same
  if(adultNoRSig != FALSE & adultNoRSig == youngNoRSig) {
    paste0("There were ", adultNoRSig, "s in the proportions of both adults and young people with no religion (",
           abs(dataNew$f2a_NoR - dataOld$f2a_NoR), " percentage points (", NILTyear, ": ", dataNew$f2a_NoR, "%; ", NILTyear - 1, ": ", dataOld$f2a_NoR, "%) and ",
           abs(dataNew$f2y_NoR - dataOld$f2y_NoR), " percentage points (", YLTyear, ": ", dataNew$f2y_NoR, "%; ", YLTyear - 1, ": ", dataOld$f2y_NoR, "%) respectively) who think relations between Protestants and Catholics will be better in five years' time.")
    # Adult not significant and young people significant change
  } else if (adultNoRSig == FALSE & youngNoRSig != FALSE) {
    paste0("There was no significant change among adult respondents with no religion who think relations between Protestants and Catholics will be better in five years' time. There was a ", youngNoRSig, " (", abs(dataNew$f2y_NoR - dataOld$f2y_NoR), " percentage points) among young people with no religion who think relations between Protestants and Catholics will be better in five years' time (", YLTyear, ": ", dataNew$f2y_NoR, "%; ", YLTyear - 1, ": ", dataOld$f2y_NoR, "%).")
    # Adult significant and young people not significant
  } else if (adultNoRSig != FALSE & youngNoRSig == FALSE) {
    paste0("Adult respondents with no religion who think relations between Protestants and Catholics will be better in five years' time has ", sub("significant", "significantly", adultNoRSig), "d (", round2(abs(dataNew$f2a_NoR - dataOld$f2a_NoR))," percentage points) (", NILTyear, ": ", dataNew$f2a_NoR, "%; ", NILTyear - 1, ": ", dataOld$f2a_NoR, "%). There was no significant change among young people with no religion who think relations between Protestants and Catholics will be better in five years' time.")
    # Neither significant
  } else if (adultNoRSig == FALSE & youngNoRSig == FALSE) {
    paste0("There was no significant change in the proportion of either adults or young people with no religion who think relations between Protestants and Catholics will be better in five years' time.")
    # Both significant but change in different directions
  } else if(adultNoRSig != FALSE & youngNoRSig != FALSE & adultNoRSig != youngNoRSig) {
    paste0("Adult respondents with no religion who think relations between Protestants and Catholics will be better in five years' time has ", adultNoRSig, "d (", abs(dataNew$f2a_NoR - dataOld$f2a_NoR)," percentage points) (", NILTyear, ": ", dataNew$f2a_NoR, "%; ", NILTyear - 1, ": ", dataOld$f2a_NoR, "%). There was a ", youngNoRSig, " (", abs(dataNew$f2y_NoR - dataOld$f2y_NoR), " percentage points) among young people with no religion who think relations between Protestants and Catholics will be better in five years' time (", YLTyear, ": ", dataNew$f2y_NoR, "%; ", YLTyear - 1, ": ", dataOld$f2y_NoR, "%).")
  }

adultCathSig <- significanceTest(p1 = dataNew$f2a_Cath,
                                 n1 = dataNew$RLRELFUT_nCath,
                                 p2 = dataOld$f2a_Cath,
                                 n2 = dataOld$RLRELFUT_nCath)

youngCathSig <- significanceTest(p1 = dataNew$f2y_Cath,
                                 n1 = dataNew$RLRELFUT_YnCath,
                                 p2 = dataOld$f2y_Cath,
                                 n2 = dataOld$RLRELFUT_YnCath)
# Third sentence of second paragraph:

cathSig <-
  # Both significant and the same
  if(adultCathSig != FALSE & adultCathSig == youngCathSig) {
    paste0("For adults and young people who identify as Catholic there were ", adultCathSig, "s of ", abs(dataNew$f2a_Cath - dataOld$f2a_Cath)," percentage points (", NILTyear, ": ", dataNew$f2a_Cath, "%; ", NILTyear - 1, ": ", dataOld$f2a_Cath, "%) and ", abs(dataNew$f2y_Cath - dataOld$f2y_Cath)," percentage points (", YLTyear, ": ", dataNew$f2y_Cath, "%; ", YLTyear - 1, ": ", dataOld$f2y_Cath, "%) respectively in the proportion who think relations between Protestants and Catholics will be better in five years' time.")
    # Adult not significant and young people significant change
  } else if (adultCathSig == FALSE & youngCathSig != FALSE) {
    paste0("There was no significant change among adult respondents identifying as Catholic who think relations between Protestants and Catholics will be better in five years' time. Among young people who identify as Catholic there was a ", youngCathSig, " of ", abs(dataNew$f2y_Cath - dataOld$f2y_Cath)," percentage points (", YLTyear, ": ", dataNew$f2y_Cath, "%; ", YLTyear - 1, ": ", dataOld$f2y_Cath, "%).")
    # Adult significant and young people not significant
  } else if (adultCathSig != FALSE & youngCathSig == FALSE) {
    paste0("Adult respondents identifying as Catholic who think relations between Protestants and Catholics will be better in five years' time has ", sub("significant", "significantly", adultCathSig), "d (", round2(abs(dataNew$f2a_Cath - dataOld$f2a_Cath))," percentage points to ", dataNew$f2a_Cath,"%) (", LTyear, ": ", dataNew$f2a_Cath, "%; ", LTyear - 1, ": ", dataOld$f2a_Cath, "%). There was no significant change among young people identifying as Catholic who think relations between Protestants and Catholics will be better in five years' time.")
    # Neither significant
  } else if (adultCathSig == FALSE & youngCathSig == FALSE) {
    paste0("There was no significant change among adults or young people identifying as Catholic who think relations between Protestants and Catholics will be better in five years' time.")
    # Both significant but change in different directions
  } else if(adultCathSig != FALSE & youngCathSig != FALSE & adultCathSig != youngCathSig) {
    paste0("Adult respondents identifying as Catholic who think relations between Protestants and Catholics will be better in five years' time has ", sub("significant", "significantly", adultCathSig), "d (", abs(dataNew$f2a_Cath - dataOld$f2a_Cath)," percentage points) (", NILTyear, ": ", dataNew$f2a_Cath, "%; ", NILTyear - 1, ": ", dataOld$f2a_Cath, "%). Among young people who identify as Catholic there was a ", youngCathSig, " of ", abs(dataNew$f2y_Cath - dataOld$f2y_Cath)," percentage points (", YLTyear, ": ", dataNew$f2y_Cath, "%; ", YLTyear - 1, ": ", dataOld$f2y_Cath, "%).")
  }

f2para2 <- paste(protSig, noRSig, cathSig)