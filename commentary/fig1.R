# To determine if this is lowest proportion of young people since 2013
youngRank <- if (dataNew$f1_young == min(data$f1_young)) {
  "lowest"
} else {
  paste0(ordinal(rank(data$f1_young)[data$year == LTyear]), " lowest")
}

adultRank <- if (dataNew$f1_adult == min(data$f1_adult)) {
  "lowest"
} else {
  paste0(ordinal(rank(data$f1_adult)[data$year == LTyear]), " lowest")
}

# First sentence of second paragraph:
adultProtSig <- significanceTest(p1 = dataNew$f1a_Prot,
                                 n1 = dataNew$RLRELAGO_nProt,
                                 p2 = dataOld$f1a_Prot,
                                 n2 = dataOld$RLRELAGO_nProt)

youngProtSig <- significanceTest(p1 = dataNew$f1y_Prot,
                                 n1 = dataNew$RLRELAGO_YnProt,
                                 p2 = dataOld$f1y_Prot,
                                 n2 = dataOld$RLRELAGO_YnProt)

protSig <-
  # Both significant and the same
  if(adultProtSig != FALSE & adultProtSig == youngProtSig) {
    paste0("The proportion of both adults and young people identifying as Protestant, who think relations between Protestants and Catholics are better now than they were five years ago, has ", sub("significant ", "", adultProtSig), "d by ",
           abs(dataNew$f1a_Prot - dataOld$f1a_Prot)," percentage points (", NILTyear, ": ", dataNew$f1a_Prot, "%; ", NILTyear - 1, ": ", dataOld$f1a_Prot, "%) and ", abs(dataNew$f1y_Prot - dataOld$f1y_Prot), " percentage points (",
           YLTyear, ": ", dataNew$f1y_Prot, "%; ", YLTyear - 1, ": ", dataOld$f1y_Prot, "%) respectively, from ", LTyear - 1, " to ", LTyear, ".")
    # Adult not significant and young people significant change
  } else if (adultProtSig == FALSE & youngProtSig != FALSE) {
    paste0("From ", LTyear - 1, " to ", LTyear, " there was no significant change in the proportion of adults identifying as Protestant who think relations between Protestants and Catholics are better now than they were five years ago, while there was a ", youngProtSig, " of ", abs(dataNew$f1y_Prot - dataOld$f1y_Prot), " percentage points among young people who identify as Protestant (", YLTyear, ": ", dataNew$f1y_Prot, "%; ", YLTyear - 1, ": ", dataOld$f1y_Prot, "%).")
    # Adult significant and young people not significant
  } else if (adultProtSig != FALSE & youngProtSig == FALSE) {
    paste0("From ", LTyear - 1, " to ", LTyear, " there was a ", adultProtSig, " of ", abs(dataNew$f1a_Prot - dataOld$f1a_Prot), " percentage points in the proportion of adults identifying as Protestant, who think relations between Protestants and Catholics are better now than they were five years ago (", LTyear, ": ", dataNew$f1a_Prot, "%; ", LTyear - 1, ": ", dataOld$f1a_Prot, "%),  whilst there was no significant change in this measure among young people who identify as Protestant.")
    # Neither significant
  } else if (adultProtSig == FALSE & youngProtSig == FALSE) {
    paste0("There was no significant change in the proportion of either adults or young people identifying as Protestant, who think relations between Protestants and Catholics are better now than they were five years ago, from ", LTyear - 1, " to ", LTyear, ".")
    # Both significant but change in different directions
  } else if(adultProtSig != FALSE & youngProtSig != FALSE & adultProtSig != youngProtSig) {
    paste0("From ", LTyear - 1, " to ", LTyear, " there was a ", adultProtSig, " of ", abs(dataNew$f1a_Prot - dataOld$f1a_Prot), " percentage points in the proportion of adults identifying as Protestant, who think relations between Protestants and Catholics are better now than they were five years ago (", NILTyear, ": ", dataNew$f1a_Prot, "%; ", NILTyear - 1, ": ", dataOld$f1a_Prot, "%), whilst there was ", youngProtSig, " of ", abs(dataNew$f1y_Prot - dataOld$f1y_Prot), " percentage points in this measure among young people who identify as Protestant (", YLTyear, ": ", dataNew$f1y_Prot, "%; ", YLTyear - 1, ": ", dataOld$f1y_Prot, "%).")
  }

# Second sentence of second paragraph:
adultNoRSig <- significanceTest(p1 = dataNew$f1a_NoR,
                                n1 = dataNew$RLRELAGO_nNoR,
                                p2 = dataOld$f1a_NoR,
                                n2 = dataOld$RLRELAGO_nNoR)

youngNoRSig <- significanceTest(p1 = dataNew$f1y_NoR,
                                n1 = dataNew$RLRELAGO_YnNoR,
                                p2 = dataOld$f1y_NoR,
                                n2 = dataOld$RLRELAGO_YnNoR)


noRSig <-
  # Both significant and the same
  if(adultNoRSig != FALSE & adultNoRSig == youngNoRSig) {
    paste0("There were ", adultNoRSig, "s in the proportions of both adults and young people with no religion (",
           abs(dataNew$f1a_NoR - dataOld$f1a_NoR), " percentage points (", NILTyear, ": ", dataNew$f1a_NoR, "%; ", NILTyear - 1, ": ", dataOld$f1a_NoR, "%) and ",
           abs(dataNew$f1y_NoR - dataOld$f1y_NoR), " percentage points (", YLTyear, ": ", dataNew$f1y_NoR, "%; ", YLTyear - 1, ": ", dataOld$f1y_NoR, "%) respectively) who think relations between Protestants and Catholics are better now than they were five years ago.")
    # Adult not significant and young people significant change
  } else if (adultNoRSig == FALSE & youngNoRSig != FALSE) {
    paste0("There was no significant change among adult respondents with no religion who think relations between Protestants and Catholics are better now than they were five years ago. There was a ", youngNoRSig, " (", abs(dataNew$f1y_NoR - dataOld$f1y_NoR), " percentage points) among young people with no religion who think relations between Protestants and Catholics are better now than they were five years ago (", YLTyear, ": ", dataNew$f1y_NoR, "%; ", YLTyear - 1, ": ", dataOld$f1y_NoR, "%).")
    # Adult significant and young people not significant
  } else if (adultNoRSig != FALSE & youngNoRSig == FALSE) {
    paste0("Adult respondents with no religion who think relations between Protestants and Catholics are better now than they were five years ago has ", sub("significant", "significantly", adultNoRSig), "d (", round2(abs(dataNew$f1a_NoR - dataOld$f1a_NoR))," percentage points) (", NILTyear, ": ", dataNew$f1a_NoR, "%; ", NILTyear - 1, ": ", dataOld$f1a_NoR, "%). There was no significant change among young people with no religion who think relations between Protestants and Catholics are better now than they were five years ago.")
    # Neither significant
  } else if (adultNoRSig == FALSE & youngNoRSig == FALSE) {
    paste0("There was no significant change in the proportion of either adults or young people with no religion who think relations between Protestants and Catholics are better now than they were five years ago.")
    # Both significant but change in different directions
  } else if(adultNoRSig != FALSE & youngNoRSig != FALSE & adultNoRSig != youngNoRSig) {
    paste0("Adult respondents with no religion who think relations between Protestants and Catholics are better now than they were five years ago has ", adultNoRSig, "d (", abs(dataNew$f1a_NoR - dataOld$f1a_NoR)," percentage points) (", NILTyear, ": ", dataNew$f1a_NoR, "%; ", NILTyear - 1, ": ", dataOld$f1a_NoR, "%). There was a ", youngNoRSig, " (", abs(dataNew$f1y_NoR - dataOld$f1y_NoR), " percentage points) among young people with no religion who think relations between Protestants and Catholics are better now than they were five years ago (", YLTyear, ": ", dataNew$f1y_NoR, "%; ", YLTyear - 1, ": ", dataOld$f1y_NoR, "%).")
  }

adultCathSig <- significanceTest(p1 = dataNew$f1a_Cath,
                                 n1 = dataNew$RLRELAGO_nCath,
                                 p2 = dataOld$f1a_Cath,
                                 n2 = dataOld$RLRELAGO_nCath)

youngCathSig <- significanceTest(p1 = dataNew$f1y_Cath,
                                 n1 = dataNew$RLRELAGO_YnCath,
                                 p2 = dataOld$f1y_Cath,
                                 n2 = dataOld$RLRELAGO_YnCath)

# Third sentence of second paragraph:
cathSig <-
  # Both significant and the same
  if(adultCathSig != FALSE & adultCathSig == youngCathSig) {
    paste0("For adults and young people who identify as Catholic there were ", adultCathSig, "s of ", abs(dataNew$f1a_Cath - dataOld$f1a_Cath)," percentage points (", NILTyear, ": ", dataNew$f1a_Cath, "%; ", NILTyear - 1, ": ", dataOld$f1a_Cath, "%) and ", abs(dataNew$f1y_Cath - dataOld$f1y_Cath)," percentage points (", YLTyear, ": ", dataNew$f1y_Cath, "%; ", YLTyear - 1, ": ", dataOld$f1y_Cath, "%) respectively in the proportion who think relations between Protestants and Catholics are better now than they were five years ago.")
    # Adult not significant and young people significant change
  } else if (adultCathSig == FALSE & youngCathSig != FALSE) {
    paste0("There was no significant change among adult respondents identifying as Catholic who think relations between Protestants and Catholics are better now than they were five years ago. Among young people who identify as Catholic there was a ", youngCathSig, " of ", abs(dataNew$f1y_Cath - dataOld$f1y_Cath)," percentage points (", YLTyear, ": ", dataNew$f1y_Cath, "%; ", YLTyear - 1, ": ", dataOld$f1y_Cath, "%).")
    # Adult significant and young people not significant
  } else if (adultCathSig != FALSE & youngCathSig == FALSE) {
    paste0("Adult respondents identifying as Catholic who think relations between Protestants and Catholics are better now than they were five years ago has ", sub("significant", "significantly", adultCathSig), "d (", round2(abs(dataNew$f1a_Cath - dataOld$f1a_Cath))," percentage points to ", dataNew$f1a_Cath,"%) (", LTyear, ": ", dataNew$f1a_Cath, "%; ", LTyear - 1, ": ", dataOld$f1a_Cath, "%). There was no significant change among young people identifying as Catholic who think relations between Protestants and Catholics are better now than they were five years ago.")
    # Neither significant
  } else if (adultCathSig == FALSE & youngCathSig == FALSE) {
    paste0("There was no significant change among adults or young people identifying as Catholic who think relations between Protestants and Catholics are better now than they were five years ago.")
    # Both significant but change in different directions
  } else if(adultCathSig != FALSE & youngCathSig != FALSE & adultCathSig != youngCathSig) {
    paste0("Adult respondents identifying as Catholic who think relations between Protestants and Catholics are better now than they were five years ago has ", sub("significant", "significantly", adultCathSig), "d (", abs(dataNew$f1a_Cath - dataOld$f1a_Cath)," percentage points) (", NILTyear, ": ", dataNew$f1a_Cath, "%; ", NILTyear - 1, ": ", dataOld$f1a_Cath, "%). Among young people who identify as Catholic there was a ", youngCathSig, " of ", abs(dataNew$f1y_Cath - dataOld$f1y_Cath)," percentage points (", YLTyear, ": ", dataNew$f1y_Cath, "%; ", YLTyear - 1, ": ", dataOld$f1y_Cath, "%).")
  }

f1para1 <- paste0("Since ", LTyear - 1, ", there has been ", if (adultSig==FALSE){"no significant change"} else{paste0("a ",adultSig,
                  " (of ", abs(dataNew$f1_adults - dataOld$f1_adults),
                  " percentage points to ", dataNew$f1_adults,
                  "%)")}," in the proportion of adult respondents who think relations between Protestants and Catholics are better now than they were five years ago",  if (adultSig != FALSE){paste0(" (",LTyear , ": ", round2(dataNew$f1_adults), "%; ", LTyear - 1, ": ", round2(dataOld$f1_adults), "%)")}, ". The proportion of young people who think relations between Protestants and Catholics are better now than they were five years ago has ",
                  if(adultSig == youngSig) {"also "}, if(youngSig != FALSE){paste0(sub("significant ", "", youngSig), "d")} else {"not undergone any significant change"},
                  " since ", LTyear - 1, if(youngSig != FALSE) {paste0(" (", YLTyear, ": ", dataNew$f1_young, "%; ", YLTyear - 1, ": ", dataOld$f1_young, "%)")},". This is the ",
                  youngRank, " proportion for ", if (adultRank == youngRank) {"adults and"}, " young people since the launch of the T:BUC strategy in 2013.")

f1para2 <- paste(protSig, noRSig, cathSig)
