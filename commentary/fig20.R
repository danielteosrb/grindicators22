belongNeigh <- round2((fig20b$prob + fig20b$def)[fig20b$belong == "Neighbourhood"])
belongNI <- round2((fig20b$prob + fig20b$def)[fig20b$belong == "Northern\nIreland"])

youngNeigh <- round2((fig20a$prob + fig20a$def)[fig20a$belong == "Neighbourhood"])
youngNI <- round2((fig20a$prob + fig20a$def)[fig20a$belong == "Northern\nIreland"])

protNeigh <- round2(colPct(NILT, UBELNGH, c("Yes, definitely", "Yes, probably"), religion = "Protestant"))
cathNeigh <- round2(colPct(NILT, UBELNGH, c("Yes, definitely", "Yes, probably"), religion = "Catholic"))
noNeigh <- round2(colPct(NILT, UBELNGH, c("Yes, definitely", "Yes, probably"), religion = "No religion"))

youngProtNeigh <- round2(colPct(YLT, BELONGNG, c("Yes, definitely", "Yes, probably"), religion = "Protestant"))
youngCathNeigh <- round2(colPct(YLT, BELONGNG, c("Yes, definitely", "Yes, probably"), religion = "Catholic"))
youngNoNeigh <- round2(colPct(YLT, BELONGNG, c("Yes, definitely", "Yes, probably"), religion = "No religion"))

protNI <- round2(colPct(NILT, UBELNI, c("Yes, definitely", "Yes, probably"), religion = "Protestant"))
cathNI <- round2(colPct(NILT, UBELNI, c("Yes, definitely", "Yes, probably"), religion = "Catholic"))
noNI <- round2(colPct(NILT, UBELNI, c("Yes, definitely", "Yes, probably"), religion = "No religion"))

youngProtNI <- round2(colPct(YLT, BELONGNI, c("Yes, definitely", "Yes, probably"), religion = "Protestant"))
youngCathNI <- round2(colPct(YLT, BELONGNI, c("Yes, definitely", "Yes, probably"), religion = "Catholic"))
youngNoNI <- round2(colPct(YLT, BELONGNI, c("Yes, definitely", "Yes, probably"), religion = "No religion"))



youngNISiglast <- significanceTest(p1 = youngNI,
                                   n1 = dataNew$BELONGNI_Yn,
                                   p2 = round2(dataOld$f20_youngNI),
                                   n2 = dataOld$BELONGNI_Yn)

youngNeighSiglast <- significanceTest(p1 = youngNeigh,
                                      n1 = dataNew$BELONGNG_Yn,
                                      p2 = round2(dataOld$f20_youngNeigh),
                                      n2 = dataOld$BELONGNG_Yn)

adultNISiglast <- significanceTest(p1 = belongNI,
                                   n1 = dataNew$UBELNI_n,
                                   p2 = round2(dataOld$f20_adultNI),
                                   n2 = dataOld$UBELNI_n)

adultNeighSiglast <- significanceTest(p1 = belongNeigh,
                                      n1 = dataNew$UBELNGH_n,
                                      p2 = round2(dataOld$f20_adultNeigh),
                                      n2 = dataOld$UBELNGH_n)

f20para1 <- if (youngNISiglast == FALSE & youngNeighSiglast == FALSE & adultNISiglast == FALSE & adultNeighSiglast == FALSE) {
  paste0("Since ", NILTyear - 1, ", the proportion of adults and young people who feel a sense of belonging to their neighbourhood and to Northern Ireland has not changed significantly.")
} else if (youngNISiglast == FALSE & youngNeighSiglast == FALSE & adultNISiglast != FALSE & adultNeighSiglast == adultNISiglast) {
  paste0("Since ", NILTyear - 1, ", the proportions of adults who feel who feel a sense of belonging to their neighbourhood (",
         NILTyear, ": ", belongNeigh, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_adultNeigh), "%) and to Northern Ireland (",
         NILTyear, ": ", belongNI, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_adultNI), "%) has ",
         if (adultNISiglast == "significant increase") {"increased"} else {"decreased"}, " significantly. However, there is no significant change in these proportions for young people.")
} else if (youngNISiglast != FALSE & youngNeighSiglast == youngNISiglast & adultNISiglast == FALSE & adultNeighSiglast == FALSE) {
  paste0("Since ", NILTyear - 1, ", the proportions of young people who feel who feel a sense of belonging to their neighbourhood (",
         NILTyear, ": ", youngNeigh, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_youngNeigh), "%) and to Northern Ireland (",
         NILTyear, ": ", youngNI, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_youngNI), "%) has ",
         if (youngNISiglast == "significant increase") {"increased"} else {"decreased"}, " significantly. However, there is no significant change in these proportions for adults.")
} else if (youngNISiglast != FALSE & youngNeighSiglast == youngNISiglast & adultNISiglast == youngNISiglast & adultNeighSiglast == youngNISiglast) {
  paste0("Since ", NILTyear - 1, ", the proportion of adults and young people who feel a sense of belonging to their neighbourhood and to Northern Ireland has ",
         if (youngNISiglast == "significant increase") {"increased"} else {"decreased"}, " significantly.")
} else if (youngNISiglast==FALSE & youngNeighSiglast==FALSE & adultNISiglast != FALSE & adultNeighSiglast == FALSE){
  paste0("Since ", NILTyear - 1, ", the proportion of adults  who feel a sense of belonging to Northern Ireland has ", 
         if (youngNISiglast == "significant increase") {"increased"} else {"decreased"}, " significantly (",
         NILTyear, ": ", belongNI, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_adultNI), "%), while there has been no significant change in adults who feel a sense of belonging in their neighbourhood. There has been no significant change in these proportions for young people.")
}else if (youngNISiglast != FALSE & youngNeighSiglast==youngNISiglast & adultNISiglast == FALSE & adultNeighSiglast == youngNISiglast){
  paste0("Since ", NILTyear - 1, ", the proportion of adults who feel a sense of belonging to their neighbourhood has ", 
         if (youngNISiglast == "significant increase") {"increased"} else {"decreased"}, " significantly (",
         NILTyear, ": ", belongNeigh, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_adultNeigh), "%), while there has been no significant change in adults who feel a sense of belonging to Northern Ireland. The proportion of young people who feel a sense of belonging to their neighbourhood (",
         YLTyear, ": ", youngNeigh, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_youngNeigh), "%) and to Northern Ireland (",
         YLTyear, ": ", youngNI, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_youngNI), "%) has also ", 
         if (youngNISiglast == "significant increase") {"increased"} else {"decreased"}, " significantly.")
}else if (youngNISiglast != FALSE & youngNeighSiglast == FALSE & adultNISiglast == youngNISiglast & adultNeighSiglast == youngNeighSiglast){
  paste0("Since ", NILTyear - 1, ", the proportion of adults (", NILTyear, ": ", belongNI, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_adultNI),
         "%) and young people (", NILTyear, ": ", youngNI, "%; ", NILTyear - 1, ": ", round2(dataOld$f20_youngNI),
         "%) who feel a sense of belonging to Northern Ireland has ", if (adultNISiglast == "significant increase") {"increased"} else {"decreased"}, " significantly. There is no
         significant change in the proportion of either adults or young people who feel a sense of belonging to their neighbourhood.")
}

youngNISig <- significanceTest(p1 = youngNI,
                               n1 = dataNew$BELONGNI_Yn,
                               p2 = round2(data$f20_youngNI[data$year == 2013]),
                               n2 = data$BELONGNI_Yn[data$year == 2013])

youngNeighSig <- significanceTest(p1 = youngNeigh,
                                  n1 = dataNew$BELONGNG_Yn,
                                  p2 = round2(data$f20_youngNeigh[data$year == 2013]),
                                  n2 = data$BELONGNG_Yn[data$year == 2013])

adultNISig <- significanceTest(p1 = belongNI,
                               n1 = dataNew$UBELNI_n,
                               p2 = round2(data$f20_adultNI[data$year == 2013]),
                               n2 = data$UBELNI_n[data$year == 2013])

adultNeighSig <- significanceTest(p1 = belongNeigh,
                                  n1 = dataNew$UBELNGH_n,
                                  p2 = round2(data$f20_adultNeigh[data$year == 2013]),
                                  n2 = data$UBELNGH_n[data$year == 2013])

f20para2 <- if (youngNISig == FALSE & youngNeighSig == FALSE & adultNISig == FALSE & adultNeighSig == FALSE) {
  "Since 2013, the proportion of adults and young people who feel a sense of belonging to their neighbourhood and to Northern Ireland has not changed significantly."
} else if (youngNISig == FALSE & youngNeighSig == FALSE & adultNISig != FALSE & adultNeighSig == adultNISig) {
  paste0("Since 2013, the proportions of adults who feel who feel a sense of belonging to their neighbourhood (",
         NILTyear, ": ", belongNeigh, "%; 2013: ", round2(data$f20_adultNeigh[data$year == 2013]), "%) and to Northern Ireland (",
         NILTyear, ": ", belongNI, "%; 2013: ", round2(data$f20_adultNI[data$year == 2013]), "%) has ",
         if (adultNISig == "significant increase") {"increased"} else {"decreased"}, " significantly. However, there is no significant change in these proportions for young people.")
} else if (youngNISig != FALSE & youngNeighSig == youngNISig & adultNISig == FALSE & adultNeighSig == FALSE) {
  paste0("Since 2013, the proportions of young people who feel who feel a sense of belonging to their neighbourhood (",
         NILTyear, ": ", youngNeigh, "%; 2013: ", round2(data$f20_youngNeigh[data$year == 2013]), "%) and to Northern Ireland (",
         NILTyear, ": ", youngNI, "%; 2013: ", round2(data$f20_youngNI[data$year == 2013]), "%) has ",
         if (youngNISig == "significant increase") {"increased"} else {"decreased"}, " significantly. However, there is no significant change in these proportions for adults.")
} else if (youngNISig != FALSE & youngNeighSig == youngNISig & adultNISig == youngNISig & adultNeighSig == youngNISig) {
  paste0("Since 2013, the proportion of adults and young people who feel a sense of belonging to their neighbourhood and to Northern Ireland has ",
         if (youngNISig == "significant increase") {"increased"} else {"decreased"}, " significantly.")
} else if (youngNISig==FALSE & youngNeighSig==FALSE & adultNISig != FALSE & adultNeighSig == FALSE){
  paste0("Since 2013, the proportion of adults  who feel a sense of belonging to Northern Ireland has ", 
         if (youngNISig == "significant increase") {"increased"} else {"decreased"}, " significantly (",
         NILTyear, ": ", belongNI, "%; 2013: ", round2(data$f20_adultNI[data$year == 2013]), "%), while there has been no significant change in adults who feel a sense of belonging in their neighbourhood. There has been no significant change in these proportions for young people.")
}else if (youngNISig != FALSE & youngNeighSig==youngNISig & adultNISig == FALSE & adultNeighSig == youngNISig){
  paste0("Since 2013, the proportion of adults who feel a sense of belonging to their neighbourhood has ", 
         if (youngNISig == "significant increase") {"increased"} else {"decreased"}, " significantly (",
         NILTyear, ": ", belongNeigh, "%; 2013: ", round2(data$f20_adultNeigh[data$year == 2013]), "%), while there has been no significant change in adults who feel a sense of belonging to Northern Ireland. The proportion of young people who feel a sense of belonging to their neighbourhood (",
         YLTyear, ": ", youngNeigh, "%; 2013: ", round2(data$f20_youngNeigh[data$year == 2013]), "%) and to Northern Ireland (",
         YLTyear, ": ", youngNI, "%; 2013: ", round2(data$f20_youngNI[data$year == 2013]), "%) has also ", 
         if (youngNISig == "significant increase") {"increased"} else {"decreased"}, " significantly.")
}else if (youngNISig != FALSE & youngNeighSig == FALSE & adultNISig == youngNISig & adultNeighSig == youngNeighSig){
  paste0("Since 2013, the proportion of adults (", NILTyear, ": ", belongNI, "%; 2013: ", round2(data$f20_adultNI[data$year == 2013]),
         "%) and young people (", NILTyear, ": ", youngNI, "%; 2013: ", round2(data$f20_youngNI[data$year == 2013]),
         "%) who feel a sense of belonging to Northern Ireland has ", if (adultNISiglast == "significant increase") {"increased"} else {"decreased"}, " significantly. There is no
         significant change in the proportion of either adults or young people who feel a sense of belonging to their neighbourhood.")
}

adultyoungneighSig <- significanceTest(p1 = youngNeigh,
                                       n1 = dataNew$BELONGNG_Yn,
                                       p2 = belongNeigh,
                                       n2 = dataNew$UBELNGH_n)

adultyoungNISig <- significanceTest(p1 = youngNI,
                                    n1 = dataNew$BELONGNI_Yn,
                                    p2 = belongNI,
                                    n2 = dataNew$UBELNI_n)

adultyoungSentence <- if (adultyoungneighSig != FALSE & adultyoungNISig == adultyoungneighSig) {
  paste0("A significantly ", if (adultyoungneighSig == "significant decrease") {"lower"} else {"higher"}, " proportion of young people say the same (", youngNeigh, "% belonging to their neighbourhood; ", youngNI, "% to Northern Ireland).")
} else if (adultyoungneighSig != FALSE & adultyoungNISig == FALSE) {
  paste0("A significantly ", if (adultyoungneighSig == "significant decrease") {"lower"} else {"higher"}, " proportion of young people say the same about their neighbourhood (", youngNeigh, "%), however, there is no significant difference in the proportion of adults and young people who feel a sense of belonging to Northern Ireland.")
} else if (adultyoungneighSig == FALSE & adultyoungNISig != FALSE) {
  paste0("A significantly ", if (adultyoungNISig == "significant decrease") {"lower"} else {"higher"}, " proportion of young people say the same about Northern Ireland (", youngNI, "%), however, there is no significant difference in the proportion of adults and young people who feel a sense of belonging to their neighbourhood.")
} else if (adultyoungneighSig != FALSE & adultyoungNISig != FALSE & adultyoungneighSig != adultyoungNISig) {
  paste0("A significantly ", if (adultyoungneighSig == "significant decrease") {"lower"} else {"higher"}, " proportion of young people say the same about their neighbourhood (", youngNeigh, "%), however, a significantly ", if (adultyoungNISig == "significant decrease") {"lower"} else {"higher"}, " proportion of young people than adults feel a sense of belonging to Northern Ireland (", youngNI, "%).")
} else if (adultyoungneighSig == FALSE & adultyoungNISig == adultyoungneighSig) {
  paste0("There are no significant differences between the proportion of adults and young people who feel a sense of belonging to either their neighbourhood or Northern Ireland.")
}

adultProtCathNeighSig <- significanceTest(p1 = protNeigh,
                                          n1 = unweighted_n(NILT$UBELNGH[NILT$RELIGCAT == "Protestant"]),
                                          p2 = cathNeigh,
                                          n2 = unweighted_n(NILT$UBELNGH[NILT$RELIGCAT == "Catholic"]))

adultProtNORNeighSig <- significanceTest(p1 = protNeigh,
                                         n1 = unweighted_n(NILT$UBELNGH[NILT$RELIGCAT == "Protestant"]),
                                         p2 = noNeigh,
                                         n2 = unweighted_n(NILT$UBELNGH[NILT$RELIGCAT == "No religion"]))

adultCathNORNeighSig <- significanceTest(p1 = cathNeigh,
                                         n1 = unweighted_n(NILT$UBELNGH[NILT$RELIGCAT == "Catholic"]),
                                         p2 = noNeigh,
                                         n2 = unweighted_n(NILT$UBELNGH[NILT$RELIGCAT == "No religion"]))

youngProtCathNeighSig <- significanceTest(p1 = youngProtNeigh,
                                          n1 = unweighted_n(YLT$BELONGNG[YLT$RELIGCAT == "Protestant"]),
                                          p2 = youngCathNeigh,
                                          n2 = unweighted_n(YLT$BELONGNG[YLT$RELIGCAT == "Catholic"]))

youngProtNORNeighSig <- significanceTest(p1 = youngProtNeigh,
                                         n1 = unweighted_n(YLT$BELONGNG[YLT$RELIGCAT == "Protestant"]),
                                         p2 = youngNoNeigh,
                                         n2 = unweighted_n(YLT$BELONGNG[YLT$RELIGCAT == "No religion"]))

youngCathNORNeighSig <- significanceTest(p1 = youngCathNeigh,
                                         n1 = unweighted_n(YLT$BELONGNG[YLT$RELIGCAT == "Catholic"]),
                                         p2 = youngNoNeigh,
                                         n2 = unweighted_n(YLT$BELONGNG[YLT$RELIGCAT == "No religion"]))

adultProtCathNISig <- significanceTest(p1 = protNI,
                                       n1 = unweighted_n(NILT$UBELNI[NILT$RELIGCAT == "Protestant"]),
                                       p2 = cathNI,
                                       n2 = unweighted_n(NILT$UBELNI[NILT$RELIGCAT == "Catholic"]))

adultProtNORNISig <- significanceTest(p1 = protNI,
                                      n1 = unweighted_n(NILT$UBELNI[NILT$RELIGCAT == "Protestant"]),
                                      p2 = noNI,
                                      n2 = unweighted_n(NILT$UBELNI[NILT$RELIGCAT == "No religion"]))

adultCathNORNISig <- significanceTest(p1 = cathNI,
                                      n1 = unweighted_n(NILT$UBELNI[NILT$RELIGCAT == "Catholic"]),
                                      p2 = noNI,
                                      n2 = unweighted_n(NILT$UBELNI[NILT$RELIGCAT == "No religion"]))

youngProtCathNISig <- significanceTest(p1 = youngProtNI,
                                       n1 = unweighted_n(YLT$BELONGNI[YLT$RELIGCAT == "Protestant"]),
                                       p2 = youngCathNI,
                                       n2 = unweighted_n(YLT$BELONGNI[YLT$RELIGCAT == "Catholic"]))

youngProtNORNISig <- significanceTest(p1 = youngProtNI,
                                      n1 = unweighted_n(YLT$BELONGNI[YLT$RELIGCAT == "Protestant"]),
                                      p2 = youngNoNI,
                                      n2 = unweighted_n(YLT$BELONGNI[YLT$RELIGCAT == "No religion"]))

youngCathNORNISig <- significanceTest(p1 = youngCathNI,
                                      n1 = unweighted_n(YLT$BELONGNI[YLT$RELIGCAT == "Catholic"]),
                                      p2 = youngNoNI,
                                      n2 = unweighted_n(YLT$BELONGNI[YLT$RELIGCAT == "No religion"]))

adultneighreligionsentence <- if (adultProtCathNeighSig != FALSE & adultProtNORNeighSig == adultProtCathNeighSig) {
  paste0("Among adults, significantly ", if (adultProtCathNeighSig == "significant increase") {"more"} else {"fewer"}, " Protestant respondents (", protNeigh, "%) than either Catholic respondents (", cathNeigh, "%) or respondents with no religion (", noNeigh, "%) said they felt a sense of belonging to their neighbourhood.")
} else if (adultProtCathNeighSig == FALSE & adultProtNORNeighSig == adultProtCathNeighSig & adultCathNORNeighSig == adultProtCathNeighSig) {
  paste0("Among adults, there were no significant differences in feelings of belonging to their neighbourhood based on religion.")
}

youngneighreligionsentence <- if (youngProtNORNeighSig != FALSE & youngCathNORNeighSig == youngProtNORNeighSig) {
  paste0("For young people, significantly ", if (youngProtNORNeighSig == "significant increase") {"more"} else {"fewer"}, " Protestant (", youngProtNeigh, "%) and Catholic (", youngCathNeigh, "%) respondents than respondents with no religion (", youngNoNeigh, "%) felt a sense of belonging to their neighbourhood.")
} else if (youngProtNORNeighSig == FALSE & youngCathNORNeighSig == youngProtNORNeighSig) {
  paste0("Among young people, there were ", if (c(adultProtCathNeighSig, adultProtNORNeighSig, adultCathNORNeighSig) == FALSE) {"also "}, "no significant differences in feelings of belonging to their neighbourhood based on religion.")
}

adultNIreligionsentence <- if (adultProtCathNISig != FALSE & adultProtNORNISig == adultProtCathNISig) {
  paste0("Again among adults, significantly ", if (adultProtCathNISig == "significant increase") {"more"} else {"fewer"}, " Protestant respondents (", protNI, "%) than either Catholic respondents (", cathNI, "%) or respondents with no religion (", noNI, "%) said they felt a sense of belonging to Northern Ireland.")
} else if (adultProtCathNISig == FALSE & adultProtNORNISig == adultProtCathNISig & adultCathNORNISig == adultProtCathNISig) {
  paste0("Again among adults, there were no significant differences in feelings of belonging to Northern Ireland based on religion.")
}

youngNIreligionsentence <- if (youngProtCathNISig != FALSE & youngProtNORNISig == youngProtCathNISig) {
  paste0("Again among young people, significantly ", if (youngProtCathNISig == "significant increase") {"more"} else {"fewer"}, " Protestant respondents (", youngProtNI, "%) than either Catholic respondents (", youngCathNI, "%) or respondents with no religion (", youngNoNI, "%) said they felt a sense of belonging to Northern Ireland.")
} else if (youngProtCathNISig == FALSE & youngProtNORNISig == youngProtCathNISig & youngCathNORNISig == youngProtCathNISig) {
  paste0("Again among young people, there were no significant differences in feelings of belonging to Northern Ireland based on religion.")
}