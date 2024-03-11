adultNeigh <- round2(dataNew$f21b_neighbourhood)

adultApprox <- if (adultNeigh == 10) {
  "One in ten"
} else if (adultNeigh %in% c(9, 11)) {
  "Approximately one in ten"
} else if (adultNeigh %in% 12:13) {
  "Around one in eight"
} else if (adultNeigh %in% 14:15) {
  "Under one in six"
} else if (adultNeigh == 16) {
  "Around one in six"
} else if (adultNeigh %in% 17:18) {
  "Just over one in six"
} else if (adultNeigh == 20) {
  "One in five"
} else if (adultNeigh %in% c(19, 21)) {
  "Around one in five"
} else if (adultNeigh %in% 22:24) {
  "Just under one in four"
} else if (adultNeigh == 25) {
  "One in four"
} else if (adultNeigh %in% 26:27) {
  "Over a quarter of"
} else if (adultNeigh %in% 28:29) {
  "Just under three in ten"
} else if (adultNeigh == 30) {
  "Three in ten"
}

adultNI <- round2(dataNew$f21b_NI)
youngNeigh <- round2((fig21a$prob + fig21a$def)[fig21a$influence == "Neighbourhood"])
youngNI <- round2((fig21a$prob + fig21a$def)[fig21a$influence == "Northern\nIreland"])
maleNeigh <- round2(colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), gender = "Male"))
femaleNeigh <- round2(colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), gender = "Female"))
maleNI <- round2(colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), gender = "Male"))
femaleNI <- round2(colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), gender = "Female"))

genderNeighSig <- significanceTest(p1 = maleNeigh,
                                   n1 = unweighted_n(NILT$INFLLOCL[NILT$RSEX == "Male"]),
                                   p2 = femaleNeigh,
                                   n2 = unweighted_n(NILT$INFLLOCL[NILT$RSEX == "Female"]))


genderNISig <- significanceTest(p1 = maleNI,
                                n1 = unweighted_n(NILT$INFLNI[NILT$RSEX == "Male"]),
                                p2 = femaleNI,
                                n2 = unweighted_n(NILT$INFLNI[NILT$RSEX == "Female"]))


genderSentence <- if (genderNeighSig != FALSE & genderNISig != FALSE) {
  paste0("Among adults, males (", maleNeigh,
  "%) are significantly ", if (genderNeighSig == "significant increase") {"more"} else {"less"}, " likely than females (", femaleNeigh,
  "%) to feel they had an influence on decisions in their neighbourhood. ", if (genderNISig == genderNeighSig) {"Similarly, a"} else {"A"}, " significantly ", if (genderNISig == "significant increase") {"higher"} else {"lower"}, " proportion of male adults (",
  maleNI, "%) than female adults (", femaleNI,
  "%) feel they have an influence on decisions made in Northern Ireland.")
} else if (genderNeighSig != FALSE & genderNISig == FALSE) {
  paste0("Among adults, males (", maleNeigh,"%) are significantly ", if (genderNeighSig == "significant increase") {"more"} else {"less"}, " likely than females (", femaleNeigh,
         "%) to feel they had an influence on decisions in their neighbourhood. There are no significant differences in the proportions of male adults and female adults who feel they have an influence on decisions made in Northern Ireland.")
} else if (genderNeighSig == FALSE & genderNISig != FALSE) {
  paste0("Among adults, males (", maleNeigh,"%) are significantly ", if (genderNISig == "significant increase") {"more"} else {"less"}, " likely than females (", femaleNeigh,
         "%) to feel they had an influence on decisions in Northern Ireland. There are no significant differences in the proportions of male adults and female adults who feel they have an influence on decisions made in their neighbourhood.")
} else if (genderNeighSig == FALSE & genderNISig == FALSE) {
  paste0("Among adults, there is no significant difference in the proportions of males (",
         maleNeigh, "%) and females (", femaleNeigh,
         "%) who felt they had an influence on decisions in their neighbourhood. Similarly, there is no significant difference in the proportions of male adults (",
         maleNI, "%) and female adults (", femaleNI, "%) who feel they have an influence on decisions made in Northern Ireland.")
}

cathprotNeighSig <- significanceTest(p1 = colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "Catholic"),
                                     n1 = unweighted_n(NILT$INFLLOCL[NILT$RELIGCAT == "Catholic"]),
                                     p2 = colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "Protestant"),
                                     n2 = unweighted_n(NILT$INFLLOCL[NILT$RELIGCAT == "Protestant"]))

cathnorNeighSig <- significanceTest(p1 = colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "Catholic"),
                                   n1 = unweighted_n(NILT$INFLLOCL[NILT$RELIGCAT == "Catholic"]),
                                   p2 = colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "No religion"),
                                   n2 = unweighted_n(NILT$INFLLOCL[NILT$RELIGCAT == "No religion"]))

protnorNeighSig <- significanceTest(p1 = colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "Protestant"),
                                    n1 = unweighted_n(NILT$INFLLOCL[NILT$RELIGCAT == "Protestant"]),
                                    p2 = colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "No religion"),
                                    n2 = unweighted_n(NILT$INFLLOCL[NILT$RELIGCAT == "No religion"]))

cathprotNISig <- significanceTest(p1 = colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "Catholic"),
                                  n1 = unweighted_n(NILT$INFLNI[NILT$RELIGCAT == "Catholic"]),
                                  p2 = colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "Protestant"),
                                  n2 = unweighted_n(NILT$INFLNI[NILT$RELIGCAT == "Protestant"]))

cathnorNISig <- significanceTest(p1 = colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "Catholic"),
                                    n1 = unweighted_n(NILT$INFLNI[NILT$RELIGCAT == "Catholic"]),
                                    p2 = colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "No religion"),
                                    n2 = unweighted_n(NILT$INFLNI[NILT$RELIGCAT == "No religion"]))

protnorNISig <- significanceTest(p1 = colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "Protestant"),
                                    n1 = unweighted_n(NILT$INFLNI[NILT$RELIGCAT == "Protestant"]),
                                    p2 = colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "No religion"),
                                    n2 = unweighted_n(NILT$INFLNI[NILT$RELIGCAT == "No religion"]))


religionneighSentence <- if(cathprotNeighSig == FALSE & cathnorNeighSig == cathprotNeighSig & protnorNeighSig == cathprotNeighSig) {
  "There are no significant differences in feelings of influence over decisions taken in their neighbourhood based on the religion of respondents."
  # Cath/prot sig, cath/nor same and prot/nor false
} else if (cathprotNeighSig != FALSE & cathnorNeighSig == cathprotNeighSig & protnorNeighSig == FALSE) {
  paste0("Catholic respondents (", colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "Catholic"), "%) are significantly more likely than either Protestant respondents (", colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "Protestant"), "%) or respondents with no religion (", colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "No religion"), "%) to feel they have an influence on decisions made in their neighbourhood.")
  # Cath/nor and prot/nor sig and same, cath/prot not sig
} else if (cathprotNeighSig == FALSE & cathnorNeighSig != FALSE & protnorNeighSig == cathnorNeighSig) {
  paste0("Catholic (", colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "Catholic"), "%) and Protestant (", 
         colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "Protestant"), "%) respondents are both more likely than respondents with no religion (", colPct(NILT, INFLLOCL, c("Yes, probably", "Yes, definitely"), religion = "No religion"), "%) to feel they have an influence on decisions made in their neighbourhood. There is no significant difference between the proportion of Catholic and Protestant respondents who report this.")
}
  
religionNISentence <- if(cathprotNISig == FALSE & cathnorNISig == cathprotNISig & protnorNISig == cathprotNISig) {
  "There are no significant differences in feelings of influence over decisions taken in Northern Ireland based on the religion of respondents."
} else if (cathprotNISig != FALSE & cathnorNISig == cathprotNISig & protnorNISig == FALSE) {
  paste0("Catholic respondents (", colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "Catholic"), "%) are significantly more likely than either Protestant respondents (", colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "Protestant"), "%) or respondents with no religion (", colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "No religion"), "%) to feel they have an influence on decisions made in Northern Ireland.")
} else if (cathprotNISig == FALSE & cathnorNISig == cathprotNISig & protnorNISig != FALSE) {
  paste0("Protestant respondents (", colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "Protestant"), "%) are significantly more likely than respondents with no religion (", colPct(NILT, INFLNI, c("Yes, probably", "Yes, definitely"), religion = "No religion"), "%) to feel to feel they have an influence on decisions made in Northern Ireland. There are no other significant differences based on religion.")
}

neighSig <- significanceTest(p1 = dataNew$f21b_neighbourhood,
                             n1 = dataNew$INFLLOCL_n,
                             p2 = dataOld$f21b_neighbourhood,
                             n2 = dataOld$INFLLOCL_n)

NISig <- significanceTest(p1 = dataNew$f21b_NI,
                             n1 = dataNew$INFLNI_n,
                             p2 = dataOld$f21b_NI,
                             n2 = dataOld$INFLNI_n)

f21sentence1 <- if (neighSig != FALSE & neighSig == NISig) {
  paste0("Since ", NILTyear - 1,
         ", there has been a ", neighSig, 
         " in the proportion of adults who feel they have an influence on local decisions (",
         NILTyear, ": ", dataNew$f21b_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21b_neighbourhood, "%) and Northern Ireland decisions (",
         NILTyear, ": ", dataNew$f21b_NI, "%; ", NILTyear - 1, ": ", dataOld$f21b_NI, "%).")
} else if (neighSig != FALSE & NISig != FALSE & neighSig != NISig) {
  paste0("Since ", NILTyear - 1,
         ", there has been a ", neighSig, 
         " in the proportion of adults who feel they have an influence on local decisions (",
         NILTyear, ": ", dataNew$f21b_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21b_neighbourhood, "%). However, there was a ",
         NISig, " in the proportion who felt they have an influence on Northern Ireland decisions (",
         NILTyear, ": ", dataNew$f21b_NI, "%; ", NILTyear - 1, ": ", dataOld$f21b_NI, "%).")
} else if (neighSig != FALSE & NISig == FALSE) {
  paste0("Since ", NILTyear - 1,
         ", there has been a ", neighSig, 
         " in the proportion of adults who feel they have an influence on local decisions (",
         NILTyear, ": ", dataNew$f21b_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21b_neighbourhood, "%). However, there was no significant change in the proportion who felt they have an influence on Northern Ireland decisions.")
} else if (neighSig == FALSE & NISig != FALSE) {
  paste0("Since ", NILTyear - 1,
         ", there has been a ", NISig, 
         " in the proportion of adults who feel they have an influence on Northern Ireland decisions (",
         NILTyear, ": ", dataNew$f21b_NI, "%; ", NILTyear - 1, ": ", dataOld$f21b_NI, "%). However, there was no significant change in the proportion who felt they have an influence on local decisions.")
} else if (neighSig == FALSE & NISig == FALSE) {
  paste0("Since ", NILTyear - 1,
         ", there has been no significant changes in the proportion of adults who feel they have an influence on local decisions or on Northern Ireland decisions.")
}

youngSig <- significanceTest(p1 = dataNew$f21a_neighbourhood,
                             n1 = dataNew$INFLHERE_Yn,
                             p2 = dataOld$f21a_neighbourhood,
                             n2 = dataOld$INFLHERE_Yn)


youngSig2 <- significanceTest(p1 = dataNew$f21a_NI,
                             n1 = dataNew$INFLUNI_Yn,
                             p2 = dataOld$f21a_NI,
                             n2 = dataOld$INFLUNI_Yn)


f21sentence2 <- if (youngSig == FALSE & youngSig2 == FALSE) {
  paste0("There has been no significant change to young people's perception of influence since ", YLTyear - 1,
         ". Young people feel less influential than adults at both neighbourhood and Northern Ireland level.")
} else if (youngSig != FALSE & youngSig == youngSig2) {
  paste0("Young people's perception of influence has seen a ", youngSig, " since ", YLTyear - 1,
         ". Young people feel less influential than adults at both neighbourhood and Northern Ireland level.")
} else if (youngSig == "significant increase" & youngSig2 == "significant decrease") {
  paste0("Young people's perception of influence on their neighbourhood has significantly increased since ", NILTyear - 1,
         " (", NILTyear, ": ", dataNew$f21a_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21a_neighbourhood, "%), however, their perception of influence on Northern Ireland has decreased significantly (",
         NILTyear, ": ", dataNew$f21a_NI, "%; ", NILTyear - 1, ": ", dataOld$f21a_NI, "%). Young people feel less influential than adults at both neighbourhood and Northern Ireland level.")
} else if (youngSig == "significant decrease" & youngSig2 == "significant increase") {
  paste0("Young people's perception of influence on Northern Ireland has significantly increased since ", NILTyear - 1,
         " (", NILTyear, ": ", dataNew$f21a_NI, "%; ", NILTyear - 1, ": ", dataOld$f21a_NI, "%), however, their perception of influence on their neighbourhood has decreased significantly (",
         NILTyear, ": ", dataNew$f21a_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21a_neighbourhood, "%). Young people feel less influential than adults at both neighbourhood and Northern Ireland level.")
} else if (youngSig != FALSE & youngSig2 == FALSE) {
  paste0("Young people's perception of influence on their neighbourhood has seen a ", youngSig, " since ", NILTyear - 1,
         " (", NILTyear, ": ", dataNew$f21a_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21a_neighbourhood, "%), however, their perception of influence on Northern Ireland has not changed significantly. Young people feel less influential than adults at both neighbourhood and Northern Ireland level.")
} else if (youngSig == FALSE & youngSig2 != FALSE) {
  paste0("Young people's perception of influence on Northern Ireland has seen a ", youngSig2, " since ", NILTyear - 1,
         " (", NILTyear, ": ", dataNew$f21a_NI, "%; ", NILTyear - 1, ": ", dataOld$f21a_NI, "%), however, their perception of influence on their neighbourhood has not changed significantly. Young people feel less influential than adults at both neighbourhood and Northern Ireland level.")
}

adultneighNIsig <- significanceTest(p1 = dataNew$f21b_neighbourhood,
                                    n1 = dataNew$INFLLOCL_n,
                                    p2 = dataNew$f21b_NI,
                                    n2 = dataNew$INFLNI_n)

adultyoungneighsig <- significanceTest(p1 = dataNew$f21b_neighbourhood,
                                       n1 = dataNew$INFLLOCL_n,
                                       p2 = dataNew$f21a_neighbourhood,
                                       n2 = dataNew$INFLHERE_Yn)

adultyoungNIsig <-significanceTest(p1 = dataNew$f21b_NI,
                                   n1 = dataNew$INFLLOCL_n,
                                   p2 = dataNew$f21a_NI,
                                   n2 = dataNew$INFLHERE_Yn)

adultneighNIsentence <- if (adultneighNIsig != FALSE) { 
  paste0(adultApprox, " adults feel they have an influence on decisions made in their neighbourhood (", adultNeigh, "%), with a ", if (adultneighNIsig == "significant increase") {"smaller"} else {"larger"}, " proportion of adults (", adultNI, "%) feeling they have an influence on decisions made in Northern Ireland.")
  } else {paste0("There are no significant differences in the proportion of adults who feel they have an influence on decisions made in Northern Ireland when compared with decisions made in their neighbourhood.")
  }

adultyoungsentence <- if (adultyoungneighsig != FALSE & adultyoungNIsig == adultyoungneighsig) {
  paste0("The proportion of young people who think this is significantly ", if (adultyoungneighsig == "significant increase") {"lower"} else {"higher"}, " - ", youngNeigh, "% feel like they have an influence on decisions made in their neighbourhood, and ", youngNI, "% on decisions made in Northern Ireland.")
} else if (adultyoungneighsig == FALSE & adultyoungNIsig == adultyoungneighsig) {
  paste0("There are no significant differences in the proportions of adults and young people who feel they have an influence on decisions made in either their neighbourhood or Northern Ireland.")
}

f21para1 <- paste(f21sentence1, f21sentence2)

f21para2 <- paste0("Since 2013, the proportion of adults who feel they have an influence on decisions made in both their neighbourhood (",
                   NILTyear, ": ", dataNew$f21b_neighbourhood, "%; 2013: ", data$f21b_neighbourhood[data$year == 2013], "%) and Northern Ireland (",
                   NILTyear, ": ", dataNew$f21b_NI, "%; 2013: ", data$f21b_NI[data$year == 2013], "%) has significantly decreased. The proportion of young people who feel the same about their neighbourhood has also significantly decreased (", NILTyear, ": ", dataNew$f21a_neighbourhood, "%; 2013: ", data$f21a_neighbourhood[data$year == 2013], "%), however there has been no significant change in the proportion of young people who feel a sense of influence over decisions taken in Northern Ireland.")