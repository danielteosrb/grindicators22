addProt <- round2((fig22$strongly + fig22$agree)[grepl("Protestant", fig22$community)])
addCath <- 71
addME <- 67

approx <- c("approxProt", "approxCath", "approxME")
add <- list(addProt, addCath, addME)

for (i in 1:length(approx)) {
  
  assign(approx[i],
         if (add[i] %in% 45:48) {
           "Just under a half"
         } else if (add[i] %in% 49:51) {
           "Around a half"
         } else if (add[i] %in% 52:59) {
           "Over a half"
         } else if (add[i] == 60) {
           "Three-fifths"
         } else if (add[i] %in% 61:63) {
           "Over three-fifths"
         } else if (add[i] %in% 64:65) {
           "Approaching two-thirds"
         } else if (add[i] %in% 66:67) {
           "Around two-thirds"
         } else if (add[i] %in% 68:70) {
           "Over two-thirds"
         } else if (add[i] %in% 71:74) {
           "Approaching three-quarters"
         } else if (add[i] == 75) {
           "Three-quarters"
         } else if (add[i] %in% 76:79) {
           "Over three-quarters"
         })
  
}

highest <- fig22$community[which(fig22$strongly + fig22$agree == max(fig22$strongly + fig22$agree))] %>%
  gsub("\ncommunities", "", .)

protcathsig22 <- significanceTest((fig22$strongly + fig22$agree)[grepl("Protestant", fig22$community)] / 100, dataNew$PRCRICH_n, (fig22$strongly + fig22$agree)[grepl("Catholic", fig22$community)] / 100, dataNew$CTHCRICH_n)

protMEsig22 <- significanceTest((fig22$strongly + fig22$agree)[grepl("Protestant", fig22$community)] / 100, dataNew$PRCRICH_n, (fig22$strongly + fig22$agree)[grepl("Minority Ethnic", fig22$community)] / 100, dataNew$EMCRICH_n)

cathMEsig22 <- significanceTest((fig22$strongly + fig22$agree)[grepl("Catholic", fig22$community)] / 100, dataNew$CTHCRICH_n, (fig22$strongly + fig22$agree)[grepl("Minority Ethnic", fig22$community)] / 100, dataNew$EMCRICH_n)


f22Statement <- if (highest == "Protestant") {
    paste0(approxProt, " (", addProt,"%) of respondents think that the culture and traditions of Protestant communities add to the richness and diversity of Northern Ireland society. ", addCath, "% think this about Catholic communities, and ", 
           addME, "% think this about Minority Ethnic communities.")
  } else if (highest == "Catholic") {
    paste0(approxCath, " (", addCath,"%) of respondents think that the culture and traditions of Catholic communities add to the richness and diversity of Northern Ireland society. ", addProt, "% think this about Protestant communities, and ", 
           addME, "% think this about Minority Ethnic communities.")
  } else if (highest == "Minority Ethnic") {
    paste0(approxME, " (", addME,"%) of respondents think that the culture and traditions of Minority Ethnic communities add to the richness and diversity of Northern Ireland society. ", addProt, "% think this about Protestant communities, and ", 
           addCath, "% think this about Catholic communities.")
  }

f22insig <- if (protcathsig22 == FALSE & protMEsig22 == protcathsig22 & cathMEsig22 == protcathsig22) {
  "There are no significant differences in these proportions."
}

# the below code makes assumptions that it does not test, and therefore can potentially make incorrect statements in the report, as is the case in 2021. It has been rewritten in a much simpler form above, 
# which provides less detailed commentary but will never make incorrect statements.

# highest <- fig22$community[which(fig22$strongly + fig22$agree == max(fig22$strongly + fig22$agree))] %>%
#   gsub("\ncommunities", "", .)
# 
# second <- fig22$community[which(fig22$strongly + fig22$agree == sort(fig22$strongly + fig22$agree, partial = nrow(fig22) - 1)[nrow(fig22) - 1])] %>%
#   gsub("\ncommunities", "", .)
# 
# p1_22 <- (fig22$strongly + fig22$agree)[grepl(highest, fig22$community)] / 100
# p2_22 <- (fig22$strongly + fig22$agree)[grepl(second, fig22$community)] / 100
# 
# n1_22 <- if (highest == "Protestant") {
#   dataNew$PRCRICH_n
# } else if (highest == "Catholic") {
#   dataNew$CTHCRICH_n
# } else if (highest == "Minority Ethnic"){
#   dataNew$EMCRICH_n
# }
# 
# n2_22 <- if (second == "Protestant") {
#   dataNew$PRCRICH_n
# } else if (second == "Catholic") {
#   dataNew$CTHCRICH_n
# } else if (second == "Minority Ethnic"){
#   dataNew$EMCRICH_n
# }
# 
# sig22 <- significanceTest(p1_22, n1_22, p2_22, n2_22)

# f22sigStatement <- if (sig22 == FALSE) {
#   if (highest == "Protestant") {
#   paste0(approxProt, " (", addProt,
#          "%) of respondents think that the culture and traditions of Protestant communities add to the richness and diversity of Northern Ireland society. Although a smaller number of respondents (",
#          addCath, "%) think this about Catholic communities, there is no significant difference in these amounts. However, a significantly smaller proportion (",
#          addME, "%) think this about Minority Ethnic communities.")
#   } else if (highest == "Catholic") {
#     paste0(approxCath, " (", addCath,
#            "%) of respondents think that the culture and traditions of Catholic communities add to the richness and diversity of Northern Ireland society. Although a smaller number of respondents (",
#            addProt, "%) think this about Protestant communities, there is no significant difference in these amounts. However, a significantly smaller proportion (",
#            addME, "%) think this about Minority Ethnic communities.")
#   }
# } else {
#   if (highest == "Protestant") {
#   paste0(approxProt, " (", addProt,
#          "%) of respondents think that the culture and traditions of Protestant communities add to the richness and diversity of Northern Ireland society. A significantly smaller number of respondents (",
#          addCath, "%) think this about Catholic communities. A significantly smaller proportion again (",
#          addME, "%) think this about Minority Ethnic communities.")
#   } else if (highest == "Catholic") {
#       paste0(approxCath, " (", addCath,
#              "%) of respondents think that the culture and traditions of Catholic communitites add to the richness and diversity of Northern Ireland society. A significantly smaller number of respondents (",
#              addProt, "%) think this about Protestant communities. A significantly smaller proportion again (",
#              addME, "%) think this about Minority Ethnic communities.")
#     }
# }

cathSig <- significanceTest(p1 = dataNew$f22_cath,
                            n1 = dataNew$CTHCRICH_n,
                            p2 = dataOld$f22_cath,
                            n2 = dataOld$CTHCRICH_n)

protSig <- significanceTest(p1 = dataNew$f22_prot,
                            n1 = dataNew$PRCRICH_n,
                            p2 = dataOld$f22_prot,
                            n2 = dataOld$PRCRICH_n)

meSig <- significanceTest(p1 = dataNew$f22_me,
                          n1 = dataNew$EMCRICH_n,
                          p2 = dataOld$f22_me,
                          n2 = dataOld$EMCRICH_n)

f22para1 <- if (cathSig == FALSE & protSig == FALSE & meSig != FALSE) {
  
  paste0("There was no significant change in proportion of respondents who think the culture and traditions of Catholic and Protestant communities add to the richness and diversity of Northern Ireland society since ",
         NILTyear - 1, ". There was a ", meSig,
         " in the proportion who think this in relation to Minority Ethnic communities (",
        NILTyear, ": ", dataNew$f22_me, "%; ", NILTyear - 1, ": ", dataOld$f22_me, "%).")
  
} else if (cathSig != FALSE & protSig == cathSig & meSig == FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic and Protestant communities add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig), "d since ", NILTyear - 1,
         " (by ", round2(abs(dataNew$f22_cath - dataOld$f22_cath))," percentage points (",
         NILTyear, ": ", dataNew$f22_cath, "%; ", NILTyear - 1, ": ", dataOld$f22_cath, "%) and ",
         round2(abs(dataNew$f22_prot - dataOld$f22_prot)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_prot, "%; ", NILTyear - 1, ": ", dataOld$f22_prot, "%) respectively. There has been no significant change in the proportion who think this in relation to Minority Ethnic communities.")
  
} else if (cathSig != FALSE & protSig == cathSig & meSig != FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic and Protestant communities add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig), "d since ", NILTyear - 1,
         " (by ", round2(abs(dataNew$f22_cath - dataOld$f22_cath))," percentage points (",
         NILTyear, ": ", dataNew$f22_cath, "%; ", NILTyear - 1, ": ", dataOld$f22_cath, "%) and ",
         round2(abs(dataNew$f22_prot - dataOld$f22_prot)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_prot, "%; ", NILTyear - 1, ": ", dataOld$f22_prot, "%) respectively. The proportion who think this in relation to Minority Ethnic communities has ",
         sub("t", "tly", meSig),"d by ", round2(abs(dataNew$f22_me - dataOld$f22_me)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_me, "%; ", NILTyear - 1, ": ", dataOld$f22_me, "%).")
  
} else if (cathSig == FALSE & protSig == FALSE & meSig == FALSE) {
  
  paste0("There was no significant change in proportion of respondents who think the culture and traditions of Catholic, Protestant and Minority Ethnic communities add to the richness and diversity of Northern Ireland society since ",
         NILTyear - 1, ".")
  
} else if (cathSig != FALSE & protSig != cathSig & protSig != FALSE & meSig == FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig),"d by ", round2(abs(dataNew$f22_cath - dataOld$f22_cath)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_cath, "%; ", NILTyear - 1, ": ", dataOld$f22_cath, "%) while the proportion who think this about Protestant communities has ",
         sub("t", "tly", protSig), "d by ", round2(abs(dataNew$f22_prot - dataOld$f22_prot)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_prot, "%; ", NILTyear - 1, ": ", dataOld$f22_prot, "%) since ", NILTyear - 1,
         ". There has been no significant change in the proportion who think this in relation to Minority Ethnic communities.")
  
} else if (cathSig != FALSE & protSig != cathSig & protSig != FALSE & meSig != FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig),"d by ", round2(abs(dataNew$f22_cath - dataOld$f22_cath)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_cath, "%; ", NILTyear - 1, ": ", dataOld$f22_cath, "%) while the proportion who think this about Protestant communities has ",
         sub("t", "tly", protSig), "d by ", round2(abs(dataNew$f22_prot - dataOld$f22_prot)),
         " percentage points since ", NILTyear - 1,
         " (",
         NILTyear, ": ", dataNew$f22_prot, "%; ", NILTyear - 1, ": ", dataOld$f22_prot, "%). The proportion who think this in relation to Minority Ethnic communities has ",
         sub("t", "tly", meSig),"d by ", round2(abs(dataNew$f22_me - dataOld$f22_me)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_me, "%; ", NILTyear - 1, ":  ", dataOld$f22_me, "%).")
  
} else if (cathSig != FALSE & protSig == FALSE & meSig == FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig),"d by ", round2(abs(dataNew$f22_cath - dataOld$f22_cath)),
         " percentage points since ", NILTyear - 1,
         " (",
         NILTyear, ": ", dataNew$f22_cath, "%; ", NILTyear - 1, ": ", dataOld$f22_cath, "%) but there has been no significant change in the proportion who think this about Protestant communities. There has also been no significant change in the proportion who think this in relation to Minority Ethnic communities.")
  
} else if (cathSig != FALSE & protSig == FALSE & meSig != FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig),"d by ", round2(abs(dataNew$f22_cath - dataOld$f22_cath)),
         " percentage points since ", NILTyear - 1,
         " (",
         NILTyear, ": ", dataNew$f22_cath, "%; ", NILTyear - 1, ": ", dataOld$f22_cath, "%) but there has been no significant change in the proportion who think this about Protestant communities. The proportion who think this in relation to Minority Ethnic communities has ",
         sub("t", "tly", meSig),"d by ", round2(abs(dataNew$f22_me - dataOld$f22_me)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_me, "%; ", NILTyear - 1, ": ", dataOld$f22_me, "%).")
  
} else if (cathSig == FALSE & protSig != FALSE & meSig == FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Protestant communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", protSig),"d by ", round2(abs(dataNew$f22_prot - dataOld$f22_prot)),
         " percentage points since ", NILTyear - 1,
         " (",
         NILTyear, ": ", dataNew$f22_prot, "%; ", NILTyear - 1, ": ", dataOld$f22_prot, "%) but there has been no significant change in the proportion who think this about Catholic communities. There has also been no significant change in the proportion who think this in relation to Minority Ethnic communities.")
  
} else if (cathSig == FALSE & protSig != FALSE & meSig != FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Protestant communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", protSig),"d by ", round2(abs(dataNew$f22_prot - dataOld$f22_prot)),
         " percentage points since ", NILTyear - 1,
         " (",
         NILTyear, ": ", dataNew$f22_prot, "%; ", NILTyear - 1, ": ", dataOld$f22_prot, "%) but there has been no significant change in the proportion who think this about Catholic communities. The proportion who think this in relation to Minority Ethnic communities has ",
         sub("t", "tly", meSig),"d by ", round2(abs(dataNew$f22_me - dataOld$f22_me)),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_me, "%; ", NILTyear - 1, ": ", dataOld$f22_me, "%).")
  
}

cathSig13 <- significanceTest(p1 = dataNew$f22_cath,
                              n1 = dataNew$CTHCRICH_n,
                              p2 = data$f22_cath[data$year == 2013],
                              n2 = data$CTHCRICH_n[data$year == 2013])

protSig13 <- significanceTest(p1 = dataNew$f22_prot,
                              n1 = dataNew$PRCRICH_n,
                              p2 = data$f22_prot[data$year == 2013],
                              n2 = data$PRCRICH_n[data$year == 2013])

meSig13 <- significanceTest(p1 = dataNew$f22_me,
                            n1 = dataNew$EMCRICH_n,
                            p2 = data$f22_me[data$year == 2013],
                            n2 = data$EMCRICH_n[data$year == 2013])

f22para2 <- if (cathSig13 == FALSE & protSig13 == FALSE & meSig13 != FALSE) {
  
  paste0("There was no Sig14nificant change in proportion of respondents who think the culture and traditions of Catholic and Protestant communities add to the richness and diversity of Northern Ireland society since 2013. There was a ", meSig13,
         " in the proportion who think this in relation to Minority Ethnic communities (",
         NILTyear, ": ", dataNew$f22_me, "%; 2013: ", data$f22_me[data$year == 2013], "%).")
  
} else if (cathSig13 != FALSE & protSig13 == cathSig13 & meSig13 == FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic and Protestant communities add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig13), "d since 2013 (by ", round2(abs(dataNew$f22_cath - data$f22_cath[data$year == 2013]))," percentage points (",
         NILTyear, ": ", dataNew$f22_cath, "%; 2013: ", data$f22_cath[data$year == 2013], "%) and ",
         round2(abs(dataNew$f22_prot - data$f22_prot[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_prot, "%; 2013: ", data$f22_prot[data$year == 2013], "%) respectively. There has been no Sig14nificant change in the proportion who think this in relation to Minority Ethnic communities.")
  
} else if (cathSig13 != FALSE & protSig13 == cathSig13 & meSig13 != FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic and Protestant communities add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig13), "d since 2013 (by ", round2(abs(dataNew$f22_cath - data$f22_cath[data$year == 2013]))," percentage points (",
         NILTyear, ": ", dataNew$f22_cath, "%; 2013: ", data$f22_cath[data$year == 2013], "%) and ",
         round2(abs(dataNew$f22_prot - data$f22_prot[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_prot, "%; 2013: ", data$f22_prot[data$year == 2013], "%) respectively. The proportion who think this in relation to Minority Ethnic communities has ",
         sub("t", "tly", meSig13),"d by ", round2(abs(dataNew$f22_me - data$f22_me[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_me, "%; 2013: ", data$f22_me[data$year == 2013], "%).")
  
} else if (cathSig13 == FALSE & protSig13 == FALSE & meSig13 == FALSE) {
  
  paste0("There was no Sig14nificant change in proportion of respondents who think the culture and traditions of Catholic, Protestant and Minority Ethnic communities add to the richness and diversity of Northern Ireland society since 2013.")
  
} else if (cathSig13 != FALSE & protSig13 != cathSig13 & protSig13 != FALSE & meSig13 == FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig13),"d by ", round2(abs(dataNew$f22_cath - data$f22_cath[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_cath, "%; 2013: ", data$f22_cath[data$year == 2013], "%) while the proportion who think this about Protestant communities has ",
         sub("t", "tly", protSig13), "d by ", round2(abs(dataNew$f22_prot - data$f22_prot[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_prot, "%; 2013: ", data$f22_prot[data$year == 2013], "%) since 2013. There has been no Sig14nificant change in the proportion who think this in relation to Minority Ethnic communities.")
  
} else if (cathSig13 != FALSE & protSig13 != cathSig13 & protSig13 != FALSE & meSig13 != FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig13),"d by ", round2(abs(dataNew$f22_cath - data$f22_cath[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_cath, "%; 2013: ", data$f22_cath[data$year == 2013], "%) while the proportion who think this about Protestant communities has ",
         sub("t", "tly", protSig13), "d by ", round2(abs(dataNew$f22_prot - data$f22_prot[data$year == 2013])),
         " percentage points since 2013 (",
         NILTyear, ": ", dataNew$f22_prot, "%; 2013: ", data$f22_prot[data$year == 2013], "%). The proportion who think this in relation to Minority Ethnic communities has ",
         sub("t", "tly", meSig13),"d by ", round2(abs(dataNew$f22_me - data$f22_me[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_me, "%; 2013:  ", data$f22_me[data$year == 2013], "%).")
  
} else if (cathSig13 != FALSE & protSig13 == FALSE & meSig13 == FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig13),"d by ", round2(abs(dataNew$f22_cath - data$f22_cath[data$year == 2013])),
         " percentage points since 2013 (",
         NILTyear, ": ", dataNew$f22_cath, "%; 2013: ", data$f22_cath[data$year == 2013], "%) but there has been no Sig14nificant change in the proportion who think this about Protestant communities. There has also been no Sig14nificant change in the proportion who think this in relation to Minority Ethnic communities.")
  
} else if (cathSig13 != FALSE & protSig13 == FALSE & meSig13 != FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Catholic communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", cathSig13),"d by ", round2(abs(dataNew$f22_cath - data$f22_cath[data$year == 2013])),
         " percentage points since 2013 (",
         NILTyear, ": ", dataNew$f22_cath, "%; 2013: ", data$f22_cath[data$year == 2013], "%) but there has been no Sig14nificant change in the proportion who think this about Protestant communities. The proportion who think this in relation to Minority Ethnic communities has ",
         sub("t", "tly", meSig13),"d by ", round2(abs(dataNew$f22_me - data$f22_me[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_me, "%; 2013: ", data$f22_me[data$year == 2013], "%).")
  
} else if (cathSig13 == FALSE & protSig13 != FALSE & meSig13 == FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Protestant communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", protSig13),"d by ", round2(abs(dataNew$f22_prot - data$f22_prot[data$year == 2013])),
         " percentage points since 2013 (",
         NILTyear, ": ", dataNew$f22_prot, "%; 2013: ", data$f22_prot[data$year == 2013], "%) but there has been no Sig14nificant change in the proportion who think this about Catholic communities. There has also been no Sig14nificant change in the proportion who think this in relation to Minority Ethnic communities.")
  
} else if (cathSig13 == FALSE & protSig13 != FALSE & meSig13 != FALSE) {
  
  paste0("The proportion of respondents who think the culture and traditions of Protestant communites add to the richness and diversity of Northern Ireland society has ",
         sub("t", "tly", protSig13),"d by ", round2(abs(dataNew$f22_prot - data$f22_prot[data$year == 2013])),
         " percentage points since 2013 (",
         NILTyear, ": ", dataNew$f22_prot, "%; 2013: ", data$f22_prot[data$year == 2013], "%) but there has been no Sig14nificant change in the proportion who think this about Catholic communities. The proportion who think this in relation to Minority Ethnic communities has ",
         sub("t", "tly", meSig13),"d by ", round2(abs(dataNew$f22_me - data$f22_me[data$year == 2013])),
         " percentage points (",
         NILTyear, ": ", dataNew$f22_me, "%; 2013: ", data$f22_me[data$year == 2013], "%).")
  
}