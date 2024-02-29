
adultYoungSig <- significanceTest(p1 = dataNew$f3a_young,
                                  n1 = dataNew$FEELCATH_Yn,
                                  p2 = dataNew$f3a_adults,
                                  n2 = unweighted_n(NILT$FEELCATH))

adultYoungSig <- if (adultYoungSig == "significant decrease") {
  "remains significantly lower than"
} else if (adultYoungSig == "significant increase") {
  "has become significantly higher than"
} else {
  "is not significantly different to"
}

CathSig <- significanceTest(p1 = dataNew$f3a_young,
                            n1 = dataNew$FEELCATH_Yn,
                            p2 = dataOld$f3a_young,
                            n2 = dataOld$FEELCATH_Yn)

CathSig <-  if (CathSig == "significant increase") {
  "significant improvement"
} else if (CathSig == "significant decrease") {
  "significant decline"
} else {FALSE}

ProtSig <- significanceTest(p1 = dataNew$f3b_young,
                            n1 = dataNew$FEELPROT_Yn,
                            p2 = dataOld$f3b_young,
                            n2 = dataOld$FEELPROT_Yn)

ProtSig <-  if (ProtSig == "significant increase") {
  "significant improvement"
} else if (ProtSig == "significant decrease") {
  "significant decline"
} else{FALSE}

MESig <- significanceTest(p1 = dataNew$f3c_young,
                          n1 = dataNew$FEELMEC_Yn,
                          p2 = dataOld$f3c_young,
                          n2 = dataOld$FEELMEC_Yn)

MESig <-  if (MESig == "significant increase") {
  "significant improvement"
} else if (MESig == "significant decrease") {
  "significant decline"
} else{FALSE}

f3sent1 <-
  # All three significant and the same
  if (CathSig != FALSE & CathSig == ProtSig & ProtSig == MESig) {
    paste0("Young people's attitudes towards people from Catholic (", YLTyear , ": ", round2(dataNew$f3a_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_young), "%), Protestant (", YLTyear , ": ", round2(dataNew$f3b_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_young), "%), and Minority  (", YLTyear , ": ", round2(dataNew$f3c_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_young), "%) communities have shown a ", CathSig, " since ", YLTyear - 1, ".")
    # No significant change in all three
  } else if (CathSig == FALSE & ProtSig == FALSE & MESig == FALSE) {
    paste0("There has been no significant change in young people's attitudes towards people from Catholic, Protestant, and Minority Ethnic communities since ", YLTyear - 1, ".")
    # Only Catholic not significant while Protestant and Minority Ethnic the same
  } else if (CathSig == FALSE & ProtSig != FALSE & ProtSig == MESig) {
    paste0("Young people's attitudes towards people from Protestant (", YLTyear , ": ", round2(dataNew$f3b_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_young), "%), and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_young), "%) have shown a ", ProtSig, " since ", YLTyear - 1, ", while there was no significant change in young people's attitudes towards Catholic Communities.")
    # Only Catholic not significant while Protestant and Minority Ethnic are significant but different
  } else if (CathSig == FALSE & ProtSig != FALSE & MESig != FALSE & ProtSig != MESig) {
    paste0("Young people's attitudes towards people from Protestant communities have shown a ", ProtSig, " since ", YLTyear - 1, if(ProtSig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3b_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_young), "%)")},
           ". However, there was a ", MESig, " in young people's attitudes towards Minority Ethnic communities", if(MESig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3c_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_young), "%)")}, ". There was no significant change in young people's attitudes towards Catholic Communities.")
    # Only Protestant not significant while Catholic and Minority Ethnic the same
  } else if (ProtSig == FALSE & CathSig != FALSE & CathSig == MESig) {
    paste0("Young people's attitudes towards people from Catholic (", YLTyear , ": ", round2(dataNew$f3a_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_young), "%), and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_young), "%) have shown a ", CathSig, " since ", YLTyear - 1, ", while there was no significant change in young people's attitudes towards Protestant Communities.")
    # Only Protestant not significant while Catholic and Minority Ethnic are significant but different
  } else if (ProtSig == FALSE & CathSig != FALSE & MESig != FALSE & CathSig != MESig) {
    paste0("Young people's attitudes towards people from Catholic communities have shown a ", CathSig, " since ", YLTyear - 1, if(CathSig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3a_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_young), "%)")},
           ". However, there was a ", MESig, " in young people's attitudes towards Minority Ethnic communities", if(MESig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3c_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_young), "%)")}, ". There was no significant change in young people's attitudes towards Protestant Communities.")
    # Only Minority Ethnic not significant while Catholic and Protestant the same
  } else if (ProtSig == FALSE & CathSig != FALSE & CathSig == MESig) {
    paste0("Young people's attitudes towards people from Catholic (", YLTyear , ": ", round2(dataNew$f3a_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_young), "%), and Protestant communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_young), "%) have shown a ", CathSig, " since ", YLTyear - 1, ", while there was no significant change in young people's attitudes towards Minority Ethnic Communities.")
    # Only Minority Ethnic not significant while Catholic and Protestant are significant but different
  } else if (ProtSig == FALSE & CathSig != FALSE & MESig != FALSE & CathSig != MESig) {
    paste0("Young people's attitudes towards people from Catholic communities have shown a ", CathSig, " since ", YLTyear - 1, if(CathSig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3a_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_young), "%)")}, 
". However, there was a ", ProtSig, " in young people's attitudes towards Protestant communities", if(ProtSig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3b_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_young), "%)")},
". There was no significant change in young people's attitudes towards Minority Ethnic Communities.")
    # Only Catholic significant
  } else if (CathSig != FALSE & ProtSig == FALSE & MESig == FALSE) {
    paste0("Young people's attitudes towards people from Catholic communities have shown a ", CathSig, " since ", YLTyear - 1, if(CathSig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3a_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_young), "%)")}, 
           ". However, there was no significant change in young people's attitudes towards Protestant or Minority Ethnic Communities.")
    # Only Protestant significant
  } else if (CathSig == FALSE & ProtSig != FALSE & MESig == FALSE) {
    paste0("Young people's attitudes towards people from Protestant communities have shown a ", ProtSig, " since ", YLTyear - 1, if(ProtSig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3b_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_young), "%)")},
           ". However, there was no significant change in young people's attitudes towards Catholic or Minority Ethnic Communities.")
    # Only Minority Ethnic significant
  } else if (CathSig == FALSE & ProtSig == FALSE & MESig != FALSE) {
    paste0("Young people's attitudes towards people from Minority Ethnic communities have shown a ", MESig, " since ", YLTyear - 1, if(MESig!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3c_young), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_young), "%)")},
           ". However, there was no significant change in young people's attitudes towards Catholic or Protestant Communities.")
  }

CathSig13 <- significanceTest(p1 = dataNew$f3a_young,
                            n1 = dataNew$FEELCATH_Yn,
                            p2 = data$f3a_young[data$year == 2013],
                            n2 = data$FEELCATH_Yn[data$year == 2013])

CathSig13 <-  if (CathSig13 == "significant increase") {
  "more positive"
} else if (CathSig13 == "significant decrease") {
  "more negative"
}

CathDiff <- abs(round2(dataNew$f3a_young - data$f3a_young[data$year == 2013]))

ProtSig13 <- significanceTest(p1 = dataNew$f3b_young,
                            n1 = dataNew$FEELPROT_Yn,
                            p2 = data$f3b_young[data$year == 2013],
                            n2 = data$FEELPROT_Yn[data$year == 2013])

ProtSig13 <-  if (ProtSig13 == "significant increase") {
  "more positive"
} else if (ProtSig13 == "significant decrease") {
  "more negative"
}

ProtDiff <- abs(round2(dataNew$f3b_young - data$f3b_young[data$year == 2013]))

MESig13 <- significanceTest(p1 = dataNew$f3c_young,
                          n1 = dataNew$FEELMEC_Yn,
                          p2 = data$f3c_young[data$year == 2013],
                          n2 = data$FEELMEC_Yn[data$year == 2013])

MESig13 <-  if (MESig13 == "significant increase") {
  "more positive"
} else if (MESig13 == "significant decrease") {
  "more negative"
}

MEDiff <- abs(round2(dataNew$f3c_young - data$f3c_young[data$year == 2013]))

f3sent2 <-
  # All three significant and the same
  if (CathSig13 != FALSE & CathSig13 == ProtSig13 & ProtSig13 == MESig13) {
    paste0("Young people in ", YLTyear," are ", CathSig13, " than their counterparts in 2013 towards Catholic communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%), Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%), and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]), "%).")
    # No significant change in all three
  } else if (CathSig13 == FALSE & ProtSig13 == FALSE & MESig13 == FALSE) {
    paste0("There is no significant change between young people in ", YLTyear, " and their counterparts in 2013 in their attitudes towards Catholic communities, Protestant Communities and Minority Ethnic communities.")
    # Only Catholic not significant while Protestant and Minority Ethnic the same
  } else if (CathSig13 == FALSE & ProtSig13 != FALSE & ProtSig13 == MESig13) {
    paste0("Young people in ", YLTyear," are ", ProtSig13,
           " than their counterparts in 2013 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%) and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]), 
           "%). However, there was no significant change in young people's attitudes towards Catholic communities.")
    # Only Catholic not significant while Protestant and Minority Ethnic are significant but different
  } else if (CathSig13 == FALSE & ProtSig13 != FALSE & MESig13 != FALSE & ProtSig13 != MESig13) {
    paste0("Young people in ", YLTyear," are ", ProtSig13, " than their counterparts in 2013 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%), but are ", MESig13, " towards Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Catholic communities since 2013.")
    # Only Protestant not significant while Catholic and Minority Ethnic the same
  } else if (ProtSig13 == FALSE & CathSig13 != FALSE & CathSig13 == MESig13) {
    paste0("Young people in ", YLTyear," are ", CathSig13, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%) and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Protestant communities since 2013.")
    # Only Protestant not significant while Catholic and Minority Ethnic are significant but different
  } else if (ProtSig13 == FALSE & CathSig13 != FALSE & MESig13 != FALSE & CathSig13 != MESig13) {
    paste0("Young people in ", YLTyear," are ", CathSig13, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%) but are ", MESig13, " towards Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Protestant communities since 2013.")
    # Only Minority Ethnic not significant while Catholic and Protestant the same
  } else if (ProtSig13 == FALSE & CathSig13 != FALSE & CathSig13 == MESig13) {
    paste0("Young people in ", YLTyear," are ", CathSig13, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%) and Protestant communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Minority Ethnic communities since 2013.")
    # Only Minority Ethnic not significant while Catholic and Protestant are significant but different
  } else if (ProtSig13 == FALSE & CathSig13 != FALSE & MESig13 != FALSE & CathSig13 != MESig13) {
    paste0("Young people in ", YLTyear," are ", CathSig13, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%) but are ", ProtSig13, " towards Protestant communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Minority Ethnic communities since 2013.")
    # Only Catholic significant
  } else if (CathSig13 != FALSE & ProtSig13 == FALSE & MESig13 == FALSE) {
    paste0("Young people in ", YLTyear," are ", CathSig13, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Protestant or Minority Ethnic communities since 2013.")
    # Only Protestant significant
  } else if (CathSig13 == FALSE & ProtSig13 != FALSE & MESig13 == FALSE) {
    paste0("Young people in ", YLTyear," are ", ProtSig13, " than their counterparts in 2013 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Catholic or Minority Ethnic communities since 2013.")
    # Only Minority Ethnic significant
  } else if (CathSig13 == FALSE & ProtSig13 == FALSE & MESig13 != FALSE) {
    paste0("Young people in ", YLTyear," are ", MESig13, " than their counterparts in 2013 towards Minority Ethnic Communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Catholic or Protestant communities since 2013.")
  }

f3para1 <- paste(f3sent1, f3sent2)

CathSigA <- significanceTest(p1 = dataNew$f3a_adults,
                             n1 = dataNew$FEELCATH_n,
                             p2 = dataOld$f3a_adults,
                             n2 = dataOld$FEELCATH_n)

CathSigA <-  if (CathSigA == "significant increase") {
  "significant improvement"
} else if (CathSigA == "significant decrease") {
  "significant decline"
} else {FALSE}

ProtSigA <- significanceTest(p1 = dataNew$f3b_adults,
                             n1 = dataNew$FEELPROT_n,
                             p2 = dataOld$f3b_adults,
                             n2 = dataOld$FEELPROT_n)

ProtSigA <-  if (ProtSigA == "significant increase") {
  "significant improvement"
} else if (ProtSigA == "significant decrease") {
  "significant decline"
} else{FALSE}

MESigA <- significanceTest(p1 = dataNew$f3c_adults,
                           n1 = dataNew$FEELMEC_n,
                           p2 = dataOld$f3c_adults,
                           n2 = dataOld$FEELMEC_n)

MESigA <-  if (MESigA == "significant increase") {
  "significant improvement"
} else if (MESigA == "significant decrease") {
  "significant decline"
} else{FALSE}

f3sent3 <-
  # All three significant and the same
  if (CathSigA != FALSE & CathSigA == ProtSigA & ProtSigA == MESigA) {
    paste0("Among adults, attitudes towards people from Catholic (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_adults), "%), Protestant (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_adults), "%), and Minority  (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_adults), "%) communities have shown a ", CathSigA, " since ", YLTyear - 1, ".")
    # No significant change in all three
  } else if (CathSigA == FALSE & ProtSigA == FALSE & MESigA == FALSE) {
    paste0("Among adults, there has been no significant change in attitudes towards people from Catholic, Protestant, and Minority Ethnic communities since ", YLTyear - 1, ".")
    # Only Catholic not significant while Protestant and Minority Ethnic the same
  } else if (CathSigA == FALSE & ProtSigA != FALSE & ProtSigA == MESigA) {
    paste0("Among adults, attitudes towards people from Protestant (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_adults), "%), and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_adults), "%) have shown a ", ProtSigA, " since ", YLTyear - 1, ", while there was no significant change in adults' attitudes towards Catholic Communities.")
    # Only Catholic not significant while Protestant and Minority Ethnic are significant but different
  } else if (CathSigA == FALSE & ProtSigA != FALSE & MESigA != FALSE & ProtSigA != MESigA) {
    paste0("Among adults, attitudes towards people from Protestant communities have shown a ", ProtSigA, " since ", YLTyear - 1, if(ProtSigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_adults), "%)")},
           ". However, there was a ", MESigA, " in adults' attitudes towards Minority Ethnic communities", if(MESigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_adults), "%)")}, ". There was no significant change in adults' attitudes towards Catholic Communities.")
    # Only Protestant not significant while Catholic and Minority Ethnic the same
  } else if (ProtSigA == FALSE & CathSigA != FALSE & CathSigA == MESigA) {
    paste0("Among adults, attitudes towards people from Catholic (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_adults), "%), and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_adults), "%) have shown a ", CathSigA, " since ", YLTyear - 1, ", while there was no significant change in adults' attitudes towards Protestant Communities.")
    # Only Protestant not significant while Catholic and Minority Ethnic are significant but different
  } else if (ProtSigA == FALSE & CathSigA != FALSE & MESigA != FALSE & CathSigA != MESigA) {
    paste0("Among adults, attitudes towards people from Catholic communities have shown a ", CathSigA, " since ", YLTyear - 1, if(CathSigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_adults), "%)")},
           ". However, there was a ", MESigA, " in adults' attitudes towards Minority Ethnic communities", if(MESigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_adults), "%)")}, ". There was no significant change in adults' attitudes towards Protestant Communities.")
    # Only Minority Ethnic not significant while Catholic and Protestant the same
  } else if (ProtSigA == FALSE & CathSigA != FALSE & CathSigA == MESigA) {
    paste0("Among adults, attitudes towards people from Catholic (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_adults), "%), and Protestant communities (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_adults), "%) have shown a ", CathSigA, " since ", YLTyear - 1, ", while there was no significant change in adults' attitudes towards Minority Ethnic Communities.")
    # Only Minority Ethnic not significant while Catholic and Protestant are significant but different
  } else if (ProtSigA == FALSE & CathSigA != FALSE & MESigA != FALSE & CathSigA != MESigA) {
    paste0("Among adults, attitudes towards people from Catholic communities have shown a ", CathSigA, " since ", YLTyear - 1, if(CathSigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_adults), "%)")}, 
           ". However, there was a ", ProtSigA, " in adults' attitudes towards Protestant communities", if(ProtSigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_adults), "%)")},
           ". There was no significant change in adults' attitudes towards Minority Ethnic Communities.")
    # Only Catholic significant
  } else if (CathSigA != FALSE & ProtSigA == FALSE & MESigA == FALSE) {
    paste0("Among adults, attitudes towards people from Catholic communities have shown a ", CathSigA, " since ", YLTyear - 1, if(CathSigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3a_adults), "%)")}, 
           ". However, there was no significant change in adults' attitudes towards Protestant or Minority Ethnic Communities.")
    # Only Protestant significant
  } else if (CathSigA == FALSE & ProtSigA != FALSE & MESigA == FALSE) {
    paste0("YAmong adults, attitudes towards people from Protestant communities have shown a ", ProtSigA, " since ", YLTyear - 1, if(ProtSigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3b_adults), "%)")},
           ". However, there was no significant change in adults' attitudes towards Catholic or Minority Ethnic Communities.")
    # Only Minority Ethnic significant
  } else if (CathSigA == FALSE & ProtSigA == FALSE & MESigA != FALSE) {
    paste0("Among adults, towards people from Minority Ethnic communities have shown a ", MESigA, " since ", YLTyear - 1, if(MESigA!=FALSE) {paste0(" (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; ", YLTyear - 1, ": ", round2(dataOld$f3c_adults), "%)")},
           ". However, there was no significant change in adults' attitudes towards Catholic or Protestant Communities.")
  }

CathSig14A <- significanceTest(p1 = dataNew$f3a_adults,
                              n1 = dataNew$FEELCATH_n,
                              p2 = data$f3a_adults[data$year == 2014],
                              n2 = data$FEELCATH_n[data$year == 2014])

CathSig14A <-  if (CathSig14A == "significant increase") {
  "more positive"
} else if (CathSig14A == "significant decrease") {
  "more negative"
}

CathDiff <- abs(round2(dataNew$f3a_adults - data$f3a_adults[data$year == 2014]))

ProtSig14A <- significanceTest(p1 = dataNew$f3b_adults,
                              n1 = dataNew$FEELPROT_n,
                              p2 = data$f3b_adults[data$year == 2014],
                              n2 = data$FEELPROT_n[data$year == 2014])

ProtSig14A <-  if (ProtSig14A == "significant increase") {
  "more positive"
} else if (ProtSig14A == "significant decrease") {
  "more negative"
}

ProtDiff <- abs(round2(dataNew$f3b_adults - data$f3b_adults[data$year == 2014]))

MESig14A <- significanceTest(p1 = dataNew$f3c_adults,
                            n1 = dataNew$FEELMEC_n,
                            p2 = data$f3c_adults[data$year == 2014],
                            n2 = data$FEELMEC_n[data$year == 2014])

MESig14A <-  if (MESig14A == "significant increase") {
  "more positive"
} else if (MESig14A == "significant decrease") {
  "more negative"
} else {FALSE}

MEDiff <- abs(round2(dataNew$f3c_adults - data$f3c_adults[data$year == 2014]))

f3sent4 <-
  # All three significant and the same
  if (CathSig14A != FALSE & CathSig14A == ProtSig14A & ProtSig14A == MESig14A) {
    paste0("Adults in ", YLTyear," are ", CathSig14A, " than their counterparts in 2014 towards Catholic communities (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; 2014: ", round2(data$f3a_adults[data$year == 2014]),
           "%), Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; 2014: ", round2(data$f3b_adults[data$year == 2014]),
           "%), and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; 2014: ", round2(data$f3c_adults[data$year == 2014]), "%).")
    # No significant change in all three
  } else if (CathSig14A == FALSE & ProtSig14A == FALSE & MESig14A == FALSE) {
    paste0("There is no significant change between young people in ", YLTyear, " and their counterparts in 2014 in their attitudes towards Catholic communities, Protestant Communities and Minority Ethnic communities.")
    # Only Catholic not significant while Protestant and Minority Ethnic the same
  } else if (CathSig14A == FALSE & ProtSig14A != FALSE & ProtSig14A == MESig14A) {
    paste0("Adults in ", YLTyear," are ", ProtSig14A,
           " than their counterparts in 2014 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; 2014: ", round2(data$f3b_adults[data$year == 2014]),
           "%) and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; 2014: ", round2(data$f3c_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Catholic communities.")
    # Only Catholic not significant while Protestant and Minority Ethnic are significant but different
  } else if (CathSig14A == FALSE & ProtSig14A != FALSE & MESig14A != FALSE & ProtSig14A != MESig14A) {
    paste0("Adults in ", YLTyear," are ", ProtSig14A, " than their counterparts in 2014 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; 2014: ", round2(data$f3b_adults[data$year == 2014]),
           "%), but are ", MESig14A, " towards Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; 2014: ", round2(data$f3c_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Catholic communities since 2014.")
    # Only Protestant not significant while Catholic and Minority Ethnic the same
  } else if (ProtSig14A == FALSE & CathSig14A != FALSE & CathSig14A == MESig14A) {
    paste0("Adults in ", YLTyear," are ", CathSig14A, " than their counterparts in 2014 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; 2014: ", round2(data$f3a_adults[data$year == 2014]),
           "%) and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; 2014: ", round2(data$f3c_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Protestant communities since 2014.")
    # Only Protestant not significant while Catholic and Minority Ethnic are significant but different
    paste0("Adults in ", YLTyear," are ", CathSig14A, " than their counterparts in 2014 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; 2014: ", round2(data$f3a_adults[data$year == 2014]),
           "%) but are ", MESig14A, " towards Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; 2014: ", round2(data$f3c_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Protestant communities since 2014.")
    # Only Minority Ethnic not significant while Catholic and Protestant the same
  } else if (ProtSig14A != FALSE & CathSig14A == ProtSig14A &  MESig14A == FALSE) {
    paste0("Adults in ", YLTyear," are ", CathSig14A, " than their counterparts in 2014 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; 2014: ", round2(data$f3a_adults[data$year == 2014]),
           "%) and Protestant communities (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; 2014: ", round2(data$f3b_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Minority Ethnic communities since 2014.")
    # Only Minority Ethnic not significant while Catholic and Protestant are significant but different
  } else if (ProtSig14A == FALSE & CathSig14A != FALSE & MESig14A != FALSE & CathSig14A != MESig14A) {
    paste0("Adults in ", YLTyear," are ", CathSig14A, " than their counterparts in 2014 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; 2014: ", round2(data$f3a_adults[data$year == 2014]),
           "%) but are ", ProtSig14A, " towards Protestant communities (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; 2014: ", round2(data$f3b_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Minority Ethnic communities since 2014.")
    # Only Catholic significant
  } else if (CathSig14A != FALSE & ProtSig14A == FALSE & MESig14A == FALSE) {
    paste0("Adults in ", YLTyear," are ", CathSig14A, " than their counterparts in 2014 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_adults), "%; 2014: ", round2(data$f3a_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Protestant or Minority Ethnic communities since 2014.")
    # Only Protestant significant
  } else if (CathSig14A == FALSE & ProtSig14A != FALSE & MESig14A == FALSE) {
    paste0("Adults in ", YLTyear," are ", ProtSig14A, " than their counterparts in 2014 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_adults), "%; 2014: ", round2(data$f3b_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Catholic or Minority Ethnic communities since 2014.")
    # Only Minority Ethnic significant
  } else if (CathSig14A == FALSE & ProtSig14A == FALSE & MESig14A != FALSE) {
    paste0("Adults in ", YLTyear," are ", MESig14A, " than their counterparts in 2014 towards Minority Ethnic Communities (", YLTyear , ": ", round2(dataNew$f3c_adults), "%; 2014: ", round2(data$f3c_adults[data$year == 2014]),
           "%). However, there was no significant change in adults' attitudes towards Catholic or Protestant communities since 2014.")
  }

f3para2 <- paste(f3sent3, f3sent4)