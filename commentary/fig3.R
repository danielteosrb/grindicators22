
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

f3para1 <-
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

CathSig <- significanceTest(p1 = dataNew$f3a_young,
                            n1 = dataNew$FEELCATH_Yn,
                            p2 = data$f3a_young[data$year == 2013],
                            n2 = data$FEELCATH_Yn[data$year == 2013])

CathSig <-  if (CathSig == "significant increase") {
  "more positive"
} else if (CathSig == "significant decrease") {
  "more negative"
}

CathDiff <- abs(round2(dataNew$f3a_young - data$f3a_young[data$year == 2013]))

ProtSig <- significanceTest(p1 = dataNew$f3b_young,
                            n1 = dataNew$FEELPROT_Yn,
                            p2 = data$f3b_young[data$year == 2013],
                            n2 = data$FEELPROT_Yn[data$year == 2013])

ProtSig <-  if (ProtSig == "significant increase") {
  "more positive"
} else if (ProtSig == "significant decrease") {
  "more negative"
}

ProtDiff <- abs(round2(dataNew$f3b_young - data$f3b_young[data$year == 2013]))

MESig <- significanceTest(p1 = dataNew$f3c_young,
                          n1 = dataNew$FEELMEC_Yn,
                          p2 = data$f3c_young[data$year == 2013],
                          n2 = data$FEELMEC_Yn[data$year == 2013])

MESig <-  if (MESig == "significant increase") {
  "more positive"
} else if (MESig == "significant decrease") {
  "more negative"
}

MEDiff <- abs(round2(dataNew$f3c_young - data$f3c_young[data$year == 2013]))

f3para2 <-
  # All three significant and the same
  if (CathSig != FALSE & CathSig == ProtSig & ProtSig == MESig) {
    paste0("Young people in ", YLTyear," are ", CathSig, " than their counterparts in 2013 towards Catholic communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%), Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%), and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]), "%).")
    # No significant change in all three
  } else if (CathSig == FALSE & ProtSig == FALSE & MESig == FALSE) {
    paste0("There is no significant change between young people in ", YLTyear, " and their counterparts in 2013 in their attitudes towards Catholic communities, Protestant Communities and Minority Ethnic communities.")
    # Only Catholic not significant while Protestant and Minority Ethnic the same
  } else if (CathSig == FALSE & ProtSig != FALSE & ProtSig == MESig) {
    paste0("Young people in ", YLTyear," are ", ProtSig,
           " than their counterparts in 2013 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%) and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]), 
           "%). However, there was no significant change in young people's attitudes towards Catholic communities.")
    # Only Catholic not significant while Protestant and Minority Ethnic are significant but different
  } else if (CathSig == FALSE & ProtSig != FALSE & MESig != FALSE & ProtSig != MESig) {
    paste0("Young people in ", YLTyear," are ", ProtSig, " than their counterparts in 2013 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%), but are ", MESig, " towards Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Catholic communities since 2013.")
    # Only Protestant not significant while Catholic and Minority Ethnic the same
  } else if (ProtSig == FALSE & CathSig != FALSE & CathSig == MESig) {
    paste0("Young people in ", YLTyear," are ", CathSig, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%) and Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Protestant communities since 2013.")
    # Only Protestant not significant while Catholic and Minority Ethnic are significant but different
  } else if (ProtSig == FALSE & CathSig != FALSE & MESig != FALSE & CathSig != MESig) {
    paste0("Young people in ", YLTyear," are ", CathSig, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%) but are ", MESig, " towards Minority Ethnic communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Protestant communities since 2013.")
    # Only Minority Ethnic not significant while Catholic and Protestant the same
  } else if (ProtSig == FALSE & CathSig != FALSE & CathSig == MESig) {
    paste0("Young people in ", YLTyear," are ", CathSig, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%) and Protesant communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Minority Ethnic communities since 2013.")
    # Only Minority Ethnic not significant while Catholic and Protestant are significant but different
  } else if (ProtSig == FALSE & CathSig != FALSE & MESig != FALSE & CathSig != MESig) {
    paste0("Young people in ", YLTyear," are ", CathSig, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%) but are ", ProtSig, " towards Protestant communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Minority Ethnic communities since 2013.")
    # Only Catholic significant
  } else if (CathSig != FALSE & ProtSig == FALSE & MESig == FALSE) {
    paste0("Young people in ", YLTyear," are ", CathSig, " than their counterparts in 2013 towards Catholic Communities (", YLTyear , ": ", round2(dataNew$f3a_young), "%; 2013: ", round2(data$f3a_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Protestant or Minority Ethnic communities since 2013.")
    # Only Protestant significant
  } else if (CathSig == FALSE & ProtSig != FALSE & MESig == FALSE) {
    paste0("Young people in ", YLTyear," are ", ProtSig, " than their counterparts in 2013 towards Protestant Communities (", YLTyear , ": ", round2(dataNew$f3b_young), "%; 2013: ", round2(data$f3b_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Catholic or Minority Ethnic communities since 2013.")
    # Only Minority Ethnic significant
  } else if (CathSig == FALSE & ProtSig == FALSE & MESig != FALSE) {
    paste0("Young people in ", YLTyear," are ", MESig, " than their counterparts in 2013 towards Minority Ethnic Communities (", YLTyear , ": ", round2(dataNew$f3c_young), "%; 2013: ", round2(data$f3c_young[data$year == 2013]),
           "%). However, there was no significant change in young people's attitudes towards Catholic or Protestant communities since 2013.")
  }