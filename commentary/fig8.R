f8 <- round2(dataNew$f8_pct)

f8Diff <- abs(round2(dataNew$f8_pct - dataOld$f8_pct))

f8Sig <- significanceTest(p1 = dataNew$f8_pct,
                          n1 = dataNew$SCHLCULT2_n,
                          p2 = dataOld$f8_pct,
                          n2 = dataOld$SCHLCULT2_n)

f8SigCP <- significanceTest(p1 = dataNew$f8_Cath,
                            n1 = dataNew$SCHLCULT2_nCath,
                            p2 = dataNew$f8_Prot,
                            n2 = dataNew$SCHLCULT2_nProt)

f8SigCNR <- significanceTest(p1 = dataNew$f8_Cath,
                             n1 = dataNew$SCHLCULT2_nCath,
                             p2 = dataNew$f8_NoR,
                             n2 = dataNew$SCHLCULT2_nNoR)

f8SigPNR <- significanceTest(p1 = dataNew$f8_Prot,
                             n1 = dataNew$SCHLCULT2_nProt,
                             p2 = dataNew$f8_NoR,
                             n2 = dataNew$SCHLCULT2_nNoR)

f8Siggen <- significanceTest(p1 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), gender = "Male"),
                             n1 = unweighted_n(NILT$SCHLCULT2[NILT$RSEX == "Male"]),
                             p2 = colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), gender = "Female"),
                             n2 = unweighted_n(NILT$SCHLCULT2[NILT$RSEX == "Female"]))

f8religionsentence <- if (f8SigCP == FALSE & f8SigCNR == f8SigCP & f8SigPNR == f8SigCP) {
  pasteo("There are no differences in responses when analysed by religion.")
  # Cath/NoRel significant, Prot/NoRel same, Cath/Prot not sig
} else if (f8SigCNR != FALSE & f8SigPNR == f8SigCNR & f8SigCP == FALSE) {
  paste0("Respondents with no religion (", dataNew$f8_NoR, "%) ", if (f8SigCNR == "significant increase") {"less"} else {"more"}, " often report that their child's school is somewhere their children can be open about their cultural identity than either Catholic (", dataNew$f8_Cath, "%) or Protestant (", dataNew$f8_Prot, "%) respondents. There is no significant difference between the proportion of Catholic and Protestant respondents who report this.")
  # All sig and same
} else if (f8SigCP != FALSE & f8SigCNR == f8SigCP & f8SigPNR == f8SigCP) {
  paste0("Catholic respondents (", colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "Catholic"), "%) are significantly ", if (f8SigCNR == "significant increase") {"more"} else {"less"}, " likely than either Protestant respondents (",
         colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "Protestant"), "%) or respondents with no religion (",
         colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), religion = "No religion"), "%) to report that their child's school is somewhere their children can be open about their cultural identity. Protestant respondents were signficantly ", if (f8SigPNR == "significant increase") {"more"} else {"less"}, " likely than respondents with no religion to report this.")
} else if (f8SigCNR != FALSE & f8SigPNR == f8SigCNR & f8SigCP != FALSE & f8SigCP != f8SigCNR) {
  paste0("Respondents with no religion (", dataNew$f8_NoR, "%) are significantly ", if (f8SigCNR == "significant increase") {"less"} else {"more"}, " likely than either Catholic (", dataNew$f8_Cath, "%) or Protestant (", dataNew$f8_Prot, "%) respondents. Protestant respondents are significantly ", if (f8SigCP == "significant increase") {"less"} else {"more"}, " likely to report this than Catholic respondents.")
}

f8gendersentence <- if (f8Siggen != FALSE) {
  paste0("Women (", colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), gender = "Female"), "%) are significantly ", if (f8Siggen == "significant decrease") {"more"} else {"less"}, " likely than men (", colPct(NILT, SCHLCULT2, c("Yes, definitely", "Yes, probably"), gender = "Male"), "%) to report that their child's school is somewhere their children can be open about their cultural identity.")
} else {
  paste0("There are ", if (f8SigCP == FALSE & f8SigCNR == f8SigCP & f8SigPNR == f8SigCP) {"also "}, "no differences in responses when analysed by gender.")
}

f8para1 <- if (f8Sig != FALSE) {
  paste0("Since ", NILTyear - 1, ", there has been a ", f8Sig, " in the proportion of those with children at school who think that their child's school is somewhere their children can be open about their cultural identity (", NILTyear , ": ", round2(dataNew$f8_pct), "%; ", NILTyear - 1, ": ", round2(dataOld$f8_pct), "%).")
} else {
  paste0("Since ", NILTyear - 1, ", there has been no significant change in the proportion of those with children at school who think that their child's school is somewhere their children can be open about their cultural identity.")
}

protSig <- significanceTest(p1 = dataNew$f8_Prot,
                            n1 = dataNew$SCHLCULT2_nProt,
                            p2 = dataOld$f8_Prot,
                            n2 = dataOld$SCHLCULT2_nProt)

cathSig <- significanceTest(p1 = dataNew$f8_Cath,
                            n1 = dataNew$SCHLCULT2_nCath,
                            p2 = dataOld$f8_Cath,
                            n2 = dataOld$SCHLCULT2_nCath)

noRSig <- significanceTest(p1 = dataNew$f8_NoR,
                            n1 = dataNew$SCHLCULT2_nNoR,
                            p2 = dataOld$f8_NoR,
                            n2 = dataOld$SCHLCULT2_nNoR)

f8para2 <- if (protSig == FALSE & cathSig == protSig & noRSig == protSig) {
  paste0("There were no significant changes since in the proportion of those with children at school who think that their child's school is somewhere their children can be open about their cultural identity since ", NILTyear - 1, " when analysed by religion.")
} else if (protSig != FALSE & cathSig == protSig & noRSig == protSig) {
  paste0("Since ", NILTyear - 1, "the proportion of Protestant respondents (", NILTyear, ": ", dataNew$f8_Prot, "%; ", NILTyear - 1, ": ", dataOld$f8_Prot, "%), Catholic respondents (", NILTyear, ": ", dataNew$f8_Cath, "%; ", NILTyear - 1, ": ", dataOld$f8_Cath, "%) and respondents with no religion (", NILTyear, ": ", dataNew$f8_NoR, "%; ", NILTyear - 1, ": ", dataOld$f8_NoR, "%) with children at school who think that their child's school is somewhere their children can be open about their cultural identity has ", if (protSig == "significant increase") {"increased"} else {"decreased"}, " significantly.")
} else if (protSig == FALSE & cathSig != FALSE & noRSig == cathSig) {
  paste0("There was no significant change in the proportion of Protestant respondents with children at school who think that their child's school is somewhere their children can be open about their cultural identity, however, the proportion of both Catholic respondents (", NILTyear, ": ", dataNew$f8_Cath, "%; ", NILTyear - 1, ": ", dataOld$f8_Cath, "%) and respondents with no religion (", NILTyear, ": ", dataNew$f8_NoR, "%; ", NILTyear - 1, ": ", dataOld$f8_NoR, "%) who think this has seen a ", cathSig, ".")
} else if (cathSig == FALSE & protSig != FALSE & noRSig == protSig) {
  paste0 ("There was no significant change in the proportion of Catholic respondents with children at school who think that their child's school is somewhere their children can be open about their cultural identity, however, the proportion of both Protestant respondents (", NILTyear, ": ", dataNew$f8_Prot, "%; ", NILTyear - 1, ": ", dataOld$f8_Prot, "%) and respondents with no religion (", NILTyear, ": ", dataNew$f8_NoR, "%; ", NILTyear - 1, ": ", dataOld$f8_NoR, "%) who think this has seen a ", protSig, ".")
} else if (noRSig == FALSE & protSig != FALSE & cathSig == protSig) {
  paste0("There was no significant change in the proportion of respondents with no religion with children at school who think that their child's school is somewhere their children can be open about their cultural identity, however, the proportion of both Protestant respondents (", NILTyear, ": ", dataNew$f8_Prot, "%; ", NILTyear - 1, ": ", dataOld$f8_Prot, "%) and Catholic respondents (", NILTyear, ": ", dataNew$f8_Cath, "%; ", NILTyear - 1, ": ", dataOld$f8_Cath, "%) who think this has seen a ", protSig, ".")
} else if (protSig == FALSE & cathSig != FALSE & noRSig != FALSE & noRSig != cathSig) {
  paste0("There was no significant change in the proportion of Protestant respondents with children at school who think that their child's school is somewhere their children can be open about their cultural identity, however, the proportion of Catholic respondents who think this has seen a ", cathSig, " (", NILTyear, ": ", dataNew$f8_Cath, "%; ", NILTyear - 1, ": ", dataOld$f8_Cath, "%), while the proprtion of respondents with no religion who think this has seen a ", noRSig, " (", NILTyear, ": ", dataNew$f8_NoR, "%; ", NILTyear - 1, ": ", dataOld$f8_NoR, "%).")
} else if (cathSig == FALSE & protSig != FALSE & noRSig != FALSE & noRSig != protSig) {
  paste0 ("There was no significant change in the proportion of Catholic respondents with children at school who think that their child's school is somewhere their children can be open about their cultural identity, however, the proportion of Protestant respondents who think this has seen a ", protSig, " (", NILTyear, ": ", dataNew$f8_Prot, "%; ", NILTyear - 1, ": ", dataOld$f8_Prot, "%), while the proportion of respondents with no religion who think this has seen a ", noRSig, " (", NILTyear, ": ", dataNew$f8_NoR, "%; ", NILTyear - 1, ": ", dataOld$f8_NoR, "%).")
} else if (noRSig == FALSE & protSig != FALSE & cathSig != FALSE & protSig != cathSig) {
  paste0("There was no significant change in the proportion of respondents with no religion with children at school who think that their child's school is somewhere their children can be open about their cultural identity, however the proportion of Protestant respondents who think this has seen a ", protSig, " (", NILTyear, ": ", dataNew$f8_Prot, "%; ", NILTyear - 1, ": ", dataOld$f8_Prot, "%), while the proportion of Catholic respondents who think this has seen a ", cathSig, " (", NILTyear, ": ", dataNew$f8_Cath, "%; ", NILTyear - 1, ": ", dataOld$f8_Cath, "%).")
}
