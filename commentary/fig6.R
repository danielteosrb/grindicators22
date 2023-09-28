f6no <- round2(dataNew$f6b_no)
f6yes <- round2(dataNew$f6b_yes)

yesApprox <- if (f6yes == 10) {
  "a tenth"
} else if (f6yes %in% c(9, 11)) {
  "around a tenth"
} else if (f6yes %in% 12:13) {
  "around an eighth"
} else if (f6yes %in% 14:15) {
  "under a sixth"
} else if (f6yes == 16) {
  "around a sixth"
} else if (f6yes %in% 17:18) {
  "around three eighths"
} else if (f6yes == 20) {
  "a fifth"
} else if (f6yes %in% c(19, 21)) {
  "around a fifth"
} else if (f6yes %in% 22:24) {
  "just under a quarter"
} else if (f6yes == 25) {
  "around a quarter"
} else if (f6yes %in% 26:27) {
  "over a quarter"
} else if (f6yes %in% 28:29) {
  "just under three tenths"
} else if (f6yes == 30) {
  "three tenths"
} else if (f6yes %in% 31:32) {
  "just under a third"
} else if (f6yes == 33) {
  "around a third"
}

cath <- round2(colPct(NILT, COMDIV, c("A lot", "A little"), religion = "Catholic"))
prot <- round2(colPct(NILT, COMDIV, c("A lot", "A little"), religion = "Protestant"))
noR <- round2(colPct(NILT, COMDIV, c("A lot", "A little"), religion = "No religion"))

male <- round2(colPct(NILT, COMDIV, c("A lot", "A little"), gender = "Male"))
female <- round2(colPct(NILT, COMDIV, c("A lot", "A little"), gender = "Female"))

if (cath == max(c(cath, prot, noR))) {
  
  relSigProt <- significanceTest(p1 = cath,
                                 n1 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "Catholic"]),
                                 p2 = prot,
                                 n2 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "Protestant"]))
  
  relStatement1 <- if (relSigProt == "significant increase") {
    paste0("A significantly higher proportion of Catholic respondents (", cath, "%) think that this happens 'a little' or 'a lot' compared to Protestant respondents (", prot, "%)")
  } else {
    paste0("There was no significant difference in the proportion of Catholic respondents (", cath, "%) who think that this happens 'a little' or 'a lot' compared to Protestant respondents (", prot, "%)")
  }
  
  relSigNo <- significanceTest(p1 = cath,
                               n1 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "Catholic"]),
                               p2 = noR,
                               n2 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "No religion"]))
  
  relStatement2 <- if (relSigProt == relSigNo) {
    paste0(" or those with no religion (", noR, "%).")
  } else if (relSigNo == FALSE) {
    paste0(" but there was no significant difference between Catholic respondents and those with no religion (", noR, "%).")
  } else {
    paste0(" but there was a significantly higher proportion of Catholic respondents who thought this than those with no religion (", noR, "%).")
  }
  
  relStatement <- paste0(relStatement1, relStatement2)
  
} else if (prot == max(c(cath, prot, noR))) {
  
  relSigCath <- significanceTest(p1 = prot,
                                 n1 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "Protestant"]),
                                 p2 = cath,
                                 n2 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "Catholic"]))
  
  relStatement1 <- if (relSigCath != FALSE) {
    paste0("A significantly ", if (relSigCath == "significant increase") {"higher"} else {"lower"}, " proportion of Protestant respondents (", prot, "%) think that this happens 'a little' or 'a lot' compared to Catholic respondents (", cath, "%).")
  } else if (relSigCath == FALSE) {
    paste0("There was no significant difference in the proportion of Protestant respondents (", prot, "%) who think that this happens 'a little' or 'a lot' compared to Catholic respondents (", cath, "%)")
  }
  
  relSigNo <- significanceTest(p1 = prot,
                               n1 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "Protestant"]),
                               p2 = noR,
                               n2 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "No religion"]))
  
  relStatement2 <- if (relSigCath == relSigNo) {
    paste0(" or those with no religion (", noR, "%).")
  } else if (relSigNo == FALSE) {
    paste0(" but there was no significant difference between Protestant respondents and those with no religion (", noR, "%).")
  } else {
    paste0(" but there was a significantly ", if (relSigNo == "significant decrease") {"lower"} else {"higher"}, " proportion of Protestant respondents who thought this than those with no religion (", noR, "%).")
  }

  relStatement <- paste0(relStatement1, relStatement2)
  
} else if (noR == max(c(cath, prot, noR))) {

  relSigCath <- significanceTest(p1 = noR,
                                 n1 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "No religion"]),
                                 p2 = cath,
                                 n2 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "Catholic"]))
  
  relStatement1 <- if (relSigCath != FALSE) {
    paste0("A ", if (relSigCath == "significant increase") {"higher"} else {"lower"}, " proportion of respondents with no religion (", noR, "%) think that this happens 'a little' or 'a lot' compared to Catholic respondents (", cath, "%)")
  } else {
    paste0("There was no significant difference in the proportion of respondents with no religion (", noR, "%) who think that this happens 'a little' or 'a lot' compared to Catholic respondents (", cath, "%)")
  }
  
  relSigProt <- significanceTest(p1 = noR,
                                 n1 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "No religion"]),
                                 p2 = prot,
                                 n2 = unweighted_n(NILT$COMDIV[NILT$RELIGCAT == "Protestant"]))
  
  relStatement2 <- if (relSigCath == relSigNo) {
    paste0(" or Protestant respondents (", prot, "%).")
  } else if (relSigNo == FALSE) {
    paste0(" but there was no significant difference between Protestant respondents and those with no religion (", noR, "%).")
  } else {
    paste0(" but there was a significantly higher proportion of respondents with no religion who thought this than Protestant respondents (", prot, "%).")
  }
  
  relStatement <- paste0(relStatement1, relStatement2)
    
}

genderSig <- significanceTest(p1 = male,
                              n1 = unweighted_n(NILT$COMDIV[NILT$RSEX == "Male"]),
                              p2 = female,
                              n2 = unweighted_n(NILT$COMDIV[NILT$RSEX == "Female"]))

genderStatement <-
  if (genderSig == FALSE) {
    "There was no significant difference between male and female respondents who think that this happens 'a little or a lot'."
  } else if (genderSig != FALSE) {
    paste0("A ", if (genderSig == "significant increase") {"higher"} else {"lower"}, " proportion of male respondents (", male, "%) think that this happens 'a little' or 'a lot' compared to female respondents (", female, "%).")
  }

noSig <- significanceTest(p1 = dataNew$f6b_no,
                          n1 = dataNew$COMDIV_n,
                          p2 = dataOld$f6b_no,
                          n2 = dataOld$COMDIV_n)

yesSig <- significanceTest(p1 = dataNew$f6b_yes,
                           n1 = dataNew$COMDIV_n,
                           p2 = dataOld$f6b_yes,
                           n2 = dataOld$COMDIV_n)

f6para1 <-
  # No change in those who do and change in those who don't
  if (yesSig == FALSE & noSig != FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been no significant difference in the proportion of adults who think Catholics and Protestants tend to go to different local shops or use different GP surgeries and other services in their area. However, there has been a ", noSig, " in the proportion of adults who think this does not happen (", NILTyear, ": ", round2(dataNew$f6b_no), "%; ", NILTyear - 1, ": ", round2(dataOld$f6b_no), "%).")
    # Change in those who do and no change in those who don't
  } else if (yesSig != FALSE & noSig == FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been a ", yesSig, " in the proportion of adults who think Catholics and Protestants tend to go to different local shops or use different GP surgeries and other services in their area (", NILTyear, ": ", round2(dataNew$f6b_yes), "%; ", NILTyear - 1, ": ", round2(dataOld$f6b_yes), "%). However, there has been no significant difference in the proportion of adults who think this does not happen.")
    # No change in either
  } else if (yesSig == FALSE & noSig == FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been no significant difference in the proportion of adults who think Catholics and Protestants tend to go to different local shops or use different GP surgeries and other services in their area. There was also no significant change in the proportion of adults who think this does not happen.")
    # Both significant
  } else if (yesSig != FALSE & noSig != FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been a ", yesSig, " in the proportion of adults who think Catholics and Protestants tend to go to different local shops or use different GP surgeries and other services in their area (", NILTyear, ": ", round2(dataNew$f6b_yes), "%; ", NILTyear - 1, ": ", round2(dataOld$f6b_yes), "%) and there was a ", noSig, " in the proportion of adults who think this does not happen (", NILTyear, ": ", round2(dataNew$f6b_no), "%; ", NILTyear - 1, ": ", round2(dataOld$f6b_no), "%).")
  }

yesSig13 <- significanceTest(p1 = dataNew$f6b_yes,
                             n1 = dataNew$COMDIV_n,
                             p2 = data$f6b_yes[data$year == 2013],
                             n2 = data$COMDIV_n[data$year == 2013])

yesSig13 <- if (yesSig13 != FALSE) {
  sub("significant", "significant overall", yesSig13)
}

f6para2 <- if (yesSig13 != FALSE) {
  paste0("Since 2013, there has been a ", yesSig13, " in the proportion who think this happens either a little or a lot (", NILTyear, ": ", round2(dataNew$f6b_yes), "%; 2013: ", round2(data$f6b_yes[data$year == 2013]), "%).")
} else {
  paste0("Since 2013, there has been no significant overall change in the proportion who think this happens either a little or a lot.")
}