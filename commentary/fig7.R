f7_LC <- round2(colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably")))
protLC <- round2(colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), religion = "Protestant"))
cathLC <- round2(colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), religion = "Catholic"))
noLC <- round2(colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), religion = "No religion"))

maxLC <- max(protLC, cathLC, noLC)

if (maxLC == protLC) {
  
  cathSig <- significanceTest(p1 = protLC,
                              n1 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "Protestant"]),
                              p2 = cathLC,
                              n2 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "Catholic"]))
  
  noSig <- significanceTest(p1 = protLC,
                              n1 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "Protestant"]),
                              p2 = noLC,
                              n2 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "No religion"]))
  
  leisureSentence <- if (cathSig != FALSE & noSig != FALSE) {
    paste0("Protestants (", protLC,
           "%) are significantly more likely than Catholics (",
           cathLC,
           "%) or those of no religion (", noLC,
           "%) to say leisure centres in their area are shared and open.")
  } else if (cathSig != FALSE & noSig == FALSE) {
    paste0("Protestants (", protLC,
           "%) are significantly more likely than Catholics (",
           cathLC,
           "%) to say leisure centres in their area are shared and open but there is no significant difference between Protestants who say this and those respondents with no religion (",
           noLC, "%).")
  } else if (cathSig == FALSE & noSig != FALSE) {
    paste0("There is no significant difference between Protestant (", protLC, "%) and Catholic (", cathLC,
           "%) respondents who think leisure centres are shared and open but Protestants are significantly more likely than those with no religion (",
           noLC, "%) to say this.")
  } else if (cathSig == FALSE & noSig == FALSE) {
    paste0("There is no significant difference between Protestant (", protLC, "%), Catholic (", cathLC,
           "%) or respondents with no religion (", noLC, "%)  who think leisure centres are shared and open.")
  }
  
} else if (maxLC == cathLC) {
  
  protSig <- significanceTest(p1 = cathLC,
                              n1 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "Catholic"]),
                              p2 = protLC,
                              n2 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "Protestant"]))
  
  noSig <- significanceTest(p1 = cathLC,
                            n1 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "Catholic"]),
                            p2 = noLC,
                            n2 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "No religion"]))
  
  leisureSentence <- if (protSig != FALSE & noSig != FALSE) {
    paste0("Catholics (", cathLC,
           "%) are significantly more likely than Protestants (",
           protLC,
           "%) or those of no religion (", noLC,
           "%) to say leisure centres in their area are shared and open.")
  } else if (protSig != FALSE & noSig == FALSE) {
    paste0("Catholics (", cathLC,
           "%) are significantly more likely than Protestants (",
           protLC,
           "%) to say leisure centres in their area are shared and open but there is no significant difference between Catholics who say this and those respondents with no religion (",
           noLC, "%).")
  } else if (protSig == FALSE & noSig != FALSE) {
    paste0("There is no significant difference between Catholic (", cathLC, "%) and Protestant (", protLC,
           "%) respondents who think leisure centres are shared and open but Catholics are significantly more likely than those with no religion (",
           noLC, "%) to say this.")
  } else if (protSig == FALSE & noSig == FALSE) {
    paste0("There is no significant difference between Catholic (", cathLC, "%), Protestant (", protLC,
           "%) or respondents with no religion (", noLC, "%)  who think leisure centres are shared and open.")
  }
  
} else if (maxLC == noLC) {
  
  cathSig <- significanceTest(p1 = noLC,
                              n1 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "No religion"]),
                              p2 = cathLC,
                              n2 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "Catholic"]))
  
  protSig <- significanceTest(p1 = noLC,
                            n1 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "No religion"]),
                            p2 = protLC,
                            n2 = unweighted_n(NILT$LCOPEN[NILT$RELIGCAT == "Protestant"]))
  
  leisureSentence <- if (cathSig != FALSE & protSig != FALSE) {
    paste0("Respondents with no religion (", noLC,
           "%) are significantly more likely than Catholics (",
           cathLC,
           "%) or Protestants (", protLC,
           "%) to say leisure centres in their area are shared and open.")
  } else if (cathSig != FALSE & protSig == FALSE) {
    paste0("Respondents with no religion (", noLC,
           "%) are significantly more likely than Catholics (",
           cathLC,
           "%) to say leisure centres in their area are shared and open but there is no significant difference between respondents with no religion who say this and Protestant respondents (",
           protLC, "%).")
  } else if (cathSig == FALSE & protSig != FALSE) {
    paste0("There is no significant difference between respondents with no religion (", noLC, "%) and Catholic (", cathLC,
           "%) respondents who think leisure centres are shared and open but respondents with no religion are significantly more likely than Protestants (",
           protLC, "%) to say this.")
  } else if (cathSig == FALSE & protSig == FALSE) {
    paste0("There is no significant difference between respondents with no religion (", noLC, "%), Catholic (", cathLC,
           "%) or Protestant respondents (", protLC, "%)  who think leisure centres are shared and open.")
  }
  
}

f7_park <- round2(colPct(NILT, PARKOPEN, c("Yes, definitely", "Yes, probably")))
protPark <- round2(colPct(NILT, PARKOPEN, c("Yes, definitely", "Yes, probably"), religion = "Protestant"))
cathPark <- round2(colPct(NILT, PARKOPEN, c("Yes, definitely", "Yes, probably"), religion = "Catholic"))
noPark <- round2(colPct(NILT, PARKOPEN, c("Yes, definitely", "Yes, probably"), religion = "No religion"))

maxPark <- max(protPark, cathPark, noPark)

if (maxPark == protPark) {
  
  cathSig <- significanceTest(p1 = protPark,
                              n1 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "Protestant"]),
                              p2 = cathPark,
                              n2 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "Catholic"]))
  
  noSig <- significanceTest(p1 = protPark,
                            n1 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "Protestant"]),
                            p2 = noPark,
                            n2 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "No religion"]))
  
  parkSentence <- if (cathSig != FALSE & noSig != FALSE) {
    paste0("Protestants (", protPark,
           "%) are significantly more likely than Catholics (",
           cathPark,
           "%) or those of no religion (", noPark,
           "%) to say parks in their area are shared and open.")
  } else if (cathSig != FALSE & noSig == FALSE) {
    paste0("Protestants (", protPark,
           "%) and respondents with no religion (", noPark, "%) are significantly more likely than Catholics (",
           cathPark, "%) to say parks in their area are shared and open.")
  } else if (cathSig == FALSE & noSig != FALSE) {
    paste0("There is no significant difference between Protestant (", protPark, "%) and Catholic (", cathPark,
           "%) respondents who think parks are shared and open but Protestants are significantly more likely than those with no religion (",
           noPark, "%) to say this.")
  } else if (cathSig == FALSE & noSig == FALSE) {
    paste0("There is no significant difference between Protestant (", protPark, "%), Catholic (", cathPark,
           "%) or respondents with no religion (", noPark, "%)  who think parks are shared and open.")
  }
  
} else if (maxPark == cathPark) {
  
  protSig <- significanceTest(p1 = cathPark,
                              n1 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "Catholic"]),
                              p2 = protPark,
                              n2 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "Protestant"]))
  
  noSig <- significanceTest(p1 = cathPark,
                            n1 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "Catholic"]),
                            p2 = noPark,
                            n2 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "No religion"]))
  
  parkSentence <- if (protSig != FALSE & noSig != FALSE) {
    paste0("Catholics (", cathPark,
           "%) are significantly more likely than Protestants (",
           protPark,
           "%) or those of no religion (", noPark,
           "%) to say parks in their area are shared and open.")
  } else if (protSig != FALSE & noSig == FALSE) {
    paste0("Catholics (", cathPark,
           "%) are significantly more likely than Protestants (",
           protPark,
           "%) to say parks in their area are shared and open but there is no significant difference between Catholics who say this and those respondents with no religion (",
           noPark, "%).")
  } else if (protSig == FALSE & noSig != FALSE) {
    paste0("There is no significant difference between Catholic (", cathPark, "%) and Protestant (", protPark,
           "%) respondents who think parks are shared and open but Catholics are significantly more likely than those with no religion (",
           noPark, "%) to say this.")
  } else if (protSig == FALSE & noSig == FALSE) {
    paste0("There is no significant difference between Catholic (", cathPark, "%), Protestant (", protPark,
           "%) or respondents with no religion (", noPark, "%)  who think parks are shared and open.")
  }
  
} else if (maxPark == noPark) {
  
  cathSig <- significanceTest(p1 = noPark,
                              n1 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "No religion"]),
                              p2 = cathPark,
                              n2 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "Catholic"]))
  
  protSig <- significanceTest(p1 = noPark,
                              n1 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "No religion"]),
                              p2 = protPark,
                              n2 = unweighted_n(NILT$PARKOPEN[NILT$RELIGCAT == "Protestant"]))
  
  parkSentence <- if (cathSig != FALSE & protSig != FALSE) {
    paste0("Respondents with no religion (", noPark,
           "%) are significantly more likely than Catholics (",
           cathPark,
           "%) or Protestants (", protPark,
           "%) to say parks in their area are shared and open.")
  } else if (cathSig != FALSE & protSig == FALSE) {
    paste0("Respondents with no religion (", noPark,
           "%) and Protestant respondents (", protPark, "%) are significantly more likely than Catholics (",
           cathPark,
           "%) to say parks in their area are shared and open.")
  } else if (cathSig == FALSE & protSig != FALSE) {
    paste0("There is no significant difference between respondents with no religion (", noPark, "%) and Catholic (", cathPark,
           "%) respondents who think parks are shared and open but respondents with no religion are significantly more likely than Protestants (",
           protPark, "%) to say this.")
  } else if (cathSig == FALSE & protSig == FALSE) {
    paste0("There is no significant difference between respondents with no religion (", noPark, "%), Catholic (", cathPark,
           "%) or Protestant respondents (", protPark, "%)  who think parks are shared and open.")
  }
  
}

f7_lib <- round2(colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably")))
protLib <- round2(colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably"), religion = "Protestant"))
cathLib <- round2(colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably"), religion = "Catholic"))
noLib <- round2(colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably"), religion = "No religion"))

maxLib <- max(protLib, cathLib, noLib)

if (maxLib == protLib) {
  
  cathSig <- significanceTest(p1 = protLib,
                              n1 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "Protestant"]),
                              p2 = cathLib,
                              n2 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "Catholic"]))
  
  noSig <- significanceTest(p1 = protLib,
                            n1 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "Protestant"]),
                            p2 = noLib,
                            n2 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "No religion"]))
  
  librarySentence <- if (cathSig != FALSE & noSig != FALSE) {
    paste0("Protestants (", protLib,
           "%) are significantly more likely than Catholics (",
           cathLib,
           "%) or those of no religion (", noLib,
           "%) to say libraries in their area are shared and open.")
  } else if (cathSig != FALSE & noSig == FALSE) {
    paste0("Protestants (", protLib,
           "%) are significantly more likely than Catholics (",
           cathLib,
           "%) to say libraries in their area are shared and open but there is no significant difference between Protestants who say this and those respondents with no religion (",
           noLib, "%).")
  } else if (cathSig == FALSE & noSig != FALSE) {
    paste0("There is no significant difference between Protestant (", protLib, "%) and Catholic (", cathLib,
           "%) respondents who think libraries are shared and open but Protestants are significantly more likely than those with no religion (",
           noLib, "%) to say this.")
  } else if (cathSig == FALSE & noSig == FALSE) {
    paste0("There is no significant difference between Protestant (", protLib, "%), Catholic (", cathLib,
           "%) or respondents with no religion (", noLib, "%)  who think libraries are shared and open.")
  }
  
} else if (maxLib == cathLib) {
  
  protSig <- significanceTest(p1 = cathLib,
                              n1 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "Catholic"]),
                              p2 = protLib,
                              n2 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "Protestant"]))
  
  noSig <- significanceTest(p1 = cathLib,
                            n1 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "Catholic"]),
                            p2 = noLib,
                            n2 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "No religion"]))
  
  librarySentence <- if (protSig != FALSE & noSig != FALSE) {
    paste0("Catholics (", cathLib,
           "%) are significantly more likely than Protestants (",
           protLib,
           "%) or those of no religion (", noLib,
           "%) to say libraries in their area are shared and open.")
  } else if (protSig != FALSE & noSig == FALSE) {
    paste0("Catholics (", cathLib,
           "%) are significantly more likely than Protestants (",
           protLib,
           "%) to say libraries in their area are shared and open but there is no significant difference between Catholics who say this and those respondents with no religion (",
           noLib, "%).")
  } else if (protSig == FALSE & noSig != FALSE) {
    paste0("There is no significant difference between Catholic (", cathLib, "%) and Protestant (", protLib,
           "%) respondents who think libraries are shared and open but Catholics are significantly more likely than those with no religion (",
           noLib, "%) to say this.")
  } else if (protSig == FALSE & noSig == FALSE) {
    paste0("There is no significant difference between Catholic (", cathLib, "%), Protestant (", protLib,
           "%) or respondents with no religion (", noLib, "%)  who think libraries are shared and open.")
  }
  
} else if (maxLib == noLib) {
  
  cathSig <- significanceTest(p1 = noLib,
                              n1 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "No religion"]),
                              p2 = cathLib,
                              n2 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "Catholic"]))
  
  protSig <- significanceTest(p1 = noLib,
                              n1 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "No religion"]),
                              p2 = protLib,
                              n2 = unweighted_n(NILT$LIBOPEN[NILT$RELIGCAT == "Protestant"]))
  
  librarySentence <- if (cathSig != FALSE & protSig != FALSE) {
    paste0("Respondents with no religion (", noLib,
           "%) are significantly more likely than Catholics (",
           cathLib,
           "%) or Protestants (", protLib,
           "%) to say libraries in their area are shared and open.")
  } else if (cathSig != FALSE & protSig == FALSE) {
    paste0("Respondents with no religion (", noLib,
           "%) are significantly more likely than Catholics (",
           cathLib,
           "%) to say libraries in their area are shared and open but there is no significant difference between respondents with no religion who say this and Protestant respondents (",
           protLib, "%).")
  } else if (cathSig == FALSE & protSig != FALSE) {
    paste0("There is no significant difference between respondents with no religion (", noLib, "%) and Catholic (", cathLib,
           "%) respondents who think libraries are shared and open but respondents with no religion are significantly more likely than Protestants (",
           protLib, "%) to say this.")
  } else if (cathSig == FALSE & protSig == FALSE) {
    paste0("There is no significant difference between respondents with no religion (", noLib, "%), Catholic (", cathLib,
           "%) or Protestant respondents (", protLib, "%)  who think libraries are shared and open.")
  }
  
}

f7_shop <- round2(colPct(NILT, SHCNOPEN, c("Yes, definitely", "Yes, probably")))
protShop <- round2(colPct(NILT, SHCNOPEN, c("Yes, definitely", "Yes, probably"), religion = "Protestant"))
cathShop <- round2(colPct(NILT, SHCNOPEN, c("Yes, definitely", "Yes, probably"), religion = "Catholic"))
noShop <- round2(colPct(NILT, SHCNOPEN, c("Yes, definitely", "Yes, probably"), religion = "No religion"))

maxShop <- max(protShop, cathShop, noShop)

if (maxShop == protShop) {
  
  cathSig <- significanceTest(p1 = protShop,
                              n1 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "Protestant"]),
                              p2 = cathShop,
                              n2 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "Catholic"]))
  
  noSig <- significanceTest(p1 = protShop,
                            n1 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "Protestant"]),
                            p2 = noShop,
                            n2 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "No religion"]))
  
  shopSentence <- if (cathSig != FALSE & noSig != FALSE) {
    paste0("Protestants (", protShop,
           "%) are significantly more likely than Catholics (",
           cathShop,
           "%) or those of no religion (", noShop,
           "%) to say shopping centres in their area are shared and open.")
  } else if (cathSig != FALSE & noSig == FALSE) {
    paste0("Protestants (", protShop,
           "%) are significantly more likely than Catholics (",
           cathShop,
           "%) to say shopping centres in their area are shared and open but there is no significant difference between Protestants who say this and those respondents with no religion (",
           noShop, "%).")
  } else if (cathSig == FALSE & noSig != FALSE) {
    paste0("There is no significant difference between Protestant (", protShop, "%) and Catholic (", cathShop,
           "%) respondents who think shopping centres are shared and open but Protestants are significantly more likely than those with no religion (",
           noShop, "%) to say this.")
  } else if (cathSig == FALSE & noSig == FALSE) {
    paste0("There is no significant difference between Protestant (", protShop, "%), Catholic (", cathShop,
           "%) or respondents with no religion (", noShop, "%)  who think shopping centres are shared and open.")
  }
  
} else if (maxShop == cathShop) {
  
  protSig <- significanceTest(p1 = cathShop,
                              n1 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "Catholic"]),
                              p2 = protShop,
                              n2 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "Protestant"]))
  
  noSig <- significanceTest(p1 = cathShop,
                            n1 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "Catholic"]),
                            p2 = noShop,
                            n2 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "No religion"]))
  
  shopSentence <- if (protSig != FALSE & noSig != FALSE) {
    paste0("Catholics (", cathShop,
           "%) are significantly more likely than Protestants (",
           protShop,
           "%) or those of no religion (", noShop,
           "%) to say shopping centres in their area are shared and open.")
  } else if (protSig != FALSE & noSig == FALSE) {
    paste0("Catholics (", cathShop,
           "%) are significantly more likely than Protestants (",
           protShop,
           "%) to say shopping centres in their area are shared and open but there is no significant difference between Catholics who say this and those respondents with no religion (",
           noShop, "%).")
  } else if (protSig == FALSE & noSig != FALSE) {
    paste0("There is no significant difference between Catholic (", cathShop, "%) and Protestant (", protShop,
           "%) respondents who think shopping centres are shared and open but Catholics are significantly more likely than those with no religion (",
           noShop, "%) to say this.")
  } else if (protSig == FALSE & noSig == FALSE) {
    paste0("There is no significant difference between Catholic (", cathShop, "%), Protestant (", protShop,
           "%) or respondents with no religion (", noShop, "%)  who think shopping centres are shared and open.")
  }
  
} else if (maxShop == noShop) {
  
  cathSig <- significanceTest(p1 = noShop,
                              n1 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "No religion"]),
                              p2 = cathShop,
                              n2 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "Catholic"]))
  
  protSig <- significanceTest(p1 = noShop,
                              n1 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "No religion"]),
                              p2 = protShop,
                              n2 = unweighted_n(NILT$SHCNOPEN[NILT$RELIGCAT == "Protestant"]))
  
  shopSentence <- if (cathSig != FALSE & protSig != FALSE) {
    paste0("Respondents with no religion (", noShop,
           "%) are significantly more likely than Catholics (",
           cathShop,
           "%) or Protestants (", protShop,
           "%) to say shopping centres in their area are shared and open.")
  } else if (cathSig != FALSE & protSig == FALSE) {
    paste0("Respondents with no religion (", noShop,
           "%) and Protestant respondents (", protShop, "%) are significantly more likely than Catholics (",
           cathShop,
           "%) to say shopping centres in their area are shared and open.")
  } else if (cathSig == FALSE & protSig != FALSE) {
    paste0("There is no significant difference between respondents with no religion (", noShop, "%) and Catholic (", cathShop,
           "%) respondents who think shopping centres are shared and open but respondents with no religion are significantly more likely than Protestants (",
           protShop, "%) to say this.")
  } else if (cathSig == FALSE & protSig == FALSE) {
    paste0("There is no significant difference between respondents with no religion (", noShop, "%), Catholic (", cathShop,
           "%) or Protestant respondents (", protShop, "%)  who think shopping centres are shared and open.")
  }
  
}

leisureGenderSig <- significanceTest(p1 = colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), gender = "Male"),
                                     n1 = unweighted_n(NILT$LCOPEN[NILT$RSEX == "Male"]),
                                     p2 = colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), gender = "Female"),
                                     n2 = unweighted_n(NILT$LCOPEN[NILT$RSEX == "Female"]))

parkGenderSig <- significanceTest(p1 = colPct(NILT, PARKOPEN, c("Yes, definitely", "Yes, probably"), gender = "Male"),
                                     n1 = unweighted_n(NILT$PARKOPEN[NILT$RSEX == "Male"]),
                                     p2 = colPct(NILT, PARKOPEN, c("Yes, definitely", "Yes, probably"), gender = "Female"),
                                     n2 = unweighted_n(NILT$PARKOPEN[NILT$RSEX == "Female"]))

libraryGenderSig <- significanceTest(p1 = 91.607397,
                                     n1 = 560,
                                     p2 = 94.476744,
                                     n2 = 834)

shopGenderSig <- significanceTest(p1 = 92.786421,
                                  n1 = 562,
                                  p2 = 95.461201,
                                  n2 = 827)


genderSentence <- if (leisureGenderSig == FALSE & parkGenderSig == FALSE & libraryGenderSig == FALSE & shopGenderSig == FALSE) {
  "There are no significant differences based on the gender of the respondent."
  # Libraries significant, all else not
} else if (leisureGenderSig == FALSE & parkGenderSig == FALSE & libraryGenderSig != FALSE & shopGenderSig == FALSE) {
  paste0("There are no significant differences based on gender in the proportion of respondents who believe that leisure centres, parks or shopping centres are shared and open, whilst male respondents (", colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably"), gender = "Male"), "%) are significantly ",
  if (libraryGenderSig == "significant increase") {"more"} else {"less"}, " likely than female respondents (", colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably"), gender = "Female"), "%) to say that libraries are shared and open.")
  # Leisure centres significant, all else not
} else if (leisureGenderSig != FALSE & parkGenderSig == FALSE & libraryGenderSig == FALSE & shopGenderSig == FALSE) {
  paste0("There are no significant differences based on gender in the proportion of respondents who believe that libraries, parks or shopping centres are shared and open, whilst male respondents (",
         colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), gender = "Male"), "%) are significantly ", if (leisureGenderSig == "significant increase") {"more"} else {"less"}, " likely than female respondents (",
         colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), gender = "Female"), "%) to say that leisure centres are shared and open.")
} else if (parkGenderSig == FALSE & leisureGenderSig != FALSE & libraryGenderSig == leisureGenderSig & shopGenderSig == leisureGenderSig) {
  paste0("Female respondents are significantly ", if (leisureGenderSig == "significant decrease") {"more"} else {"less"}, " likely than male respondents to say that leisure centres (female: ", colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), gender = "Female"), "%; male: ", colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"), gender = "Male"), "%), libraries (female: ", colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably"), gender = "Female"), "%; male: ", colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably"), gender = "Male"), "%) and shopping centres (female: ", colPct(NILT, SHCNOPEN, c("Yes, definitely", "Yes, probably"), gender = "Female"), "%; male: ", colPct(NILT, SHCNOPEN, c("Yes, definitely", "Yes, probably"), gender = "Male"), "%) are shared and open. There is no significant difference between male and female respondents' feelings concerning parks.")
} else { "There are some significant differences based on the gender of the respondent."
} 

# How we got here.

leisureSiglast <- significanceTest(p1 = dataNew$f7_LC,
                               n1 = dataNew$LCOPEN_n,
                               p2 = dataOld$f7_LC,
                               n2 = dataOld$LCOPEN_n)

parkSiglast <- significanceTest(p1 = dataNew$f7_park,
                            n1 = dataNew$PARKOPEN_n,
                            p2 = dataOld$f7_park,
                            n2 = dataOld$PARKOPEN_n)

librarySiglast <- significanceTest(p1 = dataNew$f7_lib,
                                   n1 = dataNew$LIBOPEN_n,
                                   p2 = dataOld$f7_lib,
                                   n2 = dataOld$LIBOPEN_n)

shopSiglast <- significanceTest(p1 = dataNew$f7_SHCN,
                            n1 = dataNew$SHCNOPEN_n,
                            p2 = dataOld$f7_SHCN,
                            n2 = dataOld$SHCNOPEN_n)

f7para1 <- 
  # All significant and the same
  if (leisureSiglast != FALSE & parkSiglast != FALSE & shopSiglast != FALSE & leisureSiglast == parkSiglast & parkSiglast == shopSiglast) {
    paste0("Since ", NILTyear - 1, ", there have been ", leisureSiglast, "s in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "%), parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%), and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%) in their area are 'shared and open' to both Protestants and Catholics.")
    # All 3 not significant
  } else if (leisureSiglast == FALSE & parkSiglast == FALSE & shopSiglast == FALSE) {
    paste0("Since ", NILTyear - 1, ", there have been no significant changes in the proportion of respondents who think leisure centres,
           parks and shopping centres in their area are 'shared and open' to both Protestant and Catholics.")
    # Only leisure centres significant
  } else if (leisureSiglast != FALSE & parkSiglast == FALSE & shopSiglast == FALSE & librarySiglast == FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been a ", leisureSiglast, " in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there were no significant changes in these proportions for parks, libraries or shopping centres.")
    # Only parks significant
  } else if (leisureSiglast == FALSE & parkSiglast != FALSE & shopSiglast == FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been a ", parkSiglast, " in the proportion of respondents who think parks (",
           NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there were no significant changes in these proportions for leisure centres or shopping centres.")
    # Only shopping centres significant
  } else if (leisureSiglast == FALSE & parkSiglast == FALSE & shopSiglast != FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been a ", shopSiglast, " in the proportion of respondents who think shopping centres (",
           NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there were no significant changes in these proportions for leisure centres or parks.")
    # Shopping centres not significant and other three the same
  } else if (leisureSiglast != FALSE & leisureSiglast == parkSiglast & librarySiglast == leisureSiglast & shopSiglast == FALSE) {
    paste0("Since ", NILTyear - 1, ", there have been ", leisureSiglast, "s in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "%), parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%) and libraries (", NILTyear, ": ", round2(dataNew$f7_lib), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_lib),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there was no significant change in this proportion for shopping centres.")
    # Parks not Significant and other two the same
  } else if (leisureSiglast != FALSE & leisureSiglast == shopSiglast & parkSiglast == FALSE) {
    paste0("Since ", NILTyear - 1, ", there have been ", leisureSiglast, "s in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "%) and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there was no significant change in this proportion for parks.")
    # Leisure centres not Significant and other two the same
  } else if (leisureSiglast == FALSE & parkSiglast != FALSE & parkSiglast == shopSiglast) {
    paste0("Since ", NILTyear - 1, ", there have been ", parkSiglast, "s in the proportion of respondents who think parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%) and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there was no significant change in this proportion for leisure centres.")
    # All significant but shopping centres different
  } else if (leisureSiglast != FALSE & leisureSiglast == parkSiglast & shopSiglast != FALSE & shopSiglast != leisureSiglast) {
    paste0("Since ", NILTyear - 1, ", there have been ", leisureSiglast, "s in the proportion of respondents who think leisure centres ",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "% and parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%) in their area are 'shared and open' to both Protestants and Catholics. However, there was a ", shopSiglast,
           " in this proportion for shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%).")
    # All significant but parks different
  } else if (leisureSiglast != FALSE & leisureSiglast == shopSiglast & parkSiglast != FALSE & parkSiglast != leisureSiglast) {
    paste0("Since ", NILTyear - 1, ", there have been ", leisureSiglast, "s in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "%) and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%) in their area are 'shared and open' to both Protestants and Catholics. However, there was a ", parkSiglast,
           " in this proportion for parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%).")
    # All significant but leisure centres different
  } else if (leisureSiglast != FALSE & leisureSiglast != shopSiglast & shopSiglast != FALSE & parkSiglast == shopSiglast) {
    paste0("Since ", NILTyear - 1, ", there have been ", shopSiglast, "s in the proportion of respondents who think parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%) and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%) in their area are 'shared and open' to both Protestants and Catholics. However, there was a ", leisureSiglast,
           " in this proportion for leisure centres (", NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "%).")
    # Shopping centres not Significant and other two different
  } else if (leisureSiglast != FALSE & parkSiglast != FALSE & leisureSiglast != parkSiglast & shopSiglast == FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been a ", leisureSiglast, " in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "%) in their area are 'shared and open' to both Protestant and Catholics and a ", parkSiglast,
           " in this proportion who think this about parks in their area (", NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%). However, there was no significant change in this proportion for shopping centres.")
    # Parks not Significant and other two different
  } else if (leisureSiglast != FALSE & parkSiglast == FALSE & leisureSiglast != shopSiglast & shopSiglast != FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been a ", leisureSiglast, " in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_LC),
           "%) in their area are 'shared and open' to both Protestant and Catholics and a ", shopSiglast,
           " in this proportion who think this about shopping centres in their area (",
           NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%). However, there was no significant change in this proportion for parks.")
    # Leisure centres not significant and other two different
  } else if (leisureSiglast == FALSE & parkSiglast != FALSE & parkSiglast != shopSiglast & shopSiglast != FALSE) {
    paste0("Since ", NILTyear - 1, ", there has been a ", parkSiglast, " in the proportion of respondents who think parks (",
           NILTyear, ": ", round2(dataNew$f7_park), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_park),
           "%) in their area are 'shared and open' to both Protestant and Catholics and a ", shopSiglast,
           " in this proportion who think this about shopping centres in their area (",
           NILTyear, ": ", round2(dataNew$f7_SHCN), "%; ", NILTyear - 1, ": ", round2(dataOld$f7_SHCN),
           "%). However, there was no significant change in this proportion for leisure centres.")
  }

leisureSig <- significanceTest(p1 = dataNew$f7_LC,
                               n1 = dataNew$LCOPEN_n,
                               p2 = data$f7_LC[data$year == 2013],
                               n2 = data$LCOPEN_n[data$year == 2013])

parkSig <- significanceTest(p1 = dataNew$f7_park,
                            n1 = dataNew$PARKOPEN_n,
                            p2 = data$f7_park[data$year == 2013],
                            n2 = data$PARKOPEN_n[data$year == 2013])

librarySig <- significanceTest(p1 = dataNew$f7_lib,
                               n1 = dataNew$LIBOPEN_n,
                               p2 = data$f7_lib[data$year == 2013],
                               n2 = data$LIBOPEN_n[data$year == 2013])

shopSig <- significanceTest(p1 = dataNew$f7_SHCN,
                            n1 = dataNew$SHCNOPEN_n,
                            p2 = data$f7_SHCN[data$year == 2013],
                            n2 = data$SHCNOPEN_n[data$year == 2013])


f7para2 <- 
  # All significant and the same
  if (leisureSig != FALSE & parkSig != FALSE & shopSig != FALSE & leisureSig == parkSig & parkSig == shopSig) {
  paste0("Since 2013, there have been ", leisureSig, "s in the proportion of respondents who think leisure centres (",
         NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
         "%), parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
         "%), and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
         "%) in their area are 'shared and open' to both Protestants and Catholics.")
    # All 3 not significant
  } else if (leisureSig == FALSE & parkSig == FALSE & shopSig == FALSE) {
    paste0("Since 2013, there have been no significant changes in the proportion of respondents who think leisure centres,
           parks and shopping centres in their area are 'shared and open' to both Protestant and Catholics.")
    # Only leisure centres significant
  } else if (leisureSig != FALSE & parkSig == FALSE & shopSig == FALSE) {
    paste0("Since 2013, there has been a ", leisureSig, " in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there were no significant changes in these proportions for parks or shopping centres.")
    # Only parks significant
  } else if (leisureSig == FALSE & parkSig != FALSE & shopSig == FALSE) {
    paste0("Since 2013, there has been a ", parkSig, " in the proportion of respondents who think parks (",
           NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there were no significant changes in these proportions for leisure centres or shopping centres.")
    # Only shopping centres significant
  } else if (leisureSig == FALSE & parkSig == FALSE & librarySig == FALSE & shopSig != FALSE) {
    paste0("Since 2013, there has been a ", shopSig, " in the proportion of respondents who think shopping centres (",
           NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there were no significant changes in these proportions for leisure centres, parks or libraries.")
    # Shopping centres not Significant and other three the same
  } else if (leisureSig != FALSE & leisureSig == parkSig & shopSig == FALSE & librarySig == leisureSig) {
    paste0("Since 2013, there have been ", leisureSig, "s in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "%), parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%) and libraries (",
           NILTyear, ": ", round2(dataNew$f7_lib), "%; 2013: ", round2(data$f7_lib[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there was no significant change in this proportion for shopping centres.")
    # Parks not Significant and other two the same
  } else if (leisureSig != FALSE & leisureSig == shopSig & parkSig == FALSE) {
    paste0("Since 2013, there have been ", leisureSig, "s in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "%) and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there was no significant change in this proportion for parks.")
    # Leisure centres not Significant and other two the same
  } else if (leisureSig == FALSE & parkSig != FALSE & parkSig == shopSig) {
    paste0("Since 2013, there have been ", parkSig, "s in the proportion of respondents who think parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%) and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics. However, there was no significant change in this proportion for leisure centres.")
    # All significant but shopping centres different
  } else if (leisureSig != FALSE & leisureSig == parkSig & shopSig != FALSE & shopSig != leisureSig) {
    paste0("Since 2013, there have been ", leisureSig, "s in the proportion of respondents who think leisure centres ",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "% and parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestants and Catholics. However, there was a ", shopSig,
           " in this proportion for shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
           "%).")
    # All significant but parks different
  } else if (leisureSig != FALSE & leisureSig == shopSig & parkSig != FALSE & parkSig != leisureSig) {
    paste0("Since 2013, there have been ", leisureSig, "s in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "%) and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestants and Catholics. However, there was a ", parkSig,
           " in this proportion for parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%).")
    # All significant but leisure centres different
  } else if (leisureSig != FALSE & leisureSig != shopSig & shopSig != FALSE & parkSig == shopSig) {
    paste0("Since 2013, there have been ", shopSig, "s in the proportion of respondents who think parks (", NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%) and shopping centres (", NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestants and Catholics. However, there was a ", leisureSig,
           " in this proportion for leisure centres (", NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "%).")
    # Shopping centres not Significant and other two different
  } else if (leisureSig != FALSE & parkSig != FALSE & leisureSig != parkSig & shopSig == FALSE) {
    paste0("Since 2013, there has been a ", leisureSig, " in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics and a ", parkSig,
           " in this proportion who think this about parks in their area (", NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%). However, there was no significant change in this proportion for shopping centres.")
    # Parks not Significant and other two different
  } else if (leisureSig != FALSE & parkSig == FALSE & leisureSig != shopSig & shopSig != FALSE) {
    paste0("Since 2013, there has been a ", leisureSig, " in the proportion of respondents who think leisure centres (",
           NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics and a ", shopSig,
           " in this proportion who think this about shopping centres in their area (",
           NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
           "%). However, there was no significant change in this proportion for parks.")
    # Leisure centres not significant and other two different
  } else if (leisureSig == FALSE & parkSig != FALSE & parkSig != shopSig & shopSig != FALSE) {
    paste0("Since 2013, there has been a ", parkSig, " in the proportion of respondents who think parks (",
           NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%) in their area are 'shared and open' to both Protestant and Catholics and a ", shopSig,
           " in this proportion who think this about shopping centres in their area (",
           NILTyear, ": ", round2(dataNew$f7_SHCN), "%; 2013: ", round2(data$f7_SHCN[data$year == 2013]),
           "%). However, there was no significant change in this proportion for leisure centres.")
    # Parks and leisure centres significant and same, other two not significant
  } else if (leisureSig != FALSE & parkSig == leisureSig & librarySig == FALSE & shopSig == FALSE) {
    paste0("Since 2013, there has been a ", parkSig, " in the proportion of respondents who think both parks (",
           NILTyear, ": ", round2(dataNew$f7_park), "%; 2013: ", round2(data$f7_park[data$year == 2013]),
           "%) and leisure centres (", NILTyear, ": ", round2(dataNew$f7_LC), "%; 2013: ", round2(data$f7_LC[data$year == 2013]),
           "%) are 'shared and open' to both Protestants and Catholics. However, there was no significant change in these proportions for libraries or shopping centres.")
  }