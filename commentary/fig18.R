GAAsafelast <- round2(fig18$All[grepl("GAA", fig18$location)])
orangeSafelast <- round2(fig18$All[grepl("Orange", fig18$location)])
cathSafelast <- round2(fig18$All[grepl("Catholic", fig18$location)])
protSafelast <- round2(fig18$All[grepl("Protestant", fig18$location)])

protSiglast <- significanceTest(p1 = dataNew$f18_prot,
                                n1 = dataNew$GOPSCHL_n,
                                p2 = dataOld$f18_prot,
                                n2 = dataOld$GOPSCHL_n) %>%
  sub(" ", "ly ", .)

cathSiglast <- significanceTest(p1 = dataNew$f18_cath,
                                n1 = dataNew$GOCSCHL_n,
                                p2 = dataOld$f18_cath,
                                n2 = dataOld$GOCSCHL_n) %>%
  sub(" ", "ly ", .)

gaaSiglast <- significanceTest(p1 = dataNew$f18_gaa,
                               n1 = dataNew$GOGAA_n,
                               p2 = dataOld$f18_gaa,
                               n2 = dataOld$GOGAA_n) %>%
  sub(" ", "ly ", .)

orangeSiglast <- significanceTest(p1 = dataNew$f18_orange,
                                  n1 = dataNew$GOOH_n,
                                  p2 = dataOld$f18_orange,
                                  n2 = dataOld$GOOH_n) %>%
  sub(" ", "ly ", .)

sigsLast <- c(protSiglast, cathSiglast, gaaSiglast, orangeSiglast)

f18para1 <- if (sum(sigsLast == "FALSE") == 0) {
  paste0("Feelings of safety have ",
         gaaSiglast, "d since ", NILTyear - 1, ": ",
         abs(round2(dataNew$f18_gaa - dataOld$f18_gaa)),
         " percentage points in GAA clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_gaa), "%); ",
         abs(round2(dataNew$f18_orange - dataOld$f18_orange)),
         " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_orange), "%); ",
         abs(round2(dataNew$f18_prot - dataOld$f18_prot)),
         " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_prot), "%); and ",
         abs(round2(dataNew$f18_cath - dataOld$f18_cath)),
         " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_cath), "%).")
} else if (sum(sigsLast == "FALSE") == 1) {
  if (protSiglast == "FALSE") {
    paste0("Feelings of safety have ",
           gaaSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_gaa - dataOld$f18_gaa)),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_gaa), "%), ",
           orangeSiglast, "d by ",
           abs(round2(dataNew$f18_orange - dataOld$f18_orange)),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_orange), "%) and ", cathSiglast,"d by ",
           abs(round2(dataNew$f18_cath - dataOld$f18_cath)),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_cath), "%). However, there were no significant changes in feelings of safety in Protestant Secondary Schools.")
  } else if (cathSiglast == "FALSE") {
    paste0("Feelings of safety have ", gaaSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_gaa - dataOld$f18_gaa)),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_gaa), "%), ",
           orangeSiglast, "d by ",
           abs(round2(dataNew$f18_orange - dataOld$f18_orange)),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_orange), "%) and ", protSiglast,"d by ",
           abs(round2(dataNew$f18_prot - dataOld$f18_prot)),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_prot), "%). However, there were no significant changes in feelings of safety in Catholic Secondary Schools.")
  } else if (gaaSiglast == "FALSE") {
    paste0("Feelings of safety have ", orangeSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_orange - dataOld$f18_orange)),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_orange), "%), ",
           protSiglast, "d by ",
           abs(round2(dataNew$f18_prot - dataOld$f18_prot)),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_prot), "%) and ",
           cathSiglast,"d by ",
           abs(round2(dataNew$f18_cath - dataOld$f18_cath)),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_cath), "%). 
           However, there were no significant changes in feelings of safety in GAA Clubs.")
  } else if (orangeSiglast == "FALSE") {
    paste0("Feelings of safety have ", gaaSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_gaa - dataOld$f18_gaa)),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_gaa), "%), ",
           protSiglast, "d by ",
           abs(round2(dataNew$f18_prot - dataOld$f18_prot)),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_prot), "%) and ",
           cathSiglast,"d by ",
           abs(round2(dataNew$f18_cath - dataOld$f18_cath)),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_cath), "%). 
           However, there were no significant changes in feelings of safety in Orange Halls.")
  }
} else if (sum(sigsLast == "FALSE") == 2) {
  if (protSiglast == "FALSE" & cathSiglast == "FALSE") {
    paste0("Feelings of safety have ", gaaSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_gaa - dataOld$f18_gaa)),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_gaa), "%) and ", orangeSiglast, "d by ",
           abs(round2(dataNew$f18_orange - dataOld$f18_orange)),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_orange), "%). However, there were no significant changes in feelings of safety in Catholic Secondary Schools or Protestant Secondary Schools.")
  } else if (protSiglast == "FALSE" & gaaSiglast == "FALSE") {
    paste0("Feelings of safety have ", orangeSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_orange - dataOld$f18_orange)),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_orange), "%) and ", cathSiglast, "d by ",
           abs(round2(dataNew$f18_cath - dataOld$f18_cath)),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_cath), "%). However, there were no significant changes in feelings of safety in GAA Clubs or Protestant Secondary Schools.")
  } else if (protSiglast == "FALSE" & orangeSiglast == "FALSE") {
    paste0("Feelings of safety have ", gaaSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_gaa - dataOld$f18_gaa)),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_gaa), "%) and ", cathSiglast, "d by ",
           abs(round2(dataNew$f18_cath - dataOld$f18_cath)),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_cath), "%). However, there were no significant changes in feelings of safety in Orange Halls or Protestant Secondary Schools.")
  } else if (cathSiglast == "FALSE" & gaaSiglast == "FALSE") {
    paste0("Feelings of safety have ", orangeSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_orange - dataOld$f18_orange)),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_orange), "%) and ", protSiglast, "d by ",
           abs(round2(dataNew$f18_prot - dataOld$f18_prot)),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_prot), "%). However, there were no significant changes in feelings of safety in GAA Clubs or Catholic Secondary Schools.")
  } else if (cathSiglast == "FALSE" & orangeSiglast == "FALSE") {
    paste0("Feelings of safety have ", gaaSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_gaa - dataOld$f18_gaa)),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_gaa), "%) and ", protSiglast, "d by ",
           abs(round2(dataNew$f18_prot - dataOld$f18_prot)),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_prot), "%). However, there were no significant changes in feelings of safety in Orange Halls or Catholic Secondary Schools.")
  } else if (gaaSiglast == "FALSE" & orangeSiglast == "FALSE") {
    paste0("Feelings of safety have ", 
           protSiglast, "d by ",
           abs(round2(dataNew$f18_prot - dataOld$f18_prot)),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_prot), "%) and ",
           cathSiglast,"d by ",
           abs(round2(dataNew$f18_cath - dataOld$f18_cath)),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_cath), "%). 
           However, there were no significant changes in feelings of safety in GAA Clubs or Orange Halls.")
  }
} else if (sum(sigsLast == "FALSE") == 3) {
  if (protSiglast != "FALSE") {
    paste0("Feelings of safety have ", protSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_prot - dataOld$f18_prot)),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_prot), "%). However, there were no significant changes in feelings of safety in GAA Clubs, Orange Halls or Catholic Secondary Schools.")
  } else if (cathSiglast != "FALSE") {
    paste0("Feelings of safety have ", cathSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_cath - dataOld$f18_cath)),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_cath), "%). However, there were no significant changes in feelings of safety in GAA Clubs, Orange Halls or Protestant Secondary Schools.")
  } else if (gaaSiglast != "FALSE") {
    paste0("Feelings of safety have ", gaaSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_gaa - dataOld$f18_gaa)),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_gaa), "%). However, there were no significant changes in feelings of safety in Orange Halls, Catholic Secondary Schools or Protestant Secondary Schools.")
  } else if (orangeSiglast != "FALSE") {
    paste0("Feelings of safety have ", orangeSiglast, "d since ", NILTyear - 1, " by ",
           abs(round2(dataNew$f18_orange - dataOld$f18_orange)),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; ", NILTyear - 1, ": ", round2(dataOld$f18_orange), "%). However, there were no significant changes in feelings of safety in GAA Clubs, Catholic Secondary Schools or Protestant Secondary Schools.")
  }
} else if (sum(sigsLast == "FALSE") == 4) {
  paste0("Since ", NILTyear - 1, ", there have been no significant changes in feelings of safety in GAA Clubs, Orange Halls, Catholic Secondary Schools or Protestant Secondary Schools.")
}


GAAsafe <- round2(fig18$All[grepl("GAA", fig18$location)])
orangeSafe <- round2(fig18$All[grepl("Orange", fig18$location)])
cathSafe <- round2(fig18$All[grepl("Catholic", fig18$location)])
protSafe <- round2(fig18$All[grepl("Protestant", fig18$location)])

protSig <- significanceTest(p1 = dataNew$f18_prot,
                            n1 = dataNew$GOPSCHL_n,
                            p2 = data$f18_prot[data$year == 2013],
                            n2 = data$GOPSCHL_n[data$year == 2013]) %>%
  sub(" ", "ly ", .)

cathSig <- significanceTest(p1 = dataNew$f18_cath,
                            n1 = dataNew$GOCSCHL_n,
                            p2 = data$f18_cath[data$year == 2013],
                            n2 = data$GOCSCHL_n[data$year == 2013]) %>%
  sub(" ", "ly ", .)

gaaSig <- significanceTest(p1 = dataNew$f18_gaa,
                            n1 = dataNew$GOGAA_n,
                            p2 = data$f18_gaa[data$year == 2013],
                            n2 = data$GOGAA_n[data$year == 2013]) %>%
  sub(" ", "ly ", .)

orangeSig <- significanceTest(p1 = dataNew$f18_orange,
                           n1 = dataNew$GOOH_n,
                           p2 = data$f18_orange[data$year == 2013],
                           n2 = data$GOOH_n[data$year == 2013]) %>%
  sub(" ", "ly ", .)

sigs <- c(protSig, cathSig, gaaSig, orangeSig)

f18para2 <- if (sum(sigs == "FALSE") == 0) {
  paste0("Feelings of safety have ",
         gaaSig, "d since 2013: ",
         abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
         " percentage points in GAA clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%); ",
         abs(round2(dataNew$f18_orange - data$f18_orange[data$year == 2013])),
         " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; 2013: ", round2(data$f18_orange[data$year == 2013]), "%); ",
         abs(round2(dataNew$f18_prot - data$f18_prot[data$year == 2013])),
         " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; 2013: ", round2(data$f18_prot[data$year == 2013]), "%); and ",
         abs(round2(dataNew$f18_cath - data$f18_cath[data$year == 2013])),
         " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; 2013: ", round2(data$f18_cath[data$year == 2013]), "%).")
} else if (sum(sigs == "FALSE") == 1) {
  if (protSig == "FALSE") {
    paste0("Feelings of safety have ",
           gaaSig, "d since 2013 by ",
           abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%), ",
           orangeSig, "d by ",
           abs(round2(dataNew$f18_orange - data$f18_orange[data$year == 2013])),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; 2013: ", round2(data$f18_orange[data$year == 2013]), "%) and ", cathSig,"d by ",
           abs(round2(dataNew$f18_cath - data$f18_cath[data$year == 2013])),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; 2013: ", round2(data$f18_cath[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in Protestant Secondary Schools.")
  } else if (cathSig == "FALSE") {
    paste0("Feelings of safety have ", gaaSig, "d since 2013 by ",
           abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%), ",
           orangeSig, "d by ",
           abs(round2(dataNew$f18_orange - data$f18_orange[data$year == 2013])),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; 2013: ", round2(data$f18_orange[data$year == 2013]), "%) and ", protSig,"d by ",
           abs(round2(dataNew$f18_prot - data$f18_prot[data$year == 2013])),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; 2013: ", round2(data$f18_prot[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in Catholic Secondary Schools.")
  } else if (gaaSig == "FALSE") {
    paste0("Feelings of safety have ", orangeSig, "d since 2013 by ",
           abs(round2(dataNew$f18_orange - data$f18_orange[data$year == 2013])),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; 2013: ", round2(data$f18_orange[data$year == 2013]), "%), ",
           protSig, "d by ",
           abs(round2(dataNew$f18_prot - data$f18_prot[data$year == 2013])),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; 2013: ", round2(data$f18_prot[data$year == 2013]), "%) and ",
           cathSig,"d by ",
           abs(round2(dataNew$f18_cath - data$f18_cath[data$year == 2013])),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; 2013: ", round2(data$f18_cath[data$year == 2013]), "%). 
           However, there were no significant changes in feelings of safety in GAA Clubs.")
  } else if (orangeSig == "FALSE") {
    paste0("Feelings of safety have ", gaaSig, "d since 2013 by ",
           abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%), ",
           protSig, "d by ",
           abs(round2(dataNew$f18_prot - data$f18_prot[data$year == 2013])),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; 2013: ", round2(data$f18_prot[data$year == 2013]), "%) and ",
           cathSig,"d by ",
           abs(round2(dataNew$f18_cath - data$f18_cath[data$year == 2013])),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; 2013: ", round2(data$f18_cath[data$year == 2013]), "%). 
           However, there were no significant changes in feelings of safety in Orange Halls.")
  }
} else if (sum(sigs == "FALSE") == 2) {
  if (protSig == "FALSE" & cathSig == "FALSE") {
    paste0("Feelings of safety have ", gaaSig, "d since 2013 by ",
           abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%) and ", orangeSig, "d by ",
           abs(round2(dataNew$f18_orange - data$f18_orange[data$year == 2013])),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; 2013: ", round2(data$f18_orange[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in Catholic Secondary Schools or Protestant Secondary Schools.")
  } else if (protSig == "FALSE" & gaaSig == "FALSE") {
    paste0("Feelings of safety have ", orangeSig, "d since 2013 by ",
           abs(round2(dataNew$f18_orange - data$f18_orange[data$year == 2013])),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; 2013: ", round2(data$f18_orange[data$year == 2013]), "%) and ", cathSig, "d by ",
           abs(round2(dataNew$f18_cath - data$f18_cath[data$year == 2013])),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; 2013: ", round2(data$f18_cath[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in GAA Clubs or Protestant Secondary Schools.")
  } else if (protSig == "FALSE" & orangeSig == "FALSE") {
    paste0("Feelings of safety have ", gaaSig, "d since 2013 by ",
           abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%) and ", cathSig, "d by ",
           abs(round2(dataNew$f18_cath - data$f18_cath[data$year == 2013])),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; 2013: ", round2(data$f18_cath[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in Orange Halls or Protestant Secondary Schools.")
  } else if (cathSig == "FALSE" & gaaSig == "FALSE") {
    paste0("Feelings of safety have ", gaaSig, "d since 2013 by ",
           abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%) and ", orangeSig, "d by ",
           abs(round2(dataNew$f18_orange - data$f18_orange[data$year == 2013])),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; 2013: ", round2(data$f18_orange[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in Catholic Secondary Schools or Protestant Secondary Schools.")
  } else if (cathSig == "FALSE" & orangeSig == "FALSE") {
    paste0("Feelings of safety have ", gaaSig, "d since 2013 by ",
           abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%) and ", protSig, "d by ",
           abs(round2(dataNew$f18_prot - data$f18_prot[data$year == 2013])),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; 2013: ", round2(data$f18_prot[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in Orange Halls or Catholic Secondary Schools.")
  } else if (gaaSig == "FALSE" & orangeSig == "FALSE") {
    paste0("Feelings of safety have ", 
           protSig, "d by ",
           abs(round2(dataNew$f18_prot - data$f18_prot[data$year == 2013])),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; 2013: ", round2(data$f18_prot[data$year == 2013]), "%) and ",
           cathSig,"d by ",
           abs(round2(dataNew$f18_cath - data$f18_cath[data$year == 2013])),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; 2013: ", round2(data$f18_cath[data$year == 2013]), "%). 
           However, there were no significant changes in feelings of safety in GAA Clubs or Orange Halls.")
  }
} else if (sum(sigs == "FALSE") == 3) {
  if (protSig != "FALSE") {
    paste0("Feelings of safety have ", protSig, "d since 2013 by ",
           abs(round2(dataNew$f18_prot - data$f18_prot[data$year == 2013])),
           " percentage points in Protestant Secondary Schools (", NILTyear, ": ", dataNew$f18_prot, "%; 2013: ", round2(data$f18_prot[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in GAA Clubs, Orange Halls or Catholic Secondary Schools.")
  } else if (cathSig != "FALSE") {
    paste0("Feelings of safety have ", cathSig, "d since 2013 by ",
           abs(round2(dataNew$f18_cath - data$f18_cath[data$year == 2013])),
           " percentage points in Catholic Secondary Schools (", NILTyear, ": ", dataNew$f18_cath, "%; 2013: ", round2(data$f18_cath[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in GAA Clubs, Orange Halls or Protestant Secondary Schools.")
  } else if (gaaSig != "FALSE") {
    paste0("Feelings of safety have ", gaaSig, "d since 2013 by ",
           abs(round2(dataNew$f18_gaa - data$f18_gaa[data$year == 2013])),
           " percentage points in GAA Clubs (", NILTyear, ": ", dataNew$f18_gaa, "%; 2013: ", round2(data$f18_gaa[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in Orange Halls, Catholic Secondary Schools or Protestant Secondary Schools.")
  } else if (orangeSig != "FALSE") {
    paste0("Feelings of safety have ", orangeSig, "d since 2013 by ",
           abs(round2(dataNew$f18_orange - data$f18_orange[data$year == 2013])),
           " percentage points in Orange Halls (", NILTyear, ": ", dataNew$f18_orange, "%; 2013: ", round2(data$f18_orange[data$year == 2013]), "%). However, there were no significant changes in feelings of safety in GAA Clubs, Catholic Secondary Schools or Protestant Secondary Schools.")
  }
} else if (sum(sigs == "FALSE") == 4) {
  "Since 2013, there have been no significant changes in feelings of safety in GAA Clubs, Orange Halls, Catholic Secondary Schools or Protestant Secondary Schools."
}
