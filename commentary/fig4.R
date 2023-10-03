cathprotSig <- significanceTest(p1 = fig4b$often[grepl("Catholic", fig4b$religion)],
                                n1 = unweighted_n(YLT$SOCDIFF[YLT$RELIGCAT == "Catholic"]),
                                p2 = fig4b$often[grepl("Protestant", fig4b$religion)],
                                n2 = unweighted_n(YLT$SOCDIFF[YLT$RELIGCAT == "Protestant"]))

cathnorSig <- significanceTest(p1 = fig4b$often[grepl("Catholic", fig4b$religion)],
                                n1 = unweighted_n(YLT$SOCDIFF[YLT$RELIGCAT == "Catholic"]),
                                p2 = fig4b$often[grepl("No religion", fig4b$religion)],
                                n2 = unweighted_n(YLT$SOCDIFF[YLT$RELIGCAT == "No religion"]))

protnorSig <- significanceTest(p1 = fig4b$often[grepl("Protestant", fig4b$religion)],
                               n1 = unweighted_n(YLT$SOCDIFF[YLT$RELIGCAT == "Protestant"]),
                               p2 = fig4b$often[grepl("No religion", fig4b$religion)],
                               n2 = unweighted_n(YLT$SOCDIFF[YLT$RELIGCAT == "No religion"]))

religionSigsentence <- if (cathprotSig != FALSE & cathnorSig == cathprotSig) {
  paste0("A significantly ", if (cathprotSig == "significant increase") {"higher"} else {"lower"}, " proportion of Catholic young people report this (", fig4b$often[grepl("Catholic", fig4b$religion)], "%) than Protestant young people (", fig4b$often[grepl("Protestant", fig4b$religion)], "%) or young people with no religion (", fig4b$often[grepl("No religion", fig4b$religion)], "%).")

} else if (cathprotSig == FALSE & cathnorSig == cathprotSig & protnorSig == cathprotSig) {
  "There were no significant differences in young people who report this based on religion."
}

genderSig <- significanceTest(p1 = colPct(YLT, SOCDIFF, c("Very often", "Sometimes"), gender = "Male"),
                              n1 = dataNew$SOCDIFF_YnMale,
                              p2 = colPct(YLT, SOCDIFF, c("Very often", "Sometimes"), gender = "Female"),
                              n2 = dataNew$SOCDIFF_YnFemale)

genderSigsentence <- if(genderSig != FALSE) {
  paste0("A significantly ", if (genderSig == "significant increase") {"higher"} else {"lower"}, " proportion of young male people (", colPct(YLT, SOCDIFF, c("Very often", "Sometimes"), gender = "Male"), "%) report this than young female people (", colPct(YLT, SOCDIFF, c("Very often", "Sometimes"), gender = "Female"), "%).")
} else if (genderSig ==FALSE) {
  "There is no significant difference in the proportions of young male people and young female people who report this."
}

fig4Trend <- fig4a %>%
  select(year, overall) %>%
  mutate(change = case_when(overall > lag(overall) ~ "increase",
                            overall < lag(overall) ~ "decrease",
                            TRUE ~ "N/A")) %>%
  filter(year != 2013) %>%
  mutate(consecutive = ifelse(rownames(.) == 1, 1, NA))


for (i in 2:nrow(fig4Trend)) {
  fig4Trend$consecutive[i] <-
    if(fig4Trend$change[i] == fig4Trend$change[i-1]) {
      fig4Trend$consecutive[i-1] + 1
    } else {
      1
    }
}

lastIncrease <- fig4Trend %>%
  filter(change == "increase" & year != YLTyear) %>%
  filter(year == max(year)) %>%
  pull("year")

lastDecrease <- fig4Trend %>%
  filter(change == "decrease" & year != YLTyear) %>%
  filter(year == max(year)) %>%
  pull("year")

trendStatement <-
  if (fig4Trend$consecutive[fig4Trend$year == YLTyear] == 1 & fig4Trend$change[fig4Trend$year == YLTyear] == "increase") {
    paste0("The proportion of young people who regularly socialise or play sport with young people from a different religious background has increased for the first time since ", lastIncrease, ".")
  } else if (fig4Trend$consecutive[fig4Trend$year == YLTyear] == 1 & fig4Trend$change[fig4Trend$year == YLTyear] == "decrease") {
    paste0("The proportion of young people who regularly socialise or play sport with young people from a different religious background has reduced for the first time since ", lastDecrease, ".")
  } else if (fig4Trend$consecutive[fig4Trend$year == YLTyear] != 1 & fig4Trend$change[fig4Trend$year == YLTyear] == "increase") {
    paste0("The proportion of young people who regularly socialise or play sport with young people from a different religious background has shown an increase every year since ", lastDecrease, ".")
  } else if (fig4Trend$consecutive[fig4Trend$year == YLTyear] != 1 & fig4Trend$change[fig4Trend$year == YLTyear] == "decrease") {
    paste0("The proportion of young people who regularly socialise or play sport with young people from a different religious background has shown a reduction every year since ", lastIncrease, ".")
  }

fig4Sig <- significanceTest(p1 = dataNew$f4a_overall,
                            n1 = dataNew$SOCDIFF_Yn,
                            p2 = dataOld$f4a_overall,
                            n2 = dataOld$SOCDIFF_Yn)

fig4Sig13 <- significanceTest(p1 = dataNew$f4a_overall,
                              n1 = dataNew$SOCDIFF_Yn,
                              p2 = data$f4a_overall[data$year == 2013],
                              n2 = data$SOCDIFF_Yn[data$year == 2013])

sigStatement <-
  # No significant change from last year but significantly different from 2013
  if(fig4Sig == FALSE & fig4Sig13 != FALSE) {
    paste0("The year on year changes between 2014 and 2018 do not represent a significant change between individual years. However, the proportion of young people regularly socialising or playing sport with people from a different community is significantly ",
           if (fig4Sig13 == "significant increase") {"more"} else {"less"}, " in ", YLTyear, " than it was in 2013 (", YLTyear, ": ", dataNew$f4a_overall, "%; 2013: ", round2(data$f4a_overall[data$year == 2013]), "%).")
    # Significant change from last year but none from 2013
  } else if (fig4Sig != FALSE & fig4Sig13 == FALSE) {
    paste0("This is a ", fig4Sig, " of ", abs(round2(dataNew$f4a_overall - dataOld$f4a_overall)), " percentage points on this proportion in ", YLTyear - 1, " (", YLTyear, ": ", dataNew$f4a_overall, "%; ", YLTyear - 1, ": ", dataOld$f4a_overall,
           "%).")
    # Both significant and the same
  } else if (fig4Sig != FALSE & fig4Sig13 == fig4Sig) {
    paste0("This is a ", fig4Sig, " of ", abs(round2(dataNew$f4a_overall - dataOld$f4a_overall)), " percentage points on this proportion in ", YLTyear - 1, " (", YLTyear, ": ", dataNew$f4a_overall, "%; ", YLTyear - 1, ": ", dataOld$f4a_overall,
           "%). There is also a ", fig4Sig13, " in this proportion of young people playing sport with people from a different community between 2013 and ",
           YLTyear, " (", YLTyear, ": ", dataNew$f4a_overall, "%; 2013: ", round2(data$f4a_overall[data$year == 2013]), "%).")
    # Both significant but different
  } else if (fig4Sig != FALSE & fig4Sig13 != FALSE & fig4Sig != fig4Sig13) {
    paste0("This is a ", fig4Sig, " of ", abs(round2(dataNew$f4a_overall - dataOld$f4a_overall)), " percentage points on this proportion in ", YLTyear - 1, " (", YLTyear, ": ", dataNew$f4a_overall, "%; ", YLTyear - 1, ": ", dataOld$f4a_overall,
           "%). However, there was a ", fig4Sig13, " in this proportion of young people playing sport with people from a different community between 2013 and ",
           YLTyear, " (", YLTyear, ": ", dataNew$f4a_overall, "%; 2013: ", round2(data$f4a_overall[data$year == 2013]), "%).")
    # Neither significant
  } else if (fig4Sig == FALSE & fig4Sig13 == FALSE) {
    paste0("There was no significant change between ", YLTyear - 1, " and ", YLTyear, " in this measure. There was also no significant change in the proportion of young people playing sport with people from a different community between ", YLTyear, " and 2013.")
  }

f4para1 <- paste(trendStatement, sigStatement)