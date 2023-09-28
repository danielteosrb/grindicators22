adults <- round2(dataNew$f1_adults)
young <- round2(dataNew$f1_young)

sigStatement <-
  # Adults significant change and young people not
  if (adultSig != FALSE & youngSig == FALSE) {
    paste0("There was a ", adultSig, " on the corresponding ", NILTyear - 1, " figure of ", round2(dataOld$f1_adults), "% for adults (", NILTyear, ": ", dataNew$f1_adults, "%; ", NILTyear - 1, ": ", dataOld$f1_adults, "%) but there was no significant difference in the change for young people.")
    # Young people significant change and adults not
  } else if (adultSig == FALSE & youngSig != FALSE) {
    paste0("There was a ", youngSig, " on the corresponding ", NILTyear - 1, " figure of ", round2(dataOld$f1_young), "% for young people (", NILTyear, ": ", dataNew$f1_young, "%; ", NILTyear - 1, ": ", dataOld$f1_young, "%) but there was no significant difference in the change for adults.")
    # Adults and young people both decrease
  } else if(adultSig == "significant decrease" & youngSig == "significant decrease") {
    paste0("These are significant decreases on the corresponding ", NILTyear - 1, " figures of ", round2(dataOld$f1_adults),"% (", NILTyear, ": ", dataNew$f1_adults, "%; ", NILTyear - 1, ": ", dataOld$f1_adults, "%) and ", dataOld$f1_young, "% (", NILTyear, ": ", dataNew$f1_young, "%; ", NILTyear - 1, ": ", dataOld$f1_young, "%).")
    # Adults and young people both increase
  } else if(adultSig == "significant increase" & youngSig == "significant increase") {
    paste0("These are significant increases on the corresponding ", NILTyear - 1, " figures of ", round2(dataOld$f1_adults),"% (", NILTyear, ": ", dataNew$f1_adults, "%; ", NILTyear - 1, ": ", dataOld$f1_adults, "%)
 and ", dataOld$f1_young, "% (", NILTyear, ": ", dataNew$f1_young, "%; ", NILTyear - 1, ": ", dataOld$f1_young, "%) (", NILTyear, ": ", dataNew$f1_young, "%; ", NILTyear - 1, ": ", dataOld$f1_young, "%).")
    # Adults significant increase and young people decrease
  } else if(adultSig == "significant increase" & youngSig == "significant decrease") {
    paste0("There was a significant increase on the corresponding ", NILTyear - 1, " figure of ", round2(dataOld$f1_adults), "% for adults (", NILTyear, ": ", dataNew$f1_adults, "%; ", NILTyear - 1, ": ", dataOld$f1_adults, "%)
 but there was a significant decrease in the change for young people (", round2(dataOld$f1_young), "%) (", NILTyear, ": ", dataNew$f1_young, "%; ", NILTyear - 1, ": ", dataOld$f1_young, "%).")
    # Adults significant decrease and young people increase
  } else if(adultSig == "significant decrease" & youngSig == "significant increase") {
    paste0("There was a significant increase on the corresponding ", NILTyear - 1, " figure of ", round2(dataOld$f1_young), "% for young people (", NILTyear, ": ", dataNew$f1_adults, "%; ", NILTyear - 1, ": ", dataOld$f1_adults, "%)
 but there was a significant decrease in the change for adults (", round2(dataOld$f1_adults), "%) (", NILTyear, ": ", dataNew$f1_adults, "%; ", NILTyear - 1, ": ", dataOld$f1_adults, "%).")
    # Neither significant
  } else if (adultSig == FALSE & youngSig == FALSE) {
    paste0("There was no significant change in the corresponding ", NILTyear - 1, " figures.")
  }

projects <- round2(fig5$young[fig5$activity == "Projects"])
classes <- round2(fig5$young[fig5$activity == "Classes"])
equipment <- round2(fig5$young[fig5$activity == "Facilities/\nEquipment"])