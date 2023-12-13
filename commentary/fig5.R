f5_projects <- round2(fig5$young[fig5$activity == "Projects"])
f5_classes <- round2(fig5$young[fig5$activity == "Classes"])
f5_equipment <- round2(fig5$young[fig5$activity == "Facilities/\nEquipment"])
f5_all <- colPct(YLT, DONEWHAT5, "Not Ticked")

f5_projectslast <- round2(fig5last$young[fig5last$activity == "Projects"])
f5_classeslast <- round2(fig5last$young[fig5last$activity == "Classes"])
f5_equipmentlast <- round2(fig5last$young[fig5last$activity == "Facilities/\nEquipment"])
f5_alllast <- colPct(YLTlast, DONEWHAT5, "Not ticked")

f5_projSig <- significanceTest(p1 = f5_projects,
                               n1 = f5_all,
                               p2 = f5_projectslast,
                               n2 = f5_alllast)

f5_classSig <- significanceTest(p1 = f5_classes,
                               n1 = f5_all,
                               p2 = f5_classeslast,
                               n2 = f5_alllast)

f5_equipSig <- significanceTest(p1 = f5_equipment,
                                n1 = f5_all,
                                p2 = f5_equipmentlast,
                                n2 = f5_alllast)

f5_sentence <- if (f5_projSig == FALSE & f5_classSig == FALSE & f5_equipSig == FALSE) {
  paste0("Since ", NILTyear - 1, ", there has been no significant change in the proportion of young people who have done projects, shared classes, or shared sports facilities or equipment with pupils from another school.")
} else if (f5_projSig != FALSE & f5_classSig == f5_projSig & f5_equipSig == f5_projSig) {
  paste0("Since ", NILTyear - 1, ", the proportions of young people who have done projects (", NILTyear, ": ", f5_projects, "%; ", NILTyear - 1, ": ", f5_projectslast, "%), shared classes (", NILTyear, ": ", f5_classes, "%; ", NILTyear - 1, ": ", f5_classeslast, "%) and shared sports facilities or equipment (", NILTyear, ": ", f5_equipment, "%; ", NILTyear - 1, ": ", f5_equipmentlast, "%) with pupils from another school have all seen a ", f8_projSig,".")
}
