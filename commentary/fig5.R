f5_projects <- round2(fig5$young[fig5$activity == "Projects"])
f5_classes <- round2(fig5$young[fig5$activity == "Classes"])
f5_equipment <- round2(fig5$young[fig5$activity == "Facilities/\nEquipment"])
f5_all <- colPct(YLT, DONEWHAT5, "Not Ticked")