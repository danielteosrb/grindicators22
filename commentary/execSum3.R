safeTown <- round2(dataNew$f17_compare)

sigStatement <- if (safeSig != FALSE) {
  paste0("This is a ", safeSig, " of ", abs(f17_diff), " percentage points since ", NILTyear - 1, " (", NILTyear, ": ", dataNew$f17_compare, "%; ", NILTyear - 1, ": ", dataOld$f17_compare, "%).")
} else {
  paste0("There is no significant change in this measure since ", NILTyear - 1, ".")
}

safeProtestant <- round2(fig18$All[grepl("Protestant", fig18$location)])
safeCatholic <- round2(fig18$All[grepl("Catholic", fig18$location)])
safeGAA <- round2(fig18$All[grepl("GAA", fig18$location)])
safeOrange <- round2(fig18$All[grepl("Orange", fig18$location)])