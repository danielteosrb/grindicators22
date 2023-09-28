workplace <- round2(dataNew$f12_workplace)
neighbourhood <- round2(dataNew$f12_neighbourhood)
school <- round2(dataNew$f12_school)

schoolOpen <- round2(dataNew$f8_pct)

cultSig <- significanceTest(p1 = dataNew$f8_pct,
                            n1 = dataNew$SCHLCULT2_n,
                            p2 = dataOld$f8_pct,
                            n2 = dataOld$SCHLCULT2_n)

Diff <- round2(dataNew$f8_pct - dataOld$f8_pct)

sigStatement <- if (cultSig != FALSE) {
  paste0("This is a ", cultSig, " of ", Diff, " percentage points since ", NILTyear - 1, " (", NILTyear, ": ", dataNew$f8_pct, "%; ", NILTyear - 1, ": ", dataOld$f8_pct, "%).")
} else {
  paste0("There is no significant change in this measure from ", NILTyear - 1, ".")
}

leisure <- colPct(NILT, LCOPEN, c("Yes, definitely", "Yes, probably"))
park <- colPct(NILT, PARKOPEN, c("Yes, definitely", "Yes, probably"))
library <- colPct(NILT, LIBOPEN, c("Yes, definitely", "Yes, probably"))
shopping <- colPct(NILT, SHCNOPEN, c("Yes, definitely", "Yes, probably"))