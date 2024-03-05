# Values for current year commentary

townSafe <- dataNew$f17_compare
townSafeCath <- 37
townSafeProt <- 50
townSafeNo <- round2((fig17$strongly + fig17$agree)[fig17$respondents == "Respondents with\nno religion"])

# How we got here

f17Sig <- significanceTest(p1 = dataNew$f17_compare,
                           n1 = dataNew$NISAFEWL_n,
                           p2 = dataOld$f17_compare,
                           n2 = dataOld$NISAFEWL_n)

f17sentence1 <- paste0("In ", NILTyear, ", ", dataNew$f17_compare, "% of respondents said that they see town centres as safe and welcoming places for people of all walks of life.")

f17sentence2 <- if (f17Sig != FALSE) {
  paste0("This is a ", f17Sig, " of ", abs(dataNew$f17_compare - dataOld$f17_compare), " percentage points since ", NILTyear - 1,
         " (", NILTyear, ": ", dataNew$f17_compare, "%; ", NILTyear - 1, ": ", dataOld$f17_compare, "%).")
} else {
  paste0("There has been no significant change in this measure since ", NILTyear - 1, ".")
}

f17para1 <- paste(f17sentence1, f17sentence2)