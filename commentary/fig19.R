all <- round2((fig19$now + fig19$future)[fig19$resident == "All"])
residents <- round2((fig19$now + fig19$future)[fig19$resident == "Residents*"])

approxResidents <- if (residents %in% 45:48) {
  "just under a half"
} else if (residents %in% 49:51) {
  "around a half"
} else if (residents %in% 52:59) {
  "over half"
} else if (residents == 60) {
  "three-fifths"
} else if (residents %in% 61:63) {
  "over three-fifths"
} else if (residents %in% 64:65) {
  "approaching two-thirds"
} else if (residents %in% 66:67) {
  "around two-thirds"
} else if (residents %in% 68:70) {
  "over two-thirds"
} else if (residents %in% 71:74) {
  "approaching three-quarters"
} else if (residents == 75) {
  "three-quarters"
} else if (residents %in% 76:79) {
  "over three-quarters"
}

f19Sig <- significanceTest(p1 = dataNew$f19_resident,
                            n1 = dataNew$PLINEREM_Rn,
                            p2 = dataOld$f19_resident,
                            n2 = dataOld$PLINEREM_Rn)

f19para1 <- if (f19Sig == FALSE) {
  paste0("In ", NILTyear, ", ", approxResidents," (", residents,
         "%) of residents said they want the peace lines to come down now or in the future, this does not represent a significant change from ",
         NILTyear - 1, " (", round2(dataOld$f19_resident), "%).")
} else {
  paste0("In ", NILTyear, ", ", approxResidents, " (", residents,
         "%) of residents said they want the peace lines to come down now or in the future, this represents a ",
         f19Sig," from ", NILTyear - 1, " (", round2(dataOld$f19_resident), "%).")
}