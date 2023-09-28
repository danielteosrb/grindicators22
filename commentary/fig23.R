respected <- round2(dataNew$f23_compare)

approxRespected <- if (respected %in% 45:48) {
  "Just under a half"
} else if (respected %in% c(49,51)) {
  "Around a half"
} else if (respected == 50) {
  "Half"
} else if (respected %in% 52:59) {
  "Over a half"
} else if (respected == 60) {
  "Three-fifths"
} else if (respected %in% 61:63) {
  "Over three-fifths"
} else if (respected %in% 64:65) {
  "Approaching two-thirds"
} else if (respected %in% 66:67) {
  "Around two-thirds"
} else if (respected %in% 68:70) {
  "Over two-thirds"
} else if (respected %in% 71:74) {
  "Approaching three-quarters"
} else if (respected == 75) {
  "Three-quarters"
} else if (respected %in% 76:79) {
  "Over three-quarters"
}

cathRespected <- round2((fig23$strongly + fig23$agree)[fig23$community == "Catholic"])
protRespected <- round2((fig23$strongly + fig23$agree)[fig23$community == "Protestant"])
noRespected <- round2((fig23$strongly + fig23$agree)[fig23$community == "No religion"])


respectSig <- significanceTest(p1 = dataNew$f23_compare,
                               n1 = dataNew$CULTRESP_n,
                               p2 = dataOld$f23_compare,
                               n2 = dataOld$CULTRESP_n)

f23para1 <- if (respectSig == FALSE) {
  paste0("Since ", NILTyear - 1,
         ", there has been no significant change in the proportion of those who feel their own cultural identity is respected by society.")
} else if (respectSig != FALSE ) {
  paste0("Since ", NILTyear - 1,
         ", there has been ", respectSig,
         " in the proportion of those who feel their own cultural identity is respected by society (",
         NILTyear, ": ", dataNew$f23_compare, "%; ", NILTyear - 1, ": ", dataOld$f23_compare, "%).")
}