localSig <- significanceTest(p1 = dataNew$f21b_neighbourhood,
                             n1 = dataNew$INFLLOCL_n,
                             p2 = dataOld$f21b_neighbourhood,
                             n2 = dataOld$INFLLOCL_n)

NIsig <- significanceTest(p1 = dataNew$f21b_NI,
                          n1 = dataNew$INFLNI_n,
                          p2 = dataOld$f21b_NI,
                          n2 = dataOld$INFLNI_n)

sigStatement <-
  # Both significant decrease
  if (localSig == "significant decrease" & NIsig == "significant decrease") {
    paste0("Since ", NILTyear - 1, " there has been a significant decrease in the proportion of adults who feel they have an influence on decisions in their neighbourhood (",
           NILTyear, ": ", dataNew$f21b_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21b_neighbourhood, "%) and Northern Ireland decisions (", NILTyear, ": ", dataNew$f21b_NI, "%; ", NILTyear - 1, ": ", dataOld$f21b_NI, "%).")
    # Both significant increase
  } else if (localSig == "significant increase" & NIsig == "significant increase") {
    paste0("Since ", NILTyear - 1, " there has been a significant increase in the proportion of adults who feel they have an influence on decisions in their neighbourhood (",
           NILTyear, ": ", dataNew$f21b_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21b_neighbourhood, "%) and Northern Ireland decisions (", NILTyear, ": ", dataNew$f21b_NI, "%; ", NILTyear - 1, ": ", dataOld$f21b_NI, "%).")
    # Neither significant
  } else if (localSig == FALSE & NIsig == FALSE) {
    paste0("Since ", NILTyear - 1, " there has been no significant change in the proportion of adults who feel they have an influence on decisions in their neighbourhood (", abs(dataNew$f21b_neighbourhood), "%) or on Northern Ireland decisions (", dataNew$f21b_NI, "%).")
    # Local significant and NI not significant
  } else if (localSig != FALSE & NIsig == FALSE) {
    paste0("Since ", NILTyear - 1, " there has been a ", localSig, " in the proportion of adults who feel they have an influence on decisions in their neighbourhood (",
           NILTyear, ": ", dataNew$f21b_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21b_neighbourhood, "%). However, there is no significant change in the proportion of those who feel they have an influence on Northern Ireland decisions.")
    # Local not significant and NI significant
  } else if (localSig == FALSE & NIsig != FALSE) {
    paste0("Since ", NILTyear - 1, " there has been no significant change in the proportion of thoese who feel they have an influence on decisions in their neighbourhood. However, there has been a ", NIsig, " in the proportion of adults who feel they have an influence on Northern Ireland decisions (",
           NILTyear, ": ", dataNew$f21b_NI, "%; ", NILTyear - 1, ": ", dataOld$f21b_NI, "%).")
    # Both significant but changes in opposite directions
  } else if (localSig != NIsig & localSig != FALSE & NIsig != FALSE) {
    paste0("Since ", NILTyear - 1, " there has been a ", localSig, " in the proportion of adults who feel they have an influence of decisions in their neighbourhood (",
           NILTyear, ": ", dataNew$f21b_neighbourhood, "%; ", NILTyear - 1, ": ", dataOld$f21b_neighbourhood, "%) however there was a ", NIsig, " in the proportion of adults who felt this way about Northern Ireland decisions (",
           NILTyear, ": ", dataNew$f21b_NI, "%; ", NILTyear - 1, ": ", dataOld$f21b_NI, "%).")
  }

protAgree <- (fig22$strongly + fig22$agree)[fig22$community == "Protestant\ncommunities"]
cathAgree <- 71
minorityAgree <- 67