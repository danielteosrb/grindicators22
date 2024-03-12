pct <- round2(fig13$pct[fig13$year == intYear])
diff <- round2(abs((fig13$pct - lag(fig13$pct))[fig13$year == intYear]))

intYearLast <- paste0("20", as.numeric(substr(intYear, 3, 4)) - 1, "/", substr(intYear, 3, 4))

f13Sig <- significanceTest(p1 = pct,
                           n1 = fig13_n1 / pct * 100,
                           p2 = fig13$pct[fig13$year == intYearLast],
                           n2 = fig13_n2 / fig13$pct[fig13$year == intYearLast] * 100)

f13para1 <- if (f13Sig != FALSE) {
  paste0("In ", intYear,
         ", there has been a ", f13Sig, " of ", diff,
         " percentage points compared to the proportion of first preference applications to post-primary integrated schools that did not result in admission to that particular school in ",
         intYearLast, " (", intYear, ": ", round2(fig13$pct[fig13$year == intYear]), "%; ", intYearLast, ": ", round2(fig13$pct[fig13$year == intYearLast]), "%).")
} else {
  paste0("In ", intYear,
         ", there has been no significant change in the proportion of first preference applications to post-primary integrated schools that did not result in admission to that particular school in ",
         intYearLast, ".")
}

# This line is hardcoded because we don't know the N for the 2013/14 figure so can't do a significance test. It's unlikely to ever be wrong but it would be worth reviewing

f13para2 <- paste0("Since 2013/14, there has been a significant increase in the percentage of first preference applications to post-primary integrated schools that do not result in admissions to that particular school – an increase of ",
                   round2(abs(fig13$pct[fig13 == intYear] - fig13$pct[fig13$year == "2013/14"]))," percentage points (", intYear, ": ", round2(fig13$pct[fig13$year == intYear]), "%; 2013/14: ", round2(fig13$pct[fig13$year == "2013/14"]), "%). This represents an increasing over-subscription to popular post-primary integrated schools.")