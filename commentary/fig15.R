paraPct <- round2(fig15a$num[fig15a$option == "Paramilitary"]/sum(fig15a$num, na.rm = TRUE) * 100)
intimTot <- sum(fig15a$num, na.rm = TRUE)

niheYearLast <- paste0("20", as.numeric(substr(niheYear, 3, 4)) - 1, "/", as.numeric(substr(niheYear, 6, 7)) - 1)

fig15b_compare <- fig15b  %>%
  filter(year %in% c(niheYearLast, niheYear))  %>%
  t() %>% as.data.frame() 
fig15b_compare <- cbind(rownames(fig15b_compare), fig15b_compare)
names(fig15b_compare) <- fig15b_compare[1, ]
fig15b_compare <- fig15b_compare[-1, ]
rownames(fig15b_compare) <- 1:nrow(fig15b_compare)
fig15b_compare$change <- round2((as.numeric(fig15b_compare[[2]]) - as.numeric(fig15b_compare[[3]])) / as.numeric(fig15b_compare[[2]]) * 100)

biggestChange <- max(abs(fig15b_compare$change[fig15b_compare$change != 100]), na.rm = TRUE)
biggestChangeReason <- fig15b_compare$year[abs(fig15b_compare$change) == biggestChange & !is.na(fig15b_compare$change)]

if (biggestChangeReason == "other") {
  biggestChangeReason <- "other forms of intimidation (i.e. anti-social behaviour, disability, and sexual orientation)"
} else {
  biggestChangeReason <- paste0(tolower(biggestChangeReason), " intimidation")
}