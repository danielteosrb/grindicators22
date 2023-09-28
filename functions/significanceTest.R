# Function that replicates the significance testing previously carried out in Excel workbook

significanceTest <- function(p1, n1, p2, n2) {
  
  p1 <- p1 / 100
  p2 <- p2 / 100
  
  s1 <- (1 / n1) + (1 / n2)
  s2 <- p1 * n1
  s3 <- p2 * n2
  s4 <- s2 / n1
  s5 <- s3 / n2
  s6 <- s4 - s5
  s7 <- (s2 + s3) / (n1 + n2)
  s8 <- 1 - s7
  s9 <- sqrt(s7 * s8 * s1)
  z <- s6 / s9
  
  significance <- if (abs(z) > 1.96) {
    if (p1 > p2) {
      "significant increase"
    } else {
      "significant decrease"
    }
  }
  else {
    FALSE
  }
  
  return(significance)
  
}