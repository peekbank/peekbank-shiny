
ci.95 <- function(x) {
  n <- sum(!is.na(x))
  sem <- sd(x, na.rm = TRUE) / sqrt(n)
  return(c(qnorm(0.025)*sem, qnorm(0.975)*sem))
}
