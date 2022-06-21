#'
#' @export
#'
describe <- function(i){
  i <- i[which(!is.na(i))]
  count = length(i)

  m = mean(i)
  s = sd(i)

  qs <- quantile(i)
  q0 <- qs[1] # min
  q1 <- qs[2] # 25%
  q2 <- qs[3] # median
  q3 <- qs[4] # 75%
  q4 <- qs[5] # max
  return(list(count = count, m = m, s = s, q0 = q0, q1 = q1, q2 = q2, q3 = q3, q4 = q4))
}
