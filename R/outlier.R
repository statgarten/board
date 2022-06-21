#'
#' @export
#'
outlier <- function(i){
  qs <- quantile(i)
  q1 <- qs[2] # 25%
  q3 <- qs[4] # 75%
  over <- i[which(i > ( q3 + IQR(i) ))]
  under <- i[which(i < ( q1 - IQR(i) ))]
  lo <- length(over)
  lu <- length(under)

  ov <- ifelse(lo == 0, NA, min(over))
  uv <- ifelse(lu == 0, NA, max(over))
  return(list(lo = lo, ov = ov, lu = lu, uv = uv))
}
