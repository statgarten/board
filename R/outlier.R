#' @title Outlier of data
#' @description calculate outlier
#' @examples
#' outlier(iris[, "Sepal.Width"])
#'
#' @param i data
#'
#' @return named list
#' @seealso https://en.wikipedia.org/wiki/Interquartile_range (IQR)
#'
#' @importFrom dplyr pull
#' @importFrom tibble is_tibble
#' @export
#'
outlier <- function(i) {
  if (is_tibble(i)) {
    i <- dplyr::pull(i)
  }
  i <- i[which(!is.na(i))]
  qs <- quantile(i)
  q1 <- qs[2] # 25%
  q3 <- qs[4] # 75%
  over <- i[which(i > (q3 + IQR(i)))]
  under <- i[which(i < (q1 - IQR(i)))]
  lo <- length(over)
  lu <- length(under)

  ov <- ifelse(lo == 0, NA, min(over))
  uv <- ifelse(lu == 0, NA, max(under))
  return(list(lo = lo, ov = ov, lu = lu, uv = uv))
}
