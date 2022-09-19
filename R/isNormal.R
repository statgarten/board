#' @export
isNormal <- function(i) {
  shapiro.test(i)$p.value > 0.05
}
