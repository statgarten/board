#' @title summary statistics from data with character value
#' @description count, mean, sd, quantile values.
#' @examples
#' describe(iris[,"Sepal.Width"])
#' describe(iris$Sepal.Length) # this will be work too.
#'
#' @param i data
#'
#' @return named list
#' @seealso base `summary` function
#'
#' @importFrom tibble is_tibble
#' @importFrom dplyr pull
#' @export
#'
describe <- function(i){

  i <- na.omit(i)
  if(is_tibble(i)){
    i <- dplyr::pull(i)
  }

  count <- length(i)

  m = round(mean(i), 3)
  s = round(sd(i), 3)

  qs <- quantile(i, na.rm= TRUE)
  q0 <- qs[1] # min
  q1 <- qs[2] # 25%
  q2 <- qs[3] # median
  q3 <- qs[4] # 75%
  q4 <- qs[5] # max
  return(list(count = count, m = m, s = s, q0 = q0, q1 = q1, q2 = q2, q3 = q3, q4 = q4))
}
