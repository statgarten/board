#' @title see data's distribution.
#' @description build histogram from character input
#' @examples
#' distribute(iris[, "Sepal.Width"])
#' distribute(iris$Sepal.Length) # this will be work too.
#'
#' @param i vector of numeric data
#' @param xlabel label for x axis (default is `value`), y axis set as 'count'.
#'
#' @return histogram built with ggplot
#' @seealso `geom_histogram`
#'
#' @import ggplot2
#' @import magrittr
#' @importFrom tibble is_tibble as_tibble
#' @export
#'
distribute <- function(i, xlabel = NULL) {
  # i must numeric
  if (is_tibble(i)) {
    xlabel <- names(i)
    names(i) <- "X"
  } else {
    i <- as_tibble(i)
    names(i) <- "X"
    if (is.null(xlabel)) {
      xlabel <- "value"
    }
  }

  g <- i %>%
    ggplot(aes(x = X)) +
    geom_histogram(colour = "black", fill = "#48dbfb", bins = 30) +
    xlab(xlabel)

  return(g)
}
