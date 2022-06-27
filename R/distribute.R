#' @import ggplot2
#' @import magrittr
#' @importFrom tibble is_tibble
#' @export
#'
distribute <- function(i, xlabel = NULL){

  # i must numeric

  if(is_tibble(i)){
    xlabel = names(i)
    names(i) <- 'X'
  }
  else{
    i <- as_tibble(i)
    names(i) <- 'X'
    if(is.null(xlabel)){xlabel = 'value'}
  }

  g <- i %>%
    ggplot(aes(x = X)) +
    geom_histogram(colour = 'black', fill = '#48dbfb', bins = 30)+
    xlab(xlabel)

  return(g)
}
