#' @import ggplot2
#' @export
#'
distribute <- function(i, name){

  i <- data.frame(i)
  g<- i %>% ggplot(aes(x = i)) +
    geom_histogram(colour = 'black', fill = '#48dbfb')+
    xlab(name)
  return(g)
}
