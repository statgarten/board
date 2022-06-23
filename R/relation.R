#' @import ggplot2
#' @import magrittr
#' @export
#'
relation <- function(var1, var2){
  data <- data.frame(cbind(var1, var2))
  g <- data %>% ggplot(aes(x = var1, y = var2)) + geom_point()
  return(g)
}
