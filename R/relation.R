#' @import ggplot2
#' @import magrittr
#' @importFrom tibble is_tibble
#' @export
#'
relation <- function(var1, var2, xlabel = NULL, ylabel = NULL){
  data <- data.frame(cbind(var1, var2))
  colnames(data) <- c("X", "Y")
  if(is_tibble(var1)){
    xlabel = ifelse(is.null(xlabel), names(var1), xlabel)
  }
  if(is_tibble(var2)){
    ylabel = ifelse(is.null(ylabel), names(var2), ylabel)
  }
  xlabel = ifelse(is.null(xlabel), "X", xlabel)
  ylabel = ifelse(is.null(ylabel), "Y", ylabel)

  g <- data %>%
    ggplot(aes(x = X, y = Y)) +
    geom_point() +
    xlab(xlabel) +
    ylab(ylabel)
  return(g)
}
