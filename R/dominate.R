#' @title find most common value in data
#' @description most common value and count & percent, among howmuch values
#' @examples
#' dominate(iris[,"Sepal.Length"])
#' scissor::binarize(iris,'Species', 'In', 'c("virginica", "setosa")')
#'
#' @param i data
#'
#' @return named list
#' @import magrittr
#' @export
#'
dominate <- function(i){
  ti <- table(i)
  dom <- ti[which(ti == max(ti))]
  return(list(unique = length(ti), name = names(dom), value = unname(dom), percent = unname(dom)/length(i)*100))
}
