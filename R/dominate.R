#' @export
dominate <- function(i){
  ti <- table(i)
  dom <- ti[which(ti == max(ti))]
  return(list(unique = length(ti), name = names(dom), value = unname(dom), percent = unname(dom)/length(i)*100))
}
