#' @import magrittr
#' @export
#'

brief <- function(inputData, exc = NULL){

  # dataset description
  nr <- nrow(inputData)
  nc <- ncol(inputData)
  cnt <- length(which(is.na(inputData)))
  rat <- cnt / (nc * nr) * 100

  desc <- list(nrow = nr, ncol = nc, missingCellCount = cnt, missingCellRatio = rat)

  ## cardinality
  cards <- sapply(1:nc, function(i){cardinality(inputData[,i])})

  ## correlation
  if(is.null(exc)){
    cors <- cor(inputData)
  }
  else{
    cors <- cor(inputData[,-exc])
  }

  ## missing
  miss <- c()
  miss <- sapply(1:nc, function(i){
    m <- missing(inputData[,i])
    return(paste0(m$cnt, ' (', m$rat, '%)'))
  })

  ## zero
  zeros <- sapply(1:nc, function(i){
    z <- zero(inputData[,i])
    return(paste0(z$cnt, ' (', z$rat, '%)'))
  })

  ## uniform
  unif <- sapply(1:nc, function(i){isUniform(inputData[,i])})

  ## unique
  uniq <- sapply(1:nc, function(i){isUnique(inputData[,i])})

  return(list(
    names = colnames(inputData),
    desc = desc,
    cards = cards,
    cors = cors,
    miss = miss,
    zeros = zeros,
    unif = unif,
    uniq = uniq
  ))

}

cardinality <- function(i){
  length(unique(i))
}

missing <- function(i){
  cnt <- length(which(is.na(i)))
  rat <- cnt / length(i) * 100

  return(list(cnt = cnt, rat = rat))
}

isUniform <- function(i){
  length(unique(table(i))) == 1
}

isUnique <- function(i){
  length(which(duplicated(i))) == 0
}

zero <- function(i){
  cnt <- length(which(i==0))
  rat <- cnt / length(i) * 100
  return(list(cnt = cnt, rat = rat))
}

