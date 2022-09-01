#' @title data profiling
#' @description dimension, missing, cardinality, correlation, zero, uniform, unique for each variables
#' @examples
#' brief(iris)
#'
#' @param inputData data frame
#'
#' @return data frame with changed column
#' @import magrittr
#' @importFrom dplyr select
#' @importFrom tibble is_tibble
#' @export
#'

brief <- function(inputData) {
  nr <- nrow(inputData)
  nc <- ncol(inputData)
  cnt <- length(which(is.na(inputData)))
  # dataset description
  desc <- list(
    nrow = nr,
    ncol = nc,
    missingCellCount = cnt, # NA Count
    missingCellRatio = round(cnt / (nc * nr) * 100, 3) # NA Ratio
  )

  ## cardinality
  cards <- sapply(1:nc, function(i) {
    cardinality(inputData[, i])
  })

  ## correlation
  cors <- inputData %>%
    dplyr::select(names(Filter(is.numeric, inputData))) %>%
    cor(use = 'complete.obs') %>%
    round(3)

  ## missing
  miss <- sapply(1:nc, function(i) {
    m <- missing(inputData[, i])
    if(is.na(m$cnt)){
      return(NA)
    }
    return(paste0(m$cnt, " (", m$rat, "%)"))
  })

  ## zero
  zeros <- sapply(1:nc, function(i) {
    z <- zero(inputData[, i])
    if(is.na(z$cnt)){
      return(NA)
    }
    return(paste0(z$cnt, " (", z$rat, "%)"))
  })

  ## uniform
  unif <- sapply(1:nc, function(i) {
    isUniform(inputData[, i])
  })

  ## unique
  uniq <- sapply(1:nc, function(i) {
    isUnique(inputData[, i])
  })

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

# is it necessary?
# #' @export
#
cardinality <- function(i) {
  if (is_tibble(i)) {
    return(nrow(unique(i)))
  }
  return(length(unique(i)))
}

missing <- function(i) {
  cnt <- length(which(is.na(i)))
  if(is_tibble(i)){
    rat <- round(cnt / nrow(i) * 100, 3)
  }
  else{
    rat <- round(cnt / length(i) * 100, 3)
  }
  if(cnt == 0){
    return(list(cnt = NA, rat = NA))
  }
  return(list(cnt = cnt, rat = rat))
}

isUniform <- function(i) {
  return(length(unique(table(i))) == 1)
}

isUnique <- function(i) {
  return(length(which(duplicated(i))) == 0)
}

zero <- function(i) {
  cnt <- length(which(i == 0))
  if(is_tibble(i)){
    rat <- round(cnt / nrow(i) * 100, 3)
  }
  else{
    rat <- round(cnt / length(i) * 100, 3)
  }
  if(cnt == 0){
    return(list(cnt = NA, rat = NA))
  }
  return(list(cnt = cnt, rat = rat))
}
