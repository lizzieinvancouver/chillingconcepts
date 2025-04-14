# phenology functions, from PHENOFIT model

chuine <- function(x, params){
  y = params$a * (x - params$c) * (x - params$c) + params$b * (x - params$c)
  c <- 1 / (1 + exp(y))
  return(c)
}

thresholdinf <- function(x, params){
  c <- as.numeric(x < params$Vb)
  return(c)
}

sigmoid <- function(x, params){
  y = params$d * (x - params$e)
  f <- 1 / (1 + exp(y))
  return(max(0,f))
}
