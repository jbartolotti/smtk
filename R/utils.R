UTILS.range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
UTILS.range_plusminus1 <- function(x, ...){
  extent <- max(c( abs(max(x, ...)), abs(min(x, ...)) ))
  return(x/extent)
}
