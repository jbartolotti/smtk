UTILS.range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
UTILS.range_plusminus1 <- function(x, ...){
  extent <- max(c( abs(max(x, ...)), abs(min(x, ...)) ))
  return(x/extent)
}

UTILS.PointToCurve <- function(pointx, pointy, curvexs, curveys){
  sqer <- unlist(lapply(1:dim(curvedf)[1], function(n){UTILS.SqEr(pointx, pointy, curvexs[n], curveys[n])}))
  return(sqer)

}
UTILS.SqEr <- function(x,y,x2,y2){
  return((x-x2)^2 + (y-y2)^2)
}

