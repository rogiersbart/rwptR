#' @export
xyztoijk <- function(xyz,dis)
{
  i <- dis$NROW - which(xyz[2] <= cumsum(rev(dis$DELC)))[1] + 1
  j <- which(xyz[1] <= cumsum(dis$DELR))[1]
  k <- which(xyz[3] >= dis$BOTM[i,j,])[1]
  return(c(i,j,k))
}