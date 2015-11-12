#' @export
ijktoxyz <- function(ijk,dis)
{
  x <- (cumsum(dis$DELR)-dis$DELR/2)[ijk[2]]
  y <- cumsum(dis$DELC)[dis$NROW]-(cumsum(dis$DELC)-dis$DELC/2)[ijk[1]]
  z <- ifelse(ijk[3]==1,mean(c(dis$TOP[ijk[1],ijk[2]],dis$BOTM[ijk[1],ijk[2],1])),mean(c(dis$BOTM[ijk[1],ijk[2],ijk[3]-1],dis$BOTM[ijk[1],ijk[2],ijk[3]])))
  return(c(x,y,z))
}