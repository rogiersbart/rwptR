#' @export
xyztoloc <- function(xyz,dis)
{
  ijk <- xyztoijk(xyz,dis)
  loc1 <- 1-((cumsum(dis$DELR)[ijk[2]]-xyz[1])/dis$DELR[ijk[2]])
  loc2 <- 1-((cumsum(rev(dis$DELC))[dis$NROW-ijk[1]+1]-xyz[2])/dis$DELC[ijk[1]])
  if(ijk[3]==1)
  {
    loc3 <- 1-(dis$TOP[ijk[1],ijk[2]]-xyz[3])/(dis$TOP[ijk[1],ijk[2]]-dis$BOTM[ijk[1],ijk[2],1])
  } else {
    loc3 <- 1-(dis$BOTM[ijk[1],ijk[2],ijk[3]-1]-xyz[3])/(dis$BOTM[ijk[1],ijk[2],ijk[3]-1]-dis$BOTM[ijk[1],ijk[2],ijk[3]])
  }
  return(c(loc1,loc2,loc3))
}