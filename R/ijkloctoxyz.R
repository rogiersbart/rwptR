#' @export
ijkloctoxyz <- function(ijkloc,dis)
{
  x <- cumsum(dis$DELR)[ijkloc[2]]-dis$DELR[ijkloc[2]]*(1-ijkloc[4])
  y <- cumsum(dis$DELC)[dis$NROW]-(cumsum(dis$DELC)[ijkloc[1]]-dis$DELC[ijkloc[1]]*(ijkloc[5]))
  z <- ifelse(ijkloc[3]==1,
              dis$BOTM[ijkloc[1],ijkloc[2],1]+ ijkloc[6]* (dis$TOP[ijkloc[1],ijkloc[2]]-dis$BOTM[ijkloc[1],ijkloc[2],1]),
              dis$BOTM[ijkloc[1],ijkloc[2],ijkloc[3]]+ ijkloc[6]*(dis$BOTM[ijkloc[1],ijkloc[2],ijkloc[3]-1]-dis$BOTM[ijkloc[1],ijkloc[2],ijkloc[3]]))
  return(c(x,y,z))
}