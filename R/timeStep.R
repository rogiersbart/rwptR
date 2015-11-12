#' @export
timeStep <- function(xyzt,tstep,dis,bud,dsp=FALSE,mpmain=NULL)
{
  xyz <- xyzt[1:3]
  if(!dsp)
  {
    xyz <- xyz + xyztov(xyz,dis,bud)*tstep
  } else {
    dY <- deltaY(xyzt,tstep,dis,bud,mpmain,Dm=0)
    vB <- xyztov(xyz+dY,dis,bud)
    xyz <- xyz + xyztov(xyz,dis,bud)*tstep + deltaY(xyzt+c(dY,0),tstep,dis,bud,mpmain,Dm=0)
  }
  return(as.numeric(c(xyz,xyzt[4] + tstep)))
}