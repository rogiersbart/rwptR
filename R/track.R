#' @export
track <- function(particle,tstep,dis,bud,tmax,dsp=FALSE,stopIDs,mpmain=NULL)
{
  t <- particle[4]
  stop <- FALSE
  ijk <- xyztoijk(particle[1:3],dis)
  path <- data.frame(x=particle[1],y=particle[2],z=particle[3],t=particle[4],i=ijk[1],j=ijk[2],k=ijk[3])
  while(t <= tmax)
  {
    particle <- timeStep(particle,tstep,dis,bud,dsp,mpmain)
    ijkNew <- xyztoijk(particle[1:3],dis)
    if(particle[3]>dis$TOP[ijkNew[1],ijkNew[2]]) break
    path <- rbind(path,data.frame(x=particle[1],y=particle[2],z=particle[3],t=particle[4],i=ijkNew[1],j=ijkNew[2],k=ijkNew[3])) #if(!identical(ijkNew,ijk)) 
    if(ijktoid(ijkNew,dis)%in%stopIDs | any(is.na(ijkNew))) break
    ijk <- ijkNew
    t <- particle[4]
  }
  return(na.omit(path))
}