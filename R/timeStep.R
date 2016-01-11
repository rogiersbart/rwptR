#' Timestepping for a particle set
#'
#' @export
timestep <- function(xyzt,
                     tstep,
                     dis,
                     darcy,
                     dsp = NULL,
                     mpmain = NULL) {
  xyz <- xyzt[1:3]
  if(is.null(dsp)) {
    xyz <- xyz + convert_xyzt_to_darcy(xyzt,dis,darcy)*tstep
  } else {
    dY <- deltaY(xyzt,tstep,dis,darcy,mpmain,Dm=0)
    vB <- convert_xyzt_to_darcy(xyzt+dY,dis,darcy)
    xyz <- xyz + convert_xyzt_to_darcy(xyz,dis,darcy)*tstep + deltaY(xyzt+c(dY,0),tstep,dis,darcy,mpmain,Dm=0)
  }
  return(as.numeric(c(xyz,xyzt[4] + tstep)))
}
