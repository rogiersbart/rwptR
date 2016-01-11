#' Track a particle set
#' @export
track <- function(xyzt,
                  tstep,
                  nsteps,
                  dis,
                  bas,
                  hed,
                  bud = NULL,
                  darcy = convert_bud_to_darcy(bud, dis = dis),
                  dsp = NULL,
                  terminals,
                  mpmain = NULL,
                  parallel = FALSE,
                  cores = parallel::detectCores(),
                  type = 'labolle') {
  dis <- convert_dis_to_saturated_dis(dis = dis, hed = hed) # solve time-dependent nature of satdis!
  path <- xyzt
  tstep_nr <- 1
  if(parallel) {
    # insert parallel version of below code here
  } else {
    while(tstep_nr <= nsteps) { # and there are still particles moving
      xyzt <- timestep(xyzt = xyzt, tstep = tstep, dis = dis, darcy = darcy, dsp = dsp, mpmain = mpmain)
      path <- rbind(path, xyzt)
      ijk <- convert_xyz_to_grid(x = xyzt[,1], y = xyzt[,2], z = xyzt[,3], dis = dis)[,c('i','j','k')]
      id <- convert_ijk_to_id(i = ijk[,1], j = ijk[,2], k = ijk[,3], dis = dis)
      active <- which(bas$ibound[id] == 1 & !id %in% terminals)      
      xyzt <- xyzt[active,]
      tstep_nr <- tstep_nr + 1
    }
  }
  return(na.omit(path))
}
