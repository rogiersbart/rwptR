#' @export
convert_xyzt_to_darcy <- function(xyzt,
                                 dis,
                                 darcy,
                                 prj = NULL) {
  ijk <- xyztoijk(xyz,dis)
  id <- ijktoid(ijk,dis)
  loc <- xyztoloc(xyz,dis)
  left <- ifelse(ijk[2]==1,0,darcy$'FLOW RIGHT'$data[ijktoid(c(ijk[1],ijk[2]-1,ijk[3]),dis)])
  right <- darcy$'FLOW RIGHT'$data[id]
  front <- -darcy$'FLOW FRONT'$data[id]
  back <- -ifelse(ijk[1]==1,0,darcy$'FLOW FRONT'$data[ijktoid(c(ijk[1]-1,ijk[2],ijk[3]),dis)])
  upper <- -ifelse(ijk[3]==1,darcy$'RECHARGE'$data[id],darcy$'FLOW LOWER'$data[ijktoid(c(ijk[1],ijk[2],ijk[3]-1),dis)])
  lower <- -darcy$'FLOW LOWER'$data[id]
  # check direction!!!
  return(c(left+loc[1]*(right-left),front+loc[2]*(back-front),lower+loc[3]*(upper-lower)))
}
