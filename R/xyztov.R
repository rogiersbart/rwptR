#' @export
xyztov <- function(xyz,dis,bud)
{
  ijk <- xyztoijk(xyz,dis)
  id <- ijktoid(ijk,dis)
  loc <- xyztoloc(xyz,dis)
  left <- ifelse(ijk[2]==1,0,bud$'FLOW RIGHT'$data[ijktoid(c(ijk[1],ijk[2]-1,ijk[3]),dis)])
  right <- bud$'FLOW RIGHT'$data[id]
  front <- -bud$'FLOW FRONT'$data[id]
  back <- -ifelse(ijk[1]==1,0,bud$'FLOW FRONT'$data[ijktoid(c(ijk[1]-1,ijk[2],ijk[3]),dis)])
  upper <- -ifelse(ijk[3]==1,bud$'RECHARGE'$data[id],bud$'FLOW LOWER'$data[ijktoid(c(ijk[1],ijk[2],ijk[3]-1),dis)])
  lower <- -bud$'FLOW LOWER'$data[id]
  # check direction!!!
  return(c(left+loc[1]*(right-left),front+loc[2]*(back-front),lower+loc[3]*(upper-lower)))
}