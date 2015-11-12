#' @export
xyztoid <- function(xyz,dis)
{
  ijktoid(xyztoijk(xyz,dis),dis)
}