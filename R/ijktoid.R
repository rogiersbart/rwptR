#' @export
ijktoid <- function(ijk,dis)
{
  return(ijk[1]+(ijk[2]-1)*dis$NROW+(ijk[3]-1)*dis$NCOL*dis$NROW)
}