#' Create xyzt particle set
#' 
#' @param x
#' @param y
#' @param z
#' @param t
#' @param prj
#' @return xyzt particle set
#' @export
create_xyzt <- function(x,
                        y,
                        z,
                        t,
                        prj = NULL) {
  xyzt <- data.frame(x = x, y = y, z = z, t = t)
  attr(xyzt, 'prj') <- prj
}
