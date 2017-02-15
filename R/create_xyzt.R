#' Create xyzt particle set
#' 
#' @param x x
#' @param y y 
#' @param z z
#' @param t t
#' @param prj prj
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
