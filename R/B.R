#' @export
B <- function(v,alpha,Dm)
{
  v1 <- v[1];v2 <- v[2];v3 <- v[3]
  v <- sqrt(sum(v^2))
  al <- alpha[1];ath <- alpha[2];atv <- alpha[3]
  mat <- matrix(c(
    v1/v*sqrt(2*(al*v+Dm)),-(v1*v3*sqrt(2*(atv*v+Dm)))/(v*sqrt(v1^2+v2^2)),-(v2)/(sqrt(v1^2+v2^2))*sqrt(2*((ath*(v1^2+v2^2)+atv*v3^2)/(v)+Dm)),
    v2/v*sqrt(2*(al*v+Dm)),-(v2*v3*sqrt(2*(atv*v+Dm)))/(v*sqrt(v1^2+v2^2)),-(v1)/(sqrt(v1^2+v2^2))*sqrt(2*((ath*(v1^2+v2^2)+atv*v3^2)/(v)+Dm)),
    v3/v*sqrt(2*(al*v+Dm)),sqrt(2*(v1^2+v2^2)/(v^2)*(atv*v+Dm)),0
  ),nrow=3,ncol=3,byrow=TRUE)
  return(mat)
}