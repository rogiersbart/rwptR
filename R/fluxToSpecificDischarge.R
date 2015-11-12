#' @export
fluxToSpecificDischarge <- function(bud,dis,hed)
{
  lowerArea <- apply(expand.grid(dis$DELC,dis$DELR),1,prod)
  thickness <- dis$BOTM*NA
  thickness[,,1] <- array(hed[1:(dis$NROW*dis$NCOL)],dim=c(dis$NROW,dis$NCOL)) - dis$BOTM[,,1]
  thickness[,,2:dis$NLAY] <- dis$BOTM[,,1:(dis$NLAY-1)]-dis$BOTM[,,2:dis$NLAY]
  frontArea <- apply(data.frame(c(thickness),dis$DELC),1,prod)
  rightArea <- apply(data.frame(c(thickness),dis$DELR),1,prod)
  bud$"RECHARGE        "$data <- bud$"RECHARGE        "$data/lowerArea
  bud$"FLOW LOWER FACE "$data <- bud$"FLOW LOWER FACE "$data/lowerArea
  bud$"FLOW FRONT FACE "$data <- bud$"FLOW FRONT FACE "$data/frontArea
  bud$"FLOW RIGHT FACE "$data <- bud$"FLOW RIGHT FACE "$data/rightArea
  return(bud[c(2,3,4,8)])
}