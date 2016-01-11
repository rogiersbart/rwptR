#' @import RMODFLOW
#' @export
read_mpmain <- function(file, dis) {
  mpmain.lines <- scan(file, what=character(), sep='\n')
  mpmain <- NULL
  
  # Data set 0
  mpmain.lines <- remove_comments_from_lines(mpmain.lines)
  
  # Data set 1
  mpmain$dataSet1 <- remove_empty_strings(strsplit(mpmain.lines[1],' '))
  mpmain.lines <- mpmain.lines[-1]
  
  # Data set 2
  mpmain$dataSet2 <- remove_empty_strings(strsplit(mpmain.lines[1],' '))
  mpmain.lines <- mpmain.lines[-1]
  
  # Data set 3
  mpmain$dataSet3 <- remove_empty_strings(strsplit(mpmain.lines[1],' '))
  mpmain.lines <- mpmain.lines[-1]
  
  # Data set 4
  dataSet4 <- int_get_modflow_array(mpmain.lines,dis$NROW,dis$NCOL,dis$NLAY)
  mpmain.lines <- dataSet4$remaining.lines
  mpmain$IBOUND <- dataSet4$mfarray
  class(mpmain$IBOUND) <- 'modflow_3d_array'
  rm(dataSet4)
  
  # Data set 5
  dataSet5 <- int_get_modflow_array(mpmain.lines,dis$NROW,dis$NCOL,dis$NLAY)
  mpmain.lines <- dataSet5$remaining.lines
  mpmain$POROSITY <- dataSet5$mfarray
  class(mpmain$POROSITY) <- 'modflow_3d_array'
  rm(dataSet5)
  
  # Data set 6
  dataSet6 <- int_get_modflow_array(mpmain.lines,dis$NROW,dis$NCOL,dis$NLAY)
  mpmain.lines <- dataSet6$remaining.lines
  mpmain$DISPL <- dataSet6$mfarray
  class(mpmain$DISPL) <- 'modflow_3d_array'
  rm(dataSet6)
  
  # Data set 7
  dataSet7 <- int_get_modflow_array(mpmain.lines,dis$NROW,dis$NCOL,dis$NLAY)
  mpmain.lines <- dataSet7$remaining.lines
  mpmain$DISPT <- dataSet7$mfarray
  class(mpmain$DISPT) <- 'modflow_3d_array'
  rm(dataSet7)
  
  # Data set 8
  dataSet8 <- int_get_modflow_array(mpmain.lines,dis$NROW,dis$NCOL,dis$NLAY)
  mpmain.lines <- dataSet8$remaining.lines
  mpmain$DISPV <- dataSet8$mfarray
  class(mpmain$DISPV) <- 'modflow_3d_array'
  rm(dataSet8)
  
  return(mpmain)
}
