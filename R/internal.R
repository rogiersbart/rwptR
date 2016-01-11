# Internal functions ####

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

#' @export
deltaY <- function(particle,tstep,dis,bud,mpmain,Dm=0)
{
  v <- convert_xyzt_to_darcy(particle[1:3],dis,bud)
  ijk <- xyztoijk(particle[1:3],dis)
  alpha <- c(mpmain$DISPL[ijk[1],ijk[2],ijk[3]],mpmain$DISPT[ijk[1],ijk[2],ijk[3]],mpmain$DISPV[ijk[1],ijk[2],ijk[3]])
  return(c(B(v,alpha,Dm) %*% rnorm(3) * sqrt(tstep)))
}

# To be removed from package ####

# Remove beginning comment lines.
remove.comments.from.lines <- function(lines)
{
    i <- 0
    while(i==0) ifelse(substr(lines[1], 1,1)=='#', lines <- lines[-1], i<-1)   
    #if(i==1) cat('Comments removed\n')
    return(lines)
}
# Remove comments at the end of a line
remove.comments.end.of.line <- function(line)
{
  if(grepl('#',line)) return(substr(line,1,regexpr('#',line)-1))
  else return(line)
}

# Remove empty strings from string array
remove.empty.strings <- function(stringArray)
{
    newStringArray <- NULL
    for(i in 1:length(stringArray)) 
    {
        #print(stringArray[i])
        if(stringArray[i] != '') {newStringArray <- c(newStringArray, stringArray[i])}
    }
    return(newStringArray)
}

# Split line into characters
split.line.char <- function(string)
{
  split.line <- as.character(strsplit(remove.comments.end.of.line(string),' |\t')[[1]])
  split.line <- remove.empty.strings(split.line)
  return(split.line)
}

# Split line into numbers
split.line.num <- function(string)
{
  split.line <- as.vector(na.omit(as.numeric(strsplit(remove.comments.end.of.line(string),' |\t')[[1]])))
  return(split.line)
}
# Mirror matrix
mirror.matrix <- function(mat, mirror.type='both')
{
  nr <- length(mat[,1])
  nc <- length(mat[1,])
  matmirror <- matrix(nrow=nr, ncol=nc)
  
  if(mirror.type=='both')
  {
    matmirror2 <- matrix(nrow=nr,ncol=nc)
    for(j in 1:nc) matmirror2[,j] <- mat[,nc-j+1]
    for(i in 1:nr) matmirror[i,] <- matmirror2[nr-i+1,]
  }
  if(mirror.type=='horizontal')
  {
    for(j in 1:nc) matmirror[,j] <- mat[,nc-j+1]
  }
  if(mirror.type=='vertical')
  {
    for(i in 1:nr) matmirror[i,] <- mat[(nr-i+1),]
  }
  if(mirror.type!='both' & mirror.type!='horizontal' & mirror.type!='vertical')
  {
    cat('Error: wrong mirror type specified.')
  }
  return(matmirror)
}
# RMSE
rmse <- function(observations, predictions)
{
  return(sqrt(mean((observations-predictions)^2)))
}
strsplit0 <- function(...)
{
  output <- strsplit(...)
  for(i in 1:length(output))
  {
    if(output[[i]][1]=='')
    {
      output[[i]] <- output[[i]][-1]
    }
  }
  return(output)
}
#' @export
xyztoijk <- function(xyz,dis)
{
  i <- dis$NROW - which(xyz[2] <= cumsum(rev(dis$DELC)))[1] + 1
  j <- which(xyz[1] <= cumsum(dis$DELR))[1]
  k <- which(xyz[3] >= dis$BOTM[i,j,])[1]
  return(c(i,j,k))
}
#' @export
xyztoid <- function(xyz,dis)
{
  ijktoid(xyztoijk(xyz,dis),dis)
}

#' Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param object MODFLOW input file text object, starting with the free-format control record
#' @return A list containing the array and the remaining text of the MODFLOW input file
get.mfarray <- function(mfarray.lines,NROW,NCOL,NLAY)
{
  # Initialize array object
  mfarray <- array(dim=c(NROW,NCOL,NLAY))
  
  # Read array according to format type if there is anything to be read
  if(prod(dim(mfarray))!=0)
  {
    for(k in 1:NLAY) 
    { 
      # Read in first row with format code
      # If constant and NLAY==1, return constant
      # If constant and NLAY!=1, fill layer with constant (is part of mf3darray!?)
      if(remove.empty.strings(strsplit(mfarray.lines[1],' ')[[1]])[1]=='CONSTANT') 
      {
        if(NLAY==1)
        {
          mfarray <- as.numeric(remove.empty.strings(strsplit(mfarray.lines[1],' |\t')[[1]])[2])
          mfarray.lines <- mfarray.lines[-1]
          return(list(mfarray=mfarray,remaining.lines=mfarray.lines))
        } else {
          mfarray[,,k] <- matrix(as.numeric(remove.empty.strings(strsplit(mfarray.lines[1],' |\t')[[1]])[2]),nrow=NROW,ncol=NCOL)
          mfarray.lines <- mfarray.lines[-1]
        }
      }
      else if(strsplit(mfarray.lines[1],' ')[[1]][1]=='INTERNAL')
      {
        mfarray.lines <- mfarray.lines[-1] 
        nPerLine <- length(as.numeric(remove.empty.strings(strsplit(mfarray.lines[1],' |\t')[[1]])))
        nLines <- (NCOL %/% nPerLine + ifelse((NCOL %% nPerLine)==0, 0, 1))*NROW
        mfarray[,,k] <- matrix(as.numeric(strsplit0(paste(mfarray.lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]]),nrow=NROW,ncol=NCOL,byrow=TRUE)
        mfarray.lines <- mfarray.lines[-c(1:nLines)]
      }
      else if(strsplit(mfarray.lines[1],' ')[[1]][1]=='EXTERNAL')
      {
        stop('Reading EXTERNAL arrays is not implemented yet...')
      }   
      else if(strsplit(mfarray.lines[1],' ')[[1]][1]=='OPEN/CLOSE')
      {
        stop('Reading OPEN/CLOSE arrays is not implemented yet...')
      }   
    }
  }
  
  # Set class of object (2darray; 3darray)
  if(NLAY==1){mfarray <- as.matrix(mfarray[,,1]); class(mfarray) <- 'mf2darray'}
  if(NLAY!=1) class(mfarray) <- 'mf3darray'
  
  # Return output of reading function 
  return(list(mfarray=mfarray,remaining.lines=mfarray.lines))
}
#' @export
ijktoid <- function(ijk,dis)
{
  return(ijk[1]+(ijk[2]-1)*dis$NROW+(ijk[3]-1)*dis$NCOL*dis$NROW)
}
#' @export
ijktoxyz <- function(ijk,dis)
{
  x <- (cumsum(dis$DELR)-dis$DELR/2)[ijk[2]]
  y <- cumsum(dis$DELC)[dis$NROW]-(cumsum(dis$DELC)-dis$DELC/2)[ijk[1]]
  z <- ifelse(ijk[3]==1,mean(c(dis$TOP[ijk[1],ijk[2]],dis$BOTM[ijk[1],ijk[2],1])),mean(c(dis$BOTM[ijk[1],ijk[2],ijk[3]-1],dis$BOTM[ijk[1],ijk[2],ijk[3]])))
  return(c(x,y,z))
}
