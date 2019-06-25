#' Create transformd interaction list
#'
#' @param X Matrix with n rows species and n columns traits. First column = species
#' @param Y Matrix with n rows species and n columns traits. First column = species
#' @param Z matrix with X*Y species. Interactions can be binaries or probabilities
#'
#'



createInteractionList <- function(individualsX = NULL, individualsY = NULL, interactionsZ = NULL){
  out <- list()

  if((nrow(individualsX) * nrow(individualsY)) > (nrow(interactionsZ) * ncol(interactionsZ))) stop("Dims of interaction matrix does not match input matrices")


  if(is.matrix(individualsX)){
    if(!all.equal(rownames(individualsX),rownames(interactionsZ))) stop("Pls provide the right rownames for Interaction matrix")
    if(is.null(colnames(individualsX))) stop("Pls provide colnames")
    X_id <- as.data.frame(rownames(individualsX))
    individualsX <- cbind(X_id, as.data.frame(individualsX))
  }else{
    X_id <- as.data.frame(individualsX[,1])
  }




  if(is.matrix(individualsY)){
    if(!all.equal(rownames(individualsY), colnames(interactionsZ))) stop("Pls provide the right colnames for Interaction matrix")
    if(is.null(colnames(individualsY))) stop("Pls provide colnames")
    Y_id <- as.data.frame(rownames(individualsY))
    individualsY <- cbind(Y_id, as.data.frame(individualsY))
  }else{
    Y_id <- as.data.frame(individualsY[,1])
  }




  XY_id <- rowr::cbind.fill(X_id, Y_id, fill = NA)
  XY_id <- expand.grid(XY_id)
  XY_id <- XY_id[complete.cases(as.matrix(XY_id)),]

  colnames(XY_id) <- c("X","Y")
  colnames(individualsX)[1] <- "X"
  colnames(individualsY)[1] <- "Y"

  XY_id <- merge(XY_id, individualsX, by.x = "X", by.y = "X")
  XY_id <- merge(XY_id, individualsY, by.x = "Y", by.y = "Y")

  colnames(XY_id) <- make.names(names(XY_id), unique = T)


  Z <- as.matrix(interactionsZ)
  Z_m <- subset(reshape2::melt(Z))
  Z_m <- as.data.frame(Z_m)
  colnames(Z_m) <- c("X","Y")

  XYZ <- merge(XY_id, Z_m, by.x = c("X", "Y"),
                           by.y = c("X", "Y")
               )
  #drop <- c("X","Y")
  #XYZ <- XYZ[, !(names(XYZ) %in% drop)]
  #head(XYZ)
  XYZ[,ncol(XYZ)] <- make.names(XYZ[,ncol(XYZ)])
  XYZ[XYZ == "X1"] <- "X"
  XYZ[XYZ == "X0"] <- "o"
  XYZ[,ncol(XYZ)] <- as.factor(XYZ[,ncol(XYZ)])
  colnames(XYZ)[ncol(XYZ)] <- "target"


  out$interactionlist <- XYZ
  out$target <- "target"
  if(is.matrix(individualsX) && is.matrix(individualsY)){
    out$split  <- c(ncol(individualsX), ncol(individualsY))
  } else {
    out$split  <- c((ncol(individualsX) - 1), (ncol(individualsY) -1))
  }
  class(out) <- "InteractionList"
  return(out)
}
