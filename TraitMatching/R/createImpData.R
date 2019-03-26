#' Impute Data
#'
#' @param X Matrix with n rows species and n columns traits. First column = species
#' @param Y Matrix with n rows species and n columns traits. First column = species
#'
#' @export





# Issue: Imputat? miss vs hmisc?!


createImpData<-function(X = NULL, Y = NULL){
  out<-list()
  #X <- Plants
  #Y <- Insects

  if(is.null(X) | is.null(Y)) stop("Input matrices/data frames missing")

  last_col_X <- 1:ncol(X)
  last_col_Y <- 1:ncol(Y)

  if(is.data.frame(X)) {
    X <- subset(X, !duplicated(X[,1]))
    last_col_X <- 2:ncol(X)
  }
  if(is.data.frame(Y)){
    Y <- subset(Y, !duplicated(Y[,1]))
    last_col_Y <- 2:ncol(Y)
  }




  sink("miss")

  X_imp <- missForest::missForest(X[,last_col_X])
  Y_imp <- missForest::missForest(Y[,last_col_Y])
  sink(NULL)


  #X_hmisc <- aregImpute(~type+diameter, data=X[,2:ncol(X)], n.impute = 5)
  #Y_hmisc <- aregImpute(~body+tongue, data = Y, n.impute = 5, nk = 8)
  #Y_imp$OOBerror

  out$X_OOBerror <- X_imp$OOBerror
  out$Y_OOBerror <- Y_imp$OOBerror

  if(is.data.frame(X)){
    X_imp <- cbind(X[,1], X_imp$ximp)
    colnames(X_imp)[1] <- colnames(X)[1]
  } else{
    X_imp <- X_imp$ximp
  }
  if(is.data.frame(Y)){
    Y_imp <- cbind(Y[,1], Y_imp$ximp)
    colnames(Y_imp)[1] <- colnames(Y)[1]
  } else {
    Y_imp <- Y_imp$ximp
  }

  out$X <- X_imp
  out$Y <- Y_imp
  class(out) <- "ImpData"
  return(out)
}

