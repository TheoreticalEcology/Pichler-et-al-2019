#' minOneInter
#' select only rows and columns with at least one interaction
#' @param inter interaction matrix
#' @export
minOneInter = function(inter){
  inter = as.matrix(inter)
  whichCol = apply(inter, 2, FUN = function(x){ if(any(x == 1)) return(TRUE)
    else return(FALSE)})
  whichRow = apply(inter, 1, FUN = function(x){ if(any(x == 1)) return(TRUE)
    else return(FALSE)} )
  return(data.frame(inter[whichRow,whichCol]))
}

#' createInterFeatures
#' Feature Eng funcion. Create interaction features, all combinations
#' @param df data.frame with the last variable as target

createInterFeatures = function(df){
  nF = sum(1:(ncol(df)-2))
  Result = data.frame(matrix(0,nrow(df), nF))

  counter = 1
  for(i in 1:(ncol(df)-2)){
    for(j in (i+1):(ncol(df)-1)){
      Result[,counter] = df[,i]*df[,j]
      colnames(Result)[counter] = paste0(colnames(df)[i],"_", colnames(df)[j])
      counter = counter+1
    }
  }
  return(cbind(df[,-ncol(df)],Result,target= df[,ncol(df)]))

}


