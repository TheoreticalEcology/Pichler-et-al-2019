#' createCommunity
#'
#' create Community object for \code{\link{runTM}}
#'
#' @param a, group a
#' @param b, group b
#' @param z, interaction matrix for a and b, rows == a and cols == b
#' @param community, list with communities/groups or data.frames
#' @param target, in case of community, provide name of response column
#' @param positive, in case of community, provide the positive class
#' @param impute, missing values in a and b will be imputed
#' @param log = T, log(x+1) will be applied to response (for count data)
#' @export

createCommunity = function(a = NULL, b = NULL, z = NULL, community = NULL, response = NULL, positive = NULL, impute = T, log = T){

  classCommunity = list()

  checkInput(a, b, z, community, response, positive)

  # impute
  impData = list()
  interDataList = list()
  interTmpDataList = list()
  z_list = list()

  if(!is.null(a)) impData[[1]] = imputeData(a,b)

  if(is.null(a) && !is.null(community)){
    if(is.data.frame(community)){
      interTmpDataList[[1]] = community
    } else {
      for(elements in 1:length(community)){
        if(!is.data.frame(community[[elements]])){
          impData[[elements]] = do.call(imputeData, community[[elements]][1:2])
          z_list[[elements]] = community[[elements]][[3]]
        } else {
          interTmpDataList[[elements]] = community[[elements]]
          }
      }
    }
  }
  interTmpDataList = interTmpDataList[!sapply(interTmpDataList, is.null)]


  # create interaction
  interDataList = list()
  if(length(impData) != 0){
    if(!is.null(a)) interDataList = createInter(impData, z, log)
    else interDataList = createInter(impData, z_list, log)

    classCommunity$target = interDataList$target
    classCommunity$type = interDataList$type

    if(classCommunity$type == "Classification") {
      if(is.null(positive)) classCommunity$positive = interDataList$positive
    }
  }

  if(!length(interTmpDataList) == 0) interDataList$data = c(interTmpDataList, interDataList$data)



  # merge Communities
  interData = mergeCommunities(interDataList$data)

  classCommunity$data = interData

  if(!is.null(response) && is.null(classCommunity$target)){
    classCommunity$target = response
    if(length(unique(classCommunity$data[,response])) > 2) classCommunity$type = "Regression"
    else classCommunity$type = "Classification"
  }

  # create coordinates for species cv

  block = createBlock(classCommunity$data)

  classCommunity$block = block


  class(classCommunity) = c("Community", classCommunity$type)
  return(classCommunity)


}

#' createBlock
#' create coords
#' @param data data which to block
createBlock = function(data){
  levels(data$Y) = 1:length(levels(data$Y))
  data$X = data$Y

  data$X = as.integer(data$X)
  data$Y = as.integer(data$Y)

  block = data[,c("X","Y")]
  return(block)


}


#' mergeCommunities
#' merge communities
#' @param data data to merge
mergeCommunities = function(data){
  if(length(data) == 1) return(data[[1]])

  for(i in 1:length(data)){
    if(i == 1) community = data[[i]]
    else community = rbind(community, data[[i]])
  }
  return(community)

}



#' @param impData imputed groups or inter
#' @param z interaction matrix
#' @param log log
createInter = function(impData, z, log){
  out = list()

  data = list()

  target = NULL

  for(i in 1:length(impData)){
    if(length(impData[[i]]) == 1) {

      data[[i]] = impData[[i]]
    } else {

      #if(length(impData[[i]]) == 2) z = impData[[i]][[3]]

      X = impData[[i]][[1]]
      Y = impData[[i]][[2]]

      X_id = as.data.frame(X[,1])
      Y_id = as.data.frame(Y[,1])

      XY_id <- rowr::cbind.fill(X_id, Y_id, fill = NA)
      XY_id <- expand.grid(XY_id)
      XY_id <- XY_id[complete.cases(as.matrix(XY_id)),]

      colnames(XY_id) <- c("X","Y")
      colnames(X)[1] <- "X"
      colnames(Y)[1] <- "Y"

      XY_id <- merge(XY_id, X, by.x = "X", by.y = "X")
      XY_id <- merge(XY_id, Y, by.x = "Y", by.y = "Y")

      colnames(XY_id) <- make.names(names(XY_id), unique = T)


      if(!is.data.frame(z)) Z <- as.matrix(z[[i]])
      else Z <- as.matrix(z)

      Z_m <- subset(reshape2::melt(Z))
      Z_m <- as.data.frame(Z_m)
      colnames(Z_m) <- c("X","Y")

      XYZ <- merge(XY_id, Z_m, by.x = c("X", "Y"),
                     by.y = c("X", "Y")
        )

      if(length(unique(Z_m[,3])) <= 2){
        XYZ[,ncol(XYZ)] <- make.names(XYZ[,ncol(XYZ)])
        XYZ[XYZ == "X1"] <- "positive"
        XYZ[XYZ == "X0"] <- "negative"
        XYZ[,ncol(XYZ)] <- as.factor(XYZ[,ncol(XYZ)])
        out$type = "Classification"
        out$positive = "positive"
        colnames(XYZ)[ncol(XYZ)] <- "target"
      } else {
        out$type = "Regression"
        colnames(XYZ)[ncol(XYZ)] <- "target"
        # xCount = apply(Z,1,sum)
        # yCount = apply(Z,2,sum)
        # xC = data.frame(names(xCount), xCount)
        # colnames(xC) = c("X", "CountsX")
        # yC = data.frame(names(yCount), yCount)
        # colnames(yC) = c("Y", "CountsY")
        # XYZ <- merge(XYZ, yC, by.x = c("Y"),
        #              by.y = c("Y"))
        # XYZ <- merge(XYZ, xC, by.x = c("X"),
        #              by.y = c("X"))
        if(log) XYZ[,"target"] = log(XYZ[,"target"] + 1)

      }


      target = "target"

      data[[i]] = XYZ
    }
  }

  out$data = data
  out$target = target
  return(out)

}




#' impute groups
#' @param a a
#' @param b b

imputeData = function(a, b){

  X = a
  Y = b

  X <- X[!duplicated(as.factor(X[,1])),]
  last_col_X <- 2:ncol(X)
  Y <- Y[!duplicated(as.factor(Y[,1])),]
  last_col_Y <- 2:ncol(Y)

  X_imp <- missForest::missForest(X[,last_col_X])
  Y_imp <- missForest::missForest(Y[,last_col_Y])

  X_imp <- cbind(X[,1], X_imp$ximp)
  colnames(X_imp)[1] <- colnames(X)[1]

  Y_imp <- cbind(Y[,1], Y_imp$ximp)
  colnames(Y_imp)[1] <- colnames(Y)[1]

  out = list(a = X_imp, b = Y_imp)
  return(out)

}






#' check function for input
#' @param a, group a
#' @param b, group b
#' @param z, interaction matrix for a and b, rows == a and cols == b
#' @param community, list with communities/groups or data.frames
#' @param target, in case of community, provide name of response column
#' @param positive, in case of community, provide the positive class

checkInput = function(a = NULL, b = NULL, z = NULL, community = NULL, response = NULL, positive = NULL){
  if(is.null(c(a,b,z,community))) stop("provide groups and their interaction matrix or a community list",call. = FALSE)

  if(all(sapply(c(a,b,z,community), is.null))) warning("a,b and community provided, only a,b, and z will be used", call. = FALSE)

  if(is.null(a) && !is.null(community)){
    if(is.data.frame(community)){
      if(is.null(response)) stop("pls provide response for the communnity", call. = FALSE)
     # if(is.null(positive)) warning("positive should be provided, it improves parameter tuning", call. = FALSE)
    } else if(is.list(community)){
      for(elements in 1:length(community)){
        if(!is.data.frame(community[[elements]])) do.call(checkInput,community[[elements]])
        else checkInput(community = community[[elements]], response = response)
      }
    } else {
      stop("wrong input for community", call. = FALSE)
    }
  }

  if(!is.null(a)){
    if(!is.data.frame(a) && !is.data.frame(b)) stop("provide a and b as data.frame with rownames or first column for individuals", call. = FALSE)
    if(is.null(z)) stop("z is empty, provide interaction matrix/data.frame ")
  }

}
