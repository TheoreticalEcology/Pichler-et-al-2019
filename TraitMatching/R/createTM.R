#' @title Transform the data and fit model by tuning the Parameter.
#'
#' @description
#' The function \code{\link{TM}} imputes missing values in the input predictor matrices and transforms
#' the data into a format suitable for the available methods.
#' Cross validation is used to test the models' performances. Model parameter will be tuned by default.
#'
#' @param X matrix with n row species and n cols traits. First column = species
#' @param Y matrix with n row species and n cols traits. First column = species
#' @param Z matrix with X*Y species. Interactions can be binaries or probabilities
#' @param interactionList (Species+traits)*(Species+traits) interaction list with Interactionvalue
#' @param method Method to be used. RF, RF_random, knn, knn_FS, SVM, (deep)neural net or best for selecting best of these models. RF_random is default
#' @param tune Default method = random
#' @param parallel boolean. Parallel computing. Default = T
#' @param impute boolean. Data is imputed by default = T
#' @param target Target value
#' @param iters for tuning control. Random search, default = 5
#' @param tuning_metric Tuning evaluation metric, default = GAT
#'
#'


###### ----------------------------------
### Issues/Idea
## Aim: - out of matrix prediction
##      - Na's in matrix prediction
## Issues to solve:
## - kNN
## - Find missing data in matrix and identify... -> after creating IA list
## - Silencing missForest (annoying)
## split function here



TM <- function(X = NULL, Y = NULL, Z = NULL, interactionList = NULL, method = "RF_random", tune = "random", tuning_metric = "auc",
                          parallel = T, impute = T, target = NULL, iters = NULL){
  out <- list()
  if(is.null(X) | is.null(Y) | is.null(Z)) stop("X, Y, Z or Interactionlist have to be inserted")
  if(!is.null(interactionList) && is.null(target)) stop("Target must be provided")
  if(!(method == "RF_random" | method == "SVM" | method == "kknn" | method == "naive" | method == "kknn_FS" | method == "keras" | method == "ranger" | method == "boost" | method == "keras_seq"| method == "keras_conv")) stop("Provide one of the known methods")

  if(!is.null(X) && !is.null(Y) && !is.null(Z)){

    # Impute
    imputed_Data    <- createImpData(X = X, Y = Y)
    out$Impute <- imputed_Data

    # Create interaction list
    interactionList <- createInteractionList(individualsX  = imputed_Data$X, individualsY  = imputed_Data$Y, interactionsZ = Z)
    out$split <- interactionList$split
    out$InteractionList <- interactionList$interactionlist
  }


#
#   if(method == "best"){
#     out$Result <- findBest(interactionList, parallel, tune, target, iters)
#   }
#   else{
      for(i in 1:length(method)){
        cat(method[i],i/(1/length(method)), "...\n")
        out$Result[[method[i]]] <- createModel(interactionList, method[i], parallel, tune, target, iters, tuning_metric)


        out$Result[[method[i]]]$tresh <- mlr::generateThreshVsPerfData(out$Result[[method[i]]]$pred, list(mlr::fpr, mlr::tpr))
        youden <- apply(out$Result[[method[i]]]$tresh$data[,1:2], MARGIN = 1, function(x) -1 * diff(x))
        out$Result[[method[i]]]$best_tresh <- which(youden %in% min(youden))


        for(i in 1:length(out$Result)){
          out$Result[[i]]$tresh <- mlr::generateThreshVsPerfData(out$Result[[i]]$pred, list(mlr::fpr, mlr::tpr))
          youden <- apply(out$Result[[i]]$tresh$data[,1:2], MARGIN = 1, function(x) -1 * diff(x))
          out$Result[[i]]$best_tresh <- which(youden %in% min(youden))
        }

    }
  # }
  class(out) <- "TMmodel"

  if(length(method) != 1) class(out) <- c("TMmodel", "Multi")
  #   if(method == "RF_random" | method == "RF"){
  #   out$Importance <- createImportance(out$Result)
  # }

  return(out)

}

####### Issues: plot treshold vs performance, generateCalibrationData(x)





#
# createImportance <- function(x){
# learner <- mlr::makeWeightedClassesWrapper( mlr::makeLearner("classif.ranger"))
# learner <- setHyperPars(learner, par.vals =  model$Result$extract[[1]]$x)
# importance <- mlr::generateFeatureImportanceData(model$Result$task, learner = learner)
#
#
#  imp <- mlr::generateThreshVsPerfData(model$Result)
# #   #
#  imp <- mlr::generateFilterValuesData(x$task, method = "randomForest.importance")
#   # plotFilterValues(imp)
#   data <- getTaskData(x$task)
#   pars$formula <- formula(target~.)
#   pars$data <- getTaskData(x$task)
#
#   p <- pars[c("formula", "data", "ntree", "mtry")]
#
#   rf <- randomForest(target~., data )
#   randomForest::varImpPlot(rf)
#
#   pars <- x$extract[[1]]$x
#    rrr <- do.call(randomForest,  p)
#    rrr$importance
#
#   rfv <- rf$importance
#   rfv[order(rf$importance, decreasing = T), ]
#
#   tt <- t(importance$res)
#   tt[order(tt, decreasing = T), ]
# }



