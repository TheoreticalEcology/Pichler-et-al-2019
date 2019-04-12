# modelSettings <- list("RF" = list("ntree" = 100, mtry = "200"))

#' Show Info and parameters of available learners
#' @param learner default, all
#' @param models default TRUE, show info about all available models
#' @param onlyModels default FALSE, return only available models
#' @param onlyMeasures default FALSE, return only available Measures
#' @export

showInfo <- function(learner = NULL, models = T, onlyModels = F, onlyMeasures = F, onlyBC = F){
  info <- list()
  info$availableModels <- c("RF","glm", "glm_step","SVM", "gbm","RFsrc","knn", "naive", "knnFS", "dnn", "RFranger", "boost", "cforest", "cnn", "wideDeep","preDnn","negBinDnn", "multiNomDnn", "poissonDnn")

  if(onlyBC) return(c("Over", "Under", "SMOTE", "WeightedClasses", "None","Over+WC", "Regression"))

  if(onlyModels) return(info$availableModels)

  measureList <- list(tss = tss,spearmanrho = mlr::spearmanrho,rmsle = mlr::rmsle,poisson = poisson,poissonQuasi = poissonQuasi,negBinLL = negBinLL,rmse = mlr::rmse, expvar = mlr::expvar,arsq = mlr::arsq, sae = mlr::sae,mcc = mlr::mcc, auc = mlr::auc, bac = mlr::bac, ber = mlr::ber, brier.scaled = mlr::brier.scaled, f1  = mlr::f1, gpr = mlr::gpr, kappa = mlr::kappa, logloss = mlr::logloss, mcc = mlr::mcc, ppv = mlr::ppv, ssr = mlr::ssr, tpr = mlr::tpr, wkappa = mlr::wkappa)

  if(onlyMeasures) return(measureList)

  if(!is.null(learner) && learner %in% availableModels) info$availableModels <- learner

  info$tuneablePars <- sapply(info$availableModels, FUN = function(x) names(getPars(x)$pars))

  info$setablePars  <- sapply(info$availableModels, FUN = function(x) names(ParamHelpers::getParamSet(getLearner(x, balanceClasses = F))$pars))

  info$measureList <- measureList

  class(info) <- "TMInfo"

  return(info)

}

#' print method for showInfo object
#' @param info from showInfo
print.TMinfo <- function(info){
  cat("Available Methods::\n")
  cat(info$availableModels, "\n")
  cat("--------------------------------------------------\n")
  cat("Tuneable Parameter: \n")
  for(name in names(info$tuneablePars)) cat(name, ": ", info$tuneablePars[[name]], "\n")
  cat("--------------------------------------------------\n")
  cat("Setable Parameter: \n")
  for(name in names(info$setablePars)) cat(name, ": ", info$setablePars[[name]], "\n")

}


#' Setup function for model setup. It's possible to manually set parameters to overwrite default values and specify parameters to be tuned.
#' @param RF rf from random Forest
#' @param SVM svm from
#' @param keras keras tensorflow
#' @param RFranger rf from ranger package
#' @param knn knn... from...
#' @param knnFS knn with feature selection
#' @param naive naive bayes algo
#' @param boost boost algo
#' @export

# RF = list(set = list(ntree = 100), tune = c("mtry", "importance"))

modelSetup <- function(RF = NULL, SVM = NULL, dnn = NULL, RFranger = NULL, knn = NULL, knnFS = NULL, naive = NULL, boost = NULL, modelSetup = NULL, cnn = NULL, wideDeep = NULL,preDnn = NULL){
  # if(any(!is.null(RF), !is.null(SVM), !is.null(keras), !is.null(RFranger), !is.null(knn), !is.null(knnFS), !is.null(naive), !is.null(boost))) stop("Error. No input")
  if(is.null(modelSetup)) setup <- c(as.list(environment()))
  else setup <- modelSetup
  setup <- setup[!sapply(setup, is.null)]


  #check contends:
  error <- sapply(names(setup), checkSetup, setup)

  if(any(error)) stop("Error in modelSetup")

  class(setup) <- "modelSetup"
  return(setup)
}




#' Checking input
#' @param name name of method
#' @param setup setup for the method


checkSetup <- function(name, setup){
  learner <- tryCatch(getLearner(name, balanceClasses = F), error = function(err) return(err))
  error <- FALSE
  if("error" %in% class(learner)) return(error = T)

  if(!is.logical(setup[[name]])){
    possiblePars <- names(ParamHelpers::getParamSet(learner)$pars)

    if(any(!names(setup[[name]]$set) %in% possiblePars)) return(error = T)

    if("tune" %in% names(setup[[name]])){
      possibleTunePars <- names(getPars(name)$pars)
      if(any(!setup[[name]]$tune %in% possibleTunePars)) return(error = T)
    }
  }

  return(error)
}


#mm <- modelSetup(RF = list(tune = "mtry"), SVM = T)
