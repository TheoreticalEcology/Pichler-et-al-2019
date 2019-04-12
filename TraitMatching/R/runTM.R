#' runTM function
#'
#' @param classCommunity from createCommunity()
#' @param settings settings with lists for models
#' @param method Method to be used. RF, RF_random, knn, knn_FS, SVM, (deep)neural net or best for selecting best of these models. RF_random is default
#' @param tune Default method = random
#' @param parallel boolean. Parallel computing. Default = T
#' @param iters for tuning control. Random search, default = 5
#' @param tuningMetric Tuning evaluation metric, default = GAT
#' @param balanceClasses method to balance Classes, default = "Over" for Oversampling
#' @param crossValidation cv...
#' @param fitSpecies default = FALSE, per default species (first and second column) will not be fitted along, if TRUE both will be fitted but also vector with species columns name can be given
#'
#' @export


runTM <- function(classCommunity, settings = NULL,method = "RF", tune = "random",tuningMetric = "auc", parallel = T, iters = 20, crossValidation = list(
  outer = list(method = "CV", iters = 10),
  inner = list(method = "CV", iters = 3)
), balanceClasses = "Over", fitSpecies = F, returnOnlySetup = F, block = NULL, seed = NULL, keepModels = FALSE, normalize = T){

  if(is.null(settings)) {
    settings = list()
    learnerSettings = NULL
  }
  else learnerSettings = settings
  settings$learnerSettings = learnerSettings
  settings$target = classCommunity$target
  settings$positive = classCommunity$positive
  settings$type = classCommunity$type
  settings$tune = tune
  settings$method = method
  settings$tuningMetric = tuningMetric
  settings$parallel = parallel
  settings$iters = iters
  settings$crossValidation = crossValidation
  settings$balanceClasses = balanceClasses
  settings$split = ncol(classCommunity$data[,5:ncol(classCommunity$data)])
  settings$fitSpecies = fitSpecies
  settings$VarNames = colnames(classCommunity$data)
  settings$block = classCommunity$block
  settings$seed = seed
  settings$keepModels = T
  settings$normalize = normalize
  # if(is.logical(block) && block == T) settings$block = classCommunity$block
  # else if(is.character(block)) settings$block = classCommunity$block[, block]

  settings = checkSettings(settings)

  modelFunctions = list()

  if(settings$type == "Classification"){
    for (bc in settings$balanceClasses) {
      settings2 = settings
      settings2$balanceClasses = bc
      modelFunctions[[bc]] <- sapply(method, buildClassification, bc, settings2, simplify = FALSE, USE.NAMES = TRUE)
      modelFunctions[[bc]][sapply(modelFunctions[[bc]], is.null)] <- NULL
    }
  }

  if(settings$type == "Regression") {
    modelFunctions[["Regression"]] <- sapply(method, buildRegression, settings, simplify = FALSE, USE.NAMES = TRUE)
  }

  runBuild = list()
  runBuild$classCommunity = classCommunity
  runBuild$classCommunity$settings = settings
  runBuild$modelFunctions = modelFunctions
  runBuild$settings = settings

  Result = list()

  if(!returnOnlySetup) Result = runBuilds(runBuild)
  else Result$run = runBuild
  Result$classCommunity = classCommunity
  Result$settings = settings

  return(Result)


}



#' run model func
#' @param runBuild object

runBuilds = function(runBuild){

  parallel = runBuild$settings$parallel

  out = list()

  if (length(parallel) == 1) {
    if (length(runBuild$settings$balanceClasses) == 1) {
      out$Result <- sapply(runBuild$modelFunctions[[1]], FUN = function(x, classCommunity) return(x(classCommunity)), runBuild$classCommunity, simplify = FALSE, USE.NAMES = TRUE)
      class(out) <- "TMmodel"
    }
    else {
      for (bc in runBuild$settings$balanceClasses) {
        out$Result[[bc]] <- sapply(runBuild$modelFunctions[[bc]], FUN = function(x, classCommunity) return(x(classCommunity)), runBuild$classCommunity,  simplify = FALSE, USE.NAMES = TRUE)
      }
      class(out) <- c("TMmodel", "multiBalance")
    }
  } else {
    cl <- snow::makeCluster(parallel[1])
    snow::clusterEvalQ(cl, library(mlr))
    snow::clusterEvalQ(cl, library(TraitMatching))
    # snow::clusterExport(cl, "classTMmodel")

    if (length(runBuild$settings$balanceClasses) == 1) {
      out$Result <- snow::parSapply(cl, runBuild$modelFunctions[[1]], FUN = function(x, classCommunity) return(x(classCommunity)), runBuild$classCommunity, simplify = FALSE, USE.NAMES = TRUE)
      class(out) <- "TMmodel"
    }
    else {
      for (bc in runBuild$settings$balanceClasses) {
        out$Result[[bc]] <- snow::parSapply(cl, runBuild$modelFunctions[[bc]], FUN = function(x, classCommunity) return(x(classCommunity)), runBuild$classCommunity,  simplify = FALSE, USE.NAMES = TRUE)
      }
      class(out) <- c("TMmodel", "multiBalance")
    }
    snow::stopCluster(cl)
    gc()
  }

  return(out)
}







#' check settings
#'@param settings settings
checkSettings = function(settings){

  if (!all(names(settings$method) %in% showInfo(onlyModels = T))) stop("Error. Unknown model",call. = FALSE)
  settings$tuningMetric = showInfo(onlyMeasures = T)[[settings$tuningMetric]]
  if (!is.object(settings$tuningMetric)) stop("Wrong argument for tuningMetric",call. = FALSE)

  if(settings$type == "Regression") settings$balanceClasses = "Regression"

  if (!all(settings$balanceClasses %in% showInfo(onlyBC = T)) | is.logical(settings$balanceClasses)) stop("wrong Input for balanceClasses", call. = FALSE)

  fitSpecies = settings$fitSpecies
  VarNames = settings$VarNames

  if(is.logical(fitSpecies)){
    if(fitSpecies) fitVariables = VarNames
    else fitVariables = VarNames[3:length(VarNames)]
  } else{
    fitVariables = c(fitSpecies, VarNames[3:length(VarNames)])
  }
  settings$fitVariables = fitVariables

  return(settings)
}



#' parallelSetup
#' @param settings from classTMmodel
parallelSetup <- function(settings) {
  parallel <- settings$parallel
  method <- settings$method

  if(!parallel == FALSE){
    if (is.logical(parallel) && parallel == T) {
      cp <- parallel::detectCores() - 1
      if(settings$tune == "random") parallelMap::parallelStartSocket(cpus = cp, level = "mlr.tuneParams")
      if(settings$tune == "MBO") parallelMap::parallelStartSocket(cpus = cp, level = "mlr.resample")
    }
    if (is.numeric(parallel)){
      if(length(parallel) > 1) parallel <- parallel[2]
      if(settings$tune == "random") parallelMap::parallelStartSocket(cpus = parallel, level = "mlr.tuneParams")
      if(settings$tune == "MBO") parallelMap::parallelStartSocket(cpus = parallel, level = "mlr.resample")
    }
    if ("dnn" %in% method) parallelMap::parallelExport("trainLearner.classif.keras_seq", "predictLearner.classif.keras_seq", level = "mlr.tuneParams", show.info=FALSE)
    if ("cnn" %in% method) parallelMap::parallelExport("trainLearner.classif.keras_conv", "predictLearner.classif.keras_conv", level = "mlr.tuneParams", show.info=FALSE)
    if ("wideDeep" %in% method) parallelMap::parallelExport("trainLearner.classif.wideDeep", "predictLearner.classif.wideDeep", level = "mlr.tuneParams", show.info=FALSE)
    if ("preDnn" %in% method) parallelMap::parallelExport("trainLearner.classif.preKeras", "predictLearner.classif.preKeras", level = "mlr.tuneParams", show.info=FALSE)
    if ("SVM" %in% method) parallelMap::parallelExport("trainLearner.classif.liquidSVM_self", "predictLearner.classif.liquidSVM_self", level = "mlr.tuneParams", show.info=FALSE)
    if ("glm" %in% method) parallelMap::parallelExport("trainLearner.classif.binomial_self", "predictLearner.classif.binomial_self", level = "mlr.tuneParams", show.info=FALSE)

    if ("glm" %in% method) parallelMap::parallelExport("trainLearner.regr.glm_self", "predictLearner.regr.glm_self", level = "mlr.tuneParams", show.info=FALSE)
    if ("dnn" %in% method) parallelMap::parallelExport("trainLearner.regr.keras_seq", "predictLearner.regr.keras_seq", level = "mlr.tuneParams", show.info=FALSE)
    if ("cnn" %in% method) parallelMap::parallelExport("trainLearner.regr.keras_conv", "predictLearner.regr.keras_conv", level = "mlr.tuneParams", show.info=FALSE)
    if ("wideDeep" %in% method) parallelMap::parallelExport("trainLearner.regr.wideDeep", "predictLearner.regr.wideDeep", level = "mlr.tuneParams", show.info=FALSE)
    if ("preDnn" %in% method) parallelMap::parallelExport("trainLearner.regr.preKeras", "predictLearner.regr.preKeras", level = "mlr.tuneParams", show.info=FALSE)
    if("negBinDnn"%in% method )parallelMap::parallelExport("trainLearner.regr.negBinDnn", "predictLearner.regr.negBinDnn", level = "mlr.tuneParams", show.info=FALSE)
    if("multiNomDnn"%in% method )parallelMap::parallelExport("trainLearner.regr.multiNomDnn", "predictLearner.regr.multiNomDnn", level = "mlr.tuneParams", show.info=FALSE)
    if("poissonDnn"%in% method )parallelMap::parallelExport("trainLearner.regr.poissonDnn", "predictLearner.regr.poissonDnn", level = "mlr.tuneParams", show.info=FALSE)
  }
}
