#' Fit TM model
#'
#' @description Fit, tune and cross-validate a TM model
#'
#' @param classCommunity object of class classCommunity created by \code{\link{createCommunity}}
#' @param settings list of settings for the machine learning model. See details
#' @param method Which ML algorithm to be used. RF, knn, SVM, DNN, CNN, boost, ngBinDNN, naive, or CNN. See details. "RF" is default
#' @param tune How to tune ML parameters. We support only "random" at the moment.
#' @param tuningMetric Which predictive measurement should be used for tuning. "AUC" is default. See details.
#' @param parallel boolean or numeric. See details.
#' @param iters Number of tuning steps
#' @param crossValidation List for CV plan. See details.
#' @param fitSpecies boolean for fit species as categorical predictor.
#' @param balanceClasses How to balance classes. Default is oversampling "Over".
#' @param returnOnlySetup return setup without fitting.
#' @param block ...
#' @param seed set seed
#' @param keepModels Whether to return all fitted Models or not.
#' @param normalize Whether to normalize data or not.
#'
#' @details
#' \itemize{
#' \item \code{\link{classCommunity}}: Provide a, b, z as data.frames. a and b are group matrices (e.g. plants and pollinators). First column in each data.frame must be species names. The z data.frame is the interaction matrix with rownames == speciesnames of a, and colnames == speciesnames of b.
#' \item \code{settings}: The parameters you set here will be not tuned. We support at the moment the following ML algorithms with parameters:
#' RF (Classification and Regression): mtry, nodesize, replace.
#' knn (Classification and Regression): k, kernel \code{= c("rectangular", "triangular", "epanechnikov", "optimal")}
#' SVM (Classification and Regression): kernel = \code{c("gauss_rbf", "poisson")}, lambdas, gammas.
#' DNN (Classification and Regression): lr, arch, drop, nLayer, bias, activation, archFunction, batch, decay, alpha, opti, logTarget (only for Regression).
#' boost (Classification and Regression): booster = \code{c("gbtree", "tree")}, sample_type = \code{c("uniform", "weighted")}, normalize_type = \code{c("tree", "forest")}, eta, gamma, max_depth, lambda, alpha, min_child_weight, nrounds, rate_drop, skip_drop.
#' CNN (Classification): lr, arch, drop, filter, bias, pool, nLayer, nConv, activation, archFunction, batch, decay, opti
#' naive (Classification): laplace
#' negBinDnn (Regression): lr, arch, nLayer, activation, bias, archFunction, batch, opti, distribution =\code{ c("poisson", "negBin")}, Link
#' \item \code{method}: scalar or subset of \code{c("RF", "SVM", "DNN", "CNN", "boost", "ngBinDnn", "naive", "knn")}
#' \item \code{tuningMetric}: At the moment we support: \code{c("tss", "spearmanrho", "rmsle", "poisson", "poissonQuasi", "negBinLL", "rmse", "expvar", "arsq", "sae", "mcc", "auc", "bac", "ber", "brier.scaled", "f1", "gpr", "kappa", "logloss", "mcc", "ppv", "ssr", "tpr", "wkappa")}
#' \item \code{parallel}: Boolean or numeric vector. If TRUE, all available cores will be used. If numeric vector, "double" parallelization will be used. Each model will get its own cluster and the tuning will be parallelized.
#' \item \code{crossValidation}: list of two named list that define the outer and inner resampling strategy. \code{list = list(outer = list(method = "CV", iters), inner = list(method = "CV", iters))}. Methods can be change to SpCV. With SpCV, Species from the b Matrix will be left out with all its interactions (structured CV).
#' }
#' @example /inst/examples/runTM_example.R
#' @author Maximilian Pichler
#' @export


runTM <- function(classCommunity, settings = NULL,method = "RF", tune = "random",tuningMetric = "auc", parallel = T, iters = 20, crossValidation = list(
  outer = list(method = "CV", iters = 10),
  inner = list(method = "CV", iters = 3)
), balanceClasses = "Over", fitSpecies = F, returnOnlySetup = F, block = NULL, seed = NULL, keepModels = FALSE, normalize = T){

  require(mlr, quietly = TRUE)
  if(any(!method %in% c("RF", "SVM", "DNN", "CNN", "boost", "ngBinDnn", "naive", "knn", "RFranger"))) stop("Method is not supported")

  method[which(method == "RF")] = "RFranger"

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
