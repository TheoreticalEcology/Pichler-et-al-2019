
#' build regression model function
#' @param method method
#' @param settings settings
buildRegression = function(method, settings){

  extra = settings$learnerSettings[[method]]

  learner <- getLearner(method, balanceClasses = "Regression", extra = extra)

  settings$balanceClasses = "Regression"

  parameter <- getPars(method, settings, extra = extra)

  measures <- list(mlr::rmse, mlr::mse,mlr::kendalltau, mlr::sae,settings$tuningMetric )

  if (settings$tune == "random") {
    tuneCtrl <- mlr::makeTuneControlRandom(maxit = settings$iters)
  } else if (settings$tune == "MBO") {
    tuneCtrl <- mlr::makeTuneControlMBO(tune.threshold = tune.threshold)
  } else {
    stop("Wrong tuning method",call. = FALSE)
  }
  tuningValidation   <- do.call(mlr::makeResampleDesc, settings$crossValidation$inner)
  if(!inherits(settings$crossValidation$outer,"ResampleInstance")) resampleValidation <- do.call(mlr::makeResampleDesc, settings$crossValidation$outer)
  else resampleValidation <- settings$crossValidation$outer
  mlr::configureMlr(show.info = F)

  if(method == "multiNomDnn") speciesNames = TRUE
  else speciesNames = FALSE

  if(!method == "glm") learnerR <- mlr::makeTuneWrapper(learner = learner, resampling = tuningValidation, par.set = parameter, control = tuneCtrl, measures = settings$tuningMetric)
  else learnerR <- mlr::makeFeatSelWrapper(learner = learner, resampling = tuningValidation,  control = mlr::makeFeatSelControlSequential(method = "sfbs"), measures = aic)

  modelFunction <- function(classCommunity) {
    if(!is.null(classCommunity$settings$seed)) set.seed(classCommunity$settings$seed)
    parallelSetup(classCommunity$settings)
    fitVariables = classCommunity$settings$fitVariables
    data = classCommunity$data
    target = classCommunity$settings$target
    block = classCommunity$settings$block
    if(speciesNames) task = mlr::makeRegrTask(data = data, target = target, coordinates  = block)
    else task = mlr::makeRegrTask(data = data[,fitVariables], target = target, coordinates  = block)
    if(method != "multiNomDnn") task   <- mlr::createDummyFeatures(task)
    else {
      cols = colnames(data)[which(unlist(lapply(data, is.factor)) == TRUE, arr.ind = T)[-c(1,2)]]
      if(length(cols) != 0 ) task   <- mlr::createDummyFeatures(task, cols = cols)
    }
    # if(method %in% c("dnn", "cnn", "wideDeep","preDnn")) task <- mlr::normalizeFeatures(task, method = "range")
    if(classCommunity$settings$normalize) task <- mlr::normalizeFeatures(task, method = "standardize")

    if(!method == "glm")result <- tryCatch(mlr::resample(learnerR, task, resampleValidation, measures = measures,extract = getTuneResult, models = classCommunity$settings$keepModels),
                       error = function(err) return(err))
    else result <- tryCatch(mlr::resample(learnerR, task, resampleValidation, measures = measures, models = classCommunity$settings$keepModels),
                       error = function(err) return(err))

    out <- list()
    out$result <- result
    # out$task <- task

    rm(classCommunity)
    parallelMap::parallelStop()
    gc()
    return(out)
  }
  return(modelFunction)
}




#' build model function
#' @param method method
#' @param balance method
#' @param settings settings

buildClassification = function(method, balance, settings){

  extra = settings$learnerSettings[[method]]
  parameter <- getPars(method, settings, extra)
  learner   <- getLearner(method, settings$balanceClasses,extra =  extra)
  if(is.null(learner)) return(NULL)
  tune.threshold <- FALSE

  if(settings$tuningMetric$id == "f1") measures <- list(mlr::auc, mlr::bac, mlr::acc,settings$tuningMetric)
  else measures <- list(mlr::auc, mlr::bac, mlr::acc, mlr::f1, settings$tuningMetric)

  if (settings$tune == "random") {
    tuneCtrl <- mlr::makeTuneControlRandom(maxit = settings$iters, tune.threshold = tune.threshold, tune.threshold.args = list(measure = tss))
  } else if (settings$tune == "MBO") {
    tuneCtrl <- mlr::makeTuneControlMBO(tune.threshold = tune.threshold)
  } else {
    stop("Wrong tuning method",call. = FALSE)
  }
  tuningValidation   <- do.call(mlr::makeResampleDesc, settings$crossValidation$inner)
  if(!inherits(settings$crossValidation$outer,"ResampleInstance")) resampleValidation <- do.call(mlr::makeResampleDesc, settings$crossValidation$outer)
  else resampleValidation <- settings$crossValidation$outer
  mlr::configureMlr(show.info = F)

  if(!method == "glm") learnerR <- mlr::makeTuneWrapper(learner = learner, resampling = tuningValidation, par.set = parameter, control = tuneCtrl, measures = settings$tuningMetric)
  else learnerR <- mlr::makeFeatSelWrapper(learner = learner, resampling = tuningValidation,  control = mlr::makeFeatSelControlSequential(method = "sfbs"), measures = aic)

  modelFunction <- function(classCommunity) {
    parallelSetup(classCommunity$settings)
    fitVariables = classCommunity$settings$fitVariables
    data = classCommunity$data
    target = classCommunity$settings$target
    positive = classCommunity$settings$positive
    block = classCommunity$settings$block
    task = mlr::makeClassifTask(data = data[,fitVariables], target = target, positive = positive, coordinates = block)

    task   <- mlr::createDummyFeatures(task)
    # if(method %in% c("dnn", "cnn", "wideDeep","preDnn")) task <- mlr::normalizeFeatures(task, method = "range")
    if(classCommunity$settings$normalize) task <- mlr::normalizeFeatures(task, method = "standardize")

    if(!method == "glm") result <- tryCatch(mlr::resample(learnerR, task, resampleValidation, measures = measures,extract = getTuneResult, models = classCommunity$settings$keepModels),
                       error = function(err) return(err))
    else  result <- tryCatch(mlr::resample(learnerR, task, resampleValidation, measures = measures, models = classCommunity$settings$keepModels),
                             error = function(err) return(err))

    out <- list()
    out$result <- result
    # out$task <- task

    rm(classCommunity)
    parallelMap::parallelStop()
    gc()
    return(out)
  }
  return(modelFunction)


}



