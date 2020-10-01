#' Fit TM model
#'
#' @description Fit, tune and cross-validate a TM model
#'
#' @param community object of class classCommunity created by [TraitMatching::createCommunity()]
#' @param settings list of settings for the machine learning model. See details
#' @param method Which ML algorithm to be used. RF, knn, SVM, DNN, CNN, boost, ngBinDNN, naive, or CNN. See details. "RF" is default
#' @param tune How to tune ML parameters. We support only "random" at the moment.
#' @param metric Which predictive measurement should be used for tuning. "AUC" is default. See details.
#' @param parallel boolean or numeric. See details.
#' @param iters Number of tuning steps
#' @param crossValidation List for CV plan. See details.
#' @param balance How to balance classes. Default is oversampling "Over".
#' @param block ...
#' @param seed set seed
#' @param keepModels Whether to return all fitted Models or not.
#'
#' @author Maximilian Pichler
#' @export


runTM = function(community,
                 settings = NULL,
                 method = "RF",
                 tune = c("random","grid"),
                 metric = c("AUC", "R2", "Spear"),
                 parallel = TRUE,
                 iters = 20L,
                 crossValidation = list(
                   outer = list(method = "CV", iters = 10),
                   inner = list(method = "CV", iters = 3)),
                 balance = c(FALSE, "Over", "Under", "SMOTE"),
                 block = NULL,
                 seed = 42,
                 keepModels = c(TRUE, FALSE)){

  stopifnot(
    any(!method %in% c("RF", "SVM", "kNN", "DNN", "BRT") )
  )
  tune = match.arg(tune)
  metric = match.arg(metric)
  balance = match.arg(balance)
  keepModels = match.arg(keepModels)

  type = class(community)[2]

  learners = lapply(method, function(m) getBaseLearners(m, type))

  task =
    if(type == "classif") mlr3::TaskClassif$new(id = "classif_community", backend = community$data[, -c(1,2)], target = community$target, positive = "positive")
    else mlr3::TaskRegr$new(id = "regr_community", backend = community$data[, -c(1,2)], target = community$target)

}




getBaseLearners = function(method = "RF", type = c("classif", "regr")) {
  type = match.arg(type)
  learner = switch (method,
    RF  = paste0(type,".ranger"),
    SVM = paste0(type,".svm"),
    kNN = paste0(type,".kknn"),
    DNN = paste0(type,""),
    BRT = paste0(type,".xgboost"),
  )
  if(type=="classif") return(mlr3::lrn(learner, predict_type = "prob"))
  else return(mlr3::lrn(learner))
}




getParamSets = function(method = "RF", extra = NULL) {
  pars = switch(method,
      RF = list(paradox::ParamDbl$new("alpha", lower = 0.0, upper = 1.0),
                paradox::ParamInt$new("min.node.size", lower = 1, upper = 30L),
                paradox::ParamInt$new("mtry", lower = 1, upper = extra$mtry_max),
                paradox::ParamLgl$new("regularization.usedepth", default = TRUE)),
      kNN =list(paradox::ParamInt$new("k", lower = 1L, 20L),
                paradox::ParamFct$new("kernel", levels=c("rectangular","triangular","epanechnikov","biweight","triweight","optimal"),default = "optimal")),
      SVM =list(paradox::ParamFct$new("kernel", levels=c("linear", "polynomial", "radial", "sigmoid"), default = "radial"),
                paradox::ParamDbl$new("gamma", lower = 0.0, upper = 5),
                paradox::ParamInt$new("degree", lower = 1, upper = 5),
                paradox::ParamDbl$new("cost", lower = 0, upper = 3)
                ),
      BRT= list(paradox::ParamDbl$new("alpha", lower = 0.0, upper = 3.0),
                paradox::ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"), default = "gbtree"),
                paradox::ParamDbl$new("eta",lower = 0.0, upper = 3.0),
                paradox::ParamFct$new("feature_selector", levels = c("cyclic", "shuffle", "random", "greedy", "thrifty"), default = "cyclic"),
                paradox::ParamDbl$new("gamma", lower = 0.0, upper = 3.0),
                paradox::ParamDbl$new("lambda", lower = 0.0, 5.0),
                paradox::ParamDbl$new("lambda_bias", lower = 0.0,upper = 5.0),
                paradox::ParamInt$new("max_depth", lower = 1L, upper = 50L ),
                paradox::ParamInt$new("nrounds", lower = 1L, upper = 80L))

  )
  return(paradox::ParamSet$new(pars))
}
