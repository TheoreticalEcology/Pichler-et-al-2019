

#' RF from randomForest package
#' @param method
#' @param balanceClasses
#' @param predict.type
#' @param extra settings for learner


getLearner <- function(method, balanceClasses, predict.type = "prob", predict.treshold = NULL,extra = NULL){

  if(balanceClasses == "Regression"){

    if(method == "RF") learner = do.call(mlr::makeLearner, c(list(cl = "regr.randomForest", predict.type = "se", importance = TRUE), extra))
    else if(method == "SVM") learner = do.call(mlr::makeLearner, c(list(cl = "regr.svm"), extra))
    else if(method == "knn") learner = do.call(mlr::makeLearner, c(list(cl = "regr.kknn"), extra))
    else if(method == "dnn") learner = do.call(mlr::makeLearner, c(list(cl = "regr.keras_seq"), extra))
    else if(method == "RFranger") learner = do.call(mlr::makeLearner, c(list(cl = "regr.ranger", predict.type = "se"), extra))
    else if(method == "boost") learner = do.call(mlr::makeLearner, c(list(cl = "regr.xgboost"), extra))
    else if(method == "knnFS") learner <- mlr::makeFilterWrapper(do.call(mlr::makeLearner, c(list(cl = "regr.kknn"), extra)))
    else if(method == "cforest") learner = do.call(mlr::makeLearner, c(list(cl = "regr.cforest"), extra))
    else if(method == "cnn") learner = do.call(mlr::makeLearner, c(list(cl = "regr.keras_conv"), extra))
    else if(method == "wideDeep") learner = do.call(mlr::makeLearner, c(list(cl = "regr.wideDeep"), extra))
    else if(method == "preDnn") learner = do.call(mlr::makeLearner, c(list(cl = "regr.preKeras"), extra))
    else if(method == "negBinDnn") learner = do.call(mlr::makeLearner, c(list(cl = "regr.negBinDnn"), extra))
    else if(method == "poissonDnn") learner = do.call(mlr::makeLearner, c(list(cl = "regr.poisson"), extra))
    else if(method == "multiNomDnn") learner = do.call(mlr::makeLearner, c(list(cl = "regr.multiNomDnn"), extra))
    else if(method == "gbm") learner = do.call(mlr::makeLearner, c(list(cl = "regr.gbm"), extra))
    else if(method == "RFsrc") learner = do.call(mlr::makeLearner, c(list(cl = "regr.randomForestSRC"), extra, list(importance = TRUE, proximity = "all")))
    else if(method == "glm") learner = do.call(mlr::makeLearner, c(list(cl = "regr.glm"), extra, model = TRUE))

  } else {
    if(method == "RF") learner = do.call(mlr::makeLearner, c(list(cl = "classif.randomForest", predict.type = predict.type, importance = TRUE), extra))
    else if(method == "SVM") learner = do.call(mlr::makeLearner, c(list(cl = "classif.svm", predict.type = predict.type), extra))
    else if(method == "knn") learner = do.call(mlr::makeLearner, c(list(cl = "classif.kknn", predict.type = predict.type), extra))
    else if(method == "naive") learner = do.call(mlr::makeLearner, c(list(cl = "classif.naiveBayes", predict.type = predict.type), extra))
    else if(method == "dnn") learner = do.call(mlr::makeLearner, c(list(cl = "classif.keras_seq", predict.type = predict.type), extra))
    else if(method == "RFranger") learner = do.call(mlr::makeLearner, c(list(cl = "classif.ranger", predict.type = predict.type), extra))
    else if(method == "boost") learner = do.call(mlr::makeLearner, c(list(cl = "classif.xgboost", predict.type = predict.type), extra))
    else if(method == "knnFS") learner <- mlr::makeFilterWrapper(do.call(mlr::makeLearner, c(list(cl = "classif.kknn", predict.type = predict.type), extra)))
    else if(method == "cforest") learner = do.call(mlr::makeLearner, c(list(cl = "classif.cforest", predict.type = predict.type), extra))
    else if(method == "cnn") learner = do.call(mlr::makeLearner, c(list(cl = "classif.keras_conv", predict.type = "prob"), extra))
    else if(method == "wideDeep") learner = do.call(mlr::makeLearner, c(list(cl = "classif.wideDeep", predict.type = predict.type), extra))
    else if(method == "preDnn") learner = do.call(mlr::makeLearner, c(list(cl = "classif.preKeras", predict.type = predict.type), extra))
    else if(method == "RFsrc") learner = do.call(mlr::makeLearner, c(list(cl = "classif.randomForestSRC", predict.type = predict.type), extra, list(importance = TRUE, proximity = "all")))
    else if(method == "glm") learner = do.call(mlr::makeLearner, c(list(cl = "classif.binomial", predict.type = predict.type), extra, model = TRUE))

      if(balanceClasses == "Over") return(mlr::makeOversampleWrapper(learner))
      else if(balanceClasses == "Under") return(mlr::makeUndersampleWrapper(learner))
      else if(balanceClasses == "SMOTE") return(mlr::makeSMOTEWrapper(learner))
      else if(balanceClasses == "OverBagging") return(mlr::makeOverBaggingWrapper(getLearner(method, balanceClasses = F, predict.type = "response")))
      else if(balanceClasses == "WeightedClasses"){
        if(!method %in% c("knn", "knnFS", "naive", "wideDeep")) return(mlr::makeWeightedClassesWrapper(learner))
        else return(NULL)
      }
      else if(balanceClasses == "Over+WC") return(mlr::makeOversampleWrapper(mlr::makeWeightedClassesWrapper(learner)))
      else return(learner)
  }

}




#' set Parameter
#' @param method learner
#' @param settings modelSettings to bet set
#' @param extra settings
#'

getPars <- function(method, settings = NULL, extra = NULL){
  parameter = ParamHelpers::makeParamSet()
  if(is.null(settings)){
    settings <- list(split = Inf, balanceClasses = F)
  }
  modelSettings <- settings$modelSettings
  if(method == "gbm") parameter = ParamHelpers::makeParamSet(
    ParamHelpers::makeIntegerParam("n.trees", 100, 500),
    ParamHelpers::makeIntegerParam("interaction.depth", 1, 3),
    ParamHelpers::makeIntegerParam("n.minobsinnode", 2, 20),
    ParamHelpers::makeNumericParam("shrinkage", lower=-10,upper=-1, trafo = function(x) 2^x )
  )

  if(method == "negBinDnn") parameter <- ParamHelpers::makeParamSet(
                                              ParamHelpers::makeNumericParam("lr", 0.00001, 0.01),
                                              ParamHelpers::makeIntegerParam("arch",  lower = 5, upper = 50L),
                                              ParamHelpers::makeIntegerParam("nLayer", lower = 1L, upper  = 6L),
                                              ParamHelpers::makeLogicalParam("bias", default = TRUE),
                                              ParamHelpers::makeDiscreteParam(id = "archFunction", values = c("negExp", "continous")),
                                              ParamHelpers::makeIntegerParam(id = "batch", lower = 5, upper = 200),
                                              ParamHelpers::makeDiscreteParam(id = "opti", values = c("rmsprop", "adam")),
                                              ParamHelpers::makeDiscreteParam(id = "distribution", values = c("poisson", "negBin")),
                                              ParamHelpers::makeLogicalParam(id = "Link", default = TRUE)

  )
  if(method == "multiNomDnn") parameter <- ParamHelpers::makeParamSet(
                                                  ParamHelpers::makeNumericParam("lr", 0.0001, 0.01),
                                                  ParamHelpers::makeIntegerParam("arch",  lower = 5, upper = 30),
                                                  ParamHelpers::makeNumericParam("drop",  lower = 0.2, upper = 0.5),
                                                  ParamHelpers::makeIntegerParam("nLayer", lower = 1L, upper  = 5L),
                                                  ParamHelpers::makeLogicalParam("bias", default = TRUE),
                                                  ParamHelpers::makeDiscreteParam(id = "archFunction", values = c("negExp", "continous")),
                                                  ParamHelpers::makeIntegerParam(id = "batch", lower = 1, upper = 30),
                                                  ParamHelpers::makeDiscreteParam(id = "opti", values = c("rmsprop", "adam")),
                                                  ParamHelpers::makeLogicalParam(id = "correct", default = FALSE)

  )
  if(method == "RFsrc") parameter = ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam("mtry" , lower = 2,upper = length(settings$fitVariables) - 1 ),
                                                               ParamHelpers::makeIntegerParam("nodesize",lower = 2,upper = 50),
                                                               ParamHelpers::makeIntegerParam("nodedepth",lower = 2,upper = 50)
                                                               )

  if(method == "RF") parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam("mtry",lower = 2,upper = length(settings$fitVariables) - 1),         #Parameter zu tunen
                                                             ParamHelpers::makeIntegerParam("nodesize",lower = 2,upper = 50),
                                                             ParamHelpers::makeLogicalParam("replace",default = TRUE))

  if(method == "SVM") parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeDiscreteParam("kernel", values = c("polynomial", "radial", "sigmoid")),
                                                              ParamHelpers::makeNumericParam("cost",lower=-5,upper=2, trafo = function(x) 2^x),
                                                              ParamHelpers::makeNumericParam("gamma",lower=1,upper=20)
  )


  if(method == "knn") parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam("k", 1, 15),
                                                              ParamHelpers::makeDiscreteParam("kernel", values = c("rectangular", "triangular", "epanechnikov", "optimal")))


  if(method == "naive") parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("laplace", 0, 6))


  if(method == "dnn") {
    if(settings$balanceClasses != "Regression") parameter <- ParamHelpers::makeParamSet(
                                                              ParamHelpers::makeNumericParam("lr", 0.0001, 0.01),
                                                                ParamHelpers::makeIntegerParam("arch",  lower = 5, upper = 50L),
                                                                ParamHelpers::makeNumericParam("drop",  lower = 0.2, upper = 0.5),
                                                                ParamHelpers::makeIntegerParam("nLayer", lower = 1L, upper  = 5L),
                                                              ParamHelpers::makeLogicalParam("bias", default = TRUE),
                                                                ParamHelpers::makeDiscreteParam("activation", values = c("relu", "elu", "LeakyRelu")),
                                                                ParamHelpers::makeDiscreteParam(id = "archFunction", values = c("negExp", "continous")),
                                                              ParamHelpers::makeIntegerParam(id = "batch", lower = 30, upper = 100),
                                                              ParamHelpers::makeNumericParam(id = "decay", lower = 0.9,  0.99),
                                                              ParamHelpers::makeNumericParam(id = "alpha", lower = 0.05,  0.3),
                                                              ParamHelpers::makeDiscreteParam(id = "opti", values = c("sgd", "adamax"))

                                                                )
    else parameter <- ParamHelpers::makeParamSet(
                                                  ParamHelpers::makeNumericParam("lr", 0.0001, 0.01),
                                                  ParamHelpers::makeIntegerParam("arch",  lower = 5L, upper = 30L),
                                                  ParamHelpers::makeNumericParam("drop",  lower = 0.2, upper = 0.5),
                                                  ParamHelpers::makeIntegerParam("nLayer", lower = 1L, upper  = 5L),
                                                  ParamHelpers::makeLogicalParam("bias", default = TRUE),
                                                  ParamHelpers::makeDiscreteParam(id = "archFunction", values = c("negExp", "continous")),
                                                  ParamHelpers::makeIntegerParam(id = "batch", lower = 5, upper = 50),
                                                  ParamHelpers::makeDiscreteParam(id = "opti", values = c("sgd", "adamax", "rmsprop")),
                                                  ParamHelpers::makeLogicalParam(id = "logTarget", default = FALSE)
                                                )
  }
  if(method == "cnn") parameter <- ParamHelpers::makeParamSet(
                                                              ParamHelpers::makeNumericParam("lr",lower =  0.0001, upper =  0.01),
                                                                ParamHelpers::makeIntegerParam("arch",  lower = 10, upper = 80),
                                                                ParamHelpers::makeNumericParam("drop",  lower = 0.2, upper = 0.5),
                                                                ParamHelpers::makeIntegerParam("filter", lower = 8,upper = 30),
                                                                ParamHelpers::makeLogicalParam("bias", default = TRUE),
                                                                ParamHelpers::makeDiscreteParam(id = "pool", values = c("max", "average")),
                                                              ParamHelpers::makeIntegerParam(id = "nLayer", lower = 1L, upper  = 1L),
                                                              ParamHelpers::makeIntegerParam(id = "nConv", lower = 1, upper = 2),
                                                              ParamHelpers::makeDiscreteParam("activation", values = c("relu", "elu")),
                                                              ParamHelpers::makeDiscreteParam(id = "archFunction", values = c("negExp",  "continous")),
                                                              ParamHelpers::makeIntegerParam(id = "batch", lower = 30, upper = 100),
                                                              ParamHelpers::makeNumericParam(id = "decay", lower = 0.9,  0.99),
                                                              ParamHelpers::makeDiscreteParam(id = "opti", values = c("sgd", "adamax"))
                                                              )
  if(method == "preDnn")parameter <- ParamHelpers::makeParamSet(
                                                                ParamHelpers::makeNumericParam("lr", 0.0001, 0.01),
                                                                ParamHelpers::makeLogicalParam("bias", default = TRUE),
                                                                    ParamHelpers::makeIntegerParam("arch",  lower = 5, upper = 50),
                                                                    ParamHelpers::makeIntegerParam("nLayer", lower = 1L, upper  = 3L),
                                                                    ParamHelpers::makeDiscreteParam("activation", values = c("relu", "elu", "LeakyRelu")),
                                                                   # ParamHelpers::makeDiscreteParam(id = "archFunction", values = c("negExp", "parable", "continous")),
                                                                ParamHelpers::makeIntegerParam(id = "batch", lower = 10, upper = 50),
                                                                ParamHelpers::makeNumericParam(id = "decay", lower = 0.9,  0.99),
                                                                ParamHelpers::makeNumericParam(id = "alpha", lower = 0.05,  0.3)
  )
  if(method == "wideDeep") parameter <- ParamHelpers::makeParamSet(      ParamHelpers::makeIntegerParam(id = "arch", lower = 10, upper = 200L),
                                                                         ParamHelpers::makeIntegerParam(id = "nLayer", lower = 2L, upper = 6L),
                                                                         ParamHelpers::makeIntegerParam(id = "embedding", lower = 3, upper = 50),
                                                                         ParamHelpers::makeDiscreteParam(id = "archFunction", values = c("negExp", "parable", "continous")))


  if(method == "RFranger") parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam("mtry",lower = 2,upper = length(settings$fitVariables) - 1),         #Parameter zu tunen
                                                                   ParamHelpers::makeIntegerParam("min.node.size",lower = 2,upper = 50),
                                                                   ParamHelpers::makeLogicalParam("replace",default = TRUE))


  if(method == "boost") parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeDiscreteParam("booster", values = c("gbtree", "dart")),
                                                                ParamHelpers::makeDiscreteParam("sample_type", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
                                                                ParamHelpers::makeDiscreteParam("normalize_type", values = c("tree", "forest"), requires = quote(booster == "dart")),
                                                                ParamHelpers::makeNumericParam("eta", lower = 0.01, upper = 0.5),
                                                                ParamHelpers::makeNumericParam("gamma", lower = -9, upper = 5, trafo = function(x) 2^x),
                                                                ParamHelpers::makeIntegerParam("max_depth", lower = 1, upper = 10),
                                                                ParamHelpers::makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x),
                                                                ParamHelpers::makeNumericParam("alpha", lower = -10, upper = 5, trafo = function(x) 2^x),
                                                                ParamHelpers::makeNumericParam("min_child_weight", 0, 10),
                                                                ParamHelpers::makeIntegerParam("nrounds", lower = 1, upper = 500),
                                                                ParamHelpers::makeNumericParam("rate_drop", lower = 0, upper = 0.2, requires = quote(booster == "dart")),
                                                                ParamHelpers::makeNumericParam("skip_drop", lower = 0, upper = 0.3, requires = quote(booster == "dart")))


  if(method == "knnFS")parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam("fw.abs", lower = 1, upper = sum(settings$split)),
                                                               ParamHelpers::makeDiscreteParam("fw.method", values = c("kruskal.test", "randomForest.importance")),
                                                               ParamHelpers::makeIntegerParam("k", 1, 15),
                                                               ParamHelpers::makeDiscreteParam("kernel", values = c("rectangular", "triangular", "epanechnikov", "optimal")))

  if(method == "cforest") parameter <- ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam("mtry",lower = 2,upper = sum(settings$split) - 1),
                                                                  ParamHelpers::makeIntegerParam("minsplit",lower = 5,upper = 50))


  if(!is.null(extra)){
    for(n in names(extra))parameter$pars[[n]] = NULL
  }
  balanceClasses <- settings$balanceClasses

  #update params
  if(!is.null(modelSettings) && "tune" %in% names(modelSettings[[method]])) parameter$pars <- parameter$pars[modelSettings[[method]]$tune]


    if(balanceClasses == "Over"){
      parameter$pars[["osw.rate"]] <- ParamHelpers::makeNumericParam("osw.rate", lower = 4, upper = 10)
      return(parameter)
      }
    else if(balanceClasses == "Under") {
      parameter$pars[["usw.rate"]] <- ParamHelpers::makeNumericParam("usw.rate", lower = 0, upper = 1)
      return(parameter)
    }
    else if(balanceClasses == "SMOTE") {
      {
        parameter$pars[["sw.rate"]] <- ParamHelpers::makeNumericParam("sw.rate", lower = 2, upper = 5)
        parameter$pars[["sw.nn"]]   <- ParamHelpers::makeIntegerParam("sw.nn", lower = 3, upper = 5)
        return(parameter)
      }
    } else if(balanceClasses == "OverBagging"){
        parameter$pars[["obw.iters"]]  <- ParamHelpers::makeIntegerParam("obw.iters", lower = 5, upper = 40)
        parameter$pars[["obw.rate"]]   <- ParamHelpers::makeNumericParam("obw.rate", lower = 1, upper = 10)
        return(parameter)

    } else if(balanceClasses == "WeightedClasses"){
      if(!method %in% c("knn", "knnFS", "naive")) {
       parameter$pars[["wcw.weight"]] <- ParamHelpers::makeNumericParam("wcw.weight", lower = 1.3, upper = 10)
       return(parameter)
      }
      else{
        stop("Error. Selected algorithms are not compartible with Weighted Classes",call. = F)
        tmp <- settings
        tmp$balanceClasses <- "Over"
        return(getPars(method, tmp))
        }
    } else if(balanceClasses == "Over+WC"){
      if(!method %in% c("knn", "knnFS", "naive"))
        parameter$pars[["wcw.weight"]] <- ParamHelpers::makeNumericParam("wcw.weight", lower = 1.3, upper = 10)
      else stop("Error. Selected algorithms are not compartible with Weighted Classes",call. = F)
      parameter$pars[["osw.rate"]] <- ParamHelpers::makeNumericParam("osw.rate", lower = 4, upper = 10)
      return(parameter)
    }
    else return(parameter)

}

