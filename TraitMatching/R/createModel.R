#' Create Model
#'
#' @param interactionList permuted data with binary classification for interaction
#' @param method Method to be used. RF, kNN, SVM or (deep)neural net. RF is default
#' @param parallel boolean. Parallel computing. Default = T
#' @param tune boolean. Model is tuned by default = T
#' @param target Default
#'
#'
#' @export





createModel <- function(interactionList, method = "RF_random", parallel = T,  tune = "random", target = NULL, iters = 5, tuning_metric = "auc"){
  result <- list()
  if(!(method == "RF_random" | method == "SVM" | method == "kknn" | method == "naive" | method == "kknn_FS" | method == "keras" | method == "ranger" | method == "boost" | method == "keras_seq"| method == "keras_conv")) stop("Provide one of the known methods")
  if(!is.list(interactionList) | length(interactionList) != 3) stop("Provide List with interactionlist, target and split")

  if(is.null(target)) target <- interactionList$target

  if(is.logical(parallel) && parallel == F) cp <- 1
  if(is.logical(parallel) && parallel == T) cp <- parallel::detectCores()
  if(is.numeric(parallel)) cp <- parallel

  measure_list <- list(
        mcc = mlr::mcc,
        auc = mlr::auc,
        bac = mlr::bac,
        ber = mlr::ber,
        brier.scaled = mlr::brier.scaled,
        f1  = mlr::f1,
        gpr = mlr::gpr,
        kappa = mlr::kappa,
        logloss = mlr::logloss,
        mcc = mlr::mcc,
        ppv = mlr::ppv,
        ssr = mlr::ssr,
        tpr = mlr::tpr,
        wkappa = mlr::wkappa
  )


  if(method == "naive" | method =="SVM" | method == "Deep" | method == "RF_random" | method == "kknn" | method == "kknn_FS" | method == "boost" | method == "keras" | method == "ranger" | method == "keras_seq" | method == "keras_conv"){
    # Mlr controls:
    task          <- mlr::makeClassifTask(data = interactionList$interactionlist[,3:ncol(interactionList$interactionlist)], target = target, positive = "X", fixup.data = "no")
    task          <- mlr::normalizeFeatures(task, method = "standardize")
    inner_ctrl    <- mlr::makeResampleDesc("CV", iters = 3, stratify = T)
    outer_ctrl    <- mlr::makeResampleDesc("CV", iters = 10, stratify = T)

    if(method != "Deep" | method != "RFh2o" ) parallelMap::parallelStartSocket(cpus = cp, level = "mlr.tuneParams")
    if(method == "naive"){
      learner_name <- "classif.naiveBayes"
      learner      <- mlr::makeLearner("classif.naiveBayes", predict.type = "prob")


      learner      <- mlr::makeSMOTEWrapper(learner)
      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("sw.rate", 2, 10),
                                                 ParamHelpers::makeIntegerParam("sw.nn", 2, 10),
                                                 ParamHelpers::makeNumericParam("laplace", 0, 6)
                                                 )
    }
    if(method == "RF_random"){
      learner_name <- "classif.randomForest"
      learner      <- mlr::makeLearner("classif.randomForest", predict.type = "prob", par.vals = list("importance" = TRUE))
      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam("mtry",lower = 2,upper = (ncol(interactionList$interactionlist) - 3)),         #Parameter zu tunen
                                                 ParamHelpers::makeIntegerParam("ntree",lower = 300,upper = 700),
                                                 ParamHelpers::makeIntegerParam("nodesize",lower = 2,upper = 50),
                                                 ParamHelpers::makeLogicalParam("replace",default = TRUE),
                                                 ParamHelpers::makeNumericVectorParam("classwt", len = 2, lower = 0, upper = 5)
      )
    }
    if(method == "ranger"){
      learner <- mlr::makeLearner("classif.ranger", predict.type = "prob")
      learner <- mlr::makeWeightedClassesWrapper(learner)
      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeIntegerParam("mtry",lower = 2,upper = (ncol(interactionList$interactionlist) - 3)),         #Parameter zu tunen
                                                 ParamHelpers::makeIntegerParam("num.trees",lower = 300,upper = 700),
                                                 ParamHelpers::makeIntegerParam("min.node.size",lower = 2,upper = 50),
                                                 ParamHelpers::makeLogicalParam("replace",default = TRUE),
                                                 ParamHelpers::makeNumericVectorParam("wcw.weight", len = 1, lower = 0, upper = 5)
      )


    }

    if(method == "SVM"){
      learner_name <- "classif.ksvm"


      learner <- mlr::makeLearner("classif.svm", predict.type = "prob")
      learner <- mlr::makeSMOTEWrapper(learner)
      parameterSet<- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("sw.rate", 2, 50),
                                                ParamHelpers::makeIntegerParam("sw.nn", 1, 15),
                                                ParamHelpers::makeLogicalParam("sw.standardize", default = T),
                                                ParamHelpers::makeNumericParam("gamma",lower = 0,upper=2),
                                                ParamHelpers::makeNumericParam("cost",lower=0,upper=2),
                                                ParamHelpers::makeNumericParam("tolerance", 0, 0.5),
                                                ParamHelpers::makeDiscreteParam("kernel", values = c("radial", "sigmoid")))

    }
    if(method == "Deep"){
      h2o::h2o.init(nthreads = cp, max_mem_size = "4G", enable_assertion = F)
      learner_name <- "classif.h2o.deeplearning"
      learner <- mlr::makeLearner("classif.h2o.deeplearning", predict.type = "prob",
                                  par.vals = list(l1 = 1e-5, max_w2 = 10))
      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("l1", lower = 1e-6, upper = 1e-4),
                                                 ParamHelpers::makeNumericParam("max_w2", lower = 1, upper = 9),
                                                 ParamHelpers::makeIntegerVectorParam("hidden", len = 2, lower = 5, upper = 25),
                                                 ParamHelpers::makeNumericParam("epochs", lower = 10, upper = 150),
                                                 ParamHelpers::makeNumericParam("rho", lower = 0.4, upper = 2),
                                                 ParamHelpers::makeLogicalParam("balance_classes", default = F),
                                                 #ParamHelpers::makeNumericParam("class_sampling_factors", lower = -5, upper = 5),
                                                 ParamHelpers::makeDiscreteParam("activation", values = c("Tanh", "Maxout"))
                                                 )
    }
    if(method == "kknn"){
        learner <- mlr::makeSMOTEWrapper(mlr::makeLearner("classif.kknn", predict.type = "prob"))
        parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("sw.rate", 1, 50),
                                                   ParamHelpers::makeIntegerParam("sw.nn", 1, 15),
                                                   ParamHelpers::makeLogicalParam("sw.standardize", default = T),
                                                   ParamHelpers::makeIntegerParam("k", 1, 15),
                                                   ParamHelpers::makeDiscreteParam("kernel", values = c("rectangular", "triangular", "epanechnikov", "optimal"))
                                                   )
    }
    if(method == "keras"){
      # parallelMap::parallelStop()
      # Get dummy split for plants and insect
      parallelMap::parallelExport("trainLearner.classif.keras", "predictLearner.classif.keras", level = "mlr.tuneParams")
      p <- interactionList$interactionlist[, 3 : (interactionList$split[1] + 2)]
      factor   <- sapply(p, is.factor)
      t        <- length(factor[factor == FALSE])
      p_factor <- p[, factor]
      p_levels <- as.data.frame(unclass(summary(sapply(p_factor, levels))))
      p_cols <- sum(as.numeric(levels(p_levels$Length))[p_levels$Length]) + t


      i <- interactionList$interactionlist[, (interactionList$split[1] + 3) : (ncol(interactionList$interactionlist) - 1)]
      factor   <- sapply(i, is.factor)
      t        <- length(factor[factor == FALSE])
      i_factor <- i[, factor]
      i_levels <- as.data.frame(unclass(summary(sapply(i_factor, levels))))
      i_cols <- sum(as.numeric(levels(i_levels$Length))[i_levels$Length]) + t




      parallelMap::parallelExport("trainLearner.classif.keras", "predictLearner.classif.keras", level = "mlr.tuneParams")
      learner <- mlr::makeOversampleWrapper(mlr::makeLearner("classif.keras", predict.type = "prob",split = c(p_cols, i_cols)))
      #learner <- mlr::makeLearner("classif.keras", predict.type = "prob",split = c(p_cols, i_cols))

      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("osw.rate", 2, 8),
                                                 ParamHelpers::makeNumericParam("classwt",  lower = 2, upper = 5),
                                                 ParamHelpers::makeIntegerParam("epochs", lower = 20, 60),
                                                 ParamHelpers::makeIntegerVectorParam(id = "A_arch", lower = 10, upper = 30, len = 2),
                                                 ParamHelpers::makeIntegerParam(id = "B_arch", lower = 10, upper = 30),
                                                 ParamHelpers::makeIntegerVectorParam(id = "C_arch", lower = 10, upper = 30, len = 2),
                                                 ParamHelpers::makeDiscreteParam("loss", values = c("categorical_hinge", "categorical_crossentropy", "binary_crossentropy", "kullback_leibler_divergence"))
      )
    }
    if(method == "keras_conv"){

      parallelMap::parallelExport("trainLearner.classif.keras_conv", "predictLearner.classif.keras_conv", level = "mlr.tuneParams_conv")
      #parallelMap::parallelStop()
      learner <- mlr::makeOversampleWrapper(mlr::makeLearner("classif.keras_conv", predict.type = "prob"))
      #learner <- mlr::makeLearner("classif.keras", predict.type = "prob",split = c(p_cols, i_cols))

      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("osw.rate", 2, 8),
                                                 ParamHelpers::makeNumericParam("classwt",  lower = 2, upper = 5),
                                                 ParamHelpers::makeIntegerParam("epochs", lower = 20, 60),
                                                 ParamHelpers::makeIntegerVectorParam("arch", len = 2, lower = 10, upper = 30),
                                                 ParamHelpers::makeIntegerParam("filters", lower = 3, 20),
                                                 ParamHelpers::makeDiscreteParam("loss", values = c("categorical_hinge", "categorical_crossentropy", "binary_crossentropy", "kullback_leibler_divergence"))
      )
    }
    if(method == "keras_seq"){
      parallelMap::parallelExport("trainLearner.classif.keras_seq", "predictLearner.classif.keras_seq", level = "mlr.tuneParams")

      #parallelMap::parallelStop()
      learner <- mlr::makeOversampleWrapper(mlr::makeLearner("classif.keras_seq", predict.type = "prob"))
      #learner <- mlr::makeLearner("classif.keras", predict.type = "prob",split = c(p_cols, i_cols))

      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("osw.rate", 2, 8),
                                                 ParamHelpers::makeNumericParam("classwt",  lower = 2, upper = 5),
                                                 ParamHelpers::makeIntegerParam("epochs", lower = 20, 60),
                                                 ParamHelpers::makeIntegerVectorParam("arch", len = 4, lower = 10, upper = 30),
                                                 ParamHelpers::makeDiscreteParam("loss", values = c("categorical_hinge", "categorical_crossentropy", "binary_crossentropy", "kullback_leibler_divergence"))
      )
    }
    if(method == "kknn_FS"){
      learner <- mlr::makeSMOTEWrapper(mlr::makeLearner("classif.kknn", predict.type = "prob"))
      learner <- mlr::makeFilterWrapper(learner)
      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("sw.rate", 1, 50),
                                                 ParamHelpers::makeIntegerParam("sw.nn", 1, 15),
                                                 ParamHelpers::makeLogicalParam("sw.standardize", default = T),
                                                 ParamHelpers::makeIntegerParam("fw.abs", lower = 1, upper = (ncol(interactionList$interactionlist) - 3) ),
                                                 ParamHelpers::makeDiscreteParam("fw.method", values = c("kruskal.test", "randomForest.importance")),
                                                 ParamHelpers::makeIntegerParam("k", 1, 15),
                                                 ParamHelpers::makeDiscreteParam("kernel", values = c("rectangular", "triangular", "epanechnikov", "optimal"))
      )
    }
    if(method == "boost")
    {
      learner <- mlr::makeLearner("classif.xgboost", predict.type = "prob")
      task    <- mlr::createDummyFeatures(task)
      parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeNumericParam("eta", 0.1, 0.99),
                                                 ParamHelpers::makeNumericParam("gamma", 0, 3),
                                                 ParamHelpers::makeIntegerParam("max_depth", 1, 10),
                                                 ParamHelpers::makeNumericParam("min_child_weight", 0, 10),
                                                 ParamHelpers::makeNumericParam("subsample", 0, 1),
                                                 ParamHelpers::makeNumericParam("colsample_bytree", 0.1 , 1),
                                                 ParamHelpers::makeIntegerParam("num_parallel_tree", 0, 5)
      )
    }


    if(tune == "random"){
      tune_ctrl  <- mlr::makeTuneControlRandom(maxit = iters, tune.threshold = T)
      learner    <- mlr::makeTuneWrapper(learner = learner, resampling = inner_ctrl, par.set = parameterSet, control = tune_ctrl, measures = measure_list[[tuning_metric]])
      result     <- mlr::resample(learner, task, outer_ctrl, measures = list(mlr::auc, mlr::bac, mlr::acc, mlr::f1, measure_list[[tuning_metric]]), models = TRUE, extract = mlr::getTuneResult)

    }
    if(tune == "multi"){
      control <-  mlr::makeTuneMulttaiCritControlRandom(maxit = 50)
      result  <-  mlr::tuneParamsMultiCrit(learner, task, resampling = inner_ctrl, par.set = parameterSet, control = control, measures = list(mlr::tpr, mlr::acc))
    }
    if(tune == "MBO"){
      learner      <- mlr::makeTuneWrapper(learner = learner, resampling = inner_ctrl, par.set = parameterSet, control = mlr::makeTuneControlMBO(), measures = measures)
      result       <- mlr::resample(learner, task, outer_ctrl, measures = list(mlr::wkappa, mlr::bac, mlr::acc, mlr::tpr), models = TRUE, extract = getTuneResult)
    }
    parallelMap::parallelStop()
    if(method == "Deep") h2o::h2o.shutdown()
  }

  #result$task   <- task

  return(result)
}





#' Find best Model of all available Models
#'
#' @param interactionList permuted data with binary classification for interaction
#' @param parallel boolean. Parallel computing. Default = T
#' @param tune boolean. Model is tuned by default = T
#' @param target Default
#'
#'
#' @export





findBest <- function(interactionList, parallel = T, tune = "random", target = NULL, iters = 100){
  out <- list()
  if(is.null(target)) target <- interactionList$target



  mls <- list(
    mlr::makeLearner("classif.randomForest"),
    mlr::makeSMOTEWrapper(mlr::makeFilterWrapper(mlr::makeLearner("classif.ksvm"))),
    mlr::makeSMOTEWrapper(mlr::makeFilterWrapper(mlr::makeLearner("classif.kknn")))
  )
  learner <- mlr::makeModelMultiplexer(mls)
  parameterSet <- ParamHelpers::makeParamSet(ParamHelpers::makeDiscreteParam("selected.learner", values = c("classif.randomForest", "classif.ksvm.filtered.smoted")),
                                             ParamHelpers::makeDiscreteParam("classif.ksvm.filtered.smoted.kernel", values = c("vanilladot", "rbfdot", "tanhdot"),
                                                                             requires = quote(selected.learner == "classif.ksvm.filtered.smoted")),
                                             ParamHelpers::makeDiscreteParam("classif.ksvm.filtered.smoted.type", values = c("C-svc", "c-bsvc", "spoc-svc", "kbb-svc"),
                                                                             requires = quote(selected.learner == "classif.ksvm.filtered.smoted")),
                                             ParamHelpers::makeNumericParam("classif.ksvm.filtered.smoted.class.weights", lower = 0, upper = 5,
                                                                            requires = quote(selected.learner == "classif.ksvm.filtered.smoted")),
                                             ParamHelpers::makeNumericParam("classif.ksvm.filtered.smoted.sw.rate", 1, 50,
                                                                            requires = quote(selected.learner == "classif.ksvm.filtered.smoted")),
                                             ParamHelpers::makeIntegerParam("classif.ksvm.filtered.smoted.sw.nn", 1, 15,
                                                                            requires = quote(selected.learner == "classif.ksvm.filtered.smoted")),
                                             ParamHelpers::makeLogicalParam("classif.ksvm.filtered.smoted.sw.standardize", default = T,
                                                                            requires = quote(selected.learner == "classif.ksvm.filtered.smoted")),
                                             ParamHelpers::makeIntegerParam("classif.ksvm.filtered.smoted.fw.abs", lower = 1, upper = (ncol(interactionList$interactionlist) - 3),
                                                                            requires = quote(selected.learner == "classif.ksvm.filtered.smoted")),
                                             ParamHelpers::makeIntegerParam("classif.randomForest.mtry",lower = 2,upper = (ncol(interactionList$interactionlist) - 3),
                                                                            requires = quote(selected.learner == "classif.randomForest")),         #Parameter zu tunen
                                             ParamHelpers::makeIntegerParam("classif.randomForest.ntree",lower = 300,upper = 700,
                                                                            requires = quote(selected.learner == "classif.randomForest")),
                                             ParamHelpers::makeIntegerParam("classif.randomForest.nodesize",lower = 2,upper = 50,
                                                                            requires = quote(selected.learner == "classif.randomForest")),
                                             ParamHelpers::makeLogicalParam("classif.randomForest.replace",default = TRUE,
                                                                            requires = quote(selected.learner == "classif.randomForest")),
                                             ParamHelpers::makeNumericVectorParam("classif.randomForest.classwt", len = 2, lower = 0, upper = 5,
                                                                                  requires = quote(selected.learner == "classif.randomForest")),
                                             ParamHelpers::makeLogicalParam("classif.randomForest.localImp", default = F,
                                                                            requires = quote(selected.learner == "classif.randomForest")),
                                             ParamHelpers::makeNumericParam("classif.kknn.filtered.smoted.sw.rate", 1, 50,
                                                                            requires = quote(selected.learner == "classif.kknn.filtered.smoted")),
                                             ParamHelpers::makeIntegerParam("classif.kknn.filtered.smoted.sw.nn", 1, 15,
                                                                            requires = quote(selected.learner == "classif.kknn.filtered.smoted")),
                                             ParamHelpers::makeLogicalParam("classif.kknn.filtered.smoted.sw.standardize", default = T,
                                                                            requires = quote(selected.learner == "classif.kknn.filtered.smoted")),
                                             ParamHelpers::makeIntegerParam("classif.kknn.filtered.smoted.fw.abs", lower = 1, upper = (ncol(interactionList$interactionlist) - 3),
                                                                            requires = quote(selected.learner == "classif.kknn.filtered.smoted") ),
                                             ParamHelpers::makeIntegerParam("classif.kknn.filtered.smoted.k", 1, 15,
                                                                            requires = quote(selected.learner == "classif.kknn.filtered.smoted")),
                                             ParamHelpers::makeDiscreteParam("classif.kknn.filtered.smoted.kernel", values = c("rectangular", "triangular", "epanechnikov", "optimal"),
                                                                             requires = quote(selected.learner == "classif.kknn.filtered.smoted"))

                                            )

  # Mlr controls:
  task          <- mlr::makeClassifTask(data = interactionList$interactionlist[,3:ncol(interactionList$interactionlist)], target = target, positive = "X1")
  task          <- mlr::normalizeFeatures(task, method = "standardize")

  # Nested Resampling Control:
  inner_ctrl    <- mlr::makeResampleDesc("CV", iters = 3, stratify = T)
  outer_ctrl    <- mlr::makeResampleDesc("Subsample", iters = 3, stratify = T)

  # parallel

  if(is.logical(parallel) && parallel == F) cp <- 1
  if(is.logical(parallel) && parallel == T) cp <- parallel::detectCores()
  parallelMap::parallelStartSocket(cpus = cp)
  if(tune == "random"){
    tune_ctrl  <- mlr::makeTuneControlRandom(maxit = iters, tune.threshold = TRUE)
    learner    <- mlr::makeTuneWrapper(learner = learner, resampling = inner_ctrl, par.set = parameterSet, control = tune_ctrl, measures = list(mlr::acc, mlr::tpr, mlr::bac, mlr::kappa, mlr::f1))
    result     <- mlr::resample(learner, task, outer_ctrl, measures = list(mlr::mcc, mlr::bac, mlr::acc, mlr::tpr, mlr::f1), models = TRUE, extract = getTuneResult)
    return(result)

  }
  if(tune == "multi"){

    control <-  mlr::makeTuneMultiCritControlRandom(maxit = 50)
    result  <-  mlr::tuneParamsMultiCrit(learner, task, resampling = inner_ctrl, par.set = parameterSet, control = control, measures = list(mlr::tpr, mlr::acc, GAT))
    return(result)
  }

  parallelMap::parallelStop()

}





#
# GAT.fun <- function(task, model, pred, feats, extra.args){
#
#   response <- mlr::getPredictionResponse(pred)
#   truth    <- mlr::getPredictionTruth(pred)
#   pos      <- pred$task.desc$positive
#
#   r.tpr    <- mlr::measureTPR(truth, response, pos)
#   r.acc    <- mlr::measureACC(truth, response)
#
#   x <- r.tpr / r.acc
#   y <- (1 / sqrt(2 * pi * (1 / (2 * pi)))) * exp(- 6 * (x - 1) ^ 2 / sqrt(2 * sqrt(2 * 1 / 2 * pi)))
#
#   GAT <- (y + 0.9 * r.tpr + 1.1 * r.acc) / 3
# }
#
# GAT <- mlr::makeMeasure(
#   id = "GAT", name = "GATtest Error",
#   properties = c("classif", "req.pred", "req.truth"),
#   minimize = F, best = 1, worst = 0,
#   fun = GAT.fun
# )


