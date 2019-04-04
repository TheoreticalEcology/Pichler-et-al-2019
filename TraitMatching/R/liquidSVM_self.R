#' @export
makeRLearner.classif.liquidSVM_self = function() {
  makeRLearnerClassif(
    cl = "classif.liquidSVM_self",
    package = "liquidSVM",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "d", lower = 0L, upper = 7L, tunable = FALSE),
      makeLogicalLearnerParam(id = "scale", default = TRUE),
      makeIntegerLearnerParam(id = "threads", lower = -1L, default = 0),
      makeDiscreteLearnerParam(id = "kernel", default = "gauss_rbf", values = c("gauss_rbf","poisson")),
      makeIntegerLearnerParam(id = "partition_choice", lower = 0L, upper = 6L, default = 0),
      makeIntegerLearnerParam(id = "grid_choice", lower = -2L, upper = 2L),
      makeIntegerLearnerParam(id = "adaptivity_control", lower = 0L, upper = 2L, default = 0),
      makeIntegerLearnerParam(id = "random_seed"),
      makeIntegerLearnerParam(id = "folds", lower = 1, tunable = FALSE),
      makeIntegerLearnerParam(id = "clipping", lower = -1),
      makeIntegerLearnerParam(id = "gamma_steps", lower = 0, requires = quote(is.null(grid_choice) & is.null(gammas))),
      makeNumericLearnerParam(id = "min_gamma", lower = 0, requires = quote(is.null(grid_choice) & is.null(gammas) & min_gamma <= max_gamma)),
      makeNumericLearnerParam(id = "max_gamma", lower = 0, requires = quote(is.null(grid_choice) & is.null(gammas) & min_gamma <= max_gamma)),
      makeNumericVectorLearnerParam(id = "gammas", lower = 0),
      makeIntegerLearnerParam(id = "lambda_steps", lower = 0, requires = quote(is.null(grid_choice) & is.null(lambdas))),
      makeNumericLearnerParam(id = "min_lambda", lower = 0, requires = quote(is.null(grid_choice) & is.null(lambdas) & min_lambda <= max_lambda)),
      makeNumericLearnerParam(id = "max_lambda", lower = 0, requires = quote(is.null(grid_choice) & is.null(lambdas) & min_lambda <= max_lambda)),
      makeNumericVectorLearnerParam(id = "lambdas", lower = 0),
      makeNumericVectorLearnerParam(id = "c_values", lower = 0),
      makeLogicalLearnerParam(id = "useCells", default = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "liquidSVM support vector machine",
    short.name = "liquidSVM_self",
    callees = "liquidSVM_self"
  )
}

#' @export
trainLearner.classif.liquidSVM_self = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data <- mlr::getTaskData(.task, .subset)
  positive = .task$task.desc$positive
  y = data$target
  negative = levels(y)[which(!levels(y)==positive, arr.ind = T)]
  y = as.character(y)
  y[y==positive] = "1"
  y[y==negative] = "0"
  y = as.factor(y)
  data = data[,colnames(data) != .task$task.desc$target]

  liquidSVM::svm(x = data, y = y,  ..., predict.prob = TRUE)
}

#' @export
predictLearner.classif.liquidSVM_self = function(.learner, .model, .newdata, ...) {
  p = predict(.model$learner.model, newdata = .newdata)
  colnames(p) <- c(.model$task.desc$negative, .model$task.desc$positive)
  attributes(p) <- list(dim = dim(p),
                        dimnames = list(NULL,c(.model$task.desc$negative, .model$task.desc$positive)),
                        class = "matrix")
  return(p)
}
