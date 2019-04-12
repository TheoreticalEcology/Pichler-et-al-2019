#' @export
makeRLearner.classif.binomial_self = function() {
  makeRLearnerClassif(
    cl = "classif.binomial_self",
    package = "stats",
    par.set = makeParamSet(
      makeDiscreteLearnerParam("link", values = c("logit", "probit", "cloglog", "cauchit", "log"),
                               default = "logit"),
      makeLogicalLearnerParam("model", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam("secondOrderInteractions", default = TRUE)
    ),
    par.vals = list(
      model = FALSE
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "weights"),
    name = "Binomial Regression",
    short.name = "binomial_self",
    note = "Delegates to `glm` with freely choosable binomial link function via learner parameter `link`. We set 'model' to FALSE by default to save memory.",
    callees = c("glm", "binomial")
  )
}

#' @export
trainLearner.classif.binomial_self = function(.learner, .task, .subset, .weights = NULL, link = "logit", secondOrderInteractions = TRUE, ...) {
  data = mlr::getTaskData(.task, .subset)
  target = mlr::getTaskTargetNames(.task)
  if(secondOrderInteractions) f = formula(paste0(target,"~."))
  else f = formula(paste0(target,"~.^2"))
  m = stats::glm(f, data = data, family = stats::binomial(link = link), weights = .weights, ...)
  return(m)
}

#' @export
predictLearner.classif.binomial_self = function(.learner, .model, .newdata, ...) {
  require(MASS)
  x = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  levs = .model$task.desc$class.levels
  if (.learner$predict.type == "prob") {
    mlr:::propVectorToMatrix(x, levs)
  } else {
    levs = .model$task.desc$class.levels
    p = as.factor(ifelse(x > 0.5, levs[2L], levs[1L]))
    unname(p)
  }
}
