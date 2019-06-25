#' permutation Random Forest for interaction Analysis
#'
#' @param model Provide Object from createTMmodel
#' @param parallel CPUs to use, default use all available CPU's
#' @param data If no Object from createTMmodel, provide X, Y and interactionlist in a List
#'




permutationRF <- function(model = NULL, parallel = T, data = NULL) {
  #if (is.null(model) | is.null(data)) stop("Provide Result from createTMmpdel or Data")
  out <- list()
  if (is.null(model)) {
    if(is.null(data)) stop("Pls provide list of data with X, Y, Z")
    model <- TM(data[[1]],  data[[2]],data[[3]], method = "RF_random", parallel = parallel, iters = 10, tuning_metric = "GAT")
  }




  if (is.logical(parallel) && parallel == F) cp <- 1
  if (is.logical(parallel) && parallel == T) cp <- parallel::detectCores()
  if (is.numeric(parallel)) cp <- parallel
  parallelMap::parallelStartSocket(cpus = cp, level = "mlr.resample")

  data  <- model$InteractionList
  split <- model$split
  df <- data[, 3:ncol(data)]

  ctrl <- mlr::makeResampleDesc("CV", iters = 2, stratify = T)
  result <- data.frame()
  df <- data[, 3:ncol(data)]
  target <- model$Result[[1]]$task.desc$target
  positive <- model$Result[[1]]$task.desc$positive


  learners <- list()
  for(i in 1:length(model$Result)){
    learners[[i]] <- model$Result[[i]]$models[[1]]$learner$next.learner
  }

 # prf <- function(x, ctrl, learners, df, split, target, positive){
    #
    # learner <- mlr::setHyperPars(
    #   mlr::makeWeightedClassesWrapper( mlr::makeLearner("classif.ranger", predict.type = "prob")),
    #   par.vals = model$Result[[x]]$models[[1]]$learner.model$opt.result$x
    # )
    learner <- learners[[x]] #extern
    result <- data.frame()



    #idea: calcuelate d1 and d2

    for (p in 1:split[1]) {
      for (i in (1 + split[1]):(split[1] + split[2])) {
        d1 <- df
        d2 <- df
        d1[d1$target == "X", p] <- sample(df[df$target == "X", p])
        d1[d1$target == "o", p] <- sample(df[df$target == "o", p])
        d1[d1$target == "X", i] <- sample(df[df$target == "X", i])
        d1[d1$target == "o", i] <- sample(df[df$target == "o", i])

        ind <- sample(nrow(df[df$target == "X", c(p, i)]))
        d_t <- df[df$target == "X", c(p, i)]
        d2[d2$target == "X", c(p, i)] <- d_t[ind, ]

        ind <- sample(nrow(df[df$target == "o", c(p, i)]))
        d_t <- df[df$target == "o", c(p, i)]
        d2[d2$target == "o", c(p, i)] <- d_t[ind, ]


        d1_task <- mlr::makeClassifTask(data = d1, target = target, positive = positive)
        d2_task <- mlr::makeClassifTask(data = d2, target = target, positive = positive)

        r_1 <- mlr::resample(learner, d1_task, resampling = ctrl, measures = list(mlr::mmce, mlr::auc, mlr::acc))
        r_2 <- mlr::resample(learner, d2_task, resampling = ctrl, measures = list(mlr::mmce, mlr::auc, mlr::acc))

        deltaE <- r_1$aggr[1] - r_2$aggr[1]
        together <- paste(colnames(df)[i], "*", colnames(df)[p])
        result <- rbind(result, data.frame(together, deltaE))
      }
    }
    rownames(result) <- c(1:nrow(result))
    result     <- result[order(result$deltaE, decreasing = T), ]
    rownames(result) <- c(1:nrow(result))
    return(result)
 # }

  # parallelMap::parallelStartSocket(cpus = 3)
  # parallelMap::parallelLibrary("mlr")
  # out <- parallelMap::parallelMap(1:length(model$Result), fun = prf, more.args = list(ctrl = ctrl, learners = learners, df = df, split = split, target = target, positive = positive))


  out$result <- result
  out$variables <- colnames(model$InteractionList[, 3:ncol(model$InteractionList)])
  out$split  <- model$split
  class(out) <- "PermutationRF"
  return(out)
}










