#' Keras learner
#'
#'
#' @export

makeRLearner.classif.keras <- function(){
  mlr::makeRLearnerClassif(
    cl = "classif.keras",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericLearnerParam(id = "classwt", lower = 1, default = 1),
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 0, default = 50L),
      ParamHelpers::makeDiscreteLearnerParam(id = "loss", values = c("categorical_hinge", "categorical_crossentropy", "binary_crossentropy", "kullback_leibler_divergence"),default = "categorical_crossentropy"),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c("rmsprop", "adamax"), default = "rmsprop"),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "split", lower = 0, default = 0),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "A_arch", lower = 10, default = c(20, 20)),
      ParamHelpers::makeIntegerLearnerParam(id = "B_arch", lower = 10, default = 20),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "C_arch", lower = 10, default = c(20, 20))
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "class.weights"),
    class.weights.param = "classwt",
    name       = "Keras DNN network",
    short.name = "keras",
    note       = "test...."


  )
}


#' Keras trainer
#'
#' @export

trainLearner.classif.keras <- function(.learner, .task, .subset, .weights = NULL, classwt = NULL, epochs = 50, loss = "categorical_crossentropy", opti = "adamax", split = c(20, 20),A_arch = c(20, 20),B_arch = 20,C_arch = c(20, 20), ...){
  data <- mlr::getTaskData(.task, .subset)
  split<- .learner$par.vals$split
  # X1 <- data[,1:split[1]]
  # X2 <- data[,(split[1]+1):(ncol(data) - 1)]
  #
  t <- mlr::createDummyFeatures(data[,1:(ncol(data) - 1)])
  t <- cbind(t, data[, ncol(data)])


  # X1 <- data[,1:split[1]]
  # X2 <- data[,(split[1]+1):(ncol(data) - 1)]
  # X1 <- as.matrix(mlr::createDummyFeatures(X1))
  # X2 <- as.matrix(mlr::createDummyFeatures(X2))


  levs <- levels(data[, mlr::getTaskTargetNames(.task)])
  n <- length(levs)

  colnames(t)[ncol(t)] <- .task$task.desc$target
  data <- t


  X    <- as.matrix(data[,1:(ncol(data) - 1)])


  Y    <- as.matrix(mlr::createDummyFeatures(data[,ncol(data)]))
  if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt)))
    names(classwt) = levs
  #
  k <- keras::backend()
  k$clear_session()



  #
  pl_in   <- keras::layer_input(shape = c(split[1]), name = "pl_in")
  pl_out  <- keras::layer_dropout(keras::layer_dense(keras::layer_dropout(keras::layer_dense(pl_in, units = A_arch[1], activation = "relu", name = "pl_d1"), rate = 0.1), units = B_arch, activation = "relu", name = "pl_d2"), rate = 0.2)

  in_in   <- keras::layer_input(shape = c(split[2]), name = "ins_in")
  in_out  <- keras::layer_dropout(keras::layer_dense(keras::layer_dropout(keras::layer_dense(in_in, units = A_arch[2], activation = "relu", name = "in_d1"), rate = 0.1), units = B_arch, activation = "relu", name = "in_d2"), rate = 0.2)

  plant_m <- keras::layer_dropout(keras::layer_dense(keras::layer_dropout(keras::layer_dense(keras::layer_concatenate(c(pl_out, in_out), name = "conca"), units = C_arch[1], activation = "relu", name = "togeth_d1"), rate = 0.2, name =  "togeth_dropout"), units = C_arch[2], activation = "relu", name = "togeth_d2"), rate = 0.2)

  pred    <- keras::layer_dense(plant_m, units = 2, activation = "sigmoid", name = "togeth_d3")
  model   <- keras::keras_model(inputs = c(pl_in, in_in), outputs = pred)


  # require(dplyr)
  # pl_in <- keras::layer_input(shape = c(ncol(X1)), name = "pl_in")
  # pl_out <- pl_in %>% keras::layer_dense(units = A_arch[1], activation = "relu") %>%
  #                     keras::layer_dropout(rate = 0.2) %>%
  #                     keras::layer_dense(units = A_arch[2], activation = "relu") %>%
  #                     keras::layer_dropout(rate = 0.2)
  #
  #
  # in_in <- keras::layer_input(shape = c(ncol(X2)), name = "ins_in")
  # in_out <- pl_in %>% keras::layer_dense(units = B_arch[1], activation = "relu") %>%
  #                     keras::layer_dropout(rate = 0.2) %>%
  #                     keras::layer_dense(units = B_arch[2], activation = "relu") %>%
  #                     keras::layer_dropout(rate = 0.2)
  #
  # plant_m <- keras::layer_concatenate(c(pl_out, in_out))  %>%
  #               keras::layer_dense(units = C_arch[1], activation = "relu") %>%
  #               keras::layer_dropout(rate = 0.2) %>%
  #               keras::layer_dense(units = C_arch[2], activation = "relu") %>%
  #               keras::layer_dropout(rate = 0.2)
  #
  # pred <- keras::layer_dense(plant_m, units = 2, activation = "sigmoid")


  model   <- keras::keras_model(inputs = c(pl_in, in_in), outputs = pred)


  classwt <- list("0" = 1, "1" = classwt)

  keras::compile(
    model,
    loss = loss,
    optimizer = opti,
    metrics = keras::metric_binary_accuracy
  )
  end <- ncol(X)
  X_1 <- X[,1:split[1]]
  X_2 <- X[,(split[1] +1) : end]
  history <- keras::fit(
    model,
    x = list(X_1, X_2),Y,
    epochs = epochs, batch_size = 25,
    validation_split = 0.2,
    class_weight = classwt#,
  )
  return(keras::serialize_model(model))
}


#'PredictLearner for keras implemantation into mlr
#'
#'@export



## ISSUE: input architecture wird niht übergeben....
predictLearner.classif.keras = function(.learner, .model, .newdata, split = c(20,20), ...){
  data <- .newdata
  m    <- keras::unserialize_model(.model$learner.model)
  t    <- mlr::createDummyFeatures(data[,1:ncol(data)])
  split<- .learner$par.vals$split
  X    <- as.matrix(t)
  end <- ncol(X)
  X_1 <- X[,1:split[1]]
  X_2 <- X[,(split[1] +1) : end]

  # split<- .learner$par.vals$split
  # X1 <- data[,1:split[1]]
  # X2 <- data[,(split[1]+1):(ncol(data))]
  #
  # t <- mlr::createDummyFeatures(data[,1:(ncol(data) - 1)])
  # t <- cbind(t, data[, ncol(data)])
  # X1 <- as.matrix(mlr::createDummyFeatures(X1))
  # X2 <- as.matrix(mlr::createDummyFeatures(X2))

  p = predict(m, x = list(X_1, X_2), type = "prob")
  colnames(p) <- c("o", "X")
  attributes(p) <- list(dim = dim(p),
                        dimnames = list(NULL,c("o", "X")),
                        class = "matrix")
  return(p)


}
#registerS3method("makeRLearner", "classif.keras", makeRLearner.classif.keras)
#registerS3method("trainLearner", "classif.keras", trainLearner.classif.keras)
#registerS3method("predictLearner", "classif.keras", predictLearner.classif.keras)

