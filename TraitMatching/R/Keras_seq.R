
#' Keras_seq learner
#'
#' @export


makeRLearner.classif.keras_seq <- function(){
  mlr::makeRLearnerClassif(
    cl = "classif.keras_seq",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericLearnerParam(id = "classwt", lower = 1, default = 1),
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 0, default = 50L),
      ParamHelpers::makeDiscreteLearnerParam(id = "loss", values = c("categorical_hinge", "categorical_crossentropy", "binary_crossentropy", "kullback_leibler_divergence"),default = "categorical_crossentropy"),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c("rmsprop", "adamax"), default = "rmsprop"),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "split", lower = 0, default = 0),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "arch", lower = 10, default = c(25, 15, 10, 10))
    ),
    properties = c("twoclass", "numerics", "factors", "prob", "class.weights"),
    class.weights.param = "classwt",
    name       = "Keras DNN network",
    short.name = "keras",
    note       = "test...."


  )
}
#'  Keras_seq train function
#'
#' @export

trainLearner.classif.keras_seq <- function(.learner, .task, .subset, .weights = NULL, classwt = NULL, epochs = 50, loss = "categorical_crossentropy", opti = "adamax", split = c(20, 20), arch = c(25, 15, 10, 10), ...){
  data <- mlr::getTaskData(.task, .subset)
  t <- mlr::createDummyFeatures(data[,1:(ncol(data) - 1)])
  t <- cbind(t, data[, ncol(data)])
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


  # pl_in   <- keras::layer_input(shape = split[1], name = "pl_in")
  # pl_out  <- keras::layer_dropout(keras::layer_dense(keras::layer_dropout(keras::layer_dense(pl_in, units = 30, activation = "relu", name = "pl_d1"), rate = 0.1), units = 15, activation = "relu", name = "pl_d2", activity_regularizer = keras::regularizer_l1()), rate = 0.1)
  #
  # in_in   <- keras::layer_input(shape = split[2], name = "ins_in")
  # in_out  <- keras::layer_dropout(keras::layer_dense(keras::layer_dropout(keras::layer_dense(in_in, units = 20, activation = "relu", name = "in_d1"), rate = 0.1), units = 15, activation = "relu", name = "in_d2", activity_regularizer = keras::regularizer_l1()), rate = 0.1)
  #
  # plant_m <- keras::layer_dropout(keras::layer_dense(keras::layer_dropout(keras::layer_dense(keras::layer_concatenate(c(pl_out, in_out), name = "conca"), units = 20, activation = "relu", name = "togeth_d1", activity_regularizer = keras::regularizer_l1()), rate = 0.2, name =  "togeth_dropout"), units = 20, activation = "relu", name = "togeth_d2"), rate = 0.1)
  #
  # pred    <- keras::layer_dense(plant_m, units = 2, activation = "sigmoid", name = "togeth_d3")
  # model   <- keras::keras_model(inputs = c(pl_in, in_in), outputs = pred)
  require(dplyr)
  model <- keras::keras_model_sequential()
  model %>% keras::layer_dense(units = arch[1], input_shape = c(ncol(X))) %>%
    keras::layer_activation("relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = arch[2]) %>%
    keras::layer_activation("relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = arch[3]) %>%
    keras::layer_activation("relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = arch[4]) %>%
    keras::layer_activation("relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(2) %>%
    keras::layer_activation("sigmoid")



  classwt <- list("0" = 1, "1" = classwt)

  keras::compile(
    model,
    loss = loss,
    optimizer = opti,
    metrics = keras::metric_binary_accuracy
  )


  history <- keras::fit(
    model,
    x = X,Y,
    epochs = epochs, batch_size = 25,
    validation_split = 0.2,
    class_weight = classwt#,
    #callbacks = keras::callback_tensorboard("logs/runs")
  )
  # plot(history)
  # model$split <- split
  model <- keras::serialize_model(model)
  return(model)
}


#'PredictLearner for keras_seq implemantation into mlr
#'
#'@export
#'
#'
predictLearner.classif.keras_seq = function(.learner, .model, .newdata, split = c(20,20), ...){
  data <- .newdata
  m    <- keras::unserialize_model(.model$learner.model)
  t    <- mlr::createDummyFeatures(data[,1:ncol(data)])
  split<- .learner$par.vals$split
  X    <- as.matrix(t)


  p = predict(m, x = X, type = "prob",...)
  colnames(p) <- c("o", "X")
  attributes(p) <- list(dim = dim(p),
                        dimnames = list(NULL,c("o", "X")),
                        class = "matrix")
  return(p)


}
#registerS3method("makeRLearner", "classif.keras", makeRLearner.classif.keras)
#registerS3method("trainLearner", "classif.keras", trainLearner.classif.keras)
#registerS3method("predictLearner", "classif.keras", predictLearner.classif.keras)



#keras2 <- train(makeLearner("classif.keras", predict.type = "prob", split = c(38, 21), opti = "adamax", loss= "categorical_hinge" , epochs = 2), task)
#prf <- mlr:::predictLearner.classif.randomForest(rr, .newdata = data)
#pk  <- predictLearner.classif.keras(keras, .newdata=data)

#k <- data[, 1:ncol(data) - 1] %>% select_if(is.factor) %>% sapply(levels) %>% summary() %>% unclass() %>% as.data.frame()  %>% select(Length)
#cols <- sum(as.numeric(levels(k$Length))[k$Length]) + ncol(data %>% select_if(is.numeric))
