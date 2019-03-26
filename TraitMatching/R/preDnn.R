
#' Keras_seq learner
#'
#' @export


makeRLearner.classif.preKeras <- function(){
  mlr::makeRLearnerClassif(
    cl = "classif.preKeras",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericVectorLearnerParam(id = "classwt", lower = 1),
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.001, default = 0.1),
      ParamHelpers::makeDiscreteLearnerParam(id = "loss", values = c("categorical_hinge",  "binary_crossentropy", "kullback_leibler_divergence"),default = "binary_crossentropy"),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c("rmsprop", "adamax"), default = "rmsprop"),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "split", lower = 0, default = 0),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 10, default = 150L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeDiscreteLearnerParam(id = "activation", values = c("relu", "elu", "LeakyRelu"), default = "relu"),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "continous"),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = TRUE, tunable = TRUE),
      ParamHelpers::makeNumericLearnerParam(id = "decay", lower = 0.01, default = 0.97),
      ParamHelpers::makeNumericLearnerParam(id = "alpha", lower = 0.001, default = 0.2),
      ParamHelpers::makeIntegerLearnerParam(id = "batch", lower = 5, default = 40)
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

trainLearner.classif.preKeras <- function(.learner, .task, .subset, .weights = NULL, decay = 0.97, batch = 40,alpha = 0.2,bias = T,lr = 0.01,archFunction = "continous", activation = "relu"
                                          ,classwt = NULL, epochs = 200,nLayer = 10, loss = "categorical_crossentropy", opti = "adamax", split = c(20, 20), arch = 30,drop = 0.2, ...){
  data <- mlr::getTaskData(.task, .subset)



  k <- keras::backend()
  k$clear_session()

  if(archFunction == "parable"){
    archfun = function(x,n){
      arch = 0
      for(i in 1:n) arch[i] = -0.3*x/((1-mean(1:n))^2)*(i-mean(1:n))^2 + x*1.3
      return(arch)
    }
    if(nLayer > 1) archLayer = as.integer(archfun(arch,nLayer))
    else archLayer = arch
  }


  if(archFunction == "negExp"){
    archfun <- function(x,n,m = 1,asy = 0.5){
      res <- 0
      a = (x-asy*x)/exp(-1*m)
      for(i in 1:n){
        res[i] <- a*exp(-m*i) + x*asy
        x = res[i]
        a = (x-asy*x)/exp(-1*m)

      }
      return(as.integer( res))
    }
    if(arch < 15 && nLayer > 3) archLayer = archfun(arch,nLayer,m = 0.6, asy = 0.8)
    else if(nLayer > 4) archLayer = archfun(arch,nLayer,m = 0.3, asy = 0.8)
    else archLayer = archfun(arch,nLayer,m = 0.6, asy = 0.5)
  }
  if(archFunction == "continous"){
    archLayer = rep(arch,nLayer)
  }

  require(dplyr)
  require(keras)
  early_stopping = keras::callback_early_stopping(monitor = "loss", patience = 7)

  positive = .task$task.desc$positive
  y = data$target
  negative = levels(y)[which(!levels(y)==positive, arr.ind = T)]
  y = as.character(y)
  y[y==positive] = "1"
  y[y==negative] = "0"
  y = as.integer(y)

  if(!is.null(classwt)) cw = list("0" = 1, "1" = classwt[[positive]])
  else cw = NULL

  y = to_categorical(y,num_classes = 2)

  data = data[,colnames(data) != .task$task.desc$target]


  autoencoder = c(as.integer(ncol(data)*0.7),as.integer(ncol(data)*0.2))

  model = keras_model_sequential()
  model %>%
    layer_dense(input_shape = c(ncol(data)), units = autoencoder[1])

  if(activation == "LeakyRelu") model %>% layer_activation_leaky_relu(alpha = alpha)
  else model %>% layer_activation(activation = activation)

  model %>%
    layer_dense(units = autoencoder[2])

  if(activation == "LeakyRelu") model %>% layer_activation_leaky_relu(alpha = alpha)
  else model %>% layer_activation(activation = activation)


  model %>%
    layer_dense(units = autoencoder[1])

  if(activation == "LeakyRelu") model %>% layer_activation_leaky_relu(alpha = alpha)
  else model %>% layer_activation(activation = activation)

  model %>%
    layer_dense(units = ncol(data))
  model %>%
      compile(
        loss = "mean_squared_error",
        optimizer = "rmsprop",
        metrics = keras::metric_mean_squared_error
      )

  model %>%
    fit(
      x = as.matrix(data),
      y = as.matrix(data),
      shuffle = T,
      epochs = epochs,
      batch_size = batch,
      callbacks = c(early_stopping)
    )

  model %>% pop_layer()
  model %>% pop_layer()

  for(i in 1:nLayer){
    model %>% layer_dropout(drop)
    model %>% layer_dense(units = archLayer[i])
    if(activation == "LeakyRelu") model %>% layer_activation_leaky_relu(alpha = alpha)
    else model %>% layer_activation(activation = activation)
  }

  model %>% layer_dense(units = 2, activation = "softmax")


  # model = keras_model_sequential()
  # model = model %>%
  #   layer_dense(input_shape = c(NULL,ncol(data)),activation = activation, units = 15, name = "auto1", activity_regularizer = regularizer_l2(l = 0.01)) %>%
  #   layer_dense(units = ncol(data))
  # model %>%
  #   compile(
  #     loss = "mean_squared_error",
  #     optimizer = "rmsprop",
  #     metrics = keras::metric_mean_squared_error
  #   )
  # model %>%
  #   fit(
  #     x = as.matrix(data),
  #     y = as.matrix(data),
  #     shuffle = T,
  #     epochs = epochs,
  #     batch_size = 20,
  #     callbacks = c(early_stopping)
  #     )
  # model %>% pop_layer()
  # model = model %>%
  #   layer_dense(activation = activation, units = 5, name = "auto2", activity_regularizer = regularizer_l2(l = 0.01)) %>%
  #   layer_dense(units = ncol(data))
  # model %>% freeze_weights(from = "auto1", to = "auto1")
  # model %>%
  #   compile(
  #     loss = "mean_squared_error",
  #     optimizer = "rmsprop",
  #     metrics = keras::metric_mean_squared_error
  #   )
  # model %>%
  #   fit(
  #     x = as.matrix(data),
  #     y = as.matrix(data),
  #     shuffle = T,
  #     epochs = epochs,
  #     batch_size = 20,
  #     callbacks = c(early_stopping)
  #   )
  #
  # model %>% pop_layer()
  #
  # for(i in 1:nLayer) model %>% layer_dense(units = archLayer[i], activation = activation,kernel_regularizer = keras::regularizer_l2(0.005))
  #
  # model %>% layer_dense(units = 2, activation = "softmax")




  # model %>% freeze_weights(from = 1, to = 1)
  model %>%
    compile(
      loss = "categorical_crossentropy",
      optimizer = keras::optimizer_sgd(lr,0.9,nesterov = T),
      metrics = "accuracy"
    )

  early_stopping = keras::callback_early_stopping(monitor = "loss", patience = 10)
  plat = keras::callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.5, patience = 5, verbose = 0, mode = "auto",cooldown = 0, min_lr = 0)
  stopNa = keras::callback_terminate_on_naan()
  decayCall<- R6::R6Class("decay", inherit = KerasCallback,public = list(
    losses = NULL,
    on_epoch_end = function(epoch, logs){
      if(epoch%%5==0){
        lr = k_get_value(self$model$optimizer$lr)
        lr = lr*decay
        k_set_value(self$model$optimizer$lr, lr)
      }
    }
  ))
  decC = decayCall$new()

  callbacks = list(early_stopping, plat,stopNa, decC)
  model %>%
    fit(
      x = as.matrix(data),
      y = y,
      shuffle = T,
      epochs = epochs,
      batch_size = batch,
      classwt = cw,
      callbacks = callbacks
    )

  output <- keras::serialize_model(model)
  return(output)
}


#'PredictLearner for keras_seq implemantation into mlr
#'
#'@export
#'
#'
predictLearner.classif.preKeras = function(.learner, .model, .newdata, split = c(20,20), ...){
  require(keras)
  data <- .newdata
  m    <- keras::unserialize_model(.model$learner.model)
  type = ifelse(.learner$predict.type == "response", "response", "prob")

  p = predict(m, x = as.matrix(data), type = type,...)
  colnames(p) <- c("negative", "positive")
  attributes(p) <- list(dim = dim(p),
                        dimnames = list(NULL,c("negative", "positive")),
                        class = "matrix")
  return(p)
}
