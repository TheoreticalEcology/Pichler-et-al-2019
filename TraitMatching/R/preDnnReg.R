
#' Keras_seq learner
#'
#' @export


makeRLearner.regr.preKeras <- function(){
  mlr::makeRLearnerRegr(
    cl = "regr.keras_seq",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericLearnerParam(id = "classwt", lower = 1, default = 1),
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.001, default = 0.1),
      ParamHelpers::makeDiscreteLearnerParam(id = "loss", values = c("categorical_hinge",  "poisson", "kullback_leibler_divergence"),default = "poisson"),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c("rmsprop", "adamax"), default = "rmsprop"),
      ParamHelpers::makeIntegerVectorLearnerParam(id = "split", lower = 0, default = 0),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 10, default = 150L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeDiscreteLearnerParam(id = "activation", values = c("relu", "elu"), default = "relu"),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "continous"),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = T)
    ),
    properties = c("numerics", "factors"),
    name       = "Keras pre train dnn regression network",
    short.name = "preDnnR",
    note       = ""


  )
}
#'  Keras_seq train function
#'
#' @export

trainLearner.regr.preKeras <- function(.learner, .task, .subset, .weights = NULL, archFunction = "negExp", lr = 0.01,activation = "relu",classwt = NULL, epochs = 200,nLayer = 10,bias = T, loss = "poisson", opti = "adamax", split = c(20, 20), arch = 30,drop = 0.2, ...){
  data <- mlr::getTaskData(.task, .subset)

  if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt)))
    names(classwt) = levs

  # Y = keras::to_categorical(Z)[,2]
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

  y = data$target

  data = mlr::createDummyFeatures(data[,1:(ncol(data)-1)])

  model = keras_model_sequential()
  model = model %>%
    layer_dense(input_shape = c(NULL,ncol(data)),activation = activation, units = 70, name = "auto1", activity_regularizer = regularizer_l2(l = 0.01)) %>%
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
      batch_size = 20,
      callbacks = c(early_stopping)
    )
  model %>% pop_layer()
  model = model %>%
    layer_dense(activation = activation, units = 70, name = "auto2", activity_regularizer = regularizer_l2(l = 0.01)) %>%
    layer_dense(units = ncol(data))
  model %>% freeze_weights(from = "auto1", to = "auto1")
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
      batch_size = 20,
      callbacks = c(early_stopping)
    )

  model %>% pop_layer()

  for(i in 1:nLayer) model %>% layer_dense(units = archLayer[i], activation = activation,kernel_regularizer = keras::regularizer_l2(0.005), use_bias = bias)

  model %>% layer_dense(units = 1, activation = "linear")
  # model %>% layer_lambda(f = k_exp)


  model %>% freeze_weights(from = 1, to = 1)
  model %>%
    compile(
      loss = "mean_squared_error",
      optimizer = keras::optimizer_sgd(lr,0.9,nesterov = T),
      metrics = "mean_squared_error"
    )

  early_stopping = keras::callback_early_stopping(monitor = "loss", patience = 5)
  model %>%
    fit(
      x = as.matrix(data),
      y = y,
      shuffle = T,
      epochs = epochs,
      batch_size = 20,
      callbacks = c(early_stopping)
    )
  # model %>% pop_layer()
  model <- keras::serialize_model(model)
  return(model)
}


#'PredictLearner for keras_seq implemantation into mlr
#'
#'@export
#'
#'
predictLearner.regr.preKeras = function(.learner, .model, .newdata, split = c(20,20), ...){
  require(keras)
  data <- as.matrix(mlr::createDummyFeatures(.newdata))

  m    <- keras::unserialize_model(.model$learner.model)
  # m %>% layer_lambda(f = k_exp)
  p = predict(m, x = data)
  return(p[,1])


}
