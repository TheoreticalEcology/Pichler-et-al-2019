
#' Keras_convlearner
#' @export

makeRLearner.regr.keras_conv <- function(){
  mlr::makeRLearnerRegr(
    cl = "regr.keras_conv",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeIntegerLearnerParam(id = "filter", lower = 3, default = 10),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.00001, default = 0.1),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeDiscreteLearnerParam(id = "loss", values = c("mean_squared_error", "mean_squared_logarithmic_error", "poisson"),default = "poisson"),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c("sgd", "adamax"), default = "sgd"),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 10, default = 200L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = TRUE, tunable = TRUE),
      ParamHelpers::makeIntegerLearnerParam(id = "kernel_size", lower = 2, default = 2),
      ParamHelpers::makeDiscreteLearnerParam(id = "pool", values = c("max", "average"), default = "average"),
      ParamHelpers::makeIntegerLearnerParam(id = "nConv", lower = 1, upper = 2, default = 1),
      ParamHelpers::makeDiscreteLearnerParam(id = "activation", values = c("relu", "elu","tanh"), default = "relu"),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "continous"),
      ParamHelpers::makeIntegerLearnerParam(id = "batch", lower = 5, default = 40),
      ParamHelpers::makeNumericLearnerParam(id = "decay", lower = 0.01, default = 0.97)

    ),
    properties = c("numerics", "factors"),
    name       = "keras cnn regression",
    short.name = "cnnR",
    note       = ""


  )
}
#'  Keras_conv train function
#'
#' @export

trainLearner.regr.keras_conv <- function(.learner, .task, .subset, .weights = NULL, batch = 25, decay = 0.97,
                                         archFunction = "negExp",activation = "relu",pool = "average",nConv = 1,
                                         nLayer = 2L,bias = T,kernel_size=2, epochs = 200, loss = "poisson",
                                         opti = "sgd", arch = 30L,drop = 0.25, filter = 18,lr = 0.01, ...){
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

  y = data[,.task$task.desc$target]

  if(loss == "poisson") y = log(y+1)


  output = list()
  data = data[,colnames(data) != .task$task.desc$target]


      k <- keras::backend()
      k$clear_session()
      dim = c(nrow(data), ncol(data), 1L)
      data = array(as.matrix(data), dim = dim)
      model = keras_model_sequential()

      for(i in 1:nConv){
        if(i == 1) {
          model %>% layer_conv_1d( activation = activation, input_shape = c(NULL, dim[2], dim[3]), filters = filter, kernel_size = kernel_size)

        } else {
          model %>% layer_conv_1d( activation = activation,  filters = as.integer(filter/2), kernel_size = kernel_size*2)
        }
        if(pool == "average") model %>% layer_average_pooling_1d()
        else model %>% layer_max_pooling_1d()
        model %>% layer_batch_normalization()
      }

      model %>% layer_flatten()
      for(i in 1:nLayer){
        model %>% layer_dense(units = archLayer[i], activation = activation, use_bias = bias)
      }
      model %>% layer_dropout(drop)
      model %>% layer_dense(units = 1L, activation = "linear")

      if(loss == "poisson") model %>% layer_lambda(k_exp)

      if(opti == "sgd") optimizer = keras::optimizer_sgd(lr,0.9,nesterov = T)
      if(opti == "adamax") optimizer = keras::optimizer_adam(lr = lr)

      model %>% compile(loss = loss,
                        optimizer = optimizer
      )

    early_stopping = keras::callback_early_stopping(monitor = "loss", patience = 20)
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
      fit(x = data,
          y = matrix(y,ncol = 1),
          epochs = epochs,
          batch_size = batch,
          shuffle = T,
          callbacks = callbacks
      )
  if(loss == "poisson") model %>% pop_layer()
    output$model$model <- keras::serialize_model(model)
    output$model$loss = loss
  return(output)
}







#'PredictLearner for keras_conv implemantation into mlr
#'
#'@export



predictLearner.regr.keras_conv = function(.learner, .model, .newdata, ...){
  require(keras)
  data <- .newdata
  dim = c(nrow(data), ncol(data), 1)
  data = array(as.matrix(data), dim = dim)
  m    <- keras::unserialize_model(.model$learner.model$model$model)
  if(.model$learner.model$model$loss == "poisson") m %>% layer_lambda(f = k_exp)
  p = predict(m, x = data)
  if(.model$learner.model$model$loss == "poisson") p = exp(p) -1
  return(p[,1])
}
