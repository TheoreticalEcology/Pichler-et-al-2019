
#' Keras_seq learner
#'
#' @export


makeRLearner.regr.keras_seq <- function(){
  mlr::makeRLearnerRegr(
    cl = "regr.keras_seq",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.0001, default = 0.1),
      ParamHelpers::makeDiscreteLearnerParam(id = "loss", values = c("mean_squared_error", "mean_absolute_error","mean_absolute_percentage_error"),default = "mean_squared_error"),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c("sgd", "adamax", "rmsprop"), default = "sgd"),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 5, default = 150L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeDiscreteLearnerParam(id = "activation", values = c("relu", "elu","LeakyRelu"), default = "relu"),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "negExp"),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = T ),
      ParamHelpers::makeIntegerLearnerParam(id = "batch", lower = 5, default = 40),
      ParamHelpers::makeNumericLearnerParam(id = "decay", lower = 0.01, default = 0.97),
      ParamHelpers::makeNumericLearnerParam(id = "alpha", lower = 0.001, default = 0.2),
      ParamHelpers::makeIntegerLearnerParam(id = "seed",  tunable = F),
      ParamHelpers::makeLogicalLearnerParam(id = "logTarget", tunable = T)

    ),
    properties = c("numerics", "factors"),
    name       = "Keras DNN network",
    short.name = "dnn",
    note       = ""


  )
}
#'  Keras_seq train function
#'
#' @export

trainLearner.regr.keras_seq <- function(.learner, .task, .subset, .weights = NULL, decay = 0.97,alpha = 0.2,batch = 25,
                                        archFunction = "negExp",bias = T,lr = 0.01,activation = "relu",embedding = T,
                                        classwt = NULL, epochs = 200,nLayer = 10, loss = "mean_squared_error",
                                        opti = "sgd",  arch = 30,drop = 0.2,seed = NULL,logTarget = T, ...){
  data <- mlr::getTaskData(.task, .subset)

  if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt)))
    names(classwt) = levs

  k <- keras::backend()
  k$clear_session()
  if(!is.null(seed)) keras::use_session_with_seed(seed, disable_parallel_cpu = F)


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

  require(keras)

  if(logTarget) y = log(data[,.task$task.desc$target]+1)
  else y = data[,.task$task.desc$target]


  data = data[,colnames(data) != .task$task.desc$target]

  output = list()



      k <- keras::backend()
      k$clear_session()
      model = keras_model_sequential()

    if(activation %in% c("relu", "elu")){
      model = keras_model_sequential()
      for(i in 1:nLayer){
        if(i == 1) model %>% layer_dense(units = archLayer[i],  input_shape = ncol(data), use_bias = bias)
        else model %>% layer_dense(units = archLayer[i], use_bias = bias )
        model %>% layer_batch_normalization()
        model %>% layer_activation("relu")
        model %>% layer_dropout(drop)
      }
    } else {
      model = keras_model_sequential()
      for(i in 1:nLayer){
        if(i == 1) model %>% layer_dense(units = archLayer[i],  input_shape = ncol(data), use_bias = bias)
        else model %>% layer_dense(units = archLayer[i], use_bias = bias)
        model %>% layer_batch_normalization()
        model %>% layer_activation_leaky_relu(alpha = alpha)
        model %>% layer_dropout(drop)
      }
    }

    model %>% layer_dense(units = 1, activation = "linear")

    if(opti == "sgd") optimizer = keras::optimizer_sgd(lr,0.9,nesterov = T)
    if(opti == "adamax") optimizer = keras::optimizer_adam(lr = lr)
    model %>% compile(loss = loss,
                      optimizer = opti
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
      fit(x = as.matrix(data),
          y = matrix(y,ncol = 1),
          epochs = epochs,
          batch_size = batch,
          shuffle = T,
          callbacks = callbacks
      )



  output$model$model <- keras::serialize_model(model)
  output$model$logTarget = logTarget
  return(output)
}


#'PredictLearner for keras_seq implemantation into mlr
#'
#'@export
#'
#'
predictLearner.regr.keras_seq = function(.learner, .model, .newdata, ...){
    require(keras)
    data <- .newdata
    m    <- keras::unserialize_model(.model$learner.model$model$model)
    if(.model$learner.model$model$logTarget) p = exp(predict(m, x = as.matrix(data)))- 0.99
    else p = predict(m, x = as.matrix(data))


  return(p[,1])


}
