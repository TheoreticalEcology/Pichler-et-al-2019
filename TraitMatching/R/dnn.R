
#' Keras_seq learner
#'
#' @export


makeRLearner.classif.keras_seq <- function(){
  mlr::makeRLearnerClassif(
    cl = "classif.keras_seq",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericVectorLearnerParam(id = "classwt", lower = 1),
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.0001, default = 0.01),
      ParamHelpers::makeDiscreteLearnerParam(id = "loss", values = c("categorical_hinge",  "categorical_crossentropy", "kullback_leibler_divergence"),default = "categorical_crossentropy"),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c("sgd", "adamax"), default = "sgd"),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 2, default = 150L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeDiscreteLearnerParam(id = "activation", values = c("relu", "elu", "LeakyRelu"), default = "relu"),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "negExp"),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = T ),
      ParamHelpers::makeIntegerLearnerParam(id = "batch", lower = 5, default = 40),
      ParamHelpers::makeNumericLearnerParam(id = "decay", lower = 0.01, default = 0.97),
      ParamHelpers::makeNumericLearnerParam(id = "alpha", lower = 0.001, default = 0.2),
      ParamHelpers::makeIntegerLearnerParam(id = "seed",  tunable = F)
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

trainLearner.classif.keras_seq <- function(.learner, .task, .subset, .weights = NULL, decay = 0.97, alpha = 0.2,archFunction = "negExp",
                                           batch = 32 ,activation = "relu",lr = 0.01,classwt = NULL, bias = T,epochs = 200,nLayer = 10,
                                           loss = "categorical_crossentropy", opti = "sgd",  arch = 30,drop = 0.2,seed = NULL, ...){
  data <- mlr::getTaskData(.task, .subset)



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

  output = list()


  if(activation %in% c("relu", "elu")){
    model = keras_model_sequential()
        for(i in 1:nLayer){
          if(i == 1) model %>% layer_dense(units = archLayer[i], input_shape = ncol(data), use_bias = bias)
          else model %>% layer_dense(units = archLayer[i], use_bias = bias)
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

  model %>% layer_dense(units = 2L, activation = "softmax")

  if(opti == "sgd") optimizer = keras::optimizer_sgd(lr,0.9,nesterov = T)
  if(opti == "adamax") optimizer = keras::optimizer_adam(lr = lr)

  model %>% compile(loss = loss,
                    optimizer = optimizer,
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
        fit(x = as.matrix(data),
            y = y,
            epochs = epochs,
            batch_size = batch,
            shuffle = T,
            callbacks = callbacks,
            class_weight = cw
        )



  test = list()
  test$data = .task
  test$arch = archLayer
  test$drop = drop
  test$lr = lr
  test$data = data
  test$Layer = nLayer

  output$test = test

  output$model <- keras::serialize_model(model)
  return(output)
}


#'PredictLearner for keras_seq implemantation into mlr
#'
#'@export
#'
#'
predictLearner.classif.keras_seq = function(.learner, .model, .newdata,  ...){
  require(keras)
  data <- .newdata

  m    <- keras::unserialize_model(.model$learner.model$model)
  type = ifelse(.learner$predict.type == "response", "response", "prob")
  p = predict(m, x = as.matrix(data[,.model$features]), type = type,...)


  colnames(p) <- c(.model$task.desc$negative, .model$task.desc$positive)
  attributes(p) <- list(dim = dim(p),
                        dimnames = list(NULL,c(.model$task.desc$negative, .model$task.desc$positive)),
                        class = "matrix")
  out = list()
  out$model = serialize_model(m)
  out$data = .newdata
  out$train = .model$learner.model$test
  if(any(is.na(p))) {
    saveRDS(out, file = paste(runif(1,1,10),"error.RDS",sep = "" ))
    p[,1] = 1
    p[,2] = 0
  }
  return(p)


}
