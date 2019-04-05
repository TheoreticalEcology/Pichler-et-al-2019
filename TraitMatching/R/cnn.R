
#' Keras_convlearner
#' @export

makeRLearner.classif.keras_conv <- function(){
  mlr::makeRLearnerClassif(
    cl = "classif.keras_conv",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericVectorLearnerParam(id = "classwt", lower = 1),
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeIntegerLearnerParam(id = "filter", lower = 3, default = 10),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.0001, default = 0.1),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeDiscreteLearnerParam(id = "loss", values = c("categorical_hinge", "categorical_crossentropy", "binary_crossentropy", "kullback_leibler_divergence"),default = "categorical_crossentropy"),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c("sgd", "adamax"), default = "sgd"),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 10, default = 200L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = TRUE, tunable = TRUE),
      ParamHelpers::makeIntegerLearnerParam(id = "kernel_size", lower = 2, default = 2),
      ParamHelpers::makeDiscreteLearnerParam(id = "pool", values = c("max", "average"), default = "average"),
      ParamHelpers::makeIntegerLearnerParam(id = "nConv", lower = 1, upper = 2, default = 1),
      ParamHelpers::makeDiscreteLearnerParam(id = "activation", values = c("relu", "elu"), default = "relu"),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "negExp"),
      ParamHelpers::makeIntegerLearnerParam(id = "batch", lower = 5, default = 40),
      ParamHelpers::makeNumericLearnerParam(id = "decay", lower = 0.01, default = 0.97),
      ParamHelpers::makeIntegerLearnerParam(id = "seed",  tunable = F)

    ),
    properties = c("twoclass", "numerics", "factors", "prob", "class.weights"),
    class.weights.param = "classwt",
    name       = "Keras conv network",
    short.name = "keras_conv",
    note       = "test...."


  )
}
#'  Keras_conv train function
#'
#' @export

trainLearner.classif.keras_conv <- function(.learner, .task, .subset, .weights = NULL, batch = 25, archFunction = "negExp", activation = "relu", decay = 0.97,
                                            nConv = 1, pool = "average", kernel_size = 2L, bias = T, nLayer = 10,arch = 200,opti = "sgd", loss = "categorical_crossentropy",
                                            drop = 0.6, lr = 0.01, filter = 10, epochs = 200, classwt = NULL,seed = NULL, ...){
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

  positive = .task$task.desc$positive
  y = data$target
  negative = levels(y)[which(!levels(y)==positive, arr.ind = T)]
  y = as.character(y)
  y[y==positive] = "1"
  y[y==negative] = "0"
  y = as.integer(y)
  if(!is.null(classwt)) cw = list("0" = 1, "1" = classwt[[positive]])
  else cw = NULL

  require(keras)

  y = to_categorical(y,num_classes = 2)

  output = list()
  data = data[,colnames(data) != .task$task.desc$target]

  k <- keras::backend()
  k$clear_session()

  dim = c(nrow(data), ncol(data), 1L)
  ar = array(as.matrix(data), dim = c(nrow(data), ncol(data), 1))

  model = keras_model_sequential()

  for(i in 1:nConv){
      if(i == 1) {
          model %>% layer_conv_1d(activation = activation, input_shape = c(ncol(data), 1L), filters = filter, kernel_size = kernel_size)
        } else {
          model %>% layer_conv_1d(activation = activation,  filters = as.integer(filter/2), kernel_size = kernel_size*2)
        }
        if(pool == "average") model %>% layer_average_pooling_1d()
        else model %>% layer_max_pooling_1d()
        model %>% layer_batch_normalization()
      }
   if(pool == "average") model %>% layer_average_pooling_1d()
   else model %>% layer_max_pooling_1d()
   model %>% layer_batch_normalization()

   # model %>% layer_conv_1d(activation = activation,  filters = 16, kernel_size = 1L)
   # model %>% layer_conv_1d(activation = activation,  filters = 16, kernel_size = 3L)
   #
   # if(pool == "average") model %>% layer_average_pooling_1d()
   # else model %>% layer_max_pooling_1d()
   # model %>% layer_batch_normalization()


   model %>% layer_flatten()

   for(i in 1:nLayer){
        model %>% layer_dense(units = archLayer[i], activation = activation, use_bias = bias)
   }

   model %>% layer_dropout(drop)
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
        fit(x = ar,
            y = y,
            epochs = epochs,
            batch_size = batch,
            shuffle = T,
            callbacks = callbacks,
            class_weight = cw,
            verbose = 0
        )

  test = list()
  test$data = .task
  test$arch = archLayer
  test$drop = drop
  test$lr = lr
  test$Layer = nLayer
  test$filter = filter
  test$conv = nConv

  output$test = test

  output$model <- keras::serialize_model(model)
  return(output)
}







#'PredictLearner for keras_conv implemantation into mlr
#'
#'@export



predictLearner.classif.keras_conv = function(.learner, .model, .newdata, ...){
  require(keras)

  data <- .newdata
  dim = c(nrow(data), ncol(data), 1L)
  data = array(as.matrix(data), dim = dim)
  m    <- keras::unserialize_model(.model$learner.model$model)
  type = ifelse(.learner$predict.type == "response", "response", "prob")
  p = predict(m, x = data, type = type)
  colnames(p) <- c("negative", "positive")
  attributes(p) <- list(dim = dim(p),
                        dimnames = list(NULL,c("negative", "positive")),
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
