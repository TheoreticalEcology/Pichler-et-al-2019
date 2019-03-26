
#' Keras_seq learner
#'
#' @export


makeRLearner.regr.poissonDnn <- function(){
  mlr::makeRLearnerRegr(
    cl = "regr.poissonDnn",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.0001, default = 0.1),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c( "adam", "rmsprop"), default = "sgd"),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 5, default = 150L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "negExp"),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = T ),
      ParamHelpers::makeIntegerLearnerParam(id = "batch", lower = 5, default = 40),
      ParamHelpers::makeIntegerLearnerParam(id = "seed",  tunable = F,default = 42),
      ParamHelpers::makeLogicalLearnerParam(id = "expValue", default = F,tunable = F)

    ),
    properties = "numerics",
    name       = "Keras/TensorFlow DNN network with poisson loss function",
    short.name = "poissonDnn",
    note       = ""


  )
}
#'  Keras_seq train function
#'
#' @export

trainLearner.regr.poissonDnn <- function(.learner, .task, .subset, .weights = NULL, batch = 25L,
                                        archFunction = "negExp",bias = T,lr = 0.01,
                                        epochs = 200,nLayer = 10,
                                        opti = "rmsprop",  arch = 30L,drop = 0.2,seed = NULL,expValue = F, ...){
  data <- mlr::getTaskData(.task, .subset)
  .drop = drop

  if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt)))
    names(classwt) = levs

  require(tensorflow)

  if(!is.null(seed)) tf$set_random_seed(seed)
  set.seed(seed)

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

  output = list()
  dropP = 1-.drop
  nLayer = nLayer - 1
  if(expValue) data[,.task$task.desc$target] = exp(data[,.task$task.desc$target]) - 1

  generator <- function(){
    return(as.matrix(data[sample.int(nrow(data), batch),]))
  }

  nInp = as.integer(ncol(generator())-1)
  x = tf$placeholder(tf$float32, shape(NULL, nInp))
  y = tf$placeholder(tf$float32, shape(NULL, 1L))
  drop = tf$placeholder(tf$float32)
  net = function(x,  drop){
    first = tf$layers$dense(x,units = as.integer(archLayer[1]),activation = tf$nn$relu, use_bias = bias,name = "layer_1")
    second = tf$nn$dropout(first, keep_prob = drop)
    for(i in 1:(nLayer)){
      second = tf$layers$dense(second, units = as.integer(archLayer[i+1]), activation = tf$nn$relu, use_bias = bias, name = paste0("layer_",i+1))
      second = tf$nn$dropout(second, keep_prob = drop)
    }
    out = tf$layers$dense(second, units = 1L, activation = NULL, name = paste0("layer_",nLayer+2),use_bias = bias)
    return(out)
  }
  # negBinom loss function
  pred = net(x,  drop)
  y_true = tf$cast(y, tf$float32)
  y_pred = tf$exp(tf$cast(pred, tf$float32) )
  final = tf$contrib$distributions$Poisson(rate = y_true)$log_prob(y_pred)
  loss = tf$reduce_mean(-final)

  optimizer = switch(opti,
                       rmsprop = {
                         tf$train$RMSPropOptimizer(learning_rate = lr)$minimize((loss))
                       },
                       adam = {
                         tf$train$AdamOptimizer(learning_rate = lr)$minimize((loss))
                       })

  # init variables and start session
  init = tf$global_variables_initializer()
  sess = tf$Session()
  sess$run(init)

  # prepare training
  step = 1
  targetN = which(colnames(generator()) == .task$task.desc$target, arr.ind = T)

  # training
  while(step <= as.integer(nrow(data) / batch * epochs)) {
    batch = generator()
    batch_xs <- batch[,-targetN]
    batch_ys <- batch[,targetN, drop = F]
    sess$run(optimizer, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP)))
    if( step %% 50 == 0){
      cat("\nStep:", step, ", MiniB loss = ", sess$run(loss, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP)) ))
    }
    step = step +1
  }

  ## export as keras model, easier for predictionL

  weights = vector("list",nLayer)
  if(bias){
    for(i in 1:(nLayer+2)) weights[[i]] = list(sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/kernel:0"))), sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/bias:0"))))
  } else {
    for(i in 1:(nLayer+2)) weights[[i]] = list(sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/kernel:0"))))
  }
  tf$reset_default_graph()
  sess$close()

  require(keras)
  model = keras_model_sequential()
  model %>% layer_dense(input_shape = c(NULL, nInp), units =arch ,use_bias = bias,activation = "relu") %>% layer_dropout(rate = .drop)
  for(i in 1:nLayer) model %>% layer_dense(arch, activation = "relu",use_bias = bias) %>%layer_dropout(rate = .drop)
  model %>%  layer_dense(1L, activation = "linear",use_bias = bias)

  # update keras weights with trained weights
  counter = 1
  for(i in 1:(2*(nLayer+2))) {
    if(!i%%2 == 0) {
      keras::set_weights(object = model$layers[[i]], weights[[counter]])
      counter = counter + 1
    }
  }

  model %>%
    compile(loss = "mean_squared_error",optimizer = "adam")
  output$model$model <- keras::serialize_model(model)
  output$model$expValue = expValue
  return(output)
}








#'PredictLearner for keras_seq implemantation into mlr
#'
#'@export
#'
#'
predictLearner.regr.poisson = function(.learner, .model, .newdata, ...){
  require(keras)
  data <- .newdata
  m    <- keras::unserialize_model(.model$learner.model$model$model)
  p = predict(m, x = as.matrix(data))
  if(.model$learner.model$model$expValue) p = log(p+1)
  return(p[,1])

}
