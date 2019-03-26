
#' Keras_seq learner
#'
#' @export


makeRLearner.regr.multiNomDnn <- function(){
  mlr::makeRLearnerRegr(
    cl = "regr.multiNomDnn",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.0001, default = 0.1),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c( "adam", "rmsprop"), default = "rmsprop"),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 5, default = 50L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "negExp"),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = TRUE ),
      ParamHelpers::makeIntegerLearnerParam(id = "batch", lower = 1, default = 40),
      ParamHelpers::makeIntegerLearnerParam(id = "seed",  tunable = FALSE,default = 42),
      ParamHelpers::makeLogicalLearnerParam(id = "expValue", default = FALSE,tunable = FALSE),
      ParamHelpers::makeIntegerLearnerParam(id = "plotN", default = 1, tunable = FALSE),
      ParamHelpers::makeLogicalLearnerParam(id = "correct", default = FALSE)
    ),
    properties = c("numerics","factors"),
    name       = "Keras/TensorFlow DNN network with multinom loss function",
    short.name = "multiNomDnn",
    note       = ""


  )
}
#'  Keras_seq train function
#'
#' @export

trainLearner.regr.multiNomDnn <- function(.learner, .task, .subset, .weights = NULL, batch = 25L,
                                        archFunction = "negExp",bias = T,lr = 0.01,
                                        epochs = 200,nLayer = 10,
                                        opti = "rmsprop",  arch = 30L,drop = 0.2,seed = NULL,expValue = FALSE,plotN = 1,correct = FALSE, ...){

  data <- mlr::getTaskData(.task, .subset)
  .drop = drop

  require(tensorflow)
  tf$reset_default_graph()
  data[,1] = droplevels(data[,1])
  data[,2] = droplevels(data[,2])



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
  plotN = plotN
  output = list()
  dropP = .drop
  nLayer = nLayer - 1
  if(expValue) data[,.task$task.desc$target] = exp(data[,.task$task.desc$target]) - 1

  countsA = list()

  # prepare counts for Y individuals
  for(plot in 1:plotN){
    if(plotN == 1) .train = data
    else .train = data[data[,ncol(data) - plotN + plot] > 0,]
    .countsA = data.frame()
    for(a in unique(droplevels(.train[,1]))){
      .countsA = rbind(.countsA, data.frame(A = a, counts =sum(.train[.train[,1] == a,"target"])))
    }
    countsA[[plot]] = .countsA
  }


  if(plotN == 1){
    Y_pool = unique(data[,2])
    SampleFrom = vector("list", length(Y_pool))
    out2 = data.frame()
    .countsA = countsA[[1]]
    for(i in 1:length(Y_pool)) {
      .sub = data[data[,2] == Y_pool[i],]
      SampleFrom[[i]][["countsOut"]] = sapply(droplevels(.sub[,1]), FUN = function(x) .countsA[.countsA[,1] == x,2])
      SampleFrom[[i]][["train"]] = .sub[,-c(1,2)]
    }
  } else {
    SampleFrom = vector("list", plotN)
    for(k in 1:plotN){
      subPlot = k
      sub = data[data[,(ncol(data) - plotN + subPlot)] > 0,]
      Y_pool = unique(sub[,2])
      .SampleFrom = vector("list", length(Y_pool))
      out2 = data.frame()
      .countsA = countsA[[subPlot]]
      for(i in 1:length(Y_pool)) {
        .sub = sub[sub[,2] == Y_pool[i],]
        .SampleFrom[[i]][["countsOut"]] = sapply(droplevels(.sub[,1]), FUN = function(x) .countsA[.countsA[,1] == x,2])
        .SampleFrom[[i]][["train"]] = .sub[,-c(1,2)]
      }
      SampleFrom[[subPlot]] = .SampleFrom
    }
  }

  if(plotN == 1) minBatch =  min(unlist(lapply(SampleFrom, function(x) length(x))))
  else minBatch =  min(unlist(lapply(SampleFrom, function(x) lapply(x, function(.x)length(.x)))))

  if(minBatch < batch) batch = minBatch


  # select Y-wise
  generator <- function(){
    if(plotN>2){
      subPlot = sample(1:plotN,1)
      whichSample = sample(length(SampleFrom[[subPlot]]), batch)
      out2 = data.frame()
      countsOut = NULL
      for(i in whichSample) {
        out2 = rbind(out2, SampleFrom[[subPlot]][[i]]$train)
        countsOut = c(countsOut, SampleFrom[[subPlot]][[i]]$countsOut)
      }

    }else{
      whichSample = sample(length(SampleFrom), batch)
      out2 = data.frame()
      countsOut = NULL
      for(i in whichSample) {
        out2 = rbind(out2, SampleFrom[[i]]$train)
        countsOut = c(countsOut, SampleFrom[[i]]$countsOut)
      }
    }
    countsOut = c(t(countsOut))
    result = list(train = as.matrix(out2), counts = matrix(countsOut, ncol = 1))
    return(result)
  }

  goSteps = as.integer(epochs*nrow(data)/mean(sapply(1:20, function(x) nrow(generator()$train))))

  nInp = as.integer(ncol(generator())-1)
  tf$set_random_seed(seed)
  nInp = as.integer(ncol(generator()$train)-1)


  x = tf$placeholder(tf$float64, shape(NULL, nInp))
  y = tf$placeholder(tf$float64, shape(NULL, 1L))
  C = tf$placeholder(tf$float64, shape(NULL, 1L))
  drop = tf$placeholder(tf$float64)
  net = function(x,  drop){
    first = tf$layers$dense(x,units = as.integer(archLayer[1]),activation = tf$nn$relu, use_bias = bias,name = "layer_1")
    second = tf$nn$dropout(first, keep_prob = drop)
    if(nLayer != 0) {
      for(i in 1:(nLayer)){
        second = tf$layers$dense(second, units = as.integer(archLayer[i+1]), activation = tf$nn$relu, use_bias = bias, name = paste0("layer_",i+1))
        second = tf$nn$dropout(second, keep_prob = drop)
      }
    }
    out = tf$layers$dense(second, units = 1L, activation = NULL, name = paste0("layer_",nLayer+2),use_bias = bias)
    return(out)
  }
  # multinom loss function
  pred = net(x,  drop)
  eps = tf$constant(.Machine$double.eps, tf$float64)
  y_true = tf$cast(y, tf$float64)
  if(correct) y_true = tf$reshape(tf$div(y_true, C+eps), shape = shape(as.integer(batch),-1L))
  else y_true = tf$reshape(y_true, shape = shape(as.integer(batch),-1L))
  y_pred = tf$reshape(tf$exp(tf$cast(pred, tf$float64)) , shape = shape(as.integer(batch),-1L))
  final  = tf$contrib$distributions$Multinomial(total_count = tf$reduce_sum(y_pred, -1L), probs =  tf$nn$softmax(y_true+eps))$log_prob(y_pred)
  loss   = tf$reduce_mean(tf$boolean_mask(-final, tf$logical_not(tf$is_inf(-final))))

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
  targetN = which(colnames(generator()$train) == .task$task.desc$target, arr.ind = T)
  counter = 1
  bestLoss = Inf

  # training
  while(step <= goSteps) {
    repeat{
      batchT = generator()
      batch_xs <- batchT$train[,-targetN]
      batch_ys <- batchT$train[,targetN, drop = F]
      if(correct) check = sess$run(final, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP), C = batchT$counts))
      else check = sess$run(final, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP), C = matrix(1., nrow = nrow(batchT$train))))
      if(!any(is.infinite(check))) break
    }

    if(correct) sess$run(optimizer, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP), C = batchT$counts))
    else sess$run(optimizer, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP), C = matrix(1., nrow = nrow(batchT$train))))

    if(step %% as.integer(nrow(data)/mean(sapply(1:20, function(x) nrow(generator()$train))))  == 0){
      counter = counter + 1
      if(correct) lossE = sess$run(loss, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP), C = batchT$counts))
      else lossE = sess$run(loss, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP), C = matrix(1., nrow = nrow(batchT$train))))
      if(counter > 10){
        if(lossE < bestLoss) {
          bestLoss = lossE
          breakCounter = 0
        }
        else breakCounter = breakCounter + 1

        if(breakCounter == 10) break
      }
      cat("\nEpoch:", step/(nrow(data)/batch), ", loss = ", lossE)
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
  tryCatch(keras::backend()$clear_session(), error = function(x) return(NULL))
  tryCatch(keras::reset_states(model), error = function(x) return(NULL))
  tryCatch(keras::k_reset_uids(model), error = function(x) return(NULL))
  model = keras_model_sequential()
  model %>% layer_dense(input_shape = c(NULL, nInp), units =as.integer(archLayer[1]) ,use_bias = bias,activation = "relu") %>% layer_dropout(rate = .drop)
  if(nLayer != 0) {
    for(i in 1:nLayer) model %>% layer_dense(as.integer(archLayer[i+1]), activation = "relu",use_bias = bias) %>%layer_dropout(rate = .drop)
  }
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
predictLearner.regr.multiNomDnn = function(.learner, .model, .newdata, ...){
  require(keras)
  data <- .newdata
  m    <- keras::unserialize_model(.model$learner.model$model$model)
  p = exp(predict(m, x = as.matrix(data[,-c(1,2)])))
  if(.model$learner.model$model$expValue) p = log(p+1)
  return(p[,1])

}
