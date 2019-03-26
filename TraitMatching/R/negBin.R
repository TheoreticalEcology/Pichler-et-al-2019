
#' Keras_seq learner
#'
#' @export


makeRLearner.regr.negBinDnn <- function(){
  mlr::makeRLearnerRegr(
    cl = "regr.negBinDnn",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 1, default = 200L),
      ParamHelpers::makeNumericLearnerParam(id = "lr", lower = 0.000001, default = 0.1),
      ParamHelpers::makeDiscreteLearnerParam(id = "opti", values = c( "adam", "rmsprop"), default = "rmsprop"),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 5, default = 150L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 1L, default = 10L),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"), default = "negExp"),
      ParamHelpers::makeLogicalLearnerParam(id = "bias", default = T ),
      ParamHelpers::makeIntegerLearnerParam(id = "batch", lower = 2, default = 40),
      ParamHelpers::makeIntegerLearnerParam(id = "seed",  tunable = F,default = 42),
      ParamHelpers::makeLogicalLearnerParam(id = "expValue", default = F,tunable = F),
      ParamHelpers::makeDiscreteLearnerParam(id = "distribution", default = "poisson", tunable = T,values = c("poisson", "negBin")),
      ParamHelpers::makeDiscreteLearnerParam(id = "activation", default = "relu", values = c("relu", "tanh")),
      ParamHelpers::makeLogicalLearnerParam(id = "early_stopping", default = T),
      ParamHelpers::makeLogicalLearnerParam(id = "Link", default = TRUE)


    ),
    properties = c("numerics", "factors"),
    name       = "Keras/TensorFlow DNN network with negBin loss function",
    short.name = "negBinDnn",
    note       = ""


  )
}
#'  Keras_seq train function
#'
#' @export

trainLearner.regr.negBinDnn <- function(.learner, .task, .subset, .weights = NULL, batch = 25L,
                                        archFunction = "negExp",bias = T,lr = 0.01,
                                        epochs = 200,nLayer = 10,
                                        opti = "rmsprop",  arch = 30L,seed = NULL,expValue = F,
                                        distribution = "poisson", activation = "relu",
                                        early_stopping = TRUE,
                                        Link = TRUE,...){
  data <- mlr::getTaskData(.task, .subset)
  .drop = drop

  minBatch = nrow(data)
  if(batch > minBatch ) batch = minBatch

  require(tensorflow)
  tf$reset_default_graph()


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
  dropP = .drop
  #nLayer = nLayer - 1
  if(expValue) data[,.task$task.desc$target] = exp(data[,.task$task.desc$target]) - 1



  try({keras::backend()$clear_session()}, silent = TRUE)
  try({keras::reset_states(model)}, silent = TRUE)
  try({keras::k_reset_uids(model)}, silent = TRUE)



  require(keras)
  require(tensorflow)
  tfp = reticulate::import("tensorflow_probability")
  dist = tfp$distributions


  y = data$target
  data = data[,colnames(data) != .task$task.desc$target]
  output = list()

  # nLayer = 3
  # archLayer = c(30,30,30,30)
  # activation = "relu"
  # bias = TRUE
  # opti = "rmsprop"
  # lr = 0.0005
  # distribution = "negBin"
  # y = data$target
  # data = data[,-13]
  # decay = 0.99
  # Link = TRUE
  # batch = 10L
  # epochs = 10L

  model = keras_model_sequential()
  if(activation %in% c("relu", "tanh")){
    model = keras_model_sequential()
    for(i in 1:nLayer){
      if(i == 1) model %>% layer_dense(units = archLayer[i], input_shape = ncol(data), use_bias = bias)
      else model %>% layer_dense(units = archLayer[i], use_bias = bias)
      model %>% layer_activation(activation)
      model %>% layer_batch_normalization()
    }
  } else {
    model = keras_model_sequential()
    for(i in 1:nLayer){
      if(i == 1) model %>% layer_dense(units = archLayer[i],  input_shape = ncol(data), use_bias = bias)
      else model %>% layer_dense(units = archLayer[i], use_bias = bias)
      model %>% layer_activation_leaky_relu(alpha = alpha)
      model %>% layer_batch_normalization()
    }
  }



  if(Link) model %>% layer_dense(units = 1L, activation = "linear")
  else model %>% layer_dense(units = 1L, activation = "relu")

  eps = tf$constant(.Machine$double.eps, tf$float32)

  if(distribution=="negBin"){


    model$layers[[length(model$layers)]]$add_weight(name = 'thetaX',
                                                    shape = list(),
                                                    initializer = initializer_constant(0.5),
                                                    trainable = TRUE)




    self_loss = function(y_true, y_pred){
      y_hat = tf$exp(y_pred)
      theta_0 = tf$get_default_graph()$get_tensor_by_name("thetaX:0")
      theta = tf$div(k_constant(1.0, k_floatx()),(k_softplus(theta_0) + eps))
      probs = k_constant(1., k_floatx()) - tf$div(theta , (theta+y_hat))+ eps
      final = dist$NegativeBinomial(total_count = theta, probs = probs)$log_prob(y_true)
      return(k_mean(-final))
    }
  } else {
    self_loss = function(y_true, y_pred){
      y_hat = tf$exp(y_pred)
      final = dist$Poisson(rate = y_hat+eps)$log_prob(y_true)
      return(k_mean(-final))
    }
  }


  if(opti == "rmsprop") optimizer = keras::optimizer_rmsprop(lr = lr)
  if(opti == "adam") optimizer = keras::optimizer_adam(lr = lr)

  model %>% compile(loss = self_loss,
                    optimizer = optimizer
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
        y = matrix(y,ncol = 1, byrow = T),
        epochs = epochs,
        batch_size = batch,
        shuffle = T
    )


  if(distribution == "negBin"){

    weights = lapply(1:length(model$layers), function(x) model$layers[[x]]$get_weights())
    whichTheta = which(sapply(model$layers[[length(model$layers)]]$weights, function(x) x$name)=="thetaX:0",arr.ind = T)
    theta = k_get_value(k_softplus(weights[[length(model$layers)]][[whichTheta]]))^-1

    weights[[length(model$layers)]][[whichTheta]] = NULL


    .e = tryCatch(keras::backend()$clear_session(), error = function(x) return(NULL))
    .e = tryCatch(keras::reset_states(model), error = function(x) return(NULL))
    .e = tryCatch(keras::k_reset_uids(model), error = function(x) return(NULL))


    model = keras_model_sequential()
    if(activation %in% c("relu", "tanh")){
      model = keras_model_sequential()
      for(i in 1:nLayer){
        if(i == 1) model %>% layer_dense(units = archLayer[i], input_shape = ncol(data), use_bias = bias)
        else model %>% layer_dense(units = archLayer[i], use_bias = bias)
        model %>% layer_activation(activation)
        model %>% layer_batch_normalization()
      }
    } else {
      model = keras_model_sequential()
      for(i in 1:nLayer){
        if(i == 1) model %>% layer_dense(units = archLayer[i],  input_shape = ncol(data), use_bias = bias)
        else model %>% layer_dense(units = archLayer[i], use_bias = bias)
        model %>% layer_activation_leaky_relu(alpha = alpha)
        model %>% layer_batch_normalization()
      }
    }
    if(Link) model %>% layer_dense(units = 1L, activation = "linear")
    else model %>% layer_dense(units = 1L, activation = "relu")

    for(i in 1:length(model$layers)) keras::set_weights(model$layers[[i]], weights[[i]])
  }


  #unserialize_model(serialize_model(model,include_optimizer = F))


  # freq = (table(data[,.task$task.desc$target]))
  #
  # generator <- function(batchG = batch){
  #
  #   if(weighted_resampling) ind = sample.int(nrow(data), batchG, prob = as.vector(freq[as.character(data[,.task$task.desc$target])])^-1)
  #   else ind = sample.int(nrow(data), batchG)
  #   out = as.matrix(data[ind,])
  #
  #   return(out)
  # }
  #
  #
  # # generator <- function(batchG = batch){
  # #   return(as.matrix(data[sample.int(nrow(data), batchG),]))
  # # }
  #
  # ################ Inputs and Outpus ########
  #
  # nInp = as.integer(ncol(generator())-1)
  # x = tf$placeholder(tf$float64, shape(NULL, nInp))
  # y = tf$placeholder(tf$float64, shape(NULL, 1L))
  # drop = tf$placeholder(tf$float64)
  #
  #
  # ################ Model #####################
  # net = function(x,  drop){
  #   first = tf$layers$dense(x,units = as.integer(archLayer[1]),activation = activation1, use_bias = bias,name = "layer_1")
  #   second = tf$nn$dropout(first_b, keep_prob = drop)
  #   if(nLayer != 0) {
  #     for(i in 1:(nLayer)){
  #       second = tf$layers$dense(second, units = as.integer(archLayer[i+1]), activation = activation1, use_bias = bias, name = paste0("layer_",i+1),kernel_regularizer = tf$contrib$layers$l2_regularizer(l2))
  #       second = tf$nn$dropout(second, keep_prob = drop)
  #     }
  #   }
  #   if(Link) out = tf$layers$dense(second, units = 1L, activation = NULL, name = paste0("layer_",nLayer+2),use_bias = bias,kernel_regularizer = tf$contrib$layers$l2_regularizer(l2))
  #   else out = tf$layers$dense(second, units = 1L, activation = activation1, name = paste0("layer_",nLayer+2),use_bias = bias,kernel_regularizer = tf$contrib$layers$l2_regularizer(l2))
  #
  #   return(out)
  # }
  #
  # eps = tf$constant(.Machine$double.eps, tf$float64)
  #
  # pred = net(x,  drop)
  # y_true = tf$cast(y, tf$float64)
  # if(Link) y_pred = tf$exp(tf$cast(pred, tf$float64) )
  # else y_pred = tf$cast(pred, tf$float64) + eps
  # #############################################
  #
  #
  #
  # ################# Loss ######################
  # tfp = reticulate::import("tensorflow_probability")
  # dist = tfp$distributions
  # # negBinom loss function
  # if(distribution == "negBin"){
  #
  #   thetaO = tf$cast(tf$Variable(0., tf$float64), tf$float64)
  #   thetaV = tf$nn$softplus(thetaO)+eps
  #   theta  = tf$div(tf$constant(1.0, tf$float64),tf$add(thetaV,eps))
  #
  #
  #  # tfp = reticulate::import("tensorflow_probability")
  #   #dist = tfp$distributions
  #
  #   probs = tf$constant(1., tf$float64) - tf$div(theta, (theta+y_pred))+ eps
  #
  #   ll = dist$NegativeBinomial(total_count = theta, probs = probs)$log_prob(y_true)
  #   # loss = tf$reduce_mean(l1)
  #
  #
  #   final = tf$negative(ll)
  #   loss   = tf$reduce_mean(tf$boolean_mask(final, tf$logical_not(tf$is_inf(final))))
  #
  # } else {
  #   #dnorm(0.8,dpois(15,0+0.0001,log = TRUE),sd = 0.5,log = T)
  #
  #   #final  = y_pred - y_true*tf$log(y_pred+eps) + tf$lgamma(y_true+1.0)
  #
  #   ll = dist$Poisson(y_pred+eps)$log_prob(y_true)
  #   final = tf$negative(dist$Normal(tf$constant(0., tf$float64), 0.5)$log_prob(ll))
  #
  #   loss   = tf$reduce_mean(final)
  # }
  #
  #
  # optimizer = switch(opti,
  #                    rmsprop = {
  #                      tf$train$RMSPropOptimizer(learning_rate = lr)$minimize((loss))
  #                    },
  #                    adam = {
  #                      tf$train$AdamOptimizer(learning_rate = lr)$minimize((loss))
  #                    })
  # ############################################
  #
  #
  # # init variables and start session
  # init = tf$global_variables_initializer()
  # sess = tf$Session()
  # sess$run(init)
  #
  # # prepare training
  # step = 1
  # stepToGo = as.integer(nrow(data) / batch * epochs)
  # targetN = which(colnames(generator()) == .task$task.desc$target, arr.ind = T)
  # counter = 1
  # bestLoss = Inf
  # bestLL = Inf
  # # training
  # while(step <= stepToGo) {
  #   repeat{
  #     batchT = generator()
  #     batch_xs <- batchT[,-targetN,drop = F]
  #     batch_ys <- batchT[,targetN, drop = F]
  #     check = sess$run(final, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP)))
  #     if(!any(is.infinite(check))) break
  #   }
  #   batchT = generator()
  #   batch_xs <- batchT[,-targetN, drop = F]
  #   batch_ys <- batchT[,targetN, drop = F]
  #   sess$run(optimizer, feed_dict = dict(x = batch_xs, y = batch_ys, drop = (1-dropP)))
  #
  #   if(step %% as.integer((nrow(data)/batch))  == 0){
  #     counter = counter + 1
  #     ll =  sess$run(final, feed_dict = dict(x = as.matrix(data)[,-targetN,drop = F]
  #                                            ,y = as.matrix(data)[,targetN,drop = F], drop = (1-dropP)))
  #     if(checkpoint_best){
  #
  #       if(sum(ll) < bestLL){
  #         bestLL = mean(ll)
  #
  #         weights = vector("list",nLayer)
  #         if(bias){
  #           for(i in 1:(nLayer+2)) weights[[i]] = list(sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/kernel:0"))),
  #                                                      sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/bias:0"))))
  #         } else {
  #           for(i in 1:(nLayer+2)) weights[[i]] = list(sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/kernel:0"))))
  #         }
  #         cat("\nnew best LL: ", sum(ll))
  #       }
  #     }
  #     lossE = sum(ll)
  #     if(early_stopping){
  #       if(counter > 20){
  #         if(lossE < bestLoss) {
  #           bestLoss = lossE
  #           breakCounter = 0
  #         }
  #         else breakCounter = breakCounter + 1
  #
  #         if(breakCounter == 10) break
  #       }
  #     }
  #     cat("\nEpoch:", step/as.integer((nrow(data)/batch)), ", loss = ", lossE)
  #   }
  #   step = step +1
  # }
  # if(distribution == "negBin") thetaOut = sess$run(theta, feed_dict = dict(x = batch_xs, y = batch_ys, drop = 1.))
  #
  # ## export as keras model, easier for predictionL
  # if(!checkpoint_best){
  #   weights = vector("list",nLayer)
  #   if(bias){
  #     for(i in 1:(nLayer+2)) weights[[i]] = list(sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/kernel:0"))), sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/bias:0"))))
  #   } else {
  #     for(i in 1:(nLayer+2)) weights[[i]] = list(sess$run(tf$get_default_graph()$get_tensor_by_name(paste0("layer_",i,"/kernel:0"))))
  #   }
  # }
  # tf$reset_default_graph()
  # sess$close()
  #
  # require(keras)
  # tryCatch(keras::backend()$clear_session(), error = function(x) return(NULL))
  # tryCatch(keras::reset_states(model), error = function(x) return(NULL))
  # tryCatch(keras::k_reset_uids(model), error = function(x) return(NULL))
  # model = keras_model_sequential()
  # model %>% layer_dense(input_shape = c(NULL, nInp), units =as.integer(archLayer[1]) ,use_bias = bias,activation = activation) %>% layer_dropout(rate = .drop)
  # if(nLayer != 0) {
  #   for(i in 1:nLayer) model %>% layer_dense(as.integer(archLayer[i+1]), activation = activation,use_bias = bias) %>%layer_dropout(rate = .drop)
  # }
  # model %>%  layer_dense(1L, activation = "linear",use_bias = bias)
  #
  # # update keras weights with trained weights
  # counter = 1
  # for(i in 1:(2*(nLayer+2))) {
  #   if(!i%%2 == 0) {
  #     keras::set_weights(object = model$layers[[i]], weights[[counter]])
  #     counter = counter + 1
  #   }
  # }

  model %>%
    compile(loss = "mean_squared_error",optimizer = "adam")
  output$model$model <- keras::serialize_model(model,include_optimizer = FALSE)
  output$model$expValue = expValue
  output$model$Link = Link
  if(distribution == "negBin") output$model$theta = theta
  try(detach("package:keras", unload=TRUE), silent = T)
  try(detach("package:tensorflow", unload=TRUE), silent = T)
  return(output)
}








#'PredictLearner for keras_seq implemantation into mlr
#'
#'@export
#'
#'
predictLearner.regr.negBinDnn = function(.learner, .model, .newdata, ...){
  require(keras)
  data <- .newdata
  m    <- keras::unserialize_model(.model$learner.model$model$model)
  if(.model$learner.model$model$Link) p = exp(predict(m, x = as.matrix(data[,.model$features])))
  else p = predict(m, x = as.matrix(data[,.model$features]))
  if(.model$learner.model$model$expValue) p = log(p+1)
  return(p[,1])

}
