
#' Wide and deep learner
#' @export

makeRLearner.classif.wideDeep <- function(){
  mlr::makeRLearnerClassif(
    cl = "classif.wideDeep",
    package = "keras",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerLearnerParam(id = "epochs", lower = 0, default = 50L),
      ParamHelpers::makeNumericLearnerParam(id = "drop", lower = 0.1, default = 0.6),
      ParamHelpers::makeIntegerLearnerParam(id = "arch", lower = 10, default = 30L),
      ParamHelpers::makeIntegerLearnerParam(id = "nLayer", lower = 2L, default = 6L),
      ParamHelpers::makeIntegerLearnerParam(id = "embedding", lower = 3, 50),
      ParamHelpers::makeDiscreteLearnerParam(id = "archFunction", values = c("negExp", "parable", "continous"))
    ),
    properties = c("twoclass", "numerics", "factors", "prob"),
    name       = "WideDeep network",
    short.name = "WideDeep",
    note       = "test...."


  )
}
#'  Keras_conv train function
#'
#' @export

trainLearner.classif.wideDeep <- function(.learner, .task, .subset, .weights = NULL, archFunction = "negExp", nLayer = 2L, epochs = 200, arch = 30L,drop = 0.25,embedding=8, ...){
  data <- mlr::getTaskData(.task, .subset)

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

  dnn_hidden = archLayer
  #error activation


  data$label <- ifelse(data$target == "X",1,0)
  data <- data[,-16]
  # y <-as.matrix( mlr::createDummyFeatures(data[,18]))
  for(i in 1:15){
    if(is.factor(data[,i])) data[,i] <- as.character(data[,i])
  }

  require(tfestimators)

  type <- column_categorical_with_vocabulary_list("type", vocabulary_list = unique(data$type))
  season <- column_categorical_with_vocabulary_list("season", vocabulary_list = unique(data$season))
  corolla <- column_categorical_with_vocabulary_list("corolla", vocabulary_list = unique(data$corolla))
  nectar <- column_categorical_with_vocabulary_list("nectar", vocabulary_list = unique(data$nectar))
  b.system<- column_categorical_with_vocabulary_list("b.system", vocabulary_list = unique(data$b.system))
  s.pollination <- column_categorical_with_vocabulary_list("s.pollination", vocabulary_list = unique(data$s.pollination))
  inflorescence <- column_categorical_with_vocabulary_list("inflorescence", vocabulary_list = unique(data$inflorescence))
  composite <- column_categorical_with_vocabulary_list("composite", vocabulary_list = unique(data$composite))
  guild <- column_categorical_with_vocabulary_list("guild", vocabulary_list = unique(data$guild))
  sociality <- column_categorical_with_vocabulary_list("sociality", vocabulary_list = unique(data$sociality))
  feeding <- column_categorical_with_vocabulary_list("feeding", vocabulary_list = unique(data$feeding))
  colour <- column_categorical_with_vocabulary_list("colour", vocabulary_list = unique(data$colour))


  diameter <- column_numeric("diameter")
  body <- column_numeric("body")
  tongue <- column_numeric("tongue")
  Freq1 <- column_numeric("Freq1")
  Freq2 <- column_numeric("Freq2")


  crossed_columns <- feature_columns(
    guild, sociality, feeding, corolla, body,diameter, tongue,inflorescence,colour, nectar, b.system, s.pollination, composite,Freq1,Freq2,
    column_crossed(c("body", "guild"), hash_bucket_size = 10000),
    column_crossed(c("tongue","diameter","body"), hash_bucket_size = 10000),
    column_crossed(c("guild","sociality"), hash_bucket_size = 10000),
    column_crossed(c("s.pollination","sociality"), hash_bucket_size = 10000),
    column_crossed(c("inflorescence","tongue","feeding"), hash_bucket_size = 10000),
    column_crossed(c("colour","guild","corolla"), hash_bucket_size = 10000),
    column_crossed(c("guild","nectar"), hash_bucket_size = 10000),
    column_crossed(c("Freq1","Freq2"))
  )

  deep_columns <- feature_columns(
    column_embedding(type, dimension = embedding),
    column_embedding(season, dimension = embedding),
    column_embedding(corolla, dimension = embedding),
    column_embedding(colour, dimension = embedding),
    column_embedding(nectar, dimension = embedding),
    column_embedding(b.system, dimension = embedding),
    column_embedding(s.pollination, dimension = embedding),
    column_embedding(inflorescence, dimension = embedding),
    column_embedding(composite, dimension = embedding),
    column_embedding(guild, dimension = embedding),
    column_embedding(sociality, dimension = embedding),
    column_embedding(feeding, dimension = embedding),
    diameter,
    body,
    tongue,
    Freq1,
    Freq2
  )


  model <- dnn_linear_combined_classifier(
    linear_feature_columns = crossed_columns,
    dnn_feature_columns = deep_columns,
    dnn_hidden_units = dnn_hidden,
    dnn_activation_fn = "elu",
    dnn_dropout = 0.3,
    n_classes = 2
  )
  constructed_input_fn <- function(dataset) {
    input_fn(dataset, features = -label, response = label, num_epochs = epochs)
  }
  train_input_fn <- constructed_input_fn(data)
  #eval_input_fn <- constructed_input_fn(x_test)

  tfestimators::train(model, input_fn = train_input_fn)

  out = list()
  out$model = model_dir(model)
  out$crossed_columns = crossed_columns
  out$deep_columns = deep_columns
  out$dnn_hidden_units = dnn_hidden
  #   library(tfestimators)
  # linear_feature_columns <- feature_columns(column_numeric("mpg"))
  # dnn_feature_columns <- feature_columns(column_numeric("drat"))
  #
  # loaded_model <-
  #   dnn_linear_combined_classifier(
  #     linear_feature_columns = linear_feature_columns,
  #     dnn_feature_columns = dnn_feature_columns,
  #     dnn_hidden_units = c(3, 3),
  #     dnn_optimizer = "Adagrad",
  #     model_dir = saved_model_dir
  #   )
  # loaded_model
  return(out)
}







#'PredictLearner for wideDeep implemantation into mlr
#'
#'@export



predictLearner.classif.wideDeep = function(.learner, .model, .newdata, split = c(20,20), ...){
  data <- .newdata
  require(tfestimators)
  model <-
      dnn_linear_combined_classifier(
        linear_feature_columns = .model$learner.model$crossed_columns,
        dnn_feature_columns = .model$learner.model$deep_columns,
        dnn_hidden_units = .model$learner.model$dnn_hidden,
        dnn_optimizer = "Adagrad",
        model_dir = .model$learner.model$model,
        n_classes = 2
      )
#
#   data$label <- ifelse(data$target == "X",1,0)
#   data <- data[,-16]
  # y <-as.matrix( mlr::createDummyFeatures(data[,18]))
  for(i in 1:17){
    if(is.factor(data[,i])) data[,i] <- as.character(data[,i])
  }

  type = ifelse(.learner$predict.type == "response", "response", "prob")
  constructed_input_fn <- function(dataset) {
    input_fn(dataset , features = colnames(data), num_epochs = 1)
  }
  eval_input_fn <- constructed_input_fn(data)

  p = predict(model, input_fn = eval_input_fn)
  p <- as.data.frame(p)
  p = cbind(1-unlist(p$logistic), unlist(p$logistic))

  colnames(p) <- c("o", "X")
  attributes(p) <- list(dim = dim(p),
                        dimnames = list(NULL,c("o", "X")),
                        class = "matrix")
  return(p)


}
