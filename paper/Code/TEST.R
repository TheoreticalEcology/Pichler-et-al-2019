library(TraitMatching)
library(tidyverse)

inH = read.csv("./PlantBird/interact_matrix_high.csv")
inM = read.csv("./PlantBird/interact_matrix_mid.csv")
inL = read.csv("./PlantBird/interact_matrix_low.csv")

tbH = read.csv("./PlantBird/traitbird_high.csv",col.names = c("Hummingbird", "Bill.Length", "Bill.Curvature", "Body.Mass", "Wing", "Tail")) %>% normalizeFeatures(method = "center")
tbM = read.csv("./PlantBird/traitbird_mid.csv",col.names = c("Hummingbird", "Bill.Length", "Bill.Curvature", "Body.Mass", "Wing", "Tail")) %>% normalizeFeatures(method = "center")
tbL = read.csv("./PlantBird/traitbird_low.csv",col.names = c("Hummingbird", "Bill.Length", "Bill.Curvature", "Body.Mass", "Wing", "Tail")) %>% normalizeFeatures(method = "center")

tpH = read.csv("./PlantBird/traitplant_high.csv", col.names = c("Plant", "Corolla.Length", "Corolla.Curvature", "Corolla.Volumen", "Inner.Diameter", "Ext.Diameter", "NumIndividuals")) %>% normalizeFeatures(method = "center", cols = c("Corolla.Length", "Corolla.Curvature", "Corolla.Volumen", "Inner.Diameter", "Ext.Diameter"))
tpM = read.csv("./PlantBird/traitplant_mid.csv", col.names = c("Plant", "Corolla.Length", "Corolla.Curvature", "Corolla.Volumen", "Inner.Diameter", "Ext.Diameter", "NumIndividuals")) %>% normalizeFeatures(method = "center", cols = c("Corolla.Length", "Corolla.Curvature", "Corolla.Volumen", "Inner.Diameter", "Ext.Diameter"))
tpL = read.csv("./PlantBird/traitplant_low.csv", col.names = c("Plant", "Corolla.Length", "Corolla.Curvature", "Corolla.Volumen", "Inner.Diameter", "Ext.Diameter", "NumIndividuals")) %>% normalizeFeatures(method = "center", cols = c("Corolla.Length", "Corolla.Curvature", "Corolla.Volumen", "Inner.Diameter", "Ext.Diameter"))


rownames(inH) = as.character(inH$X)
inH = inH[,-1]
rownames(inM) = as.character(inM$X)
inM = inM[,-1]
rownames(inL) = as.character(inL$X)
inL = inL[,-1]





### Train models
Create combinations of networks and fit learner in 60 tuning steps with three-fold cross-validation

if(run){
  tbH_c = tbH
  tbM_c = tbM
  tbL_c = tbL
  tbH_c$plot = as.factor(rep("High", nrow(tbH)))
  tbM_c$plot = as.factor(rep("Mid", nrow(tbM)))
  tbL_c$plot = as.factor(rep("Low", nrow(tbL)))
  
  communityCombined =  createCommunity(community = list(list(tpH, tbH_c, ifelse(inH>0,1,0)), list(tpM, tbM_c, ifelse(inM>0,1,0)), list(tpL, tbL_c, ifelse(inL>0,1,0))),log = F)
  communityCombined$data = communityCombined$data  %>% 
    select(-NumIndividuals) %>% createDummyFeatures(cols = "plot") 
  
  
  Low = LowMid = LowHigh = MidHigh = High = Mid = all =  communityCombined
  Low$data = communityCombined$data[communityCombined$data$plot.Low > 0 ,1:13 ] %>% normalizeFeatures(target = "target")
  Low$block = communityCombined$block[communityCombined$data$plot.Low > 0 ,]
  
  LowMid$data = communityCombined$data[communityCombined$data$plot.Low > 0 | communityCombined$data$plot.Mid > 0,c(1:13)] %>% normalizeFeatures(target = "target")
  LowMid$block = communityCombined$block[communityCombined$data$plot.Low > 0 | communityCombined$data$plot.Mid > 0,]
  
  LowHigh$data = communityCombined$data[communityCombined$data$plot.Low > 0 | communityCombined$data$plot.High > 0,c(1:13)] %>% normalizeFeatures(target = "target")
  LowHigh$block = communityCombined$block[communityCombined$data$plot.Low > 0 | communityCombined$data$plot.High > 0,]
  
  MidHigh$data = communityCombined$data[communityCombined$data$plot.Mid > 0 | communityCombined$data$plot.High > 0,c(1:13)] %>% normalizeFeatures(target = "target")
  MidHigh$block = communityCombined$block[communityCombined$data$plot.Mid > 0 | communityCombined$data$plot.High > 0,]
  
  High$data = communityCombined$data[communityCombined$data$plot.High > 0 ,1:13 ] %>% normalizeFeatures(target = "target")
  High$block = communityCombined$block[communityCombined$data$plot.High > 0 ,]
  
  Mid$data = communityCombined$data[communityCombined$data$plot.Mid > 0 ,1:13 ] %>% normalizeFeatures(target = "target")
  Mid$block = communityCombined$block[communityCombined$data$plot.Mid > 0 ,]
  
  all$data = communityCombined$data %>% normalizeFeatures(target = "target")
  all$block = communityCombined$block[,]
  
  community = list(Low = Low,
                   Mid = Mid,
                   High = High,
                   MidHigh = MidHigh,
                   LowMid = LowMid,
                   LowHigh = LowHigh,
                   all = all
  )
  
  
  result = vector("list", 7)
  
  library(TraitMatching)
  for(i in 1:2) result[[i]] = runTM(community[[i]],
                                    method = c("dnn", "RF", "boost"), 
                                    settings = list(dnn = list(seed = 42,  activation = "relu",drop = 0.3, archFunction = "continous", epochs = 80)),
                                    tune = "random", iters = 1,  fitSpecies = F,
                                    crossValidation = list(outer = list(method = "SpCV", iters = 5, predict = "both"), 
                                                           inner = list(method = "SpCV", iters = 3)), 
                                    balanceClasses = "None", block = T, parallel = F, seed = 42, keepModels = T)
  
  
  
  
  # two DNN: Poisson and negative binomial distributions
  
  learnerNegBinDnn = makeLearner("regr.negBinDnn",seed = 42, drop = 0.20,batch = 25,archFunction = "continous", distribution = "negBin")
  learnerPoissonDnn = makeLearner("regr.negBinDnn",seed = 42, drop = 0.20,batch = 25,archFunction = "continous", distribution = "poisson")
  learnerRF = makeLearner("regr.randomForest", predict.type = "se")
  learnerBoost = makeLearner("regr.xgboost", objective = "count:poisson")
  parDnn = makeParamSet(makeNumericParam("lr", lower = 0.0001, upper = 0.01),
                        makeIntegerParam("arch", lower = 5L, upper = 15L),
                        makeIntegerParam("nLayer", 1, 5),
                        makeLogicalParam("bias", default = T)
  )
  
  parRF = makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 10),         
                       makeIntegerParam("nodesize",lower = 2,upper = 50))
  parBoost <- makeParamSet(makeDiscreteParam("booster", values = c("gbtree", "dart")),
                           makeDiscreteParam("sample_type", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
                           makeDiscreteParam("normalize_type", values = c("tree", "forest"), requires = quote(booster == "dart")),
                           makeNumericParam("eta", lower = 0.01, upper = 0.5),
                           makeNumericParam("gamma", lower = -9, upper = 5, trafo = function(x) 2^x),
                           makeIntegerParam("max_depth", lower = 1, upper = 10),
                           makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x),
                           makeNumericParam("alpha", lower = -10, upper = 5, trafo = function(x) 2^x),
                           makeNumericParam("min_child_weight", 0, 10),
                           makeIntegerParam("nrounds", lower = 20, upper = 500),
                           makeNumericParam("rate_drop", lower = 0, upper = 0.2, requires = quote(booster == "dart")),
                           makeNumericParam("skip_drop", lower = 0, upper = 0.3, requires = quote(booster == "dart")))
  
  
  
  modelNegBin = vector("list", 7)
  modelRF = vector("list", 7)
  modelPoisson= vector("list", 7)
  modelBoost = vector("list",7)
  parallelMap::parallelStartSocket(cpus = 30, level = "mlr.tuneParams")
  predictLearner.regr.negBinDnn = TraitMatching:::predictLearner.regr.negBinDnn
  trainLearner.regr.negBinDnn = TraitMatching:::trainLearner.regr.negBinDnn
  parallelMap::parallelExport("trainLearner.regr.negBinDnn", "predictLearner.regr.negBinDnn", level = "mlr.tuneParams")
  
  
  
  for(i in 1:7){
    set.seed(42)
    task = makeRegrTask(data = community[[i]]$data[,-c(1,2)], target = "target", coordinates = community[[i]]$block)
    
    modelNegBin[[i]] = mlr::train(setHyperPars(learnerNegBinDnn,par.vals =  tuneParams(learnerNegBinDnn, par.set = parDnn,
                                                                                       control = makeTuneControlRandom(maxit = 60),measures = spearmanrho,show.info = T,
                                                                                       task = task, resampling = makeResampleDesc("SpCV", iters = 3))$x)
                                  ,task = task)
    
    modelPoisson[[i]] = mlr::train(setHyperPars(learnerPoissonDnn,par.vals =  tuneParams(learnerNegBinDnn, par.set = parDnn,
                                                                                         control = makeTuneControlRandom(maxit = 60),measures = spearmanrho,show.info = T,
                                                                                         task = task, resampling = makeResampleDesc("SpCV", iters = 3))$x)
                                   ,task = task)
    
    modelRF[[i]] = mlr::train(setHyperPars(learnerRF,par.vals =  tuneParams(learnerRF, par.set = parRF,
                                                                            control = makeTuneControlRandom(maxit = 60),measures = spearmanrho,show.info = T,
                                                                            task = task, resampling = makeResampleDesc("SpCV", iters = 3))$x)
                              ,task = task)
    modelBoost[[i]] = mlr::train(setHyperPars(learnerBoost,par.vals =  tuneParams(learnerBoost, par.set = parBoost,
                                                                                  control = makeTuneControlRandom(maxit = 60),measures = spearmanrho,show.info = T,
                                                                                  task = task, resampling = makeResampleDesc("SpCV", iters = 3))$x)
                                 ,task = task)
    
    
  }
  names(modelNegBin) = names(community)
  names(modelPoisson) = names(community)
  names(modelRF) = names(community)
  names(modelBoost) = names(community)
  
  save(modelRF, modelBoost, modelNegBin, modelPoisson, community, file = "./Results/HummingBirdResult.RData")
}


```