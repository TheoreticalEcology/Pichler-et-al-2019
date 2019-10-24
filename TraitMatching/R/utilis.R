.onLoad = function(libname, pkgname){
  require(mlr, quietly = TRUE)
}

#' @export
poisson.fun = function(task, model, pred, feats, extra.args) {


  true = getPredictionTruth(pred)
  predictions = getPredictionResponse(pred)

  eps = .Machine$double.eps
  res = mean(-2*dpois(true, predictions, log = T))
  if(is.na(res)) return(Inf)
  else return(res)
}



#' @export
poisson = mlr::makeMeasure(
  id = "poisson", name = "poisson",
  properties = c("regr",  "req.model", "req.task", "req.pred"),
  minimize = TRUE, best = 0, worst = +Inf,
  fun = poisson.fun
)



#' @export
poissonQuasi.fun = function(task, model, pred, feats, extra.args) {
  true = getPredictionTruth(pred)
  predictions = getPredictionResponse(pred)
  eps = .Machine$double.eps
  l1 = dpois(true, predictions, log = T)
  ll = dnorm(0,l1,0.5, log = TRUE)

  res = mean(abs(ll))
  if(is.na(res)) return(Inf)
  else return(res)
}



#' @export
poissonQuasi = mlr::makeMeasure(
  id = "poissonQuasi", name = "poissonQuasi",
  properties = c("regr", "req.pred", "req.truth"),
  minimize = TRUE, best = 0, worst = +Inf,
  fun = poissonQuasi.fun
)





#' @export
negBinLL.fun = function(task, model, pred, feats, extra.args) {
  true = getPredictionTruth(pred)
  predictions = getPredictionResponse(pred)

  theta = model$learner.model$model$theta
  return(sum(-2*dnbinom(true, mu = predictions, size = theta, log = T)))
}



#' @export
negBinLL = mlr::makeMeasure(
  id = "negBinLL", name = "negBinLL",
  properties = c("regr", "req.pred", "req.truth"),
  minimize = TRUE, best = 0, worst = +Inf,
  fun = negBinLL.fun
)








#' combineResults
#' combine Results
#' @param res list of results

combineResults = function(res = NULL){
  Result = list()
  Result$classCommunity = res[[1]]$classCommunity
  Result$settings = res[[1]]$settings
  Result$Result = list("Over" = list(),
                       "Under" = list(),
                       "SMOTE" = list(),
                       "WeightedClasses" = list())
  for(n in names(Result$Result)){
    for(i in 1:length(res)){
      Result$Result[[n]] = c(Result$Result[[n]],res[[i]]$Result[[n]] )
    }
  }
  class(Result) = "TMmodel"

  return(Result)
}

#' @export
measureTTS = function(true, predictions, pos, neg) {


  TP = measureTP(true, predictions,pos)
  TN = measureTN(true, predictions,neg)
  FP = measureFP(true, predictions,pos)
  FN = measureFN(true, predictions,neg)

  TSS = (TP*TN - FP*FN)/((TP+FN)*(FP+TN))

  return(TSS)
}


#' @export
tss = mlr::makeMeasure(
  id = "tss", name = "tss",
  properties = c("classif", "req.pred", "req.truth"),
  minimize = FALSE, best = 1, worst = -1,
  fun = function(task, model, pred, feats, extra.args){
    true = pred$data$truth
    predictions = pred$data$response
    pos = pred$task.desc$positive
    measureTTS(true = pred$data$truth,predictions = pred$data$response,pos = pred$task.desc$positive, neg = pred$task.desc$negative)
  }
)











#' tuneAndAggregate
#' tune treshold and get measuremens
#' @param combined Results
#' @param tuneT msr to tune treshold
#' @param measures measures to calculate
tuneAndAggregate = function(combinedResults = NULL, tuneT = tss, measures = list(auc, f1, bac, acc, fdr, fpr, npv, ppv, tnr, tpr), onlyTest = T, bc = "None"){
  require(mlr)
  Results = list()
  ResultsTss = list()
  if(bc!= "None") {
    for(n in names(combinedResults$Result)){
    for(k in names(combinedResults$Result[[n]])){
      tmp = getRRPredictions(combinedResults$Result[[n]][[k]]$result)
      iter = length(tmp$instance$train.inds )
      tresh = tuneThreshold(tmp, tss)
      ResultsTss[[n]][[k]] = tresh
      tmp = setThreshold(tmp,tresh$th)

      train = data.frame(matrix(0,iter,11))
      test = data.frame(matrix(0,iter,11))

      getMeasures = function(res, set){
        res[,1] = sapply(1:iter, FUN = function(x, tmp){
          measureAUC(tmp$data$prob.positive[tmp$data$set == set & tmp$data$iter == x], tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$negative, tmp$task.desc$positive)
        },tmp)
        res[,2] = sapply(1:iter, FUN = function(x, tmp){
          measureF1(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x],  tmp$task.desc$positive)
        },tmp)
        res[,3] = sapply(1:iter, FUN = function(x, tmp){
          measureBAC(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x])
        },tmp)
        res[,4] = sapply(1:iter, FUN = function(x, tmp){
          measureACC(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x])
        },tmp)
        res[,5] = sapply(1:iter, FUN = function(x, tmp){
          measureFDR(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$positive)
        },tmp)
        res[,6] = sapply(1:iter, FUN = function(x, tmp){
          measureFPR(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$negative, tmp$task.desc$positive)
        },tmp)
        res[,7] = sapply(1:iter, FUN = function(x, tmp){
          measureNPV(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$negative)
        },tmp)
        res[,8] = sapply(1:iter, FUN = function(x, tmp){
          measurePPV(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$positive,tmp$data$prob.positive[tmp$data$set == set & tmp$data$iter == x] )
        },tmp)
        res[,9] = sapply(1:iter, FUN = function(x, tmp){
          measureTNR(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$negative)
        },tmp)
        res[,10] = sapply(1:iter, FUN = function(x, tmp){
          measureTPR(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$positive)
        },tmp)
        res[,11] = sapply(1:iter, FUN = function(x, tmp){
          measureTTS(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$positive, tmp$task.desc$negative)
        },tmp)
        colnames(res) = c("auc", "f1", "bac", "acc", "fdr", "fpr", "npv", "ppv", "tnr", "tpr","tts")

        return(res)
      }
      if(!onlyTest) train = getMeasures(train, "train")
      test = getMeasures(test, "test")
      if(!onlyTest) Results[[n]][[k]] = list("train" = train, "test" = test)
      else Results[[n]][[k]] = list("test" = test)
    }
    }
  }else{
    for(n in names(combinedResults$Result)){

        tmp = getRRPredictions(combinedResults$Result[[n]]$result)
        iter = length(tmp$instance$train.inds )
        tresh = tuneThreshold(tmp, tss)
        ResultsTss[[n]] = tresh
        tmp = setThreshold(tmp,tresh$th)

        train = data.frame(matrix(0,iter,11))
        test = data.frame(matrix(0,iter,11))

        getMeasures = function(res, set){
          res[,1] = sapply(1:iter, FUN = function(x, tmp){
            measureAUC(tmp$data$prob.positive[tmp$data$set == set & tmp$data$iter == x], tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$negative, tmp$task.desc$positive)
          },tmp)
          res[,2] = sapply(1:iter, FUN = function(x, tmp){
            measureF1(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x],  tmp$task.desc$positive)
          },tmp)
          res[,3] = sapply(1:iter, FUN = function(x, tmp){
            measureBAC(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x])
          },tmp)
          res[,4] = sapply(1:iter, FUN = function(x, tmp){
            measureACC(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x])
          },tmp)
          res[,5] = sapply(1:iter, FUN = function(x, tmp){
            measureFDR(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$positive)
          },tmp)
          res[,6] = sapply(1:iter, FUN = function(x, tmp){
            measureFPR(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$negative, tmp$task.desc$positive)
          },tmp)
          res[,7] = sapply(1:iter, FUN = function(x, tmp){
            measureNPV(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$negative)
          },tmp)
          res[,8] = sapply(1:iter, FUN = function(x, tmp){
            measurePPV(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$positive,tmp$data$prob.positive[tmp$data$set == set & tmp$data$iter == x] )
          },tmp)
          res[,9] = sapply(1:iter, FUN = function(x, tmp){
            measureTNR(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$negative)
          },tmp)
          res[,10] = sapply(1:iter, FUN = function(x, tmp){
            measureTPR(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$positive)
          },tmp)
          res[,11] = sapply(1:iter, FUN = function(x, tmp){
            measureTTS(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x], tmp$task.desc$positive, tmp$task.desc$negative)
          },tmp)
          colnames(res) = c("auc", "f1", "bac", "acc", "fdr", "fpr", "npv", "ppv", "tnr", "tpr","tts")
          return(res)
        }
        if(!onlyTest) train = getMeasures(train, "train")
        test = getMeasures(test, "test")
        if(!onlyTest) Results[[n]] = list("train" = train, "test" = test)
        else Results[[n]] = list("test" = test)
      }
    }



  combinedTable = data.frame()
  if(bc!= "None"){
    for(j in 1:length(Results)){
    balanceGroup = sapply(Results[[j]], FUN = function(x){
     if(length(x) != 1) tr =  apply(x$train, 2, mean)
     else tr = NULL
     te = apply(x$test, 2, mean)
     if(!onlyTest) {
       tc = data.frame(rbind(tr,te))
       tc = cbind(tc, set = c("train", "test"))
     }
     else{
       tc = data.frame(t(te))
     }
    # rbind(tr,te)
    }, simplify = F)

    if(!onlyTest) balanceGroupT = sapply(1:length(balanceGroup), function(x, balanceGroup) {
      balanceGroup[[x]] = cbind(balanceGroup[[x]],method = rep(names(balanceGroup)[x],2), balance = rep(names(Results)[j],2))

    }, balanceGroup, simplify = F)
    else balanceGroupT = sapply(1:length(balanceGroup), function(x, balanceGroup) {
      balanceGroup[[x]] = cbind(balanceGroup[[x]],method = rep(names(balanceGroup)[x],1), balance = rep(names(Results)[j],1))

    }, balanceGroup, simplify = F)

    combinedTableA = data.frame()
    for(i in 1:length(balanceGroupT)) combinedTableA = rbind(combinedTableA, balanceGroupT[[i]])
    combinedTable = rbind(combinedTable, combinedTableA)
    }
  }else{
    for(j in 1:length(Results)){
        x = Results[[j]]
        if(length(x) != 1) tr =  apply(x$train, 2, mean)
        else tr = NULL
        te = apply(x$test, 2, mean)
        if(!onlyTest) {
          tc = data.frame(rbind(tr,te))
          tc = cbind(tc, set = c("train", "test"))
        }
        else{
          tc = data.frame(t(te))
        }
      names(Results)[j]


      combinedTableA = tc
      combinedTableA = cbind(combinedTableA, method = rep(names(Results)[j],2))

      combinedTable = rbind(combinedTable, combinedTableA)
    }
  }

  Results$combinedTable = combinedTable
  return(Results)
}



#' tuneAndAggregateRegression
#' tune treshold and get measuremens
#' @param combinedResults Results
#' @param measures msr to tune treshold
#' @param onlyTest measures to calculate

tuneAndAggregateRegression = function(combinedResults = NULL, measures = list(rmse, spearmanrho), onlyTest = T){
  require(mlr)
  Results = list()
  ResultsTss = list()

    for(n in names(combinedResults$Result)){

      tmp = getRRPredictions(combinedResults$Result[[n]]$result)
      iter = length(tmp$instance$train.inds )
      train = data.frame(matrix(0,iter,2))
      test = data.frame(matrix(0,iter,2))

      getMeasures = function(res, set){
        res[,1] = sapply(1:iter, FUN = function(x, tmp){
          measureRMSE(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x])
        },tmp)
        res[,2] = sapply(1:iter, FUN = function(x, tmp){
          measureSpearmanRho(tmp$data$truth[tmp$data$set == set & tmp$data$iter == x], tmp$data$response[tmp$data$set == set & tmp$data$iter == x])
        },tmp)

        colnames(res) = c("RMSE", "Spearmanrho")
        return(res)
      }
      if(!onlyTest) train = getMeasures(train, "train")
      test = getMeasures(test, "test")
      if(!onlyTest) Results[[n]] = list("train" = train, "test" = test)
      else Results[[n]] = list("test" = test)
    }



  combinedTable = data.frame()

    for(j in 1:length(Results)){
      x = Results[[j]]
      if(length(x) != 1) tr =  apply(x$train, 2, mean)
      else tr = NULL
      te = apply(x$test, 2, mean)
      if(!onlyTest) {
        tc = data.frame(rbind(tr,te))
        tc = cbind(tc, set = c("train", "test"))
      }
      else{
        tc = data.frame(t(te))
      }
      names(Results)[j]


      combinedTableA = tc
      combinedTableA = cbind(combinedTableA, method = rep(names(Results)[j],2))

      combinedTable = rbind(combinedTable, combinedTableA)
    }

  Results$combinedTable = combinedTable
  return(Results)
}



#' print function for the results
#' @export

print.TMmodel <- function(x){

  # rbindAll <- function(a, b) {
  #
  #   aDiff <- setdiff(colnames(a), colnames(b))
  #   bDiff <- setdiff(colnames(b), colnames(a))
  #
  #   a[, c(as.character(bDiff))] <- NA
  #
  #   b[, c(as.character(aDiff))] <- NA
  #
  #   return(rbind(a, b))
  # }
  #
  # Result <- data.frame()
  # if("multiBalance" %in% class(x)){
  #   for(j in 1:length(x$Result)){
  #     res <- data.frame()
  #     if(length(x$Result[[j]]) != 0){
  #       for(i in 1:length(x$Result[[j]])) {
  #         if(!"error" %in% class(x$Result[[j]][[i]]$result)) {
  #           res <- rbind(res, as.data.frame(t(x$Result[[j]][[i]]$result$aggr), row.names = names(x$Result[[j]])[i]))
  #         }
  #       }
  #     }
  #     #rownames(res) <- names(x$Result[[j]])
  #     res <- res[, unique(colnames(res))]
  #     res$Method <- rownames(res)
  #     res$BalanceMethod <- matrix(names(x$Result)[j], ncol = 1, nrow = nrow(res))
  #
  #     if(!nrow(res) == 0) {
  #       if(sum(dim(Result)) > 0)Result <- rbindAll(Result, res)
  #       else Result <- rbind(Result, res)
  #     }
  #   }
  #   Result <- Result[order(Result[paste(x$settings$tuningMetric$id, ".test.mean", sep = "")],  decreasing = T),]
  #   rownames(Result) <- 1:nrow(Result)
  #   print(Result)
  # }
  #for(i in 1:length(x$Result[[i]]$extract)){print(x$Result[[i]]$extract[[i]])}
  for(i in 1:length(x$Result)) {
    print(x$Result[[i]]$result)
  }

}







#' addA
#' color help function
#' @param col color as hex
#' @param alpha alpha
addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))



#' minOneInter
#' select only rows and columns with at least one interaction
#' @param inter interaction matrix
#' @export
minOneInter = function(inter){
  inter = as.matrix(inter)
  whichCol = apply(inter, 2, FUN = function(x){ if(any(x == 1)) return(TRUE)
    else return(FALSE)})
  whichRow = apply(inter, 1, FUN = function(x){ if(any(x == 1)) return(TRUE)
    else return(FALSE)} )
  return(data.frame(inter[whichRow,whichCol]))
}

#' createInterFeatures
#' Feature Eng funcion. Create interaction features, all combinations
#' @param df data.frame with the last variable as target

createInterFeatures = function(df){
  nF = sum(1:(ncol(df)-2))
  Result = data.frame(matrix(0,nrow(df), nF))

  counter = 1
  for(i in 1:(ncol(df)-2)){
    for(j in (i+1):(ncol(df)-1)){
      Result[,counter] = df[,i]*df[,j]
      colnames(Result)[counter] = paste0(colnames(df)[i],"_", colnames(df)[j])
      counter = counter+1
    }
  }
  return(cbind(df[,-ncol(df)],Result,target= df[,ncol(df)]))

}


#' plotOverfitting
#' plot Overfitting...
#' @param Results results from runTM
#' @param names of methods
#' @param titles titles for the plots
#' @param measure which measurement to use
#' @param box boxplot or barplot

plotOverfitting = function(Results, methods = c("RF", "dnn", "cnn", "cforest", "boost", "knn", "SVM"),titles =c("RF", "dnn", "cnn", "cforest", "boost", "knn", "SVM") , measure = "auc", box = T){

  aggResults = mylist <- sapply(methods,function(x) NULL)
  if(!box) for(method in methods){
              methodResult = data.frame(matrix(0,4,4, dimnames = list(c(names(Results)[1:4]), c("train", "trainSD", "test", "testSD"))))
              for(b in 1:4){
                if(!is.null(Results[[b]][[method]]$train[[measure]])){
                  methodResult[b,1] = mean(Results[[b]][[method]]$train[[measure]])
                  methodResult[b,2] = sd(Results[[b]][[method]]$train[[measure]])
                  methodResult[b,3] = mean(Results[[b]][[method]]$test[[measure]])
                  methodResult[b,4] = sd(Results[[b]][[method]]$test[[measure]])
                }
              }
              aggResults[[method]] = methodResult
            }
  else for(method in methods){
          methodResult = data.frame(matrix(0,10,8))
          for(b in 1:4){
            if(!is.null(Results[[b]][[method]]$train[[measure]])){
              methodResult[,b*2 - 1] = Results[[b]][[method]]$train[[measure]]
              methodResult[,b*2 ] = Results[[b]][[method]]$test[[measure]]
            }
          }
          aggResults[[method]] = methodResult
        }



  coords = matrix(0,8,4)
  coords[,1] = seq(0,0.75, length.out = 4)
  coords[,2] = seq(0.25,1, length.out = 4)
  coords[,3] = c(rep(0.5,4), rep(0,4))
  coords[,4] = c(rep(1,4), rep(0.5,4))

  if(!box){
      for(plt in 1:length(aggResults)){
        lenBar = 1/11
        recSeq = seq(0,9, by = 3)
        par(pty = "m", mar = c(4,2,2,2) + 0.3, oma = c(1,0,1,0) + 0.2)
        if(plt == 1) par(fig = coords[1,])
        else par(fig = coords[plt,], new = T)
        plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1), yaxt = "n", xaxt = "n", axes = F, xlab = "", ylab = "")
        title(main = titles[plt])
        axis(2, las = 1)
        if(aggResults[[plt]][4,1] != 0)text(y = -0.04, labels = c("Over", "Under", "SMOTE", "WC"), x = (recSeq+1)*lenBar, srt = 45, adj = 1, xpd = T, cex = 0.7)
        else text(y = -0.04, labels = c("Over", "Under", "SMOTE"), x = (recSeq[1:3]+1)*lenBar, srt = 45, adj = 1, xpd = T, cex = 0.7)
        for(n in 1:length(aggResults[[plt]])){
          if(!aggResults[[plt]][n,1]  == 0){
          rect(xleft = recSeq[n]*lenBar, xright =(recSeq[n]+1)*lenBar, ybottom = 0, ytop = aggResults[[plt]][n,1] )
          rect(xleft = (recSeq[n]+1)*lenBar, xright =(recSeq[n]+2)*lenBar, ybottom = 0, ytop = aggResults[[plt]][n,3], col = "grey" )

          segments(x0 = (recSeq[n]+0.5)*lenBar, y0 = aggResults[[plt]][n,1] -  aggResults[[plt]][n,2], y1 = aggResults[[plt]][n,1] +  aggResults[[plt]][n,2])
          segments(x0 = (recSeq[n]+0.5)*lenBar + 0.02, x1 = (recSeq[n]+0.5)*lenBar - 0.02, y0 = aggResults[[plt]][n,1] -  aggResults[[plt]][n,2])
          segments(x0 = (recSeq[n]+0.5)*lenBar + 0.02, x1 = (recSeq[n]+0.5)*lenBar - 0.02, y0 = aggResults[[plt]][n,1] +  aggResults[[plt]][n,2])

          segments(x0 = (recSeq[n]+1.5)*lenBar, y0 = aggResults[[plt]][n,3] -  aggResults[[plt]][n,4], y1 = aggResults[[plt]][n,3] +  aggResults[[plt]][n,4])
          segments(x0 = (recSeq[n]+1.5)*lenBar + 0.02, x1 = (recSeq[n]+1.5)*lenBar - 0.02, y0 = aggResults[[plt]][n,3] -  aggResults[[plt]][n,4])
          segments(x0 = (recSeq[n]+1.5)*lenBar + 0.02, x1 = (recSeq[n]+1.5)*lenBar - 0.02, y0 = aggResults[[plt]][n,3] +  aggResults[[plt]][n,4])

          }
        }


      }
      par(fig = coords[nrow(coords),], new = T)
      plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1), yaxt = "n", xaxt = "n", axes = F, xlab = "", ylab = "")
      legend(0,0.7, legend = c(paste("Train",measure), paste("Test", measure)), col = "black", pch = 22,bty = "n", pt.bg = c("white", "grey"), cex = 0.7)

  }else {


    for(plt in 1:length(aggResults)) {
      par(pty = "m", mar = c(4,0,2,0) + 0.3, oma = c(1,2,1,2) + 0.2)
      if(plt == 1) par(fig = coords[1,])
      else par(fig = coords[plt,], new = T,mar = c(4,0,2,0)+0.3)

      # if(plt == 4) par(mar = c(4,0,2,2)+0.3)
      # if(plt == 7) par(mar = c(4,0,2,2)+0.3)

      if(sum(aggResults[[plt]][,7]) == 0) boxplot(aggResults[[plt]][,1:6],pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 1.8),cex = 0.4, axes = F, at = c(1,1.8, 4,4.8,7,7.8), yaxt = "n",col = rep(c("white", "grey"),3), xlim = c(1,9), ylim = c(0.2,1), xaxt = "n")
      else boxplot(aggResults[[plt]][,1:8], pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 1.8),cex = 0.4,axes = F,at = c(1,1.8, 4,4.8,7,7.8,10,10.8), yaxt = "n",col = rep(c("white", "grey"),4), xlim = c(1,12), ylim = c(0.2,1), xaxt = "n")
      title(main = titles[plt])

      if(sum(aggResults[[plt]][,7]) != 0) text(y = 0.13, labels = c("Over", "Under", "SMOTE", "WC"), x = c(1.6, 4.6,7.6,10.6), srt = 45, adj = 1, xpd = T, cex = 0.7)
      else  text(y = 0.13, labels = c("Over", "Under", "SMOTE"), x = c(1.6, 4.6,7.6), srt = 45, adj = 1, xpd = T, cex = 0.7)
      if(plt == 1 || plt == 5) axis(2, las = 1)
      else axis(2, labels = FALSE)
    }
    par(fig = coords[nrow(coords),], new = T)
    plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1), yaxt = "n", xaxt = "n", axes = F, xlab = "", ylab = "")
    legend(0,0.7, legend = c(paste("Train",measure), paste("Test", measure)), col = "black", pch = 22,bty = "n", pt.bg = c("white", "grey"), cex = 0.7)
  }

}


#' DNNuncertanity
#' get sd for predictions
#' @param result previous result
#' @param data data for the model
#' @param inst resampling instance
DNNuncertanity = function(result, data, inst, n = 100, logTarget = TRUE, correct = TRUE, negBin = FALSE){
  require(keras)
  require(mlr)
  K = keras::backend()
  if(correct) .correct = 0.99
  else .correct = 0
  #Test:
  testPred = data.frame()
  counter = 1
  if(!negBin) modelName = "dnn"
  else modelName = "negBinDnn"
  for(k in inst$test.inds){
    model = unserialize_model(result$Result[[modelName]]$result$models[[counter]]$learner.model$next.model$learner.model$model$model)
    x = data$data[k,-c(1,2, ncol(data$data))]
    y = data$data[k,ncol(data$data), drop = F]

    get_output = K$`function`(list(model$layers[[1]]$input, K$learning_phase()), list(model$layers[[length(model$layers)]]$output))

    preds <- matrix(NA, nrow = nrow(x), ncol = n)
    dim(preds)
    for(i in seq_len(n)) {
      preds[ ,i] <- get_output(list(as.matrix(x), 1))[[1]]
    }
    dim(preds)
    if( logTarget){
      preds = exp(preds) - .correct
      mean <- apply(preds, 1, mean)
      sd <-apply(preds, 1, sd)
    }else{
      mean <- apply(preds, 1, mean)
      sd <-apply(preds, 1, sd)
    }

    testPred = rbind(testPred,cbind(data.frame(k), data.frame(y), data.frame(mean),data.frame(sd),data.frame(rep(counter, nrow(x))), data.frame(rep("test", nrow(x)) )))
    counter = counter+1
  }

  trainPred = data.frame()
  counter = 1
  for(k in inst$train.inds){
    model = unserialize_model(result$Result[[modelName]]$result$models[[counter]]$learner.model$next.model$learner.model$model$model)
    x = data$data[k,-c(1,2, ncol(data$data))]
    y = data$data[k,ncol(data$data), drop = F]

    get_output = K$`function`(list(model$layers[[1]]$input, K$learning_phase()), list(model$layers[[length(model$layers)]]$output))

    n <- 100
    preds <- matrix(NA, nrow = nrow(x), ncol = n)
    dim(preds)
    for(i in seq_len(n)) {
      preds[ ,i] <- get_output(list(as.matrix(x), 1))[[1]]
    }
    dim(preds)

    if( logTarget){
      preds = exp(preds) - .correct
      mean <- apply(preds, 1, mean)
      sd <-apply(preds, 1, sd)
    }else{
      mean <- apply(preds, 1, mean)
      sd <-apply(preds, 1, sd)
    }


    trainPred = rbind(trainPred,cbind(data.frame(k), data.frame(y), data.frame(mean),data.frame(sd),data.frame(rep(counter, nrow(x))), data.frame(rep("train", nrow(x)) )))
    counter = counter+1
  }




  colnames(testPred) = c("id", "truth", "response", "se", "iter", "set")
  colnames(trainPred) = c("id", "truth", "response", "se", "iter", "set")



  pred = rbind(testPred, trainPred)

  result$Result[[modelName]]$result$pred$data = pred

  # update RMSE's:
  l = length(inst$test.inds)

  result$Result[[modelName]]$result$measures.test$msle= sapply(1:l, function(x) measureMSLE(testPred[testPred$iter == x,2 ], testPred[testPred$iter == x,3 ]))
  result$Result[[modelName]]$result$measures.train$msle = sapply(1:l, function(x) measureMSLE(trainPred[trainPred$iter == x,2 ], trainPred[trainPred$iter == x,3 ]))
  result$Result[[modelName]]$result$aggr["msle.test.msle"][[1]] = mean(result$Result[[modelName]]$result$measures.test$rmse )

  return(result)


}





#' @export
aic.fun = function(task, model, pred, feats, extra.args) {

  m = mlr::getLearnerModel(model,more.unwrap = TRUE)
  if(any("glm" %in% class(m))) return(m$aic)

  true = getPredictionTruth(pred)
  predictions = getPredictionResponse(pred)

  if(task$type == "regr"){
    res = length(model$features)*2 + (-sum(2*dpois(true, predictions, log = T)))
  } else {
    predictions = getPredictionProbabilities(pred)
    res = length(model$features)*2 + (-sum(2*dbinom(as.numeric(true)-1, size = 1,predictions, log = T)))
  }

  if(is.na(res)) return(Inf)
  else return(res)
}



#' @export
aic = mlr::makeMeasure(
  id = "aic", name = "aic",
  properties = c("regr", "req.pred", "req.truth", "classif", "req.model"),
  minimize = TRUE, best = 0, worst = +Inf,
  fun = aic.fun
)
