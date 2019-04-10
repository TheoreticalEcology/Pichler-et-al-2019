



inside = function(data, len = 1){
  #a in b
  a = data[,3]
  b = data[,1]
  a = as.character(a)
  b = as.character(b)
  
  a = a[complete.cases(a)]
  b = b[complete.cases(b)]
  
  b_s = strsplit(b, ":")
  b_s = c(b, sapply(b_s, function(x) paste0(rev(x), collapse = ":")))
  
  strict = sum(sapply(a[1:len], function(x) any(x %in% b_s)))
  strictDouble = sum(sapply(a[1:(2*len)], function(x) any(x %in% b_s)))
  
  return(data.frame(strict = strict, strictDouble = strictDouble, 
                    strictInter = mean(data[1:len,4]),
                    strictInterDouble = mean(data[1:(2*len),4]),
                    model = data$model[1],
                    lenTrue = data$len[1],
                    sample = data$sample[1],
                    auc = data$auc[1],
                    error = data$diffPerf[1]))
  
  
}



extractInterResults = function(data , groups = groups, true = true, trueStrengths){

  a =  paste0("A", 1:length(groups$a))
  b =  paste0("B", 1:length(groups$b))
  names(a) = groups$a
  names(b) = groups$b
  av = c(a,b)
  traits = av
  memory = matrix(NA, nrow = 20, ncol = 2)
  counter = 1
  out = list()
  for(k in c("first", "second", "third", "fourth")){
    tmpBest = data[[k]]
    i= 0
    goOn = TRUE
    success = 0
    while(goOn){
      counter = counter + 1
      i = i+1
      tmp = tmpBest[i,]
      tmpTraits = unlist(strsplit(as.character(tmp[1]),split = ":"))
      tmpTraits = sapply(tmpTraits , function(x) return(av[x]))
      if(!max(apply(memory, 1, FUN = function(x) sum(tmpTraits %in% x)))==2){
        success = success +1 
        out[[counter-1]] = tmp
        if(success == 3) goOn = FALSE
        memory[counter,] = tmpTraits
      }
      
    }
  }
  out[sapply(out, is.null)] = NULL
  
  if(length(out[[1]]) > 2)found = unnest(data.frame(t(sapply(out, function(x) x[1,c(1,3)], simplify = T))))
  if(length(out[[1]]) < 3)found = unnest(data.frame(t(sapply(out, function(x) x[1,c(1,2)], simplify = T))))

  found = found[order(found[,2],decreasing = T),]
  
  trueInter = data.frame(true = true, trueInterStrength = trueStrengths)
  if(nrow(trueInter) > 1) trueInter = trueInter[order(trueInter[,2],decreasing = T),]
  
  
  feat = strsplit(str_split(found[,1],   ":", simplify = T), ".", fixed = T)
  
  
  factorInter = as.numeric(sapply(feat, function(x) if(length(x) > 1) return(x[2]) else 0))
  foundChanged = apply(matrix(lapply(feat, function(x) x[1]), ncol = 2),1, function(x) paste0(x[1],":",x[2]))
  
  found_feat = data.frame(foundChanged,foundStrength = found[,2], matrix(factorInter, ncol = 2))
  f_p = as.character(found_feat[1:length(true),1])
  f_p_r = sapply(f_p, function(x) (str_split(x, ":")))
  f_p_r = sapply(f_p_r, function(x) paste0(rev(x), collapse = ":"))
  
  i_m_r = sapply(f_p_r, function(x) which(x ==f_p,arr.ind = T))
  i_m = sapply(f_p, function(x) which(x ==f_p,arr.ind = T))
  
  i_m_l = c(sapply(i_m, function(x) if(length(x) > 1) return(x) else NULL ),
  sapply(i_m_r, function(x) if(length(x) > 1) return(x) else NULL ))
  
  i_m_l[sapply(i_m_l, is.null)] = NULL
  
  if(length(i_m_l) >= 1){
    for(un in unique(names(i_m_l))) {
      old = found_feat[i_m_l[un][[1]],]
      old_t = data.frame(foundChanged = old[1,1], foundStrength = mean(old[,2]), X1 = paste0(old[,3],collapse = "+"), X2 = paste0(old[,4],collapse = "+"))
      found_feat_new = found_feat
      found_feat_new = found_feat_new[-i_m_l[un][[1]],]
      found_feat_new = rbind(found_feat_new, old_t)
      found_feat = found_feat_new
      found_feat = found_feat[order(found_feat[,2],decreasing = T ),]
    }
  }
  
  
  
  result = rowr::cbind.fill(data.frame(trueInter),
                            found_feat,fill = NA)
  return(result)
}




aggregateResults = function(data, interResult, models = c("dnn", "rf", "boost", "knn"), TraitInter, TraitStrengths,
                            runs = c(4,10), exception=34){
  OverallResult = data.frame()
  
  for(m in 1:length(models)){
    model = models[m]
    for(i in 1:prod(runs)){
      cat(i," ")
      j = 1
      ik = i%%runs[2]
      if(ik==0) ik = runs[2]
      if(i>runs[2]) j = 2
      if(i>runs[2]*2) j = 3
      if(i>runs[2]*3) j = 4
      
      if(!is.null(data[[j]][[ik]]$Result)){
        groups = sort(colnames(mlr::createDummyFeatures(data[[j]][[ik]]$classCommunity$data[,-c(1,2,15)])))
        groups =list(a = groups[1:which(groups == "A6",arr.ind = T)], 
                     b= groups[(which(groups == "A6",arr.ind = T)+1):length(groups)])
        
        (true = apply(matrix(TraitInter[[j]][ik,], ncol = 2, byrow = T), 1, FUN = function(x) paste0(x[1],":", x[2])))
        
        result = extractInterResults(data = interResult[[i]][[m]], 
                                     groups = groups, 
                                     true = true, 
                                     trueStrengths = TraitStrengths[[j]][ik,])
        result = cbind(result,
                       auc = data[[j]][[ik]]$Result[[m]]$result$aggr[[1]],
                       diffPerf = mean(data[[j]][[ik]]$Result[[m]]$result$measures.train[,2])- mean(data[[j]][[ik]]$Result[[m]]$result$measures.test[,2]),
                       model = model,
                       lengthTrue = length(c(true)), 
                       sample = ik)
        OverallResult = rbind(OverallResult, result)
      }
    }
  }
  out = list()
  out$OverallResult = OverallResult
  
  
  aggregatedResults = data.frame()
  
  for(m in 1:runs[1]){
    m = models[m]
    sub = OverallResult[OverallResult$model == m,]
    for(i in 1:runs[1]){
      for(j in 1:runs[2]){
        aggregatedResults = rbind(aggregatedResults,
                                  inside(sub[sub$lengthTrue==i & sub$sample == j,]))
      }
    }
  }
  out$aggregatedResults = aggregatedResults
  return(out)
}



















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


tuneAndAggregateRegression = function(combinedResults = NULL, measures = list(rmse, spearmanrho), onlyTest = T){
  require(mlr)
  Results = list()
  ResultsTss = list()
  
  for(n in names(combinedResults$Result)){
    print(n)
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




drawRankingMatrix2 = function(best,true = NULL, groups, cols = RColorBrewer::brewer.pal(8,name = "YlGnBu"), cex = 0.8, 
                              lwd = 1.1, muteY = FALSE, muteX = FALSE,top_n = 2L){
  addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 
  
  out = list()
  nT = length(unlist(groups))
  nCells = 1/nT
  coordsXYHalf = seq(0,1,by = nCells)[1:nT]+nCells/2
  coordsXY = seq(0,1,by = nCells)
  gridCol = "grey"
  plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
  for(i in  1:length(coordsXY)){
    if(i > 1)
      for(j in (i-1):length(coordsXY)){
        if(j< (length(coordsXY)-length(groups$b)) || i > length(groups$a)+1) gridCol2 = addA(gridCol,0.3)
        else gridCol2 = addA(gridCol,1)
        if(i == 2) lines(x =rep(coordsXY[i-1],2), y = c(coordsXY[j], coordsXY[j+1]), col = gridCol2,xpd = NA)
        lines(x = rep(coordsXY[i],2), y = c(coordsXY[j], coordsXY[j+1]), col = gridCol2, xpd = NA)
        lines(x = c(coordsXY[i], coordsXY[i-1]), y = rep(coordsXY[j],2), col = gridCol2, xpd = NA)
      }
  }
  
  colsA = c(rep("black", length(groups$a)), rep(addA("black", 0.3), length(groups$b)))
  colsB = c(rep("black", length(groups$b)), rep(addA("black", 0.3), length(groups$a)))
  
  if(!muteX) text(x = coordsXYHalf-0.08, y = 1.03,pos = 4,xpd = NA, labels = unlist(groups), font = 2,cex = cex, srt = 60, col = colsA)
  if(!muteY) text(y = coordsXYHalf, x = 0.0,pos = 2,xpd = NA, labels = unlist(groups), font = 2,cex = cex, col = rev(colsB))
 
  a =  paste0("A", 1:length(groups$a))
  b =  paste0("B", 1:length(groups$b))
  names(a) = groups$a
  names(b) = groups$b
  av = c(a,b)
  traits = av
  memory = matrix(NA, nrow = 20, ncol = 2)
  counter = 1
  for(k in names(best)){
    tmpBest = best[[k]]
    cat(k)
    i= 0
    goOn = TRUE
    success = 0
    while(goOn){
      counter = counter + 1
      i = i+1
      tmp = tmpBest[i,,drop = FALSE]
      tmpTraits = unlist(strsplit(as.character(rownames(tmp)),split = ":"))
      tmpTraits = sapply(tmpTraits , function(x) return(av[x]))
     
      if(!max(apply(memory, 1, FUN = function(x) sum(tmpTraits %in% x)))==2){
        if(sum(grepl("A", tmpTraits)) == 1) {
          if(grepl("A", tmpTraits[1])){
            xTMP = which(tmpTraits[1] == traits, arr.ind = T)
            yTMP = which(tmpTraits[2] == traits, arr.ind = T)
          } else {
            xTMP = which(tmpTraits[2] == traits, arr.ind = T)
            yTMP = which(tmpTraits[1] == traits, arr.ind = T)
          }
          alpha = 1
        } else {
          xTMP= which(min(tmpTraits) == traits, arr.ind = T)
          yTMP = which(max(tmpTraits) == traits, arr.ind = T)
          alpha = 0.3
        }
        
        success = success +1 
        border = "grey"
        lwd = lwd
        # alpha = tmp[length(tmp)]
        out[[counter-1]] = tmp
       # col2 = cols[as.numeric(as.character(cut(tmp[length(tmp)][1,1], breaks = seq(0,1,length.out = 10),labels = 1:9)))]
        col2 = rev(cols)[i]
        col2 = addA(col2, alpha)
        rect(xleft = coordsXY[xTMP],xright = coordsXY[xTMP+1],ybottom = coordsXY[yTMP], ytop = coordsXY[yTMP+1], col = col2, border = border, lwd = lwd, xpd = NA)
        if(success == top_n) goOn = FALSE
        memory[counter,] = tmpTraits
      }

    }
  }
  
  if(!is.null(true)){
    for(i in 1:length(true)){
      tmp = true[i]
      tmpTraits = unlist(strsplit(as.character(tmp[1]),split = ":"))
      tmpTraits = sapply(tmpTraits , function(x) return(av[x]))
      
      if(sum(grepl("A", tmpTraits)) == 1) {
        if(grepl("A", tmpTraits[1])){
          xTMP = which(tmpTraits[1] == traits, arr.ind = T)
          yTMP = which(tmpTraits[2] == traits, arr.ind = T)
        } else {
          xTMP = which(tmpTraits[2] == traits, arr.ind = T)
          yTMP = which(tmpTraits[1] == traits, arr.ind = T)
        }
      } else {
        xTMP= which(min(tmpTraits) == traits, arr.ind = T)
        yTMP = which(max(tmpTraits) == traits, arr.ind = T)
      }
      lines(x = c(coordsXY[xTMP], xright = coordsXY[xTMP+1]), y = c(coordsXY[yTMP],coordsXY[yTMP+1]) , lwd = lwd, xpd = NA)
      lines(x = c(coordsXY[xTMP], xright = coordsXY[xTMP+1]), y = c(coordsXY[yTMP+1],coordsXY[yTMP]) , lwd = lwd, xpd = NA)
      rect(xleft = coordsXY[xTMP],xright = coordsXY[xTMP+1],ybottom = coordsXY[yTMP], ytop = coordsXY[yTMP+1], col = NA, border = "black", lwd = lwd, xpd = NA)
    }
  }
  return(out)
}

drawRankingMatrix = function(best, true, traits, cols = RColorBrewer::brewer.pal(9,name = "YlGnBu"), cex = 0.8, lwd = 1.1, muteY = FALSE){
  addA = function(col, alpha = 0.25) apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 
  nT = length(traits)
  nCells = 1/nT
  
  coordsXYHalf = seq(0,1,by = nCells)[1:nT]+nCells/2
  coordsXY = seq(0,1,by = nCells)
  gridCol = "grey"
  plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
  for(i in  1:length(coordsXY)){
    if(i > 1)
      for(j in (i-1):length(coordsXY)){
        if(i == 2) lines(x =rep(coordsXY[i-1],2), y = c(coordsXY[j], coordsXY[j+1]), col = gridCol,xpd = NA)
        lines(x = rep(coordsXY[i],2), y = c(coordsXY[j], coordsXY[j+1]), col = gridCol, xpd = NA)
        lines(x = c(coordsXY[i], coordsXY[i-1]), y = rep(coordsXY[j],2), col = gridCol, xpd = NA)
      }
  }
  
  text(x = coordsXYHalf, y = 0.97,pos = 3,xpd = NA, labels = traits, font = 2,cex = cex)
  if(!muteY) text(y = coordsXYHalf, x = 0.03,pos = 2,xpd = NA, labels = traits, font = 2,cex = cex)
  
  for(k in c("first", "second", "third", "fourth")){
    tmpBest = best[[k]]
    for(i in 1:2){
      tmp = tmpBest[i,]
      tmpTraits = unlist(strsplit(as.character(tmp[1]),split = ":"))
      if(sum(grepl("A", tmpTraits)) == 1) {
        if(grepl("A", tmpTraits[1])){
          xTMP = which(tmpTraits[1] == traits, arr.ind = T)
          yTMP = which(tmpTraits[2] == traits, arr.ind = T)
        } else {
          xTMP = which(tmpTraits[2] == traits, arr.ind = T)
          yTMP = which(tmpTraits[1] == traits, arr.ind = T)
        }
      } else {
        xTMP= which(min(tmpTraits) == traits, arr.ind = T)
        yTMP = which(max(tmpTraits) == traits, arr.ind = T)
      }
      if(tmp[1] %in% true){
        border = "black"
        lwd = lwd
      } else {
        border = "grey"
        lwd = lwd
      }
      alpha = tmp[length(tmp)]
      rect(xleft = coordsXY[xTMP],xright = coordsXY[xTMP+1],ybottom = coordsXY[yTMP], ytop = coordsXY[yTMP+1], col = cols[cut(tmp[length(tmp)][1,1], breaks = seq(0,1,length.out = 10),labels = 1:9)], border = border, lwd = lwd, xpd = NA)
    }
  }
  for(i in 1:length(true)){
    tmp = true[i]
    tmpTraits = unlist(strsplit(as.character(tmp),split = ":"))
    if(sum(grepl("A", tmpTraits)) == 1) {
      if(grepl("A", tmpTraits[1])){
        xTMP = which(tmpTraits[1] == traits, arr.ind = T)
        yTMP = which(tmpTraits[2] == traits, arr.ind = T)
      } else {
        xTMP = which(tmpTraits[2] == traits, arr.ind = T)
        yTMP = which(tmpTraits[1] == traits, arr.ind = T)
      }
    } else {
      xTMP= which(min(tmpTraits) == traits, arr.ind = T)
      yTMP = which(max(tmpTraits) == traits, arr.ind = T)
    }
    lines(x = c(coordsXY[xTMP], xright = coordsXY[xTMP+1]), y = c(coordsXY[yTMP],coordsXY[yTMP+1]) , lwd = lwd, xpd = NA)
    lines(x = c(coordsXY[xTMP], xright = coordsXY[xTMP+1]), y = c(coordsXY[yTMP+1],coordsXY[yTMP]) , lwd = lwd, xpd = NA)
    rect(xleft = coordsXY[xTMP],xright = coordsXY[xTMP+1],ybottom = coordsXY[yTMP], ytop = coordsXY[yTMP+1], col = NA, border = "black", lwd = lwd, xpd = NA)
  }
}



# Function to print four overall + top two pairwise interactions
plotInteractionsTop2 = function(results, x1 = c(0.09,0.43), x2 = c(0.66,1), 
                                cols = c("steelblue","steelblue", "red"), lwd = 1.2, 
                                trueInter = c("A1.1:B2", "A1.2:B2", "B2:A1.1", "B2:A1.2", "A2:B3", "B3:A2", "A3:B4", "B4:A3"), 
                                cex = 0.9, cexProcent = 0.5 ,dTree = 0.060){
  
  plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1),xlab = "", ylab = "", axes = F)
  pos1Y = rev(seq(0.07,0.93, length.out = 4))
  s = sapply(pos1Y, function(y1,dTree = 0.095,x1,x2) {
    lines(x = x1, y = rep(y1,2), lty = 2, col = "grey")
    for(d in c(dTree,-dTree)){
      lines(x = x2, y = rep(y1,2)+ d, lty = 2, col = "grey")
    }
    # Tree
    lines(y = rep(y1,2), x = c(x1[2]+0.02,x1[2]+0.04), lwd = lwd)
    lines(y = c(y1+dTree, y1-dTree), x = rep(x1[2]+0.04,2), lwd = lwd)
    lines(y = rep(y1+dTree,2), x = c(x1[2]+0.04,x1[2]+0.06), lwd = lwd)
    lines(y = rep(y1-dTree,2), x = c(x1[2]+0.04,x1[2]+0.06), lwd = lwd)
  },dTree,x1,x2)
  
  for(i in 1:4){
    if(results$All$.interaction[i] <= 1) {
      rect(xleft = x1[1], xright = results$All$.interaction[i]*diff(x1)+x1[1], ybottom = (pos1Y)[i]-0.02, ytop =(pos1Y)[i]+0.02, col = cols[1], border = cols[1] )
      text(x = x1[1] + 0.5*diff(x1), y = (pos1Y)[i]+0.003, labels = paste0(round(results$All$.interaction[i],3)*100," %"),pos = 3,font = 2, cex = cexProcent, xpd = NA)
    }else{
      text(x = x1[1] + 0.5*diff(x1), y = (pos1Y)[i]+0.003, labels = "NA",pos = 3,font = 2, cex = cexProcent, xpd = NA)
    } 
    text(x = 0, y = (pos1Y)[i], labels = results$All$.feature[i], pos = 4,font = 2, cex = cex, xpd = NA)
  }
  namesR = c("first", "second", "third" , "fourth")
  for(i in 1:4){
    dTree2 = c(dTree, -dTree)
    for(j in 1:2){
      if(results[[namesR[i]]]$.feature[j] %in% trueInter) tmpCol = cols[3]
      else tmpCol = cols[2]
      if(results[[namesR[i]]]$.interaction[j] <= 1){
        lR = round(results[[namesR[i]]]$.interaction[j],3)
        text(x = x2[1]+diff(x2)*0.5, y = pos1Y[i]+dTree2[j]+0.001, labels = paste0(lR*100," %"),pos = 3,font = 2, cex = cexProcent, xpd = NA)
        rect(xleft = x2[1], xright = results[[namesR[i]]]$.interaction[j]*diff(x2)+x2[1], ybottom = (pos1Y)[i]-0.02+dTree2[j], ytop =(pos1Y)[i]+0.02+dTree2[j], col = tmpCol, border = tmpCol, xpd = NA)
      }else{
        text(x = x2[1]+diff(x2)*0.5, y = pos1Y[i]+dTree2[j]+0.001, labels = "NA",pos = 3,font = 2, cex = cexProcent, xpd = NA)
      }
      text(x = x1[2]+0.064, y = pos1Y[i]+dTree2[j], labels = results[[namesR[i]]]$.feature[j],pos = 4,font = 2, cex = cex, xpd = NA)
      
    }
  }
  s = sapply(pos1Y, function(y1,dTree = 0.095,x1,x2) {
    for(i in 1:2){
      lines(x = rep(x1[i],2)-0.0001, y = c(y1-0.03,y1+0.03), lwd = lwd, xpd = NA)
    }
    for(d in c(dTree,-dTree)){
      for(i in 1:2){
        lines(x = rep(x2[i],2)-0.0001, y = c(y1-0.03,y1+0.03) + d, lwd = lwd, xpd = NA)
      }
    }
  },dTree,x1,x2)
  
}

# Plot function for Fig. 4


normalized = function(x) (x-min(x))/(max(x)-min(x))


# Plot function for change in response 
plotInteractionEffect = function(inter, col = c("red", "yellow"), cuts = 12, heat = NULL, cexP = 0.5){
  colorN = cuts
  if(is.null(heat))colorR = colorRampPalette(col)(colorN)
  else colorR = heat(colorN)
  legB = 0.01
  image(x=normalized(as.numeric(inter$x.values[[1]])),y= normalized(inter$x.values[[2]]), z = (inter$f.values), col =colorR, axes = F, xlab = "" ,ylab = "", font.lab = 2, cex.lab = 0.7, xpd = NA, xlim = c(0,1), ylim = c(0,1))
  ybottom = sapply(0:(colorN-1), function(x) 0.5+x*0.5/colorN)
  ytop = sapply(1:colorN, function(x) 0.5+x*0.5/colorN)
  xleft = rep(1 + legB+0.04,colorN)
  xright = rep(1+0.07+legB,colorN)
  for(i in 1:colorN) rect(xleft[i], ybottom[i], xright[i], ytop[i], col = colorR[i], xpd = NA, border =  NA)
  rect(1+legB+0.04,0.5,1+0.07+legB,1, border= "black",cex = 1, col = NA, xpd = NA,lwd = 0.5)
  text(x = rep(1+0.03+legB),y = ybottom[c(1,colorN/2,colorN)]+0.25/colorN  , labels = round(c(min(c(inter$f.values)), median(c(inter$f.values)), max(c(inter$f.values))), digits = 2), xpd = NA, pos = 4,cex = cexP, font = 2)
  if(!is.character(inter$x.values[[1]])) s = sapply(normalized(inter$x.values[[1]]),  function(x) lines(rep(x,2), y=c(1.0,1.03),lwd = 0.6, xpd = NA, col = "darkgrey"))
  s = sapply(normalized(inter$x.values[[2]]),  function(x) lines(c(1.0,1.03), y=rep(x,2),lwd = 0.6, xpd = NA, col = "darkgrey"))
}



rowchart = function(best, true, col = c("#8CFF8F", "white"), cex = 1.0){
  maxImp = max(best$featureImportance$importance)
  maxBar = 0.9
  nRow = nrow(best$featureImportance)
  trueUnique = unique(unlist(strsplit(true, ":")))
  d1 = 0.8/nRow
  d2 = 0.2/(nRow-1)
  
  df = function(alt = 0, d_1 = d1, d_2 = d2){
    
    c(alt, alt + d_1, alt+d_1+d_2)
  }
  coords = matrix(0, nrow = nRow, ncol = 3)
  for(k in 1:(nRow)){
    if(k == 1) coords[k,] = df(0)
    else coords[k,] = df(coords[k-1,3])
  }
  coords = apply(coords, 2, rev)
  
  plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1),axes = F, xlab = "", ylab = "")
  axis(2,at = apply(coords, 1, FUN = function(x) mean(c(x[1], x[2]))), labels = best$featureImportance$feature, las = 2,tick = F, cex.axis =cex )
  
  for(i in 1:nRow){
    d = best$featureImportance[i,]
    if(d[1] %in% trueUnique) colB = col[1]
    else colB = col[2]
    rect(ybottom = coords[i,1], ytop = coords[i,2], xleft = 0, xright = d[4]*maxBar/maxImp,col = colB, border = "grey")
    text(x = d[4]*maxBar/maxImp, y = mean(c(coords[i,1], coords[i,2])), labels = formatC(as.character(round(d[4],3 )), format='f', digits=3 ), pos = 4, xpd = NA,cex = cex)
  }
  lines(x = c(0,0), y = c(-0.02,1.02), col = "grey", xpd = NA, lwd = 1.2)
  
  
  
}



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

plotInteractionsTop2 = function(results, x1 = c(0.09,0.43), x2 = c(0.66,1), 
                                cols = c("steelblue","steelblue", "red"), lwd = 1.2, 
                                trueInter = c("A1.1:B2", "A1.2:B2", "B2:A1.1", "B2:A1.2", "A2:B3", "B3:A2", "A3:B4", "B4:A3"), 
                                cex = 0.9, cexProcent = 0.5 ,dTree = 0.060, dist =-0.03){
  
  plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1),xlab = "", ylab = "", axes = F)
  pos1Y = rev(seq(0.07,0.93, length.out = 4))
  s = sapply(pos1Y, function(y1,dTree = 0.095,x1,x2) {
    lines(x = x1, y = rep(y1,2), lty = 2, col = "grey")
    for(d in c(dTree,-dTree)){
      lines(x = x2, y = rep(y1,2)+ d, lty = 2, col = "grey")
    }
    # Tree
    lines(y = rep(y1,2), x = c(x1[2]+0.02,x1[2]+0.04), lwd = lwd)
    lines(y = c(y1+dTree, y1-dTree), x = rep(x1[2]+0.04,2), lwd = lwd)
    lines(y = rep(y1+dTree,2), x = c(x1[2]+0.04,x1[2]+0.06), lwd = lwd)
    lines(y = rep(y1-dTree,2), x = c(x1[2]+0.04,x1[2]+0.06), lwd = lwd)
  },dTree,x1,x2)
  
  for(i in 1:4){
    if(results$All$.interaction[i] <= 1) {
      rect(xleft = x1[1], xright = results$All$.interaction[i]*diff(x1)+x1[1], ybottom = (pos1Y)[i]-0.02, ytop =(pos1Y)[i]+0.02, col = cols[1], border = cols[1] )
      text(x = x1[1] + 0.5*diff(x1), y = (pos1Y)[i]+dist, labels = paste0(round(results$All$.interaction[i],3)*100," %"),pos = 3,font = 2, cex = cexProcent, xpd = NA)
    }else{
      text(x = x1[1] + 0.5*diff(x1), y = (pos1Y)[i]+dist, labels = "NA",pos = 3,font = 2, cex = cexProcent, xpd = NA)
    } 
    text(x = 0, y = (pos1Y)[i], labels = results$All$.feature[i], pos = 4,font = 2, cex = cex, xpd = NA)
  }
  namesR = c("first", "second", "third" , "fourth")
  for(i in 1:4){
    dTree2 = c(dTree, -dTree)
    for(j in 1:2){
      if(results[[namesR[i]]]$.feature[j] %in% trueInter) tmpCol = cols[3]
      else tmpCol = cols[2]
      if(results[[namesR[i]]]$.interaction[j] <= 1){
        text(x = x2[1]+diff(x2)*0.5, y = pos1Y[i]+dTree2[j]+dist, labels = paste0(round(results[[namesR[i]]]$.interaction[j],3)*100," %"),pos = 3,font = 2, cex = cexProcent, xpd = NA)
        rect(xleft = x2[1], xright = results[[namesR[i]]]$.interaction[j]*diff(x2)+x2[1], ybottom = (pos1Y)[i]-0.02+dTree2[j], ytop =(pos1Y)[i]+0.02+dTree2[j], col = tmpCol, border = tmpCol, xpd = NA)
      }else{
        text(x = x2[1]+diff(x2)*0.5, y = pos1Y[i]+dTree2[j]+dist, labels = "NA",pos = 3,font = 2, cex = cexProcent, xpd = NA)
      }
      text(x = x1[2]+0.064, y = pos1Y[i]+dTree2[j], labels = results[[namesR[i]]]$.feature[j],pos = 4,font = 2, cex = cex, xpd = NA)
      
    }
  }
  s = sapply(pos1Y, function(y1,dTree = 0.095,x1,x2) {
    for(i in 1:2){
      lines(x = rep(x1[i],2)-0.0001, y = c(y1-0.03,y1+0.03), lwd = lwd, xpd = NA)
    }
    for(d in c(dTree,-dTree)){
      for(i in 1:2){
        lines(x = rep(x2[i],2)-0.0001, y = c(y1-0.03,y1+0.03) + d, lwd = lwd, xpd = NA)
      }
    }
  },dTree,x1,x2)
  
}





plotResAgg = function(resAgg, labels = c("DNN", "RF", "BRT", "kNN"), col = "darkgreen"){
  
  plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", axes = FALSE)
  seq_y = matrix(c(seq(0,0.75, length.out = 4),seq(0.25,1, length.out = 4) ), ncol = 2)
  if(length(labels) != 4){
    seq_x = seq_x[1:length(labels),]
  } else {
    seq_x = seq_y
  }
  colfunc = colorRampPalette(c("white", col))
  
  axis(3,labels = labels, at = apply(seq_x,1,mean),font = 2, line = -0.7, col = NA, col.ticks = 1)
  axis(2,labels = as.character(4:1), at = apply(seq_y,1,mean),font = 2,las = 2,line = -0.7, col = NA, col.ticks = 1)
  models = unique(resAgg$model)
  for(i in models){
    for(j in 1:4){
      value = resAgg  %>% filter(model == i) %>% slice(j) %>% pull(trp)
      if(!is.na(value)) colS = colfunc(1e4+1)[value*1e4 + 1]
      else colS = "lightgrey"
      rect(xleft = seq_x[which(i == models),1], xright = seq_x[which(i == models),2], ybottom = seq_y[(4-j+1),1], ytop = seq_y[(4-j+1),2], 
           col = colS, border = "black")
      text(round(value, digits = 3), x = seq_x[which(i == models),1]*0.97,y = mean(seq_y[(4-j+1),]) ,pos = 4, cex = 1, font = 2)
    }
    
  }
}



aggTrpRep = function(trpRes, aggRes){
  out = 
    trpRes %>% 
    reshape2::melt(id.vars = c("len_fac", "rep_fac")) %>% 
    mutate(len_true = as.character(len_fac) %>% as.numeric(), 
           repetition = rep_fac %>% as.character %>% as.numeric,
           model = variable,
           trp = value) %>% 
    select(model, repetition, len_true, trp) %>% 
    right_join(aggRes, by = c("len_true", "repetition", "model")) %>% 
    filter(!duplicated(perf_diff)) %>% 
    filter(!is.na(trp)) %>% 
    group_by(model, len_true) %>% 
    select(model, len_true, trp, pos_true, found_strength, perf_test, perf_train, perf_diff) %>% 
    summarise_all(funs(mean))
  return(out)
}


aggregate_interaction_result = function(model_result = NULL, inter_results = NULL,trait_inter = NULL, trait_strengths = NULL){
  
  sim_len = sapply(model_result, length)
  ind_matrix = data.frame(ind1 = rep(1:4,sim_len), ind2 = abind::abind(sapply(1:4, function(i) do.call(seq, list(1, sim_len[i])),simplify = F)))
  
  result = data.frame()
  
  for(i in 1:nrow(ind_matrix)){
    inds = unlist(ind_matrix[i,])
    #print(inds)
    res = 
      extract_interaction_result(model_result = model_result[[inds[1]]][[inds[2]]], 
                                 inter_results = inter_results[[i]], 
                                 trait_inter = TraitInter[[inds[1]]][[inds[2]]], 
                                 trait_strengths = TraitStrengths[[inds[1]]][[inds[2]]])
    result = rbind(result, res)
  }
  result = cbind(result, type = rep(model_result[[1]][[1]]$classCommunity$type, nrow(result)))
  return(result)
}


extract_interaction_result = function(model_result, inter_results, trait_inter, trait_strengths){
  
  if(model_result$Result[[1]]$result$task.desc$type == "regr") {
    perf = "poisson"
    if(!perf %in%  names(model_result$Result[[1]]$result$measures.test)) perf = "spearmanrho"
  }
  else perf = "auc"
  
  result = data.frame()
  
  for(m in 1:length(model_result$Result)){
    tmp_inter = sort(inter_results[[m]]$pairwise_interactions$Interactions, decreasing = TRUE,na.last = TRUE)
    true = trait_inter
    true_list = lapply(strsplit(true, ":", fixed = TRUE), function(n) return(c(paste0(n[1], ":", n[2]), paste0(n[2], ":", n[1]))))
    names(true_list) = true
    pos_true = lapply(true_list, function(x) which(names(tmp_inter) %in% x,arr.ind = T))
    
    perf_test = model_result$Result[[m]]$result$measures.test[perf]
    perf_train = model_result$Result[[m]]$result$measures.train[perf]
    
    if(perf == "poisson"){
      perf_diff = perf_test - perf_train
    } else {
      perf_diff = 1- perf_test/perf_train
    }
    
    
    res= 
      data.frame(true = true, 
                 pos_true = unlist(pos_true), 
                 mean_pos_true = mean(unlist(pos_true),na.rm = TRUE),
                 found_strength = tmp_inter[unlist(pos_true)], 
                 len_true = length(true),
                 model = model_result$Result[[m]]$result$learner.id,
                 perf_test = perf_test[1,1],
                 perf_train = perf_train[1,1],
                 perf_diff = perf_diff[1,1],
                 simulated_strength = trait_strengths
      )
    result = rbind(result, res)
    
  }
  return(result)
  
}

aggTrp = function(aggRes, rate = 0.0){
  return(aggRes %>%
           filter(found_strength > rate) %>% 
           mutate(len_fac = as.factor(len_true),
                  rep_fac = as.factor(repetition)) %>% 
           group_by(model, len_fac, rep_fac) %>% 
           summarize(tpr = sum(pos_true <= len_true)/length(len_true)) %>% 
           spread(model,tpr) %>% 
           map_df(function(x) ifelse(is.na(x),0,x)))
}


plot_interaction = function(result, col = "darkred", n = 3, title = NULL){
  
  
  trp_matrix = matrix(NA, nrow = 4, ncol = 10)
  for(i in 1:nrow(result)){
    ind = result[i, 1:2]
    trp_matrix[ind[1,1] %>%  pull, ind[1,2] %>% pull] = result[i,n] %>% pull
  }
  
  seq_x = matrix(c(seq(0,0.9, length.out = 10),seq(0.1,1, length.out = 10) ), ncol = 2)
  seq_y = matrix(c(seq(0,0.75, length.out = 4),seq(0.25,1, length.out = 4) ), ncol = 2)
  
  colfunc = colorRampPalette(c("white", col))
  
  plot(NULL, NULL, xlim = c(0,1), ylim = c(0,1), xlab = "", ylab = "", axes = F)
  axis(3,labels = as.character(1:10), at = apply(seq_x,1,mean),font = 2, line = -1, col = NA, col.ticks = 1)
  axis(2,labels = as.character(4:1), at = apply(seq_y,1,mean),font = 2,las = 2,line = -1.2, col = NA, col.ticks = 1)
  for(i in 1:10){
    for(j in 1:4){
      value = trp_matrix[(4-j+1),i]*100 + 1
      if(!is.na(value)) colS = colfunc(101)[value]
      else colS = "lightgrey"
      rect(xleft = seq_x[i,1], xright = seq_x[i,2], ybottom = seq_y[j,1], ytop = seq_y[j,], 
           col = colS, border = "black")
      if(is.na(value)){
        text(x = 0.9*mean(seq_x[i,1]), y = mean(seq_y[j,]),pos = 4, labels = "NA", col = "black", cex = 0.4)
      }
    }
  }
  return(trp_matrix)
  
}

