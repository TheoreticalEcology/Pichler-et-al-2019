


#' Partial Dependence plots
#'
#' @param x Provide TM object from createTM
#'

plotDP <- function(x) {
  if(class(x)[1] != "TMmodel") stop("Error, pls provide TMmodel object from TM()")



  task <- mlr::makeClassifTask(data = x$InteractionList[, 3:ncol(x$InteractionList)], target = x$Result[[1]]$task.desc$target, positive = x$Result[[1]]$task.desc$positive)

  for(i in 1 : length(x$Result)){
    dp <- mlr::generatePartialDependenceData(x$Result[[i]]$models[[1]], input = task)
    #dp <- gp(x$Result$models[[1]], input = task)
    # mlr::plotPartialDependence(dp)

    par(mfrow = n2mfrow((sum(x$split))))
    ylim <- c(min(min(dp$data$Probability)), max(dp$data$Probability))
    for(i in 3 : ncol(dp$data)){
     t <- dp$data[complete.cases(dp$data[, c(2, i)]), c(2, i)]
     if(is.factor(t[, 2])){
       t$numeric <- as.numeric(t[, 2])
       t <- t[order(t$numeric), ]
       plot(x = t$numeric, y = t$Probability, type = "l", xlab = names(t)[2], ylab = "Probability", ylim = ylim, xaxt = "n" )
       points(x = t$numeric, y = t$Probability, pch = 16)
       axis(side = 1, at = as.integer(t$numeric), labels = as.character(t[,2]))
     } else {
       plot(x = t[, 2], y = t$Probability, type = "l", xlab = names(t)[2], ylab = "Probability", ylim = ylim)
       points(x = t[, 2], y = t$Probability, pch = 16)
     }
    }
  }
}



#' Roc Plot
#'
#' @param TM Provide TM object from createTM
#' @param cutoff show best cutoff point, default = F
#' @param smoothing parameter, 0 - 1, default = 1
#' @param cost_function show cust_function after yourden curve, default = T
#' @export



plotROC <- function(TM, cutoff = F, spar = 0.7, cost_function = T, show_in_two = F) {




  test$Result$Over$RF


  m <- mlr::addRRMeasure(res =  test$Result$Over$RF$result, measures = list(mlr::fpr, mlr::tpr))
  m2 <- mlr::generateThreshVsPerfData(m, measures = list(mlr::fpr, mlr::tpr))

  mlr::plotROCCurves(m2)







  if(!("TMmodel" %in% class(TM))) stop("Error. Provide object from TM()")

  oldpar = NULL
  on.exit(par(oldpar))
  if(cost_function) par(mfrow = c(1,2))
  if(show_in_two) par(mfrow = c(1,1))
  plot(0,0,xlab = "fpr", ylab = "tpr", type = "l",
       main = c("ROC Curves"), xlim = c(0, 1.0), ylim = c(0, 1.01), yaxs = "i", xaxs = "i", las = 1)
  abline(0,1, lty = 1)
  cols <- c("#332288",  "#88CCEE","#117733", "#999933", "#DDCC77", "#CC6677", "#882255","#44AA99", "#AA4499")

  col_leg <- 0
  tresh_points <- 0
  youden_matrix <- matrix(0, nrow = length(TM$Result), ncol = nrow(TM$Result[[1]]$tresh$data))

  for(i in 1:length(TM$Result)){
    #p <- mlr::getRRPredictions(TM$Result[[i]])
    tresh <- TM$Result[[i]]$tresh
    best_tresh <- TM$Result[[i]]$best_tresh

    lines(smooth.spline( x= tresh$data$fpr, y = tresh$data$tpr, spar = spar),lwd = 1.4, col = cols[i%%9 + 1])
    col_leg[i] <- cols[i%%9 + 1]

    tresh_points[i] <- TM$Result[[i]]$tresh$data$threshold[best_tresh]
    youden_matrix[i,] <- apply(TM$Result[[i]]$tresh$data[,1:2], MARGIN = 1, function(x) -1 * diff(x))
    # youden <- apply(tresh$data[,1:2], MARGIN = 1, function(x) -1* diff(x))
    if(cutoff){

      points(x = tresh$data$fpr[best_tresh ], y = tresh$data$tpr[best_tresh ], col = "black", pch = 18, lwd = 1.5)
      abline(v = tresh$data$fpr[best_tresh ], lty = 3, col = col_leg[i])
      abline(h = tresh$data$tpr[best_tresh ], lty = 3, col = col_leg[i])
    }
  }
#  auc <- sapply(1:length(TM$Result), FUN = function(x) return(paste(names(TM$Result)[x], round(TM$Result[[x]]$aggr[[1]], digits = 3))))

 auc <- sapply(1:length(TM$Result), FUN = function(x) return(round(TM$Result[[x]]$aggr[[1]], digits = 3)))


 legend(x=0.70,y = 0.283,  legend = names(TM$Result), col = col_leg, lty = 1, cex = 0.7, lwd = 3, bty = "n", title = expression(bold("Method:")))
 legend(x=0.85,y = 0.283,  xjust =  0,legend = auc,  cex = 0.7,  bty = "n" , title = expression(bold("AUC:")))



  if(cost_function){
    plot(0,0,xlab = "Treshold", ylab = "youden", type = "l",
         main = c("Treshold Cost Function"), xlim = c(0, 1.0), ylim = c(min(youden_matrix)*1.1 , 0), yaxs = "i", xaxs = "i", las = 1)


    for(i in 1:length(TM$Result)){
      lines(smooth.spline(y = youden_matrix[i,], x = TM$Result[[i]]$tresh$data$threshold, spar = 0.02), col = col_leg[i], lwd = 2)
      points(x = tresh_points[i], y = min(youden_matrix[i,]), lty = 4, col = col_leg[i], pch = 17)
    }
    legend("bottomright", legend = names(TM$Result), col = col_leg, lty = 1, cex = 0.8, lwd = 3)
  }
}


#' Sensitivity plots
#'
#' @param TM Provide TMsobol object from sa()
#' @param best Integer, strongest n interactions to plot
#' @export

### plot funktion fehlerhaft!!!
plot.TMsobol <- function(TMsobol, best = 30) {


  which = names(TMsobol)
  par(mfrow = n2mfrow((length(TMsobol))))
  for(n in 1:length(TMsobol)){

    if (nrow(TMsobol[[n]]$tii.scaled) >= 10) {
      TI_best_df <- TMsobol[[n]]$tii.scaled[1:best, ]
    }

    TI_best_df$rowname <- as.factor(rownames(TI_best_df))

    TI_best_df <- TI_best_df[order(TI_best_df$which_group), ]
    levs <- levels(TI_best_df$which_group)
    plot(x = 1:nrow(TI_best_df), y = TI_best_df$original, xaxt = "n", ylim = c(min(TI_best_df$`min. c.i.`), max(TI_best_df$`max. c.i.`) ), pch = 16,
         xlab = "Features", ylab = "Estimated TIIs" , col = TI_best_df$which_group)
    axis(side = 1, at = 1:nrow(TI_best_df), labels = TI_best_df$rowname)
    k <- 1
    tmp_lvl <- levs[k]
    for(i in 1:nrow(TI_best_df)){
        if(as.character(TI_best_df$which_group[i]) != tmp_lvl){
          #abline(v = i - 0.5)
          #legend(x = i - 1.2, y =  max(TI_best_df$`max. c.i.`)*1.01 , legend = tmp_lvl, col = k, pch = 16)
          tmp_lvl <- levs[k <- k +1]
        }
        segments(x0 = i, x1 = i, y0 = TI_best_df$`min. c.i.`[i], y1 = TI_best_df$`max. c.i.`[i])
        segments(x0 = i - 0.08, x1 = i + 0.08, y0 = TI_best_df$`min. c.i.`[i] , y1 = TI_best_df$`min. c.i.`[i])
        segments(x0 = i - 0.08, x1 = i + 0.08, y0 = TI_best_df$`max. c.i.`[i] , y1 = TI_best_df$`max. c.i.`[i])
    }
    legend("topright", legend = levs, col = 1:3, pch = 16)
    points(x = 1:nrow(TI_best_df), y = TI_best_df$original, xaxt = "n", pch = 16,
           col = TI_best_df$which_group)
    title(paste("Estimated Sobol TIIs with confidence Intervals", which[n]))

  }
}


#' Permutation random Forest plot
#'
#' @param PermutationRF PermutationRF object from permutationRF()
#' @param best Integer, show best n Interactionseffects, default = 15



plot.PermutationRF <- function(x, best = 15, labels = T){
  permutation_df <- x$result
  permutation_df
  if (nrow(permutation_df) >= 15) {
    permutation_df <- permutation_df[1:best, ]
  }

  plot(x = 1:nrow(permutation_df), y = permutation_df$deltaE, pch = 16, xlab = "deltaE", ylab = "Interactions", main = "Permutation RF interactions effects")
  axis(side = 1, at = 1:nrow(permutation_df), labels = permutation_df$together)
  if(labels == T) text(1:nrow(permutation_df), permutation_df$deltaE, labels=permutation_df$together, cex= 0.7, pos = 3)
  # group_A <- x$variables[1:x$split[1]]
  # group_B <- x$variables[(x$split[1] + 1 ): sum(x$split)]
  # groups  <- as.data.frame(cbind(data.frame(permutation_df$together), t(as.data.frame(sapply(as.character(permutation_df$together), function(x) strsplit(x, split = " * ", fixed = T))))))
  #
  # which_group <- 0
  #
  # for (i in 1:nrow(groups)) {
  #   if ((groups[i, 2] %in% group_A) && (groups[i, 3] %in% group_A)) {
  #     which_group[i] <- "A"
  #   } else {
  #     if ((groups[i, 2] %in% group_B) && (groups[i, 3] %in% group_B)) {
  #       which_group[i] <- "B"
  #     } else {
  #       which_group[i] <- "A*B"
  #     }
  #   }
  #   which_group[i]
  # }
  # Ti$tii.scaled <- cbind(Ti$tii.scaled, data.frame(which_group))

}

