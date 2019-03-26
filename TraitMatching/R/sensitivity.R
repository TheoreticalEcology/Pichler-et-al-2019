#' Sensitivity Analysis for analyzing effects
#'
#' @param TM Provide TM object from TM()
#' @param which methods to analyse, analyse all by default
#' @param plot show plot, default is True
#' @export


sa <- function(TM, which = names(TM$Result), plot = T) {
  out <- list()

  data   <- TM$InteractionList[, 3:ncol(TM$InteractionList)]
  data_r <- data[sample(nrow(data), size = 0.2 * nrow(data)), ]
  ind    <- sample(nrow(data_r), size = 0.5 * nrow(data_r))
  X1     <- data_r[ind, ]
  X2     <- data_r[-ind, ]

  # X3    <- data_r[1:2048,]

  parallel_sensitivity <- function(n, X1, X2, which, TM){
  #for(n in 1:length(which)){
    out <- list()
    cat(n, which[n])
    pred <- function(x) {
      p <- predict(TM$Result[[which[n]]]$models[[1]], mlr::makeClassifTask(data = x, target = TM$Result[[which[n]]]$task.desc$target, positive = TM$Result[[which[n]]]$task.desc$positive))
      return(mlr::getPredictionProbabilities(p))
    }
    Ti <- sensitivity::sobolTIIlo(model = pred, X1, X2)
    Ti_scaled     <- Ti$tii.scaled
    Ti$tii.scaled <- Ti_scaled[order(Ti_scaled$original, decreasing = T), ]
    group_A <- colnames(TM$InteractionList[, 3:ncol(TM$InteractionList)])[1:(TM$split[1])]
    group_B <- colnames(TM$InteractionList[, 3:ncol(TM$InteractionList)])[(TM$split[1] + 1):(sum(TM$split))]
    groups  <- as.data.frame(cbind(rownames(Ti$tii.scaled), t(as.data.frame(sapply(rownames(Ti$tii.scaled), function(x) strsplit(x, split = "*", fixed = T))))))

    which_group <- 0

    for (i in 1:nrow(groups)) {
      if ((groups[i, 2] %in% group_A) && (groups[i, 3] %in% group_A)) {
        which_group[i] <- "A"
      } else {
        if ((groups[i, 2] %in% group_B) && (groups[i, 3] %in% group_B)) {
          which_group[i] <- "B"
        } else {
          which_group[i] <- "A*B"
        }
      }
      which_group[i]
    }
    Ti$tii.scaled <- cbind(Ti$tii.scaled, data.frame(which_group))
  out[[which[n]]] <- Ti
  }
  parallelMap::parallelStartSocket(cpus = 4)
  parallelMap::parallelLibrary(packages = c("sensitivity", "mlr" ))
  #parallelMap::parallelExport(objnames = c("X1", "X2", "TM"))


  res <- parallelMap::parallelMap(fun = parallel_sensitivity, 1:length(which), more.args = list(X1, X2, which, TM))

  class(out) <- "TMsobol"
  if(plot) plot.TMsobol(out)

  return(out)
}

