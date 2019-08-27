#' Function to calculate Friedman's H-statistic
#'
#' @description Calculate pairwise Interaction Strengths
#'
#' @param data data.frame w/e or w/o target
#' @param model model of class mlr
#' @param grid_size size of subsampled marginal variable distribution
#' @param target target variable, character name
#' @param pairs two fields with a and b (only vector of length(a) == 1 allowed)
#'
#' @references Friedman, J. H. & Popescu, B. E. (2008), 'Predictive learning via rule ensembles', Ann. Appl. Stat. 2(3), 916--954.
#' @export
#' @author Maximilian Pichler

pairwise_interaction = function(data = NULL,
                                model,
                                grid_size = 50,
                                target = NULL,
                                pairs = list(a = c("x.1"), b = c("x.2", "x.3", "x.4", "x.5"))){


  if(is.factor(data[,target]) || length(unique(data[,target])) == 2) classif = TRUE
  else classif = FALSE

  if(!is.null(target)) data = data[,-which(colnames(data) == target, arr.ind = TRUE)]
  pair_a_ind = which(colnames(data) == pairs$a, arr.ind = TRUE)
  pair_b_ind = sapply(pairs$b, function(i) which(colnames(data)==i, arr.ind = T))




  n_row = nrow(data)
  size_matrix = n_row*grid_size
  n_col = ncol(data)
  samples = sample.int(nrow(data), size = grid_size)

  # Definition of PD:
  #  PD = mean( f^2(x_s, x_c) )

  # PD_a(x_a)
  var_1 = matrix(0, nrow = size_matrix, n_col)
  var_1[, pair_a_ind] = rep(data[samples, pair_a_ind], rep(n_row,grid_size))
  var_1[, -pair_a_ind] = apply(data[, -pair_a_ind], 2, function(x) rep(x, grid_size))
  var_1 = data.frame(var_1)
  colnames(var_1) = colnames(data)[1:n_col]
  if(!classif) pred = predict(model, newdata = var_1)$data$response
  else pred = predict(model, newdata = var_1)$data$prob.positive
  var_1 = apply(matrix(pred, ncol = grid_size),2, mean)

  combines = sapply(pair_b_ind, function(k){
    # PD_a_b(x_a_b) for all b
    var_1_2 = matrix(0, nrow = size_matrix, n_col)
    var_1_2[, c(pair_a_ind,k)] = matrix(rep(c(as.matrix(data[samples, c(pair_a_ind,k)])), rep(n_row,2*grid_size)) , ncol =  ncol(data[,c(pair_a_ind,k)]) )
    var_1_2[, c(-pair_a_ind,-k)] = apply(data[,c(-pair_a_ind,-k)], 2, function(x) rep(x, grid_size))
    var_1_2 = data.frame(var_1_2)
    colnames(var_1_2) = colnames(data)[1:n_col]
    if(!classif) pred = predict(model, newdata = var_1_2)$data$response
    else pred = predict(model, newdata = var_1_2)$data$prob.positive
    var_1_2 = apply(matrix(pred, ncol = grid_size),2, mean)

    # PD_b(x_b) for all b
    var_2 = matrix(0, nrow = size_matrix, n_col)
    var_2[,k] = rep(data[samples, k], rep(n_row,grid_size))
    var_2[,-k] = apply(data[,c(-k)], 2, function(x) rep(x, grid_size))
    var_2 = data.frame(var_2)
    colnames(var_2) = colnames(data)[1:n_col]

    if(!classif) pred = predict(model, newdata = var_2)$data$response
    else pred = predict(model, newdata = var_2)$data$prob.positive

    var_2 = apply(matrix(pred, ncol = grid_size),2, mean)

    return(list(scale(var_1_2,scale = F), scale(var_2,scale = F)))

  },simplify = F)
  var_1 = scale(var_1,scale = F)

  #H^2 = sum_i( [PD_a_b(x_a, x_b) - PD_a(x_a) - PD_b(x_b)]^2 ) / sum_i( PD_a_b(x_a, x_b)^2 )

  result = lapply(combines, function(l) sqrt(sum((l[[1]] - l[[2]] - var_1)**2) / sum(l[[1]]**2)))

  names(result) = paste0(pairs$a, ":",pairs$b)
  gc()
  return(result)

}

#' Function to calculate Friedman's H-statistic
#'
#' @description Calculate pairwise Interaction Strengths
#'
#' @param data data.frame w/e or w/o target
#' @param model model of class mlr
#' @param grid_size size of subsampled marginal variable distribution
#' @param target target variable, character
#'
#' @references Friedman, J. H. & Popescu, B. E. (2008), 'Predictive learning via rule ensembles', Ann. Appl. Stat. 2(3), 916--954.
#' @export
#' @author Maximilian Pichler

any_interaction = function(data = NULL,
                           model,
                           grid_size = 50,
                           target = NULL){


  if(is.factor(data[,target]) || length(unique(data[,target])) == 2) classif = TRUE
  else classif = FALSE

  if(!is.null(target)) data = data[,-which(colnames(data) == target, arr.ind = TRUE)]
  n_row = nrow(data)
  size_matrix = n_row*grid_size
  n_col = ncol(data)
  samples = sample.int(nrow(data), size = grid_size)
  # Definition of PD:
  #  PD = mean( f^2(x_s, x_c) )

  # PD_a(x_a) for all a
  var_1 = matrix(0, nrow = size_matrix*n_col, n_col)
  seq_along = matrix(1:(size_matrix*n_col),nrow = n_col,byrow = T)

  for(k in 1:n_col){
    ind =seq_along[k,]
    var_1[ind, k] = rep(data[samples, k], rep(n_row, grid_size))
    var_1[ind, -k] = apply(data[, -k], 2, function(x) rep(x, grid_size))
  }
  var_1 = data.frame(var_1)
  colnames(var_1) = colnames(data)[1:n_col]
  if(!classif) pred = predict(model, newdata = var_1)$data$response
  else pred = predict(model, newdata = var_1)$data$prob.positive
  var_1 = apply(seq_along,1 , function(i) apply(matrix(pred[i], ncol = grid_size),2, mean))

  # PD_(-a)(x_(-a)) for all a
  var_rest  = matrix(0, nrow = size_matrix*n_col, n_col)
  for(k in 1:n_col){
    ind =seq_along[k,]
    var_rest[ind,-k] = matrix(rep(c(as.matrix(data[samples,c(-k)])), rep(n_row,grid_size*(n_col-1))) , ncol =  (n_col-1) )
    var_rest[ind,k] =  rep(data[,k], grid_size)
  }
  var_rest = data.frame(var_rest)
  colnames(var_rest) = colnames(data)[1:n_col]

  if(!classif) pred = predict(model, newdata = var_rest)$data$response
  else pred = predict(model, newdata = var_rest)$data$prob.positive

  var_rest = apply(seq_along,1 , function(i) apply(matrix(pred[i], ncol = grid_size),2, mean))


  var_1 = apply(var_1, 2, function(x) scale(x, scale = FALSE))
  var_rest = apply(var_rest, 2, function(x) scale(x, scale = FALSE))

  # f_hat
  if(!classif) var_pred = scale(predict(model, newdata = data[samples,])$data$response, scale = FALSE)
  else var_pred = scale(predict(model, newdata = data[samples,])$data$prob.positive, scale = FALSE)
  #H^2 = sum_i( [f_hat(x)- PD_a(x_a) - PD_(-a)(x_(-a))]^2 ) / sum_i( f_hat(x)^2 )
  result = lapply(1:n_col, function(l) sqrt(sum((var_pred - var_1[,l] - var_rest[,l])**2) / sum(var_pred**2)))

  names(result) = colnames(data)
  gc()
  return(result)
}





#' Calculate interaction strengths
#'
#' @description wrapper around \code{\link{any_interaction}} and \code{\link{pairwise_interaction}}.
#'
#' @param data data.frame w/e or w/o target
#' @param model model of class mlr
#' @param any calculate overall x vs any interaction strengths
#' @param groups vector with group letters, for example c("A", "B") for A1, A2,...B1,...
#' @param grid_size size of subsampled marginal variable distribution
#' @param target target variable, character name
#' @param depth n variables with highest overall interaction strengths are used for pairwise interaction strengths
#' @param any_n_times mean of any_n_times overall interaction strength
#' @param parallel if FALSE no parallelization, otherwise numerical(n) cores are used
#'
#' @export
#' @author Maximilian Pichler
get_Interaction_Strengths = function(data,
                                     model,
                                     any = TRUE,
                                     groups = NULL,
                                     grid_size = 50L,
                                     target = NULL,
                                     depth = 4L,
                                     any_n_times = 5,
                                     parallel = FALSE){

  out = list()
  # compute first x vs any
  if(any){
    if(parallel) {
      cat("\n Using parallel backend...be cautious about memory!...")
      cl = parallel::makePSOCKcluster(parallel)
      doParallel::registerDoParallel(cl)
      parallel::clusterCall(cl, function(x) library(TraitMatching))
      any_result = snow::parSapply(cl = cl,X = 1:any_n_times,
                                   FUN = function(i, data, model, target, grid_size) unlist(any_interaction(data, model, target = target, grid_size = grid_size)),
                                   data, model, target, grid_size)
      parallel::stopCluster(cl)
      doParallel::stopImplicitCluster()

      gc()
    } else {
      any_result = sapply(1:any_n_times,
                          FUN = function(i, data, model, target, grid_size) unlist(any_interaction(data, model, target = target, grid_size = grid_size)),
                          data, model, target, grid_size)
    }
    mean_overall = apply(any_result, 1, mean)
    mean_overall = sort(mean_overall,decreasing = T)

    out$mean_overall = mean_overall

  } else {
    mean_overall = colnames(data)[!stringr::str_detect(colnames(data), target)]
    names(mean_overall) = colnames(data)[!stringr::str_detect(colnames(data), target)]
  }


  # calculate pairwise for highest 4 overall IA Strengths:

  # create pairwise work list:
  if(!is.null(groups)) {
    group_A = colnames(data)[stringr::str_detect(colnames(data), groups[1])]
    group_B = colnames(data)[stringr::str_detect(colnames(data), groups[2])]
    top = names(mean_overall)[1:depth]

    work_list = list()
    for(i in 1:depth){

      trim = names(work_list)[unlist(lapply(work_list, function(x) top[i] %in% x$b))]

      b =
        if(!top[i] %in% group_A) {
          group_A[!group_A %in% trim]
        } else {
          b = group_B[!group_B %in% trim]
        }
      work_list[[top[i]]] = list(a = top[i], b = b)
    }
  } else {
    group_all = colnames(data)[!stringr::str_detect(colnames(data), target)]
    top = names(mean_overall)[1:depth]
    work_list = list()
    for(i in 1:depth){

      trim = names(work_list)[unlist(lapply(work_list, function(x) top[i] %in% x$b))]

      b = group_all[!group_all %in% c(trim, top[i])]
      work_list[[top[i]]] = list(a = top[i], b = b)
    }

    work_list = lapply(work_list, function(x) if(length(x$b) == 0 || length(x$a) == 0) return(NULL) else x)
    work_list = work_list[!sapply(work_list, is.null)]
  }

  if(parallel) {
    #sort work_list for asynchronous efficieny
    len_works = sapply(work_list, function(x) length(x$b))
    work_list = sapply(order(len_works, decreasing = T), function(i) work_list[i])

    cat("\n Using parallel backend...be cautious about memory!...")
    cl = parallel::makePSOCKcluster(parallel)
    doParallel::registerDoParallel(cl)
    .null = parallel::clusterCall(cl, function(x) library(mlr))
    .null = parallel::clusterCall(cl, function(x) library(TraitMatching))

    pairwise_result = snow::parSapply(cl = cl,X = work_list,
                                      FUN = function(x, data, model, target, grid_size)
                                        unlist(pairwise_interaction(data, model, target = target, grid_size = grid_size, pairs = x)),
                                      data, model, target, grid_size,simplify = FALSE)
    parallel::stopCluster(cl)
    doParallel::stopImplicitCluster()
    gc()
  } else {
    pairwise_result = sapply(work_list,
                             FUN = function(x, data, model, target, grid_size)
                               unlist(pairwise_interaction(data, model, target = target, grid_size = grid_size, pairs = x)),
                             data, model, target, grid_size, simplify = FALSE)
  }

  out$pairwise_interactions = data.frame(Interactions = abind::abind(pairwise_result))

  return(out)

}
