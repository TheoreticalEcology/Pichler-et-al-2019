
#' Create species for simulateInter
#'
#'@param NumberA number of A
#'@param NumberB number of B
#'@param traitsA vector for traitsA = NumberofNUmerical, NumberofCategorical
#'@param traitsB vector for traitsB
#'@export

createSpecies = function(NumberA = 20, NumberB = 40, traitsA = c(5,5), traitsB = c(8,8), rangeDiscrete = 2:8,seed = 1337, abundance = 2,
                         speciesClass = NULL, specialist = T, coLin = NULL, sampling = function(n) runif(n, 0,1), specRange = c(1,2)){
  spec = specialist

  if(!is.null(speciesClass)){
    traitsA = speciesClass$traitsA
    traitsB = speciesClass$traitsB
    ADV = speciesClass$ADV
    BDV = speciesClass$BDV
  } else {
    ADV = list()
    BDV = list()
  }

  out = list()
  A = data.frame(matrix(NA, nrow = NumberA, ncol = sum(traitsA)))
  B = data.frame(matrix(NA, nrow = NumberB, ncol = sum(traitsB)))

  rownames(A) = sapply(1:nrow(A), function(x) return(paste("a",x,sep = "")))
  rownames(B) = sapply(1:nrow(B), function(x) return(paste("b",x,sep = "")))

  colnames(A) = sapply(1:ncol(A), function(x) return(paste("A",x,sep = "")))
  colnames(B) = sapply(1:ncol(B), function(x) return(paste("B",x,sep = "")))

  set.seed(seed)

  # Numerical:

  # if(!traitsA[2] == 0) A[,1:traitsA[2] + traitsA[1]] = sapply(1:traitsA[2], function(x, NumberA) return(rnorm(NumberA, 0, sd = 1)), NumberA)
  # if(!traitsB[2] == 0) B[,1:traitsB[2] + traitsB[1]] = sapply(1:traitsB[2], function(x, NumberB) return(rnorm(NumberB, 0, sd = 1)), NumberB)

  if(class(sampling) != "function"){

  if(!traitsA[2] == 0) A[,1:traitsA[2] + traitsA[1]] = sapply(1:traitsA[2], function(x, NumberA) return(runif(NumberA, 0,1)), NumberA)
  if(!traitsB[2] == 0) B[,1:traitsB[2] + traitsB[1]] = sapply(1:traitsB[2], function(x, NumberB) return(runif(NumberB, 0,1)), NumberB)

  } else {
    if(!traitsA[2] == 0) A[,1:traitsA[2] + traitsA[1]] = sapply(1:traitsA[2], function(x, NumberA) return(sampling(NumberA)), NumberA)
    if(!traitsB[2] == 0) B[,1:traitsB[2] + traitsB[1]] = sapply(1:traitsB[2], function(x, NumberB) return(sampling(NumberB)), NumberB)
  }

  if(!is.null(coLin)){
    for(i in 1:length(coLin)){
      if(names(coLin)[i] %in% colnames(A)) A[,names(coLin)[i]] =  model.matrix(coLin[[i]], data = A)[,2]
      else B[,names(coLin)[i]] =  model.matrix(coLin[[i]], data = B)[,2]
    }
  }


  if(is.null(speciesClass)){

    if(!traitsA[1] == 0) for(i in 1:traitsA[1]){
      ADV[[i]] = createDiscrete(rangeDiscrete)
      A[,i] = ADV[[i]](NumberA)
    }
    if(!traitsB[1] == 0) for(i in 1:traitsB[1]){
      BDV[[i]] = createDiscrete(rangeDiscrete)
      B[,i] = BDV[[i]](NumberB)
    }
  } else {
    if(!traitsA[1] == 0) for(i in 1:traitsA[1]){
      A[,i] = ADV[[i]](NumberA)
    }
    if(!traitsB[1] == 0) for(i in 1:traitsB[1]){
      B[,i] = BDV[[i]](NumberB)
    }
  }

  # if(!traitsA[1] == 0) A[,1:traitsA[1]] = createDiscrete(n = NumberA, nD = traitsA[1], range = rangeDiscrete, seed)
  # if(!traitsB[1] == 0) B[,1:traitsB[1]] = createDiscrete(n = NumberB, nD = traitsB[1], range = rangeDiscrete, seed)



  out$A = A
  out$B = B
  if(is.logical(spec) && spec)  out$spec = runif(NumberB, specRange[1],specRange[2]) #scales::rescale(rexp(NumberB,1), to = c(1,0.001))
  else if(is.logical(spec) && !spec)spec = rep(NumberB,1)
  out$traitsA = traitsA
  out$traitsB = traitsB
  if(!is.function(abundance)){
    if(is.logical(abundance) && !abundance){
      out$Aabund <- rep(1,NumberA)
      out$Babund <- rep(1,NumberB)
    } else {
      out$Aabund <- rpois(NumberA, abundance) + 1
      out$Babund <- rpois(NumberB, abundance) + 1
    }
  } else {
    out$Aabund <- abundance(NumberA, NumberA)
    out$Babund <- abundance(NumberB, NumberB)
  }
  out$ADV = ADV
  out$BDV = BDV
  class(out) = c("SpeciesClass")
  return(out)

}




#' create discrete
#' @param nD number discrete
#' @param range range of discrete
#'

createDiscrete = function(range){

  Nlevels = sample(range, 1)
  prob = sample.int(Nlevels+1,Nlevels)

  create = function(n){
    discreteV = sample(1:Nlevels, size = n, prob = prob, replace = T)
    f = as.factor(discreteV)
    levels(f) = 1:length(levels(f))
    discreteV = as.integer(f)
    return(discreteV)
  }

  #discV = sapply(1:nD, create, n, range)
  return(create)
}



#' Simulate Interaction
#' @param speciesClass speciesClass
#' @param main main traits
#' @param inter ...
#' @param weights weights for traits
#' @param cov covariance matrix, if not provided, it will randomly generated
#' @param reSim reSim
#' @param ... to speciesClass
#' @export

simulateInteraction = function(species = NULL, main = c("A1", "B9", "B10"), inter = matrix(c("A2", "B2",
                                                                                             "A6", "B7",
                                                                                             "A3", "B9"), ncol = 2, nrow = 3, byrow = T),
                               weights = list(main = c(1,2,0.5), inter = c(1,4,2)),
                               cov = NULL,
                               reSim = NULL,
                               setSeed = 42,
                               ...){
  set.seed(setSeed)
  if(is.null(reSim)){
    if(is.null(species)) species <- createSpecies(...)
    if(class(species) != "SpeciesClass") warning("species must be of class SpeciesClass")

    if(species$traitsA[1] != 0 && species$traitsB[1] != 0) discrete = c(colnames(species$A)[1:species$traitsA[1]], colnames(species$B)[1:species$traitsB[1]])
    else {
      if(species$traitsA[1] == 0) discrete = "0"
      if(species$traitsB[1] == 0) discrete = "0"
    }

    createCov = function(x){
      cv = runif(1, min = -0.5, 0.5)
      return(matrix(c(1, cv, cv, 1), ncol = 2))
    }


    #weights = lapply(weights, function(x) return(x/sum(x)))
    #weights$main = weights$main*0.5

    # main trait settings
    mainTraits = list()
    counter = 0
    for(i in main){
      counter = counter + 1
      m = i
      if(m %in% discrete){
        mainTraits[[m]]$discrete = TRUE
        if(m %in% colnames(species$A)) mainTraits[[m]]$mean = runif(20, 0,1)
        else mainTraits[[m]]$mean = runif(20, 0,1)
      } else{
        mainTraits[[m]]$discrete = FALSE
        mainTraits[[m]]$mean = 0
      }

      mainTraits[[m]]$weight = weights[["main"]][counter]
    }


    mainFunc = function(x, y, spec = 0.5){
      if(!is.null(main)){
        mT = data.frame(x, y)[main]
        res = 0
        for(k in 1:length(mT)){
          if(mainTraits[[k]]$discrete) res[k] = mainTraits[[k]]$mean[mT[[k]]]*mainTraits[[k]]$weight
          else res[k] = dnorm(mT[[k]], mean = mainTraits[[k]]$mean, spec)*mainTraits[[k]]$weight
        }
        return(prod(res))
      }else{
        return(1)
      }

    }



    counter = 0
    if(!is.null(inter)){
      interTraits = vector("list", nrow(inter))
      for(i in 1:nrow(inter)){
        paar = inter[i,]
        whichD = c(paar %in% discrete)
        k = i

        if(sum(whichD) > 1){
          interTraits[[k]]$both = TRUE
          interTraits[[k]]$interM = matrix(runif(100, 0,1), nrow = 10, ncol = 10) #change 25.3
          interTraits[[k]]$weight = weights[["inter"]][k]
        } else {
          interTraits[[k]]$both = FALSE
          if(any(whichD)){
            if(which(whichD, arr.ind = T) == 1) {
              interTraits[[k]]$which = 1
              interTraits[[k]]$mean = rnorm(length(unique(species$A[,paar[1]])),0,1)
              interTraits[[k]]$weight = weights[["inter"]][k]
            }else{
              interTraits[[k]]$which = 2
              interTraits[[k]]$mean = rnorm(length(unique(species$B[,paar[2]])),0,1)
              interTraits[[k]]$weight = weights[["inter"]][k]
            }
          } else {
            interTraits[[k]]$which = 3
            counter = counter + 1
            # if(is.null(cov)) interTraits[[k]]$cov = createCov(1)
            # else {
            #   if(!is.matrix(cov)) interTraits[[k]]$cov = cov[[counter]]
            #   else interTraits[[k]]$cov = cov
            # }
            # interTraits[[k]]$mean = rep(0,2)
            interTraits[[k]]$weight = weights[["inter"]][k]
          }
        }
      }
    }

    interFunc = function(x, y, spec = 0.5){
      if(!is.null(inter)){
        res = 0
        for(i in 1:length(interTraits)){
          if(interTraits[[i]]$both) res[i] = interTraits[[i]]$interM[x[1,inter[i,1]], y[1,inter[i,2]]] * interTraits[[i]]$weight
          else if(interTraits[[i]]$which != 3){
            if(interTraits[[i]]$which == 1) res[i] = dnorm(y[1,inter[i,2]], mean = interTraits[[i]]$mean[x[1,inter[i,1]]], sd = spec)*interTraits[[i]]$weight
            if(interTraits[[i]]$which == 2) res[i] = dnorm(x[1,inter[i,1]], mean = interTraits[[i]]$mean[y[1,inter[i,2]]], sd = spec)*interTraits[[i]]$weight
          }
          else{
            #res[i] = mvtnorm::dmvnorm(c(x[1,inter[i,1]], y[1,inter[i,2]]), mean = interTraits[[i]]$mean, interTraits[[i]]$cov*spec)*interTraits[[i]]$weight
            res[i] = dnorm(log(x[1,inter[i,1]]/y[1,inter[i,2]]),mean = 0, sd = spec)*interTraits[[i]]$weight #change 25.3
          }

        }
        return(prod(res))
      } else {
        return(1)
      }
    }



    out = list()
    interMatrix = matrix(NA, nrow=nrow(species$A), ncol = nrow(species$B) )

    for(i in 1:nrow(species$A)){
      x = species$A[i,]
      for(j in 1:nrow(species$B)){
        y = species$B[j,]
        spec = species$spec[j]
        interMatrix[i,j] = prod(mainFunc(x,y, spec), interFunc(x,y, spec), species$Aabund[i], species$Babund[j], runif(1, 0.3, 0.7)) #change 24.3
      }

    }

    out$mainFunc = mainFunc
    out$interFunc = interFunc
    rownames(interMatrix) = rownames(species$A)
    colnames(interMatrix) = rownames(species$B)

    if(species$traitsA[1] != 0) species$A[,1:species$traitsA[1]] = data.frame(apply(as.matrix(species$A[,1:species$traitsA[1]]),2,FUN = function(x) return(as.factor(x))), stringsAsFactors = T)
    if(species$traitsB[1] != 0) species$B[,1:species$traitsB[1]] = data.frame(apply(as.matrix(species$B[,1:species$traitsB[1]]),2,FUN = function(x) return(as.factor(x))), stringsAsFactors = T)

    out$A = cbind(rownames(species$A), species$A)
    colnames(out$A)[1] = "X"

    out$B = cbind(rownames(species$B), species$B)
    colnames(out$B)[1] = "Y"

    out$z = data.frame(interMatrix)

    out$poisson = function(x = 1000, seed = 42) {
      set.seed(seed)
      data.frame(matrix(as.numeric(rpois(length(interMatrix), interMatrix * x)) , ncol = ncol(interMatrix) , dimnames = list(rownames(interMatrix), colnames(interMatrix))))
    }

    out$binar = function(x = 1000, seed = 42) {
      set.seed(seed)
      data.frame(matrix(as.numeric(rpois(length(interMatrix), interMatrix * x) > 0) , ncol = ncol(interMatrix) , dimnames = list(rownames(interMatrix), colnames(interMatrix))))
    }
    out$species = species
    if(!is.null(inter)) out$settings$interT = interTraits
    out$settings$inter = inter
    out$settings$mainT = mainTraits
    out$settings$main = main
    out$interMatrix = interMatrix

    return(out)
  } else {
    species = createSpecies(speciesClass = reSim$species,...)


    out = list()
    interMatrix = matrix(NA, nrow=nrow(species$A), ncol = nrow(species$B) )

    for(i in 1:nrow(species$A)){
      x = species$A[i,]
      for(j in 1:nrow(species$B)){
        y = species$B[j,]
        spec = species$spec[j]
        interMatrix[i,j] = prod(reSim$mainFunc(x,y,spec), reSim$interFunc(x,y,spec), species$Aabund[i], species$Babund[j])
      }
    }
    out$mainFunc = reSim$mainFunc
    out$interFunc = reSim$interFunc
    rownames(interMatrix) = rownames(species$A)
    colnames(interMatrix) = rownames(species$B)

    if(species$traitsA[1] != 0) species$A[,1:species$traitsA[1]] = data.frame(apply(as.matrix(species$A[,1:species$traitsA[1]]),2,FUN = function(x) return(as.factor(x))), stringsAsFactors = T)
    if(species$traitsB[1] != 0) species$B[,1:species$traitsB[1]] = data.frame(apply(as.matrix(species$B[,1:species$traitsB[1]]),2,FUN = function(x) return(as.factor(x))), stringsAsFactors = T)

    out$A = cbind(rownames(species$A), species$A)
    colnames(out$A)[1] = "X"

    out$B = cbind(rownames(species$B), species$B)
    colnames(out$B)[1] = "Y"

    out$z = data.frame(interMatrix)

    out$poisson = function(x = 1000, seed = 42) {
      set.seed(seed)
      data.frame(matrix(as.numeric(rpois(length(interMatrix), interMatrix * x)) , ncol = ncol(interMatrix) , dimnames = list(rownames(interMatrix), colnames(interMatrix))))
    }
    out$binar = function(x = 1000, seed = 42) {
     set.seed(seed)
       data.frame(matrix(as.numeric(rpois(length(interMatrix), interMatrix * x) > 0) , ncol = ncol(interMatrix) , dimnames = list(rownames(interMatrix), colnames(interMatrix))))
    }
    out$species = species
    out$settings = reSim$settings
    return(out)
  }
}






#' Simulate batch of plant-pollinator networks
#'
#' @description wrapper around \code{\link{simulateInteraction}}. Only numerical traits are supported
#' @param numberTraitsA number traits a
#' @param numberTraitsB number traits b
#' @param size nested list size
#' @param NumberA number of individuals A
#' @param NumberB number of individuals B
#' @param one_way one way interactions or not
#' @param seed seed
#' @param cutoff only individuals with at least one interaction is used in the final data, cutoff for cropping out too small observed networks
#' @param strength_range interaction strength range
#' @export


simulate_batch = function(numberTraitsA = 6, numberTraitsB = 6, size = c(4,10), NumberA = 50, NumberB = 100,one_way = TRUE, seed = 42, cutoff = 0.5, strength_range = c(10,10), abundances = FALSE){

  cutoff = 0.5*NumberA*NumberB

  if(!is.null(seed)) set.seed(seed)
  a = paste0("A",1:numberTraitsA)
  b = paste0("B",1:numberTraitsB)

  TraitInter = sapply(1:size[1], function(x) {
    t(sapply(1:size[2], function(j) {
      n = x
      result = matrix("", nrow = x, ncol = 2, byrow = T)
      if(!one_way){
        counter = 1
        while(counter<= n){
          A = sample(a,1,replace = T, prob = sapply(a, function(v) 1/(sum(c(result) %in% v)+1)))
          B = sample(b,1, replace = T, prob = sapply(b, function(v) 1/(sum(c(result) %in% v)+1)))
          tmp = c(A,B)
          okay = apply(result, 1, FUN = function(r) sum(r == tmp))
          if(!any(okay==2)){
            result[counter,] = tmp
            counter = counter+1
          }
        }
        return(c(t(result)))

      } else {
        result = matrix("", nrow = x, ncol = 2, byrow = T)
        A = sample(a,x,replace = F)
        B = sample(b,x,replace = F)
        return(c(matrix(c(A,B), nrow = 2,byrow = T)))
      }
    }

    )
    )
  })

  TraitStrengths = sapply(1:size[1], function(x) {
    matrix(runif(size[2]*x,strength_range[1], strength_range[2]),nrow = size[2])
  }, simplify = F)


  simulatedData = vector("list", size[1])

  for(i in 1:size[1]){
    for(n in 1:size[2]) simulatedData[[i]][[n]] = simulateInteraction(main = NULL, inter = matrix(TraitInter[[i]][n,], ncol = 2, byrow = T),
                                                                      weights = list(inter = TraitStrengths[[i]][n,]),
                                                                      NumberA = NumberA, NumberB = NumberB, traitsA = c(0,numberTraitsA), traitsB = c(0,numberTraitsB),
                                                                      abundance = abundances, rangeDiscrete = 2:3, seed = NULL, setSeed = NULL)
  }
  # balances are set to ~0.4 in all cases:

  optObserv = function(params, i, n){
    if(is.na(classBalance(table(as.matrix((minOneInter(simulatedData[[i]][[n]]$binar(params)))))))) return(0.6)
    else 3*abs(0.4 - classBalance(table(as.matrix((minOneInter(simulatedData[[i]][[n]]$binar(params)))))))
  }
  classBalance = function(a) a[2]/sum(a)
  res= vector("list", size[1])
  counter = 1
  for(i in 1:size[1]){
    for(n in 1:size[2]) {
      testObs = max(strength_range)^-1 * seq(0.1,4000,by = 0.1)
      for(j in testObs) {
        preOpt = optObserv(j,i,n)
        tmp = j
        if(preOpt <0.2) break
      }
      res[[i]][[n]] = optim(tmp, optObserv, method = "Brent", lower = 0.8*tmp, upper = 1.3*tmp,i = i, n = n,control = list(maxit = 200))$par
    }
  }
  resB = vector("list",size[1])
  for(i in 1:size[1]){
    for(n in 1:size[2]) {
      resB[[i]][[n]] = classBalance(table(as.matrix((minOneInter(simulatedData[[i]][[n]]$binar(res[[i]][[n]]))))))
    }
  }
  survived_interactions = vector("list",size[1])
  for(i in 1:size[1]){
    for(n in 1:size[2]) {
      survived_interactions[[i]][[n]]=(prod(dim(minOneInter(simulatedData[[i]][[n]]$binar(res[[i]][[n]])))))
    }
  }
  print(resB)
  print(survived_interactions)

  simulatedData = lapply(1:size[1], function(i) {
    lapply(1:size[2], function(j) if(survived_interactions[[i]][j] >= cutoff) return(simulatedData[[i]][[j]]) else return(NULL))
  })

  res = lapply(1:size[1], function(i) {
    sapply(1:size[2], function(j) if(survived_interactions[[i]][j] >= cutoff) return(res[[i]][j]) else return(NA),simplify = T)
  })

  for(i in 1:size[1]){
    simulatedData[[i]][sapply(simulatedData[[i]], is.null)] <- NULL
    res[[i]] = res[[i]][!is.na(res[[i]])]
  }

  out = list()
  out$res = res
  out$survived_interactions = survived_interactions
  out$TraitStrengths = TraitStrengths
  out$TraitInter = TraitInter
  out$data = simulatedData

  return(out)

}





