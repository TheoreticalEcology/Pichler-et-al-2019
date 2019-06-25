#' GetSpecies Properties function
#' @author Stefan Ried
#' @description ...
#' @param plant_abundance_dist parameter to set the plant abundance distribution. Either a numeric value, in this case the rate of the exponential distribution, or a function with parameter n for the number of samples
#' @param plantSpeciesnumber paramter to set the number of plant species
#' @param pollinatorSpeciesnumber paramter to set the number of pollinator species
#' @param plantTraitNumber parameter to set the number of plant traits on an interval scale
#' @param factorPlantTraitNumber parameter to set the number of ordinal plant traits
#' @param pollinatorTraitNumber parameter to set the number of pollinator traits on an interval scale
#' @param factorPollinatorTraitsNumber parameter to set the number of ordinal pollinator traits
#' @return A list containing two species/trait matrices and two corresponding abundance vectors
#'
GetSpeciesProperties <- function(plantSpeciesnumber = 200, pollinatorSpeciesnumber = 200, plantTraitNumber = 11, factorPlantTraitNumber = 5,
                                 pollinatorTraitNumber = 5, factorPollinatorTraitNumber = 5, abundance_dist = 1) {
  out <- list()

  if (is.numeric(abundance_dist)) {
    pla_abund <- rexp(plantSpeciesnumber, abundance_dist)
    pol_abund <- rexp(pollinatorSpeciesnumber, abundance_dist)
  }
  else if (is.function(abundance_dist)) {
    pla_abund <- abundance_dist(plantSpeciesnumber)
    pol_abund <- abundance_dist(pollinatorSpeciesnumber)
  }
  else {
    stop("wrong argument to abundance_dist")
  }

  if (plantTraitNumber >= factorPlantTraitNumber && pollinatorTraitNumber >= factorPollinatorTraitNumber) {
    out$pla_abund <- pla_abund / sum(pla_abund) # creating a vector of plant abundances

    numericPlantMatrix <- matrix(nrow = plantSpeciesnumber, ncol = plantTraitNumber) # creating a matrix of all plant species and their continuous traits
    numericPlantMatrix <- apply(numericPlantMatrix, MARGIN = 2, function(numericPlantMatrix) rnorm(plantSpeciesnumber, mean = 0, sd = 1))

    factorPlantMatrix <- matrix(nrow = plantSpeciesnumber, ncol = factorPlantTraitNumber) # creating a matrix of all plant species and their nominal traits
    levels <- rnorm(6, mean = 0, sd = 1.5)
    factorPlantMatrix <- apply(factorPlantMatrix, 2, function(factorPlantMatrix) sample(levels, plantSpeciesnumber, replace = T, prob = dnorm(1:6, mean = 3.5, sd = 1)))
    plantMatrix <- cbind(factorPlantMatrix, numericPlantMatrix) # merging the the matrices
    out$plantMatrix <- plantMatrix

     names(out$pla_abund) <- sapply(1:length(out$pla_abund), function(x) paste("B", x, sep = ""))
    colnames(out$plantMatrix) <- sapply(1:ncol(out$plantMatrix), function(x) paste("A", x, sep = ""))
    rownames(out$plantMatrix) <- sapply(1:plantSpeciesnumber, function(x) paste("a", x, sep = ""))

    out$pol_abund <- pol_abund / sum(pol_abund) # creating a vector of pollinator abundances

    numericPollinatorMatrix <- matrix(nrow = pollinatorSpeciesnumber, ncol = pollinatorTraitNumber) # creating a matrix of all pollinator species and their continuous traits
    numericPollinatorMatrix <- apply(numericPollinatorMatrix, MARGIN = 2, function(numericPollinatorMatrix) rnorm(pollinatorSpeciesnumber, mean = 0, sd = 1))

    factorPollinatorMatrix <- matrix(nrow = pollinatorSpeciesnumber, ncol = factorPollinatorTraitNumber) # creating a matrix of all pollinator species and their nominal traits
    factorPollinatorMatrix <- apply(factorPollinatorMatrix, 2, function(factorPollinatorMatrix) sample(levels, pollinatorSpeciesnumber, replace = T, prob = dnorm(1:6, mean = 3, sd = 2)))
    pollinatorMatrix <- cbind(factorPollinatorMatrix, numericPollinatorMatrix)
    out$pollinatorMatrix <- numericPollinatorMatrix

    names(out$pol_abund) <- sapply(1:length(out$pol_abund), function(x) paste("B", x, sep = ""))
    colnames(out$pollinatorMatrix) <- sapply(1:ncol(out$pollinatorMatrix), function(x) paste("B", x, sep = ""))
    rownames(out$pollinatorMatrix) <- sapply(1:pollinatorSpeciesnumber, function(x) paste("b", x, sep = ""))
  }
  else {
    stop("number of ordinal traits cannot be larger than number of total traits")
  }

  return(out)
}





# ISSUE: inter*2...problem -> check
# ISSUE: inter werden mit zurücklegen gezogen...


#' Get Interactions...
#' @author Stefan Ried
#' @description ...
#' @param mainTraits specify the number of traits that act as main effects
#' @param randomTraits specify the number of traits that have random effects
#' @param inter specify the number of interactions between traits
#' @param binary parameter to set the format of the resulting interaction matrix, if TRUE the values are binary, if false the nuerical probability values are given out
#' @param desire, desired value between 0 and 1
#' @return A list containg the interaction matrix and vectors of the sampled traits
#' @example /inst/examples/simexample.R
#'

#plantSpeciesnumber = 50, pollinatorSpeciesnumber = 50, randomTraits = 0, binary = F, factorPlantTraitNumber = 0, factorPollinatorTraitNumber = 0

GetInteractions <- function(mainTraits = 4, randomTraits = 4, inter = 5, plantSpeciesnumber = 100, pollinatorSpeciesnumber = 100, plantTraitNumber = 10,
                            factorPlantTraitNumber = 5, pollinatorTraitNumber = 8, factorPollinatorTraitNumber = 5, abundance_dist = 1, binary = TRUE, desire = 0.05) {
  if (sum(mainTraits, randomTraits, 2 * inter) > plantTraitNumber + pollinatorTraitNumber) stop("sum of main, random and interacting traits cannot be larger than total number of traits")


  mat <- GetSpeciesProperties(plantSpeciesnumber, pollinatorSpeciesnumber, plantTraitNumber, factorPlantTraitNumber, pollinatorTraitNumber, factorPollinatorTraitNumber, abundance_dist)


  out <- list()
  left_over <- NULL
  sampledmTraits <- NULL

  if (mainTraits != 0) {
    sampledmTraits <- sample(c(colnames(mat$plantMatrix), colnames(mat$pollinatorMatrix)), mainTraits)
    out$sampledmTraits <- sampledmTraits
    # sampling main effects from total number of traits
    left_over <- c(colnames(mat$plantMatrix), colnames(mat$pollinatorMatrix))[!(c(colnames(mat$plantMatrix), colnames(mat$pollinatorMatrix)) %in% sampledmTraits)]
  }

  if (inter != 0) {
    sampledInteractingTraits <- matrix("0", ncol = 2, nrow = inter)
    # sampledInteractingTraits <- t(sapply(1:inter, function(x) sample(left_over, size = 2))) # sampling interacting traits from leftover traits
    # left_over <- left_over[!left_over %in% sampledInteractingTraits]  ##### ISSUE
    for (i in 1:inter) {
      tmp_inter <- sample(left_over, size = 2)
      left_over <- left_over[!left_over %in% tmp_inter]
      sampledInteractingTraits[i, ] <- tmp_inter
    }
    out$sampledInteractingTraits <- sampledInteractingTraits
  }
  if (is.null(left_over)) left_over <- c(colnames(mat$plantMatrix), colnames(mat$pollinatorMatrix))

  sampledRandomTraits <- sample(left_over, randomTraits) # sampling random traits from leftover traits

  interp <- matrix(nrow = plantSpeciesnumber, ncol = pollinatorSpeciesnumber)
  randomp <- matrix(nrow = plantSpeciesnumber, ncol = pollinatorSpeciesnumber)
  mainp <- matrix(nrow = plantSpeciesnumber, ncol = pollinatorSpeciesnumber)

  interactionmatrix <- matrix(nrow = plantSpeciesnumber, ncol = pollinatorSpeciesnumber)

  normalization <- dnorm(0, sd = 0.9)
  normMult <- mvtnorm::dmvnorm(c(0, 0), mean = c(0, 0), sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2))

  if (inter != 0) tempInt <- t(sampledInteractingTraits)
  m <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)

  pS = rnorm(pollinatorSpeciesnumber, mean = 1, sd = 0.1)

  out$SpecGenRatioForPoll = pS

  weightsMain = runif(length(sampledmTraits))
  weightsMain = weightsMain/sum(weightsMain)
  names(weightsMain) = sampledmTraits

  weightsInter = runif(nrow(sampledInteractingTraits))
  weightsInter = weightsInter/sum(weightsInter)

  createCov = function(x){
    cv = runif(1, min = -0.9, 0.9)
    return(matrix(c(1, cv, cv, 1), ncol = 2))
  }

  interSigmas = lapply(1:inter, FUN = createCov)

  out$weightsMain = weightsMain
  out$weightsInter = weightsInter
  out$cov  = interSigmas

  for (i in 1:plantSpeciesnumber) {
    tmp <- mat$plantMatrix[i, ]

    for (j in 1:pollinatorSpeciesnumber) {
      con <- c(tmp, mat$pollinatorMatrix[j, ]) # creating a vector containing all trait values for plant species i and pollinator species j

      sd = c(rep(0.9, plantTraitNumber), rep(pS[j],pollinatorTraitNumber ))
      names(sd) <- c(colnames(mat$plantMatrix), colnames(mat$pollinatorMatrix))

      mainprobs <- 1
      if (!is.null(sampledmTraits)){
        mainprobs <- dnorm(con[sampledmTraits],sd = sd[sampledmTraits])
        for(k in 1:length(mainprobs))
          mainprobs[k] = (mainprobs[k] / dnorm(0, sd = sd[sampledmTraits][k]))*weightsMain[sampledmTraits][k]

      }  # assigning probabilities for main effects
      randomprobs <- runif(randomTraits) # assigning probabilities for random effects
      interprobs <- 1
      if (inter != 0) {
        iamat <- matrix(con[tempInt], nrow = inter, byrow = T)
        for(k in 1:nrow(iamat)){
          interprobs[k] = mvtnorm::dmvnorm(iamat[k,],mean = c(0,0), sigma = interSigmas[[k]])
          interprobs[k] = (interprobs[k]/mvtnorm::dmvnorm(c(0, 0), mean = c(0, 0), sigma = interSigmas[[k]]))*weightsInter[k]
        }
      }
      interactionmatrix[i, j] <- prod(mainprobs) * prod(randomprobs) * prod(interprobs) * mat$pla_abund[i] * mat$pol_abund[j]
    }
  }

  out$Plant <- mat$plantMatrix
  out$Pollinator <- mat$pollinatorMatrix
  out$interactionmatrix <- interactionmatrix
  out$mat <- mat
  out$sampledRandomTraits <- sampledRandomTraits
  if (binary == TRUE) {
    out <- TraitMatching:::get_desired_balance(out, desire)
  }
  return(out)
}


#' Calculate optimal time factor for desired class proportion
#'
#' @param inter, Result from GetInteractions()
#' @param desire, desired value between 0 and 1



get_desired_balance <- function(inter, desire = 0.05){
  rpois(length(inter$interactionmatrix), inter$interactionmatrix * 10000000)
  int <- inter$interactionmatrix
  len = length(inter$interactionmatrix)
  species_cols <- ncol(int)
  time <- seq(from = 1e+06, to = 1e+09, by = 0.5e+06)
  data <- sapply(time, FUN = function(x){rpois(len, int * x)} )
  proportions <- apply(data, MARGIN = 2, FUN = function(x) {table(matrix(as.numeric(x > 0), ncol = species_cols ))[2]/len} )
  lm <- lm(proportions ~ sqrt(time))
  time_new <- (desire - lm$coefficients[[1]]) / lm$coefficients[[2]]
  inter$binar <- matrix(as.numeric(rpois(len, int * ((desire - lm$coefficients[[1]]) / lm$coefficients[[2]])^2) > 0), ncol = species_cols )
  inter$optimal_time_factor <- time_new

  rownames(inter$binar) <- rownames(inter$Plant)
  colnames(inter$binar) <- rownames(inter$Pollinator)
  return(inter)

}

