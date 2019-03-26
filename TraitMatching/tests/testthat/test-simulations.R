context("test simulation")

test_that("test createSpecies",{

  expect_error(createSpecies(), NA)
  expect_error(createSpecies(traitsA = c(1,15),traitsB = c(15,1)), NA)
  expect_error(createSpecies(traitsA = c(0,15),traitsB = c(0,1)), NA)
})

test_that("test simulateInter",{
  expect_error(simulateInteraction(), NA)
  expect_error(simulateInteraction(main = NULL), NA)
  expect_error(simulateInteraction(inter = NULL), NA)
  expect_error(simulateInteraction(traitsA = c(1,15),traitsB = c(15,1)), NA)
  expect_error(simulateInteraction(traitsA = c(0,15),traitsB = c(15,0)), NA)
})

spec = simulateInteraction(traitsA = c(0,10),traitsB = c(0,10),NumberA = 20, NumberB = 20)


cc = createCommunity(spec$A, spec$B, minOneInter(spec$poisson(1e5) ),log = F)


test1 = runTM(cc, method = "multiNomDnn", settings = list(multiNomDnn = list(opti = "rmsprop")),iters = 1, crossValidation = list(outer = list(method = "SpCV", iters = 2), inner = list(method = "SpCV", iters = 2)),fitSpecies = F,keepModels = T,parallel = FALSE)
