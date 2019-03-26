context("classCommunity")

test_that("test classCommunity", {

  a1 = data.frame(matrix(runif(50), ncol = 5, nrow = 10))
  b1 = data.frame(matrix(runif(40), ncol = 4, nrow = 20))

  z1 = data.frame(matrix(rbinom(200,1,0.3), nrow = 10, ncol = 20))
  z2 = data.frame(matrix(rbinom(200,12,0.3), nrow = 10, ncol = 20))


  a1 = cbind(data.frame(sapply(1:nrow(a1), function(x) paste("a", x, sep = ""))), a1)
  colnames(a1) <- c("X",sapply(2:ncol(a1), function(x) paste("A", x, sep = "")))

  b1 = cbind(data.frame(sapply(1:nrow(b1), function(x) paste("b", x, sep = ""))), b1)
  colnames(b1) <- c("Y",sapply(2:ncol(b1), function(x) paste("B", x, sep = "")))

  colnames(z1) = b1$Y
  rownames(z1) = a1$X

  colnames(z2) = b1$Y
  rownames(z2) = a1$X

  expect_error(createCommunity(a1,b1,z1), NA)
  expect_error(createCommunity(a1,b1,z2), NA)
  expect_error(createCommunity(community = list(a = list(a1,b1,z2), b =list(a1,b1,z2))), NA)
  expect_error(suppressWarnings(createCommunity(community = list(a = list(a1,b1,z2), b =createCommunity(a1,b1,z2)$data), response = "target", positive = "positive")), NA)
  expect_error(createCommunity(community =createCommunity(a1,b1,z2)$data, response = "target"), NA)
  expect_error(createCommunity(community =list(createCommunity(a1,b1,z2)$data,createCommunity(a1,b1,z2)$data,createCommunity(a1,b1,z2)$data), response = "target", positive = "positive"), NA)
  expect_error(createCommunity(a1,b1))
  expect_error(createCommunity(a1,z = z1))


})


