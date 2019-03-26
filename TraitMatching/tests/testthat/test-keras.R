context("test keras models")

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

classCommunity = createCommunity(a1,b1,z2)
classCommunity$data = classCommunity$data[,1:12]
classCommunityC = createCommunity(a1,b1,z1)

test_that("dnn Classification",{
  skip_on_cran()
  skip_on_travis()

  expect_s3_class(runTM(classCommunityC, method = "dnn",
        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
        parallel = F,balanceClasses = "WeightedClasses" ,iters = 1, settings = list(dnn = list(epochs = 1)))$Result$dnn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "dnn",
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
                        parallel = F, iters = 1, fitSpecies = T, settings = list(dnn = list(epochs = 1)))$Result$dnn$result,c("ResampleResult", "list"))
})

test_that("cnn Classification",{
  skip_on_cran()
  skip_on_travis()

  expect_s3_class(runTM(classCommunityC, method = "cnn",
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
                        parallel = F, iters = 1, settings = list(cnn = list(epochs = 1, nConv = 1)))$Result$cnn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "cnn",
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
                        parallel = F, iters = 1, fitSpecies = T, settings = list(cnn = list(epochs = 1)))$Result$cnn$result,c("ResampleResult", "list"))
})


# test_that("preDnn Classification",{
#   skip_on_cran()
#   skip_on_travis()
#
#   expect_s3_class(runTM(classCommunityC, method = "preDnn",
#                         crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
#                         parallel = F, iters = 1,settings = list(preDnn = list(epochs = 1)))$Result$preDnn$result,c("ResampleResult", "list"))
#   expect_s3_class(runTM(classCommunityC, method = "preDnn",
#                         crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
#                         parallel = F, iters = 1, fitSpecies = T,settings = list(preDnn = list(epochs = 1)))$Result$preDnn$result,c("ResampleResult", "list"))
#
# })


test_that("dnn Regression",{
  skip_on_cran()
  skip_on_travis()

  expect_s3_class(runTM(classCommunity, method = "dnn",
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
                        parallel = F, iters = 1, settings = list(dnn = list(epochs = 1)))$Result$dnn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunity, method = "dnn",
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
                        parallel = F, iters = 1, fitSpecies = T,settings = list(dnn = list(epochs = 1)))$Result$dnn$result,c("ResampleResult", "list"))
})

test_that("cnn Regression",{
  skip_on_cran()
  skip_on_travis()

  expect_s3_class(runTM(classCommunity, method = "cnn",
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
                        parallel = F, iters = 1,settings = list(cnn = list(epochs = 1)))$Result$cnn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunity, method = "cnn",
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
                        parallel = F, iters = 1, fitSpecies = T,settings = list(cnn = list(epochs = 1)))$Result$cnn$result,c("ResampleResult", "list"))
})


# test_that("preDnn Regression",{
#   skip_on_cran()
#   skip_on_travis()
#
#   expect_s3_class(runTM(classCommunity, method = "preDnn",
#                         crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
#                         parallel = F, iters = 1,settings = list(preDnn = list(epochs = 1)))$Result$preDnn$result,c("ResampleResult", "list"))
#   expect_s3_class(runTM(classCommunity, method = "preDnn",
#                         crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)),
#                         parallel = F, iters = 1, fitSpecies = T,settings = list(preDnn = list(epochs = 1)))$Result$preDnn$result,c("ResampleResult", "list"))
#
# })
