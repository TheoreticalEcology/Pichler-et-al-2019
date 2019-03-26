context("Test runTM")

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
classCommunityC = createCommunity(a1,b1,z1)

test_that("Regression ranger",{
  expect_s3_class(runTM(classCommunity, method = "RFranger", 
                         crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                         parallel = F, iters = 1)$Result$RFranger$result,c("ResampleResult", "list"))})
test_that("Regression knn",{
  expect_s3_class(runTM(classCommunity, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1)$Result$knn$result,c("ResampleResult", "list"))})

test_that("Regression boost",{
  expect_s3_class(runTM(classCommunity, method = "boost", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1)$Result$boost$result,c("ResampleResult", "list"))})

test_that("Regression svm",{
  expect_s3_class(runTM(classCommunity, method = "SVM", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1)$Result$SVM$result, c("ResampleResult", "list"))})

test_that("Regression fitSpecies Tests",{
  expect_s3_class(runTM(classCommunity, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1, fitSpecies = F)$Result$knn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunity, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1, fitSpecies = "X")$Result$knn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunity, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1, fitSpecies = T)$Result$knn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunity, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1, fitSpecies = c("X", "Y"))$Result$knn$result,c("ResampleResult", "list"))
  })



test_that("Classification Models",{
  skip_on_cran()
  expect_s3_class(runTM(classCommunityC, method = "RFranger", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1)$Result$RFranger$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1)$Result$knn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "boost", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1)$Result$boost$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "SVM", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1)$Result$SVM$result, c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1, fitSpecies = F)$Result$knn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1, fitSpecies = "X")$Result$knn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1, fitSpecies = T)$Result$knn$result,c("ResampleResult", "list"))
  expect_s3_class(runTM(classCommunityC, method = "knn", 
                        crossValidation = list(outer = list(method = "CV", iters = 2),inner = list(method = "CV", iters = 2)), 
                        parallel = F, iters = 1, fitSpecies = c("X", "Y"))$Result$knn$result,c("ResampleResult", "list"))
})


