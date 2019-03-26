context("workflow test")

library(TraitMatching)
# sim3 <- GetInteractions(mainTraits = 2,inter = 4,plantSpeciesnumber = 50, pollinatorSpeciesnumber = 100, randomTraits = 0, binary = F, factorPlantTraitNumber = 0, factorPollinatorTraitNumber = 0)
#
# #sim1: 31000, 170000,380000
# #sim2: 50000, 1210000,14000000
# #sim3: 2500000,65000000,800000000
# z= matrix(as.numeric(rpois(length(sim3$interactionmatrix), sim3$interactionmatrix * 50000e4)> 0) , ncol = ncol(sim3$interactionmatrix) )
# a = sim3$Plant
# b = sim3$Pollinator
#
# a = cbind(data.frame(rownames(a)), a)
# colnames(a)[1] = "X"
#
# b = cbind(data.frame(rownames(b)), b)
# colnames(b)[1] = "Y"
#
# colnames(z) = b$Y
# rownames(z) = a$X
#
# cc = createCommunity(a,b,data.frame(z))
#
#
# data = cc$data
#
# levels(data$Y) = 1:length(levels(data$Y))
# data$X = data$Y
#
# data$X = as.integer(data$X)
# data$Y = as.integer(data$Y)
#
# coords = data[,1:2]
#
# set.seed(1)
# load("data/Pollination.RData")
# cc = createCommunity(Plants, Insects, Interaction)
# r = runTM(cc, method = "RFranger", crossValidation = list(outer = list(method = "SpCV", iters = 3), inner = list(method = "SpCV", iters = 3)), block = T, tuningMetric = "f1", fitSpecies = FALSE)
#
# load("data/Pollination.RData")
# cc = createCommunity(Plants, Insects, Interaction)
# r2 = runTM(cc, method = "RFranger", crossValidation = list(outer = list(method = "CV", iters = 3), inner = list(method = "CV", iters = 3)), block = T, tuningMetric = "f1", fitSpecies = FALSE)
# r3 = runTM(cc, method = "RFranger", crossValidation = list(outer = list(method = "CV", iters = 3), inner = list(method = "CV", iters = 3)), block = T, tuningMetric = "f1", fitSpecies = "Y")
#
#
# library(TraitMatching)
# sim = simulateInteraction(traitsA = c(9,3), traitsB = c(7,3))
# table(as.matrix(sim$binar(3e3)))
#
# sim$binarZ = sim$binar(3e3)
# sim$poissonZ = sim$poisson(3e3)
#
# cC = createCommunity(sim$A, sim$B, sim$binarZ)
# r1 = runTM(createCommunity(sim$A, sim$B, sim$binarZ), balanceClasses ="Over+WC",fitSpecies = "Y",method = "cnn",
#            iters = 21,parallel = 3,crossValidation = list(outer = list(method = "SpCV", iters = 3), inner = list(method = "SpCV", iters = 3)),
#            block = T, tuningMetric = "f1")
# r2 = runTM(createCommunity(sim$A, sim$B, sim$poissonZ), iters = 9,method = c("RFranger","dnn"), parallel = 3,crossValidation = list(outer = list(method = "SpCV", iters = 3), inner = list(method = "SpCV", iters = 3)), block = T, fitSpecies = "Y")
#
#

library(TraitMatching)
library(keras)

load("data/Pollination.RData")


cc = createCommunity(Plants, Insects, Interaction)

run = runTM(cc, method = "RFranger", iters = 9, fitSpecies = F, crossValidation = list(outer = list(method = "SpCV", iters = 7), inner = list(method = "Subsample", iters = 2)), tuningMetric = "auc", tune = "random")

p = mlr::getRRPredictions(run$Result$RFranger$result)
p = mlr::setThreshold(p, 0.13)
caret::confusionMatrix(p$data$response, p$data$truth, mode = "everything", positive = "positive")
cal = generateCalibrationData(p)
plotCalibration(cal)



data = cc$data[,3:18]
x = data[,1:15]
y = data$target

p = data[,1:10]
i = data[,11:15]

x = cbind(normalizeFeatures(createDummyFeatures(p)), normalizeFeatures(createDummyFeatures(i)))
levels(y) = 1:length(levels(y))
y = as.integer(y) -1
y = keras::to_categorical(y, num_classes = 2)

subset = sample(nrow(x), nrow(x)*0.9)
subset
x = array(as.matrix(x), dim = c(nrow(x),59L, 1L) )


x_train = x[subset,,,drop = F]
y_train = y[subset,]

x_test = x[-subset,,,drop = F]
y_test = y[-subset,]

model <- keras_model_sequential()
model %>%
  layer_simple_rnn(units = 20,
                   kernel_initializer = initializer_random_normal(stddev = 0.01),
                   recurrent_initializer = initializer_identity(gain = 1.0),
                   activation = 'relu',
                   input_shape = c(59,1)) %>%
  layer_dense(units = 2) %>%
  layer_activation(activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adamax(),
  metrics = c('accuracy')
)

# Training
model %>% fit(
  x_train, y_train,
  batch_size = 50,
  epochs = 100,
  validation_data = list(x_test, y_test),
  class_weight = list("0" = 0.2, "1" = 0.8)
)













x = array_reshape(as.matrix(x), dim = c(nrow(x), 2L, 38L, 1L) )


x_train = x[subset,,,,drop = F]
y_train = y[subset,]

x_test = x[-subset,,,,drop = F]
y_test = y[-subset,]









input <- layer_input(shape = c(2L, 38L, 1L))

# Encodes a row of pixels using TimeDistributed Wrapper
encoded_rows <- input %>% time_distributed(layer_lstm(units = 20L))

# Encodes columns of encoded rows
encoded_columns <- encoded_rows %>% layer_lstm(units = 20L)

# Model output
prediction <- encoded_columns %>%
  layer_dense(units = 2L, activation = 'softmax')

model <- keras_model(input, prediction)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adamax(lr = 0.01),
  metrics = c('accuracy')
)

# Training
model %>% fit(
  x_train, y_train,
  batch_size = 50,
  epochs = 2,
  validation_data = list(x_test, y_test),
  class_weight = list("0" = 0.2, "1" = 0.8)
)

p = predict(model, x_test)

caret::confusionMatrix(as.factor(ifelse(p[,2]>0.1, 1, 0)), as.factor(y_test[,2]), positive = "1", mode = "everything")

######
library(TraitMatching)
sim1 = simulateInteraction(NumberA = 50, NumberB = 100)
sim1$z = sim1$binar(1e6)
cc = createCommunity(sim1$A, sim1$B, sim1$z)
data = cc$data

head(data)
x = data[,3:28]
y = data$target


x = as.matrix(normalizeFeatures(createDummyFeatures(x)))
levels(y) = 1:length(levels(y))
y = as.integer(y) -1
y = keras::to_categorical(y, num_classes = 2)

subset = sample(nrow(x), nrow(x)*0.9)

x_train = x[subset,]
y_train = y[subset,]

x_test = x[-subset,]
y_test = y[-subset,]


batch_size <- 30L
original_dim <- 85L
latent_dim <- 2L
intermediate_dim <- 30L
epochs <- 250L
epsilon_std <- 1.0


x <- layer_input(shape = c(original_dim))
h <- layer_dense(x, intermediate_dim, activation = "relu")
z_mean <- layer_dense(h, latent_dim)
z_log_var <- layer_dense(h, latent_dim)

sampling <- function(arg){
  z_mean <- arg[,1:2]
  z_log_var <- arg[,3:4]

  epsilon <- k_random_normal(
    shape = c(k_shape(z_mean)[[1]]),
    mean=0.,
    stddev=epsilon_std
  )

  z_mean + k_exp(z_log_var/2)*epsilon
}

# note that "output_shape" isn't necessary with the TensorFlow backend
z <- layer_concatenate(list(z_mean, z_log_var)) %>%
  layer_lambda(sampling)

# we instantiate these layers separately so as to reuse them later
decoder_h <- layer_dense(units = intermediate_dim, activation = "relu")
decoder_mean <- layer_dense(units = original_dim, activation = "sigmoid")
h_decoded <- decoder_h(z)
x_decoded_mean <- decoder_mean(h_decoded)

# end-to-end autoencoder
vae <- keras_model(x, x_decoded_mean)

# encoder, from inputs to latent space
encoder <- keras_model(x, z_mean)

# generator, from latent space to reconstructed inputs
decoder_input <- layer_input(shape = latent_dim)
h_decoded_2 <- decoder_h(decoder_input)
x_decoded_mean_2 <- decoder_mean(h_decoded_2)
generator <- keras_model(decoder_input, x_decoded_mean_2)


vae_loss <- function(x, x_decoded_mean){
  xent_loss <- (original_dim/1.0)*loss_binary_crossentropy(x, x_decoded_mean)
  kl_loss <- -0.5*k_mean(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1L)
  xent_loss + kl_loss
}

vae %>% compile(optimizer = "rmsprop", loss = vae_loss)


# Data preparation --------------------------------------------------------




# Model training ----------------------------------------------------------

vae %>% fit(
  x_train, x_train,
  shuffle = TRUE,
  epochs = epochs,
  batch_size = batch_size,
  validation_data = list(x_test, x_test)
)


# Visualizations ----------------------------------------------------------

library(ggplot2)
library(dplyr)
x_test_encoded <- predict(encoder, x_test, batch_size = batch_size)

x_test_encoded %>%
  as_data_frame() %>%
  mutate(class = as.factor(y_test[,2])) %>%
  ggplot(aes(x = V1, y = V2, colour = class)) + geom_point()


n <- 15  # figure with 15x15 digits
digit_size <- 28

# we will sample n points within [-4, 4] standard deviations
grid_x <- seq(-4, 4, length.out = n)
grid_y <- seq(-4, 4, length.out = n)

rows <- NULL
for(i in 1:length(grid_x)){
  column <- NULL
  for(j in 1:length(grid_y)){
    z_sample <- matrix(c(grid_x[i], grid_y[j]), ncol = 2)
    column <- rbind(column, predict(generator, z_sample) %>% matrix(ncol = 28) )
  }
  rows <- cbind(rows, column)
}
rows %>% as.raster() %>% plot()
