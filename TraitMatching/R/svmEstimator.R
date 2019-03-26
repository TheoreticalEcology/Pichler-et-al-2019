

#' test svm
#' @param test egal

testSVM = function(test){

model_fn <- function(features, labels, mode, params, config) {
  A = tf$Variable(tf$random_normal(shape=list(features$shape$as_list()[[2]],1L)))
  b = tf$Variable(tf$random_normal(shape=list(1L,1L)))
  features = tf$cast(features, "float32")
  matmu = tf$cast(tf$matmul(features, A), "float32")
  model_output = tf$subtract(matmu, b)

  prediction = tf$sign(model_output)

  if(mode == "infer") return(estimator_spec(mode = mode, predictions =  list(target = prediction)))


  labels = tf$cast(labels, "float32")

  l2_norm = tf$reduce_sum(tf$square(A))
  alpha = tf$constant(0.1)
  classification_term = tf$reduce_mean(tf$maximum(0., tf$subtract(1.,tf$multiply(model_output, labels))))



  loss = tf$add(classification_term, tf$multiply(alpha, l2_norm))

  # accuracy = tf$reduce_mean(tf$cast(tf$equal(prediction, y_target),
  #                                   "float32"))
  #
  # Provide an estimator spec for prediction mode

  eval_metric_ops <- list(accuracy = tf$metrics$accuracy(labels, prediction))

  optimizer <- tf$train$GradientDescentOptimizer(learning_rate = params$learning_rate)
  train_op <- optimizer$minimize(loss = loss, global_step = tf$train$get_global_step())

  # Provide an estimator spec for evaluation and training modes.
  return(estimator_spec(
    mode = mode,
    loss = loss,
    train_op = train_op,
    eval_metric_ops = eval_metric_ops
  ))
}


library(tfestimators)
library(tensorflow)
model <- estimator(model_fn, params = list(learning_rate = 0.01))

abalone_input_fn <- function(data, num_epochs = 100) {
  input_fn(data, features = colnames(train)[1:8], response = "target", num_epochs = num_epochs, batch_size = 2, num_threads = 1)
}
train(model, input_fn = abalone_input_fn(train, 50), hooks = list(
  hook_progress_bar()
))
 evaluate(model, input_fn = abalone_input_fn(test,1))
p = as.data.frame(predict(model, input_fn = abalone_input_fn(test,1)))


data = data.frame(Titanic)
dummy = mlr::createDummyFeatures(data[,1:3])
data = cbind(dummy, data$Survived)
levels(data$`data$Survived`) = 1:length(levels(data$`data$Survived`))
colnames(data)[9] = "target"


data$target = as.integer(data$target) -1
data$target[data$target == 0] = -1

library(e1071)
t = e1071::svm(as.factor(target)~., train, probability = T, tolernace = 0.00001)

t= kernlab::ksvm(target~., train)
pp = predict(t, test, probability = T)

subset = sample(nrow(data), 0.8*nrow(data))
train = data[subset,]
test = data[-subset,]






data =cC$data[,c(8:14,30)]
levels(data$target) = 1:length(levels(data$target))
data$target = as.integer(data$target)-1

train = data[1:3500,]
test = data[3501:4000,]

# construct feature columns
feature_columns <- feature_columns(
  column_numeric(colnames(data)[1:7])
)



# construct input function
abalone_input_fn <- function(data) {
  input_fn(data, features = colnames(data)[1:7], response = "target", num_epochs = 2, batch_size = 20, num_threads = 1)
}


train(model, input_fn = abalone_input_fn(train))
evaluate(model, input_fn = abalone_input_fn(test))
predict(model, input_fn = abalone_input_fn(train))




batch_size = 50L
sess <- tf$Session()


x_data = tf$placeholder(shape = list(NULL,180L), "float32")
y_target = tf$placeholder(shape = list(NULL, 1L), "float32")
prediction_grid = tf$placeholder(shape = list(NULL,180L), "float32")

b = tf$Variable(tf$random_normal(shape = c(1L, batch_size)))

gamma = tf$constant(-25.0)
sq_dists = tf$multiply(tf$constant(2, "float32"), tf$matmul(x_data, tf$transpose(x_data)))
my_kernel = tf$exp(tf$multiply(gamma, tf$abs(sq_dists)))

first_term = tf$reduce_sum(b)
b_vec_cross = tf$matmul(tf$transpose(b), b)
y_target_cross = tf$matmul(y_target, tf$transpose(y_target))
second_term = tf$reduce_sum(tf$multiply(my_kernel, tf$multiply(b_vec_cross, y_target_cross)))
loss = tf$negative(tf$subtract(first_term, second_term))


rA = tf$reshape(tf$reduce_sum(tf$square(x_data), 1L), shape =c(-1L, 1L))
rB = tf$reshape(tf$reduce_sum(tf$square(prediction_grid), 1L), shape =c(-1L, 1L))
pred_sq_dist = tf$add(tf$subtract(rA, tf$multiply(2., tf$matmul(x_data, tf$transpose(prediction_grid)))), tf$transpose(rB))
pred_kernel = tf$exp(tf$multiply(gamma, tf$abs(pred_sq_dist)))

prediction_output = tf$matmul(tf$multiply(tf$transpose(y_target), b), pred_kernel)
prediction = tf$sign(prediction_output - tf$reduce_mean(prediction_output))
accuracy = tf$reduce_mean(tf$cast(tf$equal(tf$squeeze(prediction), tf$squeeze(y_target)), "float32"))

my_opt = tf$train$GradientDescentOptimizer(0.01)
train_step = my_opt$minimize(loss)

# Initialize variables
init = tf$global_variables_initializer()
sess$run(init)
loss_vec = 0
batch_accuracy = 0
x_train = data[,-181]
y_train = data[,181]
levels(y_train) = 1:length(levels(y_train))
y_train = as.character(y_train)
y_train[y_train == "2"] = "-1"
y_train = as.integer(y_train)

rownames(x_train) = 1:nrow(x_train)


for(i in 1:1300){
  rand = sample(nrow(x_train), 50)
  rand_x = x_train[rand,]
  rand_y = y_train[rand]

  sess$run(accuracy,feed_dict = dict(x_data = as.matrix(rand_x),
                                     y_target = matrix(rand_y, ncol = 1),
                                     prediction_grid = as.matrix(rand_x)))

  temp_loss = sess$run(loss, feed_dict = dict(x_data = as.matrix(rand_x),
                                              y_target =matrix(rand_y, ncol = 1)
  )
  )

  loss_vec[i] = temp_loss

  acc_temp = sess$run(accuracy, feed_dict = dict(x_data = as.matrix(rand_x),
                                                 y_target = matrix(rand_y, ncol = 1),
                                                 prediction_grid = as.matrix(rand_x)))
  batch_accuracy[i] = acc_temp


}

eval = sess$run(prediction, feed_dict = dict(x_data = as.matrix(rand_x),
                                             y_target = matrix(rand_y, ncol = 1),
                                             prediction_grid = as.matrix(x_test)))


}


