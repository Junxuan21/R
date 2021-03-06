
# using deep learning model Keras to perform image classification
library(keras)
library(dplyr)


# load MNIST image datasets
c(c(x_train, y_train), c(x_test, y_test)) %>% dataset_mnist()

# flatten images and transform RGB values into [0,1] range
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

x_train <- x_train/255
x_test <- x_test/255

# convert class vectors to binary class matrices
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# define the model
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

# compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# print a summary
summary(model)

# fit the model
history <- model %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 10,
  validation_split = 0.2
)

# plot the training history
plot(history)

# evaluate the model
model %>% evaluate(x_test, y_test)

model %>% predict_classes(x_test[1:100,])








