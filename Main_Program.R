#Main File to import dataset, train, test, validate and plot learning curves

#Install pre-requisite packages if not present
install.packages("ggplot2")
install.packages("readr")
install.packages("palmerpenguins")

#install libraries
install.packages("tensorflow")
install.packages("keras")
install.packages("dplyr")
library(dplyr)
library(tensorflow)
library(keras)

library(readr)
library(palmerpenguins)
data("penguins")
penguins <- na.omit(penguins)
summary(penguins)

#extract columns for experimentation
penguins <- penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g", "species")]
penguins$body_mass_g <- as.double(penguins$body_mass_g)
penguins$flipper_length_mm <- as.double(penguins$flipper_length_mm)
penguins$species <- as.character(penguins$species)

#randomly shuffle dataset rows
rows_count <- nrow(penguins)
for(k in 1:5){
  penguins<-penguins[sample(rows_count),]
}

source("Perceptron.r")
source("Evaluation_Cross_Validation.r")
source("Evaluation_Validation.r")
source("Evaluation_Curves.r")

validation_instances <- sample(floor(nrow(penguins) * 0.75))
penguinsTest<-penguins[-validation_instances,]
penguinsTrain<-penguins[validation_instances, ]

head(penguinsTest)

#Build Perceptron Model
p_model <- Perceptron(0.0001)

#Set number of epochs (iterations)
num_of_epochs <- 500 #Ideally, run with 1000 number of epochs but 1000 takes considerable amount (>10 min) to train

#MLP construction
#Define the number of features (input size) and the number of distinct classes (output size)
num_features <- ncol(penguins) - 1  #Number of features is columns minus the label column
num_classes <- length(unique(penguins$species)) #Number of unique species (classes)

#Create a sequential model and add layers
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'ReLU', input_shape = c(num_features)) %>%
  layer_dense(units = 128, activation = 'sigmoid') %>%
  layer_dense(units = 32, activation = 'ReLU') %>%
  layer_dense(units = num_classes, activation = 'softmax') #Softmax for multi-class classification

#Preprocess the data
penguins$species <- as.factor(penguins$species)
penguins <- penguins %>%
  mutate(across(where(is.numeric), scale)) #Scale numerical features

penguins$species <- as.integer(penguins$species) #Convert species back to integer labels

# Split data into training and testing
set.seed(123) # for reproducibility
validation_indices <- sample(nrow(penguins), size = 0.75 * nrow(penguins)) #75% for training
penguins_train <- penguins[validation_indices, ]
penguins_test <- penguins[-validation_indices, ]

#Compile the model with settings for training
model %>% compile(
  optimiser = 'adam',
  loss = 'sparse_categorical_crossentropy', #Loss function suitable for integer-labeled classification
  metrics = 'accuracy'  #Evaluate model performance based on accuracy
)

# Train the model on the training data
history <- model %>% fit(
  as.matrix(penguins_train[, -ncol(penguins)]), penguins_train$species, 
  epochs = num_of_epochs,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the Model
model %>% evaluate(as.matrix(penguins_test[, -ncol(penguins)]), penguins_test$species)

#plot Learning Curve - Accuracy vs Training Sample size
plot_learning_curve(p_model, penguinsTrain, penguinsTest, number_of_iterations = num_of_epochs)

he#plot Learning Curve - Accuracy vs Number of Epochs (Iterations)
plot_learning_curve_epochs(p_model, penguinsTrain, penguinsTest)

#plot Learning Curve - Accuracy vs Learning Rate values
plot_learning_curve_learning_Rates(penguinsTrain, penguinsTest, num_of_epochs = num_of_epochs)

#Train - Test - Cross Validate accross 10 folds
Cross_Validate(p_model, penguinsTrain, num_of_iterations = num_of_epochs, num_of_folds = 10)
#Cross_Validate(ml_model, dataset, num_of_iterations, num_of_folds)

#Validate results with held out validation dataset

Validate(p_model, penguinsTrain, penguinsTest, number_of_iterations = 10)
