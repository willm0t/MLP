plot_learning_curve <- function(ml_model, train_dataset, validation_dataset, number_of_iterations = 10){
  library(ggplot2)
  
  # Determine the total number of rows in the training dataset
  total_rows <- nrow(train_dataset)
  
  # Calculate the number of rows for each 2% increment
  increment_rows <- as.integer(total_rows * 0.02)
  
  # Calculate the start and end points based on the percentage of the dataset
  start_rows <- as.integer(total_rows * 0.1)
  end_rows <- as.integer(total_rows * 1)
  
  # Initialize vectors for storing accuracies and the corresponding sample sizes
  accuracy_training <- vector()
  sample_sizes <- vector()
  
  # Adjust the end_rows if it does not align with the increments
  if ((end_rows - start_rows) %% increment_rows != 0) {
    end_rows <- start_rows + (increment_rows * ((end_rows - start_rows) %/% increment_rows))
  }
  
  # Train the model and calculate accuracy for each sample size increment
  for(upper_limit in seq(from = start_rows, to = end_rows, by = increment_rows)){
    # Subset the training dataset based on the current upper limit
    df_train_features <- train_dataset[1:upper_limit, 1:4]
    df_train_labels <- train_dataset[1:upper_limit, 5]
    
    correct <- 0
    total <- 0
    for(k in 1:number_of_iterations){
      for(i in 1:nrow(df_train_features)){
        actual_class <- as.character(df_train_labels[i,])
        feature_row <- as.matrix(df_train_features[i,], nrow = 1, ncol = 4)
        predicted_class <- ml_model$train(actual_class, feature_row)
        if(actual_class == predicted_class){
          correct <- correct + 1
        }
        total <- total + 1
      }
    }
    # Store the calculated accuracy and the corresponding sample size
    accuracy_training <- c(accuracy_training, correct / total)
    sample_sizes <- c(sample_sizes, (upper_limit / total_rows) * 100)
  }
  
  # Find the index of the highest accuracy
  max_accuracy_index <- which.max(accuracy_training)
  
  # Extract the highest accuracy
  highest_accuracy <- accuracy_training[max_accuracy_index]
  
  # Extract the sample size corresponding to the highest accuracy
  highest_accuracy_sample_size <- sample_sizes[max_accuracy_index]
  
  # Print the highest accuracy and corresponding sample size
  cat("Highest Accuracy:", highest_accuracy, "\n")
  cat("Corresponding Training Sample Size (%):", highest_accuracy_sample_size, "\n")
  
  # Create a dataframe for plotting
  loc_data <- data.frame(training_sample = sample_sizes, accuracy = accuracy_training)
  
  # Plot the learning curve
  ggplot(loc_data) + 
    geom_line(mapping = aes(x = training_sample, y = accuracy)) + 
    ylim(0, 1) +
    labs(x = 'Training Sample Size (%)', y = 'Accuracy')
}
plot_learning_curve_epochs <- function(ml_model, train_dataset, validation_dataset){
  #Function that plots learning curve for accuracy vs number of epochs
  #
  #Args:
  # ml_model: Perceptron Model trained earlier
  # train_dataset: 2/3rd training set
  # validation_dataset: 1/3rd held out unseen data

  #
  accuracy_training <- vector()
  accuracy_validation <- vector()
  num_of_epochs <- c(10, 50,100, 1000)
  
  
  for(num_of_iterations in num_of_epochs){

    df_train_features <- train_dataset[1:4]
    df_train_labels <- train_dataset[5]

    train_rows <- nrow(df_train_features)
    
    correct <-0
    total <- 0
    for(k in 1:num_of_iterations){
      for(i in 1:train_rows){
        #print("-------------------------------------")
        #print(paste("Run #",k))
        actual_class <- as.character(df_train_labels[i,])
        feature_row<-matrix(df_train_features[i,], nrow = 1, ncol = 4)
        feature_row <- unlist(feature_row)
        predicted_class <- ml_model$train(actual_class, feature_row)
        #print(paste("Predicted = ", pred, "Actual = ", bv))
        if(actual_class == predicted_class){
          correct <- correct +1
        }
        total <- total + 1
        #print("-------------------------------------")
        
      }
    }
    accuracy_training <- c(accuracy_training, correct/ total)
    # print(paste("Training Accuracy = ", accuracy_training))
    
  }
  library(ggplot2);
  loc_data <- data.frame(number_of_epochs = c(0, num_of_epochs), accuracy = c(0, accuracy_training))
  ggplot(loc_data) + geom_line(mapping = aes(x=number_of_epochs, y = accuracy)) + ylim(0:1) +
    labs(x = 'number of epochs (iterations)', y = 'Accuracy')
}

plot_learning_curve_learning_Rates <- function(train_dataset, validation_dataset, num_of_epochs){
  #Function that plots learning curve for accuracy vs different learning rates
  #
  #Args:
  # ml_model: Perceptron Model trained earlier
  # train_dataset: 2/3rd training set
  # validation_dataset: 1/3rd held out unseen data
  # num_of_epochs: number of epochs to train Perceptron
  #
  accuracy_training <- vector()
  accuracy_validation <- vector()
  learning_rates <- c(0.1, 0.01, 0.001, 0.0001)
  
  for(learn_rate in learning_rates){
    ml_model <- Perceptron(learn_rate)
    
    df_train_features <- train_dataset[1:4]
    df_train_labels <- train_dataset[5]
    
    train_rows <- nrow(df_train_features)
    
    correct <-0
    total <- 0
    for(k in 1:num_of_epochs){
      for(i in 1:train_rows){
        actual_class <- as.character(df_train_labels[i,])
        feature_row<-matrix(df_train_features[i,], nrow = 1, ncol = 4)
        feature_row <- unlist(feature_row)
        predicted_class <- ml_model$train(actual_class, feature_row)
        if(actual_class == predicted_class){
          correct <- correct +1
        }
        total <- total + 1
        
      }
    }
    accuracy_training <- c(accuracy_training, correct/ total)
    
  }
  # Find the index of the highest accuracy
  max_accuracy_index <- which.max(accuracy_training)
  
  # Extract the highest accuracy and corresponding learning rate
  highest_accuracy <- accuracy_training[max_accuracy_index]
  best_learning_rate <- learning_rates[max_accuracy_index]
  
  # Print the highest accuracy and corresponding learning rate
  cat("Highest Accuracy:", highest_accuracy, "\n")
  cat("Best Learning Rate:", best_learning_rate, "\n")
  
  library(ggplot2);
  loc_data <- data.frame(num_learning_rates = c(0, learning_rates), accuracy = c(0, accuracy_training))
  ggplot(loc_data) + geom_line(mapping = aes(x=num_learning_rates, y = accuracy)) + ylim(0:1) +
    labs(x = 'Learning Rates', y = 'Accuracy')
}