#!/usr/bin/env Rscript

# Test script for random_experiments function in TRexSelector

# Load the TRexSelector package
library(TRexSelector)

# Set random seed for reproducibility
set.seed(42)  # Using same seed as Python script

# Create a simple test dataset
create_test_data <- function(n = 75, p = 100, num_active = 5, snr = 1.0) {
  # Generate random predictor matrix
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  
  # Create true coefficients (only a few active variables)
  beta <- numeric(p)
  active_indices <- sample(1:p, num_active)
  beta[active_indices] <- 1
  
  # Generate response vector with noise
  y <- X %*% beta + rnorm(n) * sqrt(var(X %*% beta) / snr)
  
  return(list(X = X, y = y, beta = beta, active_indices = active_indices))
}

# Function to save test data to CSV files
save_test_data <- function(test_data, prefix) {
  # Create test_data directory if it doesn't exist
  if (!dir.exists("test_data")) {
    dir.create("test_data")
  }
  
  # Save X matrix
  write.csv(test_data$X, file = paste0("test_data/", prefix, "_X.csv"), row.names = FALSE)
  
  # Save y vector
  write.csv(test_data$y, file = paste0("test_data/", prefix, "_y.csv"), row.names = FALSE)
  
  # Save beta vector
  write.csv(test_data$beta, file = paste0("test_data/", prefix, "_beta.csv"), row.names = FALSE)
  
  # Save active_indices vector
  write.csv(test_data$active_indices, file = paste0("test_data/", prefix, "_active_indices.csv"), row.names = FALSE)
  
  cat("Test data saved to test_data/", prefix, "*.csv files\n")
}

# Test the basic functionality of random_experiments
test_random_experiments_basic <- function() {
  cat("Testing random_experiments basic functionality...\n")
  
  # Generate test data
  test_data <- create_test_data(n = 75, p = 100, num_active = 5, snr = 1.0)
  X <- test_data$X
  y <- test_data$y
  
  # Save test data to CSV
  save_test_data(test_data, "rand_exp_basic")
  
  # Run random_experiments with default parameters
  t_start <- Sys.time()
  result <- random_experiments(X = X, y = y, K = 5, T_stop = 1, num_dummies = 20)
  t_end <- Sys.time()
  
  # Print some info about the result
  cat("Time taken:", t_end - t_start, "seconds\n")
  cat("Class of result:", class(result), "\n")
  
  # Save phi_T_mat to CSV for comparison with Python
  write.csv(result$phi_T_mat, file = "test_data/rand_exp_basic_phi_T_mat_R.csv", row.names = FALSE)
  write.csv(result$Phi, file = "test_data/rand_exp_basic_Phi_R.csv", row.names = FALSE)
  
  # Print some summary statistics
  cat("Dimensions of phi_T_mat:", dim(result$phi_T_mat), "\n")
  cat("Sum of phi_T_mat:", sum(result$phi_T_mat), "\n")
  cat("Length of Phi:", length(result$Phi), "\n")
  cat("Sum of Phi:", sum(result$Phi), "\n")
  
  cat("Test completed.\n\n")
  return(result)
}

# Test random_experiments with different K values
test_random_experiments_K <- function() {
  cat("Testing random_experiments with different K values...\n")
  
  # Generate test data
  test_data <- create_test_data(n = 75, p = 100, num_active = 5, snr = 1.0)
  X <- test_data$X
  y <- test_data$y
  
  # Save test data to CSV
  save_test_data(test_data, "rand_exp_K")
  
  # Test with different K values
  for (K_val in c(3, 5)) {
    cat("K =", K_val, "\n")
    
    # Run random_experiments
    t_start <- Sys.time()
    result <- random_experiments(X = X, y = y, K = K_val, T_stop = 1, num_dummies = 20)
    t_end <- Sys.time()
    
    cat("Time taken:", t_end - t_start, "seconds\n")
    
    # Save phi_T_mat to CSV for comparison with Python
    write.csv(result$phi_T_mat, file = paste0("test_data/rand_exp_K", K_val, "_phi_T_mat_R.csv"), row.names = FALSE)
    write.csv(result$Phi, file = paste0("test_data/rand_exp_K", K_val, "_Phi_R.csv"), row.names = FALSE)
    
    # Print some summary statistics
    cat("Dimensions of phi_T_mat:", dim(result$phi_T_mat), "\n")
    cat("Sum of phi_T_mat:", sum(result$phi_T_mat), "\n")
    cat("Length of Phi:", length(result$Phi), "\n")
    cat("Sum of Phi:", sum(result$Phi), "\n\n")
  }
  
  cat("Test completed.\n\n")
}

# Test random_experiments with different T_stop values
test_random_experiments_T_stop <- function() {
  cat("Testing random_experiments with different T_stop values...\n")
  
  # Generate test data
  test_data <- create_test_data(n = 75, p = 100, num_active = 5, snr = 1.0)
  X <- test_data$X
  y <- test_data$y
  
  # Save test data to CSV
  save_test_data(test_data, "rand_exp_T_stop")
  
  # Test with different T_stop values
  for (t_stop in c(1, 2)) {
    cat("T_stop =", t_stop, "\n")
    
    # Run random_experiments
    t_start <- Sys.time()
    result <- random_experiments(X = X, y = y, K = 3, T_stop = t_stop, num_dummies = 20)
    t_end <- Sys.time()
    
    cat("Time taken:", t_end - t_start, "seconds\n")
    
    # Save phi_T_mat to CSV for comparison with Python
    write.csv(result$phi_T_mat, file = paste0("test_data/rand_exp_T_stop", t_stop, "_phi_T_mat_R.csv"), row.names = FALSE)
    write.csv(result$Phi, file = paste0("test_data/rand_exp_T_stop", t_stop, "_Phi_R.csv"), row.names = FALSE)
    
    # Print some summary statistics
    cat("Dimensions of phi_T_mat:", dim(result$phi_T_mat), "\n")
    cat("Sum of phi_T_mat:", sum(result$phi_T_mat), "\n")
    cat("Length of Phi:", length(result$Phi), "\n")
    cat("Sum of Phi:", sum(result$Phi), "\n\n")
  }
  
  cat("Test completed.\n\n")
}

# Test random_experiments with different method values
test_random_experiments_methods <- function() {
  cat("Testing random_experiments with different method values...\n")
  
  # Generate test data
  test_data <- create_test_data(n = 75, p = 100, num_active = 5, snr = 1.0)
  X <- test_data$X
  y <- test_data$y
  
  # Save test data to CSV
  save_test_data(test_data, "rand_exp_methods")
  
  # Test with different method values
  methods <- c("trex")  # Only testing "trex" for simplicity
  
  for (method in methods) {
    cat("Method =", method, "\n")
    
    # Run random_experiments
    t_start <- Sys.time()
    result <- random_experiments(X = X, y = y, K = 3, T_stop = 1, num_dummies = 20, method = method)
    t_end <- Sys.time()
    
    cat("Time taken:", t_end - t_start, "seconds\n")
    
    # Save phi_T_mat to CSV for comparison with Python
    write.csv(result$phi_T_mat, file = paste0("test_data/rand_exp_method_", method, "_phi_T_mat_R.csv"), row.names = FALSE)
    write.csv(result$Phi, file = paste0("test_data/rand_exp_method_", method, "_Phi_R.csv"), row.names = FALSE)
    
    # Print some summary statistics
    cat("Dimensions of phi_T_mat:", dim(result$phi_T_mat), "\n")
    cat("Sum of phi_T_mat:", sum(result$phi_T_mat), "\n")
    cat("Length of Phi:", length(result$Phi), "\n")
    cat("Sum of Phi:", sum(result$Phi), "\n\n")
  }
  
  cat("Test completed.\n\n")
}

# Run all tests
run_all_tests <- function() {
  cat("=== Running all tests for random_experiments in R ===\n\n")
  
  test_random_experiments_basic()
  test_random_experiments_K()
  test_random_experiments_T_stop()
  test_random_experiments_methods()
  
  cat("=== All tests completed ===\n")
}

# Execute all tests
run_all_tests() 