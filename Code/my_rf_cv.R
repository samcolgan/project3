#' Random Forest Cross-Validation Error
#'
#' This function conducts a random forest analysis of the penguins dataset and
#'   reports the cross-validation misclassification test error.
#'
#' @param k_cv Integer representing number of folds used for calculating
#'   cross-validation error.
#' @keywords statistical prediction
#'
#' @return \code{cv_err}: Integer for cross-validation error.
#'
#' @examples
#' # Test with k = 5 folds
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # Omit missing observations
  penguins <- na.omit(my_penguins)
  # Overwrite training and data set
  penguin_train <- data.frame("bill_length_mm" = penguins$bill_length_mm,
                              "bill_depth_mm" = penguins$bill_depth_mm,
                              "flipper_length" = penguins$flipper_length_mm,
                              "body_mass_g" = penguins$body_mass_g)
  # Split data into k folds, randomly
  fold <- sample(rep(1:k, length(penguin_train[[1]]) / k))
  # In case length(fold) is too small because of extra decimal amount...
  # Find extra decimal amount and calculate number of missing folds
  decimal <- (length(penguin_train[[1]]) / k) - floor(length(penguin_train[[1]]) / k)
  missing_folds <- k * decimal
  # Generate missing folds
  folds_extra <- sample(1:k)
  folds_extra <- folds_extra[1:round(missing_folds)]
  # Create full-length folds vector
  fold <- c(fold, folds_extra)
  # Data frame of training data and folds
  data <- data.frame("train" = penguin_train, "fold" = fold)
  # Create empty list with k objects to store random forest predictions
  pred <- vector(mode = "list", length = k)
  # Create empty vector to store MSE of random forest predictions per fold
  mse <- c(rep(NA, k))
  # For each fold k...
  for (i in 1:k) {
    # Training data are those with fold value not equal to k
    data_train <- data %>% filter(fold != i)
    # Test data are those with fold value k
    data_test <- data %>% filter(fold == i)
    # Initialize empty vector for each fold
    pred[[i]] <- c(rep(NA, length(data_test$test.body_mass_g)))
    # Train random forest model
    pred_model <- randomForest(train.body_mass_g ~ train.bill_length_mm + train.bill_depth_mm + train.flipper_length, data = data_train, ntree = 100)
    # Calculate predictions of body_mass_g
    pred[[i]] <- predict(pred_model, data_test[, -4])
    # Calculate MSE
    mse[i] <- mean((pred[[i]] - data_test$train.body_mass_g)^2)
  }
  # CV_err is the average of the MSE across all folds
  cv_err <- mean(mse)
  # Return cv_err
  return(cv_err)
}
