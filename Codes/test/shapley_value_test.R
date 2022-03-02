library(xgboost)
library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

ind_x_test <- 1:200
x_train <- as.matrix(Boston[-ind_x_test, x_var])
y_train <- Boston[-ind_x_test, y_var]
x_test <- as.matrix(Boston[ind_x_test, x_var])

# Looking at the dependence between the features
cor(x_train)
# >            lstat         rm        dis      indus
# > lstat  1.0000000 -0.6108040 -0.4928126  0.5986263
# > rm    -0.6108040  1.0000000  0.1999130 -0.3870571
# > dis   -0.4928126  0.1999130  1.0000000 -0.7060903
# > indus  0.5986263 -0.3870571 -0.7060903  1.0000000

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)
# > The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)
# >      none     lstat         rm       dis      indus
# > 1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
# > 2: 22.446 0.1671901 -0.7088401 0.9689005  0.3786871
# > 3: 22.446 5.9888022  5.5450858 0.5660134 -1.4304351
# > 4: 22.446 8.2142204  0.7507572 0.1893366  1.8298304
# > 5: 22.446 0.5059898  5.6875103 0.8432238  2.2471150
# > 6: 22.446 1.9929673 -3.6001958 0.8601984  3.1510531

# Finally we plot the resulting explanations
plot(explanation)