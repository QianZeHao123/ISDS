# -----------------------------------------------------------------------------
#   ___ ____  ____  ____
#  |_ _/ ___||  _ \/ ___|
#   | |\___ \| | | \___ \
#   | | ___) | |_| |___) |
#  |___|____/|____/|____/
#
#    ____                         _  ___
#   / ___|_ __ ___  _   _ _ __   / |/ _ \
#  | |  _| '__/ _ \| | | | '_ \  | | | | |
#  | |_| | | | (_) | |_| | |_) | | | |_| |
#   \____|_|  \___/ \__,_| .__/  |_|\___/
#
# -----------------------------------------------------------------------------
# install necessary R packages
# install.packages('leaps')
# install.packages("plyr")
# install.packages("bestglm")
# install.packages("mice")
# install.packages("GGally")
# install.package("car")
# -----------------------------------------------------------------------------
# clear the environment var area
rm(list = ls())
# clear all plots
graphics.off()
# clear the console area
cat("\014")
# -----------------------------------------------------------------------------
# current_directory
current_directory = getwd()
# read_csv
# joint file path
file_path = file.path(current_directory, "Happy.csv")
Happy_origin = read.csv(file_path)
# -----------------------------------------------------------------------------
# show data
head(Happy_origin)
# Check if we have any missing values
sum(is.na(Happy_origin))
# -----------------------------------------------------------------------------
# Missing Value Handling
# Using mice to insert empty data of State of Palestine
library(mice)
Happy_complete = complete(mice(Happy_origin))
head(Happy_complete)
# -----------------------------------------------------------------------------
Happy_general_continent = Happy_complete
Continent_mapping =
  c(
    "North America" = 1,
    "South America" = 2,
    "Europe" = 3,
    "Asia" = 4,
    "Africa" = 5,
    "Oceania" = 6
  )
Continent_mapping
# -----------------------------------------------------------------------------
# Happy_origin: Data Set download from ultra
# Happy_complete: Complete empty data with mice
# Happy_general_continent: All the data with numeric continent
# Happy_general: All the data with ignored continent
# Africa: Area data
# Asia: Area data
# Europe: Area data
# North_America: Area data
# Oceania: Area data
# South_America: Area data
# -----------------------------------------------------------------------------
Africa = Happy_general_continent[Happy_general_continent$Continent == "Africa",]
Africa$Country_name = NULL
Africa$Continent = NULL
# -----------------------------------------------------------------------------
Asia = Happy_general_continent[Happy_general_continent$Continent == "Asia",]
Asia$Country_name = NULL
Asia$Continent = NULL
# -----------------------------------------------------------------------------
Europe = Happy_general_continent[Happy_general_continent$Continent == "Europe",]
Europe$Country_name = NULL
Europe$Continent = NULL
# -----------------------------------------------------------------------------
North_America = Happy_general_continent[Happy_general_continent$Continent == "North America",]
North_America$Country_name = NULL
North_America$Continent = NULL
# -----------------------------------------------------------------------------
Oceania = Happy_general_continent[Happy_general_continent$Continent == "Oceania",]
Oceania$Country_name = NULL
Oceania$Continent = NULL
# -----------------------------------------------------------------------------
South_America = Happy_general_continent[Happy_general_continent$Continent == "South America",]
South_America$Country_name = NULL
South_America$Continent = NULL
# -----------------------------------------------------------------------------
Happy_general_continent$Numeric_continent = Continent_mapping[Happy_general_continent$Continent]
Happy_general_continent$Continent = NULL
Happy_general_continent$Country_name = NULL
Happy_general_continent = Happy_general_continent[, c(1, 2, 3, 4, 5, 7, 6)]
# -----------------------------------------------------------------------------
Happy_general = Happy_general_continent
Happy_general$Numeric_continent = NULL
# -----------------------------------------------------------------------------
# create scatter plot matrix with pairs()
# pairs(Happy_general_continent, main = "General Data with Numeric Continent")
# pairs(Happy_general, main = "General Data Scatterplot")
# pairs(Africa, main = "Africa Data Scatterplot")
# pairs(Asia, main = "Asia Data Scatterplot")
# pairs(Europe, main = "Europe Data Scatterplot")
# pairs(North_America, main = "North America Data Scatterplot")
# pairs(Oceania, main = "Oceania Data Scatterplot")
# pairs(South_America, main = "South America Data Scatterplot")
# -----------------------------------------------------------------------------
# create scatter plot matrix with pairs()
# install and load the GGally library
library(GGally)
# generate the pairs plot
# ggpairs(Happy_general_continent, main = "General Data with Numeric Continent")
# ggpairs(Happy_general, main = "General Data Scatterplot")
# ggpairs(Africa, main = "Africa Data Scatterplot")
# ggpairs(Asia, main = "Asia Data Scatterplot")
# ggpairs(Europe, main = "Europe Data Scatterplot")
# ggpairs(North_America, main = "North America Data Scatterplot")
# ggpairs(Oceania, main = "Oceania Data Scatterplot")
# ggpairs(South_America, main = "South America Data Scatterplot")
# -----------------------------------------------------------------------------
# calculate the correlation coefficient with pearson method
cor_Happy_general = cor(Happy_general)
# correlation among one continent
cor_Africa = cor(Africa)
cor_Asia = cor(Asia)
cor_Europe = cor(Europe)
cor_North_America = cor(North_America)
cor_Oceania = cor(Oceania)
cor_South_America = cor(South_America)
# -----------------------------------------------------------------------------
# A new dataset: Europe_Oceania
Europe_Oceania = rbind(Europe, Oceania)

# -----------------------------------------------------------------------------
#
#   ____  _             _
#  / ___|(_)_ __   __ _| | ___
#  \___ \| | '_ \ / _` | |/ _ \
#   ___) | | | | | (_| | |  __/
#  |____/|_|_| |_|\__, |_|\___|
#                 |___/
#   _     _
#  | |   (_)_ __   ___  __ _ _ __
#  | |   | | '_ \ / _ \/ _` | '__|
#  | |___| | | | |  __/ (_| | |
#  |_____|_|_| |_|\___|\__,_|_|
#
# -----------------------------------------------------------------------------
# Simple Linear Regression
regAnalytics = function(Dataset, argument) {
  reg = lm(paste("Ladder_score", "~", argument), data = Dataset)
  print(summary(reg))
  # Confidence and prediction interval
  # Regression Diagnostics
  par(mfrow = c(2, 2))
  plot(reg,
       pch = 16,
       col = "cornflowerblue",
       main = argument)
  par(mfrow = c(1, 1))
}

regAnalytics(Happy_general, 'LGDP')
regAnalytics(Happy_general, 'Support')
regAnalytics(Happy_general, 'HLE')
regAnalytics(Happy_general, 'Freedom')
regAnalytics(Happy_general, 'Corruption')
# -----------------------------------------------------------------------------
#
# Residual diagnostics
# Autocorrelation
autocor <- function(x) {
  xreg <- lm(Ladder_score ~ x, data = Happy_general)
  X <- xreg$residuals
  plot(X[1:136], X[2:137])
  cor(X[1:136], X[2:137])
  a <- sample(seq(1, 137, by = 1),
              replace = FALSE,
              prob = rep(1 / 137, 137))
  modrandom <- lm(Ladder_score[a] ~ x[a], data = Happy_general)
  P <- modrandom$residuals
  plot(P[1:136], P[2:137])
  cor(P[1:136], P[2:137])
  print(cor(X[1:136], X[2:137]))
  print(cor(P[1:136], P[2:137]))
  print(summary(xreg)$adj.r.sq)
  print(summary(modrandom)$adj.r.sq)
}

autocor(Happy_general$LGDP)
autocor(Happy_general$Support)
autocor(Happy_general$HLE)
autocor(Happy_general$Freedom)
autocor(Happy_general$Corruption)
# -----------------------------------------------------------------------------
# Remove  Outliers
RemoveOutliers <- function(Dataset) {
  boxplots_name <- boxplot.stats(Dataset$Ladder_score)
  outliers <- boxplots_name$out
  outliers_row <- Dataset$Ladder_score %in% outliers
  clean_name <- Dataset[!outliers_row,]
  return(clean_name)
}

clean_Happy_general <- RemoveOutliers(Happy_general)

# Perform linear regression
regAnalytics(clean_Happy_general, 'LGDP')
regAnalytics(clean_Happy_general, 'Support')
regAnalytics(clean_Happy_general, 'HLE')
regAnalytics(clean_Happy_general, 'Freedom')
regAnalytics(clean_Happy_general, 'Corruption')
# -----------------------------------------------------------------------------
# This code lacks organization ???
# interaction between variables
# mean(Oceania$LGDP)
# Europe
# int1 <-
#   lm(log(Ladder_score) ~ LGDP + Corruption + Support * Freedom, data = South_America)
# summary(int1)$adj.r.sq
# int2 <- lm(Ladder_score ~ LGDP * Corruption, data = Happy_general)
# summary(int2)$r.sq
# -----------------------------------------------------------------------------
#
#
#   __  __       _ _   _       _
#  |  \/  |_   _| | |_(_)_ __ | | ___
#  | |\/| | | | | | __| | '_ \| |/ _ \
#  | |  | | |_| | | |_| | |_) | |  __/
#  |_|  |_|\__,_|_|\__|_| .__/|_|\___|
#                       |_|
#   _     _
#  | |   (_)_ __   ___  __ _ _ __
#  | |   | | '_ \ / _ \/ _` | '__|
#  | |___| | | | |  __/ (_| | |
#  |_____|_|_| |_|\___|\__,_|_|
#
#
#
# -----------------------------------------------------------------------------
# Check Multi-collinearity of variables with VIF:
# -----------------------------------------------------------------------------
reg_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y = y)
  ## Fit the model to the training data
  if (ncol(Xy) > 1)
    tmp_fit = lm(y ~ ., data = Xy[!test_data,])
  else
    tmp_fit = lm(y ~ 1, data = Xy[!test_data, , drop = FALSE])
  ## Generate predictions over the test data
  yhat = predict(tmp_fit, Xy[test_data, , drop = FALSE])
  yobs = y[test_data]
  ## Compute the test MSE
  test_error = mean((yobs - yhat)^2)
  return(test_error)
}

# -----------------------------------------------------------------------------
reg_bss_cv = function(X, y, fold_ind) {
  p = ncol(X)
  Xy = cbind(X, y = y)
  nfolds = max(fold_ind)
  if (!all.equal(sort(unique(fold_ind)), 1:nfolds))
    stop("Invalid fold partition.")
  fold_errors = matrix(NA, nfolds, p)
  for (fold in 1:nfolds) {
    # Using all *but* the fold as training data, find the best-fitting models with 1, ..., p
    # predictors, i.e. M_1, ..., M_p
    tmp_fit = regsubsets(y ~ .,
                         data = Xy[fold_ind != fold,],
                         method = "exhaustive",
                         nvmax = p)
    best_models = summary(tmp_fit)$which[, 2:(1 + p)]
    # Using the fold as test data, find the test error associated with each of M_1,..., M_p
    for (k in 1:p) {
      fold_errors[fold, k] = reg_fold_error(X[, best_models[k,]], y, fold_ind ==
        fold)
    }
  }
  # Find the fold sizes
  fold_sizes = numeric(nfolds)
  for (fold in 1:nfolds)
    fold_sizes[fold] = length(which(fold_ind == fold))
  # For each of M_0, M_1, ..., M_p, compute the average test error across folds
  test_errors = numeric(p)
  for (k in 1:p) {
    test_errors[k] = weighted.mean(fold_errors[, k], w = fold_sizes)
  }
  # Return the test error for models M_1, ..., M_p
  return(test_errors)
}

# -----------------------------------------------------------------------------
# Multiple Linear Regression
library(car)
library(leaps)

perform_regression_and_selection = function(Dataset, name) {
  print('--------------------------------------------------------------------')
  lsq_fit = lm(Ladder_score ~ ., data = Dataset)
  # print(lsq_fit)
  # Check Multi-collinearity of variables with VIF:
  print('Multi-collinearity Check:')
  print(vif(lsq_fit))
  print('--------------------------------------------------------------------')
  lsq_summary = summary(lsq_fit)
  fitted_values = predict(lsq_fit, Dataset)
  # ---------------------------------------------------------------------------
  # Best Subset Selection
  p = ncol(Dataset) - 1
  bss_fit = regsubsets(Ladder_score ~ .,
                       data = Dataset,
                       method = "exhaustive",
                       nvmax = p)
  bss_summary = summary(bss_fit)
  print('Best Subset selection:')
  print(bss_summary)
  print('--------------------------------------------------------------------')
  # ---------------------------------------------------------------------------
  # Create multi-panel plotting device
  par(mfrow = c(2, 2))
  # Produce plots, highlighting optimal value of k
  best_adjr2 = which.max(bss_summary$adjr2)
  print("Adjust R square is:")
  print(bss_summary$adjr2)
  best_cp = which.min(bss_summary$cp)
  best_bic = which.min(bss_summary$bic)
  k = 5
  n = nrow(Dataset)
  set.seed(123)
  fold_index = sample(k, n, replace = TRUE)
  # Apply the function to the Happy data
  bss_mse = reg_bss_cv(Dataset[, 1:p], Dataset[, p + 1], fold_index)
  # Identify model with the lowest error
  best_cv = which.min(bss_mse)

  plot(
    1:p,
    bss_summary$adjr2,
    xlab = "Number of predictors",
    ylab = "Adjusted Rsq",
    type = "b"
  )
  points(best_adjr2,
         bss_summary$adjr2[best_adjr2],
         col = "red",
         pch = 16)
  plot(1:p,
       bss_summary$cp,
       xlab = "Number of predictors",
       ylab = "Cp",
       type = "b")
  points(best_cp,
         bss_summary$cp[best_cp],
         col = "red",
         pch = 16)
  plot(
    1:p,
    bss_summary$bic,
    xlab = "Number of predictors",
    ylab = "BIC",
    type = "b"
  )
  points(best_bic,
         bss_summary$bic[best_bic],
         col = "red",
         pch = 16)
  plot(1:p,
       bss_mse,
       xlab = "Number of predictors",
       ylab = "K-fold CV Error",
       type = "b")
  points(best_cv, bss_mse[best_cv], col = "red", pch = 16)
  title = paste("Variable selection for", name)
  mtext(
    title,
    side = 3,
    outer = TRUE,
    line = -3,
    cex = 1.5
  )
  par(mfrow = c(1, 1))
}

# -----------------------------------------------------------------------------
perform_regression_and_selection(Happy_general, "General")
perform_regression_and_selection(Happy_general_continent, "General with Continent")
perform_regression_and_selection(Africa, "Africa")
perform_regression_and_selection(Asia, "Asia")
perform_regression_and_selection(Europe, "Europe")
perform_regression_and_selection(North_America, "North America")
perform_regression_and_selection(Europe_Oceania, "Oceania")
perform_regression_and_selection(South_America, "South America")
# -----------------------------------------------------------------------------
# Does this change with outlier removal
clean_South_America <- RemoveOutliers(South_America)
clean_Europe <- RemoveOutliers(Europe)
clean_North_America <- RemoveOutliers(North_America)
clean_Africa <- RemoveOutliers(Africa)
clean_Asia <- RemoveOutliers(Asia)
clean_Oceania = RemoveOutliers(Europe_Oceania)

perform_regression_and_selection(clean_Happy_general, "clean General")
# perform_regression_and_selection(Happy_general_continent, "General with Continent")
perform_regression_and_selection(clean_Africa, "clean Africa")
perform_regression_and_selection(clean_Asia, "clean Asia")
perform_regression_and_selection(clean_Europe, "clean Europe")
perform_regression_and_selection(clean_North_America, "clean North America")
# perform_regression_and_selection(Oceania, "Oceania")
perform_regression_and_selection(clean_South_America, "clean South America")
perform_regression_and_selection(clean_Oceania, "clean Oceania")
# -----------------------------------------------------------------------------
fold_cv_error = function(Dataset) {
  ## Create matrix to store the fold assignments:
  k = 10
  n = nrow(Dataset)
  p = ncol(Dataset) - 1
  fold_indices = matrix(NA, 8, n)
  ## Sample the fold assignments:
  for (i in 1:8)
    fold_indices[i,] = sample(k, n, replace = TRUE)
  ## Create a matrix to store the test errors:
  bss_mses = matrix(NA, 8, p)
  ## Calculate the test errors for the p models for each fold assignment:
  for (i in 1:8)
    bss_mses[i,] = reg_bss_cv(Dataset[, 1:p], Dataset[, p + 1], fold_indices[i,])
  ## Identify the best model in each case:
  best_cvs = apply(bss_mses, 1, which.min)
  plot(1:p,
       bss_mses[1,],
       xlab = "Number of predictors",
       ylab = "10-fold CV Error",
       type = "l")
  points(best_cvs[1], bss_mses[1, best_cvs[1]], pch = 16)
  for (i in 2:8) {
    lines(1:p, bss_mses[i,], col = i)
    points(best_cvs[i], bss_mses[i, best_cvs[i]], pch = 16, col = i)
  }
}

# -----------------------------------------------------------------------------
# With bugs
# fold_cv_error(Happy_general)
# fold_cv_error(Happy_general_continent)
# fold_cv_error(Africa)
# fold_cv_error(Asia)
# fold_cv_error(Europe)
# fold_cv_error(Europe_Oceania)
# fold_cv_error(South_America)
# -----------------------------------------------------------------------------
mean_Oceania <- colMeans(Oceania)
mean_Asia <- colMeans(Asia)
mean_Africa <- colMeans(Africa)
mean_Europe <- colMeans(Europe)
mean_South_America <- colMeans(South_America)
mean_North_America <- colMeans(North_America)

comparison_result <- data.frame(
  # Continent = c("Asia", "Europe", "North America", "South America", "Africa"),
  Continent = rep(
    c("Asia", "Europe", "North America", "South America", "Africa"),
    each = length(mean_Oceania)
  ),
  Mean_Oceania = rep(mean_Oceania, times = 5),
  Mean_Continent = c(
    mean_Asia,
    mean_Europe,
    mean_North_America,
    mean_South_America,
    mean_Africa
  ),
  Comparison = ifelse(
    mean_Oceania > c(
      mean_Asia,
      mean_Europe,
      mean_North_America,
      mean_South_America,
      mean_Africa
    ),
    "Greater",
    ifelse(
      mean_Oceania < c(
        mean_Asia,
        mean_Europe,
        mean_North_America,
        mean_South_America,
        mean_Africa
      ),
      "Less",
      "Equal"
    )
  )
)

