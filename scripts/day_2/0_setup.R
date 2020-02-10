# Load all the packages for today
pkgs <- c('tidyverse','magrittr','rpart','rpart.plot','vip','pdp','ipred','ranger','gbm','xgboost','h2o','gganimate','transformr','zeallot')
lapply(pkgs, require, character.only = TRUE)

# Load the distRforest package if you managed to install it from GitHub
# source: https://github.com/henckr/distRforest
require(distRforest)

# Set a global black&white theme for ggplot
ggplot2::theme_set(theme_bw())

# Define a relative path to the data folder
data_path <- 'data/'


# Artificial data for the regression toy problem
set.seed(54321) # reproducibility
dfr <- tibble(
  x = seq(0, 2*pi, length.out = 500),
  m = 2*sin(x),
  y = m + rnorm(length(x), sd = 1)
)


# Artificial data for the classification toy problem
set.seed(54321) # reproducibility
dfc <- tibble(
  x1 = rep(seq(0.1,10,by = 0.1), times = 100),
  x2 = rep(seq(0.1,10,by = 0.1), each = 100),
  y = as.factor(
    pmin(1,
         pmax(0,
              round(
                1*(x1+2*x2<8) + 1*(3*x1+x2>30) + 
                  rnorm(10000,sd = 0.5))
         )
    )
  )
)


# MTPL data
mtpl <- readRDS(paste0(data_path,'MTPL.rds'))
set.seed(54321)
train_id <- caret::createDataPartition(y = mtpl$nclaims/mtpl$expo, p = 0.8, groups = 100)[[1]]
mtpl_trn <- mtpl[train_id,]
mtpl_tst <- mtpl[-train_id,]


# PDP ids
set.seed(48927)
pdp_ids <- mtpl_trn %>%  nrow %>% sample(size = 5000)



# Below are some helper functions that might be useful:


#' Plot the predictions for the regression toy problem
#' @param dt The underlying data (data.frame or tibble)
#' @param preds The predictions (numeric vector)
#' @return A ggplot showing the prediction function
plot_pred_reg <- function(dt, preds){
  dt %>% mutate(pred = preds) %>% ggplot(aes(x = x)) +
    geom_point(aes(y = y), alpha = 0.3) +
    geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
    geom_line(aes(y = pred), colour = 'darkred', size = 1.5)
}


#' Plot the predictions for the classification toy problem
#' @param dt The underlying data (data.frame or tibble)
#' @param preds The predictions (numeric vector)
#' @return A ggplot showing the prediction function
plot_pred_class <- function(dt, preds){
  dt %>% mutate(pred = preds) %>% ggplot(aes(x = x1, y = x2)) +
    geom_point(aes(color = pred))
}


#' Calculate the Poisson deviance
#' @param y The true values (numeric vector)
#' @param yhat The estimates for y (numeric vector or matrix with one col per model)
#' @param w Optional case weights (numeric vector)
#' @param scaled Deviance scaled by number of observations or not (boolean)
#' @return A single number or numeric vector if ncol(yhat)>1
dev_poiss <- function(y, yhat, w = 1, scaled = TRUE){
  sf <- ifelse(scaled, 1/length(y[!is.na(y)]), 1)
  if(!is.matrix(yhat)) return(-2*sf*sum(w*(dpois(y,yhat,log=TRUE) - dpois(y,y,log=TRUE)), na.rm = TRUE))
  return(-2*sf*colSums(w*(dpois(y,yhat,log=TRUE) - dpois(y,y,log=TRUE)), na.rm = TRUE))
}