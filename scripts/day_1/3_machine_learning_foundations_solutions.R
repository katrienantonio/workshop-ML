## Machine learning foundations
library(tidyverse)
library(gridExtra)
library(AmesHousing)
library(caret)
library(rsample)


KULbg <- "#116E8A"

## ----------------------------------------------------------------------------------------------
set.seed(123) 
index_1 <- sample(1 : nrow(ames), 
                  size = round(nrow(ames) * 0.7))   
train_1 <- ames[index_1, ]   
test_1  <- ames[-index_1, ]


## ----------------------------------------------------------------------------------------------
set.seed(123)  
index_2 <- caret::createDataPartition(
  y = ames$Sale_Price,  
  p = 0.7,          
  list = FALSE)     
train_2 <- ames[index_2, ]
test_2  <- ames[-index_2, ]


## ----------------------------------------------------------------------------------------------
set.seed(123) 
split_1  <- rsample::initial_split(ames, prop = 0.7)  
train_3  <- training(split_1) 
test_3   <- testing(split_1) 


## ----------------------------------------------------------------------------------------------
p_1 <- ggplot(train_1, aes(x = Sale_Price)) + theme_bw() +
  geom_density(trim = TRUE) +
  geom_density(data = test_1, trim = TRUE, col = "red") +
  ggtitle("base R")


## ----------------------------------------------------------------------------------------------
p_2 <- ggplot(train_2, aes(x = Sale_Price)) + theme_bw() +
  geom_density(trim = TRUE) +
  geom_density(data = test_2, trim = TRUE, col = "red") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("caret") 


## ----------------------------------------------------------------------------------------------
p_3 <- ggplot(train_3, aes(x = Sale_Price)) + theme_bw() +
  geom_density(trim = TRUE) + 
  geom_density(data = test_3, trim = TRUE, col = "red") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("rsample")


# side-by-side plots
gridExtra::grid.arrange(p_1, p_2, p_3, nrow = 1)


# clean up
rm(p_1, p_2, p_3)


## ----------------------------------------------------------------------------------------------
# Simulate some data 
n <- 100
set.seed(8451)
df <- tibble::tibble(
  x = runif(n, min = -2, max = 2),
  y = rnorm(n, mean = 1 + 2*x + x^2, sd = 1)
)

p <- ggplot(df, aes(x, y)) + 
  geom_point(alpha = 0.3) +  
  theme_bw()

p_1 <- p + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.5, color = KULbg) +
  ggtitle("Underfitting")

p_2 <- p + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, size = 1.5, color = KULbg) +
  ggtitle("Just right?")

p_3 <- p + 
  geom_smooth(method = "loess", span = 0.1, se = FALSE, size = 1.5, color = KULbg) +
  ggtitle("Overfitting")


gridExtra::grid.arrange(p_1, p_2, p_3, nrow = 1)


## ----bias-variance-knn-------------------------------------------------------------------------
# Simulate some nonlinear monotonic data
set.seed(123)  # for reproducibility
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>%
  filter(x < 4.5)


# Single biased model fit
bias_model <- lm(y ~ I(x^3), data = df)
df$predictions <- predict(bias_model, df)
p_1 <- ggplot(df, aes(x, y)) +
  geom_point(alpha = .3) +
  geom_line(aes(x, predictions), size = 1.5, color = KULbg) +
  scale_y_continuous("Response", limits = c(-1.75, 1.75), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0)) + theme_bw() +
  ggtitle("Biased model fit")

p_1


# Single high variance model fit
variance_model <- knnreg(y ~ x, k = 3, data = df)
df$predictions <- predict(variance_model, df)
p_2 <- ggplot(df, aes(x, y)) +
  geom_point(alpha = .3) +
  geom_line(aes(x, predictions), size = 1.5, color = KULbg) +
  scale_y_continuous("Response", limits = c(-1.75, 1.75), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0)) + theme_bw() +
  ggtitle("High variance model fit")

p_2


## ----------------------------------------------------------------------------------------------
set.seed(123)  
cv_folds <- caret::createFolds(ames$Sale_Price, 
                               k = 5, list = TRUE, 
                               returnTrain = TRUE)
str(cv_folds)


## ----------------------------------------------------------------------------------------------
mean(ames[cv_folds$Fold1, ]$Sale_Price)


## ----------------------------------------------------------------------------------------------
map_dbl(cv_folds,
        function(x) {
          mean(ames[x, ]$Sale_Price)
        })


## ----------------------------------------------------------------------------------------------
set.seed(123)  
cv_rsample <- rsample::vfold_cv(ames, v = 5)
cv_rsample$splits


## ----------------------------------------------------------------------------------------------
cv_rsample$splits[[1]]


## ----------------------------------------------------------------------------------------------
cv_rsample$splits[[1]] %>% analysis() %>% dim()


## ----------------------------------------------------------------------------------------------
cv_rsample$splits[[1]] %>% assessment() %>% dim()


## ----------------------------------------------------------------------------------------------
map_dbl(cv_rsample$splits,
        function(x) {
          mean(rsample::analysis(x)$Sale_Price)
        })


## -----------------------------------------------------------------------------------------------
map_dbl(cv_rsample$splits,
        function(x) {
          nrow(rsample::analysis(x))
        })


## ----Your Turn----------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
set.seed(123)  # for reproducibility
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>%
  filter(x < 4.5)

ggplot() + theme_bw() +
  geom_point(data = df, aes(x, y), alpha = .3) +
  scale_y_continuous(limits = c(-1.75, 1.75), 
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4.5), 
                     expand = c(0, 0))


## -----------------------------------------------------------------------------------------------
k <- 3
fit <- caret::knnreg(y ~ x, k = k, data = df)
df$pred <- predict(fit, df)
df %>% slice(1:6) 


## -----------------------------------------------------------------------------------------------
ggplot(data = df) + theme_bw() + 
  geom_point(aes(x, y), alpha = .3) +
  geom_line(aes(x, pred), color = KULbg, size = 1.0) +
  scale_y_continuous(limits = c(-1.75, 1.75), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0))


## -----------------------------------------------------------------------------------------------
k_results <- NULL
k <- c(2, 5, 10, 20, 50, 150)

# fit the different models
for(i in seq_along(k)) {
  df_sim <- df
  fit <- knnreg(y ~ x, k = k[i], data = df_sim)
  df_sim$pred <- predict(fit, df_sim)
  df_sim$model <- paste0("k = ", stringr::str_pad(k[i], 3, pad = " "))
  k_results <- rbind(k_results, df_sim)
}

ggplot() + theme_bw() +
  geom_point(data = df, aes(x, y), alpha = .3) +
  geom_line(data = k_results, aes(x, pred), color = KULbg, size = 1.0) +
  scale_y_continuous("Response", limits = c(-1.75, 1.75), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
  facet_wrap(~ model)

## ----Your Turn ends here------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------------
set.seed(123)
cv <- trainControl(method = "cv", number = 5, 
                   returnResamp = "all",  
                   selectionFunction = "best") 
hyper_grid <- expand.grid(k = seq(2, 150, by = 2))  
knn_fit <- train(y ~ x, data = df, method = "knn", 
                 trControl = cv, 
                 tuneGrid = hyper_grid) 
knn_fit$bestTune  

ggplot() + theme_bw() +
  geom_line(data = knn_fit$results, aes(k, RMSE)) +
  geom_point(data = knn_fit$results, aes(k, RMSE)) +
  geom_point(data = filter(knn_fit$results, k == as.numeric(knn_fit$bestTune)),
             aes(k, RMSE),
             shape = 21,
             fill = "yellow",
             color = "black",
             stroke = 1,
             size = 3) +
  scale_y_continuous("Error (RMSE)")


## -----------------------------------------------------------------------------------------------
set.seed(123)
cv <- trainControl(method = "cv", number = 5, 
                   returnResamp = "all",  
                   selectionFunction = "oneSE") 
hyper_grid <- expand.grid(k = seq(2, 150, by = 2))  
knn_fit <- train(y ~ x, data = df, method = "knn", 
                 trControl = cv, 
                 tuneGrid = hyper_grid) 
knn_fit$bestTune  

ggplot() + theme_bw() +
  geom_line(data = knn_fit$results, aes(k, RMSE)) +
  geom_point(data = knn_fit$results, aes(k, RMSE)) +
  geom_point(data = filter(knn_fit$results, k == as.numeric(knn_fit$bestTune)),
             aes(k, RMSE),
             shape = 21,
             fill = "yellow",
             color = "black",
             stroke = 1,
             size = 3) +
  scale_y_continuous("Error (RMSE)")


## ----------------------------------------------------------------------------------------------
set.seed(123)  # for reproducibility
cv_rsample <- vfold_cv(df, 5)
cv_rsample$splits 

## ----Your Turn---------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------
hyper_grid <- expand.grid(k = seq(2, 150, by = 2))  
hyper_grid %>% slice(1:3) 


## ----------------------------------------------------------------------------------------------
holdout_results <- function(s, k_val) {
  # Fit the model to the analysis data in split s
  df_train <- analysis(s)
  mod <- knnreg(y ~ x, k = k_val, data = df_train)
  # Get the remaining group
  holdout <- assessment(s)
  # Get predictions with the holdout data set
  res <- predict(mod, newdata = holdout)
  # Return observed and predicted values 
  #                            on holdout set
  res <- tibble(obs = holdout$y, pred = res)
  res
}
res <- holdout_results(cv_rsample$splits[[3]], hyper_grid[1, ])
sqrt(sum((res$obs - res$pred)^2)/nrow(res))


## -----------------------------------------------------------------------------------------------
RMSE <- numeric(nrow(hyper_grid))
SE <- numeric(nrow(hyper_grid))
for(i in 1:nrow(hyper_grid)){
  cv_rsample$results <- map(cv_rsample$splits,
                            holdout_results,
                            hyper_grid[i, ])
  res <- map_dbl(cv_rsample$results, 
                 function(x) mean((x$obs - x$pred)^2))
  RMSE[i] <- mean(sqrt(res)) ; SE[i] <- sd(sqrt(res))
}

df <- tibble(RMSE, SE, k = hyper_grid$k)
df <- df %>% mutate(lower = RMSE-SE, upper = RMSE + SE)
best <- df %>% filter(RMSE == min(RMSE)) 
best 


## ----------------------------------------------------------------------------------------------
one_SE <- df %>% filter(RMSE <= best$upper) %>% filter(k == max(k)) 
one_SE


## ----one-SE-rule-------------------------------------------------------------------------------
big_g <- ggplot(data = df) + theme_bw() +
  geom_line(aes(k, RMSE)) +
  geom_point(aes(k, RMSE)) +
  geom_pointrange(aes(k, RMSE, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_point(data = best,
             aes(k, RMSE),
             shape = 21,
             fill = "yellow",
             color = "black",
             stroke = 1,
             size = 2) +
  geom_point(data = one_SE,
             aes(k, RMSE),
             shape = 21,
             fill = "yellow",
             color = "black",
             stroke = 1,
             size = 3) +
  geom_hline(data = best,
             aes(yintercept = upper)) +
  scale_y_continuous("Error (RMSE)")
big_g

## ----Your Turn ends here------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------



