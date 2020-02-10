## Target and feature engineering
library(tidyverse)
library(gridExtra)
library(AmesHousing)
library(caret)
library(rsample)
library(recipes)


KULbg <- "#116E8A"


## ----------------------------------------------------------------------------------------------
set.seed(123)  
split  <- rsample::initial_split(ames, prop = 0.7, strata = "Sale_Price")
ames_train  <- rsample::training(split)
ames_test   <- rsample::testing(split)


## ----------------------------------------------------------------------------------------------
summary(ames_train$Sale_Price)
summary(ames_test$Sale_Price)

## ----Your Turn----------------------------------------------------------------------------------
## ----two-linear-models-------------------------------------------------------------------------
m_1 <- lm(Sale_Price ~ Year_Built, data = ames_train)
m_2 <- lm(log(Sale_Price) ~ Year_Built, data = ames_train)


## ----------------------------------------------------------------------------------------------
res_1 <- m_1 %>% broom::augment()
res_2 <- m_2 %>% broom::augment()


## ----------------------------------------------------------------------------------------------
res_1 %>% slice(1:2) %>% select(Sale_Price, Year_Built, .resid) 


## ----------------------------------------------------------------------------------------------
m_1 <- lm(Sale_Price ~ Year_Built, data = ames_train)
m_2 <- lm(log(Sale_Price) ~ Year_Built, data = ames_train)

res_1 <- m_1 %>% broom::augment()
res_2 <- m_2 %>% broom::augment()

g_res_1 <- ggplot(data = res_1, aes(.resid)) + theme_bw() +
  geom_histogram(bins = 75, col = KULbg, fill = KULbg, alpha = .5) +
  ylab(NULL) + ggtitle("AMES - original target") +
  xlab("Residuals")

g_res_2 <- ggplot(data = res_2, aes(.resid)) + theme_bw() +
  geom_histogram(bins = 75, col = KULbg, fill = KULbg, alpha = 0.5) +
  ylab(NULL) + ggtitle("AMES - log transformed target") +
  xlab("Residuals")

gridExtra::grid.arrange(g_res_1, g_res_2, nrow = 1)


## ----------------------------------------------------------------------------------------------
models <- c("Non-log transformed model residuals", 
            "Log transformed model residuals")

l <- list(
  m1 = lm(Sale_Price ~ Year_Built, data = ames_train),
  m2 = lm(log(Sale_Price) ~ Year_Built, data = ames_train)
)

# try map_df
f_1 <- map_df(l, function(x){broom::augment(x)})
# or even better map2_df
f_2 <- map2_df(l, models, function(x,y){ broom::augment(x) %>% mutate(model = y)})

g <- ggplot(data = f_2, aes(.resid)) + theme_bw() +
  geom_histogram(bins = 75, col = KULbg, fill = KULbg, alpha = .5) +
  facet_wrap(~ model, scales = "free_x") +
  ylab(NULL) +
  xlab("Residuals")
g


## ----------------------------------------------------------------------------------------------
ames_train %>% group_by(Neighborhood) %>% summarize(n_obs = n()) %>% arrange(n_obs) %>% slice(1:4) 


## ----------------------------------------------------------------------------------------------
df <- ames_train %>% group_by(Neighborhood) %>% summarize(n_obs = n()) %>% arrange(n_obs)

ggplot(ames_train, aes(x = fct_infreq(Neighborhood))) + theme_bw() +
  geom_bar(col = KULbg, fill = KULbg, alpha = .5) + 
  coord_flip() + 
  xlab("") 


## ----------------------------------------------------------------------------------------------
mod_rec <- recipe(Sale_Price ~ ., data = ames_train)
mod_rec


## ----------------------------------------------------------------------------------------------
mod_rec <- mod_rec %>% step_log(all_outcomes()) %>%
  step_other(Neighborhood, threshold = 0.05)
mod_rec


## ----trained_recipe----------------------------------------------------------------------------
mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE, retain = TRUE)
mod_rec_trained


## ----------------------------------------------------------------------------------------------
ames_test_prep <- bake(mod_rec_trained, new_data = ames_test)


## ----------------------------------------------------------------------------------------------
ames_test_prep %>% group_by(Neighborhood) %>% 
  summarize(n_obs = n()) %>% 
  arrange(n_obs) 


## ----------------------------------------------------------------------------------------------
juice(mod_rec_trained) %>% group_by(Neighborhood) %>% 
  summarize(n_obs = n()) %>% 
  arrange(n_obs) 


## ----------------------------------------------------------------------------------------------
ames_train %>% group_by(House_Style) %>% summarize(n_obs = n()) %>% arrange(n_obs) 


## ----------------------------------------------------------------------------------------------
ggplot(ames_train, aes(x = fct_infreq(House_Style))) + theme_bw() +
  geom_bar(col = KULbg, fill = KULbg, alpha = .5) + 
  coord_flip() + 
  xlab("") 


## ----------------------------------------------------------------------------------------------
nzv <- caret::nearZeroVar(ames_train, saveMetrics = TRUE)


## ----------------------------------------------------------------------------------------------
names(ames_train)[nzv$zeroVar]


## ----------------------------------------------------------------------------------------------
names(ames_train)[nzv$nzv]


## ----------------------------------------------------------------------------------------------
mod_rec <- recipe(Sale_Price ~ ., data = ames_train) %>%
  step_log(all_outcomes()) %>%
  step_other(Neighborhood, threshold = 0.05) %>%
  step_other(House_Style, threshold = 0.05) %>%
  step_zv(all_predictors()) %>% 
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())
summary(mod_rec) %>% slice(1:6) 
mod_rec


## ----------------------------------------------------------------------------------------------
mod_rec_trained <- prep(mod_rec, 
                        training = ames_train, 
                        verbose = TRUE, retain = TRUE)


## ----------------------------------------------------------------------------------------------
ames_test_prep <- bake(mod_rec_trained, 
                       new_data = ames_test)


## ----------------------------------------------------------------------------------------------
dim(juice(mod_rec_trained)) 


## ----------------------------------------------------------------------------------------------
options(digits = 4)
head(juice(mod_rec_trained)$Sale_Price) 


## ----------------------------------------------------------------------------------------------
options(digits = 4)
head(ames_train$Sale_Price) 


## ----------------------------------------------------------------------------------------------
options(digits = 4)
head(ames_test_prep$Sale_Price) 


## ----------------------------------------------------------------------------------------------
options(digits = 4)
head(ames_test$Sale_Price) 


## ----------------------------------------------------------------------------------------------
levels(juice(mod_rec_trained)$House_Style)[1:2]
levels(juice(mod_rec_trained)$House_Style)[3:4]


## ----------------------------------------------------------------------------------------------
levels(ames_test_prep$House_Style)[1:2]
levels(ames_test_prep$House_Style)[3:4]


## ----------------------------------------------------------------------------------------------
# get the simulated data
set.seed(123)  # for reproducibility
x <- seq(from = 0, to = 2 * pi, length = 500)
y <- sin(x) + rnorm(length(x), sd = 0.3)
df <- data.frame(x, y) %>% filter(x < 4.5)


## ----------------------------------------------------------------------------------------------
# specify the recipe
library(recipes)
rec <- recipe(y ~ x, data = df)
rec <- rec %>% step_center(all_predictors()) %>%
  step_scale(all_predictors())


## ----------------------------------------------------------------------------------------------
# doing this on complete data set df
rec_df <- prep(rec, training = df)
mean(juice(rec_df)$x) # centered!
sd(juice(rec_df)$x)   # scaled!


## ----------------------------------------------------------------------------------------------
# now we combine the recipe with rsample steps
library(rsample)
set.seed(123)  # for reproducibility
cv_rsample <- vfold_cv(df, 5)


## ----------------------------------------------------------------------------------------------
# we apply the steps in the recipe to each fold
library(purrr)
cv_rsample$recipes <- map(cv_rsample$splits, prepper, 
                          recipe = rec)
# check `?prepper`


## ----------------------------------------------------------------------------------------------
cv_rsample$recipes[[1]]
juice(cv_rsample$recipes[[1]])
bake(cv_rsample$recipes[[1]],
      new_data = assessment(cv_rsample$splits[[1]]))


## ----------------------------------------------------------------------------------------------
holdout_results <- function(s, rec, k_val) {
  # Fit the model to the analysis data in split s
  df_train <- juice(rec)
  mod <- knnreg(y ~ x, k = k_val, data = df_train)
  # Get the remaining group
  holdout <- bake(rec, new_data = assessment(s))
  # Get predictions with the holdout data set
  res <- predict(mod, newdata = holdout)
  # Return observed and predicted values 
  #                            on holdout set
  res <- tibble(obs = holdout$y, pred = res)
  res
}


## ----------------------------------------------------------------------------------------------
res <- holdout_results(cv_rsample$splits[[2]], 
                       cv_rsample$recipes[[2]], 
                       k_val = 58)
sqrt(sum((res$obs - res$pred)^2)/nrow(res))


## ----------------------------------------------------------------------------------------------
RMSE <- numeric(nrow(hyper_grid))
SE <- numeric(nrow(hyper_grid))
for(i in 1:nrow(hyper_grid)){
  cv_rsample$results <- map2(cv_rsample$splits, cv_rsample$recipes,
                             holdout_results,
                             hyper_grid[i, ])
  res <- map_dbl(cv_rsample$results, 
                 function(x) mean((x$obs - x$pred)^2))
  RMSE[i] <- mean(sqrt(res)) ; SE[i] <- sd(sqrt(res))
}