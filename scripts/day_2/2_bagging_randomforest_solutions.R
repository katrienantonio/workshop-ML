## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('0_setup.R')


## ----boot-sample-1----------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(45678) # reproducibility
# Generate the first bootstrapped sample
bsample_1 <- dfr %>% nrow %>% 
  sample(replace = TRUE)
# Generate another bootstrapped sample
bsample_2 <- dfr %>% nrow %>% 
  sample(replace = TRUE)

## ----boot-sample-2----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use the indices to sample the data
dfr_b1 <- dfr %>%
  dplyr::slice(bsample_1)
dfr_b2 <- dfr %>% 
  dplyr::slice(bsample_2)


## ----boot-sample-3----------------------------------------------------------------------------------------------------------------------------------------------------------------
dfr_b1 %>% dplyr::arrange(x) %>% print(n = 5)
dfr_b2 %>% dplyr::arrange(x) %>% print(n = 5)


## ----boot-tree-1------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit_b1 <- rpart(formula = y ~ x,
                data = dfr_b1,
                method = 'anova',
                control = rpart.control(
                  maxdepth = 20,
                  minsplit = 10,
                  minbucket = 5,
                  cp = 0
                )
)
plot_pred_reg(dt = dfr, preds = predict(fit_b1, dfr))


## ----boot-tree-2------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit_b2 <- rpart(formula = y ~ x,
                data = dfr_b2,
                method = 'anova',
                control = rpart.control(
                  maxdepth = 20,
                  minsplit = 10,
                  minbucket = 5,
                  cp = 0
                )
)
plot_pred_reg(dt = dfr, preds = predict(fit_b2, dfr))


## ----boot-avg-1-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Predictions for the first tree
pred_b1 <- fit_b1 %>% predict(dfr)
# Predictions for the first tree
pred_b2 <- fit_b2 %>% predict(dfr)


## ----boot-avg-2-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Average the predictions
pred <- rowMeans(cbind(pred_b1,
                       pred_b2))


## ----boot-avg-3-------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = pred)


## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
# Generate the third bootstrapped sample
set.seed(28726)
bsample_3 <- dfr %>% nrow %>% 
  sample(replace = TRUE)
# Use the indices to sample the data
dfr_b3 <- dfr %>% dplyr::slice(bsample_3)
# Fit an unpruned tree
fit_b3 <- rpart(formula = y ~ x,
                data = dfr_b3,
                method = 'anova',
                control = rpart.control(
                  maxdepth = 20,
                  minsplit = 10,
                  minbucket = 5,
                  cp = 0))
# Predictions for the third tree
pred_b3 <- fit_b3 %>% predict(dfr)
# Average the predictions
pred_new <- rowMeans(cbind(pred_b1,
                           pred_b2,
                           pred_b3))
plot_pred_reg(dt = dfr, preds = pred)
plot_pred_reg(dt = dfr, preds = pred_new)



## ----oob-1------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(12345)
N <- 100000 ; x <- 1:N
mean(x %in% sample(N,
                   replace = TRUE))


## ----oob-2------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mean(x %in% sample(N,
                   size = 0.75*N,
                   replace = TRUE))


## ----bag-reg-1--------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(83946) # reproducibility
# Fit a bagged tree model
fit <- ipred::bagging(formula = y ~ x,
                      data = dfr,
                      nbagg = 200,
                      ns = nrow(dfr),
                      coob = TRUE,
                      control = rpart.control(
                        maxdepth = 20,
                        minsplit = 40,
                        minbucket = 20,
                        cp = 0
                        )
)

## ----bag-reg-2--------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(fit, dfr)


## ----bag-reg-3--------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = predict(fit, dfr))


## ----oob-evo-1--------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(98765) # reproducibility
# Define a grid for B
nbags <- 10*(1:20)
oob <- rep(0, length(nbags))
# Fit a bagged tree model
for(i in 1:length(nbags)){
  fit <- ipred::bagging(formula = y ~ x,
                        data = dfr,
                        nbagg = nbags[i],
                        ns = nrow(dfr),
                        coob = TRUE,
                        control = rpart.control(
                          maxdepth = 20,
                          minsplit = 40,
                          minbucket = 20,
                          cp = 0)
  )
  oob[i] <- fit$err
}


## ----oob-evo-2--------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data.frame('B' = nbags, 'RMSE' = oob), aes(x = B, y = RMSE)) + geom_line()


## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(98765) # reproducibility
# Fit a bagged tree model
fit <- ipred::bagging(formula = y ~ x1 + x2,
                      data = dfc,
                      nbagg = 100,
                      ns = nrow(dfc),
                      control = rpart.control(
                        maxdepth = 20,
                        minsplit = 10,
                        minbucket = 5,
                        cp = 0)
)
plot_pred_class(dt = dfc, preds = predict(fit, dfc, type = 'class', aggregation = 'majority'))


## ----bag-mtpl-1-------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(486291) # reproducibility
# Generate the first bootstrapped sample
bsample_1 <- mtpl_trn %>% nrow %>% 
  sample(replace = TRUE)
# Generate another bootstrapped sample
bsample_2 <- mtpl_trn %>% nrow %>% 
  sample(replace = TRUE)


## ----bag-mtpl-2-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use the indices to sample the data
mtpl_b1 <- mtpl_trn %>% dplyr::slice(bsample_1)
mtpl_b2 <- mtpl_trn %>% dplyr::slice(bsample_2)


## ----bag-mtpl-3-------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit_b1 <- rpart(formula = 
                  cbind(expo,nclaims) ~
                  ageph + agec + bm + power + 
                  coverage + fuel + sex + fleet + use,
                data = mtpl_b1,
                method = 'poisson',
                control = rpart.control(
                  maxdepth = 3,
                  minsplit = 2000,
                  minbucket = 1000,
                  cp = 0
                )
)
fit_b2 <- rpart(formula = 
                  cbind(expo,nclaims) ~
                  ageph + agec + bm + power + 
                  coverage + fuel + sex + fleet + use,
                data = mtpl_b2,
                method = 'poisson',
                control = rpart.control(
                  maxdepth = 3,
                  minsplit = 2000,
                  minbucket = 1000,
                  cp = 0
                )
)


## ----bag-mtpl-4-------------------------------------------------------------------------------------------------------------------------------------------------------------------
rpart.plot(fit_b1, cex = 1.4, extra = 0)
rpart.plot(fit_b2, cex = 1.4, extra = 0)


## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
c(p,m,n) %<-% c(10,4,100)
(m/p)*n
set.seed(54321)
samples <- sapply(1:n,
                  function(i) sample(p,
                                     size = m)
)
samples[,1:9]
sapply(1:p,
       function(i) sum(samples == i)
)


## ----ranger-1--------------------------------------------------------------------------------------------------------------------------------------------------------------------
search_grid <- expand.grid(
  num.trees = c(100,200),
  mtry = c(3,6,9),
  min.node.size = c(0.001,0.01)*nrow(mtpl),
  error = NA
)


## ----ranger-2--------------------------------------------------------------------------------------------------------------------------------------------------------------------
print(search_grid)


## ----ranger-3--------------------------------------------------------------------------------------------------------------------------------------------------------------------
## WATCH OUT: this block can take some time
for(i in seq_len(nrow(search_grid))) {
  # fit a random forest for the ith combination
  fit <- ranger(
    formula = nclaims ~
      ageph + agec + bm + power + 
      coverage + fuel + sex + fleet + use, 
    data = mtpl_trn, 
    num.trees = search_grid$num.trees[i],
    mtry = search_grid$mtry[i],
    min.node.size = search_grid$min.node.size[i],
    replace = TRUE,
    sample.fraction = 0.75,
    verbose = FALSE,
    seed = 54321
  )
  # get the OOB error 
  search_grid$error[i] <- fit$prediction.error
}


## ----ranger-4--------------------------------------------------------------------------------------------------------------------------------------------------------------------
search_grid %>% arrange(error)


## ----distRforest-1---------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(54321) # reproducibility
fit_rf <- rforest(
  formula = cbind(expo,nclaims) ~
    ageph + agec + bm + power + coverage +
    fuel + sex + fleet + use,
  data = mtpl_trn,
  method = 'poisson',
  control = rpart.control(
    maxdepth = 20,
    minsplit = 2000,
    minbucket = 1000,
    cp = 0,
    xval = 0
  ),
  ntrees = 100,
  ncand = 5,
  subsample = 0.75,
  redmem = TRUE
)


## ----distRforest-2---------------------------------------------------------------------------------------------------------------------------------------------------------------
class(fit_rf)
class(fit_rf[[1]])


## ----rf-vip-1--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get vi of each individual tree
var_imps <- lapply(fit_rf, vip::vi)


## ----rf-vip-2--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Some data-wrangling to get rf vi
do.call(rbind, var_imps) %>% 
  group_by(Variable) %>% 
  summarise(Importance = mean(Importance)) %>%
  arrange(-Importance)


## ----rf-vip-3--------------------------------------------------------------------------------------------------------------------------------------------------------------------
do.call(rbind, var_imps) %>% 
  group_by(Variable) %>% 
  summarise(Importance = mean(Importance)) %>% 
  arrange(-Importance) %>% 
  ggplot(aes(x = reorder(Variable,Importance), y = Importance/max(Importance))) + geom_bar(stat = 'identity') + coord_flip() + labs(x = 'Variable')


## ----rf-pdp-1--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Need to define this helper function
pred.fun <- function(object,newdata){
  pred <- rep(0,nrow(newdata))
  for(i in 1:length(object)) {
    pred <- pred + predict(object[[i]], newdata)
  }
  return(mean((1/length(object))*pred))
} 


## ----rf-pdp-2--------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit_rf %>% 
  partial(pred.var = 'ageph',
          pred.fun = pred.fun,
          train = mtpl_trn[pdp_ids,]) %>% 
  autoplot()


## ----rf-pdp-3--------------------------------------------------------------------------------------------------------------------------------------------------------------------
## WATCH OUT: this block can take some time
fit_rf %>% 
  partial(pred.var = c('ageph','power'),
          pred.fun = pred.fun,
          train = mtpl_trn[pdp_ids,]) %>% 
  autoplot()