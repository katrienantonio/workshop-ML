## ----setup-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('0_setup.R')


## ----gbm-param-1-----------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 10,
           interaction.depth = 1,
           shrinkage = 1
) 

preds <- do.call(rbind, lapply(0:fit$n.trees,
                               function(i) dfr %>% mutate(iter = i,
                                                          pred = predict(fit, n.trees = i, type = 'response'))))

preds %>% ggplot(aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
  geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + 
  transition_states(iter, transition_length = 0.1, state_length = 0.5) + labs(title = "Iteration: {closest_state}")


## ----gbm-param-2-----------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 10,
           interaction.depth = 1,
           shrinkage = 0.1
) 

preds <- do.call(rbind, lapply(0:fit$n.trees,
                               function(i) dfr %>% mutate(iter = i,
                                                          pred = predict(fit, n.trees = i, type = 'response'))))

preds %>% ggplot(aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
  geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + 
  transition_states(iter, transition_length = 0.1, state_length = 0.5) + labs(title = "Iteration: {closest_state}")


## ----gbm-param-3-----------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 10,
           interaction.depth = 3,
           shrinkage = 0.1
) 

preds <- do.call(rbind, lapply(0:fit$n.trees,
                               function(i) dfr %>% mutate(iter = i,
                                                          pred = predict(fit, n.trees = i, type = 'response'))))

preds %>% ggplot(aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
  geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + 
  transition_states(iter, transition_length = 0.1, state_length = 0.5) + labs(title = "Iteration: {closest_state}")


## ----gbm-param-4-----------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 10,
           interaction.depth = 3,
           shrinkage = 1
) 

preds <- do.call(rbind, lapply(0:fit$n.trees,
                               function(i) dfr %>% mutate(iter = i,
                                                          pred = predict(fit, n.trees = i, type = 'response'))))

preds %>% ggplot(aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = m), colour = 'darkgreen', size = 1.5) +
  geom_line(aes(y = pred), colour = 'darkred', size = 1.5) + 
  transition_states(iter, transition_length = 0.1, state_length = 0.5) + labs(title = "Iteration: {closest_state}")


## ----gbm-large-1-----------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- gbm(formula = y ~ x,
           data = dfr,
           distribution = 'gaussian',
           n.trees = 300,
           interaction.depth = 3,
           shrinkage = 0.01
) 


## ----gbm-large-2-----------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = predict(fit, n.trees = fit$n.trees, type = 'response'))


## ----------------------------------------------------------------------------------------------
## ----Your Turn---------------------------------------------------------------------------------


## ----Your Turn ends here-----------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------



## ----gbm-mtpl-1------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(76539) # reproducibility
fit <- gbm(formula = nclaims ~ 
             ageph + agec + bm + power + 
             coverage + fuel + sex + fleet + use + 
             offset(log(expo)),
           data = mtpl_trn,
           distribution = 'poisson',
           var.monotone = c(0,0,1,0,0,0,0,0,0),
           n.trees = 200,
           interaction.depth = 3,
           n.minobsinnode = 1000,
           shrinkage = 0.1,
           bag.fraction = 0.75,
           cv.folds = 0
)
# Track the improvement in the OOB error
oob_evo <- fit$oobag.improve


## ----gbm-mtpl-2------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data.frame(x = 1:200, y = oob_evo), aes(x = x, y = y)) + geom_point() + labs(x = 'Iteration', y = 'OOB error improvement')


## ----gbm-mtpl-3------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit %>% 
  pretty.gbm.tree(i.tree = 1) %>%
  print(digits = 5)


## ----------------------------------------------------------------------------------------------
## ----Your Turn---------------------------------------------------------------------------------


## ----Your Turn ends here-----------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------


## ---- xgb-data-1------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Features go into the data argument (needs to be converted to a matrix)
# The response and offset are specified via 'label' and 'base_margin' in info respectively
mtpl_xgb <- xgb.DMatrix(data = mtpl_trn %>% 
                          select(ageph,power,bm,agec,coverage,fuel,sex,fleet,use) %>%
                          data.matrix,
                        info = list(
                          'label' = mtpl_trn$nclaims,
                          'base_margin' = log(mtpl_trn$expo)))


## ---- xgb-data-2------------------------------------------------------------------------------------------------------------------------------------------------------------------
print(mtpl_xgb)


## ---- xgboost-1-------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(86493) # reproducibility
fit <- xgboost(
  data = mtpl_xgb,
  nrounds = 200,
  early_stopping_rounds = 20,
  verbose = FALSE,
  params = list(
    booster = 'gbtree',
    objective  = 'count:poisson',
    eval_metric = 'poisson-nloglik',
    eta = 0.1, nthread = 1,
    subsample = 0.75, colsample_bynode = 0.5,
    max_depth = 3, min_child_weight = 1000,
    gamma = 0, lambda = 1, alpha = 1
  )
)


## ---- xgboost-2-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Save xgboost model to a file in binary format
xgb.save(fit, fname = 'xgb.model')
# Load xgboost model from the binary model file
fit <-  xgb.load('xgb.model')


## ---- xgboost-3-------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_xgb <- mtpl_tst %>%
  select(ageph,power,bm,agec,
         coverage,fuel,sex,fleet,use) %>%
  data.matrix %>% 
  xgb.DMatrix

test_xgb %>% setinfo('base_margin',
                     rep(log(1),
                         nrow(mtpl_tst))
)


## ---- xgboost-4-------------------------------------------------------------------------------------------------------------------------------------------------------------------
preds <- fit %>% predict(
  newdata = test_xgb,
  ntreelimit = fit$niter
) 

preds %>% mean


## ---- xgb-add-1-------------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb.plot.tree(feature_names = colnames(mtpl_xgb),
              model = fit,
              trees = 0)


## ---- xgb-add-2-------------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb.plot.multi.trees(model = fit,
                     feature_names = colnames(mtpl_xgb))


## ---- xgb-add-3-------------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb.ggplot.importance(
  importance_matrix = xgb.importance(
    feature_names = colnames(mtpl_xgb),
    model = fit
  )
)


## ---- xgb-cv-1--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# WATCH OUT: this block can take some time to run
set.seed(86493) # reproducibility
xval <- xgb.cv(data = mtpl_xgb,
               nrounds = 200,
               early_stopping_rounds = 20,
               verbose = FALSE,
               nfold = 5,
               stratified = TRUE,
               params = list(booster = 'gbtree',
                             objective  = 'count:poisson',
                             eval_metric = 'poisson-nloglik',
                             eta = 0.1, nthread = 1,
                             subsample = 0.75, colsample_bynode = 0.5,
                             max_depth = 3, min_child_weight = 1000,
                             gamma = 0, lambda = 1, alpha = 1))




## ---- xgb-cv-2--------------------------------------------------------------------------------------------------------------------------------------------------------------------
xval$evaluation_log %>% print(digits = 5)


## ---- xgb-cv-3--------------------------------------------------------------------------------------------------------------------------------------------------------------------
xval_log <- xval$evaluation_log
xval_log <- as.data.frame(rbind(as.matrix(xval_log[,c(1,2,3)]),as.matrix(xval_log[,c(1,4,5)])))
names(xval_log) <- c('iteration','poisson_nloglik','std')
xval_log$loss <- c(rep('train',nrow(xval_log)/2),rep('test',nrow(xval_log)/2))


## ---- xgb-cv-4--------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(xval_log, aes(x=iteration, y=poisson_nloglik, colour=loss, linetype = loss)) + geom_line(size = 1.3)

ggplot(xval_log[c(150:200,350:400),], aes(x=iteration, y=poisson_nloglik, colour=loss, linetype = loss)) + geom_line(size = 1.5)
# geom_errorbar(aes(ymin=poisson_nloglik-std, ymax=poisson_nloglik+std), width=.1)
