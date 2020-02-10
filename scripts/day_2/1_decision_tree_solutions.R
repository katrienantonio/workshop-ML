## ----setup-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('0_setup.R')


## ----reg-toy-data-1--------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(54321) # reproducibility
dfr <- tibble(
  x = seq(0, 2*pi, length.out = 500),
  m = 2*sin(x),
  y = m + rnorm(length(x), sd = 1)
)


## ----reg-toy-data-2--------------------------------------------------------------------------------------------------------------------------------------------------------------
print(dfr)


## ----reg-toy-data-3--------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(dfr, aes(x = x)) + geom_point(aes(y = y), alpha = 0.3) + geom_line(aes(y = m), colour = 'darkgreen', size = 1.5)


## ----reg-toy-stump-1-------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova',
             control = rpart.control(
               maxdepth = 1
             )
)
print(fit)


## ----reg-toy-stump-2-------------------------------------------------------------------------------------------------------------------------------------------------------------
rpart.plot(fit, digits = 4, cex = 2)


## ----reg-toy-stump-3-------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = predict(fit, dfr))


## ----reg-toy-tree-1--------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova',
             control = rpart.control(
               maxdepth = 2
             )
)
print(fit)


## ----reg-toy-tree-2--------------------------------------------------------------------------------------------------------------------------------------------------------------
rpart.plot(fit, digits = 4, cex = 1.5)


## ----reg-toy-tree-3--------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = predict(fit, dfr))



## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
# Subset observations in node 6
obs <- dfr %>% dplyr::filter(x < 0.535141)
# Prediction
pred <- obs$y %>%  mean
pred
# Deviance
dev <- (obs$y - pred)^2 %>% sum
dev


## ----reg-toy-deep-1--------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova',
             control = rpart.control(
               maxdepth = 20,
               minsplit = 10,
               minbucket = 5,
               cp = 0
             )
)


## ----reg-toy-deep-2--------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, predict(fit, dfr))


## ----reg-toy-prune-1-------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(87654) # reproducibility
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova',
             control = rpart.control(
               maxdepth = 10,
               minsplit = 20,
               minbucket = 10,
               cp = 0,
               xval = 5
             )
)


## ----reg-toy-prune-2-------------------------------------------------------------------------------------------------------------------------------------------------------------
plotcp(fit)


## ----reg-toy-prune-3-------------------------------------------------------------------------------------------------------------------------------------------------------------
cpt <- fit$cptable
print(cpt[1:20,], digits = 6)


## ----reg-toy-prune-4-------------------------------------------------------------------------------------------------------------------------------------------------------------
min_xerr <- which.min(cpt[,'xerror'])
fit_1 <- prune(fit, cp = cpt[min_xerr, 'CP'])
plot_pred_reg(dt = dfr, preds = predict(fit_1, dfr))


## ----reg-toy-prune-5-------------------------------------------------------------------------------------------------------------------------------------------------------------
se_rule <- min(which(cpt[, 'xerror'] < (cpt[min_xerr, 'xerror'] + cpt[min_xerr, 'xstd'])))
fit_2 <- prune(fit, cp = cpt[se_rule, 'CP'])
plot_pred_reg(dt = dfr, preds = predict(fit_2, dfr))


## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
# Generate the data
set.seed(83625493)
dfr2 <- tibble(
  x = seq(0, 2*pi, length.out = 500),
  m = 2*sin(x),
  y = m + rnorm(length(x), sd = 1)
)
# Fit a deep tree
set.seed(87654)
fit <- rpart(formula = y ~ x,
             data = dfr2,
             method = 'anova',
             control = rpart.control(
               maxdepth = 10,
               minsplit = 20,
               minbucket = 10,
               cp = 0,
               xval = 5
             )
)
# Get xval results via 'cptable' attribute
cpt <- fit$cptable
# Minimal cross-validation error
min_xerr <- which.min(cpt[,'xerror'])
fit_3 <- prune(fit, cp = cpt[min_xerr, 'CP'])
plot_pred_reg(dt = dfr2, preds = predict(fit_3, dfr2))
# One standard error rule
se_rule <- min(which(cpt[, 'xerror'] < (cpt[min_xerr, 'xerror'] + cpt[min_xerr, 'xstd'])))
fit_4 <- prune(fit, cp = cpt[se_rule, 'CP'])
plot_pred_reg(dt = dfr2, preds = predict(fit_4, dfr2))


## ----class-toy-data-1------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ----class-toy-data-2------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(dfc, aes(x = x1, y = x2)) + geom_point(aes(color = y))


## ----class-toy-tree-1------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x1 + x2,
             data = dfc,
             method = 'class',
             control = rpart.control(
               maxdepth = 2
             )
)
print(fit)


## ----class-toy-tree-2------------------------------------------------------------------------------------------------------------------------------------------------------------
rpart.plot(fit, digits = 4, cex = 1.5)


## ----class-toy-tree-3------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_class(dt = dfc, preds = predict(fit, dfc, type = 'class'))


## ----class-toy-deep-1------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x1 + x2,
             data = dfc,
             method = 'class',
             control = rpart.control(
               maxdepth = 20,
               minsplit = 10,
               minbucket = 5,
               cp = 0
             )
)


## ----class-toy-deep-2------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_class(dt = dfc, preds = predict(fit, dfc, type = 'class'))


## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(87654) # reproducibility
fit <- rpart(formula = y ~ x1 + x2,
             data = dfc,
             method = 'class',
             control = rpart.control(
               maxdepth = 20,
               minsplit = 10,
               minbucket = 5,
               cp = 0,
               xval = 5
             )
)
# Plot xval results
plotcp(fit)
# Get xval results
cpt <- fit$cptable
# Which cp value do we choose?
min_xerr <- which.min(cpt[,'xerror'])
se_rule <- min(which(cpt[, 'xerror'] < 
                       (cpt[min_xerr, 'xerror'] + cpt[min_xerr, 'xstd'])))
unname(min_xerr)
se_rule
print(cpt[16:35,], digits = 6)
fit_1 <- prune(fit, cp = cpt[min_xerr, 'CP'])
plot_pred_class(dt = dfc, preds = predict(fit_1, dfc, type = 'class'))
fit_2 <- prune(fit, cp = cpt[se_rule, 'CP'])
plot_pred_class(dt = dfc, preds = predict(fit_2, dfc, type = 'class'))


## ----mtpl-data-1-----------------------------------------------------------------------------------------------------------------------------------------------------------------
mtpl <- readRDS(paste0(data_path,'MTPL.rds'))
str(mtpl)


## ----mtpl-data-2-----------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(54321) # reproducubility
# Create a stratified data partition
train_id <- caret::createDataPartition(
  y = mtpl$nclaims/mtpl$expo,
  p = 0.8,
  groups = 100
)[[1]]


## ----mtpl-data-3-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Divide the data in training and test set
mtpl_trn <- mtpl[train_id,]
mtpl_tst <- mtpl[-train_id,]


## ----mtpl-data-4-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Proportions of the number of claims in train data
mtpl_trn$nclaims %>% table %>% prop.table %>% round(5)
# Proportions of the number of claims in test data
mtpl_tst$nclaims %>% table %>% prop.table %>% round(5)


## ----mtpl-tree-1-----------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = 
               cbind(expo,nclaims) ~
               ageph + agec + bm + power + 
               coverage + fuel + sex + fleet + use,
             data = mtpl_trn,
             method = 'poisson',
             control = rpart.control(
               maxdepth = 3,
               cp = 0)
)


## ----mtpl-tree-2-----------------------------------------------------------------------------------------------------------------------------------------------------------------
print(fit)


## ----mtpl-tree-3-----------------------------------------------------------------------------------------------------------------------------------------------------------------
rpart.plot(fit, cex = 1.5)


## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
mtpl_trn %>%
  dplyr::filter(bm >= 10,
                power >= 40) %>% 
  dplyr::summarise(claim_freq = 
                     sum(nclaims)/sum(expo))


## ----rpart-myst-1----------------------------------------------------------------------------------------------------------------------------------------------------------------
k <- 1
alpha <- 1/k^2
mu <- mtpl_trn %>% 
  with(sum(nclaims)/sum(expo))
beta <- alpha/mu
mtpl_trn %>% 
  dplyr::filter(bm >= 10, power >= 40) %>% 
  dplyr::summarise(prediction = 
                     (alpha + sum(nclaims))/(beta + sum(expo)))


## ----rpart-myst-2----------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = 
               cbind(expo,nclaims) ~
               ageph + agec + bm + power + 
               coverage + fuel + sex + fleet + use,
             data = mtpl_trn,
             method = 'poisson',
             control = rpart.control(
               maxdepth = 3,
               cp = 0),
             parms = list(shrink = 10^-5)
)
print(fit)


## ----rpart-myst-3----------------------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = 
               cbind(expo,nclaims) ~
               ageph + agec + bm + power + 
               coverage + fuel + sex + fleet + use,
             data = mtpl_trn,
             method = 'poisson',
             control = rpart.control(
               maxdepth = 3,
               cp = 0),
             parms = list(shrink = 10^5)
)
print(fit)

# Remember this number?
mtpl_trn %>% 
  dplyr::filter(bm >= 10, power >= 40) %>% 
  dplyr::summarise(claim_freq = 
                     sum(nclaims)/sum(expo))



## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(9753) # reproducibilty
fit <- rpart(formula = 
               cbind(expo,nclaims) ~
               ageph + agec + bm + power + 
               coverage + fuel + sex + fleet + use,
             data = mtpl_trn,
             method = 'poisson',
             control = rpart.control(
               maxdepth = 20,
               minsplit = 2000,
               minbucket = 1000,
               cp = 0,
               xval = 5
             )
)
# Plot the cross-validation results
plotcp(fit)
# Get the cross-validation results
cpt <- fit$cptable
# Look for the minimal xerror
min_xerr <- which.min(cpt[,'xerror'])
cpt[min_xerr,]
# Prune the tree
fit_srt <- prune(fit,
                 cp = cpt[min_xerr, 'CP'])
# Plot the tree
rpart.plot(fit_srt, type = 0, extra = 0, cex = 1.1)


## ----tree-vip-1------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Function vi gives you the data
var_imp <- vip::vi(fit_srt)
print(var_imp)


## ----tree-vip-2------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Function vip makes the plot
vip::vip(fit_srt, scale = TRUE)


## ----tree-pdp-1------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Need to define this helper function for Poisson
pred.fun <- function(object,newdata){
  mean(predict(object, newdata))
} 

## ----tree-pdp-2------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Sample 5000 observations to speed up pdp generation
set.seed(48927)
pdp_ids <- mtpl_trn %>%  nrow %>% 
  sample(size = 5000)


## ----tree-pdp-3------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit_srt %>% 
  partial(pred.var = 'ageph',
          pred.fun = pred.fun,
          train = mtpl_trn[pdp_ids,]) %>% 
  autoplot()


## ----tree-pdp-4------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit_srt %>% 
  partial(pred.var = c('ageph','power'),
          pred.fun = pred.fun,
          train = mtpl_trn[pdp_ids,]) %>% 
  autoplot()


## ----Your Turn: solution---------------------------------------------------------------------------------------------------------------------------------------------------------
fit_srt %>% 
  partial(pred.var = 'bm',
          pred.fun = pred.fun,
          train = mtpl_trn[pdp_ids,]) %>% 
  autoplot()

fit_srt %>% 
  partial(pred.var = 'coverage',
          pred.fun = pred.fun,
          train = mtpl_trn[pdp_ids,]) %>% 
  autoplot()
