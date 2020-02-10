## Regression models
library(tidyverse)
library(gridExtra)
library(AmesHousing)
library(caret)
library(rsample)
library(recipes)
library(glmnet)
library(mgcv)


KULbg <- "#116E8A"


## ----------------------------------------------------------------------------------------------
g_freq <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg, alpha = .5) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g_freq


## ----------------------------------------------------------------------------------------------
g_sev <- ggplot(mtpl, aes(x = avg)) + theme_bw() +
  geom_histogram(bins = 30, boundary = 0, color = KULbg, fill = KULbg, alpha = .5) + 
  labs(x = "claim severity") +
  xlim(c(0, 20000))
g_sev


## ---------------------------------------------------------------------------------------------
freq_by_gender <- mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) 
freq_by_gender 


## ----------------------------------------------------------------------------------------------
ggplot(freq_by_gender, aes(x = sex, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", col = KULbg, fill = KULbg, alpha = .5)


## ----------------------------------------------------------------------------------------------
freq_glm_1 <- glm(nclaims ~ sex, offset = log(expo), 
                  family = poisson(link = "log"), 
                  data = mtpl)
freq_glm_1 %>% broom::tidy() 


## ----------------------------------------------------------------------------------------------
freq_glm_1 %>% broom::augment(type.predict = "response") %>% slice(1:2) %>% select(nclaims, sex, .fitted, .se.fit) 


## ----------------------------------------------------------------------------------------------
exp(coef(freq_glm_1)[1])
exp(coef(freq_glm_1)[1] + coef(freq_glm_1)[2])


## ----------------------------------------------------------------------------------------------
male_driver <- data.frame(expo = 1, sex = "male")
female_driver <- data.frame(expo = 1, sex = "female")


## ----------------------------------------------------------------------------------------------
predict(freq_glm_1, newdata = male_driver, type = "response")


## ----------------------------------------------------------------------------------------------
predict(freq_glm_1, newdata = male_driver, type = "link")


## ----------------------------------------------------------------------------------------------
sev_glm_1 <- glm(avg ~ sex, family = Gamma(link = "log"), data = mtpl)
sev_glm_1


## ----------------------------------------------------------------------------------------------
mtpl %>% group_by(ageph) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) %>% 
  ggplot(aes(x = ageph, y = emp_freq)) + theme_bw() +
  geom_point(color = KULbg)


## ----------------------------------------------------------------------------------------------
a <- min(mtpl$ageph):max(mtpl$ageph)


## ----------------------------------------------------------------------------------------------
freq_glm_age <- glm(nclaims ~ ageph, offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age <- predict(freq_glm_age, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age <- pred_glm_age$fit
l_glm_age <- pred_glm_age$fit - qnorm(0.975)*pred_glm_age$se.fit
u_glm_age <- pred_glm_age$fit + qnorm(0.975)*pred_glm_age$se.fit
df <- data.frame(a, b_glm_age, l_glm_age, u_glm_age)


## ----------------------------------------------------------------------------------------------
p_glm_age <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age <- p_glm_age + geom_line(aes(a, b_glm_age), size = 1, col = KULbg)   
p_glm_age <- p_glm_age + geom_line(aes(a, u_glm_age), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age), size = 0.5, linetype = 2, col = KULbg)
p_glm_age <- p_glm_age + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age


## ----------------------------------------------------------------------------------------------
freq_glm_age_f <- glm(nclaims ~ as.factor(ageph), offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age_f <- predict(freq_glm_age_f, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age_f <- pred_glm_age_f$fit
l_glm_age_f <- pred_glm_age_f$fit - 
  qnorm(0.975)*pred_glm_age_f$se.fit
u_glm_age_f <- pred_glm_age_f$fit + 
  qnorm(0.975)*pred_glm_age_f$se.fit
df <- data.frame(a, b_glm_age_f, 
                 l_glm_age_f, u_glm_age_f)


## ----------------------------------------------------------------------------------------------
p_glm_age_f <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age_f <- p_glm_age_f + geom_line(aes(a, b_glm_age_f), size = 1, col = KULbg)   
p_glm_age_f <- p_glm_age_f + geom_line(aes(a, u_glm_age_f), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age_f), size = 0.5, linetype = 2, col = KULbg)
p_glm_age_f <- p_glm_age_f + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age_f


## ----------------------------------------------------------------------------------------------
level <- seq(min(mtpl$ageph), max(mtpl$ageph), by = 5)
freq_glm_age_c <- glm(nclaims ~ cut(ageph, level), offset = log(expo), data = mtpl, family = poisson(link = "log"))
pred_glm_age_c <- predict(freq_glm_age_c, newdata = data.frame(ageph = a, expo = 1), type = "terms", se.fit = TRUE)
b_glm_age_c <- pred_glm_age_c$fit
l_glm_age_c <- pred_glm_age_c$fit - 
  qnorm(0.975)*pred_glm_age_c$se.fit
u_glm_age_c <- pred_glm_age_c$fit + 
  qnorm(0.975)*pred_glm_age_c$se.fit
df <- data.frame(a, b_glm_age_c, 
                 l_glm_age_c, u_glm_age_c)


## ----------------------------------------------------------------------------------------------
p_glm_age_c <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_glm_age_c <- p_glm_age_c + geom_line(aes(a, b_glm_age_c), size = 1, col = KULbg)   
p_glm_age_c <- p_glm_age_c + geom_line(aes(a, u_glm_age_c), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_glm_age_c), size = 0.5, linetype = 2, col = KULbg)
p_glm_age_c <- p_glm_age_c + xlab("ageph") + ylab("fit") + theme_bw()
p_glm_age_c


## ----------------------------------------------------------------------------------------------
library(mgcv)
freq_gam_age <- gam(nclaims ~ s(ageph), 
                    offset = log(expo), 
                    data = mtpl, 
                    family = poisson(link = "log"))
pred_gam_age <- predict(freq_gam_age, 
                        newdata = data.frame(ageph = a, expo = 1), 
                        type = "terms", se.fit = TRUE)
b_gam_age <- pred_gam_age$fit
l_gam_age <- pred_gam_age$fit -
  qnorm(0.975)*pred_gam_age$se.fit
u_gam_age <- pred_gam_age$fit +
  qnorm(0.975)*pred_gam_age$se.fit
df <- data.frame(a, b_gam_age, 
                 l_gam_age, u_gam_age)


## ----------------------------------------------------------------------------------------------
p_gam_age <- ggplot(df, aes(x = a)) + ylim(-0.5, 1)
p_gam_age <- p_gam_age + geom_line(aes(a, b_gam_age), size = 1, col = KULbg)   
p_gam_age <- p_gam_age + geom_line(aes(a, u_gam_age), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_gam_age), size = 0.5, linetype = 2, col = KULbg)
p_gam_age <- p_gam_age + xlab("ageph") + ylab("fit") + theme_bw()
p_gam_age


## ----------------------------------------------------------------------------------------------
library(mgcv)
freq_gam <- gam(nclaims ~ s(ageph), offset = log(expo), family = poisson(link = "log"), data = mtpl)
plot(freq_gam, scheme = 4)


## ----------------------------------------------------------------------------------------------
freq_gam_2 <- gam(nclaims ~ sex + fuel + use + 
                    s(ageph) + s(bm), 
                  offset = log(expo), 
                  family = poisson(link = "log"), 
                  data = mtpl)


## ----------------------------------------------------------------------------------------------
summary(freq_gam_2)


## ----------------------------------------------------------------------------------------------
plot(freq_gam_2, select = 1)


## ----------------------------------------------------------------------------------------------
plot(freq_gam_2, select = 2)


## ----------------------------------------------------------------------------------------------
drivers <- data.frame(expo = c(1, 1, 1), sex = c("female", "female", "female"), fuel = c("diesel", "diesel", "diesel"), use = c("private", "private", "private"), ageph = c(18, 45, 65), bm = c(20, 5, 0))
drivers 


## ----------------------------------------------------------------------------------------------
predict(freq_gam_2, newdata = drivers, 
        type = "response") %>% kable(format = 'html')


## ----------------------------------------------------------------------------------------------
library(glmnet)
data(QuickStartExample)
fit <- glmnet(x, y, family = "gaussian", alpha = 1, standardize = TRUE, intercept = TRUE)


## ----------------------------------------------------------------------------------------------
tidy(fit) %>% slice(1:5) 


## ----------------------------------------------------------------------------------------------
plot(fit, label = TRUE)


## ----------------------------------------------------------------------------------------------
plot(fit, label = TRUE, xvar = 'lambda')


## ----------------------------------------------------------------------------------------------
plot(fit, xvar = 'dev', label = TRUE)


## ----------------------------------------------------------------------------------------------
print(fit) 


## ----------------------------------------------------------------------------------------------
coef(fit, s = 0.1)


## ----------------------------------------------------------------------------------------------
cv_fit <- cv.glmnet(x, y)


## ----------------------------------------------------------------------------------------------
cv_fit$lambda.min


## ----------------------------------------------------------------------------------------------
cv_fit$lambda.1se


## ----------------------------------------------------------------------------------------------
plot(cv_fit)


## ----------------------------------------------------------------------------------------------
coef(fit, s = cv_fit$lambda.min)


## ----------------------------------------------------------------------------------------------
col <- c(1, 3, 5:8, 11, 14, 20)
subset <- data.frame(y = y, V1 = x[, 1], V3 = x[, 3], V5 = x[, 5], V6 = x[, 6], V7 = x[, 7], V8 = x[, 8], V11 = x[, 11], V14 = x[, 14], V20 = x[, 20])
final_model <- lm(y ~ V1 + V3 + V5 + V6 + V7 + V8 + V11 + V14 + V20, data = subset)
final_model %>% broom::tidy() %>% kable(format = 'html')


## ----------------------------------------------------------------------------------------------
map(mtpl[, c("coverage")], contrasts, 
    contrasts = FALSE)


## ----------------------------------------------------------------------------------------------
map(mtpl[, c("coverage")], contrasts, 
    contrasts = TRUE)


## ---------------------------------------------------------------------------------------------
y <- mtpl$nclaims

x <- model.matrix( ~ coverage + fuel + use + fleet + sex + ageph + bm +
                     agec + power, data = mtpl, 
                   contrasts.arg = map(mtpl[, c("coverage")], contrasts, 
                                       contrasts = FALSE))[,-1]

x[1:6,]


## ---------------------------------------------------------------------------------------------
y <- mtpl$nclaims

x <- model.matrix( ~ coverage + fuel + use + fleet + sex + ageph + bm +
                     agec + power, data = mtpl, 
                   contrasts.arg = map(mtpl[, c("coverage")], contrasts, 
                                       contrasts = FALSE))[,-1]
alpha <- 1 # for lasso penalty
mtpl_glmnet <- glmnet(x = x, y = y, 
                      family = "poisson", 
                      offset = log(mtpl$expo), 
                      alpha = alpha, 
                      standardize = TRUE, 
                      intercept = TRUE)


## ----------------------------------------------------------------------------------------------
row.names(mtpl_glmnet$beta) 


## ----------------------------------------------------------------------------------------------
plot(mtpl_glmnet, xvar = 'lambda', label = TRUE)  


## ----------------------------------------------------------------------------------------------
set.seed(123)
fold_id <- sample(rep(1:10, length.out = nrow(mtpl)), 
                  nrow(mtpl))
mtpl_glmnet_cv <- cv.glmnet(x, y, family = "poisson", 
                            alpha = alpha, 
                            nfolds = 10, 
                            foldid = fold_id, 
                            type.measure = "deviance", 
                            standardize = TRUE, 
                            intercept = TRUE)
plot(mtpl_glmnet_cv)


## ----------------------------------------------------------------------------------------------
coef(mtpl_glmnet_cv, s = "lambda.min")


## ----------------------------------------------------------------------------------------------
coef(mtpl_glmnet_cv, s = "lambda.1se")


## ----------------------------------------------------------------------------------------------
mtpl$coverage <- relevel(mtpl$coverage, "PO")
mtpl_formula_refit <- nclaims ~ 1 + coverage + 
  fuel + use + fleet + sex + 
  ageph + bm + agec + power
mtpl_glm_refit <- glm(mtpl_formula_refit, 
                      data = mtpl, 
                      offset = log(mtpl$expo), 
                      family = poisson())


## -----------------------------------------------------------------------------------------------
mtpl_formula_refit_2 <- nclaims ~ 1 + ageph + bm 
mtpl_glm_refit_2 <- glm(mtpl_formula_refit_2, 
                        data = mtpl, 
                        offset = log(mtpl$expo), 
                        family = poisson())



