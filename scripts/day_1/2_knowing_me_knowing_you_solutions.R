## Knowing me, knowing you: statistical and machine leanrning
library(tidyverse)
library(AmesHousing)

## ----regression---------------------------------------------------------------------------------
ggplot2::theme_set(ggplot2::theme_light())

model_1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames)

p_1 <- model_1 %>%
  broom::augment() %>%
  ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Regression with AMES housing data")

p_1


## ----------------------------------------------------------------------------------------------
model_1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames)


## ----------------------------------------------------------------------------------------------
summary(model_1)


## ----------------------------------------------------------------------------------------------
model_1$coefficients


## ----------------------------------------------------------------------------------------------
coef(model_1)


## ----------------------------------------------------------------------------------------------
summary(model_1)$coefficients


## ----------------------------------------------------------------------------------------------
head(model_1$fitted.values)


## ----------------------------------------------------------------------------------------------
summary(model_1)$r.squared


## ----------------------------------------------------------------------------------------------
model_1 %>% broom::tidy()


## ----------------------------------------------------------------------------------------------
model_1 %>% broom::glance()


## ----------------------------------------------------------------------------------------------
model_1 %>% broom::augment() %>% slice(1:5)


## ----------------------------------------------------------------------------------------------
g_lm_1 <- ggplot(data = ames, aes(Gr_Liv_Area, Sale_Price)) + theme_bw() +
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(se = TRUE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Regression with AMES housing data")
g_lm_1


## ----------------------------------------------------------------------------------------------
model_1 %>% broom::augment() %>% ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  theme_bw() +
  geom_point(size = 1, alpha = 0.3) +
  geom_line(aes(y = .fitted), col = KULbg) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Regression with AMES housing data")