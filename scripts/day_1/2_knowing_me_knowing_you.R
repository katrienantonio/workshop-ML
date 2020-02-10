## Knowing me, knowing you: statistical and machine leanrning
library(tidyverse)
library(AmesHousing)

## ----regression---------------------------------------------------------------------------------
model_1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames)

p_1 <- model_1 %>%
  broom::augment() %>%
  ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Regression with AMES housing data")

p_1

## ----Your Turn---------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------




## ----Your Turn ends here------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------



## ----------------------------------------------------------------------------------------------
g_lm_1 <- ggplot(data = ames, aes(Gr_Liv_Area, Sale_Price)) + theme_bw() +
  geom_point(size = 1, alpha = 0.3) +
  geom_smooth(se = TRUE, method = "lm") +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Regression with AMES housing data")
g_lm_1


## ----------------------------------------------------------------------------------------------
g_lm_2 <- model_1 %>% broom::augment() %>% ggplot(aes(Gr_Liv_Area, Sale_Price)) + 
  theme_bw() +
  geom_point(size = 1, alpha = 0.3) +
  geom_line(aes(y = .fitted), col = KULbg) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Regression with AMES housing data")
g_lm_2