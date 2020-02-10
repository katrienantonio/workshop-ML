## Data sets used in the course
library(tidyverse)
library(AmesHousing)

## ----colors-----------------------------------------------------------------------------------
KULbg <- "#116E8A"

## ----indeeddotcom-----------------------------------------------------------------------------
# The popularity data (by Katrien on Jan 12, 2020 via indeed.com)
pop_df <- 
  data.frame(
    lang = c("SQL", "Python", "R", "SAS", "Matlab", "SPSS", "Stata"),
    n_jobs = c(80329, 71894, 51865, 24355, 11710, 3497, 1874),
    free = c(T, T, T, F, F, F, F)
  )
## Plot it
pop_df %>% 
  mutate(lang = lang %>% factor(ordered = T)) %>%
  ggplot(aes(x = lang, y = n_jobs, fill = free)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  aes(x = reorder(lang, -n_jobs), fill = reorder(free, -free)) +
  xlab("Statistical language") +
  scale_y_continuous(label = scales::comma) +
  ylab("Number of jobs") +
  labs(
    title = "Comparing statistical languages",
    subtitle = "Number of job postings on Indeed.com, 2020/01/12"
  ) +
  scale_fill_manual(
    "Free?",
    labels = c("True", "False"),
    values = c("#116E8A", "slategray")
  ) +
  ggthemes::theme_pander(base_size = 17) +
  # theme_ipsum() +
  theme(legend.position = "bottom")


## ----------------------------------------------------------------------------------------------
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
mtpl_orig <- read.table('./data/P&Cdata.txt',
                        header = TRUE)
mtpl_orig <- as_tibble(mtpl_orig)


## ----------------------------------------------------------------------------------------------
mtpl_orig %>% slice(1:3) %>% select(-LONG, -LAT) 


## ----prepare-mtpl------------------------------------------------------------------------------
mtpl <- mtpl_orig %>%
  # rename all columns 
  rename_all(function(.name) {
    .name %>% 
      # replace all names with the lowercase versions
      tolower 
    # replace all spaces with underscores is also useful, with `str_replace(" ", "-")`
  })
mtpl <- rename(mtpl, expo = exp)


## ----first-inspection-mtpl---------------------------------------------------------------------
dim(mtpl)


## ----first-risk-calculations-mtpl-2------------------------------------------------------------
mtpl %>% summarize(emp_freq = sum(nclaims) / sum(expo)) 


## ----first-risk-calculations-mtpl-3------------------------------------------------------------
mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo)) 


## ----first-graphs-mtpl-------------------------------------------------------------------------
g <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g


## ----get-ames-data-----------------------------------------------------------------------------
library(AmesHousing)
ames <- AmesHousing::make_ames()


## ----first-inspection-ames---------------------------------------------------------------------
dim(ames)


## ----risk-calculations-ames--------------------------------------------------------------------
ames %>% summarize(avg_price = mean(Sale_Price)) 


## ----risk-calculations-ames-2, eval = F--------------------------------------------------------
ames %>% 
  group_by(Neighborhood) %>% 
  summarize(avg_price = mean(Sale_Price)) %>% slice(1:3) 


## ----distribution-response-ames----------------------------------------------------------------
g_dens <- ggplot(ames, aes(x = Sale_Price)) + theme_bw() +
  geom_density(data = ames, col = KULbg, fill = KULbg, alpha = .5) +
  ggtitle("Ames housing data - sale prices")
g_dens


## ----------------------------------------------------------------------------------------------
## ----Your Turn---------------------------------------------------------------------------------


## ----Your Turn ends here-----------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------
