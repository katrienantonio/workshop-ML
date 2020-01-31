## Step 1: install and load packages

packages <- c("tidyverse", "here", "gridExtra", "AmesHousing", "caret", "rsample", "broom", 
              "recipes", "mgcv", "glmnet", "evtree", "classInt", "rgdal", "RColorBrewer", 
              "ggmap", "grid", "rpart", "rpart.plot", "rpart.utils", "vip", "pdp", "ipred", 
              "ranger", "gbm", "xgboost", "gganimate", "transformr", "zeallot", "sp", 
              "tmap", "partykit", "rattle", "sf", "leaflet", "rstudioapi")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# from github
install.packages(c("Rtools", "devtools"))
devtools::install_github("henckr/distRforest")
library(distRforest) # from https://github.com/henckr/distRforest

all_packages <- c("ggplot2", "dplyr", "tidyr", "purrr", "readr", "tibble", "lubridate", 
                  "here", "gridExtra", "AmesHousing", "caret", "rsample", "broom", 
                  "recipes", "mgcv", "glmnet", "evtree", "classInt", "rgdal", 
                  "RColorBrewer", "ggmap", "grid", "rpart", "RColorBrewer", "ggmap", 
                  "grid", "gridExtra", "rpart", "rpart.plot", "rpart.utils", "vip", "pdp", 
                  "ipred", "ranger", "gbm", "xgboost", "gganimate", "transformr", 
                  "zeallot", "sp", "tmap", "partykit", "rattle", "sf", "leaflet", 
                  "rstudioapi", "distRforest")

if(sum(!(all_packages %in% installed.packages()[, "Package"]))) {
  stop(paste('The following required packages are not installed:\n', 
          paste(all_packages[which(!(all_packages %in% installed.packages()[, "Package"]))], collapse = ', ')));
} else {
  message("Everything is set up correctly. Now go to the next steps (h2o and keras).")
}

## Step 2: get R interface to h2o

### Option 1

# The following two commands remove any previously installed h2o packages for R
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that h2o depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the h2o package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yu/1/R")

### Option 2 (if above does not work)

#install.packages("h2o")
#library(h2o)

## Step 3: get R interface to keras

# Start by intalling Anaconda from https://www.anaconda.com/distribution/#download-section
# Execute the following lines of code once Anaconda is installed.

devtools::install_github("rstudio/keras")
library(keras)
install_keras()

# if the above instructions for Step 3 do not work, please consult the alternative
# steps on https://github.com/katrienantonio/workshop-ML

