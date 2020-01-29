
## Workshop Machine Learning in R, 2020

by Katrien Antonio, Jonas Crevecoeur and Roel Henckaerts

Course materials for the *Machine Learning in R* course in February 2020
in The Hague.

📆 February 11, 12 and 13, 2020 <br> ⏲ From 9 am to 4.30 pm <br> 📍
Nationale Nederlanden, The Hague

Course materials will be posted in the week before the workshop.

### Overview

<p align="justify">

This three-day workshop introduces the *essential concepts of building
machine learning models with R*. Throughout the workshop you will gain
insights in the foundations of machine learning methods, including
*resampling methods*, *data preprocessing steps* and the *tuning of
parameters*. You will cover a variety of *statistical and machine
learning methods*, ranging from GLMs, over tree-based machine learning
methods to neural networks. You will acquire insights in the foundations
of these methods, learn how to set-up the model building process, and
focus on building a good understanding of the resulting model output and
predictions.

</p>

<p align="justify">

Leaving this workshop, you should have a firm grasp of the working
principles of a variety of machine learning methods and be able to
explore their use in practical settings. Moreover, you should have
acquired the fundamental insights to explore some other methods on your
own.

</p>

### Schedule

The detailed schedule follows in the week before the workshop.

| Session | Duration  | Description | Material                         |
| :-----: | --------- | ----------- | -------------------------------- |
|  Day 1  | 6.5 hours |             | [slides](https://www.google.com) |
|  Day 2  | 6.5 hours |             | ..                               |
|  Day 3  | 6.5 hours | …           | ..                               |

##### Day 1: Machine learning foundations, regression methods

Topics include:

  - statistical and machine learning: a tour
  - machine learning foundations
  - feature pre-processing steps
  - regression models (including regularization): `lm()`, `glm()`,
    `mgcv()`, `glmnet()`

##### Day 2: Tree-based machine learning methods

Topics include:

  - decision trees
  - ensembles of trees (bagging, random forests, gradient boosting
    machine, XGboost)
  - ML interpretability tools (variable importance, PDP, ICE, and more)
  - H2O

##### Day 3: Deep learning methods

Topics include:

  - neural network infrastructure, calibration steps and tuning of
    (hyper)parameters
  - building deep learning models with TensorFlow and Keras via the
    `keras` library in R
  - a variety of deep learning algorithms (e.g. CNNs and ANNs)
  - putting it all together: case studies
  - wrap up.

### Prework

<p align="justify">

The workshop requires a basic understanding of R. A good starting level
is the material covered in the
[1-Basic](https://github.com/katrienantonio/workshop-R/tree/master/1%20-%20Basic%20R)
folder of the [Programming in
R](https://github.com/katrienantonio/workshop-R) workshop taught at
Nationale Nederlanden in June 2019. From time to time you will also rely
on concepts taught in the
[2-Advanced](https://github.com/katrienantonio/workshop-R/tree/master/2%20-%20Advanced%20R)
folder of the same workshop, including `purrr` and some data handling
and visualisation tools. This workshop will provide some review of these
topics.

</p>

Familiarity with statistical or machine learning methods is *not*
required. The workshop gradually builds up these concepts, with an
emphasis on hands-on demonstrations and exercises.

### Software Requirements

Please bring a laptop with a recent version of R and RStudio installed.
Make sure you can connect your laptop to the internet (or download the
course material one day before the start of the workshop. You will need:

  - R (at least 3.6 <https://cloud.r-project.org/bin/windows/base/> )
  - RStudio (
    <https://www.rstudio.com/products/rstudio/download/#download> )

Run the following script in your R session to install the required
packages

``` r
packages <- c("tidyverse", "here", "gridExtra", "AmesHousing", "caret", "rsample", "broom", "recipes", "mgcv", "glmnet", "evtree", "classInt", "rgdal", "RColorBrewer", "ggmap", "grid", "gridExtra", "rpart")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

all_packages <- c("ggplot2", "dplyr", "tidyr", "purrr", "readr", "tibble", "lubridate", "here", "gridExtra", "AmesHousing", "caret", "rsample", "broom", "recipes", "mgcv", "glmnet", "evtree", "classInt", "rgdal", "RColorBrewer", "ggmap", "grid", "gridExtra", "rpart")

if(sum(!(all_packages %in% installed.packages()[, "Package"]))) {
  stop("Not all required packages are installed!")
} else {
  message("Everything is set up correctly. You are ready for the workshop!")
}
```

On **Day 3** you will be working with the R interface to `keras`. We
recommend the installation instructions on <https://keras.rstudio.com/>.

``` r
install.packages("devtools")
devtools::install_github("rstudio/keras")
library(keras)
install_keras()
```

If the above instructions do **not** work, you can **alternatively**
proceed as follows

  - download Anaconda at
    <https://www.anaconda.com/distribution/#download-section>, select
    the version for Python 3.7 and make sure to pick the right (top of
    the page: select Windows, macOS or Linux)

  - install Anaconda. This straightforward after launching the
    installer, but (in case you are in doubt) some instructions are at
    <https://docs.anaconda.com/anaconda/install/windows/>

  - open an Anaconda Prompt (or terminal for macOS) (e.g. via Start in
    Windows and search for Anaconda Prompt) and install keras and
    tensorflow with the following instructions

<!-- end list -->

``` python
pip install keras 
pip install tensorflow
```

  - install `keras` in R

<!-- end list -->

``` r
install.packages("devtools")
devtools::install_github("rstudio/keras")
library(keras)
```

### Course material

Lecture sheets will become available a week before the workshop (in pdf
and HTML format). R scripts, notebooks and the data sets used throughout
the course will be at your disposal.

### Instructors

<img src="img/Katrien.jpg" alt="Drawing" style="height: 20px;"/>

<p align="justify">

[Katrien Antonio](https://katrienantonio.github.io/) is professor in
insurance data science at KU Leuven and associate professor at
University of Amsterdam. She teaches courses on data science for
insurance, life and non-life insurance mathematics and loss models.
Research-wise Katrien puts focus on pricing, reserving and fraud
analytics, as well as mortality dynamics. Katrien will teach Day 1 of
thw workshop, and serve as TA on Day 2 and 3.

</p>

<p align="justify">

*Jonas Crevecoeur* is PhD student in insurance data science at KU
Leuven. Jonas will teach Day 3 of the workshop. He holds the degrees of
MSc in Mathematics, MSc in Insurance Studies and MSc in Financial and
Actuarial Engineering (KU Leuven). Before starting the PhD program he
worked as an intern with QBE Re (Belgium office) where he studied
multiline products and copulas. Jonas is PhD fellow of the Research
Foundation - Flanders (FWO, PhD fellowship fundamental research).

</p>

<p align="justify">

*Roel Henckaerts* is PhD student in insurance data science at KU Leuven.
Roel will teach Day 2 of the workshop. Roel holds the degrees of MSc in
Mathematical Engineering, MSc in Insurance Studies and Financial and
Actuarial Engineering (KU Leuven). Before starting the PhD program he
worked as an intern with AIG (London office) and KBC. Roel is PhD fellow
of the Research Foundation - Flanders (FWO, PhD fellowship strategic
basic research).

</p>

Happy learning\!

-----
