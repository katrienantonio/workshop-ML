## Workshop machine learning, 2020

In these folders you will find all the material for a 2 day workshop in basic and advanced R.

### Schedule

| Session | Duration      | Description | Material |
|:-------:|-----------|-------------|------|
| Day 1   | 3.5 hours | Basic R     | [slides](https://www.google.com)|
| Day 2   | 3.5 hours | Advanced R  | .. |
| Day 3   | 3.5 hours | ...         | .. |

#### Day 1: Basic R

The R programming language is an invaluable tool for data science. This course equips you with the fundamental tools for data analysis in R and RStudio. You'll come out of this course with basic understanding of R as an environment for data handling, data exploration and data visualization. Via a set of hands-on demonstrations and exercises you first study the basics of the R syntax, you will explore RStudio as an integrated development environment for R programming and learn about R packages. You explore the different types of objects, data structures and data types used in R. Then, you focus on working with data sets in R; import, export, data manipulations, data cleaning and wrangling. You will meet powerful packages such as data.table and the packages from the tidyverse. Finally, you will use R to create various types of insightful graphics and discover the basics of writing and using funtions in R. The course is designed for new R users, no prior knowledge is required.

#### Day 2: Advanced R

This course is designed for R users who want to improve their skills and understanding of the language, with a specific focus on using R efficiently for data science projects. You unlock the full potential of the tidyverse by learning more methods for manipulating data sets. You learn more about working with common R data types such as dates and factor variables. After this course you are able to write more efficient R code through the use of functionals. As you will be writing more complex programs, error handling becomes more important. You learn the most important principles for discovering and dealing with errors. Hands-on demonstrations and exercises allow you to experience with these new concepts and become confident with your newly acquired skills.

### Software Requirements

Please bring a laptop with a recent version of R and RStudio installed.

- R (at least 3.5.2 <https://cloud.r-project.org/bin/windows/base/> )
- RStudio ( <https://www.rstudio.com/products/rstudio/download/#download> )

Run the following script in your R session to install the required packages


```r
packages <- c("tidyverse", "sas7bdat", "corrplot", "readxl", "data.table", "gapminder", "microbenchmark")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

all_packages <- c("ggplot2", "dplyr", "tidyr", "purrr", "readr", "tibble", "lubridate", "sas7bdat", "corrplot", "readxl", "data.table", "gapminder", "microbenchmark")

if(sum(!(all_packages %in% installed.packages()[, "Package"]))) {
  stop("Not all required packages are installed!")
} else {
  message("Everything is set up correctly. You are ready for the workshop!")
}
```

### Course material

Lecture sheets are available in the folders `1 - Basic R` and `2 - Advanced R`. These folders also host the scripts used throughout the workshop. Data sets are stored in the `data` folder. Happy learning!

