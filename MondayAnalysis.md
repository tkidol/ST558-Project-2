Bicycle Analysis
================
Todd Idol
10/10/2020

-   [Project Repo](#project-repo)
-   [Packages](#packages)
    -   [Tidyverse](#tidyverse)
    -   [Rmarkdown](#rmarkdown)
    -   [Caret](#caret)
-   [Set Parameters & Knit](#set-parameters-knit)
-   [Data](#data)
-   [Create Parameters & Render](#create-parameters-render)

Project Repo
============

Find the project repo [here](https://github.com/tkidol/ST558-Project-2).

Packages
========

Tidyverse
---------

The workhorse of this project for 3 main core packages: DPLYR -
manipulated all of my data (selects, joins, filter, new variables… ),
Tibble for rendering Data Frames more effectively, & () ggplot2 for
discrete & continuous data plots

Rmarkdown
---------

Key to the project for the Rmd file itself including the knitr::
functions for consumable object output (kable) & README.md github doc
output through render()

Caret
-----

Used to create standardized train/test data & create controls and tuning
for LM & GLM analysis

Set Parameters & Knit
=====================

Data
====

    bikeData <- read.csv("hour.csv")

    # removing casual & registered vars since they will not be used for modling
    bikeData <- bikeData  %>% select(everything(), -c("casual", "registered")) %>% mutate(season = as.factor(season), mnth= as.factor(mnth), hr = as.factor(hr), weekday = as.factor(weekday), workingday = as.factor(workingday), weathersit = as.factor(weathersit))

    bikeData <- bikeData  %>% filter(weekday == params$day)

    # Changing weekday variable from numeric to character

    head(bikeData)

    ##   instant     dteday season yr mnth hr holiday weekday workingday weathersit
    ## 1      48 2011-01-03      1  0    1  0       0       1          1          1
    ## 2      49 2011-01-03      1  0    1  1       0       1          1          1
    ## 3      50 2011-01-03      1  0    1  4       0       1          1          1
    ## 4      51 2011-01-03      1  0    1  5       0       1          1          1
    ## 5      52 2011-01-03      1  0    1  6       0       1          1          1
    ## 6      53 2011-01-03      1  0    1  7       0       1          1          1
    ##   temp  atemp  hum windspeed cnt
    ## 1 0.22 0.1970 0.44    0.3582   5
    ## 2 0.20 0.1667 0.44    0.4179   2
    ## 3 0.16 0.1364 0.47    0.3881   1
    ## 4 0.16 0.1364 0.47    0.2836   3
    ## 5 0.14 0.1061 0.50    0.3881  30
    ## 6 0.14 0.1364 0.50    0.1940  64

Create Parameters & Render
==========================

\#{r, parameters}
