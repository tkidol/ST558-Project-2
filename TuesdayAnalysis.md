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
    ## 1      70 2011-01-04      1  0    1  0       0       2          1          1
    ## 2      71 2011-01-04      1  0    1  1       0       2          1          1
    ## 3      72 2011-01-04      1  0    1  2       0       2          1          1
    ## 4      73 2011-01-04      1  0    1  4       0       2          1          1
    ## 5      74 2011-01-04      1  0    1  5       0       2          1          1
    ## 6      75 2011-01-04      1  0    1  6       0       2          1          1
    ##   temp  atemp  hum windspeed cnt
    ## 1 0.16 0.1818 0.55    0.1045   5
    ## 2 0.16 0.1818 0.59    0.1045   2
    ## 3 0.14 0.1515 0.63    0.1343   1
    ## 4 0.14 0.1818 0.63    0.0896   2
    ## 5 0.12 0.1515 0.68    0.1045   4
    ## 6 0.12 0.1515 0.74    0.1045  36

Create Parameters & Render
==========================

\#{r, parameters}
