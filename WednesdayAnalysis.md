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
    ## 1      93 2011-01-05      1  0    1  0       0       3          1          1
    ## 2      94 2011-01-05      1  0    1  1       0       3          1          1
    ## 3      95 2011-01-05      1  0    1  2       0       3          1          1
    ## 4      96 2011-01-05      1  0    1  4       0       3          1          1
    ## 5      97 2011-01-05      1  0    1  5       0       3          1          1
    ## 6      98 2011-01-05      1  0    1  6       0       3          1          1
    ##   temp  atemp  hum windspeed cnt
    ## 1 0.20 0.2576 0.64    0.0000   6
    ## 2 0.16 0.1970 0.74    0.0896   6
    ## 3 0.16 0.1970 0.74    0.0896   2
    ## 4 0.24 0.2273 0.48    0.2239   2
    ## 5 0.22 0.2273 0.47    0.1642   3
    ## 6 0.20 0.1970 0.47    0.2239  33

Create Parameters & Render
==========================

\#{r, parameters}
