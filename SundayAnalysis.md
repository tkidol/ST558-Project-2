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
    ## 1      25 2011-01-02      1  0    1  0       0       0          0          2
    ## 2      26 2011-01-02      1  0    1  1       0       0          0          2
    ## 3      27 2011-01-02      1  0    1  2       0       0          0          2
    ## 4      28 2011-01-02      1  0    1  3       0       0          0          2
    ## 5      29 2011-01-02      1  0    1  4       0       0          0          2
    ## 6      30 2011-01-02      1  0    1  6       0       0          0          3
    ##   temp  atemp  hum windspeed cnt
    ## 1 0.46 0.4545 0.88    0.2985  17
    ## 2 0.44 0.4394 0.94    0.2537  17
    ## 3 0.42 0.4242 1.00    0.2836   9
    ## 4 0.46 0.4545 0.94    0.1940   6
    ## 5 0.46 0.4545 0.94    0.1940   3
    ## 6 0.42 0.4242 0.77    0.2985   2

Create Parameters & Render
==========================

\#{r, parameters}
