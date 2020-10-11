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
    ## 1       1 2011-01-01      1  0    1  0       0       6          0          1
    ## 2       2 2011-01-01      1  0    1  1       0       6          0          1
    ## 3       3 2011-01-01      1  0    1  2       0       6          0          1
    ## 4       4 2011-01-01      1  0    1  3       0       6          0          1
    ## 5       5 2011-01-01      1  0    1  4       0       6          0          1
    ## 6       6 2011-01-01      1  0    1  5       0       6          0          2
    ##   temp  atemp  hum windspeed cnt
    ## 1 0.24 0.2879 0.81    0.0000  16
    ## 2 0.22 0.2727 0.80    0.0000  40
    ## 3 0.22 0.2727 0.80    0.0000  32
    ## 4 0.24 0.2879 0.75    0.0000  13
    ## 5 0.24 0.2879 0.75    0.0000   1
    ## 6 0.24 0.2576 0.75    0.0896   1

Create Parameters & Render
==========================

\#{r, parameters}
