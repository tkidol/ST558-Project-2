Bicycle Analysis
================
Todd Idol
10/10/2020

-   [Project Repo](#project-repo)
-   [Packages](#packages)
    -   [Tidyverse](#tidyverse)
    -   [Rmarkdown](#rmarkdown)
    -   [JSONLite](#jsonlite)
    -   [HTTR](#httr)
-   [Data](#data)

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

JSONLite
--------

PUsed to converting API pulled JSON data into R objects

HTTR
----

Per R documentation site: Functions for the most important http verbs:
GET(), HEAD(), PATCH(), PUT(), DELETE() and POST().

Data
====

    bikeData <- read.csv("hour.csv")
    bikeData_Monday <- bikeData %>% filter(weekday == 1)
    head(bikeData_Monday)

    ##   instant     dteday season yr mnth hr holiday weekday workingday weathersit temp  atemp  hum windspeed
    ## 1      48 2011-01-03      1  0    1  0       0       1          1          1 0.22 0.1970 0.44    0.3582
    ## 2      49 2011-01-03      1  0    1  1       0       1          1          1 0.20 0.1667 0.44    0.4179
    ## 3      50 2011-01-03      1  0    1  4       0       1          1          1 0.16 0.1364 0.47    0.3881
    ## 4      51 2011-01-03      1  0    1  5       0       1          1          1 0.16 0.1364 0.47    0.2836
    ## 5      52 2011-01-03      1  0    1  6       0       1          1          1 0.14 0.1061 0.50    0.3881
    ## 6      53 2011-01-03      1  0    1  7       0       1          1          1 0.14 0.1364 0.50    0.1940
    ##   casual registered cnt
    ## 1      0          5   5
    ## 2      0          2   2
    ## 3      0          1   1
    ## 4      0          3   3
    ## 5      0         30  30
    ## 6      1         63  64
