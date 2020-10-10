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

    bikeData <- read.csv("day.csv")
    bikeData_Monday <- bikeData %>% filter(weekday == 1)
    head(bikeData_Monday)

    ##   instant     dteday season yr mnth holiday weekday workingday weathersit
    ## 1       3 2011-01-03      1  0    1       0       1          1          1
    ## 2      10 2011-01-10      1  0    1       0       1          1          1
    ## 3      17 2011-01-17      1  0    1       1       1          0          2
    ## 4      24 2011-01-24      1  0    1       0       1          1          1
    ## 5      31 2011-01-31      1  0    1       0       1          1          2
    ## 6      38 2011-02-07      1  0    2       0       1          1          1
    ##        temp    atemp      hum windspeed casual registered  cnt
    ## 1 0.1963640 0.189405 0.437273 0.2483090    120       1229 1349
    ## 2 0.1508330 0.150888 0.482917 0.2232670     41       1280 1321
    ## 3 0.1758330 0.176771 0.537500 0.1940170    117        883 1000
    ## 4 0.0973913 0.117930 0.491739 0.1583300     86       1330 1416
    ## 5 0.1808330 0.186250 0.603750 0.1871920     42       1459 1501
    ## 6 0.2716670 0.303658 0.738333 0.0454083    120       1592 1712
