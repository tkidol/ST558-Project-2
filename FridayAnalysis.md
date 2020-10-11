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

JSONLite
--------

Used for converting API pulled JSON data into R objects

HTTR
----

Per R documentation site: Functions for the most important http verbs:
GET(), HEAD(), PATCH(), PUT(), DELETE() and POST().

Set Parameters & Knit
=====================

Data
====

    bikeData <- read.csv("hour.csv")

    # removing casual & registered vars since they will not be used for modling
    bikeData <- bikeData  %>% select(everything(), -c("casual", "registered")) %>% mutate(weekday = as.factor(weekday), workingday = as.factor(workingday), weathersit = as.factor(weathersit))

    bikeData <- bikeData  %>% filter(weekday == params$day)

    # Changing weekday variable from numeric to character

    head(bikeData)

Create Parameters & Render
==========================

\#{r, parameters}
