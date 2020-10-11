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
    bikeData <- bikeData %>% filter(weekday == params$day) %>% select(everything(), -c("casual", "registered"))

    # Changing weekday variable from numeric to character

    head(bikeData)

    ##   instant     dteday season yr mnth hr holiday weekday workingday weathersit temp  atemp  hum windspeed
    ## 1     116 2011-01-06      1  0    1  0       0       4          1          1 0.18 0.2424 0.55    0.0000
    ## 2     117 2011-01-06      1  0    1  1       0       4          1          1 0.16 0.2273 0.64    0.0000
    ## 3     118 2011-01-06      1  0    1  2       0       4          1          1 0.16 0.2273 0.64    0.0000
    ## 4     119 2011-01-06      1  0    1  4       0       4          1          2 0.16 0.1970 0.64    0.0896
    ## 5     120 2011-01-06      1  0    1  5       0       4          1          2 0.14 0.1818 0.69    0.0896
    ## 6     121 2011-01-06      1  0    1  6       0       4          1          2 0.14 0.1667 0.63    0.1045
    ##   cnt
    ## 1  11
    ## 2   4
    ## 3   2
    ## 4   1
    ## 5   4
    ## 6  36

Create Parameters & Render
==========================

\#{r, parameters}

charDay &lt;- c(“Sunday”, “Monday”, “Tuesday”, “Wednesday”, “Thursday”,
“Friday”, “Saturday”) str(charDay)

output\_file &lt;- paste0(charDay,“Analysis.md”) params &lt;- lapply()

reports &lt;- tibble(output\_file, params) reports

apply(reports, MARGIN = 1, FUN = function(x) { render(input =
“P2\_TKIdol.Rmd”, output\_file = x\[\[1\]\], params = x\[\[2\]\]) })
\`\`\`
