README
================
Todd Idol
10/16/2020

-   [Project Repo](#project-repo)
-   [Weekly Analysis](#weekly-analysis)
-   [Packages](#packages)
    -   [Tidyverse](#tidyverse)
    -   [Rmarkdown](#rmarkdown)
    -   [Caret](#caret)
-   [Render Code](#render-code)

Project Repo
============

Find the project repo [here](https://github.com/tkidol/ST558-Project-2).

Weekly Analysis
===============

-   The analysis for [Sunday is available here](SundayAnalysis.md).

-   The analysis for [Monday is available here](MondayAnalysis.md).

-   The analysis for [Tuesday is available here](TuesdayAnalysis.md).

-   The analysis for [Wednesday is available here](Wedesdaynalysis.md).

-   The analysis for [Thursday is available here](ThursdayAnalysis.md).

-   The analysis for [Friday is available here](FridayAnalysis.md).

-   The analysis for [Saturday is available here](SaturdayAnalysis.md).

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

Render Code
===========

Link to file
[here](https://github.com/tkidol/ST558-Project-2/blob/main/P2_Render.Rmd)

\`ibrary(dplyr) library(rmarkdown)

charDay &lt;- c(“Sunday”, “Monday”, “Tuesday”, “Wednesday”, “Thursday”,
“Friday”, “Saturday”)

numDay &lt;- c(“0”, “1”, “2”, “3”, “4”, “5”, “6”)

output\_file &lt;- paste0(charDay,“Analysis.md”)

params &lt;- lapply(numDay, FUN = function(x) (list(day = x)))

reports &lt;- tibble(output\_file, params)

apply(reports, MARGIN = 1, FUN = function(x) { render(“P2\_TKIdol.Rmd”,
output\_file = x\[\[1\]\], params = x\[\[2\]\]) })\`
