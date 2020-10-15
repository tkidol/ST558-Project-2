Bicycle Analysis
================
Todd Idol
10/10/2020

-   [Project Repo](#project-repo)
-   [Weekly Analysis](#weekly-analysis)
-   [Packages](#packages)
    -   [Tidyverse](#tidyverse)
    -   [Rmarkdown](#rmarkdown)
    -   [Caret](#caret)
-   [Set Parameters & Knit](#set-parameters-knit)
-   [Introduction](#introduction)
-   [Data](#data)
-   [Summarizations](#summarizations)
    -   [Summaries for Season & Weather](#summaries-for-season-weather)

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

Set Parameters & Knit
=====================

Introduction
============

You should have an introduction section that briefly describes the data
and the variables you have to work with (no need to discuss all of them,
just the ones you want to use). If you are analyzing the bike share
data, do not use the casual and registered variables to do any modeling!
You should also mention the purpose of your analysis and the methods
you’ll use (no need to detail them here) for analysis.

Data
====

    bikeData <- read.csv("hour.csv")

    # removing casual & registered vars since they will not be used for modeling
    bikeData <- bikeData  %>% select(everything(), -c("casual", "registered")) %>% mutate(weekday = as.factor(weekday))

    # filtering for individual weekday reporting
    bikeData <- bikeData  %>% filter(weekday == params$day)

    latenight <- bikeData %>% filter(hr == 3)
    latenightcount <- sum(latenight$cnt)
    latenightcount

    ## [1] 2403

Summarizations
==============

    sumBikeData <- bikeData %>% select(dteday, season, hr, workingday, weathersit, temp, hum, windspeed, cnt)

    # create factors for season, timeofday and weather
    sumBikeData <- mutate(sumBikeData, workday = as.factor(ifelse(sumBikeData$workingday == 0, "no", "yes")),
                                       season = as.factor(ifelse(sumBikeData$season == 1, "winter",
                                                          ifelse(sumBikeData$season == 2, "spring",
                                                          ifelse(sumBikeData$season == 3, "summer", "fall")
                                                          ))),
                                       hours = as.factor(ifelse((sumBikeData$hr > 3) & (sumBikeData$hr < 6),
                                                               "earyl AM", 
                                                         ifelse((sumBikeData$hr >= 6) & (sumBikeData$hr < 10), 
                                                               "AM commute", 
                                                         ifelse((sumBikeData$hr >= 10) & (sumBikeData$hr < 16),
                                                               "mid day", 
                                                         ifelse((sumBikeData$hr >= 16 )& (sumBikeData$hr < 20), 
                                                               "PM commute", 
                                                         ifelse((sumBikeData$hr >= 20) & (sumBikeData$hr <= 23),
                                                               "night", "late night")
                                                        ))))),
                                       weather = as.factor(ifelse(sumBikeData$weathersit == 1, "clear", 
                                                           ifelse(sumBikeData$weathersit == 2, "mist",
                                                           ifelse(sumBikeData$weathersit == 3, "light precip", 
                                                                                               "heavy precip")
                                                          ))))
                                      
    # select new vars for analysis 
    sumBikeData <- sumBikeData %>% select(dteday, season, weather, temp, hum, windspeed, workingday, hours, cnt) %>% 
                                   rename("humidity" = hum, "rentals" = cnt, "workday" = workingday)
    head(filter(sumBikeData, hours == "late night"), n = 20)

    ##        dteday season      weather temp humidity windspeed workday      hours
    ## 1  2011-01-01 winter        clear 0.24     0.81    0.0000       0 late night
    ## 2  2011-01-01 winter        clear 0.22     0.80    0.0000       0 late night
    ## 3  2011-01-01 winter        clear 0.22     0.80    0.0000       0 late night
    ## 4  2011-01-01 winter        clear 0.24     0.75    0.0000       0 late night
    ## 5  2011-01-08 winter         mist 0.18     0.51    0.1642       0 late night
    ## 6  2011-01-08 winter         mist 0.18     0.55    0.0896       0 late night
    ## 7  2011-01-08 winter         mist 0.18     0.55    0.0000       0 late night
    ## 8  2011-01-08 winter light precip 0.18     0.55    0.1642       0 late night
    ## 9  2011-01-15 winter        clear 0.18     0.55    0.0000       0 late night
    ## 10 2011-01-15 winter         mist 0.16     0.59    0.0896       0 late night
    ## 11 2011-01-15 winter         mist 0.16     0.59    0.0896       0 late night
    ## 12 2011-01-15 winter         mist 0.16     0.59    0.0000       0 late night
    ## 13 2011-01-22 winter        clear 0.04     0.45    0.2537       0 late night
    ## 14 2011-01-22 winter         mist 0.04     0.41    0.3881       0 late night
    ## 15 2011-01-22 winter         mist 0.04     0.41    0.2537       0 late night
    ## 16 2011-01-22 winter         mist 0.04     0.41    0.2836       0 late night
    ## 17 2011-01-29 winter        clear 0.22     0.64    0.3582       0 late night
    ## 18 2011-01-29 winter        clear 0.22     0.64    0.1940       0 late night
    ## 19 2011-01-29 winter        clear 0.22     0.64    0.1642       0 late night
    ## 20 2011-01-29 winter        clear 0.20     0.64    0.1343       0 late night
    ##    rentals
    ## 1       16
    ## 2       40
    ## 3       32
    ## 4       13
    ## 5       25
    ## 6       16
    ## 7       16
    ## 8        7
    ## 9       28
    ## 10      20
    ## 11      12
    ## 12       8
    ## 13      13
    ## 14      12
    ## 15      11
    ## 16       7
    ## 17      28
    ## 18      20
    ## 19      15
    ## 20       8

Summaries for Season & Weather
------------------------------

    # Function to knit 5 number sum + ean given season
    sumSeason <- function(x, ...) {
      summ <- sumBikeData %>% filter(season == x) %>% select(9) %>% apply(2, sum)
      kable(round(summ, 2), caption = paste0("Season: ", x))
    }

    # Summaries for Winter
    sumSeason("winter")

|         |     x |
|:--------|------:|
| rentals | 65673 |

Season: winter

    # Summaries for Spring
    sumSeason("spring")

|         |      x |
|:--------|-------:|
| rentals | 138689 |

Season: spring

    # Summaries for Summer
    sumSeason("summer")

|         |      x |
|:--------|-------:|
| rentals | 150794 |

Season: summer

    # Summaries for Fall
    sumSeason("fall")

|         |      x |
|:--------|-------:|
| rentals | 122651 |

Season: fall

    sum_meanFx <- function(cat, ...) {
       if ((!is.null(cat) & (!is.character(cat) & (cat %in% sumBikeData[2])))) 
        stop("invalid input")
                                                                         
       if (cat == "season") { 
         seasonSum_Mean <- sumBikeData %>% group_by(season) %>%
                                           mutate(rental_mean = round(mean(rentals),
                                                                     digits = 2), 
                                                  rental_sum = round(sum(rentals),
                                                                    digits = 2)) %>% 
                                           select(season, rental_mean, rental_sum)
         seasonSum_Mean <- as_tibble(unique(seasonSum_Mean))
         return(seasonSum_Mean)
         
       } else if (cat == "weather") {
          weatherSum_Mean <- sumBikeData %>% group_by(weather) %>%
                                             mutate(rental_mean = round(mean(rentals),
                                                                       digits = 2), 
                                                    rental_sum = round(sum(rentals),
                                                                       digits = 2)) %>% 
                                             select(weather, rental_mean, rental_sum) 
          weatherSum_Mean <- as_tibble(unique(weatherSum_Mean))
          return(weatherSum_Mean)
           
       } else if (cat == "hours") {
          hoursSum_Mean <- sumBikeData %>% group_by(hours) %>% 
                                           mutate(rental_mean = round(mean(rentals),
                                                                     digits = 2), 
                                                  rental_sum = round(sum(rentals),
                                                                    digits = 2)) %>% 
                                           select(hours, rental_mean, rental_sum)
           hoursSum_Mean <- as_tibble(unique(hoursSum_Mean))
           return(hoursSum_Mean)
           
       } else {
           return(null)
       }
    }

    kable(sum_meanFx("season"), caption = "Rentals by Season")

| season | rental\_mean | rental\_sum |
|:-------|-------------:|------------:|
| winter |       101.66 |       65673 |
| spring |       222.26 |      138689 |
| summer |       234.88 |      150794 |
| fall   |       204.42 |      122651 |

Rentals by Season

    seasonBox <- ggplot(sumBikeData, aes(x = rentals, y = season, color = season))
    seasonBox + geom_boxplot() + labs(title = "Rentals by Season")

![](SaturdayAnalysis_files/figure-gfm/season%20statbox-1.png)<!-- -->

    kable(sum_meanFx("weather"), caption = "Rentals by Weather Condition")

| weather      | rental\_mean | rental\_sum |
|:-------------|-------------:|------------:|
| clear        |       207.57 |      347681 |
| mist         |       171.01 |      110813 |
| light precip |       102.61 |       19290 |
| heavy precip |        23.00 |          23 |

Rentals by Weather Condition

    weatherBox <- ggplot(sumBikeData, aes(x = rentals, y = weather, color = weather))
    weatherBox + geom_boxplot() + labs(title = "Rentals by Weather Condiion")

![](SaturdayAnalysis_files/figure-gfm/weather%20statbox-1.png)<!-- -->

    kable(sum_meanFx("hours"), caption = "Rentals by Hours")

| hours      | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        58.87 |       24724 |
| earyl AM   |         7.97 |        1658 |
| AM commute |        92.06 |       38664 |
| mid day    |       352.81 |      222269 |
| PM commute |       308.33 |      128884 |
| night      |       148.10 |       61608 |

Rentals by Hours

    hoursBox <- ggplot(sumBikeData, aes(x = rentals, y = hours, color = hours))
    hoursBox + geom_boxplot() + labs(title = "Rentals by Hours")

![](SaturdayAnalysis_files/figure-gfm/hours%20statbox-1.png)<!-- -->

    dayOff <- sumBikeData %>% filter(workday == 0)
    dayOffStat <- dayOff %>% group_by(hours) %>% mutate(rental_mean = round(mean(rentals),
                                                                            digits = 2), 
                                                        rental_sum = round(sum(rentals),
                                                                           digits = 2)) %>% 
                                                        select(hours, rental_mean, rental_sum)
    dayOffStat <- as_tibble(unique(dayOffStat))
    kable(dayOffStat, caption = "Rentals by Hours (Day Off")

| hours      | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        58.87 |       24724 |
| earyl AM   |         7.97 |        1658 |
| AM commute |        92.06 |       38664 |
| mid day    |       352.81 |      222269 |
| PM commute |       308.33 |      128884 |
| night      |       148.10 |       61608 |

Rentals by Hours (Day Off

    dayOffBox <- ggplot(dayOff, aes(x = rentals, y = hours, color = hours))
    dayOffBox + geom_boxplot() + labs(title = "Rentals by Hours (Day Off)")

![](SaturdayAnalysis_files/figure-gfm/hours%20statbox-2.png)<!-- -->

    dayOn <- sumBikeData %>% filter(workday == 1)
    dayOnStat <- dayOn %>% group_by(hours) %>% mutate(rental_mean = round(mean(rentals),
                                                                          digits = 2), 
                                                      rental_sum = round(sum(rentals),
                                                                         digits = 2)) %>% 
                                                      select(hours, rental_mean, rental_sum)
    dayOnStat <- as_tibble(unique(dayOnStat))
    kable(dayOnStat, caption = "Rentals by Hours (Work Day)")

| hours | rental\_mean | rental\_sum |
|:------|-------------:|------------:|

Rentals by Hours (Work Day)

    dayOnBox <- ggplot(dayOn, aes(x = rentals, y = hours, color = hours))
    dayOnBox + geom_boxplot() + labs(title = "Rentals by Hours (Work Day)")

![](SaturdayAnalysis_files/figure-gfm/hours%20statbox-3.png)<!-- -->

    # 
    quantStats <- sumBikeData %>%  select(c(4, 5, 6, 9)) %>% apply(2, summary)
    kable(round((quantStats), 2), caption = "Summary: Quantitative Varibles")

|         | temp | humidity | windspeed | rentals |
|:--------|-----:|---------:|----------:|--------:|
| Min.    | 0.02 |     0.12 |      0.00 |    1.00 |
| 1st Qu. | 0.32 |     0.45 |      0.10 |   42.00 |
| Median  | 0.46 |     0.62 |      0.19 |  129.00 |
| Mean    | 0.48 |     0.62 |      0.20 |  190.21 |
| 3rd Qu. | 0.64 |     0.79 |      0.28 |  300.00 |
| Max.    | 1.00 |     1.00 |      0.84 |  783.00 |

Summary: Quantitative Varibles

    rentalCor <- select(sumBikeData, c(temp, humidity, windspeed, rentals))
    rentalCor <- as.data.frame(round(cor(rentalCor), digits = 2))
    kable(select(rentalCor, rentals), caption = "Rental Correlations")

|           | rentals |
|:----------|--------:|
| temp      |    0.51 |
| humidity  |   -0.43 |
| windspeed |    0.10 |
| rentals   |    1.00 |

Rental Correlations

    # Base plot aesthetic with Total Points on x axis

    humPoint_season <- ggplot(sumBikeData, aes(x = humidity, y = rentals, color = season))

    # Avg PM point plot
    humPoint_season + geom_point() + geom_smooth(aes(group = season, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Humidity") +
                facet_wrap(~ season)

![](SaturdayAnalysis_files/figure-gfm/humidity%20plots-1.png)<!-- -->

    humPoint_weather <- ggplot(sumBikeData, aes(x = humidity, y = rentals, color = weather))

    humPoint_weather + geom_point() + geom_smooth(aes(group = weather, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Humidity") +
                facet_wrap(~ weather)

![](SaturdayAnalysis_files/figure-gfm/humidity%20plots-2.png)<!-- -->

    humPoint_hours <- ggplot(sumBikeData, aes(x = humidity, y = rentals, color = hours))

    humPoint_hours + geom_point() + geom_smooth(aes(group = hours, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Hours Rentals by Humidity") +
                facet_wrap(~ hours)

![](SaturdayAnalysis_files/figure-gfm/humidity%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    windPoint_season <- ggplot(sumBikeData, aes(x = windspeed, y = rentals, color = season))

    # Avg PM point plot
    windPoint_season + geom_point() + geom_smooth(aes(group = season, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Wind Speed") +
                facet_wrap(~ season)

![](SaturdayAnalysis_files/figure-gfm/wind%20plots-1.png)<!-- -->

    windPoint_weather <- ggplot(sumBikeData, aes(x = windspeed, y = rentals, color = weather))

    windPoint_weather + geom_point() + geom_smooth(aes(group = weather, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Wind Speed") +
                facet_wrap(~ weather)

![](SaturdayAnalysis_files/figure-gfm/wind%20plots-2.png)<!-- -->

    windPoint_hours <- ggplot(sumBikeData, aes(x = windspeed, y = rentals, color = hours))

    windPoint_hours + geom_point() + geom_smooth(aes(group = hours, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Hours Rentals by Wind Speed") +
                facet_wrap(~ hours)

![](SaturdayAnalysis_files/figure-gfm/wind%20plots-3.png)<!-- -->
