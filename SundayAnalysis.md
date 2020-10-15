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

    ## [1] 3239

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

    ##        dteday season weather temp humidity windspeed workday      hours rentals
    ## 1  2011-01-02 winter    mist 0.46     0.88    0.2985       0 late night      17
    ## 2  2011-01-02 winter    mist 0.44     0.94    0.2537       0 late night      17
    ## 3  2011-01-02 winter    mist 0.42     1.00    0.2836       0 late night       9
    ## 4  2011-01-02 winter    mist 0.46     0.94    0.1940       0 late night       6
    ## 5  2011-01-09 winter   clear 0.10     0.42    0.3881       0 late night      25
    ## 6  2011-01-09 winter   clear 0.10     0.42    0.4627       0 late night      12
    ## 7  2011-01-09 winter   clear 0.10     0.46    0.4627       0 late night      11
    ## 8  2011-01-09 winter   clear 0.10     0.46    0.4179       0 late night       4
    ## 9  2011-01-16 winter   clear 0.26     0.56    0.0000       0 late night      39
    ## 10 2011-01-16 winter   clear 0.26     0.56    0.1343       0 late night      23
    ## 11 2011-01-16 winter   clear 0.26     0.56    0.0896       0 late night      16
    ## 12 2011-01-16 winter   clear 0.22     0.69    0.0000       0 late night      15
    ## 13 2011-01-23 winter   clear 0.04     0.57    0.1045       0 late night      22
    ## 14 2011-01-23 winter   clear 0.04     0.57    0.1045       0 late night      13
    ## 15 2011-01-23 winter   clear 0.02     0.62    0.1343       0 late night      18
    ## 16 2011-01-23 winter   clear 0.02     0.62    0.1343       0 late night       5
    ## 17 2011-01-30 winter   clear 0.16     0.80    0.1045       0 late night      33
    ## 18 2011-01-30 winter   clear 0.14     0.80    0.0000       0 late night      29
    ## 19 2011-01-30 winter   clear 0.16     0.80    0.0000       0 late night      11
    ## 20 2011-01-30 winter   clear 0.14     0.93    0.0000       0 late night       8

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
| rentals | 60195 |

Season: winter

    # Summaries for Spring
    sumSeason("spring")

|         |      x |
|:--------|-------:|
| rentals | 129650 |

Season: spring

    # Summaries for Summer
    sumSeason("summer")

|         |      x |
|:--------|-------:|
| rentals | 138431 |

Season: summer

    # Summaries for Fall
    sumSeason("fall")

|         |      x |
|:--------|-------:|
| rentals | 115751 |

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
| winter |        94.35 |       60195 |
| spring |       208.11 |      129650 |
| summer |       224.36 |      138431 |
| fall   |       185.50 |      115751 |

Rentals by Season

    seasonBox <- ggplot(sumBikeData, aes(x = rentals, y = season, color = season))
    seasonBox + geom_boxplot() + labs(title = "Rentals by Season")

![](SundayAnalysis_files/figure-gfm/season%20statbox-1.png)<!-- -->

    kable(sum_meanFx("weather"), caption = "Rentals by Weather Condition")

| weather      | rental\_mean | rental\_sum |
|:-------------|-------------:|------------:|
| mist         |       142.18 |       80757 |
| light precip |       116.43 |       19677 |
| clear        |       194.67 |      343593 |

Rentals by Weather Condition

    weatherBox <- ggplot(sumBikeData, aes(x = rentals, y = weather, color = weather))
    weatherBox + geom_boxplot() + labs(title = "Rentals by Weather Condiion")

![](SundayAnalysis_files/figure-gfm/weather%20statbox-1.png)<!-- -->

    kable(sum_meanFx("hours"), caption = "Rentals by Hours")

| hours      | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        66.07 |       27353 |
| earyl AM   |         9.04 |        1807 |
| AM commute |        72.28 |       30211 |
| mid day    |       339.90 |      214139 |
| PM commute |       293.07 |      123089 |
| night      |       112.92 |       47428 |

Rentals by Hours

    hoursBox <- ggplot(sumBikeData, aes(x = rentals, y = hours, color = hours))
    hoursBox + geom_boxplot() + labs(title = "Rentals by Hours")

![](SundayAnalysis_files/figure-gfm/hours%20statbox-1.png)<!-- -->

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
| late night |        66.07 |       27353 |
| earyl AM   |         9.04 |        1807 |
| AM commute |        72.28 |       30211 |
| mid day    |       339.90 |      214139 |
| PM commute |       293.07 |      123089 |
| night      |       112.92 |       47428 |

Rentals by Hours (Day Off

    dayOffBox <- ggplot(dayOff, aes(x = rentals, y = hours, color = hours))
    dayOffBox + geom_boxplot() + labs(title = "Rentals by Hours (Day Off)")

![](SundayAnalysis_files/figure-gfm/hours%20statbox-2.png)<!-- -->

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

![](SundayAnalysis_files/figure-gfm/hours%20statbox-3.png)<!-- -->

    # 
    quantStats <- sumBikeData %>%  select(c(4, 5, 6, 9)) %>% apply(2, summary)
    kable(round((quantStats), 2), caption = "Summary: Quantitative Varibles")

|         | temp | humidity | windspeed | rentals |
|:--------|-----:|---------:|----------:|--------:|
| Min.    | 0.02 |     0.16 |      0.00 |    1.00 |
| 1st Qu. | 0.34 |     0.48 |      0.10 |   40.00 |
| Median  | 0.48 |     0.63 |      0.16 |  116.00 |
| Mean    | 0.48 |     0.63 |      0.19 |  177.47 |
| 3rd Qu. | 0.64 |     0.78 |      0.25 |  288.00 |
| Max.    | 0.96 |     1.00 |      0.85 |  776.00 |

Summary: Quantitative Varibles

    rentalCor <- select(sumBikeData, c(temp, humidity, windspeed, rentals))
    rentalCor <- as.data.frame(round(cor(rentalCor), digits = 2))
    kable(select(rentalCor, rentals), caption = "Rental Correlations")

|           | rentals |
|:----------|--------:|
| temp      |    0.51 |
| humidity  |   -0.40 |
| windspeed |    0.12 |
| rentals   |    1.00 |

Rental Correlations

    # Base plot aesthetic with Total Points on x axis

    humPoint_season <- ggplot(sumBikeData, aes(x = humidity, y = rentals, color = season))

    # Avg PM point plot
    humPoint_season + geom_point() + geom_smooth(aes(group = season, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Humidity") +
                facet_wrap(~ season)

![](SundayAnalysis_files/figure-gfm/humidity%20plots-1.png)<!-- -->

    humPoint_weather <- ggplot(sumBikeData, aes(x = humidity, y = rentals, color = weather))

    humPoint_weather + geom_point() + geom_smooth(aes(group = weather, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Humidity") +
                facet_wrap(~ weather)

![](SundayAnalysis_files/figure-gfm/humidity%20plots-2.png)<!-- -->

    humPoint_hours <- ggplot(sumBikeData, aes(x = humidity, y = rentals, color = hours))

    humPoint_hours + geom_point() + geom_smooth(aes(group = hours, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Hours Rentals by Humidity") +
                facet_wrap(~ hours)

![](SundayAnalysis_files/figure-gfm/humidity%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    windPoint_season <- ggplot(sumBikeData, aes(x = windspeed, y = rentals, color = season))

    # Avg PM point plot
    windPoint_season + geom_point() + geom_smooth(aes(group = season, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Wind Speed") +
                facet_wrap(~ season)

![](SundayAnalysis_files/figure-gfm/wind%20plots-1.png)<!-- -->

    windPoint_weather <- ggplot(sumBikeData, aes(x = windspeed, y = rentals, color = weather))

    windPoint_weather + geom_point() + geom_smooth(aes(group = weather, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Wind Speed") +
                facet_wrap(~ weather)

![](SundayAnalysis_files/figure-gfm/wind%20plots-2.png)<!-- -->

    windPoint_hours <- ggplot(sumBikeData, aes(x = windspeed, y = rentals, color = hours))

    windPoint_hours + geom_point() + geom_smooth(aes(group = hours, color = "white"), method = lm) + 
                scale_fill_continuous() + labs(title =  "Hours Rentals by Wind Speed") +
                facet_wrap(~ hours)

![](SundayAnalysis_files/figure-gfm/wind%20plots-3.png)<!-- -->
