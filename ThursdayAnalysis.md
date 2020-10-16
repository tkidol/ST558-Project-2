Bicycle Analysis
================
Todd Idol
10/10/2020

-   [Set Parameters & Knit](#set-parameters-knit)
-   [Introduction](#introduction)
-   [Data](#data)
    -   [Summaries for Season & Weather](#summaries-for-season-weather)
-   [Modeling](#modeling)
    -   [Tree Based Model
        (non-ensemble)](#tree-based-model-non-ensemble)
    -   [Boosted Tree Model](#boosted-tree-model)

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

    # split data 70% train & 80% test
    set.seed(100)
    trainIndex <- createDataPartition(bikeData$cnt, p = .7, list = FALSE)

    bikeTrain <- bikeData[trainIndex, ]
    bikeTest <- bikeData[-trainIndex, ]

### Summary Data

    sumBikeTrain <- bikeTrain %>% select(dteday, season, hr, workingday, weathersit, temp, hum, windspeed, cnt)

    # create factors for season, timeofday and weather
    sumBikeTrain <- mutate(sumBikeTrain, workday = as.factor(ifelse(sumBikeTrain$workingday == 0, "no", "yes")),
                           
                                         season = as.factor(ifelse(sumBikeTrain$season == 1, "winter",
                                                            ifelse(sumBikeTrain$season == 2, "spring",
                                                            ifelse(sumBikeTrain$season == 3, "summer", "fall")
                                                           ))),
                           
                                         hour = as.factor(ifelse((sumBikeTrain$hr >= 6) & (sumBikeTrain$hr < 10), 
                                                                "AM commute", 
                                                          ifelse((sumBikeTrain$hr >= 10) & (sumBikeTrain$hr < 16),
                                                               "mid day", 
                                                          ifelse((sumBikeTrain$hr >= 16 )& (sumBikeTrain$hr < 20), 
                                                               "PM commute", 
                                                          ifelse((sumBikeTrain$hr >= 20) & (sumBikeTrain$hr <= 23),
                                                               "night", "late night")
                                                         )))),
                           
                                         weather = as.factor(ifelse(sumBikeTrain$weathersit == 1, "clear", 
                                                             ifelse(sumBikeTrain$weathersit == 2, "mist",
                                                             ifelse(sumBikeTrain$weathersit == 3, "light precip", 
                                                                                               "heavy precip")
                                                           ))))
                                      
    # select new vars for analysis 
    sumBikeTrain <- sumBikeTrain %>% select(dteday, season, weather, temp, hum, windspeed, workingday, hour, cnt) %>% 
                                     rename("humidity" = hum, "rentals" = cnt, "workday" = workingday)
    head(filter(sumBikeTrain, hour == "late night"))

    ##       dteday season weather temp humidity windspeed workday       hour rentals
    ## 1 2011-01-06 winter   clear 0.18     0.55    0.0000       1 late night      11
    ## 2 2011-01-06 winter   clear 0.16     0.64    0.0000       1 late night       4
    ## 3 2011-01-06 winter   clear 0.16     0.64    0.0000       1 late night       2
    ## 4 2011-01-06 winter    mist 0.16     0.64    0.0896       1 late night       1
    ## 5 2011-01-13 winter   clear 0.14     0.50    0.2836       1 late night       2
    ## 6 2011-01-13 winter   clear 0.14     0.50    0.3284       1 late night       3

### Modeling Data

    bikeTrain1 <- sumBikeTrain %>% select(c(season, hour, weather, temp, humidity, rentals))
                      
    str(bikeTrain1)

    ## 'data.frame':    1731 obs. of  6 variables:
    ##  $ season  : Factor w/ 4 levels "fall","spring",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ hour    : Factor w/ 5 levels "AM commute","late night",..: 2 2 2 2 1 1 1 1 3 3 ...
    ##  $ weather : Factor w/ 3 levels "clear","light precip",..: 1 1 1 3 3 3 1 3 1 1 ...
    ##  $ temp    : num  0.18 0.16 0.16 0.16 0.14 0.16 0.16 0.18 0.22 0.26 ...
    ##  $ humidity: num  0.55 0.64 0.64 0.64 0.63 0.59 0.59 0.51 0.44 0.35 ...
    ##  $ rentals : int  11 4 2 1 36 95 219 122 59 67 ...

    bikeTest1 <- bikeTest %>% select(c(season, hr, weathersit, temp, hum, cnt)) %>%
                              mutate(season = as.factor(ifelse(bikeTest$season == 1, "winter",
                                                        ifelse(bikeTest$season == 2, "spring",
                                                        ifelse(bikeTest$season == 3, "summer", "fall")
                                                       ))),
                           
                              hour = as.factor(ifelse((bikeTest$hr >= 6) & (bikeTest$hr < 10), "AM commute", 
                                               ifelse((bikeTest$hr >= 10) & (bikeTest$hr < 16), "mid day", 
                                               ifelse((bikeTest$hr >= 16 )& (bikeTest$hr < 20), "PM commute", 
                                               ifelse((bikeTest$hr >= 20) & (bikeTest$hr <= 23), "night", "late night")
                                              )))),
                           
                             weather = as.factor(ifelse(bikeTest$weathersit == 1, "clear", 
                                                 ifelse(bikeTest$weathersit == 2, "mist",
                                                 ifelse(bikeTest$weathersit == 3, "light precip", "heavy precip")
                                                )))) %>%
                                rename("humidity" = hum, "rentals" = cnt)
    bikeTest1 <- bikeTest1 %>% select(c(season, hour, weather, temp, humidity, rentals))        

    str(bikeTest1)

    ## 'data.frame':    740 obs. of  6 variables:
    ##  $ season  : Factor w/ 4 levels "fall","spring",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ hour    : Factor w/ 5 levels "AM commute","late night",..: 2 3 3 3 3 5 4 4 2 2 ...
    ##  $ weather : Factor w/ 3 levels "clear","light precip",..: 3 1 1 1 1 1 1 3 1 1 ...
    ##  $ temp    : num  0.14 0.2 0.26 0.28 0.28 0.22 0.2 0.22 0.14 0.14 ...
    ##  $ humidity: num  0.69 0.47 0.35 0.36 0.36 0.51 0.51 0.55 0.59 0.5 ...
    ##  $ rentals : int  4 45 84 70 62 172 69 48 7 2 ...

Summaries for Season & Weather
------------------------------

    sum_meanFx <- function(cat, ...) {
       if ((!is.null(cat) & (!is.character(cat)) & ((cat %in% sumBikeTrain[2]) |
                                                    (cat %in% sumBikeTrain[3]) |
                                                    (cat %in% sumBikeTrain[8]))))
        stop("invalid input")
                                                                         
       if (cat == "season") { 
         seasonSum_Mean <- sumBikeTrain %>% group_by(season) %>%
                                           mutate(rental_mean = round(mean(rentals),
                                                                     digits = 2), 
                                                  rental_sum = round(sum(rentals),
                                                                    digits = 2)) %>% 
                                           select(season, rental_mean, rental_sum)
         seasonSum_Mean <- as_tibble(unique(seasonSum_Mean))
         return(seasonSum_Mean)
         
       } else if (cat == "weather") {
          weatherSum_Mean <- sumBikeTrain %>% group_by(weather) %>%
                                             mutate(rental_mean = round(mean(rentals),
                                                                       digits = 2), 
                                                    rental_sum = round(sum(rentals),
                                                                       digits = 2)) %>% 
                                             select(weather, rental_mean, rental_sum) 
          weatherSum_Mean <- as_tibble(unique(weatherSum_Mean))
          return(weatherSum_Mean)
           
       } else if (cat == "hour") {
          hourSum_Mean <- sumBikeTrain %>% group_by(hour) %>% 
                                           mutate(rental_mean = round(mean(rentals),
                                                                     digits = 2), 
                                                  rental_sum = round(sum(rentals),
                                                                    digits = 2)) %>% 
                                           select(hour, rental_mean, rental_sum)
           hourSum_Mean <- as_tibble(unique(hourSum_Mean))
           return(hourSum_Mean)
           
       } else {
           return(null)
       }
    }

    kable(sum_meanFx("season"), caption = "Rentals by Season")

| season | rental\_mean | rental\_sum |
|:-------|-------------:|------------:|
| winter |       121.17 |       47255 |
| spring |       217.46 |       96118 |
| summer |       235.98 |      113979 |
| fall   |       200.19 |       83281 |

Rentals by Season

    seasonBox <- ggplot(sumBikeTrain, aes(x = rentals, y = season, color = season))
    seasonBox + geom_boxplot() + labs(title = "Rentals by Season")

![](ThursdayAnalysis_files/figure-gfm/season%20statbox-1.png)<!-- -->

    kable(sum_meanFx("weather"), caption = "Rentals by Weather Condition")

| weather      | rental\_mean | rental\_sum |
|:-------------|-------------:|------------:|
| clear        |       213.23 |      244580 |
| mist         |       183.52 |       84050 |
| light precip |        95.26 |       12003 |

Rentals by Weather Condition

    weatherBox <- ggplot(sumBikeTrain, aes(x = rentals, y = weather, color = weather))
    weatherBox + geom_boxplot() + labs(title = "Rentals by Weather Condiion")

![](ThursdayAnalysis_files/figure-gfm/weather%20statbox-1.png)<!-- -->

    kable(sum_meanFx("hour"), caption = "Rentals by Hour")

| hour       | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        17.55 |        7477 |
| AM commute |       279.77 |       84769 |
| mid day    |       179.16 |       76323 |
| PM commute |       423.08 |      120155 |
| night      |       177.77 |       51909 |

Rentals by Hour

    hourBox <- ggplot(sumBikeTrain, aes(x = rentals, y = hour, color = hour))
    hourBox + geom_boxplot() + labs(title = "Rentals by Hour")

![](ThursdayAnalysis_files/figure-gfm/hour%20statbox-1.png)<!-- -->

    dayOff <- sumBikeTrain %>% filter(workday == 0)
    dayOffStat <- dayOff %>% group_by(hour) %>% mutate(rental_mean = round(mean(rentals),
                                                                            digits = 2), 
                                                        rental_sum = round(sum(rentals),
                                                                           digits = 2)) %>% 
                                                        select(hour, rental_mean, rental_sum)
    dayOffStat <- as_tibble(unique(dayOffStat))
    kable(dayOffStat, caption = "Rentals by Hour (Day Off")

| hour       | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        19.22 |         173 |
| AM commute |        54.43 |         381 |
| mid day    |       186.78 |        1681 |
| PM commute |        61.17 |         367 |
| night      |        27.50 |         110 |

Rentals by Hour (Day Off

    dayOffBox <- ggplot(dayOff, aes(x = rentals, y = hour, color = hour))
    dayOffBox + geom_boxplot() + labs(title = "Rentals by Hour (Day Off)")

![](ThursdayAnalysis_files/figure-gfm/hour%20statbox-2.png)<!-- -->

    dayOn <- sumBikeTrain %>% filter(workday == 1)
    dayOnStat <- dayOn %>% group_by(hour) %>% mutate(rental_mean = round(mean(rentals),
                                                                          digits = 2), 
                                                      rental_sum = round(sum(rentals),
                                                                         digits = 2)) %>% 
                                                      select(hour, rental_mean, rental_sum)
    dayOnStat <- as_tibble(unique(dayOnStat))
    kable(dayOnStat, caption = "Rentals by Time of Day (Work Day)")

| hour       | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        17.52 |        7304 |
| AM commute |       285.09 |       84388 |
| mid day    |       179.00 |       74642 |
| PM commute |       430.89 |      119788 |
| night      |       179.86 |       51799 |

Rentals by Time of Day (Work Day)

    dayOnBox <- ggplot(dayOn, aes(x = rentals, y = hour, color = hour))
    dayOnBox + geom_boxplot() + labs(title = "Rentals by Time of Day (Work Day)")

![](ThursdayAnalysis_files/figure-gfm/hour%20statbox-3.png)<!-- -->

    # 
    quantStats <- sumBikeTrain %>% select(c(4, 5, 6, 9)) %>% apply(2, summary)

    rentalCor <- select(sumBikeTrain, c(temp, humidity, windspeed, rentals))
    rentalCor <- as.data.frame(round(cor(rentalCor), digits = 2))


    kable(round((quantStats), 2), caption = "Summary: Quantitative Varibles")

|         | temp | humidity | windspeed | rentals |
|:--------|-----:|---------:|----------:|--------:|
| Min.    | 0.12 |     0.00 |      0.00 |    1.00 |
| 1st Qu. | 0.34 |     0.47 |      0.10 |   42.00 |
| Median  | 0.52 |     0.61 |      0.19 |  154.00 |
| Mean    | 0.51 |     0.61 |      0.19 |  196.78 |
| 3rd Qu. | 0.66 |     0.77 |      0.25 |  280.00 |
| Max.    | 0.94 |     1.00 |      0.66 |  941.00 |

Summary: Quantitative Varibles

    kable(rentalCor, caption = "Rental Correlations")

|           |  temp | humidity | windspeed | rentals |
|:----------|------:|---------:|----------:|--------:|
| temp      |  1.00 |    -0.05 |      0.01 |    0.34 |
| humidity  | -0.05 |     1.00 |     -0.27 |   -0.26 |
| windspeed |  0.01 |    -0.27 |      1.00 |    0.12 |
| rentals   |  0.34 |    -0.26 |      0.12 |    1.00 |

Rental Correlations

    # Base plot aesthetic with Total Points on x axis

    tempPoint_season <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = season))

    # Avg PM point plot
    tempPoint_season + geom_point() + geom_smooth(aes(group = season,), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Temperature") +
                facet_wrap(~ season)

![](ThursdayAnalysis_files/figure-gfm/temp%20plots-1.png)<!-- -->

    tempPoint_weather <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = weather))

    tempPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Temperature") +
                facet_wrap(~ weather)

![](ThursdayAnalysis_files/figure-gfm/temp%20plots-2.png)<!-- -->

    tempPoint_hour <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = hour))

    tempPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Temperature") +
                facet_wrap(~ hour)

![](ThursdayAnalysis_files/figure-gfm/temp%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    humPoint_season <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = season))

    # Avg PM point plot
    humPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Humidity") +
                facet_wrap(~ season)

![](ThursdayAnalysis_files/figure-gfm/humidity%20plots-1.png)<!-- -->

    humPoint_weather <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = weather))

    humPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Humidity") +
                facet_wrap(~ weather)

![](ThursdayAnalysis_files/figure-gfm/humidity%20plots-2.png)<!-- -->

    humPoint_hour <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = hour))

    humPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Humidity") +
                facet_wrap(~ hour)

![](ThursdayAnalysis_files/figure-gfm/humidity%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    windPoint_season <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = season))

    # Avg PM point plot
    windPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Wind Speed") +
                facet_wrap(~ season)

![](ThursdayAnalysis_files/figure-gfm/wind%20plots-1.png)<!-- -->

    windPoint_weather <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = weather))

    windPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Wind Speed") +
                facet_wrap(~ weather)

![](ThursdayAnalysis_files/figure-gfm/wind%20plots-2.png)<!-- -->

    windPoint_hour <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = hour))

    windPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Wind Speed") +
                facet_wrap(~ hour)

![](ThursdayAnalysis_files/figure-gfm/wind%20plots-3.png)<!-- -->

Modeling
========

words

### Post Summary Train & Test Data

Tree Based Model (non-ensemble)
-------------------------------

words

    # train all predictors

    rfitAll <- train(rentals ~ ., data = bikeTrain1,
                      method = "rpart", preProcess = 
                      c("center", "scale"), trControl = 
                      trainControl(method = "LOOCV"))

    plot(varImp(rfitAll))

![](ThursdayAnalysis_files/figure-gfm/rtree%20all-1.png)<!-- -->

    round(rfitAll$results[, c(1,2)], digits = 2)

    ##     cp   RMSE
    ## 1 0.07 145.27
    ## 2 0.17 173.86
    ## 3 0.30 191.29

    rfitTop3 <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", preProcess = 
                             c("center", "scale"), trControl = 
                             trainControl(method = "LOOCV"))
                             
    kable(round(rfitTop3$results[, c(1,2)], digits = 2))

|   cp |   RMSE |
|-----:|-------:|
| 0.07 | 145.27 |
| 0.17 | 173.86 |
| 0.30 | 191.29 |

    tune <- c(0, .01, .015, .02)
     
    # Train system tuned CP
    ufitTop3X<- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", preProcess = 
                             c("center", "scale"), trControl = 
                             trainControl(method = "LOOCV"),
                             tuneGrid = expand.grid(cp = tune))

    kable(round(ufitTop3X$results[, c(1,2)], digits = 2))

|   cp |   RMSE |
|-----:|-------:|
| 0.00 | 121.50 |
| 0.01 | 122.23 |
| 0.02 | 127.52 |
| 0.02 | 125.61 |

    ufitFinal <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", 
                             preProcess = c("center", "scale"),
                             trControl = trainControl(method = "LOOCV"), 
                             tuneGrid = expand.grid(cp = .02))

    plot(ufitFinal$finalModel)
    text(ufitFinal$finalModel, pretty = 1, cex = .8)

![](ThursdayAnalysis_files/figure-gfm/rtree%20final%20model-1.png)<!-- -->

    ufitPred <- predict(ufitFinal, newdata = bikeTest1)
    ufitPred_results <- as_tibble(postResample(ufitPred, bikeTest1$rentals))
    ufitPred_results$value <- round(ufitPred_results$value, digits = 2)
    setattr(ufitPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))
    kable(ufitPred_results, caption = "Regression Tree Prediction Results")

|          |  value |
|:---------|-------:|
| RMSE     | 125.93 |
| Rsquared |   0.55 |
| MAE      |  84.60 |

Regression Tree Prediction Results

Boosted Tree Model
------------------

    # Train program defined tuning parameters
    boostedFit <- train(rentals ~ ., data = bikeTrain1,
                       method = "gbm", preProcess =
                       c("center", "scale"), trControl = 
                       trainControl(method = "cv", 
                       number = 50), verbose = FALSE)
                      
    kable(round(boostedFit$results, digits = 2))

|     | shrinkage | interaction.depth | n.minobsinnode | n.trees |   RMSE | Rsquared |   MAE | RMSESD | RsquaredSD | MAESD |
|:----|----------:|------------------:|---------------:|--------:|-------:|---------:|------:|-------:|-----------:|------:|
| 1   |       0.1 |                 1 |             10 |      50 | 127.52 |     0.56 | 89.37 |  22.23 |       0.15 | 12.65 |
| 4   |       0.1 |                 2 |             10 |      50 | 115.75 |     0.63 | 79.22 |  20.19 |       0.14 | 11.64 |
| 7   |       0.1 |                 3 |             10 |      50 | 111.28 |     0.65 | 75.36 |  19.24 |       0.13 | 11.38 |
| 2   |       0.1 |                 1 |             10 |     100 | 121.85 |     0.59 | 86.16 |  21.32 |       0.14 | 12.20 |
| 5   |       0.1 |                 2 |             10 |     100 | 111.62 |     0.65 | 76.44 |  18.80 |       0.12 | 11.05 |
| 8   |       0.1 |                 3 |             10 |     100 | 108.74 |     0.67 | 73.28 |  18.65 |       0.12 | 11.42 |
| 3   |       0.1 |                 1 |             10 |     150 | 119.99 |     0.60 | 85.24 |  20.82 |       0.13 | 12.37 |
| 6   |       0.1 |                 2 |             10 |     150 | 110.26 |     0.66 | 75.43 |  18.47 |       0.12 | 11.21 |
| 9   |       0.1 |                 3 |             10 |     150 | 108.26 |     0.67 | 72.85 |  18.33 |       0.11 | 11.36 |

    # Tuned parameters
    gbm_grid <- expand.grid(n.trees = c(200, 250),
                            interaction.depth = (4),
                            shrinkage = c(.1, .2), 
                            n.minobsinnode = 10)

    boostedFitX <- train(rentals ~ ., data = bikeTrain1,
                       method = "gbm", preProcess =
                       c("center", "scale"), trControl = 
                       trainControl(method = "cv", 
                       number = 50), verbose = FALSE,
                       tuneGrid = expand.grid(gbm_grid))

    kable(round(boostedFitX$results, digits = 2))

|     | shrinkage | interaction.depth | n.minobsinnode | n.trees |   RMSE | Rsquared |   MAE | RMSESD | RsquaredSD | MAESD |
|:----|----------:|------------------:|---------------:|--------:|-------:|---------:|------:|-------:|-----------:|------:|
| 1   |       0.1 |                 4 |             10 |     200 | 107.90 |     0.68 | 72.58 |  21.30 |       0.10 | 12.52 |
| 3   |       0.2 |                 4 |             10 |     200 | 109.15 |     0.68 | 74.31 |  21.73 |       0.10 | 12.34 |
| 2   |       0.1 |                 4 |             10 |     250 | 107.87 |     0.68 | 72.43 |  21.60 |       0.10 | 12.57 |
| 4   |       0.2 |                 4 |             10 |     250 | 109.26 |     0.68 | 74.25 |  21.97 |       0.11 | 12.53 |

    gbm_grid1 <- expand.grid(n.trees = 150,
                            interaction.depth = 3,
                            shrinkage = .1, 
                            n.minobsinnode = 10)

    boostedFinal <- train(rentals ~ ., data = bikeTrain1,
                       method = "gbm", preProcess =
                       c("center", "scale"), trControl = 
                       trainControl(method = "cv", 
                       number = 50), verbose = FALSE,
                       tuneGrid = expand.grid(gbm_grid1))

    boostedFinal$finalModel

    ## A gradient boosted model with gaussian loss function.
    ## 150 iterations were performed.
    ## There were 11 predictors of which 11 had non-zero influence.

    boostedPred <- predict(boostedFinal, newdata = bikeTest1)
    boostedPred_results <- as_tibble(postResample(boostedPred, bikeTest1$rentals))
    boostedPred_results$value <- round(boostedPred_results$value, digits = 2)
    setattr(boostedPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))
    kable(boostedPred_results, caption = "Boosted Tree Prediction Results")

|          |  value |
|:---------|-------:|
| RMSE     | 111.82 |
| Rsquared |   0.64 |
| MAE      |  74.71 |

Boosted Tree Prediction Results
