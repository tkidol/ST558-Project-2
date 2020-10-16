Bicycle Analysis
================
Todd Idol
10/10/2020

-   [Set Parameters & Knit](#set-parameters-knit)
-   [Introduction](#introduction)
-   [Data](#data)
-   [Summarizations](#summarizations)
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

Summarizations
==============

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
    ## 1 2011-01-04 winter   clear 0.16     0.55    0.1045       1 late night       5
    ## 2 2011-01-04 winter   clear 0.16     0.59    0.1045       1 late night       2
    ## 3 2011-01-04 winter   clear 0.14     0.63    0.1343       1 late night       1
    ## 4 2011-01-04 winter   clear 0.14     0.63    0.0896       1 late night       2
    ## 5 2011-01-11 winter   clear 0.14     0.59    0.1045       1 late night      12
    ## 6 2011-01-11 winter    mist 0.16     0.55    0.1940       1 late night       3

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
| winter |       124.08 |       48762 |
| spring |       198.65 |       90386 |
| summer |       241.06 |      109200 |
| fall   |       190.09 |       79456 |

Rentals by Season

    seasonBox <- ggplot(sumBikeTrain, aes(x = rentals, y = season, color = season))
    seasonBox + geom_boxplot() + labs(title = "Rentals by Season")

![](TuesdayAnalysis_files/figure-gfm/season%20statbox-1.png)<!-- -->

    kable(sum_meanFx("weather"), caption = "Rentals by Weather Condition")

| weather      | rental\_mean | rental\_sum |
|:-------------|-------------:|------------:|
| clear        |       214.76 |      228500 |
| mist         |       163.78 |       77631 |
| light precip |       119.74 |       21673 |

Rentals by Weather Condition

    weatherBox <- ggplot(sumBikeTrain, aes(x = rentals, y = weather, color = weather))
    weatherBox + geom_boxplot() + labs(title = "Rentals by Weather Condiion")

![](TuesdayAnalysis_files/figure-gfm/weather%20statbox-1.png)<!-- -->

    kable(sum_meanFx("hour"), caption = "Rentals by Hour")

| hour       | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        12.99 |        5352 |
| AM commute |       284.28 |       79031 |
| mid day    |       166.06 |       76555 |
| PM commute |       417.71 |      120718 |
| night      |       165.41 |       46148 |

Rentals by Hour

    hourBox <- ggplot(sumBikeTrain, aes(x = rentals, y = hour, color = hour))
    hourBox + geom_boxplot() + labs(title = "Rentals by Hour")

![](TuesdayAnalysis_files/figure-gfm/hour%20statbox-1.png)<!-- -->

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
| late night |         9.00 |          27 |
| AM commute |        13.00 |          52 |
| mid day    |        93.00 |         465 |
| PM commute |        57.33 |         172 |
| night      |        26.50 |          53 |

Rentals by Hour (Day Off

    dayOffBox <- ggplot(dayOff, aes(x = rentals, y = hour, color = hour))
    dayOffBox + geom_boxplot() + labs(title = "Rentals by Hour (Day Off)")

![](TuesdayAnalysis_files/figure-gfm/hour%20statbox-2.png)<!-- -->

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
| late night |        13.02 |        5325 |
| AM commute |       288.24 |       78979 |
| mid day    |       166.86 |       76090 |
| PM commute |       421.49 |      120546 |
| night      |       166.41 |       46095 |

Rentals by Time of Day (Work Day)

    dayOnBox <- ggplot(dayOn, aes(x = rentals, y = hour, color = hour))
    dayOnBox + geom_boxplot() + labs(title = "Rentals by Time of Day (Work Day)")

![](TuesdayAnalysis_files/figure-gfm/hour%20statbox-3.png)<!-- -->

    # 
    quantStats <- sumBikeTrain %>% select(c(4, 5, 6, 9)) %>% apply(2, summary)

    rentalCor <- select(sumBikeTrain, c(temp, humidity, windspeed, rentals))
    rentalCor <- as.data.frame(round(cor(rentalCor), digits = 2))


    kable(round((quantStats), 2), caption = "Summary: Quantitative Varibles")

|         | temp | humidity | windspeed | rentals |
|:--------|-----:|---------:|----------:|--------:|
| Min.    | 0.10 |     0.16 |      0.00 |    1.00 |
| 1st Qu. | 0.36 |     0.49 |      0.10 |   35.00 |
| Median  | 0.52 |     0.65 |      0.19 |  147.00 |
| Mean    | 0.51 |     0.64 |      0.19 |  190.69 |
| 3rd Qu. | 0.66 |     0.81 |      0.28 |  275.50 |
| Max.    | 0.94 |     1.00 |      0.78 |  970.00 |

Summary: Quantitative Varibles

    kable(rentalCor, caption = "Rental Correlations")

|           |  temp | humidity | windspeed | rentals |
|:----------|------:|---------:|----------:|--------:|
| temp      |  1.00 |    -0.10 |      0.03 |    0.36 |
| humidity  | -0.10 |     1.00 |     -0.19 |   -0.29 |
| windspeed |  0.03 |    -0.19 |      1.00 |    0.09 |
| rentals   |  0.36 |    -0.29 |      0.09 |    1.00 |

Rental Correlations

    # Base plot aesthetic with Total Points on x axis

    tempPoint_season <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = season))

    # Avg PM point plot
    tempPoint_season + geom_point() + geom_smooth(aes(group = season,), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Temperature") +
                facet_wrap(~ season)

![](TuesdayAnalysis_files/figure-gfm/temp%20plots-1.png)<!-- -->

    tempPoint_weather <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = weather))

    tempPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Temperature") +
                facet_wrap(~ weather)

![](TuesdayAnalysis_files/figure-gfm/temp%20plots-2.png)<!-- -->

    tempPoint_hour <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = hour))

    tempPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Temperature") +
                facet_wrap(~ hour)

![](TuesdayAnalysis_files/figure-gfm/temp%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    humPoint_season <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = season))

    # Avg PM point plot
    humPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Humidity") +
                facet_wrap(~ season)

![](TuesdayAnalysis_files/figure-gfm/humidity%20plots-1.png)<!-- -->

    humPoint_weather <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = weather))

    humPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Humidity") +
                facet_wrap(~ weather)

![](TuesdayAnalysis_files/figure-gfm/humidity%20plots-2.png)<!-- -->

    humPoint_hour <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = hour))

    humPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Humidity") +
                facet_wrap(~ hour)

![](TuesdayAnalysis_files/figure-gfm/humidity%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    windPoint_season <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = season))

    # Avg PM point plot
    windPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Wind Speed") +
                facet_wrap(~ season)

![](TuesdayAnalysis_files/figure-gfm/wind%20plots-1.png)<!-- -->

    windPoint_weather <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = weather))

    windPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Wind Speed") +
                facet_wrap(~ weather)

![](TuesdayAnalysis_files/figure-gfm/wind%20plots-2.png)<!-- -->

    windPoint_hour <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = hour))

    windPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Wind Speed") +
                facet_wrap(~ hour)

![](TuesdayAnalysis_files/figure-gfm/wind%20plots-3.png)<!-- -->

Modeling
========

words

### Post Summary Analysis Train & Test Data

    bikeTrain1 <- bikeTrain %>% select(c(season, hr, weathersit, temp, hum, cnt)) %>%
                                mutate(season = as.factor(ifelse(bikeTrain$season == 1, "winter",
                                                          ifelse(bikeTrain$season == 2, "spring",
                                                          ifelse(bikeTrain$season == 3, "summer", "fall")
                                                         ))),
                           
                                hour = as.factor(ifelse((bikeTrain$hr >= 6) & (bikeTrain$hr < 10), "AM commute", 
                                                 ifelse((bikeTrain$hr >= 10) & (bikeTrain$hr < 16), "mid day", 
                                                 ifelse((bikeTrain$hr >= 16 )& (bikeTrain$hr < 20), "PM commute", 
                                                 ifelse((bikeTrain$hr >= 20) & (bikeTrain$hr <= 23), "night", "late night")
                                                )))),
                           
                                weather = as.factor(ifelse(bikeTrain$weathersit == 1, "clear", 
                                                    ifelse(bikeTrain$weathersit == 2, "mist",
                                                    ifelse(bikeTrain$weathersit == 3, "light precip", "heavy precip")
                                                   )))) %>%
                                rename("humidity" = hum, "rentals" = cnt)
    bikeTrain1 <- bikeTrain1 %>% select(c(season, hour, weather, temp, humidity, rentals))        
    str(bikeTrain1)

    ## 'data.frame':    1719 obs. of  6 variables:
    ##  $ season  : Factor w/ 4 levels "fall","spring",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ hour    : Factor w/ 5 levels "AM commute","late night",..: 2 2 2 2 1 1 1 1 3 3 ...
    ##  $ weather : Factor w/ 3 levels "clear","light precip",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ temp    : num  0.16 0.16 0.14 0.14 0.12 0.12 0.14 0.16 0.22 0.24 ...
    ##  $ humidity: num  0.55 0.59 0.63 0.63 0.74 0.74 0.69 0.64 0.51 0.56 ...
    ##  $ rentals : int  5 2 1 2 36 94 179 100 57 97 ...

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

    ## 'data.frame':    734 obs. of  6 variables:
    ##  $ season  : Factor w/ 4 levels "fall","spring",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ hour    : Factor w/ 5 levels "AM commute","late night",..: 2 3 3 3 3 5 4 4 4 2 ...
    ##  $ weather : Factor w/ 3 levels "clear","light precip",..: 1 3 1 1 1 1 1 1 1 1 ...
    ##  $ temp    : num  0.12 0.16 0.22 0.26 0.28 0.28 0.24 0.22 0.2 0.14 ...
    ##  $ humidity: num  0.68 0.69 0.51 0.52 0.52 0.48 0.48 0.64 0.69 0.59 ...
    ##  $ rentals : int  4 42 78 63 65 212 54 48 11 3 ...

Tree Based Model (non-ensemble)
-------------------------------

words

    # train all predictors

    rfitAll <- train(rentals ~ ., data = bikeTrain1,
                      method = "rpart", preProcess = 
                      c("center", "scale"), trControl = 
                      trainControl(method = "LOOCV"))

    plot(varImp(rfitAll))

![](TuesdayAnalysis_files/figure-gfm/rtree%20all-1.png)<!-- -->

    round(rfitAll$results[, c(1,2)], digits = 2)

    ##     cp   RMSE
    ## 1 0.07 143.73
    ## 2 0.17 163.83
    ## 3 0.30 203.51

    rfitTop3 <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", preProcess = 
                             c("center", "scale"), trControl = 
                             trainControl(method = "LOOCV"))
                             
    kable(round(rfitTop3$results[, c(1,2)], digits = 2))

|   cp |   RMSE |
|-----:|-------:|
| 0.07 | 143.73 |
| 0.17 | 163.83 |
| 0.30 | 203.51 |

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
| 0.00 | 116.75 |
| 0.01 | 119.89 |
| 0.02 | 118.80 |
| 0.02 | 126.69 |

    ufitFinal <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", 
                             preProcess = c("center", "scale"),
                             trControl = trainControl(method = "LOOCV"), 
                             tuneGrid = expand.grid(cp = .02))

    plot(ufitFinal$finalModel)
    text(ufitFinal$finalModel, pretty = 1, cex = .8)

![](TuesdayAnalysis_files/figure-gfm/rtree%20final%20model-1.png)<!-- -->

    ufitPred <- predict(ufitFinal, newdata = bikeTest1)
    ufitPred_results <- as_tibble(postResample(ufitPred, bikeTest1$rentals))
    ufitPred_results$value <- round(ufitPred_results$value, digits = 2)
    setattr(ufitPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))
    kable(ufitPred_results, caption = "Regression Tree Prediction Results")

|          |  value |
|:---------|-------:|
| RMSE     | 118.46 |
| Rsquared |   0.61 |
| MAE      |  78.17 |

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
| 1   |       0.1 |                 1 |             10 |      50 | 124.12 |     0.58 | 87.94 |  27.56 |       0.12 | 15.69 |
| 4   |       0.1 |                 2 |             10 |      50 | 112.31 |     0.65 | 77.16 |  25.77 |       0.11 | 14.42 |
| 7   |       0.1 |                 3 |             10 |      50 | 108.23 |     0.67 | 73.81 |  23.90 |       0.11 | 13.42 |
| 2   |       0.1 |                 1 |             10 |     100 | 117.89 |     0.61 | 83.67 |  26.01 |       0.12 | 15.01 |
| 5   |       0.1 |                 2 |             10 |     100 | 107.45 |     0.68 | 73.50 |  23.54 |       0.10 | 13.31 |
| 8   |       0.1 |                 3 |             10 |     100 | 104.78 |     0.70 | 70.84 |  21.65 |       0.10 | 12.62 |
| 3   |       0.1 |                 1 |             10 |     150 | 115.70 |     0.63 | 82.33 |  24.78 |       0.11 | 14.57 |
| 6   |       0.1 |                 2 |             10 |     150 | 105.68 |     0.69 | 72.13 |  22.32 |       0.10 | 12.87 |
| 9   |       0.1 |                 3 |             10 |     150 | 104.12 |     0.70 | 70.49 |  21.03 |       0.09 | 12.56 |

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
| 1   |       0.1 |                 4 |             10 |     200 | 104.32 |     0.70 | 69.48 |  17.89 |       0.10 | 10.58 |
| 3   |       0.2 |                 4 |             10 |     200 | 107.58 |     0.68 | 72.16 |  16.52 |       0.09 | 10.09 |
| 2   |       0.1 |                 4 |             10 |     250 | 104.78 |     0.70 | 69.96 |  17.23 |       0.09 | 10.32 |
| 4   |       0.2 |                 4 |             10 |     250 | 107.76 |     0.68 | 72.67 |  16.56 |       0.09 | 10.28 |

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
| RMSE     | 103.07 |
| Rsquared |   0.71 |
| MAE      |  68.12 |

Boosted Tree Prediction Results
