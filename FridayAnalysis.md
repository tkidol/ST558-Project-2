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
    ## 1 2011-01-07 winter    mist  0.2     0.64    0.1940       1 late night      17
    ## 2 2011-01-07 winter    mist  0.2     0.69    0.2239       1 late night       7
    ## 3 2011-01-07 winter    mist  0.2     0.69    0.2239       1 late night       1
    ## 4 2011-01-07 winter    mist  0.2     0.69    0.1343       1 late night       1
    ## 5 2011-01-14 winter   clear  0.1     0.54    0.1642       1 late night       5
    ## 6 2011-01-14 winter   clear  0.1     0.54    0.1343       1 late night       1

### Modeling Data

    bikeTrain1 <- sumBikeTrain %>% select(c(season, hour, weather, temp, humidity, rentals))
                      
    str(bikeTrain1)

    ## 'data.frame':    1743 obs. of  6 variables:
    ##  $ season  : Factor w/ 4 levels "fall","spring",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ hour    : Factor w/ 5 levels "AM commute","late night",..: 2 2 2 2 1 1 1 1 3 3 ...
    ##  $ weather : Factor w/ 3 levels "clear","light precip",..: 3 3 3 3 3 1 1 1 3 3 ...
    ##  $ temp    : num  0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 ...
    ##  $ humidity: num  0.64 0.69 0.69 0.69 0.69 0.69 0.51 0.47 0.4 0.37 ...
    ##  $ rentals : int  17 7 1 1 34 84 210 134 67 73 ...

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

    ## 'data.frame':    744 obs. of  6 variables:
    ##  $ season  : Factor w/ 4 levels "fall","spring",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ hour    : Factor w/ 5 levels "AM commute","late night",..: 2 3 3 3 3 5 5 4 4 2 ...
    ##  $ weather : Factor w/ 3 levels "clear","light precip",..: 2 1 3 3 3 3 1 1 3 1 ...
    ##  $ temp    : num  0.22 0.22 0.2 0.2 0.2 0.2 0.16 0.18 0.18 0.12 ...
    ##  $ humidity: num  0.55 0.37 0.37 0.4 0.37 0.37 0.55 0.47 0.43 0.5 ...
    ##  $ rentals : int  5 63 59 50 72 187 95 51 36 14 ...

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
| winter |       121.79 |       50543 |
| spring |       217.53 |       97235 |
| summer |       242.83 |      113886 |
| fall   |       202.42 |       83398 |

Rentals by Season

    seasonBox <- ggplot(sumBikeTrain, aes(x = rentals, y = season, color = season))
    seasonBox + geom_boxplot() + labs(title = "Rentals by Season")

![](FridayAnalysis_files/figure-gfm/season%20statbox-1.png)<!-- -->

    kable(sum_meanFx("weather"), caption = "Rentals by Weather Condition")

| weather      | rental\_mean | rental\_sum |
|:-------------|-------------:|------------:|
| mist         |       205.82 |       92621 |
| clear        |       204.09 |      237555 |
| light precip |       115.40 |       14886 |

Rentals by Weather Condition

    weatherBox <- ggplot(sumBikeTrain, aes(x = rentals, y = weather, color = weather))
    weatherBox + geom_boxplot() + labs(title = "Rentals by Weather Condiion")

![](FridayAnalysis_files/figure-gfm/weather%20statbox-1.png)<!-- -->

    kable(sum_meanFx("hour"), caption = "Rentals by Hour")

| hour       | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        22.06 |        9374 |
| AM commute |       256.02 |       73223 |
| mid day    |       217.12 |       96619 |
| PM commute |       397.27 |      119182 |
| night      |       162.59 |       46664 |

Rentals by Hour

    hourBox <- ggplot(sumBikeTrain, aes(x = rentals, y = hour, color = hour))
    hourBox + geom_boxplot() + labs(title = "Rentals by Hour")

![](FridayAnalysis_files/figure-gfm/hour%20statbox-1.png)<!-- -->

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
| late night |        12.71 |          89 |
| AM commute |       162.17 |         973 |
| mid day    |       182.67 |        1644 |
| PM commute |       250.80 |        1254 |
| night      |        87.60 |         438 |

Rentals by Hour (Day Off

    dayOffBox <- ggplot(dayOff, aes(x = rentals, y = hour, color = hour))
    dayOffBox + geom_boxplot() + labs(title = "Rentals by Hour (Day Off)")

![](FridayAnalysis_files/figure-gfm/hour%20statbox-2.png)<!-- -->

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
| late night |        22.21 |        9285 |
| AM commute |       258.04 |       72250 |
| mid day    |       217.83 |       94975 |
| PM commute |       399.76 |      117928 |
| night      |       163.92 |       46226 |

Rentals by Time of Day (Work Day)

    dayOnBox <- ggplot(dayOn, aes(x = rentals, y = hour, color = hour))
    dayOnBox + geom_boxplot() + labs(title = "Rentals by Time of Day (Work Day)")

![](FridayAnalysis_files/figure-gfm/hour%20statbox-3.png)<!-- -->

    # 
    quantStats <- sumBikeTrain %>% select(c(4, 5, 6, 9)) %>% apply(2, summary)

    rentalCor <- select(sumBikeTrain, c(temp, humidity, windspeed, rentals))
    rentalCor <- as.data.frame(round(cor(rentalCor), digits = 2))


    kable(round((quantStats), 2), caption = "Summary: Quantitative Varibles")

|         | temp | humidity | windspeed | rentals |
|:--------|-----:|---------:|----------:|--------:|
| Min.    | 0.06 |     0.08 |      0.00 |    1.00 |
| 1st Qu. | 0.34 |     0.46 |      0.10 |   47.50 |
| Median  | 0.50 |     0.60 |      0.16 |  163.00 |
| Mean    | 0.50 |     0.61 |      0.19 |  197.97 |
| 3rd Qu. | 0.66 |     0.77 |      0.25 |  286.50 |
| Max.    | 0.96 |     1.00 |      0.81 |  957.00 |

Summary: Quantitative Varibles

    kable(rentalCor, caption = "Rental Correlations")

|           |  temp | humidity | windspeed | rentals |
|:----------|------:|---------:|----------:|--------:|
| temp      |  1.00 |    -0.08 |     -0.04 |    0.39 |
| humidity  | -0.08 |     1.00 |     -0.39 |   -0.35 |
| windspeed | -0.04 |    -0.39 |      1.00 |    0.14 |
| rentals   |  0.39 |    -0.35 |      0.14 |    1.00 |

Rental Correlations

    # Base plot aesthetic with Total Points on x axis

    tempPoint_season <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = season))

    # Avg PM point plot
    tempPoint_season + geom_point() + geom_smooth(aes(group = season,), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Temperature") +
                facet_wrap(~ season)

![](FridayAnalysis_files/figure-gfm/temp%20plots-1.png)<!-- -->

    tempPoint_weather <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = weather))

    tempPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Temperature") +
                facet_wrap(~ weather)

![](FridayAnalysis_files/figure-gfm/temp%20plots-2.png)<!-- -->

    tempPoint_hour <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = hour))

    tempPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Temperature") +
                facet_wrap(~ hour)

![](FridayAnalysis_files/figure-gfm/temp%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    humPoint_season <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = season))

    # Avg PM point plot
    humPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Humidity") +
                facet_wrap(~ season)

![](FridayAnalysis_files/figure-gfm/humidity%20plots-1.png)<!-- -->

    humPoint_weather <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = weather))

    humPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Humidity") +
                facet_wrap(~ weather)

![](FridayAnalysis_files/figure-gfm/humidity%20plots-2.png)<!-- -->

    humPoint_hour <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = hour))

    humPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Humidity") +
                facet_wrap(~ hour)

![](FridayAnalysis_files/figure-gfm/humidity%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    windPoint_season <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = season))

    # Avg PM point plot
    windPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Wind Speed") +
                facet_wrap(~ season)

![](FridayAnalysis_files/figure-gfm/wind%20plots-1.png)<!-- -->

    windPoint_weather <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = weather))

    windPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Wind Speed") +
                facet_wrap(~ weather)

![](FridayAnalysis_files/figure-gfm/wind%20plots-2.png)<!-- -->

    windPoint_hour <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = hour))

    windPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Wind Speed") +
                facet_wrap(~ hour)

![](FridayAnalysis_files/figure-gfm/wind%20plots-3.png)<!-- -->

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

![](FridayAnalysis_files/figure-gfm/rtree%20all-1.png)<!-- -->

    round(rfitAll$results[, c(1,2)], digits = 2)

    ##     cp   RMSE
    ## 1 0.06 135.50
    ## 2 0.15 159.85
    ## 3 0.32 180.15

    rfitTop3 <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", preProcess = 
                             c("center", "scale"), trControl = 
                             trainControl(method = "LOOCV"))
                             
    kable(round(rfitTop3$results[, c(1,2)], digits = 2))

|   cp |   RMSE |
|-----:|-------:|
| 0.06 | 135.50 |
| 0.15 | 159.85 |
| 0.32 | 180.15 |

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
| 0.00 | 111.18 |
| 0.01 | 114.66 |
| 0.02 | 116.21 |
| 0.02 | 118.19 |

    ufitFinal <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", 
                             preProcess = c("center", "scale"),
                             trControl = trainControl(method = "LOOCV"), 
                             tuneGrid = expand.grid(cp = .02))

    plot(ufitFinal$finalModel)
    text(ufitFinal$finalModel, pretty = 1, cex = .8)

![](FridayAnalysis_files/figure-gfm/rtree%20final%20model-1.png)<!-- -->

    ufitPred <- predict(ufitFinal, newdata = bikeTest1)
    ufitPred_results <- as_tibble(postResample(ufitPred, bikeTest1$rentals))
    ufitPred_results$value <- round(ufitPred_results$value, digits = 2)
    setattr(ufitPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))
    kable(ufitPred_results, caption = "Regression Tree Prediction Results")

|          |  value |
|:---------|-------:|
| RMSE     | 115.64 |
| Rsquared |   0.53 |
| MAE      |  78.00 |

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
| 1   |       0.1 |                 1 |             10 |      50 | 113.58 |     0.61 | 81.40 |  20.50 |       0.12 | 10.52 |
| 4   |       0.1 |                 2 |             10 |      50 | 102.98 |     0.67 | 71.69 |  20.94 |       0.12 | 10.49 |
| 7   |       0.1 |                 3 |             10 |      50 | 100.06 |     0.68 | 69.37 |  20.41 |       0.12 | 10.69 |
| 2   |       0.1 |                 1 |             10 |     100 | 108.15 |     0.63 | 77.75 |  21.16 |       0.12 | 11.08 |
| 5   |       0.1 |                 2 |             10 |     100 |  99.17 |     0.69 | 69.08 |  20.93 |       0.11 | 10.93 |
| 8   |       0.1 |                 3 |             10 |     100 |  97.30 |     0.70 | 67.39 |  20.56 |       0.11 | 10.98 |
| 3   |       0.1 |                 1 |             10 |     150 | 106.43 |     0.64 | 76.87 |  21.00 |       0.12 | 11.37 |
| 6   |       0.1 |                 2 |             10 |     150 |  97.78 |     0.70 | 68.09 |  20.39 |       0.11 | 10.77 |
| 9   |       0.1 |                 3 |             10 |     150 |  96.47 |     0.70 | 66.72 |  19.82 |       0.11 | 10.75 |

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

|     | shrinkage | interaction.depth | n.minobsinnode | n.trees |  RMSE | Rsquared |   MAE | RMSESD | RsquaredSD | MAESD |
|:----|----------:|------------------:|---------------:|--------:|------:|---------:|------:|-------:|-----------:|------:|
| 1   |       0.1 |                 4 |             10 |     200 | 95.78 |     0.71 | 66.21 |  18.04 |       0.10 | 11.35 |
| 3   |       0.2 |                 4 |             10 |     200 | 97.31 |     0.70 | 67.45 |  18.49 |       0.11 | 12.04 |
| 2   |       0.1 |                 4 |             10 |     250 | 95.62 |     0.71 | 66.20 |  17.99 |       0.10 | 11.55 |
| 4   |       0.2 |                 4 |             10 |     250 | 97.54 |     0.70 | 67.83 |  17.97 |       0.11 | 12.01 |

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
| RMSE     | 102.43 |
| Rsquared |   0.63 |
| MAE      |  68.43 |

Boosted Tree Prediction Results
