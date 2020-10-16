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
    ## 1 2011-01-03 winter   clear 0.22     0.44    0.3582       1 late night       5
    ## 2 2011-01-03 winter   clear 0.20     0.44    0.4179       1 late night       2
    ## 3 2011-01-03 winter   clear 0.16     0.47    0.3881       1 late night       1
    ## 4 2011-01-03 winter   clear 0.16     0.47    0.2836       1 late night       3
    ## 5 2011-01-10 winter   clear 0.12     0.50    0.2836       1 late night       1
    ## 6 2011-01-10 winter   clear 0.12     0.50    0.2239       1 late night       1

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
| winter |       108.27 |       45691 |
| spring |       187.15 |       85341 |
| summer |       229.02 |      101456 |
| fall   |       205.06 |       85303 |

Rentals by Season

    seasonBox <- ggplot(sumBikeTrain, aes(x = rentals, y = season, color = season))
    seasonBox + geom_boxplot() + labs(title = "Rentals by Season")

![](MondayAnalysis_files/figure-gfm/season%20statbox-1.png)<!-- -->

    kable(sum_meanFx("weather"), caption = "Rentals by Weather Condition")

| weather      | rental\_mean | rental\_sum |
|:-------------|-------------:|------------:|
| clear        |       189.74 |      211943 |
| mist         |       182.39 |       91926 |
| light precip |       119.63 |       13758 |
| heavy precip |       164.00 |         164 |

Rentals by Weather Condition

    weatherBox <- ggplot(sumBikeTrain, aes(x = rentals, y = weather, color = weather))
    weatherBox + geom_boxplot() + labs(title = "Rentals by Weather Condiion")

![](MondayAnalysis_files/figure-gfm/weather%20statbox-1.png)<!-- -->

    kable(sum_meanFx("hour"), caption = "Rentals by Hour")

| hour       | rental\_mean | rental\_sum |
|:-----------|-------------:|------------:|
| late night |        16.67 |        7067 |
| AM commute |       237.68 |       65601 |
| mid day    |       185.90 |       84400 |
| PM commute |       408.90 |      117354 |
| night      |       146.52 |       43369 |

Rentals by Hour

    hourBox <- ggplot(sumBikeTrain, aes(x = rentals, y = hour, color = hour))
    hourBox + geom_boxplot() + labs(title = "Rentals by Hour")

![](MondayAnalysis_files/figure-gfm/hour%20statbox-1.png)<!-- -->

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
| late night |        27.51 |        1623 |
| AM commute |       111.50 |        4906 |
| mid day    |       277.27 |       17468 |
| PM commute |       287.91 |       12380 |
| night      |       106.40 |        4575 |

Rentals by Hour (Day Off

    dayOffBox <- ggplot(dayOff, aes(x = rentals, y = hour, color = hour))
    dayOffBox + geom_boxplot() + labs(title = "Rentals by Hour (Day Off)")

![](MondayAnalysis_files/figure-gfm/hour%20statbox-2.png)<!-- -->

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
| late night |        14.92 |        5444 |
| AM commute |       261.62 |       60695 |
| mid day    |       171.18 |       66932 |
| PM commute |       430.22 |      104974 |
| night      |       153.34 |       38794 |

Rentals by Time of Day (Work Day)

    dayOnBox <- ggplot(dayOn, aes(x = rentals, y = hour, color = hour))
    dayOnBox + geom_boxplot() + labs(title = "Rentals by Time of Day (Work Day)")

![](MondayAnalysis_files/figure-gfm/hour%20statbox-3.png)<!-- -->

    # 
    quantStats <- sumBikeTrain %>% select(c(4, 5, 6, 9)) %>% apply(2, summary)

    rentalCor <- select(sumBikeTrain, c(temp, humidity, windspeed, rentals))
    rentalCor <- as.data.frame(round(cor(rentalCor), digits = 2))


    kable(round((quantStats), 2), caption = "Summary: Quantitative Varibles")

|         | temp | humidity | windspeed | rentals |
|:--------|-----:|---------:|----------:|--------:|
| Min.    | 0.02 |     0.15 |      0.00 |    1.00 |
| 1st Qu. | 0.34 |     0.49 |      0.10 |   37.00 |
| Median  | 0.52 |     0.64 |      0.19 |  138.00 |
| Mean    | 0.50 |     0.63 |      0.19 |  182.95 |
| 3rd Qu. | 0.66 |     0.78 |      0.25 |  268.00 |
| Max.    | 0.90 |     1.00 |      0.72 |  968.00 |

Summary: Quantitative Varibles

    kable(rentalCor, caption = "Rental Correlations")

|           |  temp | humidity | windspeed | rentals |
|:----------|------:|---------:|----------:|--------:|
| temp      |  1.00 |    -0.05 |      0.06 |    0.37 |
| humidity  | -0.05 |     1.00 |     -0.38 |   -0.27 |
| windspeed |  0.06 |    -0.38 |      1.00 |    0.13 |
| rentals   |  0.37 |    -0.27 |      0.13 |    1.00 |

Rental Correlations

    # Base plot aesthetic with Total Points on x axis

    tempPoint_season <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = season))

    # Avg PM point plot
    tempPoint_season + geom_point() + geom_smooth(aes(group = season,), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Temperature") +
                facet_wrap(~ season)

![](MondayAnalysis_files/figure-gfm/temp%20plots-1.png)<!-- -->

    tempPoint_weather <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = weather))

    tempPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Temperature") +
                facet_wrap(~ weather)

![](MondayAnalysis_files/figure-gfm/temp%20plots-2.png)<!-- -->

    tempPoint_hour <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = hour))

    tempPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Temperature") +
                facet_wrap(~ hour)

![](MondayAnalysis_files/figure-gfm/temp%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    humPoint_season <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = season))

    # Avg PM point plot
    humPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Humidity") +
                facet_wrap(~ season)

![](MondayAnalysis_files/figure-gfm/humidity%20plots-1.png)<!-- -->

    humPoint_weather <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = weather))

    humPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Humidity") +
                facet_wrap(~ weather)

![](MondayAnalysis_files/figure-gfm/humidity%20plots-2.png)<!-- -->

    humPoint_hour <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = hour))

    humPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Humidity") +
                facet_wrap(~ hour)

![](MondayAnalysis_files/figure-gfm/humidity%20plots-3.png)<!-- -->

    # Base plot aesthetic with Total Points on x axis

    windPoint_season <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = season))

    # Avg PM point plot
    windPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Season Rentals by Wind Speed") +
                facet_wrap(~ season)

![](MondayAnalysis_files/figure-gfm/wind%20plots-1.png)<!-- -->

    windPoint_weather <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = weather))

    windPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Weather Rentals by Wind Speed") +
                facet_wrap(~ weather)

![](MondayAnalysis_files/figure-gfm/wind%20plots-2.png)<!-- -->

    windPoint_hour <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = hour))

    windPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
                scale_fill_continuous() + labs(title =  "Time of Day Rentals by Wind Speed") +
                facet_wrap(~ hour)

![](MondayAnalysis_files/figure-gfm/wind%20plots-3.png)<!-- -->

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

    ## 'data.frame':    1737 obs. of  6 variables:
    ##  $ season  : Factor w/ 4 levels "fall","spring",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ hour    : Factor w/ 5 levels "AM commute","late night",..: 2 2 2 2 1 1 1 3 3 5 ...
    ##  $ weather : Factor w/ 4 levels "clear","heavy precip",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ temp    : num  0.22 0.2 0.16 0.16 0.14 0.14 0.16 0.2 0.24 0.26 ...
    ##  $ humidity: num  0.44 0.44 0.47 0.47 0.5 0.5 0.43 0.4 0.35 0.3 ...
    ##  $ rentals : int  5 2 1 3 64 154 88 51 61 76 ...

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

    ## 'data.frame':    742 obs. of  6 variables:
    ##  $ season  : Factor w/ 4 levels "fall","spring",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ hour    : Factor w/ 5 levels "AM commute","late night",..: 1 3 3 3 3 5 4 4 2 2 ...
    ##  $ weather : Factor w/ 3 levels "clear","light precip",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ temp    : num  0.14 0.18 0.22 0.26 0.26 0.24 0.2 0.18 0.12 0.12 ...
    ##  $ humidity: num  0.5 0.43 0.35 0.3 0.3 0.3 0.47 0.64 0.5 0.5 ...
    ##  $ rentals : int  30 44 61 77 72 157 52 52 5 3 ...

Tree Based Model (non-ensemble)
-------------------------------

words

    # train all predictors

    rfitAll <- train(rentals ~ ., data = bikeTrain1,
                      method = "rpart", preProcess = 
                      c("center", "scale"), trControl = 
                      trainControl(method = "LOOCV"))

    plot(varImp(rfitAll))

![](MondayAnalysis_files/figure-gfm/rtree%20all-1.png)<!-- -->

    round(rfitAll$results[, c(1,2)], digits = 2)

    ##     cp   RMSE
    ## 1 0.07 137.55
    ## 2 0.16 156.57
    ## 3 0.31 196.47

    rfitTop3 <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", preProcess = 
                             c("center", "scale"), trControl = 
                             trainControl(method = "LOOCV"))
                             
    kable(round(rfitTop3$results[, c(1,2)], digits = 2))

|   cp |   RMSE |
|-----:|-------:|
| 0.07 | 137.55 |
| 0.16 | 156.57 |
| 0.31 | 196.47 |

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
| 0.00 | 115.43 |
| 0.01 | 120.39 |
| 0.02 | 120.87 |
| 0.02 | 120.09 |

    ufitFinal <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                             method = "rpart", 
                             preProcess = c("center", "scale"),
                             trControl = trainControl(method = "LOOCV"), 
                             tuneGrid = expand.grid(cp = .02))

    plot(ufitFinal$finalModel)
    text(ufitFinal$finalModel, pretty = 1, cex = .8)

![](MondayAnalysis_files/figure-gfm/rtree%20final%20model-1.png)<!-- -->

    ufitPred <- predict(ufitFinal, newdata = bikeTest1)
    ufitPred_results <- as_tibble(postResample(ufitPred, bikeTest1$rentals))
    ufitPred_results$value <- round(ufitPred_results$value, digits = 2)
    setattr(ufitPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))
    kable(ufitPred_results, caption = "Regression Tree Prediction Results")

|          |  value |
|:---------|-------:|
| RMSE     | 116.13 |
| Rsquared |   0.57 |
| MAE      |  76.14 |

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
| 1   |       0.1 |                 1 |             10 |      50 | 119.62 |     0.58 | 84.81 |  20.83 |       0.11 | 11.74 |
| 4   |       0.1 |                 2 |             10 |      50 | 110.69 |     0.63 | 76.48 |  19.53 |       0.11 | 10.94 |
| 7   |       0.1 |                 3 |             10 |      50 | 108.25 |     0.65 | 74.85 |  19.71 |       0.11 | 11.48 |
| 2   |       0.1 |                 1 |             10 |     100 | 115.79 |     0.60 | 82.83 |  20.10 |       0.11 | 12.06 |
| 5   |       0.1 |                 2 |             10 |     100 | 107.79 |     0.65 | 75.06 |  19.12 |       0.10 | 11.13 |
| 8   |       0.1 |                 3 |             10 |     100 | 107.06 |     0.65 | 74.56 |  19.51 |       0.11 | 12.04 |
| 3   |       0.1 |                 1 |             10 |     150 | 114.10 |     0.61 | 82.33 |  19.93 |       0.11 | 12.63 |
| 6   |       0.1 |                 2 |             10 |     150 | 107.00 |     0.65 | 74.96 |  19.30 |       0.11 | 11.66 |
| 9   |       0.1 |                 3 |             10 |     150 | 106.36 |     0.66 | 74.19 |  19.14 |       0.10 | 11.87 |

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
| 1   |       0.1 |                 4 |             10 |     200 | 106.37 |     0.66 | 73.82 |  16.04 |        0.1 |  9.69 |
| 3   |       0.2 |                 4 |             10 |     200 | 107.94 |     0.65 | 74.97 |  16.39 |        0.1 | 10.05 |
| 2   |       0.1 |                 4 |             10 |     250 | 106.69 |     0.66 | 74.01 |  15.48 |        0.1 |  9.58 |
| 4   |       0.2 |                 4 |             10 |     250 | 108.85 |     0.64 | 75.54 |  16.03 |        0.1 |  9.90 |

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
    ## There were 12 predictors of which 11 had non-zero influence.

    boostedPred <- predict(boostedFinal, newdata = bikeTest1)
    boostedPred_results <- as_tibble(postResample(boostedPred, bikeTest1$rentals))
    boostedPred_results$value <- round(boostedPred_results$value, digits = 2)
    setattr(boostedPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))
    kable(boostedPred_results, caption = "Boosted Tree Prediction Results")

|          |  value |
|:---------|-------:|
| RMSE     | 104.69 |
| Rsquared |   0.65 |
| MAE      |  69.67 |

Boosted Tree Prediction Results
