library(readr)
library(GGally)
library(dplyr)

## set the working directory
setwd(choose.dir())

## load in the data set
auto_data <- read_csv("Automobile price data _Raw_.csv")

## see the top rows in the data set
head(auto_data)

## clean up the colnames
colnames(auto_data) <- gsub("-","_", names(auto_data))

## Display the characteristics of each field
str(auto_data)

## There are 205 obs. of  26 variables
## There are many chr variables, might want to convert to dummy
## what are the variables with int or num?
fields <- c("bore", "stroke", "price")
auto_data[, fields] <- lapply(auto_data[, fields], as.numeric)
auto_data <- na.omit(auto_data)
## remove symboling, normalized_losses, and make
auto_data_slim <- select(auto_data
                         , -symboling
                         , -normalized_losses
                         , -make) 

## check fields again -- everything converted properly
str(auto_data)
str(auto_data_slim)

## run summary stat function, and find the variance of each numeric field
summary(auto_data)
numeric_fields <- sapply(auto_data, is.numeric)
auto_data_num <- auto_data[,numeric_fields]

## Variance
lapply(auto_data_num, var)

## Standard Deviation
sapply(auto_data_num, sd)

## Correlation Grid
cor(auto_data_num)

## Which outcome variables have the highest correlation with different features?
# Specific Outcomes:
#         city-mpg
#         highway-mpg
#         horsepower
#         price

## Run Regression models for these outcome variables
city_mpg_frame <- select(auto_data_slim, -highway_mpg)
city_mpg_lm <- lm(city_mpg ~ ., city_mpg_frame)
summary(city_mpg_lm)

highway_mpg_frame <- select(auto_data_slim, -city_mpg)
highway_mpg_lm <- lm(highway_mpg ~ ., highway_mpg_frame)
summary(highway_mpg_lm)

hp_lm <- lm(horsepower ~ ., auto_data_slim)
summary(hp_lm)

price_lm <- lm(price ~ ., auto_data_slim)
summary(price_lm)

## scatter plot matrix
## Filter down to specific variables for a pairs plot
auto_data_cor <- select(auto_data 
                        , curb_weight
                        , horsepower
                        , engine_size
                        , length
                        , compression_ratio
                        , price
                        , body_style)

## What are the counts of body style?
table(auto_data$body_style)

## plot the distributions and correlations with
## these variables.
ggpairs(auto_data_cor
        , lower=list(continuous = "smooth")
        , upper=list(params=list(corSize=5)), axisLabels='show'
        , colour = 'body_style')
        



