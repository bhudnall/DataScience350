library(dplyr)
library(ggplot2)
library(knitr)
set.seed(42)
## Assignment # 2

## R-script that outputs the probabilities and variances
## Should include proof of correct answer: Chart and Table

## Total number of simulations
sim_n <- 10000

## This simulates the number of initial choices
sim_df <- function(n = sim_n) {
        
        door_choice <- runif(n)
        door_choice <- ifelse(door_choice <= 1/3, 1
               , ifelse(door_choice <= 2/3, 2, 3))
        car <- runif(n)
        car <- ifelse(car <= 1/3, 1
                      , ifelse(car <= 2/3, 2, 3))
        final_choice <- runif(n)
        final_choice <- ifelse(final_choice <= 1/2, 'switch', 'stay')
        df <- data.frame(door_choice, car, final_choice)
}

## This simulates the choice a contestant would make 
## assuming the car is behind door #1 and they choose to make a switch
sim_result <- function(df = sim_df) {
        
        df[,4] <- NA;
        names(df)[4] <- "result"
        for(i in 1:nrow(df)) {
                if(df[i,"final_choice"] == "stay") {
                        if(df[i,"door_choice"] == df[i, "car"]) {
                                df[i, "result"] <- 1
                        }
                        else {
                                df[i, "result"] <- 0
                        }
                }
                else {
                        if(df[i, "door_choice"] != df[i, "car"]) {
                                df[i, "result"] <- 1
                        }
                        else {
                                df[i, "result"] <- 0
                        }
                        
                }
        }
        df
}

## summarise the results and return a data Frame
## also add a column for the probability of results 
## given a car or a goat output
result_summary <- function(df) {
        
        df <- df %>%
                group_by(final_choice) %>%
                summarise(probability = mean(result), variance = var(result))
        
}

## Plot the initial results
plot_summary <- function(df, title) {
        
        ggplot(df, aes(x = final_choice, y = probability)) +
                geom_bar(stat = "identity") 
        
}

## run the pick simulation
initial_choice <- sim_initial_choice()

## what are the counts of results in the simulation
## and their probabilities
table(initial_choice)
table(initial_choice)/sim_n

## Convert choice list to DataFrame
initial_choice_df <- as.data.frame(initial_choice)

## Run second pick simulation -- either switch or stay
switch_df <- sim_switch_choice(sim_n, initial_choice_df)

## Run the result summary function to see the probabilities
## and results
switch_df_agg <- result_summary(switch_df, sim_n)
switch_df_agg 

## Plot the results
plot_summary(switch_df_agg, "Results given switch")
plot_summary(stay_df_agg, "Results given stay")

