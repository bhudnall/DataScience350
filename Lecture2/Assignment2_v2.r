library(dplyr)
library(ggplot2)
library(knitr)
set.seed(42)
## Assignment # 2

## R-script that outputs the probabilities and variances
## Should include proof of correct answer: Chart and Table

## Total number of simulations
sim_n <- 1000

## This simulates the number of initial choices
sim_initial_choice <- function(n = sim_n) {
        
        pick <- runif(n)
        ifelse(pick <= .33, 'door_1'
               , ifelse(pick <= .66, 'door_2', 'door_3'))
}

## This simulates the choice a contestant would make 
## assuming the car is behind door #1 and they choose to make a switch
sim_switch_choice <- function(n = sim_n, df = initial_choice_df) {
        
        df[,2] <- NA; df[,3] <- NA
        names(df)[2:3] <- c("final_choice", "result")
        for(i in 1:n) {
                if(df[i, "initial_choice"] == "door_1") {
                        choice <- runif(1)
                        df[i, "final_choice"] <- ifelse(choice > .50
                               , "door_2"
                               , "door_3")
                        df[i, "result"] <- "goat_given_switch"
                } 
                else {
                        df[i, "final_choice"] <- "door_1"
                        df[i, "result"] <- "car_given_switch"
                }
        }
        df
}

## This simulates the choice a contestant would make 
## assuming the car is behind door #1 and they choose to make a switch
sim_stay_choice <- function(n = sim_n, df = initial_choice_df) {
        
        df[,2] <- NA; df[,3] <- NA
        names(df)[2:3] <- c("final_choice", "result")
        for(i in 1:n) {
                choice <- df[i, "initial_choice"]
                if(choice == "door_1") {
                        df[i, "final_choice"] <- "door_1"
                        df[i, "result"] <- "car_given_stay"
                } 
                else if(choice == "door_2") {
                        df[i, "final_choice"] <- "door_2"
                        df[i, "result"] <- "goat_given_stay"
                }
                else {
                        df[i, "final_choice"] <- "door_3"
                        df[i, "result"] <- "goat_given_stay"
                        
                }
        }
        df
}

## summarise the results and return a data Frame
## also add a column for the probability of results 
## given a car or a goat output
result_summary <- function(df, sim_n) {
        
        df <- df %>% 
                count(initial_choice, final_choice, result) %>%
                mutate(prob = n/sim_n) 
        
}

## Plot the initial results
plot_summary <- function(df, title) {
        
        ggplot(df, aes(x = result, y = prob)) +
                geom_bar(stat = "identity") + 
                ggtitle(title)
        
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
stay_df <- sim_stay_choice(sim_n, initial_choice_df)

## Run the result summary function to see the probabilities
## and results
switch_df_agg <- result_summary(switch_df, sim_n)
switch_df_agg 
stay_df_agg <- result_summary(stay_df, sim_n)
stay_df_agg

## Plot the results
plot_summary(switch_df_agg, "Results given switch")
plot_summary(stay_df_agg, "results given stay")

