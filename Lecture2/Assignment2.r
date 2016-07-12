library(dplyr)
library(ggplot2)
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

## This simulates the number of potential switches for Monty Hall
## assuming the car is behind door #1 and a switch
sim_switch_choice <- function(n = sim_n, df = initial_choice_df) {
        
        df[,2] <- NA
        names(df)[2] <- "switch_choice"
        for(i in 1:n) {
                if(df[i, "initial_choice"] == "door_1") {
                        choice <- runif(1)
                        df[i, "switch_choice"] <- ifelse(choice > .50
                               , "door_2"
                               , "door_3")
                } 
                else {
                        df[i, "switch_choice"] <- "door_1"
                }
        }
        df
}

## run the pick simulation
initial_choice <- sim_initial_choice()
## what are the counts of results in the simulation
table(initial_choice)

## What are the probabilities of each choice in the simulation
initial_choice_probs <- table(initial_choice)/sim_n
## Convert choice list to DataFrame
initial_choice_df <- as.data.frame(initial_choice)

## Run second pick simulation
switch_df <- sim_switch_choice(sim_n, initial_choice_df)
## Total counts of first and second pick
table(switch_df)
## the probabilities of the counts
switch_df_prob <- as.data.frame(table(switch_df)/sim_n)
names(switch_df_prob)[3] <- "probability"
## correct door is 1, so add door_2 and door_3 as incorrect answer
switch_df_prob[,4] <- NA
names(switch_df_prob)[4] <- "result"
for(i in 1:nrow(switch_df_prob)) {
        
        if(switch_df_prob[i,"switch_choice"] == "door_1") {
                switch_df_prob[i, "result"] <- "car_given_switch"
        }
        else {
                switch_df_prob[i, "result"] <- "goat_given_switch"
        }
}
switch_df_result <- select(switch_df_prob, probability, result)
switch_df_result <- switch_df_result %>% 
        group_by(result) %>% 
        summarise(probability = sum(probability))

## Plot the results
ggplot(switch_df_result, aes(x = result, y = probability)) +
        geom_bar(stat = "identity") + 
        ggtitle("Results given a switch")
