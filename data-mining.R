library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(arules)
library(arulesViz)
library(caTools)
rm(list = ls())


source('pre-process.R')


data <- read_csv('data/rcs.csv')

ds <- preprocess(data)

transactions.pre <- transactions(ds)
warnings(transactions.pre)
problems(transactions.pre)

# ----
# Extract association rules from the dataset
# only subset with valid causes
valid.cause <- c('day', 'month', 'day_of_week', 'hour', 'first_road_class', 'road_type', 'speed_limit', 'junction_detail', 'junction_control', 'second_road_class',  'pedestrian_crossing_human_control',
                 'pedestrian_crossing_physicial_facilities', 'light_conditions', 'weather_conditions', 'road_surface_conditions', 'special_conditions_at_site', 'carriageway_hazards', 'trunk_road_flag',
                 'time_quarter', 'season', 'speed_limit', 'number_of_casualties', 'accident_in_urban', 'time_quarter', 'did_police_officer_attend_scene_of_accident',
                 'number_of_vehicles')
valid.cause <- paste(valid.cause, '=', sep= '')
valid.cause

valid.consequence <- c('accident_severity')
valid.consequence <- paste(valid.consequence, '=', sep = '')


association_rules <- apriori(transactions.pre,
                             parameter = list(
                               support = 0.3,
                               confidence = 0.8,
                               maxlen = 5,
                               target = 'rules'))
length(association_rules)
# inspect(association_rules)
# subset the association rules to keep only the ones with valid causes and consequences
association_rules_valid <- subset(association_rules, subset = (lhs %pin% valid.cause | rhs %pin% valid.consequence))
length(association_rules_valid)
inspect(association_rules_valid)

plot(association_rules_valid, shading = 'lift', control = NULL, main = '')

plot(association_rules_valid, method = 'graph', shading = 'support', control = list())

# We create sub association rules, with lhs containing number_of_vehicles
vehicles_cause <- subset(association_rules_valid, subset = (lhs %pin% c('number_of_vehicles=')))

length(vehicles_cause)

plot(vehicles_cause, shading = 'lift', control = NULL, main = '')

plot(vehicles_cause, method = 'graph', shading = 'support', control = list())

# casualties
casualties_cause <- subset(association_rules_valid, subset = (lhs %pin% c('number_of_casualties=')))
length(casualties_cause)


plot(casualties_cause, shading = 'lift', control = NULL)

plot(casualties_cause, method = 'graph', shading = 'support', control = list())


# causes without number_of_vehicles and number_of_casualties
other_causes <- subset(association_rules_valid, subset = !(lhs %pin% 'number_of_vehicles=' | lhs %pin% 'number_of_casualties='))
length(other_causes)

plot(other_causes, method = 'graph', shading = 'support', control = list())

