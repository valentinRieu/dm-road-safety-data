
library(readr)
library(tidyr)
library(dplyr)
library(hms)
library(purrr)

rm(list = ls())

preprocess <- function(ds) {
  ds.clean <- ds %>%
    drop_na()
  
  ds.clean.drop <- ds.clean %>%
    dplyr::select(-c(police_force, accident_index, accident_year, accident_reference, local_authority_ons_district, local_authority_highway,
                     local_authority_district, lsoa_of_accident_location, first_road_number, second_road_number, 
                     location_easting_osgr, location_northing_osgr, longitude, latitude))
  
  # for (i in 5:ncol(ds.clean.numeric)) {
  #   print(colnames(ds.clean.numeric)[i])
  #   print(sort(unique(ds.clean.numeric[[i]])))
  # }
  
  # So we will group them
  
  ds.pre <- ds.clean.drop %>%
    mutate(
      number_of_vehicles = ifelse(number_of_vehicles <= 3, number_of_vehicles, 4)
    )
  
  # Same for number of casualties
  
  ds.pre <- ds.pre %>%
    mutate(
      number_of_casualties = ifelse(number_of_casualties <= 3, number_of_casualties, 4)
    )
  
  
  # for `pedestrian_crossing_human_control`, we group the values since none is the majority
  ds.pre <- ds.pre %>%
    mutate(
      pedestrian_crossing_human_control = as.integer(ifelse(pedestrian_crossing_human_control==0, 0, 1))
    )
  
  ds.pre <- ds.pre %>%
    mutate(
      pedestrian_crossing_physical_facilities = as.integer(ifelse(pedestrian_crossing_physical_facilities==0, 0, 1))
    )
  
  # We group for weather condition as well
  ds.pre <- ds.pre %>%
    mutate(
      weather_conditions = case_when(
        weather_conditions %in% c(1, 2) ~ weather_conditions,
        (weather_conditions >= 3 | weather_conditions == -1) ~ 3
      )
    )
  
  
  # Road surface, except 1 and 2, we group the rest
  ds.pre <- ds.pre %>%
    mutate(
      road_surface_conditions = case_when(
        road_surface_conditions == 1 ~ 1,
        road_surface_conditions == 2 ~ 2,
        (road_surface_conditions == -1 | road_surface_conditions >= 3) ~ 3
      )
    )
  
  # Special condition : no condition or a condition
  ds.pre <- ds.pre %>%
    mutate(
      special_conditions_at_site = as.integer(ifelse(special_conditions_at_site==0, 0, 1))
    )
  
  # Hazard: no hazard or a hazard
  ds.pre <- ds.pre %>%
    mutate(
      carriageway_hazards = as.integer(ifelse(carriageway_hazards==0, 0, 1))
    )
  
  ds.pre <- ds.pre %>%
    mutate(
      accident_in_urban = as.integer(ifelse(urban_or_rural_area==1, 1, 0))) %>%
    dplyr::select(-urban_or_rural_area)
  
  ds.pre <- ds.pre %>%
    mutate(
      did_police_officer_attend_scene_of_accident = as.integer(ifelse(did_police_officer_attend_scene_of_accident==1, 1, 0))
    )
  
  ds.pre <- ds.pre %>%
    mutate(
      trunk_road_flag = as.integer(ifelse(trunk_road_flag==1, 1, 0)),
    )
  
  ds.pre <- ds.pre %>%
    separate(time,
             sep = ':',
             into = c('hour', 'minute', 'second'),
             convert = TRUE,
             remove = TRUE
    ) %>%
    dplyr::select(!c(second, minute))    
  
  
  ds.pre <- ds.pre %>%
    separate(date,
             sep = '/',
             into = c('day', 'month', 'year'),
             remove = TRUE,
             convert = TRUE
    ) %>%
    dplyr::select(!c(year))
  
  
  # time will be separated in 4: early morning, morning until lunch, afternoon until evening, evening until noon
  
  ds.pre <- ds.pre %>%
    mutate(
      season = case_when(
        month %in% c(3, 4, 5) ~ 1,
        month %in% c(6, 7, 8) ~ 2,
        month %in% c(9, 10, 11) ~ 3,
        month %in% c(12, 1, 2) ~ 4
      ))
  
  ds.pre <- ds.pre %>%
    mutate(
      across(everything(), as.factor))

  
  return(ds.pre)
 }
  
  
  
  
  