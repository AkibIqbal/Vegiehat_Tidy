library(readxl)
library(tidyverse)
VEGIEHAT_Pilot_Database <- read_excel("C:/Projects/Vegiehat/Vegiehat_Full_Data/VEGIEHAT-Pilot-Database.xlsx")
#View(VEGIEHAT_Pilot_Database)
colnames(VEGIEHAT_Pilot_Database)

Vegiehat_Full_Data = VEGIEHAT_Pilot_Database %>% 
  select(
    `Submission ID`, UserId, `Submission time`, DistrictName, UpazilaName, 
    `Items to Choose`, `Value - Rice`, `Value - Flour`, `Value - Lentil`, 
    `Value - Soybean Oil`, `Value - Salt`, `Value - Sugar`, `Value - Eggs`, 
    `Value - Chicken`, `Value - Potato`, `Value - Eggplant`, `Value - Onion`, 
    `Value - Green Chilli`, `Rice Purchase Options`, `Flour Purchase Options`, 
    `Lentils Purchase Options`,`Soybean Oil Purchase Options`,`Salt Purchase Options`,
    `Sugar Purchase Options`,`Eggs Purchase Options`,`Chicken Purchase Options`,`Name of Rice`,
    `Source of Sugar`,`Potato Purchase Options`,`Types of Eggplant`,`Source of Onion`,
    `Type of Shop for Rice`,
    `Type of Shop for Flour` ,
    `Type of Shop for Lentils`,
    `Type of Shop for Soybean Oil`,
    `Type of Shop for Salt`,
    `Type of Shop for Sugar`,
    `Eggs Purchase Options`, 
    `Type of Shop for Chicken`,
    `Type of Shop for Potato`,
    `Type of Shop for Eggplant` , 
    `Type of Shop for Onion`,`Type of Shop for Green Chilli`,
    `Name of Outlet for Rice`,
    `Name of Outlet for Flour`,
    `Name of Outlet for Lentils`,
    `Name of Outlet for Soybean Oil`,
    `Name of Outlet for Salt`,
    `Name of Outlet for Sugar`,
    `Name of Outlet for Eggs`,
    `Name of Outlet for Chicken`,
    `Name of Outlet for Potato`,
    `Name of Outlet for Eggplant`,
    `Name of Outlet for Onion`,
    `Name of Outlet for Green Chilli`
    
    
  ) %>% 
  separate_rows(
    `Items to Choose`, sep = ","
  ) %>% 
  mutate(
    `Items to Choose` = str_trim(`Items to Choose`, side = "both")
  ) %>% 
  mutate(hasProduct = case_when(
    `Items to Choose` == "Rice" ~ 1,
    `Items to Choose` == "Flour" ~ 2,
    `Items to Choose` == "Lentil" ~ 3,
    `Items to Choose` == "Soybean Oil" ~ 4,
    `Items to Choose` == "Salt" ~ 5,
    `Items to Choose` == "Sugar" ~ 6,
    `Items to Choose` == "Eggs" ~ 7,
    `Items to Choose` == "Chicken" ~ 8,
    `Items to Choose` == "Potato" ~ 9,
    `Items to Choose` == "Eggplant" ~ 10,
    `Items to Choose` == "Onion" ~ 11,
    `Items to Choose` == "Green Chilli" ~ 12,
    TRUE ~ 0
  )) %>%
  mutate(
    Price = case_when(
      hasProduct == 1 ~ `Value - Rice`,
      hasProduct == 2 ~ `Value - Flour`,
      hasProduct == 3 ~ `Value - Lentil`,
      hasProduct == 4 ~ `Value - Soybean Oil`,
      hasProduct == 5 ~ `Value - Salt`,
      hasProduct == 6 ~ `Value - Sugar`,
      hasProduct == 7 ~ `Value - Eggs`,
      hasProduct == 8 ~ `Value - Chicken`,
      hasProduct == 9 ~ `Value - Potato`,
      hasProduct == 10 ~ `Value - Eggplant`,
      hasProduct == 11 ~ `Value - Onion`,
      hasProduct == 12 ~ `Value - Green Chilli`,
      TRUE ~ NA_real_
    )
  )%>%
  mutate(
    Purchase_Options = case_when(
      hasProduct == 1 ~ `Rice Purchase Options`,
      hasProduct == 2 ~ `Flour Purchase Options`,
      hasProduct == 3 ~ `Lentils Purchase Options`,
      hasProduct == 4 ~ `Soybean Oil Purchase Options`,
      hasProduct == 5 ~ `Salt Purchase Options`,
      hasProduct == 6 ~ `Sugar Purchase Options`,
      hasProduct == 7 ~ NA_character_,
      hasProduct == 8 ~ `Chicken Purchase Options`,
      hasProduct == 9 ~ NA_character_,
      hasProduct == 10 ~ NA_character_, 
      hasProduct == 11 ~ NA_character_,
      hasProduct == 12 ~ NA_character_, 
      TRUE ~ NA_character_
    )
  )%>%
  mutate(
    Type = case_when(
      hasProduct == 1 ~ `Name of Rice`,
      hasProduct == 2 ~ NA_character_,
      hasProduct == 3 ~ NA_character_,
      hasProduct == 4 ~ NA_character_,
      hasProduct == 5 ~ NA_character_,
      hasProduct == 6 ~ `Source of Sugar`,
      hasProduct == 7 ~ NA_character_,
      hasProduct == 8 ~ NA_character_,
      hasProduct == 9 ~ `Potato Purchase Options`,
      hasProduct == 10 ~ `Types of Eggplant`, 
      hasProduct == 11 ~ `Source of Onion`  ,
      hasProduct == 12 ~ NA_character_, 
      TRUE ~ NA_character_
    )
  )%>%
  mutate(
    Type_of_Shop = case_when(
      hasProduct == 1 ~ `Type of Shop for Rice`,
      hasProduct == 2 ~ `Type of Shop for Flour` ,
      hasProduct == 3 ~ `Type of Shop for Lentils`,
      hasProduct == 4 ~ `Type of Shop for Soybean Oil`,
      hasProduct == 5 ~ `Type of Shop for Salt`,
      hasProduct == 6 ~ `Type of Shop for Sugar`,
      hasProduct == 7 ~  `Eggs Purchase Options`, 
      hasProduct == 8 ~ `Type of Shop for Chicken`,
      hasProduct == 9 ~ `Type of Shop for Potato`,
      hasProduct == 10 ~ `Type of Shop for Eggplant` , 
      hasProduct == 11 ~ `Type of Shop for Onion`,
      hasProduct == 12 ~ `Type of Shop for Green Chilli`, 
      TRUE ~ NA_character_
    )
  )%>%
  mutate(
    Name_Of_Outlet = case_when(
      hasProduct == 1 ~ `Name of Outlet for Rice`,
      hasProduct == 2 ~ `Name of Outlet for Flour`,
      hasProduct == 3 ~ `Name of Outlet for Lentils`,
      hasProduct == 4 ~ `Name of Outlet for Soybean Oil`,
      hasProduct == 5 ~ `Name of Outlet for Salt`,
      hasProduct == 6 ~ `Name of Outlet for Sugar`,
      hasProduct == 7 ~ `Name of Outlet for Eggs`, 
      hasProduct == 8 ~ `Name of Outlet for Chicken`,
      hasProduct == 9 ~ `Name of Outlet for Potato`,
      hasProduct == 10 ~`Name of Outlet for Eggplant`, 
      hasProduct == 11 ~`Name of Outlet for Onion`,
      hasProduct == 12 ~`Name of Outlet for Green Chilli`,
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    `Submission ID`, UserId, `Submission time`, DistrictName, UpazilaName,
    `Items to Choose`, hasProduct, Price, Purchase_Options, Type, Type_of_Shop, Name_Of_Outlet
  ) %>%
  arrange(`Submission ID`, hasProduct) %>% 
  filter(Price != "NA")


View(Vegiehat_Full_Data)

#write.csv(Vegiehat_Full_Data,"C:\\Projects\\Vegiehat\\Vegiehat_Full_Data\\Vegiehat_Full_Data.csv") 
