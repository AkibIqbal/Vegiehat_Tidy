library(readxl)
library(tidyverse)
VEGIEHAT_Pilot_Database <- read_excel("C:/Projects/Vegiehat/Vegiehat_Full_Data/VEGIEHAT-Pilot-Database.xlsx")
#View(VEGIEHAT_Pilot_Database)
#colnames(VEGIEHAT_Pilot_Database)

Vegiehat_Full_Data_New = VEGIEHAT_Pilot_Database %>% 
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
  mutate(
    Price = case_when(
      str_detect(`Items to Choose`, "Rice") ~ `Value - Rice`,
      str_detect(`Items to Choose`, "Flour") ~ `Value - Flour`,
      str_detect(`Items to Choose`, "Lentil") ~ `Value - Lentil`,
      str_detect(`Items to Choose`, "Soybean Oil") ~ `Value - Soybean Oil`,
      str_detect(`Items to Choose`, "Salt") ~ `Value - Salt`,
      str_detect(`Items to Choose`, "Sugar") ~ `Value - Sugar`,
      str_detect(`Items to Choose`, "Eggs") ~ `Value - Eggs`,
      str_detect(`Items to Choose`, "Chicken") ~ `Value - Chicken`,
      str_detect(`Items to Choose`, "Potato") ~ `Value - Potato`,
      str_detect(`Items to Choose`, "Eggplant") ~ `Value - Eggplant`,
      str_detect(`Items to Choose`, "Onion") ~ `Value - Onion`,
      str_detect(`Items to Choose`, "Green Chilli") ~ `Value - Green Chilli`,
      TRUE ~ NA_real_
    ),
    Purchase_Options = case_when(
      str_detect(`Items to Choose`, "Rice") ~ `Rice Purchase Options`,
      str_detect(`Items to Choose`, "Flour") ~ `Flour Purchase Options`,
      str_detect(`Items to Choose`, "Lentil") ~ `Lentils Purchase Options`,
      str_detect(`Items to Choose`, "Soybean Oil") ~ `Soybean Oil Purchase Options`,
      str_detect(`Items to Choose`, "Salt") ~ `Salt Purchase Options`,
      str_detect(`Items to Choose`, "Sugar") ~ `Sugar Purchase Options`,
      str_detect(`Items to Choose`, "Chicken") ~ `Chicken Purchase Options`,
      str_detect(`Items to Choose`, "Potato") ~ `Potato Purchase Options`,
      TRUE ~ NA_character_
    ),
    Type = case_when(
      str_detect(`Items to Choose`, "Rice") ~ `Name of Rice`,
      str_detect(`Items to Choose`, "Sugar") ~ `Source of Sugar`,
      str_detect(`Items to Choose`, "Potato") ~ `Potato Purchase Options`,
      str_detect(`Items to Choose`, "Eggplant") ~ `Types of Eggplant`,
      str_detect(`Items to Choose`, "Onion") ~ `Source of Onion`,
      TRUE ~ NA_character_
    ),
    Type_of_Shop = case_when(
      str_detect(`Items to Choose`, "Rice") ~ `Type of Shop for Rice`,
      str_detect(`Items to Choose`, "Flour") ~ `Type of Shop for Flour`,
      str_detect(`Items to Choose`, "Lentil") ~ `Type of Shop for Lentils`,
      str_detect(`Items to Choose`, "Soybean Oil") ~ `Type of Shop for Soybean Oil`,
      str_detect(`Items to Choose`, "Salt") ~ `Type of Shop for Salt`,
      str_detect(`Items to Choose`, "Sugar") ~ `Type of Shop for Sugar`,
      str_detect(`Items to Choose`, "Eggs") ~ `Eggs Purchase Options`,
      str_detect(`Items to Choose`, "Chicken") ~ `Type of Shop for Chicken`,
      str_detect(`Items to Choose`, "Potato") ~ `Type of Shop for Potato`,
      str_detect(`Items to Choose`, "Eggplant") ~ `Type of Shop for Eggplant`,
      str_detect(`Items to Choose`, "Onion") ~ `Type of Shop for Onion`,
      str_detect(`Items to Choose`, "Green Chilli") ~ `Type of Shop for Green Chilli`,
      TRUE ~ NA_character_
    ),
    Name_Of_Outlet = case_when(
      str_detect(`Items to Choose`, "Rice") ~ `Name of Outlet for Rice`,
      str_detect(`Items to Choose`, "Flour") ~ `Name of Outlet for Flour`,
      str_detect(`Items to Choose`, "Lentil") ~ `Name of Outlet for Lentils`,
      str_detect(`Items to Choose`, "Soybean Oil") ~ `Name of Outlet for Soybean Oil`,
      str_detect(`Items to Choose`, "Salt") ~ `Name of Outlet for Salt`,
      str_detect(`Items to Choose`, "Sugar") ~ `Name of Outlet for Sugar`,
      str_detect(`Items to Choose`, "Eggs") ~ `Name of Outlet for Eggs`,
      str_detect(`Items to Choose`, "Chicken") ~ `Name of Outlet for Chicken`,
      str_detect(`Items to Choose`, "Potato") ~ `Name of Outlet for Potato`,
      str_detect(`Items to Choose`, "Eggplant") ~ `Name of Outlet for Eggplant`,
      str_detect(`Items to Choose`, "Onion") ~ `Name of Outlet for Onion`,
      str_detect(`Items to Choose`, "Green Chilli") ~ `Name of Outlet for Green Chilli`,
      TRUE ~ NA_character_
    )
  )%>%
  select(
    `Submission ID`, UserId, `Submission time`, DistrictName, UpazilaName,
    `Items to Choose`, Price, Purchase_Options, Type, Type_of_Shop, Name_Of_Outlet
  ) %>%
  arrange(`Submission ID`, `Items to Choose`) %>% 
  filter(Price != "NA")


View(Vegiehat_Full_Data_New)
