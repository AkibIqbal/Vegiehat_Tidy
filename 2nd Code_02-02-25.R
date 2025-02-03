library(readxl)
library(tidyverse)
VEGIEHAT_Pilot_Database <- read_excel(".\\VEGIEHAT-Pilot-Database.xlsx")
#View(VEGIEHAT_Pilot_Database)

colnames(VEGIEHAT_Pilot_Database)

Veg_Full_Data = VEGIEHAT_Pilot_Database %>% 
  select(
    `Submission ID`, UserId, `Submission time`, DistrictName, UpazilaName, 
    `Items to Choose`, `Value - Rice`, `Value - Flour`, `Value - Lentil`, 
    `Value - Soybean Oil`, `Value - Salt`, `Value - Sugar`, `Value - Eggs`, 
    `Value - Chicken`, `Value - Potato`, `Value - Eggplant`, `Value - Onion`, 
    `Value - Green Chilli`, `Rice Purchase Options`, `Flour Purchase Options`,
     `Lentils Purchase Options`, `Soybean Oil Purchase Options`, `Salt Purchase Options`,
     `Sugar Purchase Options`, `Eggs Purchase Options`, `Chicken Purchase Options`,
     `Potato Purchase Options`, `Type of Shop for Rice`, `Type of Shop for Flour`, 
    `Type of Shop for Lentils`, `Type of Shop for Soybean Oil`, `Type of Shop for Salt`, 
    `Type of Shop for Sugar`, `Type of Shop for Chicken`, 
    `Type of Shop for Potato`, `Type of Shop for Eggplant`, `Type of Shop for Onion`, 
    `Type of Shop for Green Chilli`
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
  ) %>%
  mutate(
    `Purchase Options` = case_when(
      hasProduct == 1 ~ `Rice Purchase Options`,
      hasProduct == 2 ~ `Flour Purchase Options`,
      hasProduct == 3 ~ `Lentils Purchase Options`,
      hasProduct == 4 ~ `Soybean Oil Purchase Options`,
      hasProduct == 5 ~ `Salt Purchase Options`,
      hasProduct == 6 ~ `Sugar Purchase Options`,
      hasProduct == 7 ~ `Eggs Purchase Options`,
      hasProduct == 8 ~ `Chicken Purchase Options`,
      hasProduct == 9 ~ `Potato Purchase Options`,
      TRUE ~ NA_character_

    )
  )  %>%
  mutate(
    ShopType = case_when(
      hasProduct == 1 ~ `Type of Shop for Rice`,
      hasProduct == 2 ~ `Type of Shop for Flour`,
      hasProduct == 3 ~ `Type of Shop for Lentils`,
      hasProduct == 4 ~ `Type of Shop for Soybean Oil`,
      hasProduct == 5 ~ `Type of Shop for Salt`,
      hasProduct == 6 ~ `Type of Shop for Sugar`,
      hasProduct == 8 ~ `Type of Shop for Chicken`,
      hasProduct == 9 ~ `Type of Shop for Potato`,
      hasProduct == 10 ~ `Type of Shop for Eggplant`,
      hasProduct == 11 ~ `Type of Shop for Onion`,
      hasProduct == 12 ~ `Type of Shop for Green Chilli`,
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    `Submission ID`, UserId, `Submission time`, DistrictName, UpazilaName,
    `Items to Choose`, hasProduct, Price, `Purchase Options`, ShopType
  ) %>%
  arrange(`Submission ID`, hasProduct) %>% 
  filter(Price != "NA")



View(Veg_Full_Data)

write.csv(Veg_Full_Data, "Veg_Full_Data.csv", row.names = FALSE)
