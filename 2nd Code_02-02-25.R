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
    `Value - Green Chilli`
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
  select(
    `Submission ID`, UserId, `Submission time`, DistrictName, UpazilaName,
    `Items to Choose`, hasProduct, Price
  ) %>%
  arrange(`Submission ID`, hasProduct) %>% 
  filter(Price != "NA")


View(Veg_Full_Data)

write.csv(Veg_Full_Data, "Veg_Full_Data.csv", row.names = FALSE)
