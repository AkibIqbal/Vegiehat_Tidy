library(readxl)
library(tidyverse)
VEGIEHAT_Pilot_Database <- read_excel("C:/Projects/Vegiehat/VEGIEHAT-Pilot-Database.xlsx")
#View(VEGIEHAT_Pilot_Database)

colnames(VEGIEHAT_Pilot_Database)

Veg_Full_Data = VEGIEHAT_Pilot_Database %>% 
  select(
    `Submission ID`,UserId, `Submission time`, DistrictName, UpazilaName, `Items to Choose`
  ) %>% 
  separate_rows(
    `Items to Choose`, sep = ","
  ) %>% 
  mutate(
    `Items to Choose`  = str_trim(
      `Items to Choose`, side = "both"
    )
  ) %>% 
  distinct(
    `Submission ID`, `UserId`,`Submission time`, DistrictName, UpazilaName, `Items to Choose`
  ) %>% 
  select(`Submission ID`,UserId, `Submission time`, DistrictName, UpazilaName, , `Items to Choose`
  )%>% 
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
  ) 
  )%>%
  mutate(
    Price = NA
  )

#View(Veg_Full_Data)

