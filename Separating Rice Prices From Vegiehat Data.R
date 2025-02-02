library(readxl)
library(tidyverse)
VEGIEHAT_Pilot_Database <- read_excel("C:/Projects/Vegiehat/VEGIEHAT-Pilot-Database.xlsx")
#View(VEGIEHAT_Pilot_Database)

dfRice = VEGIEHAT_Pilot_Database %>% 
  filter(
    !is.na(`Items to Choose`),
    nchar(`Items to Choose`)>0, 
    nchar(`Submission ID`)>0,
    nchar(`Submission time`)>0,
    nchar(DistrictName)>0,
    nchar(UpazilaName)>0,
  ) %>% 
  select(
    `Submission ID`,UserId, `Submission time`, DistrictName, UpazilaName, `Items to Choose`, `Value - Rice`
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
    `Submission ID`, `UserId`,`Submission time`, DistrictName, UpazilaName, `Items to Choose`, `Value - Rice`
  ) %>% 
  mutate(hasRice = ifelse(`Items to Choose` == "Rice", 1, 0)
  ) %>% 
  pivot_wider(
    names_from = `Items to Choose`,
    values_from = hasRice, 
    values_fill = list(hasRice = 0)
  )%>%
  select(`Submission ID`,UserId, `Submission time`, DistrictName, UpazilaName, `Rice`, `Value - Rice`
  ) %>% 
  filter(
    Rice != 0
  )%>% 
  select(
    `Submission ID`,UserId, `Submission time`, DistrictName, UpazilaName, `Rice`,`Value - Rice`
  )
#View(dfRice)

write.csv(dfRice,"C:\\Projects\\Vegiehat\\Rice.csv") 

