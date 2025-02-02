library(readxl)
library(tidyverse)
VEGIEHAT_Pilot_Database <- read_excel("C:/Projects/Vegiehat/VEGIEHAT-Pilot-Database.xlsx")
#View(VEGIEHAT_Pilot_Database)


dfItemsChosen <- VEGIEHAT_Pilot_Database %>%
  filter(
    !is.na(`Items to Choose`),
    nchar(`Items to Choose`) > 0,
    nchar(UserId) > 0
  ) %>%
  select(
    `Submission ID`, UserId, `Items to Choose`
  ) %>%
  separate_rows(
    `Items to Choose`, sep = ","
  ) %>%
  mutate(
    `Items to Choose` = str_trim(
      `Items to Choose`, 
      side = "both"
    )
  ) %>%
  distinct(
    `Submission ID`, UserId, `Items to Choose`
  ) %>%
  mutate(
    itemChosen = 1
  ) %>%
  pivot_wider(
    names_from = `Items to Choose`,
    values_from = itemChosen,
    values_fill = list(itemChosen = 0)
  )

View(dfItemsChosen)
