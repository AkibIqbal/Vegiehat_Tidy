library(dplyr)
library(rvest)
library(purrr)
library(lubridate)
library(tidyr)

getPriceTable <- function(path_to_file){
  html_data <- read_html(
    x = path_to_file
  ) 
  
  col_1 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[1]") %>%
    html_text(trim = TRUE)
  
  col_2 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[2]") %>%
    html_text(trim = TRUE)
  
  col_3 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[3]") %>%
    html_text(trim = TRUE)
  
  col_4 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[4]") %>%
    html_text(trim = TRUE)
  
  col_5 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[5]") %>%
    html_text(trim = TRUE)
  
  col_6 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[6]") %>%
    html_text(trim = TRUE)
  
  col_7 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[7]") %>%
    html_text(trim = TRUE)
  
  col_8 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[8]") %>%
    html_text(trim = TRUE)
  
  col_9 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[9]") %>%
    html_text(trim = TRUE)
  
  col_9 = paste0(col_8, col_9, collapse = "_")
  
  col_10 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[10]") %>%
    html_text(trim = TRUE)
  
  col_11 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[11]") %>%
    html_text(trim = TRUE)
  
  col_12 <- html_data %>%
    html_node(xpath = "/html/body/table/thead/tr[13]/th[12]") %>%
    html_text(trim = TRUE)
  
  body <- html_data %>%
    html_node(xpath = "/html/body/table/tbody") %>%
    html_table(fill = TRUE)
  
  colnames(body) <- c(
    col_1, col_2, col_3, col_4, col_5, col_6, col_7, col_8, col_9, col_10, col_11, col_12
  )
  
  return(body)
}

fileNames <- dir(
  path = "./Govt Data",
  pattern = ".*\\.xls$",
  full.names = TRUE
)

dfRice <- 1:length(fileNames) %>%
  map_dfr(
    function(indx){
      getPriceTable(
        path_to_file = fileNames[indx]
      ) %>%
        mutate(
          priceDate = mdy(fileNames[indx])
        ) %>% 
        select(-c(6, 10, 11, 12)
        )  %>%
        separate(`Retail Price`, into = c("Lowest Reail Price", "Highest Reatil Price"), sep = "--")
    } 
  )

write.csv(dfRice,"C:\\Projects\\Vegiehat\\Vegiehat Govt Data\\Govt_rice_data_Dec_1_24_to_Feb_13_25.csv")
