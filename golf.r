#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")

# Initiate global variables
playersAllYears <- data.frame(
  Year = numeric(),
  Rank = numeric(),
  ID = factor(),
  Name = character(),
  Country = factor(),
  OfficialMoney = numeric(),
  EventsPlayed = numeric()
)

year <- seq(from = 2018, to = 2000, by = -1)
url <- c('http://www.lpga.com/statistics/money/official-money?year=')
colTypes <- c("numeric","character","character","numeric")

for (y in year) {
  print(paste("Processing data for year",y))

  # Defining working URL 
  tableURL <- paste(url,y,sep = "")
  
  # Players' Table Text scraped
  players <- readHTMLTable(tableURL, colClasses = colTypes, stringsAsFactors = FALSE, as.data.frame = TRUE, which = 1)
  names(players)[3:4] <- c("OfficialMoney", "EventsPlayed")
  
  # Converting official money to numeric to enable analysis
  players$`OfficialMoney`%<>% gsub("\\$|,","",.) %>%
                            as.numeric()  
  
  # Identifying erroneous spacing in player profile links, fixing
  if (str_detect(read_html(tableURL),"/players/.+ [a-z0-9/-]*>")){
    print(paste("Errors identified in table of year", y,"... Fixing!"))
    
    # Reading HTML nodes 
    golfHTML <- readLines(tableURL) %>%
                sapply(.,str_replace, "(/players/.+) ([a-z0-9/-]*>)","\\1-\\2") %>%
                paste(.,collapse = "") %>%
                read_html()
  } else {
    golfHTML <- read_html(tableURL)
  }
  
  # Reading profile links 
  golfPlayersNodes <- html_nodes(golfHTML, xpath = "//tbody/tr/td[contains(@class, 'table-content left')]/a")
  
  # Identifying missing link attributes
  missingProfileLinks <- html_attrs(golfPlayersNodes) %>%
                        sapply(.,is_empty) %>%
                        which()-1 
  
  # Extracting player ID 
  playersID <- html_attrs(golfPlayersNodes) %>%
              unlist() %>%
              strsplit(.,"/") %>%
              unlist() %>% 
              matrix(.,nrow=(length(golfPlayersNodes)-length(missingProfileLinks)), byrow = TRUE)
  playersID <- playersID[,4]
  
  # Adding NAs for missing profiles
  for (i in missingProfileLinks){
    playersID %<>% append(., NA, after = i)
  }
  
  # Adding player ID to main table
  players %<>% add_column(.,as.factor(playersID), .after = "Rank")
  names(players)[2] <- c("ID")
  
  # Reading HTML nodes for country flag links
  golfPlayersCountriesNodes <- html_nodes(golfHTML, xpath = "//tbody/tr/td[contains(@class, 'table-content left')]/a/div")
  
  # Identifying missing link attributes
  missingCountryLinks <- lapply(golfPlayersCountriesNodes,html_children) %>%
                        sapply(.,is_empty) %>%
                        which()-1
  
  # Extracting player country
  playersCountries <- html_children(golfPlayersCountriesNodes) %>%
                      html_attrs() %>%
                      unlist() %>%
                      str_match(.,"countries/(.*[a-z])\\.") %>%
                      toupper(.)
  playersCountries <- playersCountries[,2]
  
  # Adding NAs for missing profiles
  for (i in missingCountryLinks){
    playersCountries %<>% append(., NA, after = i)
  }
  
  # Adding Player countries to main table
  players %<>% add_column(.,as.factor(playersCountries), .after = "Name")
  names(players)[4] <- c("Country")
  
  # Add year to main table
  players %<>% add_column(., y, .before = "Rank")
  names(players)[1] <- c("Year")
  
  # Add this years' players to main table
  playersAllYears %<>% rbind(., players)
}
str(playersAllYears)
write_excel_csv(playersAllYears, file.path("~","GitHub","ieseDataSciProject","playersAllYears.csv"))
