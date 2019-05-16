#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("ggplot2")

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
  players$`OfficialMoney`<- as.numeric(gsub("\\$|,","",players$`OfficialMoney`))
  
  # Identifying erroneous spacing in player profile links, fixing
  if (str_detect(read_html(tableURL),"/players/.+ [a-z0-9/-]*>")){
    print(paste("Errors identified in table of year", y,"... Fixing!"))
    cleanHTML <- paste(sapply(readLines(tableURL),str_replace, "(/players/.+) ([a-z0-9/-]*>)","\\1-\\2"),collapse="")
    
    # Reading HTML nodes 
    golfHTML <- read_html(cleanHTML)
  } else {
    golfHTML <- read_html(tableURL)
  }
  
  # Reading profile links 
  golfPlayers <- html_nodes(golfHTML, xpath = "//tbody/tr/td[contains(@class, 'table-content left')]/a")
  
  # Identifying missing link attributes
  missingProfileLinks <- which(sapply(html_attrs(golfPlayers),is_empty)) -1
  
  # Extracting player ID 
  golfPlayersURL <- unlist(html_attrs(golfPlayers))
  playersID <- matrix(unlist(strsplit(golfPlayersURL,"/")),nrow=length(golfPlayersURL), byrow = TRUE) [,4]
  
  # Adding NAs for missing profiles
  for (i in missingProfileLinks){
    playersID <- append(playersID, NA, after = i)
  }
  
  # Adding player ID to main table
  players <- add_column(players,as.factor(playersID), .after = "Rank")
  names(players)[2] <- c("ID")
  
  # Reading HTML nodes for country flag links
  golfPlayersCountries <- html_nodes(golfHTML, xpath = "//tbody/tr/td[contains(@class, 'table-content left')]/a/div")
  
  # Identifying missing link attributes
  missingCountryLinks <- which(sapply(lapply(golfPlayersCountries,html_children),is_empty)) -1
  
  # Extracting player country
  golfPlayersCountriesURL <- unlist(html_attrs(html_children(golfPlayersCountries)))
  playersCountries <- toupper(str_match(golfPlayersCountriesURL,"countries/(.*[a-z])\\.")[,2])
  
  # Adding NAs for missing profiles
  for (i in missingCountryLinks){
    playersCountries <- append(playersCountries, NA, after = i)
  }
  
  # Adding Player countries to main table
  players <- add_column(players,as.factor(playersCountries), .after = "Name")
  names(players)[4] <- c("Country")
  
  # Add year to main table
  players <- add_column(players, y, .before = "Rank")
  names(players)[1] <- c("Year")
  
  # Add this years' players to main table
  playersAllYears <- rbind(playersAllYears, players)
}
str(playersAllYears)
write_excel_csv(playersAllYears, file.path("~","GitHub","ieseDataSciProject","playersAllYears.csv"))
