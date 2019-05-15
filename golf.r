#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("ggplot2")

# Initiate variable
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

for (y in year) {

  # Downloading html
  fileName <- paste("golf",y,".html")
  fileNameClean <- paste("golfClean",y,".html")
  path <- file.path("~","GitHub","ieseDataSciProject",fileName)
  download.file(paste(url,y), destfile = path)
  
  # Players scraped in table 
  colTypes <- c("numeric","character","character","numeric")
  players <- readHTMLTable(fileName, colClasses = colTypes, stringsAsFactors = FALSE, as.data.frame = TRUE, which = 1)
  names(players)[3:4] <- c("OfficialMoney", "EventsPlayed")
  str(players)
  
  # Converting official money to numeric
  players$`OfficialMoney`<- as.numeric(gsub("\\$|,","",players$`OfficialMoney`))
  str(players)
  
  # Identifying errors in player profile links and fixing 
  cleanHTML <- sapply(read_lines(fileName),str_replace, "(/players/.*) ","\\1-")
  write(cleanHTML, fileNameClean)
  
  # Reading HTML nodes 
  golfHTML <- read_html(fileNameClean)
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
  str(players)
  
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
  
  str(players)
  
  # Add year to main table
  
  players <- add_column(players, y, .before = "Rank")
  names(players)[1] <- c("Year")
  str(players)
  
  playersAllYears <- rbind(playersAllYears, players)
}

write_excel_csv(playersAllYears, file.path("~","GitHub","ieseDataSciProject","playersAllYears.csv"))
# Finding if there is a corellation between money and events played
# ggplot(data = players, mapping = aes(x=players$`Events Played`, y=players$`Official Money`)) +
#  geom_point() +
#  scale_y_log10()

# cor(players$`Events Played`,players$`Official Money`)

# seems like there is a positive corellation between events played and money made