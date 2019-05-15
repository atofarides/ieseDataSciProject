#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("ggplot2")

# Downloading html

path <- file.path("~","GitHub","ieseDataSciProject","golf.html")
year <- c("2016")
download.file(paste('http://www.lpga.com/statistics/money/official-money?year=',year), destfile=
                path)

# Players scraped in table 
colTypes <- c("numeric","character","character","numeric")
players <- readHTMLTable("golf.html", colClasses = colTypes, stringsAsFactors = FALSE, as.data.frame = TRUE, which = 1)
str(players)

# Converting official money to numeric
players$`Official Money`<- as.numeric(gsub("\\$|,","",players$`Official Money`))
str(players)

# Identifying errors in player profile links and fixing 
cleanHTML <- sapply(read_lines("golf.html"),str_replace, "(/players/.*) ","\\1-")
write(cleanHTML, "golfClean.html")

# Reading HTML nodes 
golfHTML <- read_html("golfClean.html")
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
players <- add_column(players,as.numeric(playersID), .after = "Rank")
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

# Finding if there is a corellation between money and events played
# ggplot(data = players, mapping = aes(x=players$`Events Played`, y=players$`Official Money`)) +
#  geom_point() +
#  scale_y_log10()

# cor(players$`Events Played`,players$`Official Money`)

# seems like there is a positive corellation between events played and money made