#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("ggplot2")

# Downloading html

path <- file.path("~","GitHub","ieseDataSciProject","golf.html")
download.file('http://www.lpga.com/statistics/money/official-money?year=2018', destfile=
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

# Extracting player ID
golfHTML <- read_html("golfClean.html")
golfPlayers <- html_nodes(golfHTML, xpath = "//tbody/tr/td[contains(@class, 'table-content left')]/a")
golfPlayersURL <- unlist(html_attrs(golfPlayers))
playersID <- data.frame(matrix(unlist(strsplit(golfPlayersURL,"/")),nrow=length(golfPlayersURL), byrow = TRUE),stringsAsFactors = FALSE) [,4]

players <- add_column(players,as.numeric(playersID), .after = 1)
names(players)[2] <- c("ID")

str(players)

# Extracting Player Country
golfPlayersCountries <- html_nodes(golfHTML, xpath = "//tbody/tr/td[contains(@class, 'table-content left')]/a/div/img")
golfPlayersCountriesURL <- unlist(html_attrs(golfPlayersCountries))
playerCountries <- toupper(str_match(golfPlayersCountriesURL,"countries/(.*[a-z])\\.")[,2])

players <- add_column(players,as.factor(playerCountries), .after = "Name")
names(players)[4] <- c("Country")

str(players)

# Finding if there is a corellation between money and events played
# ggplot(data = players, mapping = aes(x=players$`Events Played`, y=players$`Official Money`)) +
#  geom_point() +
#  scale_y_log10()

# cor(players$`Events Played`,players$`Official Money`)

# seems like there is a positive corellation between events played and money made