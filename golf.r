#Loading libraries
library("XML")
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
class(players)

# Converting official money to numeric
players$`Official Money`<- as.numeric(gsub("\\$|,","",players$`Official Money`))
str(players)

# Extracting player ID
playerProfileURLPath <- getHTMLLinks("golf.html", xpQuery = "//a[contains(@href,'players') and contains(@href,'overview')]/@href")
playerProfile <- data.frame(matrix(unlist(strsplit(playerProfileURLPath,"/")),nrow=length(playerProfileURLPath), byrow = TRUE),stringsAsFactors = FALSE) [,3:4]
names(playerProfile) <- c("Name","ID")

if(dim(playerProfile)[1] == dim(players)[1]){
  print("Numbers match")
} else {
  print(paste("Number of players found is ", dim(players)[1]," while number of URLs is ",dim(playerProfile)[1]))
}

# Add a player row manually. Row was originally ommitted due to bad link on the website
playerProfile <- add_row(playerProfile,Name = c("wei-ling-hsu"),ID =c("98290"), .after=30)

players <- add_column(players,as.numeric(playerProfile[,2]), .after = 1)
names(players)[2] <- c("ID")

str(players)

# Extracting Player Country
playerCountryImageURLPath <- getHTMLExternalFiles("golf.html", xpQuery = "//img[contains(@src,'countries')]/@src")

playerCountries <- toupper(str_match(playerCountryImageURLPath,"countries/(.*[a-z])\\.")[,2])

if(length(playerCountries) == dim(players)[1]){
  print("Numbers match")
} else {
  print(paste("Number of players found is ", dim(players)[1]," while number of nationalities is ",length(playerCountries)))
}

players <- add_column(players,as.factor(playerCountries), .after = "Name")
names(players)[4] <- c("Country")

str(players)

# Finding if there is a corellation between money and events played
ggplot(data = players, mapping = aes(x=players$`Events Played`, y=players$`Official Money`)) +
  geom_point() +
  scale_y_log10()

cor(players$`Events Played`,players$`Official Money`)

# seems like there is a positive corellation between events played and money made