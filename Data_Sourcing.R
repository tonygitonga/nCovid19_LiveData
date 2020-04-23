#------------------- Corona Virus Statistics in 2019 -------------------------------------------------
#-------------- Created by Antony Gitonga
#----------- agitonga01@outlook.com
rm(list = ls(all = TRUE))
#--------- Required libraries for data mining --------------------------------------
library(RCurl)
library(tidyr)
#------------ Required libraries for shiny ---------------------------
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(scales)
library(visNetwork)
library(ggrepel)
library(tidyr)
library(dplyr)
library(highcharter)
library(purrr)
library(magrittr)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(sf)
library(maps)
library(RColorBrewer)
library(data.table)
library(plotly)

#----------- Download Confirmed Cases data and transform it to a data frame
myfile <- getURL('https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
mydata <- read.csv(textConnection(myfile), header = T)

#-------------- Gather the dataset ---------------------
confirmedCases <- tidyr::gather(mydata, "Date", "Count", 5:dim(mydata)[2])
confirmedCases$Date <- gsub("X", "", confirmedCases$Date)
confirmedCases$Date <- as.Date(confirmedCases$Date, "%m.%d.%y")
colnames(confirmedCases) <- c("Province", "Country", "Lat", "Long", "Date", "Count")
confirmedCases <- confirmedCases[sort(order(confirmedCases$Date),decreasing = FALSE),]
max(confirmedCases$Date)
min(confirmedCases$Date)
tabConfirmed <- sum(confirmedCases[which(confirmedCases$Date == max(confirmedCases$Date)),]$Count)

#----------- Download Dead Cases data and transform it to a data frame
myfile <- getURL('https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
mydata <- read.csv(textConnection(myfile), header = T)

#-------------- Gather the dataset ---------------------
deadCases <- tidyr::gather(mydata, "Date", "Count", 5:dim(mydata)[2])
deadCases$Date <- gsub("X", "", deadCases$Date)
deadCases$Date <- as.Date(deadCases$Date, "%m.%d.%y")
colnames(deadCases) <- c("Province", "Country", "Lat", "Long", "Date", "Count")
deadCases <- deadCases[sort(order(deadCases$Date),decreasing = FALSE),]
max(deadCases$Date)
min(deadCases$Date)
tabDead<- sum(deadCases[which(deadCases$Date == max(deadCases$Date)),]$Count)

#----------- Download Recovered Cases data and transform it to a data frame
myfile <- getURL('https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv&filename=time_series_covid19_recovered_global.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
mydata <- read.csv(textConnection(myfile), header = T)

#-------------- Gather the dataset ---------------------
recoveredCases <- tidyr::gather(mydata, "Date", "Count", 5:dim(mydata)[2])
recoveredCases$Date <- gsub("X", "", recoveredCases$Date)
recoveredCases$Date <- as.Date(recoveredCases$Date, "%m.%d.%y")
colnames(recoveredCases) <- c("Province", "Country", "Lat", "Long", "Date", "Count")
recoveredCases <- recoveredCases[sort(order(recoveredCases$Date),decreasing = FALSE),]
max(recoveredCases$Date)
min(recoveredCases$Date)
tabRecovered <- sum(recoveredCases[which(recoveredCases$Date == max(recoveredCases$Date)),]$Count)

confirmedCases$Type <- rep("Infected", dim(confirmedCases)[1])
deadCases$Type <- rep("Dead", dim(deadCases)[1])
recoveredCases$Type <- rep("Recovered", dim(recoveredCases)[1])

allData <- rbind(confirmedCases, deadCases, recoveredCases)
# allData = plyr::ddply(allData, ~Province + Country + Lat + Long + Date + Type, function(x){x[which.max(x$Count),]})

allData_pt <- as.data.frame(allData[,c("Country", "Lat", "Long", "Date", "Count", "Type")])
allData_pt <- setNames(allData_pt, c("name", "lat", "lon", "date", "z", "type"))
allData_pt <- tibble::as_tibble(allData_pt)

Infected <- confirmedCases
Dead <- deadCases
Recovered <- recoveredCases

world_map <- map_data("world")
cntryPlots <- allData[,c("Country", "Count", "Type", "Date")]
names(cntryPlots)[names(cntryPlots) == "Country"] <- "region"
cntryPlots$region <- as.character(cntryPlots$region)
county_data <- left_join(cntryPlots, world_map, by = "region")

rm(allData)
rm(deadCases)
rm(confirmedCases)
rm(recoveredCases)
rm(cntryPlots)

# plot_data <- county_data[which(county_data$region == 'Kenya' & county_data$Date == max(county_data$Date) & county_data$Type == 'Infected'),]
# 
# 
# ggplot(plot_data, aes(long, lat, group = group)) + geom_polygon(aes(fill = Count), color = "white") +
#   scale_fill_viridis_c(option = "C", name = "Number of Cases") + 
#   labs(title = paste0("These are the cases", " in ", "Kenya"),caption  = paste0("Last updated on: ", max(plot_data$Date))) +
#   theme_minimal()
