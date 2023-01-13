library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(leaflet)
library(rgdal)
library(sp)
library(grid)
library(data.table)



leafMap <- leaflet(data = crimes2) %>%
  setView(lat = 41.74294, lng = -87.58578, zoom=3) %>%
  addTiles() %>%
  addMarkers(-87.58578, 41.74294)
leafMap

crimes <- read.csv(file='Crimes2021.csv')

crimes2 <- crimes

crimes2 <- tidyr::separate(data = crimes2,
                           col = Date,
                           sep = " ",
                           into = c("Day","Time","AM/PM"),
                           remove = FALSE)


# Get Days of week
crimes2$weekday <- wday(as.Date(crimes2$Day, format = "%m/%d/%Y"),label=TRUE)


# Get Times
crimes2$hour <-sapply(strsplit(crimes2$Time,":"),'[',1)

crimes2$hourZ <- ifelse(crimes2$`AM/PM`== 'PM',as.numeric(crimes2$hour)+12, as.numeric(crimes2$hour))



crimes2 %>% group_by(Day)

crimes2 %>% group_by(Day) %>% summarise(x = count(Day))

sumcrime = table(crimes2$Day)

ctcrime = crimes2 %>% count(crimes2$Day)

ctcrime = ctcrime %>% rename(exe = ctcrime$crimes2$Day)


ctcrime %>% ggplot(data = ctcrime, mapping = aes(x=crimes2$Day,y=n))


# Get Month
crimes2$month <- format(as.Date(crimes2$Day, '%m/%d/%y'), '%m')

crimes2$month2 <- month.abb[as.numeric(crimes2$month)]

myvec = c(5,7,2,4)

#Filtered values
dummy = filter(crimes2, Primary.Type == "NARCOTICS")
ctdummy = dummy %>% count(dummy$Day)
barplot(ctdummy$n)


#Filtered Days of Week
dummy = filter(crimes2, Primary.Type == "NARCOTICS")
ctdummy = dummy %>% count(dummy$weekday)
barplot(ctdummy$n)


#Bar plot of different crimes
totalcrime = crimes2 %>% count(crimes2$Primary.Type)
totalcrime <- totalcrime[order(totalcrime$n),]
barplot(height=totalcrime$n,names=totalcrime$`crimes2$Primary.Type`,
        col='#69b3a2', horiz=T , las=1,cex.axis=1.2,cex.names = 0.45,main="Total Crimes")

ggplot(data = totalcrime, 
                      mapping = aes(x = totalcrime$`crimes2$Primary.Type`,
                                    y = totalcrime$n)) + geom_bar(stat="identity")




#Calculate percentage of arrests
crimeArrest = filter(crimes2, Arrest == "true")
totalArrest = crimeArrest %>% count(crimeArrest$Primary.Type)
totalArrest <- totalArrest[order(totalArrest$n),]
barplot(height=totalArrest$n,names=totalArrest$`crimeArrest$Primary.Type`,col='#69b3a2', horiz=T , las=1)

# Get percentage of arrests for a crime
pctArrest = merge(totalArrest,totalcrime,by.x=c("crimeArrest$Primary.Type"),by.y=c("crimes2$Primary.Type"))
pctArrest$percent = pctArrest$'n.x' / pctArrest$'n.y'
pctArrest <- pctArrest[order(pctArrest$percent),]
par(mar = c(3, 9, 2, 1) + 0.1)
barplot(height=pctArrest$percent,names=pctArrest$`crimeArrest$Primary.Type`,col='#69b3a2', horiz=T , las=1,cex.axis=1.2,cex.names = 0.45,main="Percentage of Arrests")


crimeWards = crimes2 %>% count(crimes2$Ward)
par(mar = c(3, 8, 4, 1) + 0.1)
barplot(height=crimeWards$n,names=crimeWards$`crimes2$Ward`,col='#69b3a2', horiz=T , las=1)
# Something to add- population density:


#Subset based on a crime:
specificCrime = crimes2 %>% filter(crimes2$Primary.Type == 'BATTERY')
countSpecific = specificCrime %>% count(specificCrime$Description)
countSpecific <- countSpecific[order(countSpecific$n),]
par(mar = c(3, 8, 4, 1) + 0.1)
barplot(height=countSpecific$n,names=countSpecific$`specificCrime$Description`,
        col='#69b3a2',space=0.2,xlab="Count",ylab="x", horiz=T , las=1)

dummydummy = crimes2 %>% count(crimes2$month2)
dummydummy = dummydummy %>% arrange(factor(`crimes2$month2`, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov','Dec','zzz')))
barplot(height=dummydummy$n, names=dummydummy$`crimes2$month2`, col='#69b3a2',las=2)

# Spatial data

crimes2full = crimes2 %>% filter(!is.na(Longitude))
crimes2full = crimes2full %>% filter(!is.na(Latitude))
crime_dat = as.data.table(crimes2full)

coordinates(crime_dat) = c("Longitude","Latitude")
crs.geol = CRS("+proj=longlat")
proj4string(crime_dat) = crs.geol
plot(crime_dat, pch = 20, col = "steelblue")

#Ignore this for now
latlong <- names(crimes2) %in% c("Latitude", "Longitude")
latlong2 <-crimes2[latlong]
latlong2 = latlong2[,c("Longitude", "Latitude")]
latlong2 = latlong2[complete.cases(latlong2),]

coordinates(latlong2) = c("Longitude","Latitude")
crs.geol = CRS("+proj=longlat")
proj4string(latlong2) = crs.geol
#Stop Ignoring


plot(latlong2, pch = 20, col = "steelblue")

chicago = readOGR(dsn = "./boundaries", layer = "geo_export_421e64a3-4ffd-4e4e-a31a-2c9310af0244")
chicago

proj4string(chicago) = crs.geol

chi_agg = aggregate(x=crime_dat,by=chicago,FUN=length)
qpal = colorBin("Reds", chi_agg$Location, bins=4)


leaflet(chi_agg) %>% addPolygons(stroke = TRUE, opacity = 1, fillOpacity = 0.5,
                                 smoothFactor = 0.5, color = "black",
                                 fillColor = ~qpal(Location),
                                 weight = 1) %>% addLegend(values=~location,pal=qpal,title="Crimes")
  


leaflet() %>% addTiles()



