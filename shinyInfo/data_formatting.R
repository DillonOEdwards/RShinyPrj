library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)


crimes <- read.csv(file='Crimes2021.csv')

crimes2 <- crimes

crimes2 <- tidyr::separate(data = crimes2,
                           col = Date,
                           sep = " ",
                           into = c("Day","Time","AM/PM"),
                           remove = FALSE)


# Get Days of week
crimes2$weekday <- wday(as.Date(crimes2$Day, format = "%m/%d/%Y"),label=TRUE)



crimes2 %>% group_by(Day)

crimes2 %>% group_by(Day) %>% summarise(x = count(Day))

sumcrime = table(crimes2$Day)

ctcrime = crimes2 %>% count(crimes2$Day)

ctcrime = ctcrime %>% rename(exe = ctcrime$crimes2$Day)


ctcrime %>% ggplot(data = ctcrime, mapping = aes(x=crimes2$Day,y=n))



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
barplot(height=pctArrest$percent,names=pctArrest$`crimeArrest$Primary.Type`,col='#69b3a2', horiz=T , las=1,cex.axis=1.2,cex.names = 0.45,main="Percentage of Arrests")


crimeWards = crimes2 %>% count(crimes2$Ward)
barplot(height=crimeWards$n,names=crimeWards$`crimes2$Ward`,col='#69b3a2', horiz=T , las=1)
# Something to add- population density:


#Subset based on a crime:
specificCrime = crimes2 %>% filter(crimes2$Primary.Type == 'BATTERY')
countSpecific = specificCrime %>% count(specificCrime$Description)
countSpecific <- countSpecific[order(countSpecific$n),]
barplot(height=countSpecific$n,names=countSpecific$`specificCrime$Description`,
        col='#69b3a2',space=0.2,xlab="Count",ylab="x", horiz=T , las=1)








