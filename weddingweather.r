## Airport URL format http://www.wunderground.com/history/airport/LGA/2015/10/19/PlannerHistory.html?dayend=25&monthend=10&yearend=2015&format=1

weddingweather <- function(weddate = "10/22/2015",airportcode = "LGA"){

# Install and load necessary packages
# install.packages("dplyr","lubridate")
library(dplyr)
library(lubridate)

weddate <- mdy(weddate)
mintest <- weddate - days(3)
maxtest <- weddate + days(3)



testurl <- paste('http://www.wunderground.com/history/airport/'
     ,airportcode
     ,'/'
	 ,year(mintest)
	 ,'/'
	 ,month(mintest)
	 ,'/'
	 ,mday(mintest)
	 ,'/PlannerHistory.html?dayend='
	 ,mday(maxtest)
	 ,'&monthend='
	 ,month(maxtest)
	 ,'&yearend='
	 ,year(maxtest)
	 ,'&format=1'
     ,sep=''
	 )

mydf <- read.csv(testurl)

options(warn=-1)

# Print everything
temp <- paste ('5pm temp:',round(mean(mydf$Max.TemperatureF) * 2/3 + mean(mydf$Mean.TemperatureF) / 3,0),'degrees')
print(temp)
temp <- paste ('10pm temp:',round(mean(mydf$Mean.TemperatureF) * 5/6 + mean(mydf$Mean.TemperatureF) / 6,0),'degrees')
print(temp)
temp <- paste ('Chance of precipitation:', 
              round(
             nrow(mydf[(
			 (!is.na(mydf$Events) &
			 (mydf$Events == 'Fog-Rain'
			 | mydf$Events == 'Fog-Rain-Snow'
			 | mydf$Events == 'Rain'
			 | mydf$Events == 'Rain-Snow'
			 | mydf$Events == 'Rain-Thunderstorm'
			 | mydf$Events == 'Snow'))
			 | (!is.na(mydf$PrecipitationIn) & 
			 (mydf$PrecipitationIn == 'T'
			 | as.numeric(paste(mydf$PrecipitationIn)))) > 0),])
             / nrow(mydf) * 100,0)
             , 'percent')

print(temp)

options(warn=0)

}
	 
