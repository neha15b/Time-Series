

# Loading the packages
install.packages("sqldf")
install.packages("plotly")
install.packages("xts")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("forecast")
install.packages("timeSeries")
install.packages("ggpubr")
install.packages("qdapTools")
install.packages("fpp")
install.packages("urca")
install.packages("dplyr")
library(dplyr)
library(urca)
library(fpp)
library(sqldf)
library(plotly)
library(xts)
library(corrplot)
library(forecast)
library(ggplot2)
library(ggpubr)
library(qdapTools)

# Loading the data 
# bombing data
aerial=read.csv("/Users/nehabhandari/Desktop/operations.csv")
# first weather data that includes locations like country, latitude and longitude
weather_station_location=read.csv("/Users/nehabhandari/Desktop/Weather Station Locations.csv")
# Second weather data that includes measured min, max and mean temperatures
weather=read.csv("/Users/nehabhandari/Desktop/Summary of Weather.csv")

colnames(aerial)
colnames(weather_station_location)
colnames(weather)
str(aerial)
dim(aerial)
summary(aerial)


str(weather_station_location)
dim(weather_station_location)
summary(weather_station_location)

str(weather)
dim(weather)
summary(weather)

# Data CLeaning
#Check for na values in the data set
sapply(aerial, function(x) sum(is.na(x)))
sapply(weather_station_location, function(x) sum(is.na(x)))
sapply(weather, function(x) sum(is.na(x)))

#AERIAL DATA
# drop unused features
aerial = subset(aerial, select = -c(Mission.ID,Unit.ID,Mission.ID,Unit.ID,Target.ID,Altitude..Hundreds.of.Feet.,Airborne.Aircraft,
                                    Attacking.Aircraft, Bombing.Aircraft, Aircraft.Returned,
                                    Aircraft.Failed, Aircraft.Damaged, Aircraft.Lost,
                                    High.Explosives, High.Explosives.Type, Mission.Type,
                                    High.Explosives.Weight..Pounds.,High.Explosives.Weight..Tons.,
                                    Incendiary.Devices, Incendiary.Devices.Type,
                                    Incendiary.Devices.Weight..Pounds.,
                                    Incendiary.Devices.Weight..Tons.,Fragmentation.Devices,
                                    Fragmentation.Devices.Type, Fragmentation.Devices.Weight..Pounds.,
                                    Fragmentation.Devices.Weight..Tons.,Total.Weight..Pounds.,
                                    Total.Weight..Tons.,Time.Over.Target, Bomb.Damage.Assessment,Source.ID))
# Drop if takeoff longitude is NaN
aerial <- aerial[!is.na(aerial$Takeoff.Longitude),]
# drop if target longitude is NaN
aerial <- aerial[!is.na(aerial$Target.Longitude),]


# drop this takeoff latitude
aerial=subset(aerial, Takeoff.Latitude != 4248 )
# drop this takeoff longitude
aerial=subset(aerial, Takeoff.Longitude != 1355 )
#Check the data after cleaning
sapply(aerial, function(x) sum(is.na(x)))
#describing the data
str(aerial)
View(aerial)
# WEATHER LOCATION DATA
# drop unused features
weather_station_location = subset(weather_station_location,select = c(WBAN,NAME,STATE.COUNTRY.ID,Latitude,Longitude))
#Check the data after cleaning
sapply(weather_station_location, function(x) sum(is.na(x)))
#describing the data
str(weather_station_location)

#WEATHER DATA
# drop unused features
weather = subset(weather,select = c(STA,Date,MeanTemp))
#Check the data after cleaning
sapply(weather, function(x) sum(is.na(x)))

#describing the data
str(aerial)
str(weather)
str(weather_station_location)



#DATA VISUALIZATION
#TOP COUNTRIES WHICH ATTACK

sqldf('SELECT "Country", count(*) FROM aerial WHERE "Country" IS NOT NULL and "Country" !=""
      GROUP BY "Country"
      ORDER BY count(*) DESC')

ggplot(aerial, aes(Country)) +
  geom_bar(fill = "#0073C2FF")+
  theme_pubclean()


# Top target countries

sqldf('SELECT "Target.Country", count(*) FROM aerial WHERE "Target.Country" IS NOT NULL and 
      "Target.Country" != ""
      GROUP BY "Target.Country" 
      ORDER BY count(*) DESC
      LIMIT 10')       

plot1=ggplot(aerial, aes(Target.Country)) +
  geom_bar(fill = "#0073C2FF")+
  theme_pubclean()

plot1+theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 7),
            axis.text.y = element_text(face = "bold", color = "blue", 
                                       size = 7 ))+
  coord_flip()




#WHich AIRCRAFT SERIES is used more


airtop10=sqldf('SELECT "Aircraft.Series", count(*) as Count FROM aerial WHERE "Aircraft.Series" IS NOT NULL and
               "Aircraft.Series" !=""
               GROUP BY "Aircraft.Series" 
               ORDER BY count(*) DESC
               LIMIT 10')  

airtop10
# Plotting the frequency

ggplot(airtop10, aes(x=Aircraft.Series,y=Count)) +
  geom_bar(fill = "#0073C2FF",stat="identity")






#LETS visualize take off bases and locations of countries who attack

# geo styling
g <- list(
  scope = 'world',
  projection = list(type = 'Mercator'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

#Making Latitude numeric before the plotting
aerial$Takeoff.Latitude=as.numeric(aerial$Takeoff.Latitude)

#plotting
p <- plot_geo(aerial, sizes = c(1, 250)) %>%
  add_markers(
    x = ~Takeoff.Longitude, y = ~Takeoff.Latitude, color = ~Country, hoverinfo = "text",
    text = ~paste(paste("Country:",Country), paste("TakeoffLocation:",Takeoff.Location),paste("TakeoffBase:",Takeoff.Base ), sep = "<br />")
  ) %>%
  layout(title = 'Top countries who attack<br>(Hover for info)', geo = g)

p




#now lets visualize bombing paths which country from which take off base bomb  which countries and cities.
g1 <- list(
  scope = 'world',
  projection = list(type = 'Mercator'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)
p1 <- plot_geo(aerial, sizes = c(1, 250)) %>%
  add_markers(
    x = ~Takeoff.Longitude, y = ~Takeoff.Latitude, color = ~Country, hoverinfo = "text",
    text = ~paste(paste("Country:",Country), paste("TakeoffLocation:",Takeoff.Location),paste("TakeoffBase:",Takeoff.Base ), sep = "<br />")
  ) %>%
  
  add_markers(
    x = ~Target.Longitude, y = ~Target.Latitude, color = I("red"), hoverinfo = "text",
    text = ~paste(paste("TargetCountry:",Target.Country), paste("TargetCity:",Target.City), sep = "<br />")
  ) %>%
  layout(title = 'Bombing Paths from Attacker Country to Target', geo = g1)

p1<-add_trace(p1,
              x = ~Takeoff.Longitude, xend = ~Target.Longitude,
              y = ~Takeoff.Latitude, yend = ~Target.Latitude,
              alpha = 0.3, size = I(1),color = I("black"),mode = "markers+lines",type="scatter" )



p1




# PLOT the bombing attacks in different Theater of operations:

ope=sqldf('SELECT "Theater.of.Operations", count(*) as Count FROM aerial WHERE "Theater.of.Operations" IS NOT NULL and "Theater.of.Operations" !=""
          GROUP BY "Theater.of.Operations"
          ORDER BY count(*) DESC')

ope

# Plotting the frequency

ggplot(ope, aes(x=Theater.of.Operations,y=Count)) +
  geom_bar(fill = "#0073C2FF",stat="identity")+
  labs(title="TheaterOfOperations", 
       subtitle="No of Attacks")



# PLotting the weather stations locations


g2 <- list(
  scope = 'world',
  projection = list(type = 'Mercator'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

p2 <- plot_geo(weather_station_location, sizes = c(1, 250)) %>%
  add_markers(
    x = ~Longitude, y = ~Latitude, color = I("blue"), hoverinfo = "text",
    text = ~paste(paste("Name:",NAME), paste("Country:",STATE.COUNTRY.ID), sep = "<br />")
  ) %>%
  layout(title = 'Weather Station Locations<br>(Hover for Info)', geo = g2)

p2



#Focusing on USA and BURMA war
#In this war USA bomb BURMA( KATHA city) from 1942 to 1945.
#The closest weather station to this war is BINDUKURI and it has temperature record from 1943 to 1945.
#Now lets visualize this situation. 

View(weather_station_location)
View(weather)

#Getting WBAN id for Bindukuri
sqldf('SELECT "WBAN" FROM weather_station_location WHERE "NAME" = "BINDUKURI" ')

#Getting a subset for only Bindukuri
bar <- subset(weather, STA == "32907")



#Plotting Mean Temperature of Bindukuri Area
ggplot(data = bar, aes(x =as.Date(Date, "%Y-%m-%d"), y = MeanTemp)) +
  geom_line() +
  labs(x = "Date",
       y = "Temperature",
       title = "Mean Temp of Bindukuri Area")



#Plotting mean temperature that is measured in Bindukuri & bombing dates and bombing date temp
#in the same graph


# Reading the entire data set 
aerial2=read.csv("/Users/nehabhandari/Desktop/operations.csv") 
# Converting date from factor to date
aerial2$FormattedDate = as.Date(aerial2$Mission.Date, "%m/%d/%Y")
#Creating new colum year and month from Date
aerial2$year = as.numeric(format(aerial2$FormattedDate, "%Y"))
aerial2$month = as.numeric(format(aerial2$FormattedDate, "%m"))


# Keeping the data for year >=1943 and month>=8
aerial2=sqldf('SELECT * FROM aerial2 WHERE "year" >= 1943 ')
aerial2=sqldf('SELECT * FROM aerial2 WHERE "month" >= 8 ')

View(aerial2)

str(aerial2)

# Making a new data set for country:USA , Target Country:BURMA, Target City:KATHA
aerial_war=sqldf('SELECT * FROM aerial2 WHERE "Country" = "USA" ')

aerial_war=sqldf('SELECT * FROM aerial_war WHERE "Target.Country" = "BURMA" ')
aerial_war=sqldf('SELECT * FROM aerial_war WHERE "Target.City" = "KATHA" ')


#Getting mean temp compared to bombing date and mean temp on the same date
#creating lists to get the temp for the common dates
liste=list()



# Converting date from factor to date
bar$FormattedDate = as.Date(bar$Date, "%Y-%m-%d")

# converting into character format
aerial_war$FormattedDate=as.character(aerial_war$FormattedDate)
bar$FormattedDate=as.character(bar$FormattedDate)



#Getting column number for Formatted Date in the data set
which( colnames(aerial_war)=="FormattedDate" )
which( colnames(bar)=="FormattedDate" )

#creating a loop to go through every value of date in aerial_war & bar dataset to find a match
#in dates and get the corresponding temp for that date


for(i in 1:length(aerial_war$Mission.Date)) {
  for(j in 1:length(bar$Date)) {
    if(aerial_war[i,47]==(bar[j,4])){
      dummy=bar[j,4]
      df=fn$sqldf("select MeanTemp from bar where FormattedDate = '$dummy'")
      #liste=list(df$MeanTemp)
      #liste=append(liste,liste)
      liste=append(liste,df)
    }
  }
}

# creating a new column for temp in aerial_war

#converting list into data frame
mydf=list2df(liste)
str(mydf)
#adding X1 column from mydf into aerial_War as a new column
aerial_war$temp=mydf$X1
View(aerial_war)

# PLotting Mean Temperature --- Bombing Dates and Mean Temperature at this Date

ggplot(data = bar, aes(x =as.Date(FormattedDate, "%Y-%m-%d"), y = MeanTemp),colour=temp) +
  geom_line(color="blue") +
  labs(x = "Date",
       y = "Temperature",
       title = "Mean Temp of Bindukuri Area vs Bombing temp")+
  geom_point(data=aerial_war,aes(x =as.Date(FormattedDate, "%Y-%m-%d"),
                                 y = temp))

#As it can be seen from plot, USA bomb at high temperatures.
#The question is that can we predict future weather and according 
#to this prediction can we know whether bombing will be done or not.


#TIME SERIES
View(bar)
str(bar)
#creating time series graph for the weather data 


bar_ts=ts(bar$MeanTemp,start=c(1943,5,),end=c(1945),frequency = 365)
tsdisplay(bar_ts)
plot(bar_ts,col="red",ylab="Mean Temp")



# Checking for Stationarity of Time Series

#Now we will utilize the Augmented Dickey-Fuller Test for stationarity. 
#The null hypothesis states that large p values indicate non-stationarity 
#and smaller p values indicate stationarity. 
#(We will be using 0.05 as our alpha value.)

adf.test(bar_ts)

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. Here we will test the null hypothesis of trend
#stationarity (a low p-value will indicate a signal that is not trend stationary

kpss.test(bar_ts)

ur.df(bar_ts,type='trend', lags = 10, selectlags = "BIC")

#Decompose a Time Series
#we will break down our time series 
#into its seasonal component, trend, and residuals.
decomp_weather=decompose(bar_ts)
plot(decomp_weather)

#de seasonalize a time series
TseaAdj<-seasadj(decomp_weather)
plot(bar_ts)
lines(TseaAdj,col="dark red")

#PLOT ACF AND PACF
#ACF stands for "autocorrelation function"
#and PACF stands for "partial autocorrelation function.
par(mfrow=c(2,2))
acf(bar_ts)
pacf(bar_ts)
Acf(bar_ts)
Pacf(bar_ts)

#MAKING TIME SERIES STATIONARY

#DIFFERENCING
# will have  take the difference of our time series object.

nsdiffs(bar_ts)
tsDiff <- diff(bar_ts,differences=1)
plot(tsDiff)
ndiffs(tsDiff)



#Checking for Stationarity again after differencing
adf.test(tsDiff)

par(mfrow=c(2,2))

Acf(tsDiff)
Pacf(tsDiff)
acf(tsDiff)
pacf(tsDiff)
#FORECASTING THE VALUES USING DIFFERENT MODELS

#Arima( forecasting ,plotting and accuracy)
AF<- auto.arima(bar_ts)
AF
Arima_forecast<-forecast(AF, h=50)
plot(Arima_forecast)
summary(Arima_forecast)
#accuracy of the arima
accuracy(Arima_forecast)
#plotting the residuals

par(mfrow=c(2,3))

plot(Arima_forecast$residuals)
hist(Arima_forecast$residuals)

plot(Arima_forecast$fitted, Arima_forecast$residuals)
plot(bar_ts, Arima_forecast$residuals)
Acf(Arima_forecast$residuals)


#Holt Winters ( forecasting , plotting and accuracy)
HW <-HoltWinters(bar_ts)
HW_forecast<-forecast(HW,12)
plot(HW_forecast)
summary(HW_forecast)
accuracy(HW_forecast)

par(mfrow=c(2,3))

plot(HW_forecast$residuals)
hist(HW_forecast$residuals)

plot(HW_forecast$fitted, HW_forecast$residuals)
plot(bar_ts, HW_forecast$residuals)
Acf(HW_forecast$residuals)

#SIMPLE EXPONENTIAL SMOOTHING ( forecasting, plotting and accuracy)

SS=ses(bar_ts,12)
SS_forecast<-forecast(SS,12)
plot(SS_forecast)
summary(SS_forecast)
accuracy(SS_forecast)

par(mfrow=c(2,3))
plot(SS_forecast$residuals)
hist(SS_forecast$residuals)

plot(SS_forecast$fitted, SS_forecast$residuals)
plot(bar_ts, SS_forecast$residuals)
Acf(SS_forecast$residuals)

#SIMPLE MOVING AVERAGES

MA <- ma(bar_ts,order=6) #moving average of order 6
MA1 <- ma(bar_ts,order=9)
MA_forecast<-forecast(MA,12)
MA1_forecast<-forecast(MA1,12)
plot(MA_forecast)
plot(MA1_forecast)
summary(MA_forecast)
accuracy(MA_forecast)



par(mfrow=c(2,3))

plot(MA_forecast$residuals)
hist(MA_forecast$residuals)

plot(MA_forecast$fitted, MA_forecast$residuals)
plot(bar_ts, MA_forecast$residuals)
Acf(MA_forecast$residuals)

#NAIVE BAYES
NF<-naive(bar_ts)
NF_forecast<-forecast(NF,12)
plot(NF_forecast)
summary(NF_forecast)
accuracy(NF_forecast)

plot(NF_forecast$residuals)
hist(NF_forecast$residuals)

plot(NF_forecast$fitted, NF_forecast$residuals)
plot(bar_ts, NF_forecast$residuals)
Acf(NF_forecast$residuals)


#ETS
EF<-ets(bar_ts)
EF_forecast<-forecast(EF,12)
plot(EF_forecast)
summary(EF_forecast)
accuracy(EF_forecast)

plot(EF_forecast$residuals)
hist(EF_forecast$residuals)

plot(EF_forecast$fitted, EF_forecast$residuals)
plot(bar_ts, EF_forecast$residuals)
Acf(EF_forecast$residuals)

#

accNF<-as.table(accuracy(NF_forecast))
accEF<-as.table(accuracy(EF_forecast))


accHW<-as.table(accuracy(HW_forecast))
accSS<-as.table(accuracy(SS_forecast))
accMA<-as.table(accuracy(MA_forecast))
accAR<-as.table(accuracy(Arima_forecast))


accuracies <- as.data.frame(rbind(accMA,accSS,accHW,accAR)) %>% mutate(Method = c('Moving Average','Simple Smoothing','Holt Winter','Arima'))
accuracies

