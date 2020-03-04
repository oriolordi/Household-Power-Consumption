# Code for Module 3 Task 1 by Oriol Ordi

# Load libraries ####
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
# require() gives a warning message and returns FALSE if the requested package is not found
# load al the necessary libraries with pacman
pacman::p_load(RMySQL,
               tidyverse,
               lubridate,
               ggplot2,
               ggfortify,
               scales,
               forecast,
               plotly,
               reshape,
               prophet,
               imputeTS # used to replace na's in time series data
               )



# Function definitions ####

# Function to plot the time series predictions more beautifully ####
plotforecast <- function(forecast, h = 1, title = ""){
  
  df1 <- as_data_frame(fortify(forecast)) # tibble object
  #df1 <- fortify(fut) # data frame object
  
  # Create Date column, remove Index column and rename other columns 
  df1 %<>% 
    mutate(Date = as.Date(Index, "%Y-%m-%d")) %>% 
    select(-Index) %>% 
    dplyr::rename("Low95" = "Lo 95",
                  "Low80" = "Lo 80",
                  "High95" = "Hi 95",
                  "High80" = "Hi 80",
                  "Forecast" = "Point Forecast")
  
  lastNonNAinData <- max(which(complete.cases(df1$Data)))
  df1[lastNonNAinData, !(colnames(df1) %in% c("Data", "Fitted", "Date"))] <- df1$Fitted[lastNonNAinData]
  df1 <- df1[c(1:(lastNonNAinData+h)),]
  
  ggplot(df1, aes(x = Date)) + 
    geom_ribbon(aes(ymin = Low95, ymax = High95, fill = "95%"),fill='dark green',alpha=0.2) +
    geom_ribbon(aes(ymin = Low80, ymax = High80, fill = "80%"),fill='dark green',alpha=0.4) +
    geom_point(aes(y = Data, colour = "Consumption"), size = 3) +
    geom_line(aes(y = Data, group = 1, colour = "Consumption"), 
              linetype = "dotted", size = 0.75) +
    geom_line(aes(y = Fitted, group = 2, colour = "Prediction"), size = 0.75) +
    geom_line(aes(y = Forecast, group = 3, colour = "Forecast"), size = 0.75) +
    geom_vline(xintercept=as.numeric(df1[lastNonNAinData,'Date']),  lty=2) +
    scale_x_date(breaks = scales::pretty_breaks(), date_labels = "%b %y") +
    scale_colour_brewer(name = "Legend", type = "qual", palette = "Dark2") +
    scale_fill_brewer(name = "Intervals") +
    guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
    theme_bw(base_size = 14) +
    ylab('Power consumption (kWh)') +
    ggtitle(title)
}

# Function to plot the Forecast errors more beautifully ####
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# Function to 
decomp.plot <- function(x, main, ...)
{
  if(missing(main))
    main <- paste("Decomposition of", x$type, "time series")
  plot(cbind(observed = x$random + if (x$type == "additive")
    x$trend + x$seasonal
    else x$trend * x$seasonal, trend = x$trend, seasonal = x$seasonal,
    random = x$random), main = main, ...)
}



# Start of the code ####

# Turn off the system locale for the POSIXct time. This will be especially useful when extracting weekday names or month names ####
Sys.setlocale("LC_ALL", "C")

# Create SQL connection and query the data ####
con <- dbConnect(MySQL(),user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', 
                 host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
dbListTables(con)
dbListFields(con,'yr_2006')
yr_2006 <- dbGetQuery(con,'SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power FROM yr_2006')
yr_2007 <- dbGetQuery(con,'SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power FROM yr_2007')
yr_2008 <- dbGetQuery(con,'SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power FROM yr_2008')
yr_2009 <- dbGetQuery(con,'SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power FROM yr_2009')
yr_2010 <- dbGetQuery(con,'SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_active_power FROM yr_2010')

# Initial analysis of the Queried dataframes ####
str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)
str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)
str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)
str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)
str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

# Join all the years in a single dataframe ####
allyears <- bind_rows(yr_2006,yr_2007,yr_2008,yr_2009,yr_2010)

# Calculate the power consumed by the rest of the house, by converting the Global_active_power from kW to Wh and extracting the 3 submeters to the global power ####
allyears$Sub_metering_house <- allyears$Global_active_power * 1000 / 60 - allyears$Sub_metering_1 - allyears$Sub_metering_2 - allyears$Sub_metering_3

# Rearrange the column order and add the Global active power ####
allyears <- allyears[,c(1:(ncol(allyears)-2), ncol(allyears), (ncol(allyears)-1))]
allyears$Global_active_power <- allyears$Global_active_power * 1000 / 60

# Convert the Date and Time character columns into a single DateTime POSIXct column ####
## Combine Date and Time attribute values in a new attribute column
allyears <-cbind(allyears,paste(allyears$Date,allyears$Time), stringsAsFactors=FALSE)
## Give the new attribute in the last column a header name
colnames(allyears)[ncol(allyears)] <-"DateTime"
## Move the DateTime attribute within the dataset
allyears <- allyears[,c(ncol(allyears), 1:(ncol(allyears)-1))]
str(allyears)
## Convert DateTime to POSIXct
allyears$DateTime <- as.POSIXct(allyears$DateTime, "%Y-%m-%d %H:%M:%S")
## Add the time zone
attr(allyears$DateTime, "tzone") <- "Europe/Paris"
tz(allyears$DateTime)
str(allyears)

# Get rid of the Date and Time character columns and change the name of the energy consumption columns ####
## Get rid of Date and Time and change the data frame name
df <- allyears[c(-2,-3)]
## Change the names of the energy consumption columns ##
names <- colnames(df)
names[2:6] <- c('Kitchen','Laundry','ACWH','Other','Total')
colnames(df) <- names

# Fill the missing rows (minutes) with the same value of the last available previous week ####
## Add the rows that are missing
df_test <- df_test %>%
  complete(DateTime = seq.POSIXt(min(DateTime), max(DateTime), by="min"))
## Save the indices rows that have been added (which contain NA's in all columns (except DateTime))
missing_rows_ind <- vector()
missing_rows_ind <- which(is.na(df_test$Total))
## For each row with NA's, go to the last non NA week at the exact same time and copy the values (or the next week if previous week doesn't exist)
weektime <- 60*24*7
for (i in 1:length(missing_rows_ind)){
  j <- 1
  while (is.na(df[missing_rows_ind[i] - (j*weektime),'Total'])){
    j <- j + 1
    if (missing_rows_ind[i] - (j*weektime) < 1){
      break
    }
  }
  if (missing_rows_ind[i] - (j*weektime) < 1){
    j <- 1
    while (is.na(df[missing_rows_ind[i] + (j*weektime),'Total'])){
      j <- j + 1
    }
    df[missing_rows_ind[i],c('Kitchen','Laundry Room','AC and Water heater','Other','Total')] <- df[missing_rows_ind[i] + (j*weektime),c('Kitchen','Laundry Room','AC and Water heater','Other','Total')]
    next
  }
  df[missing_rows_ind[i],c('Kitchen','Laundry Room','AC and Water heater','Other','Total')] <- df[missing_rows_ind[i] - (j*weektime),c('Kitchen','Laundry Room','AC and Water heater','Other','Total')]
}

# Convert from Wh to KWh ####
df[,c('Kitchen','Laundry','ACWH','Other','Total')] <- df[,c('Kitchen','Laundry','ACWH','Other','Total')] / 1000

# Add columns for various time periods to later group by ####
df$year <- year(df$DateTime)
df$quarter <- quarter(df$DateTime)
df$month <- month(df$DateTime)
df$monthname <- month(df$DateTime,label = TRUE, abbr = FALSE)
df$yearmonth <- paste(df$monthname,df$year)
df$week <- week(df$DateTime)
df$weekday <- weekdays(df$DateTime)
df$weekday <- factor(df$weekday, ordered = TRUE, levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
df$day <- day(df$DateTime)
df$monthday <- paste(df$month,df$day)
df$hour <- hour(df$DateTime)
df$minute <- minute(df$DateTime)

# Remove the year 2006 from the dataset ####
df <- filter(df, year != 2006)

# Calculate the average consumption of monthly, daily and hourly data of the years 2007-2009 ####
## Monthly average
monthly_average <- df %>% 
  group_by(year,monthname) %>% 
  filter(year != 2010) %>% 
  summarize(Kitchen = sum(Kitchen), Laundry=sum(Laundry),ACWH=sum(ACWH), Other=sum(Other)) %>%
  group_by(monthname) %>% 
  summarize(Kitchen = mean(Kitchen),Laundry=mean(Laundry),ACWH=mean(ACWH),Other=mean(Other))
monthly_average
## Daily average
daily_average <- df %>% 
  group_by(year,month,day) %>% 
  filter(year != 2010) %>% 
  summarize(weekday = min(weekday), Kitchen = sum(Kitchen), Laundry=sum(Laundry),ACWH=sum(ACWH), Other=sum(Other)) %>%
  group_by(weekday) %>% 
  summarize(Kitchen = mean(Kitchen),Laundry=mean(Laundry),ACWH=mean(ACWH),Other=mean(Other))
daily_average
## Hourly average
hourly_average <- df %>% 
  group_by(year,month,day,hour) %>% 
  filter(year != 2010) %>% 
  summarize(Kitchen = sum(Kitchen), Laundry=sum(Laundry),ACWH=sum(ACWH), Other=sum(Other)) %>%
  group_by(hour) %>% 
  summarize(Kitchen = mean(Kitchen),Laundry=mean(Laundry),ACWH=mean(ACWH),Other=mean(Other))
hourly_average

# Plot the average consumption of monthly, daily and hourly data of the years 2007-2009 ####
## Monthly average
monthly_average %>% 
  gather(val='Consumption',key='Submeter',-1) %>% 
  ggplot(aes(x=monthname,y=Consumption,fill=Submeter)) + 
  geom_col(position='dodge') +
  theme(axis.text.x = element_text(angle = 45))
## Daily average
daily_average %>% 
  gather(val='Consumption',key='Submeter',-1) %>% 
  ggplot(aes(x=weekday,y=Consumption,fill=Submeter)) + 
  geom_col(position='dodge') +
  theme(axis.text.x = element_text(angle = 45))
## Hourly average
hourly_average %>% 
  gather(val='Consumption',key='Submeter',-1) %>% 
  ggplot(aes(x=hour,y=Consumption,fill=Submeter)) + 
  geom_col(position='dodge') +
  theme(axis.text.x = element_text(angle = 45))

# Plot the submeter consumption on a winter day and a summer day ####
## Winter day
p <- ggplot(df[(df$year==2009) & (df$month==2) & (df$day ==9),], aes(x=(DateTime))) +
  geom_line(aes(y=Kitchen, col = '1 - Kitchen')) +
  geom_line(aes(y=Laundry, col = '2 - Laundry room')) +
  geom_line(aes(y=ACWH, col = '3 - AC and Water heater')) +
  ylab('Submetering consumption') +
  xlab('Date') +
  ggtitle('Submeter consumption on a winter day') +
  labs(col = "Submeter")
ggplotly(p)

# Summer day
p <- ggplot(df[(df$year==2009) & (df$month==7) & (df$day ==10),], aes(x=(DateTime))) +
  geom_line(aes(y=Kitchen, col = '1 - Kitchen')) +
  geom_line(aes(y=Laundry, col = '2 - Laundry room')) +
  geom_line(aes(y=ACWH, col = '3 - AC and Water heater')) +
  ylab('Submetering consumption') +
  xlab('Date') +
  ggtitle('Submeter consumption on a summer day') +
  labs(col = "Submeter")
ggplotly(p)



# Time Series Analysis ####

# Visualization and Granularity Analysis ####
## Plot a random day (2009-02-09) and reduce granularity until it is seen clearly
df %>% 
  filter(year == 2009 & month == 2 & day == 9 & (minute == 0 | minute == 20 | minute == 40)) %>%
  plot_ly(x = ~data20090209$DateTime, y = ~data20090209$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~data20090209$Laundry, name = 'Laundry Room', mode = 'lines') %>%
    add_trace(y = ~data20090209$ACWH, name = 'Water Heater & AC', mode = 'lines') %>%
    layout(title = "Power Consumption February 9th, 2009",
          xaxis = list(title = "Time"),
          yaxis = list (title = "Power (watt-hours)"))

## Plot a random week and reduce granularity until it is seen clearly
df %>% 
  filter(year == 2009 & week == 16 & (minute == 0 | minute == 30)) %>%
  filter(hour %in% seq(from=0,to=23,by=3)) %>%
  plot_ly(x = ~data200917_1_2$DateTime, y = ~data200917_1_2$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~data200917_1_2$Laundry, name = 'Laundry Room', mode = 'lines') %>%
    add_trace(x = ~data200917_3$DateTime, y = ~data200917_3$ACWH, name = 'Water Heater & AC', mode = 'lines') %>%
    layout(title = "Power Consumption 16-22 of April, 2009",
          xaxis = list(title = "Time"),
          yaxis = list (title = "Power (watt-hours)"))

## Plot a random month and reduce granularity until it is seen clearly
df %>% 
  filter(year == 2009 & month == 2) %>%
  filter(hour %in% seq(from=0,to=23,by=6)) %>%
  plot_ly(x = ~data200902_1_2$DateTime, y = ~data200902_1_2$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~data200902_1_2$ACWH, name = 'Laundry Room', mode = 'lines') %>%
    add_trace(x = ~data200902_3$DateTime, y = ~data200902_3$ACWH, name = 'Water Heater & AC', mode = 'lines') %>%
    layout(title = "Power Consumption February, 2009",
          xaxis = list(title = "Time"),
          yaxis = list (title = "Power (watt-hours)"))

# # Percentage of sub-meter use ####
# ## On a specific day
# dfpie_daily <- data.frame(type=c('No usage','Usage'),rbind(df %>% group_by(year, month, day) %>% filter(Kitchen == 0, year == 2007, month == 11, day == 27) %>% summarize(tot = n()),df %>% group_by(year,month,day) %>% filter(Kitchen != 0, year == 2007, month == 11, day == 27) %>% summarize(tot = n())))
# dfpie_daily$prp <- dfpie_daily$tot / sum(dfpie_daily$tot)
# ggplot( data = dfpie_daily, aes( x= "", y = tot, fill = type))+
#   geom_bar(stat = "identity", color = "white")+
#   coord_polar("y", start=0)+
#   scale_fill_brewer(palette="Blues", name = "", labels = c("No usage", "Usage"))+
#   theme_void()+
#   geom_text(aes(label=paste0(round(prp*100), "%")), position = position_stack(vjust = 0.5)) +
#   ggtitle('Percentage of usage time of Submeter 1 in 27 of november of 2007') +
#   theme(plot.title = element_text(hjust = 0.5,size = 16))
# 
# ## On a specific month
# dfpie_monthly <- data.frame(type=c('No usage','Usage'),rbind(df %>% group_by(year, month) %>% filter(Kitchen == 0, year == 2007, month == 11) %>% summarize(tot = n()),df %>% group_by(year,month) %>% filter(Kitchen != 0, year == 2007, month == 11) %>% summarize(tot = n())))
# dfpie_monthly$prp <- dfpie_monthly$tot / sum(dfpie_monthly$tot)
# ggplot( data = dfpie_monthly, aes( x= "", y = tot, fill = type))+
#   geom_bar(stat = "identity", color = "white")+
#   coord_polar("y", start=0)+
#   scale_fill_brewer(palette="Blues", name = "", labels = c("No usage", "Usage"))+
#   theme_void()+
#   geom_text(aes(label=paste0(round(prp*100), "%")), position = position_stack(vjust = 0.5)) +
#   ggtitle('Percentage of usage time of Submeter 1 in the november of 2007') +
#   theme(plot.title = element_text(hjust = 0.5,size = 16))
# 
# ## On a specific year
# dfpie_yearly <- data.frame(type=c('No usage','Usage'),rbind(df %>% group_by(year) %>% filter(Kitchen == 0, year == 2007) %>% summarize(tot = n()),df %>% group_by(year) %>% filter(Kitchen != 0, year == 2007) %>% summarize(tot = n())))
# dfpie_yearly$prp <- dfpie_yearly$tot / sum(dfpie_yearly$tot)
# ggplot( data = dfpie_yearly, aes( x= "", y = tot, fill = type))+
#   geom_bar(stat = "identity", color = "white")+
#   coord_polar("y", start=0)+
#   scale_fill_brewer(palette="Blues", name = "", labels = c("No usage", "Usage"))+
#   theme_void()+
#   geom_text(aes(label = paste0(round(prp*100), "%")), position = position_stack(vjust = 0.5)) +
#   ggtitle('Percentage of usage time of Submeter 1 in the year 2007') +
#   theme(plot.title = element_text(hjust = 0.5,size = 16))



# Monthly time series analysis ####

# Create and plot monthly time series ####
# Create monthly time series
df_monthly <- df %>%
  group_by(year,monthname) %>%
  summarize(DateTime = min(DateTime), Kitchen = round(sum(Kitchen),2), Laundry = round(sum(Laundry),2), ACWH = round(sum(ACWH),2), Other = round(sum(Other),2), Total = round(sum(Total),2))
ts_monthly <- ts(df_monthly[,c('Kitchen','Laundry','ACWH','Other','Total')],frequency=12,start=c(2007,1),end=c(2010,10))
## Plot monthly time series
plot(ts_monthly, 
     plot.type='s',
     col=c('red', 'green', 'blue', 'purple', 'black'),
     main='Monthly time series',
     xlab='Year', ylab = 'kWh')
legend('topleft', c('Kitchen', 'Laundry Room', 'Air conditioning and Water heater','Non-submetered','Total'), 
       col=c('red', 'green', 'blue', 'purple', 'black'), lwd=2, bty='n')
## Plot monthly time series but only Kitchen and Laundry room to see them better
plot(ts_monthly[,c('Kitchen','Laundry')], 
     plot.type='s',
     col=c('red', 'green'),
     main='Monthly time series',
     xlab='Year', ylab = 'kWh')
legend('topleft', c('Kitchen', 'Laundry Room'), 
       col=c('red', 'green'), lwd=2, bty='n')

# Decompose the time series ####
## Monthly
monthly_list <- list()
for (i in seq(colnames(ts_monthly))){
  monthly_list[[i]] <- decompose(ts_monthly[,colnames(ts_monthly)[i]])
  decomp.plot(monthly_list[[i]],main= paste('Decomposition of',colnames(ts_monthly)[i],'energy consumption time series'))
  
}



# Start of forecasting code ####

# Holt Winters instructions:
# If you have a time series that can be described using an additive model 
# with constant level and no seasonality, you can use 
# simple exponential smoothing to make short-term forecasts (beta = FALSE, gamma = FALSE)
# If you have a time series that can be described using an additive model 
# with increasing or decreasing trend and no seasonality, you can use 
# Holt’s exponential smoothing to make short-term forecasts (gamma = FALSE)
# If you have a time series that can be described using an additive model 
# with increasing or decreasing trend and seasonality, you can use 
# Holt-Winters exponential smoothing to make short-term forecasts (use Holtwinters as is)

# Separate the monthly time series by each submeter and split train data ####
## Separate the monthly time series by each submeter
ts_submeter <- list('Kitchen' = ts_monthly[,'Kitchen'], 
                    'Laundry' = ts_monthly[,'Laundry'], 
                    'ACWH' = ts_monthly[,'ACWH'],
                    'Other' = ts_monthly[,'Other'],
                    'Total' = ts_monthly[,'Total'])
## Split train data (2007/01 - 2009/12)
ts_submeter_train <- list('Kitchen' = window(ts_submeter[['Kitchen']],end=c(2009,12)), 
                          'Laundry' = window(ts_submeter[['Laundry']],end=c(2009,12)), 
                          'ACWH' = window(ts_submeter[['ACWH']],end=c(2009,12)),
                          'Other' = window(ts_submeter[['Other']],end=c(2009,12)),
                          'Total' = window(ts_submeter[['Total']],end=c(2009,12)))

# Define the arima coefficients for all 5 time series ####
## Initialize an empty list of p,d,q and P,D,Q values for every time series
arima_coeffs <- list('Kitchen' = list('order' = c(0,0,0), 'seasonal' = c(0,0,0)),
                     'Laundry' = list('order' = c(0,0,0), 'seasonal' = c(0,0,0)),
                     'ACWH' = list('order' = c(0,0,0), 'seasonal' = c(0,0,0)),
                     'Other' = list('order' = c(0,0,0), 'seasonal' = c(0,0,0)),
                     'Total' = list('order' = c(0,0,0), 'seasonal' = c(0,0,0)))

# Check the values of d and D for every time series by using ndiffs and nsdiffs ####
for (submeter in names(ts_submeter)){
  d <- ndiffs(ts_submeter[[submeter]])
  D <- nsdiffs(ts_submeter[[submeter]])
  arima_coeffs[[submeter]][['order']][2] <- d
  arima_coeffs[[submeter]][['seasonal']][2] <- D
  print(paste0(submeter,': d = ',d,', D = ',D))
}

# Check acf and pacf to determine possible p, q and P, Q values for every time series ####
for (submeter in names(ts_submeter)){
  d <- arima_coeffs[[submeter]][['order']][2]
  ts_diff <- ts_submeter[[submeter]]
  while(d>0){
    ts_diff <- diff(ts_diff)
    d <- d - 1
  }
  p <- acf(ts_diff, lag.max = 36, plot = FALSE)
  plot(p, main = paste('Acf for',submeter))
  p <- pacf(ts_diff, lag.max = 36, plot = FALSE)
  plot(p, main = paste('Pacf for',submeter))
}

# Manually update the p, q and P, Q coefficients ####
# The updates are based on the visual information of the acf and pacf and then changed according to the accuracy
arima_coeffs$Kitchen$order[1] <- 0
arima_coeffs$Kitchen$order[3] <- 0
arima_coeffs$Kitchen$seasonal[1] <- 1 # or 0
arima_coeffs$Kitchen$seasonal[3] <- 1
arima_coeffs$Laundry$order[1] <- 1
arima_coeffs$Laundry$order[3] <- 1
arima_coeffs$Laundry$seasonal[1] <- 0
arima_coeffs$Laundry$seasonal[3] <- 0
arima_coeffs$ACWH$order[1] <- 1
arima_coeffs$ACWH$order[3] <- 1
arima_coeffs$ACWH$seasonal[1] <- 1 # or 0
arima_coeffs$ACWH$seasonal[3] <- 1
arima_coeffs$Other$order[1] <- 1
arima_coeffs$Other$order[3] <- 2
arima_coeffs$Other$seasonal[1] <- 0
arima_coeffs$Other$seasonal[3] <- 2
arima_coeffs$Total$order[1] <- 1
arima_coeffs$Total$order[3] <- 1 # or 2
arima_coeffs$Total$seasonal[1] <- 0
arima_coeffs$Total$seasonal[3] <- 2



# Creating the models ####
h = 10 # forecast for January to October 2010
# ## Test manual and auto.arima and choose the best aicc metric
# fit_list.names <- names(ts_submeter)
# fit_list <- vector("list", length(fit_list.names))
# names(fit_list) <- fit_list.names
# for (submeter in names(ts_submeter)){
#   # Initialize each list component to an empty vector
#   fit_list[[submeter]] <- c()
#   # Autoarima
#   fit <- auto.arima(ts_submeter[[submeter]])
#   AutoArima <- c(fit$aicc)
#   fit_list[[submeter]] <- cbind(fit_list[[submeter]],AutoArima)
#   for (k in seq(6)){
#     try({
#       fit <- auto.arima(ts_submeter[[submeter]],xreg=fourier(ts_submeter[[submeter]],K=k))
#       AutoArimaFourier <- c(fit$aicc)
#       fit_list[[submeter]] <- cbind(fit_list[[submeter]],AutoArimaFourier,k)
#     })
#   }
#   # Manual arima
#   fit <- Arima(ts_submeter[[submeter]], order = arima_coeffs[[submeter]][['order']], seasonal = arima_coeffs[[submeter]][['seasonal']])
#   Arima <- c(fit$aicc)
#   fit_list[[submeter]] <- cbind(fit_list[[submeter]],Arima)
#   for (k in seq(6)){
#     try({
#       fit <- Arima(ts_submeter[[submeter]], order = arima_coeffs[[submeter]][['order']], seasonal = arima_coeffs[[submeter]][['seasonal']],xreg=fourier(ts_submeter[[submeter]],K=k))
#       ArimaFourier <- c(fit$aicc)
#       fit_list[[submeter]] <- cbind(fit_list[[submeter]],ArimaFourier,k)
#     })
#   }
#   # Change the row name to the name of the metric
#   rownames(fit_list[[submeter]]) <- 'Aicc'
# }
# fit_list
## Use the models to forecast and calculate the accuracy
acc_list.names <- names(ts_submeter)
acc_list <- vector("list", length(acc_list.names))
names(acc_list) <- acc_list.names
for (submeter in names(ts_submeter)){
  # Initialize each list component to an empty vector
  acc_list[[submeter]] <- c()
  # Linear Model
  fit <- tslm(ts_submeter_train[['Kitchen']] ~ trend + season)
  fut <- forecast(fit,h=h)
  acc <- accuracy(fut,ts_submeter[[submeter]])
  LM <- acc[,'MAPE']
  acc_list[[submeter]] <- cbind(acc_list[[submeter]],LM)
  # Holt Winters
  fit <- HoltWinters(ts_submeter_train[[submeter]])
  fut <- forecast(fit,h)
  acc <- accuracy(fut,ts_submeter[[submeter]])
  HoltWinters <- acc[,'MAPE']
  acc_list[[submeter]] <- cbind(acc_list[[submeter]],HoltWinters)
  # Auto Arima
  fit <- auto.arima(ts_submeter_train[[submeter]])
  fut <- forecast(fit,h)
  acc <- accuracy(fut,ts_submeter[[submeter]])
  AutoArima <- acc[,'MAPE']
  acc_list[[submeter]] <- cbind(acc_list[[submeter]],AutoArima)
  # Arima
  fit <- Arima(ts_submeter_train[[submeter]], order = arima_coeffs[[submeter]][['order']], seasonal = arima_coeffs[[submeter]][['seasonal']])
  fut <- forecast(fit,h)
  acc <- accuracy(fut,ts_submeter[[submeter]])
  Arima <- acc[,'MAPE']
  acc_list[[submeter]] <- cbind(acc_list[[submeter]],Arima)
  # Change the row names to the names of the metrics
  rownames(acc_list[[submeter]]) <- c('Train MAPE', 'Test MAPE')
}
acc_list

# Check the residuals of the best models and plot the predictions ####
h = 24 # predict for 24 months (2 years)
## Kitchen - Holt Winters
fit <- HoltWinters(ts_submeter[['Kitchen']])
fut <- forecast(fit,h)
checkresiduals(fut)
plotforecast(fut,h,'Kitchen forecast')
## Laundry room - Linear Model
fit <- tslm(ts_submeter[['Laundry']] ~ trend + season)
fut <- forecast(fit,h=h)
checkresiduals(fut)
plotforecast(fut,h,'Laundry room forecast')
## ACWH - Holt Winters
fit <- HoltWinters(ts_submeter[['ACWH']])
fut <- forecast(fit,h)
checkresiduals(fut)
plotforecast(fut,h,'AC / Water heater forecast')
## Other - Arima
fit <- Arima(ts_submeter_train[['Other']], order = arima_coeffs[['Other']][['order']], seasonal = arima_coeffs[['Other']][['seasonal']])
fut <- forecast(fit,h)
checkresiduals(fut)
plotforecast(fut,h,'Non-submetered forecast')
## Total - Arima
fit <- Arima(ts_submeter_train[['Total']], order = arima_coeffs[['Total']][['order']], seasonal = arima_coeffs[['Total']][['seasonal']])
fut <- forecast(fit,h)
checkresiduals(fut)
plotforecast(fut,h,'Total forecast')



# Create the dataframe for use in tableau (hourly) and save it to a csv file ####
df_tableau <- df %>% 
  group_by(year,month,day,hour) %>% 
  filter(year != 2010) %>%
  summarize(DateTime = min(DateTime), Kitchen = sum(Kitchen), Laundry = sum(Laundry), ACWH = sum(ACWH), Other = sum(Other), Total = sum(Total))
df_tableau <- gather(df_tableau,key='Submeter',value='Consumption',Kitchen:Total)
write.csv2(df_tableau,'data/df_tableau_energy_consumption(hourly).csv',row.names = FALSE)

# Prophet testing ####
## Daily data frame used for prophet
df_prophet_day <- df %>% 
  filter(year != 2010) %>%
  group_by(year,month,day) %>% 
  summarize(DateTime = min(DateTime), Kitchen = sum(Kitchen), Laundry = sum(Laundry), ACWH = sum(ACWH), Other = sum(Other), Total = sum(Total))
## Daily analysis
df_prophet_day <- df_prophet_day[,c('DateTime','Total')]
colnames(df_prophet_day) <- c('ds','y')
m <- prophet(df_prophet_day)
future <- make_future_dataframe(m, periods = 730)
forecast <- predict(m, future)
#plot(forecast$yhat)
plot(m, forecast) + 
  ggtitle('Total daily consumption prediction') +
  xlab('Date') +
  ylab('Power consumption (kWh)')
prophet_plot_components(m, forecast)
## Hourly data frame used for prophet
df_prophet_hour <- df %>% 
  group_by(year,month,day,hour) %>% 
  summarize(DateTime = min(DateTime), Kitchen = sum(Kitchen), Laundry = sum(Laundry), ACWH = sum(ACWH), Other = sum(Other), Total = sum(Total))
## Hourly analysis
df_prophet_hour <- df_prophet_hour[,c('DateTime','Total')]
colnames(df_prophet_hour) <- c('ds','y')
m <- prophet(df_prophet_hour)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
#plot(forecast$yhat)
plot(m, forecast) +
  ggtitle('Total daily consumption prediction') +
  xlab('Date') +
  ylab('Power consumption (kWh)')
prophet_plot_components(m, forecast)
