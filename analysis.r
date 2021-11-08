#pckgs <- c("Quandl", "Sweep", "tidyverse", "tidyquant", "ggplot", "forcats", "stringr")
#install.packages(pckgs)

library(Quandl)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(timetk)
library(forcats)
library(stringr)
library(gganimate)
library(plyr)
library(stringr)
library(gridExtra)

# API KEY FOR QUANDL 2MrJyxVvPtkwJ2JJ6TeA
rm(list = ls())
Quandl.api_key("2MrJyxVvPtkwJ2JJ6TeA")

#get the dataset
ICICI = Quandl("NSE/ICICIBANK", collapse = "daily", start_date = "2018-09-01", type = "raw")
PNB= Quandl("NSE/PNB",collapse="daily",start_date="2018-09-01",type="raw")
Axis=Quandl("NSE/AXISBANK",collapse="daily",start_date="2018-09-01",type="raw")
Canara=Quandl("NSE/CANBK",collapse="daily",start_date="2018-09-01",type="raw")
BOB=Quandl("NSE/BANKBARODA",collapse="daily",start_date="2018-09-01",type="raw")
SBI=Quandl("NSE/SBIN",collapse="daily",start_date="2018-09-01",type="raw")

#add a stock column
ICICI<-cbind(ICICI,Stock="")
PNB<-cbind(PNB,Stock="")
Axis<-cbind(Axis,Stock="")
SBI<-cbind(SBI,Stock="")
Canara<-cbind(Canara,Stock="")
BOB<-cbind(BOB,Stock="")

# Paste the stock name in stock column
# for plotting purpose
ICICI$Stock<-paste(ICICI$Stock,"ICICI",sep="")
PNB$Stock<-paste(PNB$Stock,"PNB",sep="")
Axis$Stock<-paste(Axis$Stock,"Axis",sep="")
SBI$Stock<-paste(SBI$Stock,"SBI",sep="")
Canara$Stock<-paste(Canara$Stock,"Canara",sep="")
BOB$Stock<-paste(BOB$Stock,"BOB",sep="")

## Consolidate under one dataset
Master_Data = rbind(ICICI,PNB,Axis,SBI,Canara,BOB)

#Convert the dates into character in order to split the column into "Y" "m" "dd"" columns
Master_Data$Date = as.character(Master_Data$Date)
#split the date column by - and create a list for the same
list = strsplit(Master_Data$Date, "-")

library(plyr)
# to return results of the above list in a data frame
Master_date = ldply(list)
#changing the column name of the master date dataframe
colnames(Master_date) = c("Year", "Month", "Day")

#now we'll column bind the above master_date datafrom to the master data
Master_Data<-cbind(Master_Data,Master_date)
names(Master_Data)

# Change the scale for Traded Quantity
Master_Data$`Total Trade Quantity` = Master_Data$`Total Trade Quantity`/100000

# Convert the Date to as.Date()
Master_Data$Date<-as.Date(Master_Data$Date)


## Group By Stock

Master_Data<-Master_Data%>%
  tibble::as_tibble()%>%
  group_by(Stock)

## Visualization for month-wise daily stock prices
##facet_wrap is used to group the plots by respective stocks
##and also display in a row-col format

ggplot(Master_Data, aes(x = Date, y = Close, color = Stock)) +
geom_point() +
labs(title = "Daily Close Price", x = "Month",y="Close Price") +
facet_wrap(~ Stock, ncol = 3, scale = "free_y") +
theme_tq() +
theme(
  panel.border = element_blank(),
  axis.line = element_line(colour="black"),
  plot.title = element_text(hjust = 0.5,size=18,colour="brown"),
  panel.background = element_rect(fill = 'lemonchiffon'),
  legend.position = 'none'
)

  
#=====Finding the Density Distribution of Deviation of High Price from Open Price
#Deviation from High & Low Price
#mutate() adds new variables and preserves existing ones
#here Dev_high and Dev_low are the new variables and are
#appended to the master_data and stored in new variables --> Master_Data_High & Master_Data_Low
#-------------------------------------------------------------------------------
#the motive is to get the price range which will be useful for intraday trading 
Master_Data_High <- mutate(Master_Data , Dev_High = High-Open)
Master_Data_Low <-mutate(Master_Data , Dev_Low = Open-Low)
#computing the weekly high prices
#for this we use tq_transmute from the tidyquant package
#this method adds new variables to and existing dataset and
#returns only newly created columns, typically used when periodicity changes
Master_Data_High_Week <- Master_Data_High %>%
  tq_transmute(
    select     = Dev_High,
    mutate_fun = apply.weekly, 
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "Dev_High_Mean"
  )



#similarly doing to compute weekly low prices
Master_Data_Low_Week<-Master_Data_Low%>%
  tq_transmute(
    select  = Dev_Low,
    mutate_fun = apply.weekly,
    FUN = mean,
    na.rm = TRUE,
    col_rename = "Dev_Low_Mean"
  )

##Visualization of density distribution of high Price
High<-Master_Data_High_Week%>%ggplot(aes(x=Dev_High_Mean,color=Stock))+
  geom_dotplot(binwidth=0.50,aes(fill=Stock))+
  xlim(0,10)+
  labs(title="Distribution of High Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  theme_tq()+
  theme(
    panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.line=element_line(colour="black"),
    plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"),
    legend.position =  'none',
    panel.background = element_rect(fill = 'lemonchiffon')
  )
High

Low<-Master_Data_Low_Week%>%ggplot(aes(x=Dev_Low_Mean,color=Stock))+
  geom_dotplot(binwidth=0.50,aes(fill=Stock))+
  xlim(0,10)+
  labs(title="Distribution of Weekly Low Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  theme_tq()+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.line=element_line(colour="black"),
    plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"),
    legend.position =  'none',
    panel.background = element_rect(fill = 'lemonchiffon')
    )
Low

##arrange both the graphs in one plot
grid.arrange(High, Low)

