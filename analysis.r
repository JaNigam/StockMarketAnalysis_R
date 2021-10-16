pckgs <- c("Quandl", "Sweep", "tidyverse", "tidyquant", "ggplot", "forcats", "stringr")
install.packages(pckgs)

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
Master_Data<-rbind(ICICI,PNB,Axis,SBI,Canara,BOB)


