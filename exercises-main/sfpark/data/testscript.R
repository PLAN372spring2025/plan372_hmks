rm(list=ls())
setwd("/Users/julia/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents/PLAN372/plan372_hmks/exercises-main/sfpark/data")
SFPark <- read.csv("sfpark.csv")
library(tidyverse)

colnames(SFPark)
head(SFPark)
tail(SFPark)
dim(SFPark)
unique(SFPark$date)
class(SFPark)
class(SFPark$entries)

head(SFPark, n=10L)
unique(SFPark$usage_type)  

summary(SFPark)

test_data <- mutate(SFPark, mission_dummy = ifelse(district=="Mission", 1,0))

test_data2 <- mutate(SFPark, mission_dummy2 = ifelse(district=="Mission", 1,0), filmore_dummy = ifelse(district=="Fillmore",1,0))

test_data <- SFPark %>% 
  mutate(mission_dummy = ifelse(district=="Mission",1,0))

entry_by_district <- SFPark %>% 
  group_by(district) %>% 
  summarize(avg=mean(entries)) %>% 
  ungroup
mean(SFPark$entries)

total_entries <- SFPark %>% 
  group_by(date, facility) %>% 
  summarize(entries=sum(entries), exits=sum(exits)) %>% 
  ungroup

unique(SFPark$facility)  



exercise1 <- SFPark %>% 
  group_by(date, facility) %>% 
  summarize(entries=sum(entries, na.rm=T), 
            exits=sum(exits,na.rm=T)) %>% 
  group_by(facility) %>% 
  summarize(mean_entries = mean(entries,na.rm=T),
            median_entries=median(entries,na.rm=T), 
            mean_exits=mean(exits,na.rm=T),
            median_exits=mean(exits,na.rm=T))
  

SFPark$date2 <- as.Date(SFPark$date, format="%m/%d/%Y")
class(SFPark$date2)
summary(SFPark$date2)

test <- total_entries %>% 
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         weekday=wday(date),
         weekday_lbl=wday(date,label=T),
         month=month(date),
         year=year(date),
         day=day(date))


exercise3 <- test %>% 
  group_by(facility, year) %>% 
  summarize(entries=mean(entries,na.rm=T))
#which month has the most demand for parking
exercise4 <- test %>% 
  group_by(facility, month) %>% 
  summarize(entries=mean(entries,na.rm=T))

#pivoting data, go from long to wide
wide_data <- test %>% 
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         month=month(date)) %>% 
  group_by(facility,month) %>% 
  summarize(entries=mean(entries)) %>% 
  group_by(facility) %>% 
  pivot_wider(names_from=month,
              values_from=entries)
#pivoting to long would be pivot_longer

monthdemand <- test %>% 
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         month=month(date)) %>% 
  group_by(month) %>% 
  summarize(entries=mean(entries)) 



