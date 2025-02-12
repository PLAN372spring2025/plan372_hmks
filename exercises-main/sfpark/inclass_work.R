library(tidyverse)

getwd()
setwd("/Users/wolfge/Documents/bootstrap_plan372/cloned hwk repos/plan372_hmks/exercises-main/sfpark")
#^^ setting wd obv

#-======-=======----

sfpark_data <- read_csv("data/sfpark.csv") #reading data

colnames(sfpark_data)
head(sfpark_data)


unique(sfpark_data$usage_type) #finding unique vals of usage variable
unique(sfpark_data$facility)

#experimenting with shit in class lmao

testdf <- rename(sfpark_data,"temporal location" = "date")



# filter example

test_data <- mutate(sfpark_data, 
                    mission_dummy = ifelse(district=="Mission",1,0))


test_data2 <- mutate(sfpark_data,
                     mission_dummy=ifelse(district=="Mission",1,0),
                     filmore_dummy = ifelse(district=="Fillmore",1,0))

#this is how piping works
#you can Cmd Shift M to make a pipe
test_data2 %>% 
  rename("fillmore_dummy" = "filmore_dummy") %>% 
  colnames()
  #mutate(test_data2,another_dummy=ifelse(district!="Mission" & district!="Fillmore",2,0))


alt <- sfpark_data %>% 
  group_by(district) %>% 
  summarize(district_avg_entries = mean(entries)) %>% 
  ungroup



#finding entrances to garages on a given day

exits_entries_by_facility_dates <- sfpark_data %>% 
  group_by(date, facility) %>% 
  summarize(entries = sum(entries),exits = sum(exits)) %>% 
  ungroup


# avg_daily_entr_exit_facil <- exits_entries_by_facility_dates %>% 
#   group_by(facility) %>% 
#   summarize(avg_daily_entries = mean(entries),avg_daily_exits = mean(exits)) %>% 
#   ungroup
# 
# median_daily_entr_exit_facil <- exits_entries_by_facility_dates %>% 
#   group_by(facility) %>% 
#   summarize(med_daily_entries = median(entries),med_daily_exits = median(exits)) %>% 
#   ungroup

#creating a df that tell us the average daily exits and entrances for each parking
#garage facility
daily_entr_exits_facil <- exits_entries_by_facility_dates %>% 
  group_by(facility) %>% 
  summarize(mean_entries_daily = mean(entries),median_entries_daily = median (entries),
            mean_exits_daily = mean(exits), median_exits_daily = median(exits)) %>% 
  ungroup



#========

class(sfpark_data$date)

#turn it into dates!
head(sfpark_data)
#date column looks like <chr>
#4/1/2011 

dateconvert <- sfpark_data %>% 
  mutate(date2 = as.Date(date,format = "%m/%d/%Y"))


tail(dateconvert)



#-========
#can extract year month, day, or detect if weekday or weekend
#needs to be told how to read it properly
#as.Date(column,format="strthattellsyouhowtoreadit")

sfpark_data <- sfpark_data %>% 
  mutate(date = as.Date(date,format = "%m/%d/%Y"),
         weekday = wday(date),
         weekday_lbl = wday(date, label=T),
         month = month(date),
         year = year(date),
         day = day(date))
  

#======


#"what month has the highest parking demand"

#create df of entries by garage and year

#so find the highest mean of parkings sorted by month, yes?

parking_demand <- sfpark_data %>% 
  group_by(facility,month) %>% 
  summarize(mean_entries_monthly = mean(entries),
            median_entries_monthly = median(entries)) %>% 
  ungroup


unique(sfpark_data$facility)

wide_data <- sfpark_data %>% 
  group_by(facility,year) %>% 
  summarize(entries = mean(entries)) %>% 
  ungroup %>% 
  pivot_wider(names_from=year,values_from = entries)



head(sfpark_data)


#finding peak months per facility

facil_monthly_activity <- sfpark_data %>% 
  group_by(facility,month,year) %>% 
  summarize(entries = sum(entries)) %>%  #we want the whole month's worth
  arrange(facility,year,month) %>% #you could also just rearrange the group_by()
  #now we have the sum of each month for each garage, yearsly
  
  #we want the mean for month X for all years
  #don't have to ungroup first
  group_by(facility,month) %>% 
  summarize(entries = mean(entries)) %>% 
  
  ungroup()
  
head(facil_monthly_activity,16)


#I'm a genius :3c

# testbit <-facil_monthly_activity %>% 
#   group_by(facility) %>% 
#   summarise(max = max(entries)) %>% 
#   ungroup()

#but you don't get the month-number here
#testbit

#if you want the row chunked off

facil_max_months <- facil_monthly_activity %>% 
  #head(facil_monthly_activity)
  group_by(facility) %>% 
  mutate(max_value_entries = max(entries, na.rm =T),
         maxval = ifelse(entries == max_value_entries, 1,0)) %>% 
  filter(maxval == 1) %>% 
  select(-c(max_value_entries,maxval)) %>% 
  ungroup()

facil_max_months


#=======
#=======


#we are going to use ggplot to make some nice lookin graphs that demonstrate 
#the monthly usage levels of each parkin garage

library(ggplot2)
#ggplot2 is the name of the package
#ggplot is the name of the function from that package

?ggplot

#what do we want? color coding, easier distinction, legends, better y label
#better xlabeling


unique(facil_monthly_activity$month)

class(facil_monthly_activity$month)
#it's assuming they're numeric, not categorical
#because they are saved like that lol

facil_monthly_activity <-mutate(facil_monthly_activity,
                                catmonth = as.factor(facil_monthly_activity$month))

facil_monthly_activity <-mutate(facil_monthly_activity,
                                nammonth = month.name[month])

#x aaxis labels not in right order
#can use fct_inorder

facil_monthly_activity$nammonth <- fct_inorder(facil_monthly_activity$nammonth)
#fixed! fascinating

#get rid of "Garage" on the end of the Facility names for neatness
?str_replace_all
facil_monthly_activity <-mutate(facil_monthly_activity,
                                garage = str_replace_all(facil_monthly_activity$facility,"Garage",""))

facil_monthly_activity$garage

# group_by(facil_monthly_activity,facility)
# ungroup(facil_monthly_activity)
ggplot(data = facil_monthly_activity, 
       mapping = aes(x = nammonth, y = entries,group=garage, color=garage)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  xlab("Month") +
  ylab("Entries")+
  labs(color = "Garage name")+
  ggtitle("Average Number of Entries Montly to SF Parking Garages (2011-2014)")+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))
  
  

ggplot(data = facil_max_months,
       aes(x = monthlabel)) +
  geom_bar() +
  ylab("Peak Month Tally")+
  xlab("Month") +
  ggtitle("Peak months SF Parking Garages (2011-2014)")

facil_max_months$month2 <- as.factor(facil_max_months$month)

facil_max_months <- facil_max_months %>% 
  arrange(month) %>% 
  mutate(monthlabel = month.name[month],
         monthlabel = fct_inorder(monthlabel))
  
facil_max_months



Sys.Date()
