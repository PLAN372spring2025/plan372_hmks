library(tidyverse)
rm(list=ls())
setwd("/Users/julia/Library/CloudStorage/OneDrive-UniversityofNorthCarolinaatChapelHill/Documents/PLAN372/plan372_hmks/exercises-main/sfpark/data")
sfpark <- read_csv("sfpark.csv")


aggregate_data <- sfpark %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"), #note, specifying the format requires what you have in front of you and how to specify that format. For help, read more here: https://www.r-bloggers.com/2013/08/date-formats-in-r/
         month = month(date),
         year = year(date))  


#check your data. Ok, this looks right. Ok, now we can aggregate by month year and add up entries
# we can do this as another block of code or keep going with our previous one. I'm going to keep going.


aggregate_data <- sfpark %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"), #note, specifying the format requires what you have in front of you and how to specify that format. For help, read more here: https://www.r-bloggers.com/2013/08/date-formats-in-r/
         month = month(date),
         year = year(date))  %>% 
  group_by(facility, month, year) %>% #why did I pick these three?
  summarize(entries = sum(entries)) 

#check your data, do they look right? Do you have month year information for each garage?
#sometimes it's easier to sort the data to visually examine. Let's do that.

aggregate_data <- sfpark %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"), #note, specifying the format requires what you have in front of you and how to specify that format. For help, read more here: https://www.r-bloggers.com/2013/08/date-formats-in-r/
         month = month(date),
         year = year(date))  %>% 
  group_by(facility, month, year) %>% #why did I pick these three?
  summarize(entries = sum(entries)) %>% 
  arrange(facility, year, month) #why this order??

#ok, this passes the visual check. 

#now we want to average across years to find months where each garage has it's highest value.
#again, you can create a new block of code or keep going with the previous block. 
#I'm going to keep going.



aggregate_data <- sfpark %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"), #note, specifying the format requires what you have in front of you and how to specify that format. For help, read more here: https://www.r-bloggers.com/2013/08/date-formats-in-r/
         month = month(date),
         year = year(date))  %>% 
  group_by(facility, month, year) %>% #why did I pick these three?
  summarize(entries = sum(entries)) %>% 
  arrange(facility, year, month) %>% #why this order??
  group_by(facility, month) %>% #why these variables???
  summarize(entries=mean(entries))

final_data <- aggregate_data %>% #why am I using aggregate data here instead of sfpark?
  group_by(facility) %>% 
  mutate(max_value_entries = max(entries, na.rm = T)) #this creates a column with the maximum value

#check your data, did it do it right?

#ok, cool. Now let's identify the max month for each garage. keep going with your block.

final_data <- aggregate_data %>% #why am I using aggregate data here instead of sfpark?
  group_by(facility) %>% 
  mutate(max_value_entries = max(entries, na.rm = T),#this creates a column with the maximum value
         max_value_dummy = ifelse(entries == max_value_entries, 1, 0))  

#check your data, does this look good?

#ok, final step. Filter your data to months where you have the max value
final_data <- aggregate_data %>% #why am I using aggregate data here instead of sfpark?
  group_by(facility) %>% 
  mutate(max_value_entries = max(entries, na.rm = T),#this creates a column with the maximum value
         max_value_dummy = ifelse(entries == max_value_entries, 1, 0))  %>% 
  filter(max_value_dummy == 1)


#===============================================================================
# Exercise 2
#===============================================================================

#plot the average monthly values for each garage
#this information was saved in our aggregate_data object
#we're going to want the x axis to be time (month)
#and the y axis to be amount (entries)

ggplot(data = aggregate_data, aes(x = month, y=entries)) + 
  geom_point()


#we're going to want lines for our example
ggplot(data = aggregate_data, aes(x = month, y=entries)) + 
  geom_line()





#this looks awful. For many reasons. 
#1) lines are weird. Want to show by garage <- Can do this with group option
#2) aesthetically, I don't like the gray and the numbers look small/funny, axis labels could use some touching up

ggplot(data = aggregate_data, aes(x = month, y=entries, group=facility)) + 
  geom_line()


#ok, this is getting us closer on lines issue but I want colors. Can use the color option


ggplot(data = aggregate_data, aes(x=month, y=entries, group=facility, color=facility)) + 
  geom_line() +
  theme_classic() + 
  labs(x="Month", y="Entries") +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12))
  
#have to reclass month so X axis doesn't give decimals thinking it is numeric
aggregate_data$month <- as.factor(aggregate_data$month)
class(aggregate_data$month)


ggplot(data = aggregate_data, aes(x=month, y=entries, group=facility, color=facility)) + 
  geom_line() +
  theme_classic() + 
  labs(x="Month", y="Entries") +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12))


aggregate_data <- mutate(aggregate_data, month_label = month.name[month])
#use month.name[month] function to assign month names to 1-12
ggplot(data = aggregate_data, aes(x=month_label, y=entries, group=facility, color=facility)) + 
  geom_line() +
  theme_classic() + 
  labs(x="Month", y="Entries") +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=90)) 
#need to order month names so they are not alphebetized 
aggregate_data$month_label <- fct_inorder(aggregate_data$month_label)
#now months are in the **order of first appearance**
ggplot(data = aggregate_data, aes(x=month_label, y=entries, group=facility, color=facility)) + 
  geom_line() +
  theme_classic() + 
  labs(x="Month", y="Entries", color="Facility Name") +
  ggtitle("Average Number of Entries to SF Garages (2011-2014)")
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=90)) 
  
  #===============================================================================
  # Exercise 3
  #===============================================================================
  
  #create a histogram of values by month for final dataset
  
  ggplot(data = final_data, aes(x = month)) + geom_histogram()
  
  final_data$month2 <- as.factor(final_data$month)

  #can add a label variable, takes a little bit more work
  final_data <- final_data %>% 
    arrange(month) %>% 
    mutate(month_lable = month.name[month], #gets names
           month_lable = fct_inorder(month_lable))  #preserves order
  
    #MUST USE GEOM_BAR, NOT GEOM_HIST FOR FACTORED VARIABLES AS OPPOSED TO NUMERIC
#plot with numbered months
ggplot(data=final_data, aes(x=month2))+
       geom_bar() + 
         theme_classic()+ 
      xlab("Month") +
    theme(axis.title = element_text(size=12),
          axis.text = element_text(size=12))
  
#plot with month labels
ggplot(data=final_data, aes(x=month_label))+
  geom_bar() + 
  theme_classic()+ 
  xlab("Month") +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12))

#if you use second approach, x-axis may still may look wrong. Data is still grouped!
final_data <- final_data %>% 
  ungroup %>% 
  arrange(month) %>% 
  mutate(month_lable = month.name[month], #gets names
         month_lable = fct_inorder(month_lable))  #preserves order
