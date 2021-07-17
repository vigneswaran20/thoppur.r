###-----------------------------Heat Map---------------------------------------

heat_map <- x_cleaned_2_

heat_map


#Month variable
heat_map<-heat_map %>% 
  group_by(month = month(Date, label = TRUE))

##Weekd variable
heat_map$weekd <- weekdays(heat_map$Date)


heat_map$weekd


heat_map$weekday = factor(heat_map$weekd,
                 levels =  c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday"))

heat_map

##round time
##convert to numeric and then remove

heat_map$TIME <- as.numeric(heat_map$Time)

heat_map <- heat_map %>%
  filter( !is.na(TIME))




heat_map <- heat_map %>%
  filter( !is.na(Age))


#ROUND OFF

heat_map$TIME <- round(heat_map$TIME)


heat_map <- heat_map %>% select(-Time)

heat_map
##-------------------------------------------------------------------





##More Cleaning



# Group by the weekday, hour, and devicetype, and then summarise by getting the
# mean of the visits.

heat_map <- heat_map %>% select(-Age)


install.packages("writexl")
library("writexl")
write_xlsx(heat_map,"E:\\heat_map.xlsx")

##Manually cleaned it to 1

heat_map_t <- heat_map %>% 
  group_by(weekday, TIME) %>% count(Head) 


heat_map_t

heat_map_t <- heat_map_t %>%
  filter( !is.na(TIME))


str(heat_map_t)

heat_map_t$weekday = factor(heat_map_t$weekday,
                 levels =  c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday"))


