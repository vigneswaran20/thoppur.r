time_bar <- x_time

##Checking the variable types. 
str(time_bar)  
'So we are dealing with character variables. Mmm'
##Converting it to factor variables and ignoring the missing data

time_bar <- time_bar %>%
  filter( !is.na(Gender), !is.na(Head), !is.na(Date), !is.na(Time))  %>%
  select(Head, Date,Time, Gender, Age) %>% 
  mutate_if(is.character, factor) %>% select(-Age)

#Check whether NA is in each column

apply(time_bar, 2, function(x) any(is.na(x)))


library(tidyr)
library(extrafont)
loadfonts(device = "win")
library(kableExtra)
require(dplyr)
library(ggplot2)

library(tidyverse)
library(lubridate)
library(RSocrata)
library(readxl)




thoppur_trak_change <- time_bar %>% mutate() %>% mutate(time = as.numeric(Time)) %>% select(-Gender, -Time)

thoppur_trak_change_new <- thoppur_trak_change %>% mutate() %>% mutate(time = as.numeric(time)) %>%
  mutate(
    # Create categories
    time_period = dplyr::case_when(
      time >= 7 & time <= 10           ~ "MOH",
      time >= 10 & time <= 16 ~ "AN",
      time >= 16 & time <= 21 ~ "EOH",
      time >= 21             ~ "N",
      time < 7  ~ "N"
    ),
    # Convert to factor
    time_period = factor(
      time_period,
      level = c("MOH", "AN","EOH", "N")
    )
  )

# How To Extract Year From Date in R - example

# make sure the date is indeed a date
 thoppur_trak_change_new$Date <- as.Date( thoppur_trak_change_new$Date)

# extract the year and convert to numeric format
 thoppur_trak_change_new$Date <- as.numeric(format(thoppur_trak_change_new$Date , "%Y"))


###------------------------------------------------
thoppur_trak_change_new
 thoppur_track_summrized <- thoppur_trak_change_new %>% count(Date, time_period)


thoppur_track_summrized


thoppur_track_summrized %>% 
  mutate(scales::percent(n / sum(n)))%>%
  kable(
    col.names = c("YEAR", "time_period", "Number of CASES", "% of CASES"),
    align = "llrr"
  )



###---------------------------------------------------------------


thoppur_track_summrized
salem_n <- thoppur_track_summrized %>% arrange(desc(n)) %>% group_by(Date) %>% mutate(pct = prop.table(n))

salem_n


salem_n$Date <- factor(salem_n$Date)

salem_n

salem_no <- salem_n %>% mutate(time = as.factor(Date)) %>% arrange(desc(n)) %>% group_by(Date) %>% mutate(pct = prop.table(n))%>% 
  ggplot(aes(x = Date, y = n, fill = reorder(time_period,-n), label = scales::percent(round(pct,2)))) + 
  geom_col(alpha = 0.8, position = "dodge", show.legend = TRUE,  width=0.80, colour="black") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.4,    # nudge above top of bar
            size = 3)  + theme_minimal() + theme(plot.title = element_text(hjust = 0.4),
                                                 text = element_text( size = 12, family = "Open Sans")) + 
  theme(axis.text.x=element_text(angle=45, hjust=0.5)) + guides(fill=guide_legend(title="Time Period"))+
  labs(x = NULL, y = "Case Numbers") 

salem_no
