


'Accidents with respect to Time'

library(zoo)

b

floor(1.70)

b$Time <- floor(b$Time)

##Saving it as excel 
library("writexl")
write_xlsx(b,"E:\\time_freq_modified.xlsx")

time_freq <- read_xlsx("E:\\time_freq_modified.xlsx")

##Now I am importing it 

str(time_freq)

time_freq




time_freq <- time_freq %>% 
  group_by(Time) %>% count(Head) 


View(time_freq)

time_freq <- time_freq %>%
  filter( !is.na(Time))


time_freq

##checking for sum

sum(time_freq$n)

#Convert Time to factor

time_freq$Time = factor(time_freq$Time)

str(time_freq)

ggplot(time_freq, aes(y=n, x=Time)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Accident with respect to time") +
  labs(
    x = NULL, y = "Number of traffic accidents",
    color = "CASES"
  ) +
  theme_ipsum() +
  xlab("") + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))
