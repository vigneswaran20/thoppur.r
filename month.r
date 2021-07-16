

salem_tra <- x_cleaned_

salem_s <- salem_tra %>%
  mutate(Date = floor_date(Date, unit = "week")) %>% count(Date, Head)

##Omit the nth row in  a data frame

b <- salem_s[-nrow(salem_s),]

kpop<-b %>% 
  group_by(month = month(Date, label = TRUE))

'-------------------------------------------------------------'



library(ggplot2)
library(viridis)
library(hrbrthemes)

ggplot(kpop, aes(fill=Head, y=n, x=month)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Accident with respect to month") +
  labs(
    x = NULL, y = "Number of traffic accidents per month",
    color = "CASES"
  ) +
  theme_ipsum() +
  xlab("") + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))



