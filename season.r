

'Accidents with respect to Season'

library(zoo)

kpop
yq <- as.yearqtr(as.yearmon(kpop$Date, "%m/%d/%Y") + 1/12)

yq
kpop$Season <- factor(format(yq, "%q"), levels = 1:4, 
                      labels = c("winter", "spring", "summer", "fall"))

kpop_n<-kpop %>% 
  group_by(month = month(Date, label = TRUE)) 

kpop_n

ggplot(kpop_n, aes(fill=Head, y=n, x=Season)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Accident with respect to season") +
  labs(
    x = NULL, y = "Number of traffic accidents per Season",
    color = "CASES"
  ) +
  theme_ipsum() +
  xlab("") + theme_minimal()  + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))




