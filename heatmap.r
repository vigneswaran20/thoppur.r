
heat_map <- x_cleaned_2_




#Month variable
heat_map<-heat_map %>% 
  group_by(month = month(Date, label = TRUE))

##Weekd variable
heat_map$weekd <- weekdays(heat_map$Date)

##round time
##convert to numeric and then remove

heat_map$TIME <- as.numeric(heat_map$Time)

heat_map <- heat_map %>%
  filter( !is.na(TIME))

heat_map


heat_map <- heat_map %>%
  filter( !is.na(Age))


#ROUND OFF

heat_map$TIME <- round(heat_map$TIME)


heat_map <- heat_map %>% select(-Time)
##-------------------------------------------------------------------

# Create a theme that is a bit more friendly. This isn't required to do the visualization,
# but it's a painful heatmap to look at otherwise. We start with a minimalist theme... and
# then basically make it even more minimalist. Thank you, Tufte and Few.
theme_heatmap <- theme_light() +                 # Start with a minimalist theme
  theme(panel.grid = element_blank(),            # Remove the gridlines
        panel.border = element_blank(),          # Remove the border around the heatmap
        plot.title = element_text(face = "bold", # Make the title bold
                                  size = 11,     # Adjust the title size
                                  hjust = 0.5),  # Center the title
        axis.ticks = element_blank(),            # Remove the axis tickmarks
        axis.title.x = element_blank(),          # Turn off the x-axis title 
        axis.title.y = element_text(size=10),    # Adjust the size of the y-axis title
        axis.text.y = element_text(size = 8),    # Adjust the size of the y-axis labels
        axis.text.x = element_text(size = 10),   # Adjust the size of the x-axis labels
        legend.position = "none")                # Turn off the legend





##More Cleaning



# Group by the weekday, hour, and devicetype, and then summarise by getting the
# mean of the visits.

heat_map <- heat_map %>% select(-Age)


install.packages("writexl")
library("writexl")
write_xlsx(heat_map,"E:\\heat_map.xlsx")

##Manually cleaned it to 1

heat_map_t <- heat_map %>% 
  group_by(weekd, TIME) %>% count(Head) 



##ggplot

gg <- ggplot(heat_map_t, mapping = aes(x = weekd, y = TIME, 
                                           fill = n)) +
  geom_tile(colour="white") +  # This makes the heatmap (the colour is the lines in the grid)
  scale_fill_gradient(low = "#f4fff6", high="#0bb730") +  # The colour range to use
  scale_y_reverse(breaks=c(23:0), labels=c(23:0),    # Put the 0 at the top of the y-axis
                  expand = c(0,0)) +                 # Remove padding around the heatmap
  scale_x_discrete(expand = c(0,0), position = "top") +
  labs(title = "Accidents by Day of Week / Hour of Day", y = "Hour of Day") +
  theme_heatmap  # Apply the theme defined earlier for styling


gg_labeled <- gg +
  geom_text(aes(label = sprintf("%.1f", n)),  # sprintf formats to 2 decimals
            size = 3)                              # And we need to tweak the font size




#Re-check

heat_map_t
sum(heat_map_t$n, NA, na.rm = TRUE)
