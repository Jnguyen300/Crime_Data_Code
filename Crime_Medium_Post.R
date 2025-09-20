library(tidyverse)

crimes <- read.csv("C:/Users/thatc/Downloads/Crime Dataset.csv")
view(crimes)
head(crimes)

#Lets clean the data and adjust the columns.
crimes <- select(crimes, -Neighborhood, -Gender_of_Suspect, -Age_of_Suspect, -Victims)
head(crimes)

#Lets combine City and State together for cleaner data.
crimes <- crimes %>%
  mutate(City_State = paste(City, State, sep = ', '), 
         Date_Time = paste(Date, Time, sep = ', '))


crimes <- select(crimes, -City, -State, -Date, -Time)

head(crimes)

#Lets Reorder the columns 
crimes <- crimes[, c("Crime_ID","Crime_Type", "City_State", "Date_Time",
                     "Incidents", "Arrests", "Severity", "Weapons_Involved", 
                     "Property_Damage")]

head(crimes)

#Creating a graph for correlation of the crime data set

#This shows a correlation of the severity of each crime recorded
ggplot(crimes, aes(x = Severity, y = Incidents, fill = Severity)) +
  geom_col() +
  labs(title = "Incidents by Severity",
       x = "Severity Level", y = "Number of Incidents") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#This shows the amount of crimes that appears in major cities in the U.S. 
ggplot(crimes, aes(x = City_State, y = Incidents, fill = City_State)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Total Incidents by City",
       x = "City, State", y = "Number of Incidents") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


#I am grouping incidents by city and severity for better visualization
city_severity <- crimes %>%
  group_by(City_State, Severity) %>%
  summarise(total_incidents = sum(Incidents, na.rm = TRUE), .groups = "drop")

#Stacking the bar chart, where it correlates with city and severity 
ggplot(city_severity, aes(x = reorder(City_State, total_incidents), 
                          y = total_incidents, fill = Severity)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Incidents by City and Severity",
    x = "City, State", y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Now we are going to see how much money is lost ot these crimes per city 

#We need to clean more data where we can remove $ 
crimes$Property_Damage <- parse_number(crimes$Property_Damage)

#Group city_state by the total damage 
city_damage <- crimes %>% group_by(City_State) %>% 
  summarize(total_damage = sum(Property_Damage), .groups = "drop")
            

#Creating the stacked bar chart as the previous one 
ggplot(city_damage, aes(x = reorder(City_State, total_damage), y = total_damage)) +
  geom_col(fill = "red") + coord_flip() + 
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Total Property Damage by City", x = "City, State",
       y = "Total Property Damage(USD)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))


head(crimes)
