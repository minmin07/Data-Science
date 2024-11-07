install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
data<- read.csv("C:/Users/loday/OneDrive/Desktop/Assignment_MAT205/titani_dataset.csv")

#1.Obtain the descriptive statistics summary of the given dataframe, focusing
#on both numeric and categorical variables.
summary(data)


#2. Histogram of the Number of Passengers in Each Class

ggplot(titanic_df, aes(x = factor(Pclass))) +
  geom_bar() +
  ggtitle("Number of Passengers in Each Class") +
  xlab("Class") +
  ylab("Number of Passengers")


#3.Create a histogram of passengers' ages, highlighting the number of
#passengers over 60 years old.
ggplot(data, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") + # Group ages into bins of width 5
  geom_vline(xintercept = 60, color = "red", linetype = "dashed") +
  ggtitle("Distribution of Passenger Ages with Highlight at 60+") +
  xlab("Age") + 
  ylab("Passenger")

  
#4.Visualize the distribution of the fare paid using a density plot.
ggplot(data, aes(x = fare)) + 
  geom_density(fill = "blue", alpha = 0.5) +
  ggtitle("Density Plot of Fare Paid") + 
  xlab("Fare") +
  ylab("Density")


#5.Create a bar plot showing the count of passengers who survived versus
#those who did not.
ggplot(data, aes(x = factor(survived))) + 
  geom_bar(fill = c("red", "green"), color = "black") + ## Red for non-survivors, green for survivors
  ggtitle("Bar Plot of Passenger who Survived") + 
  xlab("Survived (0 = No, 1 = Yes)") + 
  ylab("Count")

#6. Generate a pie chart representing the proportion of passengers from each
#embarkation town.
ggplot(data, aes(x = "", fill = factor(embark_town))) + 
  geom_bar(width = 1) + 
  coord_polar(theta = "y") + 
  ggtitle("Proportion of Passengers by Embarkation Town") + 
  xlab("") + ylab("") + 
  scale_fill_brewer(palette = "Set3")



#7. Show the relationship between age and fare using a scatter plot, with age
#on the x-axis and fare on the y-axis.
ggplot(data, aes(x = age, y = fare)) + 
  geom_point(aes(color = factor(survived))) + 
  ggtitle("Scatter Plot: Age vs Fare") + 
  xlab("Age") + ylab("Fare")

#8. Plot a line chart representing the average fare paid by class over the
#range of ages.
ggplot(data, aes(x = age, y = fare, color = factor(pclass))) + 
  stat_summary(fun = "mean", geom = "line") + # Calculate mean fare by age and plot it
  ggtitle("Average Fare by Class over Age") + 
  xlab("Age") + ylab("Average Fare")

#9. Display a bar plot comparing the survival rates of males and females.
ggplot(data, aes(x = sex, fill = factor(survived))) + 
  geom_bar(position = "dodge") + 
  ggtitle("Survival by Gender") + 
  xlab("Gender") + ylab("Count")

#10.Create a stacked bar plot of passenger counts by class, segmented by
#survival status.
ggplot(data, aes(x = factor(pclass), fill = factor(survived))) + 
  geom_bar(position = "stack") + # Stack the bars to show proportion of survival status within each class
  ggtitle("Passenger Class by Survival Status") + 
  xlab("Class") + ylab("Count")

#11. Plot a side-by-side boxplot comparing fares paid by passengers who
#survived and those who did not.
ggplot(data, aes(x = factor(survived), y = fare, fill = factor(survived))) + 
  geom_boxplot() + 
  ggtitle("Comparison of Fares by Survival Status") + 
  xlab("Survival Status") + ylab("Fare")

#12.Visualize the correlation between the number of siblings/spouses aboard
#and the survival status using a scatter plot.
ggplot(data, aes(x = sibsp, y = survived)) + 
  geom_jitter(aes(color = factor(survived))) + 
  ggtitle("Siblings/Spouses Aboard vs Survival") + 
  xlab("Siblings/Spouses Aboard") + ylab("Survival Status")

#13.Create a histogram of passengers' ages, segmented by whether they had
#parents/children aboard.
ggplot(data, aes(x = age, fill = alone)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  ggtitle("Histogram of Passengers' Ages Segmented by Family Onboard")+
  xlab("age") + ylab("count")

#14.Generate a bar plot displaying the average age of passengers in each
#class.
AverageAge <- data %>%
  group_by(pclass) %>%                  
  summarise(AverageAge = mean(age, na.rm = TRUE)) 

ggplot(AverageAge, aes(x = factor(pclass), y = AverageAge, fill = factor(pclass))) +
  geom_bar(stat = "identity") +
  ggtitle("Average Age of Passengers by Class") +
  labs(x = "Passenger Class", y = "Average Age") +
  theme_minimal()

#15.Show the distribution of passenger ages using a violin plot.
ggplot(data, aes(x = factor(pclass), y = age, fill = factor(pclass))) + 
  geom_violin() + 
  ggtitle("Distribution of Passenger Ages by Class") +
  xlab("Class") + 
  ylab("Age")

#16.Create a scatter plot with the fare paid on the y-axis and age on the
#x-axis, color-coded by the embarkation town.
ggplot(data, aes(x = age, y = fare, color = embarked)) + 
  geom_point() + 
  ggtitle("Fare vs Age, Colored by Embarkation Town") +
  xlab("Age") + 
  ylab("Fare")


#17.Display the cumulative distribution function (CDF) of fares paid by
#passengers.
ggplot(data, aes(x = fare)) + 
  stat_ecdf(geom = "step") + 
  ggtitle("Cumulative Distribution Function of Fares paid by passengers") +
  xlab("Fare") + 
  ylab("CDF")

#18.Create a box plot comparing ages of passengers across different
#embarkation towns.
ggplot(data, aes(x = factor(embarked), y = age, fill=embark_town)) + 
  geom_boxplot() + 
  ggtitle("Age Distribution by Embarkation Town") +
  xlab("Embarkation Town") + 
  ylab("Age")

#19.Generate a line chart of the total number of passengers who survived
#over time (if considering a time component).
data <- data %>% 
  mutate(time = row_number())

ggplot(data, aes(x = time, y = cumsum(survived))) +
  geom_line(color = "blue") +
  labs(title = " Total Number of Survivors Over Time", 
       x = "Time", 
       y = "Number of Survivors")


#20.Visualize the relationship between survival and the number of
#siblings/spouses aboard using a grouped bar chart.
ggplot(data, aes(x = factor(sibsp), fill = factor(survived))) + 
  geom_bar(position = "dodge") + 
  ggtitle("Survival Status by Number of Siblings/Spouses Aboard") +
  xlab("Siblings/Spouses Aboard") + 
  ylab("Count")

#21.Create a histogram of the fare paid by passengers, highlighting those who
#survived versus those who did not.
ggplot(data, aes(x = fare, fill = factor(survived))) + 
  geom_histogram(binwidth = 10, position = "identity") + 
  ggtitle("Fare Distribution by Survival Status") + 
  xlab("Fare") + 
  ylab("Count")

#22.Plot a stacked histogram of passenger ages, differentiated by survival
#status.
ggplot(data, aes(x = age, fill = factor(survived))) + 
  geom_histogram(binwidth = 5, position = "stack",color="black") + 
  ggtitle("Age Distribution by Survival Status") + 
  xlab("Age") + 
  ylab("Count")

#Is this a good way to present the information for comparison?
#Ans.Using a stacked histogram can be effective for comparing distributions 
#of ages between different groups (survivors vs. non-survivors). 
#However, it can sometimes make it difficult to assess the 
#individual distribution of each group.Using side-by-side histograms or density plots
#will give clear comparison.


#23.Generate a bar plot showing the count of passengers in each class,
#segmented by embarkation town.
ggplot(data, aes(x = factor(pclass), fill = factor(embarked))) + 
  geom_bar(position = "dodge") + 
  ggtitle("Passenger Count in each Class Segemented by Embarkation Town") + 
  xlab("Class") + 
  ylab("Count")

#24.Create a scatter plot of the fare paid and the age of passengers,
#differentiating the plot color by gender.
ggplot(data, aes(x = age, y = fare, color = factor(sex))) + 
  geom_point() + 
  ggtitle("Scatter Plot of Fare Paid vs Age, Differentiated by Gender") +
  xlab("Age") + 
  ylab("Fare")

#25.Plot a line chart showing the average age of passengers over the fare
#ranges
  # Create fare ranges by binning the Fare column
data <- data %>%
  mutate(fare_bin = cut(fare, breaks = seq(0, max(fare, na.rm = TRUE), by = 10)))
  # Calculate the average age for each fare range
average_age_fare <- data %>%
  group_by(fare_bin) %>%
  summarise(average_age = mean(age, na.rm = TRUE))

  # Line chart of average age over fare ranges
ggplot(average_age_fare, aes(x = fare_bin, y = average_age, group = 1)) +
  geom_line(color = "blue") +
  labs(title = "Average Age of Passengers Over Fare Ranges", 
       x = "Fare Range", 
       y = "Average Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###BONUS QUESTIONS####

#26.What was the average fare paid by passengers in each class and survival status?
  # Calculate the average fare by class and survival status
avg_fare_by_class_survived <- data %>% 
  group_by(pclass, survived) %>% 
  summarise(AverageFare = mean(fare, na.rm = TRUE))

  # Print the result
print(avg_fare_by_class_survived)

  # Plot the result
ggplot(avg_fare_by_class_survived, aes(x = factor(pclass), y = AverageFare, fill = factor(survived))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Average Fare by Class and Survival Status") + 
  xlab("Passenger Class") + 
  ylab("Average Fare") + 
  scale_fill_manual(values = c("violet", "lightgreen"), labels = c("Did Not Survive", "Survived"))

#27.What percentage of males and females survived in each class?
  # Calculate the percentage of survivors by gender and class
survival_by_gender_class <- data %>% 
  group_by(sex, pclass) %>% 
  summarise(SurvivalRate = mean(survived) * 100)

  # Print the result
print(survival_by_gender_class)

  # Plot the result
ggplot(survival_by_gender_class, aes(x = factor(pclass), y = SurvivalRate, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Survival Rate by Gender and Class") + 
  xlab("Passenger Class") + 
  ylab("Survival Rate (%)") + 
  scale_fill_manual(values = c("lightgreen", "purple"))

#28. What was the average age of survivors versus non-survivors for each gender?
  # Calculate the average age of survivors vs. non-survivors by gender
avg_age_by_survival_gender <- data %>% 
  group_by(sex, survived) %>% 
  summarise(AverageAge = mean(age, na.rm = TRUE))

  # Print the result
print(avg_age_by_survival_gender)

  # Plot the result
ggplot(avg_age_by_survival_gender, aes(x = factor(survived), y = AverageAge, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Average Age of Survivors vs. Non-Survivors by Gender") + 
  xlab("Survived (0 = No, 1 = Yes)") + 
  ylab("Average Age") + 
  scale_fill_manual(values = c("blue", "pink"))


#29.What is the survival rate for passengers based on their embarkation town?
  # Calculate survival rate by embarkation town
survival_by_embarked <- data %>% 
  group_by(embarked) %>% 
  summarise(SurvivalRate = mean(survived, na.rm = TRUE) * 100)

  # Print the result
print(survival_by_embarked)

  # Plot the result
ggplot(survival_by_embarked, aes(x = embarked, y = SurvivalRate, fill = embarked)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Survival Rate by Embarkation Town") + 
  xlab("Embarkation Town") + 
  ylab("Survival Rate (%)") + 
  scale_fill_brewer(palette = "Set2")

#30.What is the average fare paid by passengers who traveled alone (Family Size = 0) vs. those
  #who traveled with family?
  # Calculate family size (sibsp + parch)
data <- data %>% mutate(FamilySize = sibsp + parch)

  # Calculate the average fare by family size (0 = traveling alone)
avg_fare_by_family_size <- data %>% 
  group_by(FamilySize) %>% 
  summarise(AverageFare = mean(fare, na.rm = TRUE))

  # Print the result
print(avg_fare_by_family_size)

  # Plot the result
ggplot(avg_fare_by_family_size, aes(x = factor(FamilySize), y = AverageFare, fill = factor(FamilySize))) + 
  geom_bar(stat = "identity") + 
  ggtitle("Average Fare by Family Size") + 
  xlab("Family Size (SibSp + ParCh)") + 
  ylab("Average Fare") + 
  scale_fill_brewer(palette = "Blues")

