mean(alive$age,na.rm=TRUE)
rm(list=ls())
library(Hmisc)
data <- read.csv("C:/Users/91877/OneDrive/Desktop/covid_R/COVID19_line_list_data.csv")
describe(data)
data$death_dummy<-as.integer(data$death!=0)
sum(data$death_dummy)/nrow(data)
#people who died are older than people who survived
dead=subset(data,death_dummy==1)
alive=subset(data,death_dummy==0)
mean(dead$age,na.rm=TRUE)
mean(alive$age,na.rm=TRUE)
#statistical signifiant
t.test(alive$age,dead$age,alternative ="two.sided" ,conf.level =0.95)
#gender based
men=subset(data,gender=="male")
women=subset(data,gender=="female")
mean(men$death_dummy,na.rm=TRUE)
mean(women$death_dummy,na.rm=TRUE)
#statistical signifiant
t.test(men$death_dummy,women$death_dummy,alternative ="two.sided" ,conf.level =0.95)
#p-value<0.05,reject null hypothesis
#library(psych)
install.packages("ggplot2")
install.packages("ggplot2")
library(ggplot2)
# Gender Distribution
ggplot(data, aes(x = gender)) +
geom_bar(fill = "blue") +
labs(title = "Gender Distribution", x = "Gender", y = "Count")
# Death Distribution
ggplot(data, aes(x = death_dummy)) +
geom_bar(fill = "red") +
labs(title = "Death Distribution", x = "Death (0: Survived, 1: Died)", y = "Count")
# Age Distribution
ggplot(data, aes(x = age, fill = death_dummy)) +
geom_histogram(bins = 20) +
labs(title = "Age Distribution by Survival", x = "Age", y = "Count") +
scale_fill_manual(values = c("blue", "red"), guide = FALSE)  # Blue for survived, red for died
# Country Distribution
ggplot(data, aes(x = country)) +
geom_bar(fill = "green") +
labs(title = "Country Distribution", x = "Country", y = "Count") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
ggplot(data, aes(x = gender)) +
geom_bar(fill = "blue") +
labs(title = "Gender Distribution", x = "Gender", y = "Count")
ggplot(data, aes(x = death_dummy)) +
geom_bar(fill = "red") +
labs(title = "Death Distribution", x = "Death (0: Survived, 1: Died)", y = "Count")
ggplot(data, aes(x = age, fill = death_dummy)) +
geom_histogram(bins = 20) +
labs(title = "Age Distribution by Survival", x = "Age", y = "Count") +
scale_fill_manual(values = c("blue", "red"), guide = FALSE)
ggplot(data, aes(x = country)) +
geom_bar(fill = "green") +
labs(title = "Country Distribution", x = "Country", y = "Count") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data, aes(x = age, y = country)) +
geom_point() +
xlab("Age") +
ylab("Country") +
ggtitle("Scatterplot of Age vs. Country")
gender_plot <- ggplot(data, aes(x = gender, fill = factor(death_dummy))) +
geom_bar(position = "fill") +
labs(title = "Gender vs. Death Ratio",
x = "Gender",
y = "Proportion") +
scale_fill_manual(values = c("Alive" = "lightblue", "Dead" = "red")) +
theme_minimal()
print(gender_plot)
#scatterplot for age and country
ggplot(data, aes(x = age, y = country)) +
geom_point() +
xlab("Age") +
ylab("Country") +
ggtitle("Scatterplot of Age vs. Country")
# Country Distribution
ggplot(data, aes(x = country)) +
geom_bar(fill = "green") +
labs(title = "Country Distribution", x = "Country", y = "Count") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
gender_plot <- ggplot(data, aes(x = gender, fill = factor(death_dummy))) +
geom_bar(position = "fill") +
labs(title = "Gender vs. Death Ratio",
x = "Gender",
y = "Proportion") +
scale_fill_manual(values = c("Alive" = "lightblue", "Dead" = "red")) +
theme_minimal()
print(gender_plot)
ggplot(data, aes(x = age)) +
geom_histogram(binwidth = 5, fill = "blue", color = "black") +
labs(title = "Age Distribution", x = "Age", y = "Frequency")
ggplot(data, aes(x = gender)) +
geom_bar(fill = "green", color = "black") +
labs(title = "Gender Distribution", x = "Gender", y = "Frequency")
dead=subset(data,death_dummy==1)
alive=subset(data,death_dummy==0)
mean(dead$age,na.rm=TRUE)
mean(alive$age,na.rm=TRUE)
ggplot(data, aes(x = age, fill = death_dummy)) +
geom_histogram(bins = 20) +
labs(title = "Age Distribution by Survival", x = "Age", y = "Count") +
scale_fill_manual(values = c("blue", "red"), guide = FALSE)  # Blue for survived, red for died
