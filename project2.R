library(ggplot2)

data <- read.csv("C:/Local Files/University/YR1/SEM 2/DATA1002/Code/Datasets/Project2_SurveyData13.9.csv")

data
# later dinner
# alcohol consumption
# do whatever with the data, make as many different types of graphs as possible like time of sleep and quality 


#RMB FORMAT

head(data)

unique(data$AlcoholConsumed)
data$OrderedAlcoholConsumed <- factor(data$AlcoholConsumed, levels = c("None", "<1", "1 to 3", 
																  "3 to 5", "5 to 7", "7 to 9", 
																  "10 to 12", "12 to 15", "15+"))

ggplot(data, aes(x = OrderedAlcoholConsumed)) + geom_bar()

ggplot(data, aes(x = OrderedAlcoholConsumed, y = HoursOfSleep)) + geom_point()

ggplot(data, aes(x = OrderedAlcoholConsumed, y = QualityOfSleep)) + geom_point()

ggplot(data, aes(x = HoursOfSleep, y = OrderedAlcoholConsumed)) + geom_boxplot()

