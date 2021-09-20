library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("C:/Local Files/University/YR1/SEM 2/DATA1002/Code/Datasets/Project2_SurveyData13.9.csv")

#ordering the data into levels and saving it into the data set as a new variable OrderedAlcoholConsumed
data$OrderedAlcoholConsumed <- factor(data$AlcoholConsumed, levels = c("None", "<1", "1 to 3", 
																	   "3 to 5", "5 to 7", "7 to 9", 
																	   "10 to 12", "12 to 15", "15+"))


## Create a new data frame called data_aggrigateByAlcohol, using the original data set
data_aggrigateByAlcohol <- data %>% 
	## group this data by the Alcohol the person consumed
	group_by(OrderedAlcoholConsumed) %>%  
	## summarise the variables Hours of sleep and times woken up at this class level. 
	#Calculate the mean and label it "avg"; calculate the median and label it "med".  Remove any null values with the na.rm=TRUE
	summarise_at(vars(HoursOfSleep,TimesWakeUp, QualityOfSleep), list(avg = ~mean(., na.rm=TRUE), med = ~median(., na.rm=TRUE)))

ggplot(data = data_aggrigateByAlcohol) + 
	geom_bar(mapping = aes(x = OrderedAlcoholConsumed, y = TimesWakeUp_avg, fill = OrderedAlcoholConsumed), stat ="identity") + 
	
	#recolouring the columns to different colours
	scale_fill_manual(values=c("#E35966","#F0965D","#F0D969","#B0DE78","#78DEB2", "#78A1DE", "#A378DE", "#D078DE", "#DE78B5")) +
	
	#putting all the labels
	xlab("Number of standard drinks consumed per week") + 
	ylab("Average number of times woken up during sleep") + 
	labs(title = "Times woken up against alcohol consumption")


ggplot(data = data_aggrigateByAlcohol) + 
	geom_bar(mapping = aes(x = OrderedAlcoholConsumed, y = HoursOfSleep_avg, fill = OrderedAlcoholConsumed), stat ="identity") + 
	
	scale_fill_manual(values=c("#E35966","#F0965D","#F0D969","#B0DE78","#78DEB2", "#78A1DE", "#A378DE", "#D078DE", "#DE78B5")) +
	
	xlab("Number of standard drinks consumed per week") + 
	ylab("Average hours of sleep") + 
	labs(title = "Hours of sleep against alcohol consumption") + 
	#recommended hours of sleep
	geom_hline(yintercept = 7)


ggplot(data = data_aggrigateByAlcohol) + 
	geom_bar(mapping = aes(x = OrderedAlcoholConsumed, y = QualityOfSleep_avg, fill = OrderedAlcoholConsumed), stat ="identity") + 
	scale_fill_manual(values=c("#E35966","#F0965D","#F0D969","#B0DE78","#78DEB2", "#78A1DE", "#A378DE", "#D078DE", "#DE78B5")) +
	xlab("Number of standard drinks consumed per week") + 
	ylab("Average hours of sleep") + 
	labs(title = "Hours of sleep against alcohol consumption")


# WIP
ggplot(data, aes(x = HoursOfSleep, y = OrderedAlcoholConsumed)) + geom_boxplot()

ggplot(data) + 
	geom_boxplot(aes(x = TimesWakeUp, y = OrderedAlcoholConsumed, fill = OrderedAlcoholConsumed)) + 
	scale_fill_manual(values=c("#e359c1", "#bf4389", "#a13564", "#7d2538", "#e35959","#e37c59","#e39c59","#e3ac59","#e3be59")) + 
	ylab("Number of standard drinks consumed per week") + 
	xlab("Times woken up at night")
	
?ggplot


