library(ggplot2)
library(tidyr)
library(dplyr)


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

melted <- melt(data, id.vars=c("id", "group"))

means <- ddply(melted, c("group", "variable"), summarise,
			   mean=mean(value))

means.barplot <- qplot(x=group, y=mean, fill=variable,
					   data=means, geom="bar", stat="identity",
					   position="dodge")

# Syntax: aggregate(x = dataset_Name , by = group_list, FUN = any_function) 

HoursOfSleepForAlcohol <- aggregate(x = data$HoursOfSleep, #uses the hours of sleep to get the means
						  #gets the alcohol as a list so that every hours of sleep correlates to it 
						  by = list(data$OrderedAlcoholConsumed), 
						  #uses the function mean for each value of OrderedAlcoholConsumed
						  FUN = mean) 

data_aggrigateByAlcohol <- data %>%  ## Create a new data frame called mpg_aggbyclass, using mpg data
							group_by(OrderedAlcoholConsumed) %>%  ## group this data by the "class" variable
							summarise_at(vars(HoursOfSleep,TimesWakeUp,QualityOfSleep), list(avg = ~mean(., na.rm=TRUE), med = ~median(., na.rm=TRUE)))
						## summarise the variables hwy and cty at this class level. Calculate the mean and label it "avg"; calculate the median and label it "med".  Remove any null values with the na.rm=TRUE
data_aggrigateByAlcohol$HoursOfSleep_avg

ggplot(data = data_aggrigateByAlcohol) + 
	geom_bar(mapping = aes(x = OrderedAlcoholConsumed, y = TimesWakeUp_avg, fill = OrderedAlcoholConsumed), stat ="identity") + 
	scale_fill_manual(values=c("#E35966","#F0965D","#F0D969","#B0DE78","#78DEB2", "#78A1DE", "#A378DE", "#D078DE", "#DE78B5")) +
	xlab("Number of standard drinks consumed per week") + 
	ylab("Average number of times woken up during sleep") + 
	labs(title = "Times woken up against alcohol consumption")

ggplot(data = data_aggrigateByAlcohol) + 
	geom_bar(mapping = aes(x = OrderedAlcoholConsumed, y = HoursOfSleep_avg, fill = OrderedAlcoholConsumed), stat ="identity") + 
	scale_fill_manual(values=c("#E35966","#F0965D","#F0D969","#B0DE78","#78DEB2", "#78A1DE", "#A378DE", "#D078DE", "#DE78B5")) +
	xlab("Number of standard drinks consumed per week") + 
	ylab("Average hours of sleep") + 
	labs(title = "Hours of sleep against alcohol consumption")

ggplot(data = data_aggrigateByAlcohol) + 
	geom_bar(mapping = aes(x = OrderedAlcoholConsumed, y = QualityOfSleep_avg, fill = OrderedAlcoholConsumed), stat ="identity") + 
	scale_fill_manual(values=c("#E35966","#F0965D","#F0D969","#B0DE78","#78DEB2", "#78A1DE", "#A378DE", "#D078DE", "#DE78B5")) +
	xlab("Number of standard drinks consumed per week") + 
	ylab("Average hours of sleep") + 
	labs(title = "Hours of sleep against alcohol consumption")



?scale_fill_manual

data_aggrigateByAlcohol


HoursOfSleepForAlcohol
HoursOfSleepForAlcohol$Group.1
HoursOfSleepForAlcohol$x

geom_bar()

ggplot(HoursOfSleepForAlcohol, aes(y = x)) + geom_bar()

plot(HoursOfSleepForAlcohol$x)

?geom_bar()

#pure number for the hours of sleep			
mean(data$HoursOfSleep)

ggplot(data, aes(x = HoursOfSleep, y = TimesWakeUp)) + geom_point()


unique(data$TimeOfSleep)
#sk
data$OrderedTimeOfSleep <- factor(data$TimeOfSleep, levels = c("earlier than 7:00pm", "7:00pm - 8:00pm", "8:00pm - 9:00pm", 
																"9:00pm - 10:00pm" ,"10:00pm - 11:00pm", "11:00pm - 12:00am", 
																"12:00am - 1:00am", "1:00am - 2:00am", "after 2:00am"))

data$OrderedTimeOfSleep

ggplot(data, aes(x = OrderedTimeOfSleep)) + geom_bar()




