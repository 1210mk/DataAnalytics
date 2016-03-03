setwd("~/Desktop/Data Analytics /Data ")
Final<-read.csv("Final.csv")
View(Final)
FinalOrg<-Final
names(Final)
length(Final)
nrow(Final)
summary(Final$U5MR)
summary(Final$HDI.2013)
summary(Final$Contraceptive.Prevelance..)
summary(Final$ODA.inflow)
var.keep<-c("U5MR","ODA.inflow","HDI.2013","Sanitation.Facilities")
Final<-Final[order(Final$Country,decreasing = F),]
library(descr)
library(Hmisc)
freq(Final$Internet.Users.Per.100.population,main="Internet Users per 100 in population",xlab="number of users",ylab="Frequency")
freq(Final$HDI.2013,main="Human Development Index 2013",xlab="HDI index",ylab="Frequency") 
freq(Final$U5MR, main="Under 5 mortality rate",xlab="Mortality Rate",ylab="Frequency")
freq(Final$Skilled.Attendant.at..birth,main="Skilled Attendant at Birth",xlab="number of attendants",ylab="frequency")
#freq(Final$HDI.2013)......Warning message:display list redraw incomplete 
#freq(Final$U5MR)- Error in plot.new() : figure margins too large

#DATA MANAGMENT EXCERSISE 
#ggplot2 example 
library(descr)
library(ggplot2)
options(digits=2)
View(CleanData)
#ggplot(CleanData,aes(x=as.factor(U5MR)))+geom_bar(stat="count",) - Because my data is discrete and not continuous 
ggplot(CleanData, aes(x=HDI.2013, y=U5MR)) + geom_point(shape=16, size = 4, color = "red") +labs(x = "Human Development Index", y = "Under 5 Mortality Rate") + ggtitle ("Relationship between Human Development and Under 5 mortality rates") + theme_bw()
#Sanitation Facilities and HDI show curve/line fit 
ggplot(na.omit(CleanData), aes(x=Sanitatation.Facilities, y=U5MR)) 
+ geom_point(shape=16, size = 4, color = "red") 
+labs(x = "Sanitation Facilities", y = "Under 5 Mortality Rate") 
+ ggtitle ("Relationship between Sanitation Facilities and Under 5 mortality rates") + theme_bw()

#Regression Analysis 
CleanData$Sanitatation.Facilities<-as.integer(CleanData$Sanitatation.Facilities) #I had to change my data from factors to integer
CleanData$Skilled.Attendant.at..birth<-as.integer(CleanData$Skilled.Attendant.at..birth)
CleanData$ODA.inflow<-as.integer(CleanData$ODA.inflow)
CleanData$HDI.2013<-as.integer(CleanData$HDI.2013)
CleanData$GNI.2013<-as.integer(CleanData$GNI.2013)
CleanData$Population..Thousands..2013<-as.integer(CleanData$Population..Thousands..2013)
CleanData$Life.Expectancy.2013<-as.integer(CleanData$Life.Expectancy.2013)
CleanData$Total.Fertility.Rate<-as.integer(CleanData$Total.Fertility.Rate)
HumanDevelopment.lm <- lm(U5MR ~ HDI.2013, data=CleanData) #This is where regression analysis starts 
summary(HumanDevelopment.lm)
HumanDevelopment3.lm<-lm(U5MR ~ HDI.2013^2 -HDI.2013, data = CleanData)
HumanDevelopment4.lm<-lm(log(U5MR) ~ HDI.2013, data =CleanData) #Logarithmic relationship between U5MR and HDI it shows higher values and fits better with my data
summary(HumanDevelopment4.lm)
Life.Expectancy.lm<-lm(U5MR ~ Life.Expectancy.2013, data=CleanData)
summary(Life.Expectancy.lm)
HumanDevelopment4.lm<-lm(log(U5MR) ~ HDI.2013 + Life.Expectancy.2013, data =CleanData)
 HumanDevelopment5.lm<-lm(log(U5MR) ~ HDI.2013 + Life.Expectancy.2013, data =CleanData)
 #HumanDevelopment6.lm<-lm(log(U5MR) ~ HDI.2013 + Life.Expectancy.2013 + Total.Fertility.Rate, data =CleanData) #Multiple Linear Regression
 