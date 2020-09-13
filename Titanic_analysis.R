##############TITANIC :( ###################
setwd("/Users/akankwasa/Desktop/R/Personal R/titanic/")

library(dplyr)
library(ggplot2)
library(lattice)

titanic.df <- read.csv("~/Desktop/R/Personal R/titanic/train.csv", header = TRUE, sep = ",", na.strings = "")
# With header=TRUE we are specifying that the data includes a header(column names) 
# sep=”,” specifies that the values in data are comma separated
#passed the parameter na.string=”” so that empty values are read as NA values
# row.names = NULL was put because there was an error with duplicate row names so one of the columns may be missing one or more names.


titanic.df

head(titanic.df,5)
tail(titanic.df, 6) #head and tail () take default value of 5 and displays 5 rows.
names(titanic.df)
str(titanic.df) #This helps in understanding the structure of the data set, data type of each attribute and number of rows and columns present in the data.
summary(titanic.df)
#summary()gives summary descriptive statistics. for factor, gives table of frequencies, character variables gives length of class, etc
# in case of factor + numerical variables, gives number of missing values, for numerical varaibles, gives Mean, Median, Mode, Range and Quartiles.

titanic.df$Survived <- as.factor(titanic.df$Survived)
titanic.df$Pclass <- as.factor(titanic.df$Pclass)
titanic.df$Sex <- as.factor(titanic.df$Sex)
titanic.df$Embarked <- as.factor(titanic.df$Embarked)

ggplot(data = titanic.df,
       aes(x = Survived, fill = Survived))+
  geom_bar()+
  theme(legend.position = "bottom")

prop.table(table(titanic.df$Survived)) #checking out percentage of surviavl rate
# we can see about 38.38% survived, ~61.62% did not.

# let us visualise survival rate by gender
ggplot(titanic.df, 
       aes(x = Sex, fill = Survived))+
  theme_bw()+ # this uses the class black-white theme
  geom_bar()+
  labs(y = "number of...", caption = "more men died :(",
  title = "Survival rates by gender")
  
# survival rate by passenger class
ggplot(titanic.df,
       aes(x = Pclass, fill = Survived))+
  geom_bar()+
  labs(y = "num. of ...", title = "Survival by passenger class", caption = "look at capitalism :(")

# survival rate by age
ggplot(titanic.df,
       aes(x = Age, fill = Survived))+
  theme_bw()+
  geom_histogram(binwidth = 5)+
  labs(y = "num. of ...", title = "Survival by age", caption = "")

ggplot(titanic.df,
       aes(x = Sex, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  labs(y = "num. of ...", title = "Survival by passenger class", caption = "Ladies in first class were pretty lucky")+
  theme_bw()


library(GGally) #plotting a correlation plot
ggcorr(titanic.df,
       nbreaks = 4,
       label = TRUE,
       label_size = 3, 
       color = "grey50")


#survival patterns considering all 3 variables
ggplot(titanic.df,
       aes(x = Age, fill = Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(Sex~Pclass)+
  labs(y = "num. of ...", title = "Survival by passenger class", 
       caption = "Notice that younger people, children survived compared to older")+
  theme_bw()
  