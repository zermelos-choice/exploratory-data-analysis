#### Udacity - Exploratory Data Analysis workbook ####
#8.7.18#
library(ggplot2)
library(RColorBrewer)
data(diamonds)
qplot(data=diamonds, x=carat,y=price,color=cut) + scale_color_brewer(palette = 'Accent')



getwd()
setwd('~/Documents/EDA_Data')
statesInfo<-read.csv('stateData.csv')
subset(statesInfo, state.region==1)


#dataSet[Row condition, column condition]
#If left blank, yields all columns

statesInfo[statesInfo$state.region==1,]
#find subsets for illerteracy, highschool graduation rate
illiteracybracket<-statesInfo[statesInfo$illiteracy==0.5,]

highschoolgradbracket <- statesInfo[statesInfo$highSchoolGrad>50,]

qplot(data=statesInfo, x=state.region, y=illiteracy, color=statesInfo$highSchoolGrad) 

## REDDIT DATA SET
reddit<-read.csv("reddit.csv")
#str(reddit)
#Factor == Categorical information
table(reddit$employment.status)
#the above yields a list of emplyomeny status entries. 
summary(reddit$employment.status)
levels(reddit$age.range)
# notice 7 different ranges
qplot(reddit$age.range)
qplot(data=reddit, x=income.range)

#We no rearrange the levels, so that the graph is easier to read/more cogent.

reddit$age.range<-ordered(reddit$age.range, levels=c('Under 18', '18-24', '25-34', '35-44', '45-54', '55-64','65 or Above', 'NA'))
qplot(data=reddit, x=age.range)


#now the data is rearranged as required. 
#Alternatively 

reddit$age.range <- factor(reddit$age.range, levels=c('Under 18', '18-24', '25-34', '35-44', '45-54', '55-64','65 or Above', 'NA'), ordered = T)


#similarly, we can do the same for the income brackets. 

reddit$income.range<-ordered(reddit$age.range, levels=c("Under $20,000", '$20,000 - $29,999', '$30,000 - $39,999', '$40,000 - $49,999', '$50,000 - $69,999', '$70,000 - $99,999', '$100,000 - $149,999', '$150,000 or more', 'NA'))
qplot(data=reddit, x=income.range)

##Lesson 3 - exploring one variable
# First I downloaded the pseudo-facebook data, which is found in the EDA folder
getwd()

setwd('~/Documents/EDA_Data')
list.files()
#notice that the pseudo_facebook data is actually a TAB separated file. Therefore, we must use the following command. 
pf<-read.csv('pseudo_facebook.tsv', sep='\t')
names(pf)
#This data set was created by Udacity, so the stats may not align.


### First we will look at birth dates 
#first, we load ggplot

library(ggplot2)
install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
names(pf)
#a histogram of days of the month that individuals were born on is given by the command below
#qplot(x=dob_day, data=pf) + 
 # geom_histogram(binwidth=1) +
  #scale_x_discrete(breaks = 1:31)
#ALternatively, we have the following. 

ggplot(aes(x = dob_day), data = pf) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:31)
#data question: whether a users perception of their audience, matches audience.
#Interesting because it depends on how a user presents themselve. 
# survey: "How many people do you think saw this post?"
#gap betweeen expectation and reality was apparent. 
# people actually underestimated audience size. 

### The + sign indicates a " layer " of a plot or graph. 
#Now we add facet_wrap function to break up the data into months.
ggplot(aes(x = dob_day), data = pf) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 1:31) +
  facet_wrap(~dob_month,ncol=4)
#changing ncol=n, yields a different number of columns, i.e. 3 or 4, etc.
#facet_wrap(~variable)

#How to handle outliers 


### HOW TO ADJUST AXES
#Friend count histogram
qplot(x=friend_count, data=pf) 

#This can then be limited as follow
qplot(x=friend_count, data=pf, xlim=c(0,1000)) 

# or rather
qplot(x=friend_count, data=pf) + 
  scale_x_continuous(limit=c(0,1000))

#notice the bin width error. 
#adding the following command then yields
qplot(x=friend_count, data=pf, binwidth=25) + 
  scale_x_continuous(limit=c(0,1000), breaks=seq(0,1000,50))  
#this makes the histogram more apparent.
# Data question: Which gender tends to have more friends? 
# A: We can see this by computing the following data. 
qplot(x=friend_count, data=pf, binwidth=25) + 
  scale_x_continuous(limit=c(0,1000), breaks=seq(0,1000,50)) +
  facet_grid(~gender)
# gender is the splitting variable in the facet_grid. 

#Notice in the above that we got three columns, since there was some missing data which R interprets as NA.
# To look at just the two genders, we can 

ggplot(aes(x = friend_count), data = subset(pf, !is.na(gender))) +
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)
#Note we caould us omit.na(), too, but it is a bit more problematic since it could remove more than we intended.

#Now we want to determine who has more friends on average. To see this run the table command to see if there are more or less men v women.
table(pf$gender)
#This tells us that there are female 40254 and  male 58574

by(pf$friend_count,pf$gender,summary)
# the function above applies a function to both the efirst and second 


### NOW WE WILL ADD COLORS!!!!!

#Consider the following 
ggplot(aes(x = tenure), data = pf) +
  geom_histogram(binwidth = 30, color = 'black', fill = '#099DD9')
#tenure is the number of days that a person has been a member. 

qplot(x=tenure/365, data=pf, binwidth=1/12, color = I('black'), fill = I('#099DD9'), ylab='tenure', xlab='years')+ 
  scale_x_continuous(seq(1,7,1), limits = c(0,7))


qplot(x=age, data=pf, binwidth=1, color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(seq(5,110,5), limits = c(5,110))

#Transforming DATA
#engagment variables have fat tail. i.e. over dispersed.

qplot(x=friend_count, data=pf)
summary(pf$friend_count)
summary(log10(pf$friend_count+1))
#transformation by log10 demonstrates order of magnitude by 10s.
#We add 1 so that the 0 count is well defined. 
summary(sqrt(pf$friend_count))      
#Now try to add all three plots to a single page 
#first install the following
install.packages('gridExtra')
library(gridExtra)
#Then we can plot all three as follows
p1=qplot(x=friend_count, data=pf)
p2=qplot(x=log10(friend_count+1), data=pf)
p3=qplot(x=sqrt(friend_count),data=pf)
grid.arrange(p1,p2,p3)

#alternative
p1<-ggplot(aes(x=friend_count), data=pf) + geom_histogram()
p2<-p1+scale_x_log10()
p3<-p1+scale_x_sqrt()
grid.arrange(p1,p2,p3)

#There is some subtle difference in how the qplot and ggplot solutions work, so we will compare them as follows

logScale<-qplot(x=log10(friend_count+1), data=pf)
countScale<-ggplot(aes(x=friend_count), data=pf) + geom_histogram() +scale_x_log10()
grid.arrange(logScale,countScale, ncol = 2)
#ncol plots side by side
#notice that the x axis is different. 


#FREQUENCY POLYGON
qplot(x=friend_count, data= subset(pf,!is.na(gender)), binwidth=10, geom='freqpoly', color=gender) +
  scale_x_continuous(lim=c(0,1000), breaks=seq(0,1000,50))
#notice that we added geom and color functions

#ggplot syntax
ggplot(aes(x = friend_count, y = ..count../sum(..count..)),
       data = subset(pf, !is.na(gender))) +
  geom_freqpoly(aes(color = gender), binwidth=10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  xlab('Friend Count') +
  ylab('Proportion of users with that friend count')


#Next we will consider proportions 
qplot(x=friend_count, y=..count../sum(..count..),
      data= subset(pf,!is.na(gender)),
      xlab='Friend Count',
      ylab='Proportion of users w/ friend count',
      binwidth=10, geom='freqpoly', color=gender) +
  scale_x_continuous(lim=c(0,1000), breaks=seq(0,1000,50))
#notice the y axis count is different 
#CHANGE: limits and breaks

#now analyze for www likes
qplot(x=www_likes,
      data= subset(pf,!is.na(gender)),
      geom='freqpoly', color=gender) +
  scale_x_continuous()+
  scale_x_log10()

#Numerical analysis of likes
by(pf$www_likes,pf$gender, sum)



### BOX PLOTS
qplot(x=friend_count, data= subset(pf,!is.na(gender)), binwidth=10, geom='freqpoly', color=gender) +
  scale_x_continuous(lim=c(0,1000), breaks=seq(0,1000,50))
qplot(geom='boxplot', data=subset(pf,!is.na(gender)),
      x=gender,
      y=log10(friend_count),
      ylim=c(0,1000))
#rescale so that the number of outliers is minimized.
#or we can use
qplot(geom='boxplot', data=subset(pf,!is.na(gender)),
      x=gender,
      y=friend_count)+
  scale_y_continuous(limits=c(0,1000))

#or rather
qplot(geom='boxplot', data=subset(pf,!is.na(gender)),
      x=gender,
      y=friend_count)+
  coord_cartesian(ylim=c(0,1000))
#notice that the female box has moved closer to 250 in comparison to the other.
#we can further adjust this as follows 

qplot(geom='boxplot', data=subset(pf,!is.na(gender)),
      x=gender,
      y=friend_count)+
  coord_cartesian(ylim=c(0,250))
#now compute a summary
by(pf$friend_count,pf$gender,summary)


#friendships initiated
qplot(x=gender, y=friendships_initiated,data=subset(pf,!is.na(gender)),geom='boxplot')+
  coord_cartesian(ylim=c(0,150))
#numerical verification is then given by
by(pf$friendships_initiated,pf$gender,summary)


#check if people use mobile check in. Use booleans 
summary(pf$mobile_likes)
summary(pf$mobile_likes>0)

pf$mobile_check_in<-NA
pf$mobile_check_in<-ifelse(pf$mobile_likes>0,1,0)
pf$mobile_check_in<-factor(pf$mobile_check_in)
summary(pf$mobile_check_in)
sum(pf$mobile_check_in==1)/length(pf$mobile_check_in)

#QUIZ EXPLORE SINGLE VARIABLE 
library(ggplot2)
data(diamonds)
summary(diamonds)

#notice that three of the variables are ordered

summary(diamonds$price)
sum(ifelse(diamonds$price>=15000,1,0))
ggplot(aes(x = price), data = diamonds) +
  geom_histogram(binwidth = 20, color = 'black', fill = '#099DD9')+
  scale_x_continuous(limits=c(0,1500))
#now we price by cut (this ought to be useful)

ggplot(aes(x = price),data = diamonds) +
  geom_histogram() +
  scale_x_continuous() +
  facet_wrap(~cut, scales="free_y")

by(diamonds$price,diamonds$cut,summary)
summary(diamonds)

ggplot(aes(x = log10(price/carat)),data = diamonds) +
  geom_histogram() +
  scale_x_continuous() +
  facet_wrap(~cut, scales="free_y")

qplot(geom='boxplot', data=diamonds,
      x=color,
      y=price/carat) +
  coord_cartesian( ylim=c(0,7000))


by(diamonds$price,diamonds$color,IQR)
?diamonds


qplot(x=price, data=diamonds, binwidth=40, geom='freqpoly', color=color)

qplot(data=diamonds,x=carat, geom = 'freqpoly',binwidth=0.1, color=carat)+
  scale_x_continuous(breaks = seq(0,3,0.1),lim= c(0,3))+
  scale_y_continuous(breaks = seq(2000,10000,1000))


#With the diamonds dataset questions complete, I will now begin analyzing the unemployment dataset I found on mindgapper or whatever database.

setwd('~/Documents/EDA_Data')
library(tidyr)
gdp <- read.csv("GDP.csv", header=T, check.names = F)

gdp.T <- gather(data=gdp, key='Year', value='GDP', '1960':'2011',convert = TRUE)
range(gdp.T$Year)


hiv <- read.csv("HIV.csv", header=T, check.names = F)
hiv.T <- gather(data=hiv, key='Year', value='HIV_prev', '1979':'2011', convert = TRUE)
hiv.T$HIV_prev <- as.numeric(hiv.T$HIV_prev)

summary(hiv.T)

gdp.HIV <- merge(gdp.T, hiv.T)
str(gdp.HIV)

summary((is.na(gdp.HIV$GDP)) / nrow(gdp.HIV) * 100)
summary(gdp.HIV$GDP)
summary(gdp.HIV$HIV_prev)
qplot(y=gdp.HIV$`Income per person (fixed 2000 US$)`,x=gdp.HIV$GDP, data=gdp.HIV, xlim = c(0,4500))


###SECTION 5
library(ggplot2)
pf<-read.csv('pseudo_facebook.tsv', sep='\t')
#relationship between two cont. variables.
#this creates a scatter plot.
qplot(x=age,y=friend_count,data=pf)
#identically
qplot(age,friend_count,data=pf)
#observations:
#peaks in the 0-30 range, several peaks ~65-70 and 100<

#now we switch to ggplot syntax, allowing more complicated plots.

ggplot(aes(x=age, y=friend_count),data=pf)+geom_point()+xlim(13,90)
#Computationally, is one of these faster than the other?

#geom=chart type. look at reference.
#aes wrapper - aesthetic wrapper. 
summary(pf$age)
#note we added the extra x lim layer since we want to cut the data down a little bit. 

#overplotting= makes it difficult to see how many points are in a certain area. 
#using the alpha proerty of geom_point() we can adjust the transparency. 1/20 means it takes 20 points to appear as one appoint. 
ggplot(aes(x=age, y=friend_count),data=pf)+
  geom_point(alpha=1/20)+
  xlim(13,90)

#now add jitter, which adds noise. 
ggplot(aes(x=age, y=friend_count),data=pf)+
  geom_jitter(alpha=1/20)+
  xlim(13,90)
#adding jitter makes sense since we were given that age as an integer, when in reality it ought to be a continous variable. 

#Now we add a transformation to the y axis, so that we may change the friend count scale.
ggplot(aes(x=age, y=friend_count),data=pf)+
  geom_point(alpha=1/20, position=position_jitter(h=0))+
  xlim(13,90)+
  coord_trans(y = "sqrt")
#Notice that I had to add the position_jitter(h=0), so that we do not get negative values. 
#Think of the friend count conditioned on age now. 

#Now explore the relationship between friends initiated vs age.range

ggplot(aes(x=age, y=friendships_initiated),data=pf)+
  geom_point(alpha=1/10, position=position_jitter(h=1))+
   xlim(13,90)


#conditional MEANS
#relationship between two or more variables.
install.packages('dplyr')
library(dplyr)
age_groups<-group_by(pf,age)
pf.fc_by_age<-summarise(age_groups, friend_count_mean=mean(friend_count), friend_count_median=median(friend_count),n=n())
#this yields a new data frame
pf.fc_by_age<-arrange(pf.fc_by_age,age)
#this arranges the frame by age in decending order(?)
head(pf.fc_by_age)
#this yields the first few rows

#AN ALTERNATE WAY TO GET THE SAME TABLE

#just use the original data set as follows

pf.fc_by_age<-pf %>%
  group_by(age) %>%
  summarise(friend_count_mean=mean(friend_count),
            friend_count_median=median(friend_count),
            n=n()) %>%
  arrange(age)
  
  #%>% is called the matching operator (?)
  #chains functions onto data set
  #the matching operator is part of the dplyr package
head(pf.fc_by_age)

#PROJECT: PLOT TABLE OF AVERAGES FC vs AGE
ggplot(aes(x=friend_count_mean,y=age),data=pf.fc_by_age)+geom_point()


ggplot(aes(y=friend_count_mean,x=age),data=pf.fc_by_age)+geom_line()


#USING GGPLOT TO SUMMARISE DATA
#First we compute the first scatter plot, then use the additional layer geom_line(summary, ....) to 
#
ggplot(aes(x=age,y=friend_count),data=pf)+
  xlim(13,90)+
  geom_point(alpha=0.05,position=position_jitter(h=0),
             color='orange')+
  coord_trans(y='sqrt')+
  geom_line(stat="summary",fun.y=mean)+
  geom_line(stat="summary", fun.y=quantile, fun.args=list(probs=0.1),color='blue',linetype=2)+
  geom_line(stat="summary", fun.y=quantile, fun.args=list(probs=0.5),color='blue')+
  geom_line(stat="summary", fun.y=quantile, fun.args=list(probs=0.9),color='blue',linetype=2)
  
#This plots show us that most of the younger users do NOT have more than 2000 friends.
#dashed lines (linetype=2) are the 10th, 50th, and 90th percentiles.

#Reanalyzing, if we remove the xlim and coodr_trans layers, and replace them with the coord_cart() function
#we can analyze 
ggplot(aes(x=age,y=friend_count),data=pf)+
  coord_cartesian(xlim=c(13,70), ylim=c(0,1000))+
  geom_point(alpha=0.05,position=position_jitter(h=0),
             color='orange')+
  geom_line(stat="summary",fun.y=mean,color='red')+
  geom_line(stat="summary", fun.y=quantile, fun.args=list(probs=0.1),color='blue',linetype=2)+
  geom_line(stat="summary", fun.y=quantile, fun.args=list(probs=0.5),color='blue',linetype=2)+
  geom_line(stat="summary", fun.y=quantile, fun.args=list(probs=0.9),color='blue',linetype=2)

#INTERPRETATIONS OF THIS PLOT 
# It is very rare to have more than 800 friends 


##CORRELATION##
cor.test(x=pf$age,y=pf$friend_count, method="pearson", conf.level = 0.95)
with(pf,cor.test(age,friend_count,method='pearson'))
#Notice that the corellation coefficent is very very small.

#Correlation coeff for subsets of the data so that we can only consider the range 13-70

with(subset(pf,age<68),cor.test(age,friend_count,method='pearson'))
#note cor.test uses pearson by default. 

#Measuring strength of increasing or decreasing, i.e. rank.

#CREATING SCATTER POINTS
ggplot(data = pf,aes(x = www_likes_received, y = likes_received)) +
  geom_point(alpha=1/20)+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$likes_received,0.95))+
  geom_smooth(method = 'lm', color="red")

#we can check to see how strong the relationship is 
with(pf,cor.test(www_likes_received,likes_received,method='pearson'))
qqplot(x=pf$www_likes_received,y=pf$likes_received)
#The distribution looks approx. normal, so the regression is valid. Also, we are appealing to the llnn.

#### 24.7.18

#Here is a new package
install.packages('alr3')
library(alr3)
data("Mitchell")

#WITH this data set create a new scatter plot 
ggplot(data=Mitchell, aes(x=Month,y=Temp))+geom_point()
#Notice this doesn't look like the two variables are even correlated
#this observation is coroborated by the following 
with(Mitchell,cor.test(Month,Temp,method='pearson'))

#we want to break up the months into 12 month intervals 
ggplot(data=Mitchell, aes(x=Month,y=Temp))+
  geom_point()+
  scale_x_continuous(breaks=seq(0,203,12))+
  geom_line()
#Notice that if we stretch the plot, then the plot reveals a sinusoidal pattern. 
#Is there any way to add a curve layer 


#NOISE
#Now we return to the original mean friend count data
#recall the following graph
p1<-ggplot(aes(y=friend_count_mean,x=age),data=subset(pf.fc_by_age,age<71))+
  geom_line()+
  geom_smooth()

head(pf.fc_by_age,10)
pf.fc_by_age[17:19,]
#conditional mean for age by months
pf$age_with_months<-pf$age+(12-pf$dob_month)/12
head(pf$age_with_months)
summary(pf$age_with_months)

#conditional mean for months 
#use dplyr again, chaining functions
pf.fc_by_age_months<-pf %>%
  group_by(age_with_months) %>%
  summarise(friend_count_mean=mean(friend_count),
            friend_count_median=median(friend_count),
            n=n()) %>%
  arrange(age_with_months)
head(pf.fc_by_age_months)
#Now we have age with months means, so that we can determine the conditional means 

#Now I will create a plot of the average friend count with ages measured in months.
#I will also subset the data so that the we only look at the ages less than 71

p2<-ggplot(aes(y=friend_count_mean,x=age_with_months),data=subset(pf.fc_by_age_months, age_with_months<71))+
  geom_line()+
  geom_smooth()
library(gridExtra)
grid.arrange(p1,p2,ncol=1)

#notice that when grouped by months there is a lot of noise in the graph. 
#we can filter this data even further. Consider the following filter

p3<-ggplot(aes(y=friend_count,x=round(age/5)*5),data=subset(pf, age<71))+
  geom_line(stat = 'summary',fun.y=mean)+
  geom_smooth()
grid.arrange(p1,p2,p3,ncol=1)

#bias variance trade off

#How do these plots communicate the finding s of this particular analysis? 