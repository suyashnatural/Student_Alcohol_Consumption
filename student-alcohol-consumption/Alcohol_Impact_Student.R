#######################################################################################
### Project - Impact of Alcohol Consumption on Students
### DataNiners - Suyash Nande, Abhishesk Deshpande, Baby Saranya Nakka, Himanshu Prihar
### Date - June 28, 2017
#######################################################################################

####################################################
### removing objects and free memory and get report
####################################################
rm(list = ls())
gc()

# installing various packages
install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("waffle")
install.packages("Hmisc")
install.packages("DMwR")
install.packages("rpart")
install.packages("fpc")
install.packages("rpart","rpart.plot")
install.packages("randomForest")
install.packages("C50")
install.packages("arules")
install.packages("arulesViz")
install.packages("gmodels")
install.packages("plotly")


library(ggplot2)      # for creating complex plot
library(plyr)         # for creating split-apply-combine procedures
library(dplyr)        # for data manipulation
library(gridExtra)    # for working with grid graphics
library(waffle)       # for square pie charts on categirical variables
library(Hmisc)        # for high level graphics
library(DMwR)         # Function in this package- abind, class, lattice, quantmod, ROCR, rpart, xts, zoo4 
library(rpart)        # implements  recursive partitioning
library(fpc)          # for clustering
library("rpart","rpart.plot") # for classification and regression trees
library(randomForest) # for random forest
library(C50)          # for C50 algorithm
library(arules)       # for apriori - association rules mining
library(arulesViz)    # for visualising association rules
library(gmodels)      # for model fitting

#########################################################
### reading and exploring the math class student dataset
#########################################################
maths_students<-read.csv("/Users/suyashnande/KDD/ KDD_Project/student-alcohol-consumption/student-mat.csv",stringsAsFactors = FALSE)
dim(maths_students)
names(maths_students)
str(maths_students)
attributes(maths_students)

###############################################################
### reading and exploring the portuguese class student dataset
###############################################################
portuguese_students<-read.csv("/Users/suyashnande/KDD/ KDD_Project/student-alcohol-consumption/student-por.csv",stringsAsFactors = FALSE)
dim(portuguese_students)
names(portuguese_students)
str(portuguese_students)
attributes(portuguese_students)

###############################################################
### merging the datasets and removing any duplicate values
###############################################################
students <- rbind(maths_students, portuguese_students)
View(students)
duplicated(students) # No duplicate values

###############################################################
### handling missing values in dataset, if any
### imputation of missing values
###############################################################
sapply(students, function(x) sum(is.na(x))) # No missing values

###############################################################
### Outliers Detection
###############################################################

# Outlier for Age variable
outlier_age <- boxplot.stats(students$age)$out
boxplot(students$age, main="Students Age", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_age, collapse=", ")), cex=0.6)
count(outlier_age)

x <- students$age
normalize <- (x-min(x))/(max(x)-min(x))
hist(students$age, breaks=10, xlab="Age", col="lightblue", main="")
hist(normalize, breaks=10, xlab="Normalized Age", col="lightblue", main="")

# Outlier for Absences variable
outlier_absences <- boxplot.stats(students$absences)$out
boxplot(students$absences, main="Students Absences", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_absences, collapse=", ")), cex=0.6)
count(outlier_absences)

###############################################################
### Patterns and trends - Frequency Distribution
###############################################################

# plots based on Gender
ggplot(students, aes(sex)) + geom_bar(aes(fill = as.factor(students$sex))) +
  scale_fill_discrete(name = "Sex" ,labels = c("Female " , "Male")) +
  labs(x= "Sex of Students",y= "Number of Students" , title = "Sex of Students")
# Observation - Here we can see that number of females are greater than number of males in our dataset.

# plots based on Address
ggplot(students, aes(address)) + geom_bar(aes(fill = as.factor(students$address))) +
  scale_fill_discrete(name = "Address", labels = c("Rural","Urban")) +
  labs(x = "Address" , y = "Number of Students" , title = "Address of Students")
# Observation - Here we can see that urban population is greater than rural population.


# plots based on Parents staying together or apart
ggplot(students, aes(famsize)) + geom_bar(aes(fill = as.factor(students$Pstatus))) + 
  scale_fill_discrete(name = "Family Cohabitation Status" , labels = c("Living Apart", "Living Together")) +
  labs(x = "Family Cohabitation Status" , y = "Number of Students" , title = "Family Cohabitation Status")
# Observation - Family living together are greater than family living apart.

# plots based on higher education
ggplot(students, aes(higher)) + geom_bar(aes(fill = as.factor(students$higher))) + 
  scale_fill_discrete(name = "Wants to Take Higher Education" , labels = c("No","Yes")) +
  labs(y = "Number of Students" , title = "Students who want to take Higher Education") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# Observation - Students wants to take higher education are higher.

# plots based on Internet
ggplot(students, aes(internet)) + geom_bar(aes(fill = as.factor(students$internet))) + scale_fill_discrete(name = "Internet Access" , labels = c("No","Yes")) +
  labs(x = "Internet Access" , y = "Number of Students" , title = "Students who have Internet Access") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# Observation - More number of students have internet connectivity.


# plots based on Workday alcohol
ggplot(students, aes(Dalc)) + geom_bar(aes(fill = as.factor(students$Dalc))) + 
  scale_fill_discrete(name = "Workday Alcohol Consumption", labels =c("Very Low", "Low", "Medium", "High", "Very High")) +
  labs(x = "Workday Alcohol Consumption" , y = "Number of Students" , title = "Graph of Student Workday Alcohol Consumption") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# Observation - During workday, maximum number of students consumes less alcohol.


# plots based on Weekend alcohol
ggplot(students, aes(Walc)) + geom_bar(aes(fill = as.factor(students$Walc))) + 
  scale_fill_discrete(name = "Weekend Alcohol Consumption", labels =c("Very Low", "Low", "Medium", "High", "Very High")) +
  labs(x = "Weekend Alcohol Consumption" , y = "Number of Students" , title = "Graph of Student Weekend Alcohol Consumption") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
# Observation - During weekend, maximum number of students consumes less alcohol.


# plots based on Absences
ggplot(students, aes(absences)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "Number of Absences",y= "Number of Students" , title = "Plot of Student Absences")
# Observation - Number of absences are low as number of students increases.


# Daily alcohol consumtption by age
ggplot(aes(x=age),data=students)+geom_histogram()+facet_wrap(~Dalc)
# Observation - The highest level of workday alcohol consumption is among 17 year olds


# Distribution of ages of students on weekday alcohol consumtion
ggplot(students, aes(x=(as.numeric(age))))+ geom_histogram(fill="blue", colour="black",binwidth=1) +
  facet_grid(Dalc ~ .)+ggtitle("Age and Workday Consumption")


# Distribution of ages of students on weekend alcohol consumtion
ggplot(students, aes(x=(as.numeric(age))))+ geom_histogram(fill="red", colour="black",binwidth=1) +
  facet_grid(Walc ~ .)+ggtitle("Age and Weekend Consumption")
# Observation - Patterns suggest high consumption of alcohol for any age group are less.

# Distribution of test score (G3) on male and female students
stu_by_fgrade <- students %>% select(sex,G3)
stu_count_by_fgrade<-ggplot(test, aes(x=G3)) + geom_histogram(fill="blue", colour="black",binwidth=1) +
  facet_grid(sex ~ .)+geom_vline(data=aggregate(stu_by_fgrade[2], stu_by_fgrade[1], median), 
                                 mapping=aes(xintercept=G3), color="red")
grid.arrange(stu_count_by_fgrade)
# Observation - Female students have greater mean of test score(G3) than male students.


#########################################################################
### Subsets
### Subset-1: Does high alcohol consumption affect the health condition?
#########################################################################

bwplot(students$Dalc~students$G3|students$school,ylab="Weekday alcohol", xlab="grades", 
       main="Weekday alcohol consumption levels and grades in period 3 by school",layout=c(1,3))
# The median grades become lower as the alcohol consumption increases

students$Dalc <- as.numeric(students$Dalc)
stu_by_dalc <- students[students$Dalc>=4, ]
nrow(stu_by_dalc)

nrow(filter(stu_by_dalc,health==1 |health==2))
# Only 8 out of 52 students consumes alcohol have bad health


#########################################################################
### Subsets
### Subset-2: How many Students with high alcohol consumption have good
###           family relation?
#########################################################################

family_sub<-subset(stu_by_dalc, famrel==4|famrel==5)
nrow(family_sub)
# 38 out of 52 students who have high alcohol consumption
# maintain good family relationship.


#########################################################################
### Subsets
### Subset-3: How many Students with high daily alcohol consumption 
###           also has high weekend alcohol consumption?
#########################################################################
stu_by_dalc$Walc <- as.numeric(stu_by_dalc$Walc)
stu_daily_weekly<-stu_by_dalc[stu_by_dalc$Walc>=4, ]
nrow(stu_daily_weekly)
# Here, we can observe that 42 out of 52 students who have high
# weekday consumption also has high weekend consumption levels.





###############################################################
### Pattern Prediction
### Pattern 1 - Daily alcohol consumption among students
### Pattern 2 - Weekend alcohol consumption among students
###############################################################
students$Dalc <- as.factor(students$Dalc)
students$Dalc <- mapvalues(students$Dalc, 
                           from = 1:5, 
                           to = c("Very Low", "Low", "Medium", "High", "Very High"))

students$Walc <- as.factor(students$Walc)      
students$Walc <- mapvalues(students$Walc, 
                           from = 1:5, 
                           to = c("Very Low", "Low", "Medium", "High", "Very High"))

alcohol.d <- as.data.frame(table(students$Dalc))
par.d <- as.numeric(alcohol.d$Freq)
names(par.d) <- alcohol.d$Var1
par.d <- round(par.d/10)


#waffle.col <- c("#ff4e50","#fc913a","#f9d62e","#00d27f","#adff00")
xyz<-rainbow(5)


c1 <- waffle(par.d, rows=5, 
             #use_glyph="glass", 
             size=2, 
             title = "Workday alcohol consumption among students",
             glyph_size=8,
             xlab="1 glass == 10 students",
             colors=xyz,
             legend_pos= "top"
)

alcohol.w <- as.data.frame(table(students$Walc))
par.w <- as.numeric(alcohol.w$Freq)
names(par.w) <- alcohol.w$Var1
par.w <- round(par.w/10)

c2 <- waffle(par.w, rows=5, 
             #use_glyph="glass", 
             size=2, 
             title = "Weekend alcohol consumption among students",
             glyph_size=8,
             xlab="1 glass == 10 students",
             colors=xyz,
             legend_pos= "top"
)


#workday
c5 <- ggplot(students, aes(x=Dalc, y=G1, fill=Dalc))+
  geom_point()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("First period grade")

c6 <- ggplot(students, aes(x=Dalc, y=G2, fill=Dalc))+
  geom_point()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Second period grade")

c7 <- ggplot(students, aes(x=Dalc, y=G3, fill=Dalc))+
  geom_point()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Final period grade")

grid.arrange(c5,c6,c7,ncol=3)


#weekend
c8 <- ggplot(students, aes(x=Walc, y=G1, fill=Walc))+
  geom_point()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("First period grade")

c9 <- ggplot(students, aes(x=Walc, y=G2, fill=Walc))+
  geom_point()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Second period grade")

c10 <- ggplot(students, aes(x=Walc, y=G3, fill=Walc))+
  geom_point()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Final period grade")

grid.arrange(c8,c9,c10,ncol=3)


######################################################################
### Pattern Prediction
### Pattern - Distribution of First, Second & Third Period Grades 
###           on Number of Students
######################################################################
# plots based on GRADES
graph1 <- ggplot(students, aes(G1)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x="First Period Grade", y = "Number of Students", title = "Plot of First Period Grades") +
  geom_vline(data=students, aes(xintercept=mean(students$G1),  colour= "mean" ),
             linetype="dashed", size=1)

graph2 <- ggplot(students, aes(G2)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x="Second Period Grade", y = "Number of Students", title = "Plot of Second Period Grades") +
  geom_vline(data=students, aes(xintercept=mean(students$G2),  colour= "mean" ),
             linetype="dashed", size=1)

graph3 <- ggplot(students, aes(G3)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x="Third Period Grade", y = "Number of Students", title = "Plot of Third Period Grades") +
  geom_vline(data=students, aes(xintercept=mean(students$G3),  colour= "mean" ),
             linetype="dashed", size=1)
grid.arrange(graph1,graph2,graph3, nrow=3)


######################################################################
### Pattern Prediction
### Pattern - Weekly alcohol consumption of males and females and
###           their effects on G1, G2 and G3 grades
######################################################################
#Daily and weekly alcohol consumption and Grade Scores.
graph4 <- ggplot(students, aes(x = Walc, y = G1 , color = sex)) + geom_boxplot
graph5 <- ggplot(students, aes(x = Walc, y = G2 , color = sex)) + geom_boxplot
graph6 <- ggplot(students, aes(x = Walc, y = G3 , color = sex)) + geom_boxplot
grid.arrange(graph4,graph5,graph6, nrow=3)

######################################################################
### Pattern Prediction
### Pattern - Daily alcohol consumption of males and females and
###           their effects on G1, G2 and G3 grades
######################################################################
graph7 <- ggplot(students, aes(x = Dalc, y = G1 , color = sex)) +  geom_point(color='darkblue')
graph8 <- ggplot(students, aes(x = Dalc, y = G2 , color = sex)) +  geom_point(color='darkblue')
graph9 <- ggplot(students, aes(x = Dalc, y = G3 , color = sex)) +  geom_point(color='darkblue')
grid.arrange(graph7,graph8,graph9, nrow=3)


# 2 sample t-test
absences_low<-subset(students, students$absences<=10)
absences_high<-subset(students, students$absences>10)

summary(absences_low$absences)
summary(absences_high$absences)

mean_low<-mean(absences_low$absences)
mean_high<-mean(absences_high$absences)

sd_low<-sd(absences_low$absences)
sd_high<-sd(absences_high$absences)

str(absences_low$absences)
str(absences_high$absences)

num1<-929
num2<-115

dfs<-min(num1-1,num2-1)
tdata <- (mean_high - mean_low) / sqrt((sd_low^2/num1)+(sd_high^2/num2))
pvalue <- 2*pt(tdata, df = dfs, lower.tail=FALSE)
tdata
pvalue
# Performing 2-sample t-test to test the hypothesis
# Let's consider the hypothesis as below:
# H0 : mean_low = mean_high ; H1 : mean_low != mean_high
# Here from tdata we get p value as 2.991679e-33 which is very low. Therefore we have extremely strong 
# evidence to reject our null hypothesis(H0)



#After combining both the datasets, now we will eliminate the repeats here
studentsnorepeats<-students %>% distinct(school,sex,age,address,famsize,Pstatus,
                                         Medu,Fedu,Mjob,Fjob,reason,
                                         guardian,traveltime,studytime,failures,
                                         schoolsup, famsup,activities,nursery,higher,internet,
                                         romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)

# Now add a column with average grades(Avergae of G1,G2,G3)
studentsnorepeats$avggrades=rowMeans(cbind(studentsnorepeats$G1,studentsnorepeats$G2,studentsnorepeats$G3))
View(studentsnorepeats)
# drop the individual grades in 3 marking periods.
studentsnorepeats<-studentsnorepeats[,-(31:33)]

#Boxplot of Avergae subject grades grouped by the levels of daily alcohol consumption
ggplot(studentsnorepeats, aes(x=Dalc, y=avggrades, group=Dalc))+
  geom_boxplot(fill='#A4A4A4', color="blue")+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Daily Alcohol consumption")+
  ylab("Average Grades")+
  ggtitle("Average Grade vs Dalc")

#Boxplot of Avergae subject grades grouped by the levels of weekend alcohol consumption
ggplot(studentsnorepeats, aes(x=Walc, y=avggrades, group=Walc))+
  geom_boxplot(fill='#A4A4A4', color="Darkred")+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Weekend Alcohol consumption")+
  ylab("Average Grades")+
  ggtitle("Average Grade vs Walc")

# We can observe the median average grade is higher among those students who had low levels of daily alcohol consumption
# But the  median grade of students with medium, high and very high levels of daily alcohol consumption very similar.
# Hence we will do a multiple linear regrssion and build a regression tree  of average grades on variables.

# Here we are removing the variable failures from the dataset as failures and average grades represent the same general student aptitude.
removefail<-which(names(studentsnorepeats)=="failures")
studentsnorepeats<-studentsnorepeats[,-removefail]
summary(studentsnorepeats)

# Now we wll perform multiple regression
# average grades with all other predictor variables
lm2<-lm(avggrades~., data=studentsnorepeats[,1:30])
summary(lm2)

# Here the adjusted r-squared value is 0.17 which is very low, which implies that only 17% of variation in the average grades
# is explained by everything else, so lets see whch other variables have a stronger impact on average grades


# Generating the Regression tree
library(rpart)
regtree<-rpart(avggrades~., data=studentsnorepeats[,1:30])
prettyTree(regtree)

# As per the above regression tree analysis , one variable which has significant impact is  "higher
# which  indicates whether the student wants to pursue higher education or no.
# The majority of students want to pursue higher education which is evident from theier average grade which is (11.4 out of 20)
# and this average is higher than the students average(8.47 out of 20) who do not want to pursue higher education
# the regression tree analysis also specfies that moder's education is another feature which can have impact on average gardes as well

# Date - June 18, 2017
# Now lets evaluate the predictive perfomance of two models , we are normalizing the mean squared error of two models

lreg.predictions<-predict(lm2,studentsnorepeats)
rtree.predictions<-predict(regtree,studentsnorepeats)

nmse.lm<-mean((lreg.predictions-studentsnorepeats[,"avggrades"])^2)/mean((mean(studentsnorepeats$avggrades)-studentsnorepeats[,"avggrades"])^2)
nmse.rt<-mean((rtree.predictions-studentsnorepeats[,"avggrades"])^2)/mean((mean(studentsnorepeats$avggrades)-studentsnorepeats[,"avggrades"])^2)

print(nmse.lm) #0.7984877
print(nmse.rt) #0.849412

# We can observe that the linear regression model performs better than the regression tree as the
# standard error for linear model(0.76) is less than the regression tree(0.81)


# We now show the model which depicts the error scatter plots

lregplotdata=data.frame(cbind(lreg.predictions,studentsnorepeats[,"avggrades"]))
colnames(lregplotdata)<-c("lm.predictions","avggrades")
rtreeplotdata=data.frame(cbind(rtree.predictions,studentsnorepeats[,"avggrades"]))
colnames(rtreeplotdata)<-c("rtree.predictions","avggrades")

studentsnorepeats$Dalc<-as.factor(studentsnorepeats$Walc)

errorplot.lreg = ggplot(lregplotdata,aes(lreg.predictions,avggrades))+
  geom_point(aes(color=as.factor(studentsnorepeats[,"Walc"])))+
  xlab("Predicted Grades (Linear Model)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  scale_colour_brewer(palette = "Set1",name = "Weekend Alcohol \nConsumption")

errorplot.rtree = ggplot(rtreeplotdata,aes(rtree.predictions,avggrades))+
  geom_point(aes(color=as.factor(studentsnorepeats[,"Walc"])))+
  xlab("Predicted Grades (Regression Tree)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  scale_colour_brewer(palette = "Set1",name = "Weekend Alcohol \nConsumption")

grid.arrange(errorplot.rtree)
grid.arrange(errorplot.lreg,errorplot.rtree,nrow=2)

# Observation : In the above graphs, horizontal axes represents predicted grades and vertical axes represet actual grades
# For the model to be accurate , the predicted grades should be equal to actual grades and the scatter points 
# should be inline along the 45 degree line(blue line).
# But, that not the case here and both the models do not help in accurately predicting student average grades
# so, we now process to use randon forest and find out if it helps in accurately predition.


#Random forest 
library(randomForest)
set.seed(4543)
View(studentsnorepeats)
is.finite(studentsnorepeats$avggrades)
is.infinite(studentsnorepeats$avggrades)

studentsnorepeats$school <- as.factor(studentsnorepeats$school)
studentsnorepeats$sex <- as.factor(studentsnorepeats$sex)
studentsnorepeats$address <- as.factor(studentsnorepeats$address)
studentsnorepeats$famsize <- as.factor(studentsnorepeats$famsize)
studentsnorepeats$Pstatus <- as.factor(studentsnorepeats$Pstatus)
studentsnorepeats$Mjob <- as.factor(studentsnorepeats$Mjob)
studentsnorepeats$Fjob <- as.factor(studentsnorepeats$Fjob)
studentsnorepeats$reason <- as.factor(studentsnorepeats$reason)
studentsnorepeats$guardian <- as.factor(studentsnorepeats$guardian)
studentsnorepeats$schoolsup <- as.factor(studentsnorepeats$schoolsup)
studentsnorepeats$famsup <- as.factor(studentsnorepeats$famsup)
studentsnorepeats$paid <- as.factor(studentsnorepeats$paid)

studentsnorepeats$activities <- as.factor(studentsnorepeats$activities)
studentsnorepeats$nursery <- as.factor(studentsnorepeats$nursery)
studentsnorepeats$higher <- as.factor(studentsnorepeats$higher)
studentsnorepeats$internet <- as.factor(studentsnorepeats$internet)
studentsnorepeats$romantic <- as.factor(studentsnorepeats$romantic)
studentsnorepeats$paid <- as.factor(studentsnorepeats$paid)


randfor<-randomForest(avggrades~., data=studentsnorepeats[,1:30], ntree=500, importance=T)
rf.predictions<-predict(randfor,studentsnorepeats)


# normalised mean standard error
nmse.rf<-mean((rf.predictions-studentsnorepeats[,"avggrades"])^2)/mean((mean(studentsnorepeats$avggrades)-studentsnorepeats[,"avggrades"])^2)
print(nmse.rf)
# 0.2016583

# Observation : We can see that the normalized mean standard error of the random forest is 0.2016583 which is
# very much lower than the linear and regression tree models.

# To validate this, lets plot the error plot of the random forest to compare it with the error plots of linear 
# and regression models 


# first combine the rf predictions and actual scores in a single data frame
rfplot=data.frame(cbind(rf.predictions,studentsnorepeats[,"avggrades"]))
colnames(rfplot)<-c("rf.predictions","avggrades")

# then create the error plot.
errplot.rf1<-ggplot(rfplot,aes(rf.predictions,avggrades))+
  geom_point(aes(color=as.factor(studentsnorepeats[,"Walc"])))+
  xlab("Predicted Grades (Random Forest with 500 Trees)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  scale_colour_brewer(palette = "Set1",name = "Weekend Alcohol \nConsumption")

# finally, plot the error plot from the random forest with the error plots of the linear and regression tree models.
grid.arrange(errplot.rf1, errorplot.lreg,errorplot.rtree,nrow=3)


# Observation : From the grapgh of error plots, we can say that random forest seems to be much better predictor
# of average grades than the linear or regression model.


# Lets plot the graph to check which varibales most impact the student grades as measured by random forest with 500 trees
varImpPlot(randfor,type=1, main="Highest Impact Variable")




######################################################################################
### Pattern Prediction
### Pattern - Mother's education and Average grades 
### Description - Here x- axis represents the avereage grades , y-axis the count, 
### and on the right side the number 0,1,2,3,4 represents the level of education
######################################################################################
ggplot(studentsnorepeats, aes(x=avggrades)) + geom_histogram(fill="yellow", colour="red", binwidth = 2) +
  facet_grid(Medu ~ .)+ggtitle("Mother's Education and Average Grades")+
  geom_vline(data=aggregate(studentsnorepeats[30], studentsnorepeats[7], median), mapping=aes(xintercept=avggrades), color="black")



######################################################################################
### Pattern Prediction
### Pattern - Father's education and Average grades
### Description - Here x- axis represents the avereage grades , y-axis the count, 
### and on the right side the number 0,1,2,3,4 represents the level of education
######################################################################################
ggplot(studentsnorepeats, aes(x=avggrades)) + geom_histogram(fill="yellow", colour="red",binwidth = 2) +
  facet_grid(Fedu ~ .)+ggtitle("Father's education and Final Grades")+
  geom_vline(data=aggregate(studentsnorepeats[30], studentsnorepeats[8], median), mapping=aes(xintercept=avggrades), color="black")


# Observation : As the mother's and father's level of education increases, the median avergae grade also increases.
# We can conculde that mother's and father's education level is certainly a factor for scoring good grades


# Lets see if having romantic relationship will affect average grades

studentsnorepeats %>% group_by(romantic) %>% summarise(n=mean(avggrades,na.rm=T)) %>%
  ggplot(aes(x=romantic,y=n))+geom_bar(stat="identity")


# As per the graph, the mean final grade (average grade) for those who were not in  
# a relationship is greatehr than those who were.


# Let's see how are these scores distributed
ggplot(studentsnorepeats, aes(x=avggrades, fill=romantic)) +
  geom_histogram(position="identity", alpha=0.4,binwidth=1.0)

# Observation : We can see from the distribution that Students who were not in relationships scored 
#higher grades more frequently than those students who were in relationships.

# Now we will check if students involved in extra curricular activities are  more likely to be in a romantic relationship?

romact <- studentsnorepeats %>% group_by(romantic,activities) %>% summarise(n=n()) 
test123 <- ddply(romact, "activities", transform,
                 percent_weight = n / sum(n) * 100)
ggplot(test123, aes(x=activities, y=percent_weight, fill=romantic)) +
  geom_bar(stat="identity")+geom_text(aes(label=percent_weight), vjust=1.5, colour="white",
                                      position=position_dodge(.9), size=3)+ggtitle("Activities and Romantic Relationships")


# So we can conclude that if students are being involved in extra curricular atvitites, it increases the
# chances of being in a relationship  by around 4.5%.

#=========================================================================
# Data Transformation
# We have many variables which are numerical whcih are stored as categorical, 
# so lets transfrom those variables
# so that the numerical variables are'nt treated as continous variables'
#=========================================================================

transformdata = students  %>% 
  mutate(Medu = ordered(Medu, levels = c(0:4), labels = c("No Ed?","Primary Ed", "5th-9th Grade", "Secondary Ed", "Higher Ed")),
         Fedu = ordered(Fedu, levels = c(0:4), labels = c("No Ed?", "Primary Ed", "5th-9th Grade", "Secondary Ed", "Higher Ed")),
         traveltime = ordered(traveltime, levels = c(1:4), labels = c("<15 min", "15-30 min", "30 min-1 hr", ">1 hr")),
         studytime = ordered(studytime, levels = c(1:4), labels = c("<2 hrs","2-5 hrs","5-10 hrs","10 hrs"))) %>% 
  mutate_at(.vars = vars(reason, school, sex, address, famsize, Pstatus, Mjob, Fjob, 
                         reason,guardian,schoolsup:health), .funs = as.factor)

View(transformdata)
###### Z-score standardization for absences
students$absences <- (students$absences-mean(students$absences))/sd(students$absences)
students$absences
str(students)

absences_outliers <- subset(students, students$absences > 3 & students$absences < -3)
nrow(absences_outliers)
#Number of Outliers after standardization : 0

# Z-score standardization for age
students$age <- (students$age-mean(students$age))/sd(students$age)
students$age
str(students)

age_outliers <- subset(students, students$age > 3 & students$age < -3)
nrow(age_outliers)
#Number of Outliers after standardization : 0


#### Min Max Normalization for absences
summary(students$absences)

class(students$absences)
x <- students$absences
normalize = (x-min(x))/(max(x)-min(x))

hist(students$absences, breaks=10, xlab="Absences", col="lightblue", main="")
hist(normalize, breaks=10, xlab="Normalized Absences", col="lightblue", main="")


#### Min Max Normalization for age
summary(students$age)

class(students$age)
x <- students$age
normalize = (x-min(x))/(max(x)-min(x))

hist(students$age, breaks=10, xlab="Age", col="lightblue", main="")
hist(normalize, breaks=10, xlab="Normalized Age", col="lightblue", main="")



#=========================================================================
# Binning using k-means clustering
# create a new variable to hold the category of the record(HIGH,MEDIUM,LOW)
#=========================================================================
library(cluster)
View(studentsnorepeats)

stuclusters <- kmeans(studentsnorepeats$avggrades, centers = 2)
whichbin <- stuclusters$cluster;

bin1<- rownames(studentsnorepeats[ stuclusters$cluster==1,])
bin2 <- rownames(studentsnorepeats[ stuclusters$cluster==2,])

bin1meantemp <- mean(studentsnorepeats$avggrades[which(rownames(studentsnorepeats) %in% bin1)])
bin2meantemp <- mean(studentsnorepeats$avggrades[which(rownames(studentsnorepeats) %in% bin2)])

avgGradeCat <- c("-1", "-1")
maxMean <- max(bin1meantemp,bin2meantemp)
minMean <- min(bin1meantemp,bin2meantemp)

if(bin1meantemp == maxMean){
  avgGradeCat[1]="HIGH"
}else{
    avgGradeCat[1]="LOW"
  }

if(bin2meantemp == maxMean){
  avgGradeCat[2]="HIGH"
}else{
    avgGradeCat[2]="LOW"
  }


stuclusters

for(i in 1:nrow(studentsnorepeats)){
  if(whichbin[i]==1){
    studentsnorepeats$avgGradeCat[i] <- avgGradeCat[1]
  }
  else if(whichbin[i]==2){
    studentsnorepeats$avgGradeCat[i] <- avgGradeCat[2]
  }
}

library(fpc)
plotcluster(studentsnorepeats$avggrades,stuclusters$cluster)




#Binning for Dalc
studentsnorepeats$Dalc <- as.numeric(studentsnorepeats$Dalc)
stuclustersDalc <- kmeans(studentsnorepeats$Dalc, centers = 2)
whichbinDalc <- stuclustersDalc$cluster;

bin1D <- rownames(studentsnorepeats[ stuclustersDalc$cluster==1,])
bin2D <- rownames(studentsnorepeats[ stuclustersDalc$cluster==2,])

bin1Dmean <- mean(studentsnorepeats$Dalc[which(rownames(studentsnorepeats) %in% bin1D)])
bin2Dmean <- mean(studentsnorepeats$Dalc[which(rownames(studentsnorepeats) %in% bin2D)])

DalcCat <- c("-1", "-1")
maxMean <- max(bin1Dmean,bin2Dmean)
minMean <- min(bin1Dmean,bin2Dmean)

if(bin1Dmean == maxMean){
  DalcCat[1]="HIGH"
}else{
    DalcCat[1]="LOW"
  }

if(bin2Dmean == maxMean){
  DalcCat[2]="HIGH"
}else{
    DalcCat[2]="LOW"
  }


stuclustersDalc

for(i in 1:nrow(studentsnorepeats)){
  if(whichbinDalc[i]==1){
    studentsnorepeats$DalcCat[i] <- DalcCat[1]
  }
  else if(whichbinDalc[i]==2){
    studentsnorepeats$DalcCat[i] <- DalcCat[2]
  }

}

library(fpc)
plotcluster(studentsnorepeats$Dalc,stuclustersDalc$cluster)


####
#Binning for Walc
studentsnorepeats$Walc <- as.numeric(studentsnorepeats$Walc)
stuclustersWalc <- kmeans(studentsnorepeats$Walc, centers = 2)
whichbinWalc <- stuclustersWalc$cluster;

bin1W <- rownames(studentsnorepeats[ stuclustersWalc$cluster==1,])
bin2W <- rownames(studentsnorepeats[ stuclustersWalc$cluster==2,])

View(studentsnorepeats)

bin1Wmean <- mean(studentsnorepeats$Walc[which(rownames(studentsnorepeats) %in% bin1W)])
bin2Wmean <- mean(studentsnorepeats$Walc[which(rownames(studentsnorepeats) %in% bin2W)])

WalcCat <- c("-1", "-1")
maxMean <- max(bin1Wmean,bin2Wmean)
minMean <- min(bin1Wmean,bin2Wmean)

if(bin1Wmean == maxMean){
  WalcCat[1]="HIGH"
}else{
    WalcCat[1]="LOW"
  }

if(bin2Wmean == maxMean){
  WalcCat[2]="HIGH"
}else{
    WalcCat[2]="LOW"
  }


stuclustersWalc

for(i in 1:nrow(studentsnorepeats)){
  if(whichbinWalc[i]==1){
    studentsnorepeats$WalcCat[i] <- WalcCat[1]
  }
  else if(whichbinWalc[i]==2){
    studentsnorepeats$WalcCat[i] <- WalcCat[2]
  }
}

library(fpc)
plotcluster(studentsnorepeats$Walc,stuclustersWalc$cluster)


#=========================================================================
# EDA of all the variables
# based on the risk of alcohol consumption
#=========================================================================

# copying the dataset into new dataset
students11 <- studentsnorepeats

# typical x-axis label
ns <- list(title = "Number of Students")
# typical x-axis label
risk <- list(title = "Risk")

students11$Fjob <- factor(students11$Fjob, levels = c("at_home","health","other","services","teacher"), 
                          labels = c("At_Home", "Health","Other","Services","Teacher"))

m <- list(pad = 5,
          t = 80,
          b = 80)
View(students11)
names(students11)[26] <- "Weekday"
names(students11)[27] <- "Weekend"
# Create a new features for the sum of students' drinking habits
students11$Total <- as.numeric(students11$Weekend) + as.numeric(students11$Weekday)

# Create the drinking risk levels for students
students11$Risk <- "Low"
students11$Risk[students11$Total > 5 | students11$Weekend >= 4 | students11$Weekday >= 4] <- "High"
students11$Risk <- factor(students11$Risk, levels = c("Low","High"))

#Length of the dataframe
len_df <- dim(students11)[1]

library(plotly)
plot_ly(students11, x = ~Weekday, type = "histogram", 
        marker = list(line = list(color = 'white', width = 1))) %>%
  layout(title = "Alcohol Consumption during the Week",
         yaxis = ns,
         xaxis = list(title = "Amount of Alcohol"),
         margin = m)
print("Percent of students in each drinking level:")
table(students11$Weekday) / len_df
# we can observe from above that just  about more than 5% of students drink more amount of alchohol(i.e >4) 
# during the week

plot_ly(students11, x = ~Weekend, type = "histogram", 
        marker = list(line = list(color = 'white', width = 1))) %>%
  layout(title = "Alcohol Consumption on Weekends",
         yaxis = ns,
         xaxis = list(title = "Amount of Alcohol"),
         margin = m)
print("Percent of students in each drinking level:")
table(students11$Weekend) / len_df

#For weekends, we can observe that more number of students consume alcohol when compared to weekdays.
#On weekends, the consumption in % is just about 20% whereas in weekdays it is 5%.
#Also those drinking very less alcohol has been decreased in weekends (356 students) when compared to weekdays(663)

#Total consumtion of alcohol
plot_ly(students11, x = ~Total, type = "histogram", 
        marker = list(line = list(color = 'white', width = 1))) %>%
  layout(title = "Total Alcohol Consumption (Weekday + Weekend)",
         xaxis = list(title = "Amount of Alcohol"),
         yaxis = ns,
         margin = m)
print("Percent of students in each group:")
table(students11$Total) / len_df

#Based on gender
students11$sex <- factor(students11$sex, levels = c("F","M"), 
                         labels = c("Female", "Male"))


g1 <- plot_ly(students11, x=~sex, color = ~Risk, type = "histogram") %>%
  layout(barmode = "stack",
         xaxis = list(title = "Gender"))
ab <- data.frame(prop.table(table(students11$Risk, students11$sex),1))
g2 <-plot_ly(ab, x=ab$Var1, y =~Freq, color = ab$Var2, type = "bar") %>%
  layout(barmode = "stack",
         xaxis = risk)
subplot(g1,g2, margin = 0.04, titleX = TRUE) %>%
  layout(title = "Sex vs Risk",
         yaxis = ns,
         margin = m)
print("Number of students of each gender:")
table(students11$sex)
table(students11$sex)/len_df

#Despite 56% of the students being female, 70% of those in the high risk group are males.

#Pattern by age
a1 <- plot_ly(students11, x=~age, color = ~Risk, type = "histogram",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = list(title = "Age"),
         margin = m)
abc <- data.frame(prop.table(table(students11$Risk, students11$age),1))
a2 <-plot_ly(abc, x=abc$Var1, y =~Freq, color = abc$Var2, type = "bar",
             marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = risk,
         margin = m)
subplot(a1,a2, margin = 0.04, titleX = TRUE) %>%
  layout(title = "Age vs Risk")
print("Number of students of each age:")
table(students11$age)
table(students11$age) / len_df
print("Correlation between age and risk")
cor(students11$age, as.numeric(students11$Risk))

# Simplify age to 2 groups, >= 19, < 19.
# You will notice that more features like this one will be created. This goal for these
# features is to simplify the data to improve predictions.
students11$ageSimple <- 0
students11$ageSimple[students11$age >= 19] <- 1
#We can see that as age increases, students tend to consume more alcohol
#Now lets see if the parents marital status has any affect on 
# alcohol consumption as we think it will definitely affect studnets
students11$Pstatus <- factor(students11$Pstatus, levels = c("A","T"), 
                             labels = c("Apart", "Together"))

m1 <- plot_ly(students11, x=~Pstatus, color = ~Risk, type = "histogram",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = list(title = "Parents' Marriage Status"))
abcd <- data.frame(prop.table(table(students11$Risk, students11$Pstatus),1))
m2 <-plot_ly(abcd, x=abcd$Var1, y =~Freq, color = abcd$Var2, type = "bar",
             marker = list(line = list(color = "white", width = 1)),
             showlegend = FALSE) %>%
  layout(barmode = "stack",
         xaxis = risk)
subplot(m1,m2, margin = 0.04, titleX = TRUE) %>%
  layout(title = "Parents' Marriage Status vs Risk",
         yaxis = ns,
         margin = m)
print("Number of students of each parental marriage status group:")
table(students11$Pstatus)
table(students11$Pstatus)/len_df

#But form the above observations its clear that there is no effect of parents martial status on students alcohol consumption

#Let's see if father's job has nay affect on students alcohol consumption
students11$Fjob <- studentsnorepeats$Fjob
students11$Fjob <- factor(students11$Fjob, levels = c("at_home","health","other","services","teacher"), 
                          labels = c("At_Home", "Health","Other","Services","Teacher"))

View(students11)
f1 <- plot_ly(students11, x=~Fjob, color = ~Risk, type = "histogram",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = list(title = "Type of Father's job"))

xy <- data.frame(prop.table(table(students11$Risk, students11$Fjob),1))
f2 <-plot_ly(xy, x=xy$Var1, y =~Freq, color = xy$Var2, type = "bar",
             marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = risk)
subplot(f1,f2, margin = 0.04, titleX = TRUE) %>%
  layout(title = "Father's Job Type vs Risk",
         yaxis = ns,
         margin = m)
print("Number of students for each type of job (Father):")
table(students11$Fjob)
table(students11$Fjob)/len_df

students11$FjobSimple <- 0 
students11$FjobSimple[df$Fjob == 'Services'] <- 1

#Observation : We can say that if father works in service, then their child is more likely to abuse alcohol.

#Lets see whether students who recieve educational support are abusive to alcohol
students11$famsup <- factor(students11$famsup, levels = c("no","yes"), labels = c("No", "Yes"))

fs <- plot_ly(students11, x=~famsup, color = ~Risk, type = "histogram",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = list(title = "Educational Support from Family"))
fps <- data.frame(prop.table(table(students11$Risk, students11$famsup),1))
fs1 <-plot_ly(fps, x=fps$Var1, y =~Freq, color = fps$Var2, type = "bar",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = risk)
subplot(fs,fs1, margin = 0.04, titleX = TRUE) %>%
  layout(title = "Educational Support From Family vs Risk",
         yaxis = ns,
         margin = m)
print("Number of students for each educational support group:")
table(students11$famsup)
table(students11$famsup)/len_df
by(as.numeric(students11$famsup), students11$Risk, summary)

# So, we can say that students who do not recieve educational support 
# are very much likely to be abussive to alcohol

####
#Lets see if students who go out with friends have a tendency to drink more alcohol
students11$goout <- factor(students11$goout, levels = c("1","2","3","4","5"),
                           labels = c("Very Low","Low","Average","High","Very High"))


go <- plot_ly(students11, x=~goout, color = ~Risk, type = "histogram",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = list(title = "Frequency of Going Out with Friends"))
g <- data.frame(prop.table(table(students11$Risk, students11$goout),1))
go1 <-plot_ly(g, x=g$Var1, y =~Freq, color = g$Var2, type = "bar",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = risk)
subplot(go,go1, margin = 0.04, titleX = TRUE) %>%
  layout(title = "Frequency of Going Out with Friends vs Risk",
         yaxis = ns,
         margin = m)
print("Number of students for each social group:")
table(students11$goout)
table(students11$goout)/len_df


ggplotly(ggplot(aes(Risk, as.numeric(goout)), data = students11) +
           geom_violin(alpha = 0.3, color = I("Red"), fill = I("Red")) +
           geom_boxplot(alpha = 0.8, color = I("black")) +
           geom_point(position = position_jitter(width = 0.5, height = 0.1), 
                      color = I("blue"),
                      alpha = 0.4) +
           ylab("Frequency of Going Out with Friends") +
           ggtitle("Frequency of Going Out with Friends vs Risk")) %>%
  layout(margin = list(t = 80,
                       b = 80,
                       pad = 5,
                       l = 60))
by(as.numeric(students11$goout), students11$Risk, summary)
print("Correlation between frequency of going out with friends and risk")
cor(as.numeric(students11$goout), as.numeric(students11$Risk))

students11$gooutSimple <- 0
students11$gooutSimple[as.numeric(students11$goout) == 4] <- 1
students11$gooutSimple[as.numeric(students11$goout) == 5] <- 2

#This could be the most differentiating feature that we have seen yet. 
# We can clearly see that students who go out with their friends more often 
# are more likely to be in the high risk group.

##
#Lets see if student's study time has any affect on alcohol consumption
students11$studytime <- studentsnorepeats$studytime
View(students11)
students11$studytime <- factor(students11$studytime, levels = c("1","2","3","4"), 
                               labels = c("Less than 2 Hours", "2 to 5 Hours","5 to 10 Hours",
                                          "More than 10 Hours"))


st <- plot_ly(students11, x= ~studytime, color = ~Risk, type = "histogram",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = list(title = "Time Spent Studying (Weekly)"))
st
stime <- data.frame(prop.table(table(students11$Risk, students11$studytime),1))
st1 <-plot_ly(stime, x=stime$Var1, y =~Freq, color = stime$Var2, type = "bar",
              marker = list(line = list(color = "white", width = 1))) %>%
  layout(barmode = "stack",
         xaxis = risk)
subplot(st,st1, margin = 0.04, titleX = TRUE) %>%
  layout(title = "Time Spent Studying vs Risk",
         yaxis = ns,
         margin = m)
print("Number of students for each study time group:")
table(students11$studytime)
table(students11$studytime)/len_df

ggplotly(ggplot(aes(Risk, as.numeric(studytime)), data = students11) +
           geom_violin(alpha = 0.3, color = I("Red"), fill = I("Red")) +
           geom_boxplot(alpha = 0.5, color = I("black")) +
           geom_point(position = position_jitter(width = 0.5, height = 0.1), 
                      color = I("blue"),
                      alpha = 0.4) +
           ylab("Time Spent Studying (Groupings)") +
           ggtitle("Time Spent Studying vs Risk")) %>%
  layout(margin = list(pad = 0,
                       l = 60))
by(as.numeric(students11$studytime), students11$Risk, summary)
print("Correlation between time spent studying and risk:")
cor(as.numeric(students11$studytime), as.numeric(students11$Risk))

students11$studytimeSimple <- 0
students11$studytimeSimple[as.numeric(students11$studytime) == 1] <- 1

# There is a reasonable relationship here. 
# If a student spends less time studying, s/he is more likely to abuse alcohol. 
# Note: The values for 'Time Spent Studying' (1,2,3,4) represent 
# 'Less than 2 Hours', '2 to 5 Hours', '5 to 10 Hours', and 'More than 10 Hours'.


#Multivariate analysis
ggplot(aes(as.numeric(goout), as.numeric(studytime), color = Risk), data = students11) +
  geom_jitter() +
  geom_smooth(se = FALSE, method = 'lm') +
  ggtitle("Going Out vs Studying Time vs Risk") +
  xlab("Frequency of Going Out with Friends") +
  ylab("Time Spent Studying")
students11$gooutStudy <- 0
students11$gooutStudy[as.numeric(students11$goout) == 5 & as.numeric(students11$studytime) == 1] <- 1

# We can observe from the pattern that studnets who go out more frequentlt and study less consume more alcohol
# as seen in the bottom right corener of the graph



#=========================================================================
# Modeling and Evaluation
#=========================================================================
library(rpart)
library(rpart.plot)
set.seed(12345)
data_random <- studentsnorepeats[order(runif(958)), ] #creating  a new dataframe 
# where rows are copies of the original dataframe but selected on a random generation of 1000 number

View(data_random)

summary(studentsnorepeats$avggrades)
summary(data_random$avggrades) # we check that we get the same data in both dataframes...

head(studentsnorepeats$avggrades)
head(data_random$avggrades) # we check the order of both dataframes are different !

# splitting the dataset
data_train <- data_random[1:700, ]
data_test  <- data_random[701:959, ]

# check the data sets  
prop.table(table(data_train$avgGradeCat))
prop.table(table(data_test$avgGradeCat))

data_random$freetime
data_random$goout

#=========================================================================
# Modeling with CART
#=========================================================================
data_model_cart <- rpart(avgGradeCat ~.,
                         method="class", data = data_train, control = rpart.control(cp = 0.0001))

printcp(data_model_cart) # display the results 
plotcp(data_model_cart) # visualize cross-validation results 
summary(data_model_cart) # detailed summary of splits

# plot tree 
plot(data_model_cart, uniform=TRUE, 
     main="Classification Tree for Alcohol Consmuption")
text(data_model_cart, use.n=TRUE, all=TRUE, cex=.8)

library("rpart.plot")
rpart.plot(data_model_cart)
str(data_random)

#=========================================================================
# Modeling with C5.0
#=========================================================================

# Testing the model using test data set
data_predict_cart <- predict(data_model_cart,data_test)

# Training the model
x<-data_train[,c(1:30)]
y<-data_train$avgGradeCat
y<-as.factor(y)
library(C50)
data_model_c50 <- C5.0(x,y)
data_model_c50
summary(data_model_c50)

#==========================================================================
# Validating the accuracy of the model using cross table for propotions
#==========================================================================
library(gmodels)
data_predict_c50 <- predict(data_model_c50,data_test)
CrossTable(data_test$avgGradeCat, data_predict_c50,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted '))

data_train %>% select(-avggrades) -> data_train
View(data_train)
data_model_c50_01<- C5.0(data_train[,c(1:30)],as.factor(data_train$avgGradeCat))
summary(data_model_c50_01)

# predicting again after removing avggrades from the dataset
data_predict_c50_01<- predict(data_model_c50_01,data_test)
CrossTable(data_test$avgGradeCat, data_predict_c50_01,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted '))


#=========================================================================
# Association - Finding Rules
#=========================================================================
dataset_for_association <- data_random # new dataset
View(dataset_for_association) # display new dataset

# Converting all the attributes into factor
dataset_for_association$school <- as.factor(dataset_for_association$school)
dataset_for_association$sex <- as.factor(dataset_for_association$sex)
dataset_for_association$address <- as.factor(dataset_for_association$address)
dataset_for_association$famsize <- as.factor(dataset_for_association$famsize)
dataset_for_association$Pstatus <- as.factor(dataset_for_association$Pstatus)
dataset_for_association$Mjob <- as.factor(dataset_for_association$Mjob)
dataset_for_association$Fjob <- as.factor(dataset_for_association$Fjob)
dataset_for_association$reason <- as.factor(dataset_for_association$reason)
dataset_for_association$guardian <- as.factor(dataset_for_association$guardian)
dataset_for_association$schoolsup <- as.factor(dataset_for_association$schoolsup)
dataset_for_association$famsup <- as.factor(dataset_for_association$famsup)
dataset_for_association$paid <- as.factor(dataset_for_association$paid)

dataset_for_association$activities <- as.factor(dataset_for_association$activities)
dataset_for_association$nursery <- as.factor(dataset_for_association$nursery)
dataset_for_association$higher <- as.factor(dataset_for_association$higher)
dataset_for_association$internet <- as.factor(dataset_for_association$internet)
dataset_for_association$romantic <- as.factor(dataset_for_association$romantic)
dataset_for_association$paid <- as.factor(dataset_for_association$paid)

dataset_for_association$age <- as.factor(dataset_for_association$age)
dataset_for_association$Medu <- as.factor(dataset_for_association$Medu)
dataset_for_association$Fedu <- as.factor(dataset_for_association$Fedu)
dataset_for_association$traveltime <- as.factor(dataset_for_association$traveltime)
dataset_for_association$studytime <- as.factor(dataset_for_association$studytime)
dataset_for_association$famrel <- as.factor(dataset_for_association$famrel)
dataset_for_association$freetime <- as.factor(dataset_for_association$freetime)
dataset_for_association$goout <- as.factor(dataset_for_association$goout)
dataset_for_association$Dalc <- as.factor(dataset_for_association$Dalc)
dataset_for_association$Walc <- as.factor(dataset_for_association$Walc)
dataset_for_association$health <- as.factor(dataset_for_association$health)
dataset_for_association$absences <- as.factor(dataset_for_association$absences)
dataset_for_association$avggrades <- as.factor(dataset_for_association$avggrades)
dataset_for_association$WalcCat <- as.factor(dataset_for_association$WalcCat)
dataset_for_association$DalcCat <- as.factor(dataset_for_association$DalcCat)
dataset_for_association$avgGradeCat <- as.factor(dataset_for_association$avgGradeCat)

# view structure of the dataset
str(dataset_for_association)

# removing unwanted attributes from the dataset
dataset_for_association %>% select(-sex, -school, -Dalc, -Walc) -> dataset_for_association
View(dataset_for_association)

# using apriori algorithm to find out association rules for whole dataset
rules <- apriori(dataset_for_association) 
inspect(rules)

# rules with rhs containing "avgGrades" only (using apriori with 0.5 confidence and 0.005 support)
rules <- apriori(dataset_for_association, 
                 parameter = list(sup = 0.005 , conf = 0.5, target="rules"), 
                 appearance = list(rhs = c("avgGradeCat=LOW", "avgGradeCat=HIGH"),
                                   default = "lhs"),
                 control = list(verbose=F))

# sorting the rules by lift-factor
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# visaulizing the rules using scatter plot
library(arulesViz)
plot(rules) 
