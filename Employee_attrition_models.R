
library("readxl")
data <- read_xlsx(file.choose())

dim(data)

head(data,10)

tail(data,10)
 
View(data) #view of data

str(data) # kind of data

table(data$status) # 1 status means already attrited and 0 not attrited

# to see what percentage attrited

(table(data$status)==0)/((table(data$status)==0)+ (table(data$status)==1))


summary(data)

# changing some of the variable types
data$status <- as.factor(data$status)
data$job_level <- as.factor(data$job_level) # count the number of people at each job level
data$no_of_promotions <- as.factor(data$no_of_promotions)
data$risk_of_attrition <- as.factor(data$risk_of_attrition)
data$potential_rating <- as.factor(data$potential_rating)

library(summarytools)

dfSummary(data)

# no of missing values per column:

colSums(is.na(data))




## Feature Engineering -
##Data transformation
##Dimension Reduction
##Variable Creatin (eg- for derived variables)

## Since we are focusing on Attrition, so one of the reasons could be wide variation in the performance rating of the employees, so we need to determine the variance in the performance rating of year 2017 and 2018

data$var_rating <- as.factor(data$performance_rating_2018 - data$performance_rating_2017)
data$var_rating


# low rise in salary could also be reason of attrition

data$perc_salary_change <- ((data$salary_2018 - data$salary_2017)/(data$salary_2017))*100
data$perc_salary_change

# Retirememnt age could also be the reason, so here's calculating teh age of all the employees
data$age <- 2018 - data$year_of_birth
data$age

# Summary of each of the variables created above

dfSummary(data$age)
dfSummary(data$perc_salary_change)
dfSummary((data$var_rating)) # 'Freqs' shows the count of people and 'stats' shows whether the rating is in positive (which is desired) or negative (not desired))




# Dimension reduction
data[,c('year_of_birth','salary_2018','salary_2017','performance_rating_2018','performance_rating_2017','hire_date','e_code')] <-  list(NULL)
names(data)


# Bivariate Analysis - Relation between two Variables (categorical and categorical variables here)
# we create mosaic plots

library(vcd)

mosaic(~ gender + status, data = data, gp=gpar(fill=matrix(c("red","blue"),2,2)))  # to see proportion of males/females with their status

tab <- table(data$status)
round(prop.table(tab,1)*100,digits =2) # for geting the proprtions for each category

tab2 <- table(data$status,data$gender) # gender wise status
round(prop.table(tab2,1)*100,digits =2)  

round(prop.table(tab2,2)*100,digits =2) #column wise perspective

# Similar analysis to be done for the column 'service agreement'
mosaic(~ service_agreement + status, data = data, gp=gpar(fill=matrix(c("red","blue"),2,2)))
#metric approach

ser1 <- table(data$status)
round(prop.table(ser1,1)*100,digits =2)

ser2 <- table(data$status,data$service_agreement)
round(prop.table(ser2,1)*100,digits =2)

round(prop.table(ser2,2)*100,digits =2)

#Job level (has 5 levels)

mosaic(~ job_level + status, data = data, gp=gpar(fill=matrix(c("red","blue","green","yellow","pink"),5,2)))  # to see proportion of males/females with their status

job1 <- table(data$status)
round(prop.table(job1,1)*100,digits =2) # for geting the proprtions for each category

job2 <- table(data$status,data$job_level) 
round(prop.table(job2,1)*100,digits =2)  

round(prop.table(job2,2)*100,digits =2) #column wise perspective


#for the rating

mosaic(~ var_rating + status, data = data, gp=gpar(fill=matrix(c("red","blue","green","yellow","pink"),5,2)))  # to see proportion of males/females with their status

rating <- table(data$status)
round(prop.table(rating,1)*100,digits =2) # for geting the proprtions for each category

rating2 <- table(data$status,data$var_rating) 
round(prop.table(rating2,1)*100,digits =2)  

round(prop.table(rating2,2)*100,digits =2) #column wise perspective


# no of promotions is also a very imp factor for someone to stay in here

prom <- table(data$status,data$no_of_promotions) 
round(prop.table(prom,1)*100,digits =2)  

round(prop.table(prom,2)*100,digits =2) #column wise perspective

# risk of attrition

mosaic(~ risk_of_attrition + status, data = data, gp=gpar(fill=matrix(c("red","blue","green","yellow"),4,2)))  # to see proportion of males/females with their status

roa <- table(data$status)
round(prop.table(roa,1)*100,digits =2) # for geting the proprtions for each category

roa2 <- table(data$status,data$risk_of_attrition) 
round(prop.table(roa2,1)*100,digits =2)  

round(prop.table(roa2,2)*100,digits =2) #column wise perspective

# potential rating

mosaic(~ potential_rating + status, data = data, gp=gpar(fill=matrix(c("red","blue","green","yellow","black"),5,2)))  # to see proportion of males/females with their status

pr <- table(data$status)
round(prop.table(pr,1)*100,digits =2) # for geting the proprtions for each category

pr2 <- table(data$status,data$potential_rating) 
round(prop.table(pr2,1)*100,digits =2)  

round(prop.table(pr2,2)*100,digits =2) #column wise perspective

#awards (whether people have received awards) (v.imp to keep the employees motivated)

mosaic(~ awards + status, data = data, gp=gpar(fill=matrix(c("red","blue"),2,2)))  # to see proportion of males/females with their status

dec <- table(data$status)
round(prop.table(dec,1)*100,digits =2) # for geting the proprtions for each category

dec2 <- table(data$status,data$awards) 
round(prop.table(dec2,1)*100,digits =2)  

round(prop.table(dec2,2)*100,digits =2) #column wise perspective

# signon (signing bonus)

mosaic(~ signon + status, data = data, gp=gpar(fill=matrix(c("red","blue"),2,2)))  # to see proportion of males/females with their status

signed <- table(data$status)
round(prop.table(signed,1)*100,digits =2) # for geting the proprtions for each category

signed2 <- table(data$status,data$signon) 
round(prop.table(signed2,1)*100,digits =2)  

round(prop.table(signed2,2)*100,digits =2) #column wise perspective



#Hypothesis Testing 
#We will do a Chi Square test here to determine the rekationship between the two variables
##Hypothesis testing on the categorical variables worked upon above


## Ho: no relationship between the variables
## H1: there is a relationship between the variables


var1 <- table(data$gender, data$status)
chisq.test(var1)

# high p value - so there is no relation between the attrition status of a person and that person being a male or a female

var2 <- table(data$service_agreement, data$status)
chisq.test(var2)  # low p value

var3 <- table(data$job_level, data$status)
chisq.test(var3)

var4 <- table(data$no_of_promotions, data$status)
chisq.test(var4)

var5 <- table(data$risk_of_attrition, data$status)
chisq.test(var5)

var6 <- table(data$potential_rating, data$status)
chisq.test(var6)

var7 <- table(data$awards, data$status)
chisq.test(var7)

var8 <- table(data$signon, data$status)
chisq.test(var8) # does not drive attrition

var9 <- table(data$var_rating, data$status)
chisq.test(var9)


# Numerical and categorical data

library (ggplot2)
p <- ggplot(data, aes(x=status,y=age)) + geom_violin()

p + geom_violin(trim = FALSE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

avg <- by(data$age, data$status, mean)
avg
med <- by(data$age, data$status, median)
med
maxi <- by(data$age, data$status, max)
maxi
mini <- by(data$age, data$status, min)
mini

p2 <- ggplot(data, aes(x=status,y=distance_from_home)) + geom_violin()
p2 + geom_violin(trim = FALSE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

p3 <- ggplot(data, aes(x=status,y=manager_sat)) + geom_violin()
p3 + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

p4 <- ggplot(data, aes(x=status,y=employee_sat)) + geom_violin()
p4 + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75)) #quite interesting to see the skewness in the plots. For the one who underwent attrition, its more skewed on the right whereas the ones who did not undergo attrition, skewness was on the left hand side.

p5 <- ggplot(data, aes(x=status,y=bonus)) + geom_violin()
p5 + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))
# high difference  between mean and the median which states that the data is skewed

p6 <- ggplot(data, aes(x=status,y=no_courses_taken)) + geom_violin()
p6 + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75)) #from the avg, max, min and median, we could conclude that may be the people are not happy with the courses and hence are leaving the company or could be managed horizontally than vertically or vice-versa

p6 <- ggplot(data, aes(x=status,y=no_courses_taken)) + geom_violin()
p6 + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75))

p7 <- ggplot(data, aes(x=status,y=time_in_position)) + geom_violin()
p7 + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75)) # maximum value - very very high, so there might be the case that something wrong with the data

p8 <- ggplot(data, aes(x=status,y=perc_salary_change)) + geom_violin()
p8 + geom_violin(trim = TRUE) + geom_violin(draw_quantiles = c(0.25,0.5,0.75)) # maximum salary change is the key.. if you dont give a good hike to the employee, they are bound to leave the company.

# Hypothesis testing for numerical-categorical variable (T-test)

#Ho: the variable are not related/independent of each other
#H1 : the variables are related to each other

t.test(age ~ status, data = data)
t.test(distance_from_home ~ status, data = data)
# reject null hypothesis due to low p value
t.test(manager_changes ~ status, data = data)
t.test(manager_sat ~ status, data = data)
t.test(employee_sat ~ status, data = data)
t.test(bonus ~ status, data = data)
t.test(no_courses_taken ~ status, data = data)
t.test(time_in_position ~ status, data = data) # independent
t.test(perc_salary_change ~ status, data = data)


# Now dimension Reduction- we need to remove the variables who does not have impact on Y variable- status
data[,c('gender','job_level','signon','age','manager_changes','bonus','time_in_position')] <-  list(NULL)
names(data)


library(fastDummies)
results_dummy <- dummy_cols(data, remove_most_frequent_dummy = T) # the one which has the highest frequency will not be shown among all its levels

names(results_dummy)

# now remove the original variable from where we have created dummy variables:

results_dummy [,c("status","service_agreement","no_of_promotions","risk_of_attrition","potential_rating","awards")] <- list(NULL)

names(results_dummy)


colnames(results_dummy)[26] <- "var_rating_minus_1"
colnames(results_dummy)[25]<- "var_rating_minus_3"
colnames(results_dummy)[27]<- "var_rating_1"

library(caret)
# splitting the data into train test split
results_dummy <- results_dummy[sample(nrow(results_dummy)),] # shuffle the data as per rows

#dummy index

trainIndex <- createDataPartition(results_dummy$status_1, p=0.7, list = FALSE)


x_train<- results_dummy[trainIndex,] # get all the columns but onlky those rows that match the ones in trainIndex
x_test <- results_dummy[-trainIndex,]

dim(x_train)
dim(x_test)


library(randomForest)
model_1 <- randomForest(status_1~., data=x_train)

model_1

# the output showed regression because our desired variable i.e status_1 is integer, and we need to convert it into categorical!
#SO now we need to convert it into factors with the help of as.factor so that random forest can do classification.

x_train$status_1 <- as.factor(x_train$status_1)
x_test$status_1 <- as.factor(x_test$status_1)

model_1 <- randomForest(status_1~., data=x_train)# main thing to focus - classification matrix

getTree(model_1,1) # getting the first tree
# Prediction = 0 means no Prediction
# Prediction = 1 means for us in this case, it is 0 i.e no attrition
# Prediction = 2 means for us that in this case, there was an attrition

# status = -1 means that the tree is ending
# status = 1 means that the tree will go further

# Now to determine the most important model across the trees:

library(varImp)
importance_frame <- importance(model_1)

varImpPlot(model_1)


predictions2 <- predict(model_1, x_train) # to get predictive values 

table(x_train$status_1, predictions2) # creating a confusion matrix


predictions3 <- predict(model_1, x_test)
table(x_test$status_1, predictions3)


# creating ROC Curve
library(pROC)
result.roc <- roc(as.numeric(as.character(x_test$status_1)),as.numeric(as.character(predictions3)))
plot(result.roc)


# plotting the logistic regression 

logistic <- glm(status~.,data= data, family = binomial)
summary(logistic)

# so from the output of the variables in the summary, we an see that the significant variables are:
#-> when employees have a service agreement, they are bound to leave the company (highlyy significant)
#-> risk of attrition at level 2 and 3
#-> When an employee has a potential rating = 4 (Which is not bad though)but still company could face the attrition. The main reason could be those people are getting better carer opportunities outside the current organixzation
#-> no of courses  taken highly significant
#-> awards/appreciation play a very important role, as seen in the random forest too in employee attrition
#-> manager satisfaction and employee satistfaction also play a significant role.


# Gradient Boosting

# A similar ensemble model to build decision trees, but after the first tree is made, for the next round it takes into consideration those factors also which were not considered in the first go.
# It combines weak learners into a strong prediction



objControl <- trainControl(method = 'cv', number = 3, returnResamp='none',
                           summaryFunction = twoClassSummary,classProbs = TRUE)



require(gbm)
emp_attrit <- gbm(status_1 ~. , data = x_train,distribution = "gaussian",n.trees = 10000,
                  shrinkage = 0.01,cv.folds=5, verbose=F)

summary(emp_attrit)

no_of_iter <- gbm.perf(emp_attrit, method = "cv")  #Estimates the optimal number of boosting iterations for a gbm object and optionally plots various performance measures


library(caret)

set.seed(300)

fitControl = trainControl(method="cv", number=5, returnResamp = "all")

model4 = train(status_1 ~. , data = x_train, method="gbm",distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=no_of_iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))

emp_predict <- predict(object =
                         model4,
                              x_test,
                                na.action=na.pass)

confusionMatrix(emp_predict, x_test)