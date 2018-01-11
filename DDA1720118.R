#**********************************************************************************************************
# HR Analytics case study  - Logistic regression and evaluation of model                                  *
# This is a group case study                                                                              *
#                                                                                                         *
# Members:                                                                                                *
# 1.Anargha Biswas                                                                                        *     
# 2.Prasasthy .K.B                                                                                        *
# 3.Sekhar Sahu                                                                                           *
# 4.Surabhi Varshney                                                                                      *
#                                                                                                         *
# Brief on subject:                                                                                       * 
#---------------------------------------------------------------------------------------------------------*
# A large company named XYZ, employs, at any given point of time, around 4000 employees. However, every   * 
# year, around 15% of its employees leave the company and need to be replaced with the talent pool        *
# available in the job market. The management believes that this level of attrition (employees leaving,   *
# either on their own or because they got fired) is bad for the company, because of the following reasons-*
#                                                                                                         *
# 1.The former employees' projects get delayed, which makes it difficult to meet timelines, resulting in a* 
#   reputation loss among consumers and partners                                                          *
# 2.A sizeable department has to be maintained, for the purposes of recruiting new talent                 *
# 3.More often than not, the new employees have to be trained for the job and/or given time to acclimatise*
#   themselves to the company                                                                             *
#                                                                                                         *
# Input files used:                                                                                       *
#                                                                                                         *
# 1. general_data.csv  - Provides general information which includes demographic informations             * 
#                      - information on Age, working experience educational background, department he/she *
#                        working in, monthly income, salary hike recieved etc                             *
# 2. employee_survey.csv  - information on survey done for employee regarding job statisfacton, enviroment*
#                           statisfaction etc                                                             *
# 3. manager_survey.csv   - information on survey given by manager regarding performance of the employee  *
#                           job involvement level                                                         *
# 4. in_time.csv          - information on in time of employee, helps for calculation of working hours    *
# 5. out_time.csv         - information on out time of employee, helps for calcualtion of working hours of*
#                           employee                                                                      *
#Goal:                                                                                                    *
#To create a model with the probability of attrition using a logistic regression.                         *
#---------------------------------------------------------------------------------------------------------*
#libraries used 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(MASS)
library(car)
library(GGally)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(ROCR)
library(scales)
library(Information)
library(InformationValue)

#Reading input data
general_data             <- read.csv("general_data.csv", stringsAsFactors = F) #Demographic data
employee_survey          <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey           <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time_initial          <- read.csv("in_time.csv", stringsAsFactors = F)
out_time_initial         <- read.csv("out_time.csv", stringsAsFactors = F)

#Retaining initial versions
in_time  <- in_time_initial
out_time <- out_time_initial

colnames(in_time) [1]  <- "EmployeeID"
colnames(out_time) [1] <- "EmployeeID"

#Checking for duplicates in key field
sum(duplicated(general_data$EmployeeID))    #Return 0
sum(duplicated(employee_survey$EmployeeID)) #Return 0
sum(duplicated(manager_survey$EmployeeID))  #Return 0
sum(duplicated(in_time$EmployeeID))         #Return 0
sum(duplicated(out_time$EmployeeID))        #Return 0

#checking for NA's in different datasets
sum(is.na(general_data)) # 28 
sum(is.na(employee_survey)) # 83 
sum(is.na(manager_survey))  # 0

#Checking for individual fields in general data
sapply(general_data, function(x) length(which(is.na(x))))

###################################################################################################################
#As there are around 19 NA's in NumCompaniesWorked  and 9 in TotalWorkingYears
#For NumCompaniesWorked:
#1. NA - can be replaced with 2 if TotalWorkingYears and YearsAtCompany is not same and difference between them
#   is just one years or else can be replaced as 1 where  TotalWorkingYears = YearsAtCompany
#2. zero's are replaced with 2 if difference between TotalWorkingYears and TotalWorkingYears is 1 , assuming that
#   the employee wouldn't be possibly changing the company more than once in an year as most of the companies have
#   probationary period of 1 year with contract
#
#For TotalWorkingYears
#1. NA - replace it with YearsAtCompany if NumCompaniesWorked is 0 or 1 
##################################################################################################################

#Calculating difference between total working years and years at company
general_data$diff_wrking_yrs <- general_data$TotalWorkingYears - general_data$YearsAtCompany

general_data$NumCompaniesWorked [(which(is.na(general_data$NumCompaniesWorked)))] <- 
  ifelse(general_data$diff_wrking_yrs [(which(is.na(general_data$NumCompaniesWorked)))] == 1, 2, 
         ifelse(general_data$diff_wrking_yrs [(which(is.na(general_data$NumCompaniesWorked)))] == 0, 1, NA))

sum(is.na(general_data$NumCompaniesWorked)) #Returns 9 which can be ignored

sum(is.na(general_data$TotalWorkingYears))  #Returns 9
general_data$TotalWorkingYears [(which(is.na(general_data$TotalWorkingYears)))] <- 
  ifelse(general_data$NumCompaniesWorked [(which(is.na(general_data$TotalWorkingYears)))] == 0, 
         general_data$YearsAtCompany [(which(is.na(general_data$TotalWorkingYears)))], 
         ifelse(general_data$NumCompaniesWorked [(which(is.na(general_data$TotalWorkingYears)))] == 1,
                general_data$YearsAtCompany [(which(is.na(general_data$TotalWorkingYears)))], NA))

sum(is.na(general_data$TotalWorkingYears)) #Returns 5

length(general_data$NumCompaniesWorked [(which(general_data$NumCompaniesWorked == 0))]) # Returns 586

general_data$NumCompaniesWorked [(which(general_data$NumCompaniesWorked == 0))] <- 
  ifelse(general_data$diff_wrking_yrs [(which((general_data$NumCompaniesWorked == 0)))] == 1, 2, 0)

#Removing difference field of total working years and years at company

general_data <- general_data[ , -25]

sum(is.na(general_data)) # 16 we can be removed

###################################################################################################################
#Data manipulation in IN & OUT time of employees 
#Calculating average working hours of each of previous year
###################################################################################################################
#converting from wide to long format and removing X predecessors
in_time <-  gather(in_time, in_day, in_day_time, X2015.01.01 :X2015.12.31)
in_time$in_day <- str_replace(in_time$in_day, "X", "")

out_time <-  gather(out_time, out_day, out_day_time, X2015.01.01 :X2015.12.31)
out_time$out_day <- str_replace(out_time$out_day, "X", "")

sum(is.na(in_time$in_day_time))     #109080 NA's including holidays and leaves taken
sum(is.na(out_time$out_day_time))   #109080 NA's including holidays and leaves taken

#since both have same number of NA's, which shows in-out are same. So we can remove NA's from the dataset
in_time       <- in_time[ (!is.na(in_time$in_day_time)), ]
out_time      <- out_time[ (!is.na(out_time$out_day_time)), ]

#Combining in and out data into a single dataset
in_out_time   <- data.frame(cbind(in_time$EmployeeID,in_time$in_day_time,out_time$out_day_time ))

#Converting to date and time format for further manipulation
in_out_time$X2 <- parse_date_time(in_out_time$X2, c("Ymd_HMS"), tz="")
in_out_time$X3 <- parse_date_time(in_out_time$X3, c("Ymd_HMS"), tz="")

#Calculating working hours which is the difference between in and out time of a particular date
in_out_time$X4 <-  difftime(in_out_time$X3, in_out_time$X2, units = c("hours"))

#Calculating average working hours
emp_avg <- aggregate(in_out_time$X4, by = list(in_out_time$X1), mean, na.rm=TRUE)

#Renaming the columns
colnames(emp_avg) <- c("EmployeeID", "avg_working_hrs") 

#Rounding the average working hours by 2 decimal
emp_avg$avg_working_hrs <- round(emp_avg$avg_working_hrs, 2)

################################################################################################################
#Checking for difference in key field before merging data
################################################################################################################
setdiff(general_data$EmployeeID, emp_avg$EmployeeID)          #All are identical no difference in employee id
setdiff(general_data$EmployeeID, employee_survey$EmployeeID)  #All are identical no difference in employee id
setdiff(general_data$EmployeeID, manager_survey$EmployeeID)   #All are identical no difference in employee id

#merging data into a single file
base_data <- merge(general_data , emp_avg, by = "EmployeeID")
base_data <- merge(base_data, employee_survey, by = "EmployeeID")
base_data <- merge(base_data, manager_survey, by = "EmployeeID")

str(base_data)

sum(is.na(base_data)) # Returns 99 which is hardly 2% of data
base_data <- na.omit(base_data) #Removing NA's

#Checking for individual fields
sapply(base_data, function(x) length(which(is.na(x)))) #As expected

sapply(base_data, function(x) length(which(x == ""))) #No blank spaces found

#######################################################################################################################
#Overall plotting of data on the basis of attrition 
#######################################################################################################################
data_over_all <- base_data %>% group_by(Attrition) %>% summarise(count1 = n())
data_over_all$count1 <- 100 * data_over_all$count1/nrow(base_data)
data_over_all$count2 <- str_c(round(data_over_all$count1,2),"%")
plot_attrition <- ggplot(data_over_all,aes(x=Attrition,y=count1,fill=Attrition)) + geom_bar(stat="identity") +
                  geom_text(aes(label=count2),vjust = 2)
plot_attrition
#Conclusion : About 16.16% of employees left the company in the year 2015

#Plotting in figures
data_over_all2     <- base_data %>% group_by(Attrition) %>% summarise(counta = n())
plot_attrition_fig <- ggplot(data_over_all2,aes(Attrition,y=counta,fill=Attrition))+geom_bar(stat="identity") +
  geom_text(aes(label=counta),vjust = 2)
plot_attrition_fig

##################################################################################################################
# Barcharts for categorical features with stacked base_data information
#################################################################################################################
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(base_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(),
          ggplot(base_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(base_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(base_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(base_data, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(base_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

#################################################################################################################
#Conclusions from the chart:
#Those who travelled rarely tend to leave company more.
#Attrition is less for HR dept. as low proportion of HR employees are there in an organization. Also Research & 
#Development Dept. have high attrition 
#In correlation with Department, HR have less attrition if considering education field
#Attrition rate is more among male employees as we can assume compared to female employees they tend to have more
#dependants which results in seeking a good package
#Assuming Joblevel 1 denotes employees who have just entered the role/field and 5 denoting highly experienced or 
#experts of the field. From the plot we understand that as the level increases people are less tend to leave 
#company
#Attrition is more among singles and less among divorcees
##################################################################################################################

plot_grid(ggplot(base_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(),
          ggplot(base_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(base_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(base_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(base_data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(base_data, aes(x= factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

##################################################################################################################
#Conclusions from the chart:
#StockOptionLevel - More employees quit in lower levels
#EnvironmentSatisfaction - More employees quit where the statisfaction level is 1 compared to other levels. But not
#showing any significant trend
#JobSatisfaction - Higher attrition among Job Satisfaction level 1.
#WorkLifeBalance - if considering the proportion of data, attrition is more in level 1 which is as expected.
#JobInvolvement  - Attrition is more with involvement level 3
#PerformanceRating - compared to rating 4, rating 3 has high attrition amount.
##################################################################################################################
# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(base_data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(base_data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(base_data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 20),
          ggplot(base_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(base_data, aes(DistanceFromHome))+ geom_histogram(binwidth = 20),
          ggplot(base_data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 
#No outliers in the above numeric variables

plot_grid(ggplot(base_data, aes(YearsAtCompany))+ geom_histogram(binwidth = 20),
          ggplot(base_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(base_data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 20),
          ggplot(base_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(base_data, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(base_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#few outliers in  MonthlyIncome, YearsAtCompany, YearsSinceLastPromotion, which is quite possible so ignoring

##################################################################################################################
#creating new field to understand whether employee is doing extended working hours or not
#As the standard working hours is 8 from data, those who have an average working hours more 8 can be considered 
#for doing extended working hours
##################################################################################################################
base_data$extended_working_hrs <- ifelse(base_data$avg_working_hrs > 8,"Yes","No")

data_over_all3     <- base_data %>% group_by(Attrition, extended_working_hrs ) %>% summarise(countb = n())
plot_extended_wrkhrs <- ggplot(data_over_all3,aes(extended_working_hrs,y=countb,fill=extended_working_hrs))+
                        geom_bar(stat="identity") + facet_grid(~Attrition) + geom_text(aes(label=countb),vjust = 2)
plot_extended_wrkhrs

#Conclusion: There is a relatively higher amount of people working in extended working hours, in the group of 
#those who left the company

##################################################################################################################
#JobTenure: People who have tendency of changing jobs frequently are tend to leave the company within short
#periods. This can be found out from Jobtenure field.
##################################################################################################################
base_data$JobTenure <- ifelse(base_data$NumCompaniesWorked!=0,
                                 round(base_data$TotalWorkingYears/base_data$NumCompaniesWorked,2),0)

plot_tenure <- ggplot(base_data,aes(JobTenure))+geom_density()+facet_grid(~Attrition)
plot_tenure

#Conclusion: This clearly shows the trend as the number of years per job is less for those who quit.

# Boxplots of numeric variables relative to Attrition 
plot_grid(ggplot(base_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(base_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(base_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Correlation between numeric variables
ggpairs(base_data[, c("MonthlyIncome", "PercentSalaryHike", "Age")])

##################################################################################################################
#Since everyone is above the age of 18 this column will not bring any additional information to the model
#so we can drop this variable from overall analysis, similarly we have other variables such as employee count
#and employee number and standard hours which are having unique values through out data
#Employee number/ID not required as it is not contributing towards attrition
##################################################################################################################

base_data <- base_data[, - c( 1, 9, 16, 18)]

str(base_data)

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
base_data$Attrition <- ifelse(base_data$Attrition =="Yes",1,0)

# converting extended_working_hrs from No/Yes character to factorwith levels 0/1 
base_data$extended_working_hrs <- ifelse(base_data$extended_working_hrs =="Yes",1,0)

##################################################################################################################
#converting categorical to factors
##################################################################################################################
# creating dataframe for categorical features and numerical features
base_data_chr<- base_data[,c(3:4, 6:11, 15, 22:27 )]
base_data_num <- base_data[,-c(3:4, 6:11, 15, 22:27 )]

#Standardising numerical data
str(base_data_num)
base_data_num$avg_working_hrs <- as.numeric(base_data_num$avg_working_hrs)
base_data_num_scale <- base_data_num [, c(1,3:13)]
base_data_num_scale <- data.frame(sapply(base_data_num_scale, function(x) scale(x)))
base_data_num       <-  data.frame(Attrition = base_data_num$Attrition)

str(base_data_chr)
# converting categorical attributes to factor
base_data_fact<- data.frame(sapply(base_data_chr, function(x) factor(x)))
str(base_data_fact)

# Making short of long variable names
levels(base_data_fact$BusinessTravel) <- c("NT", "TF", "TR")
levels(base_data_fact$Department)     <- c("HRD", "RNDD", "SD")
levels(base_data_fact$EducationField) <- c("HR", "LS", "MRK", "MED", "NA", "TD")
levels(base_data_fact$JobRole)        <- c("HC", "HR", "Lab", "Man", "MDir", "RsD", "RsSci", "SlEx", "SlRep")

#Checking for summary
sapply(base_data_fact, function(x) summary(x))

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(base_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =base_data_fact))[,-1]))

#Confirming for 2 level categorical fields denotions
sum(dummies$Gender)       #Returns 2579 which is the number of males, so male are represented by 1 and females by 0
sum(dummies$PerformanceRating) #Returns 663 which is the number of records with rating 4, so for rating 3 it is 0
sum(dummies$extended_working_hrs) #Returns 1278 so extended working hours 1 otherwise 0

# Final dataset
base_data_final <- cbind(base_data_num, base_data_num_scale, dummies) #4312 rows with 58 variables

str(base_data_final)

#checking for correlation between Attrition and other variables
correlation_matrix <- cor(base_data_final)
corrplot(correlation_matrix, method = "number", title = "Correlation Map", mar=c(0,0,1,0),
         type = "lower", order = "FPC",col = c("red", "orange", "blue", "green"), number.cex = .5, tl.cex = 0.5)

#Conclusion from correlation matrix 
# Findings: Performance Rating & PercentageSalaryHike is highly correlated so does yearswithchurnmanager
# & yearsatcompany, Also totatl working years is directly correlated with age
##################################################################################################################
# splitting the data between train and test
#################################################################################################################
set.seed(100)

indices = sample.split(base_data_final$Attrition, SplitRatio = 0.7)

train = base_data_final[indices,]

test = base_data_final[!(indices),]

##################################################################################################################
# Logistic Regression: 
##################################################################################################################

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2093.7....58 coeff..nullDev 2670.8...resDev 1977.7

########################################################################################################################
#Model created using STEPAIC function
#######################################################################################################################

model_2<- stepAIC(model_1, direction="both")

summary(model_2)

sort((vif(model_2)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing EducationField.xNA +  as VIF and p value is high

model_3 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                BusinessTravel.xTR + Education.x5 + EducationField.xLS + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
                family = "binomial", data = train)

summary(model_3)

sort((vif(model_3)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing YearsAtCompany +  as VIF and p value is high

model_4 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                BusinessTravel.xTR + Education.x5 + EducationField.xLS + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_4)

sort((vif(model_4)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing BusinessTravel.xTR +   as VIF and p value is high

model_5 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + EducationField.xLS + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_5)

sort((vif(model_5)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing EducationField.xLS +    as VIF and p value is high

model_6 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_6)

sort((vif(model_6)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing WorkLifeBalance.x2 +  as VIF and p value is high

model_7 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                EducationField.xMRK + EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_7)

sort((vif(model_7)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing EducationField.xMRK  as p value is high

model_8 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                 EducationField.xMED +  
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_8)

sort((vif(model_8)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  EducationField.xMED +  as p value is high

model_9 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                EducationField.xTD + JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_9)

sort((vif(model_9)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  EducationField.xTD +  as p value is high

model_10 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                Education.x5 + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                StockOptionLevel.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
              family = "binomial", data = train)

summary(model_10)

sort((vif(model_10)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  StockOptionLevel.x3 + as p value is high

model_11 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xLab + 
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_11)

sort((vif(model_11)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing   JobRole.xLab + as p value is high

model_12 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + MonthlyIncome + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_12)

sort((vif(model_12)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing    MonthlyIncome + as p value is high

model_13 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_13)

sort((vif(model_13)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing    JobLevel.x5 + as p value is high

model_14 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_14)

sort((vif(model_14)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing    MaritalStatus.xMarried +  as p value is high

model_15 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir + JobRole.xRsD + JobRole.xRsSci + JobRole.xSlEx + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_15)

sort((vif(model_15)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing     JobRole.xRsSci +  as p value is high

model_16 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir + JobRole.xRsD + JobRole.xSlEx + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_16)

sort((vif(model_16)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing     StockOptionLevel.x1 + as p value is high

model_17 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir + JobRole.xRsD + JobRole.xSlEx + 
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_17)

sort((vif(model_17)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  JobRole.xRsD + as p value is high

model_18 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir +  JobRole.xSlEx + 
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + extended_working_hrs, 
               family = "binomial", data = train)

summary(model_18)

sort((vif(model_18)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  JobInvolvement.x3 + as p value is high

model_19 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobLevel.x2 +  
                 JobRole.xMDir +  JobRole.xSlEx + 
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_19)

sort((vif(model_19)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing  JobLevel.x2 +  as p value is high

model_20 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobRole.xMDir +  JobRole.xSlEx + 
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_20)

sort((vif(model_20)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing   JobRole.xSlEx + as p value is high

model_21 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 Education.x5 + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_21)

sort((vif(model_21)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable


#Removing   Education.x5 + as p value is high

model_22 <-glm(formula = Attrition ~ NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_22)

sort((vif(model_22)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable


#Removing   TrainingTimesLastYear +  as p value is high

model_23 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 +  extended_working_hrs, 
               family = "binomial", data = train)

summary(model_23)

sort((vif(model_23)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing WorkLifeBalance.x4 +  as p value is high

model_24 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                   extended_working_hrs, 
               family = "binomial", data = train)

summary(model_24)

sort((vif(model_24)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing JobSatisfaction.x3 +   as p value is high

model_25 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 extended_working_hrs, 
               family = "binomial", data = train)

summary(model_25)

sort((vif(model_25)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing JobSatisfaction.x2 +  as p value is high

model_26 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 +  
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 extended_working_hrs, 
               family = "binomial", data = train)

summary(model_26)

sort((vif(model_26)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing WorkLifeBalance.x3 +   as p value is high

model_27 <-glm(formula = Attrition ~ NumCompaniesWorked + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 Age + TotalWorkingYears + BusinessTravel.xTF + 
                 JobRole.xMDir +  
                 MaritalStatus.xSingle +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 +  
                 JobSatisfaction.x4 +  
                 extended_working_hrs, 
               family = "binomial", data = train)

summary(model_27)

sort((vif(model_27)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

##################################################################################################################
# With 13 significant variables in the model
final_model<- model_27
#################################################################################################################
#Model Evaluation :
##################################################################################################################
#Test Data

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

# Let's see the summary 
summary(test_pred)

test$prob <- test_pred

# Let's use the probability cutoff of 50%.
test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_Attrition,test_pred_Attrition)

#################################################################################################################
#Checking for other levels of cut off
#################################################################################################################
#At 0.40 
test_pred_Attrition_1 <- factor(ifelse(test_pred >= 0.40, "Yes", "No")) 
test_conf2 <- confusionMatrix(test_pred_Attrition_1, test_actual_Attrition, positive = "Yes")
test_conf2

#At 0.30
test_pred_Attrition_2 <- factor(ifelse(test_pred >= 0.30, "Yes", "No")) 
test_conf3 <- confusionMatrix(test_pred_Attrition_2, test_actual_Attrition, positive = "Yes")
test_conf3

#At 0.25
test_pred_Attrition_3 <- factor(ifelse(test_pred >= 0.25, "Yes", "No")) 
test_conf4 <- confusionMatrix(test_pred_Attrition_3, test_actual_Attrition, positive = "Yes")
test_conf4
################################################################################################################
# finding the optimal probalility cutoff value
###############################################################################################################
perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff #0.1536364

# Let's choose a cutoff value of 0.1536364 
test_cutoff_Attrition <- factor(ifelse(test_pred >=0.1536364, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]
acc

sens <- conf_final$byClass[1]
sens

spec <- conf_final$byClass[2]
spec

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
###################################################################################################################
#Plot Receiver Operating Characteristics (ROC) Curve: AUC calculation 
##################################################################################################################

plot(performance_measures_test, type = "b", col = "red", lwd=1.5,
     main = "ROC Curve",
     ylab = "Sensitivity:TPR", 
     xlab = "(1 - Specificity):FPR")
abline(0,1, lty = 8, col = "grey", untf = T)
auc<-performance(pred_object_test,"auc")
auc.value <- unlist(auc@y.values)
text(0.8, 0.23, labels=sprintf("AUC: %0.3f", auc.value))

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
################################################################################################################
# Lift & Gain Chart 
# plotting the lift chart
################################################################################################################

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}
Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)

#majority of resp is in top 4 deciles
#################################################################################################################
#plot the lift chart 
################################################################################################################
plot(Attrition_decile$Cumlift, type="l", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

#################################################################################################################
#Plot Gain Chart 
################################################################################################################
ks_plot(test_actual_Attrition, test_cutoff_Attrition) # Gain chart plot

#Correlation of final fields of the model
cor(base_data_final$Attrition,base_data_final$NumCompaniesWorked)           # 0.03360575
cor(base_data_final$Attrition,base_data_final$YearsSinceLastPromotion)      #-0.03068946
cor(base_data_final$Attrition,base_data_final$YearsWithCurrManager)         #-0.1563666
cor(base_data_final$Attrition,base_data_final$Age)                          #-0.15621
cor(base_data_final$Attrition,base_data_final$TotalWorkingYears)            #-0.1693355
cor(base_data_final$Attrition,base_data_final$BusinessTravel.xTF)           #0.1110897
cor(base_data_final$Attrition,base_data_final$JobRole.xMDir)                #-0.04374527
cor(base_data_final$Attrition,base_data_final$MaritalStatus.xSingle)        #0.1720183
cor(base_data_final$Attrition,base_data_final$EnvironmentSatisfaction.x2)   #-0.014466
cor(base_data_final$Attrition,base_data_final$EnvironmentSatisfaction.x3)   #-0.04393481
cor(base_data_final$Attrition,base_data_final$EnvironmentSatisfaction.x4)   #-0.04844704
cor(base_data_final$Attrition,base_data_final$JobSatisfaction.x4)           #-0.08616976
cor(base_data_final$Attrition,base_data_final$extended_working_hrs)         #.2295856

################################################################################################################
#Conclusion:
#The Number of companies worked by a person is more they tend to quit more
#If an employee works with the same manager for a longer period of time the lesser are the chances that 
#employee will leave the company.
#As the age increases the people are less tend to quit, which shows more comfortable with environment
#People with more experience as they are less likely to leave the company. 
#Environment Satisfaction, Job Satisfaction are some of the main features that need to be taken for retaining
#employees
#The more an employee works extended work hours on an average the more are the chances that he/she will leave
#the company.
#Employees who are unmarried are prone to leaving the company.
################################################################################################################
