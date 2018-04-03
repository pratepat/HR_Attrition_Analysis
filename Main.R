## Load relevant libraries

library(dplyr)
library(reshape2)
library(StatMeasures)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(gridExtra)

## Load Dataset files
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

######################################################################################
############## Data Cleaning
######################################################################################

## Merge files

# Check if all employees are present in all data files
setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID)
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID)
setdiff(general_data$EmployeeID,in_time$X)
setdiff(general_data$EmployeeID,in_time$X)

# Start merging
general_data <- merge(general_data , employee_survey_data, by="EmployeeID", all = F)
general_data <- merge(general_data , manager_survey_data, by="EmployeeID", all = F)

# Melt in time and out time to use long format
in_time <- melt(in_time, id.vars = "X" )
out_time <- melt(out_time, id.vars = "X" )

sapply(in_time, function(y) sum(length(which(is.na(y)))))


# Convert time data into relevant format before merging
time_sheet <- merge(in_time, out_time , by = c("X","variable"))

colnames(time_sheet) <- c("EmployeeID","Date","InTime","OutTime")

# Convert text to date fields
time_sheet$InTime <- as.POSIXct(time_sheet$InTime)
time_sheet$OutTime <- as.POSIXct(time_sheet$OutTime)

# Derive month and week
time_sheet$Month <- format(time_sheet$InTime , "%m")
time_sheet$Week <- format(time_sheet$InTime , "%W")

# Find the time spent in office by employees
time_sheet$TimeSpent <- as.numeric(round(time_sheet$OutTime - time_sheet$InTime))

# Remove NA rows pertaining to holidays and weekends
sapply(time_sheet, function(y) sum(length(which(is.na(y)))))

time_sheet <- na.omit(time_sheet)

# Derive the daily monthly and weekly averages
DailyAverageHrs <- time_sheet %>%
  group_by(EmployeeID) %>%
  summarise(DailyAverageHrs = round(mean(TimeSpent,na.rm = 'T')))

MonthlyAverageHrs <- time_sheet %>%
  group_by(EmployeeID,Month) %>%
  summarise(MonthlyAverageHrs = round(mean(TimeSpent,na.rm = 'T')))

WeeklyAverageHrs <- time_sheet %>%
  group_by(EmployeeID,Week) %>%
  summarise(WeeklyAverageHrs = round(mean(TimeSpent,na.rm = 'T')))

# Merge the monthly weekly and daily averages
time_sheet <- merge(time_sheet, DailyAverageHrs , by = c("EmployeeID"))
time_sheet <- merge(time_sheet, MonthlyAverageHrs , by = c("EmployeeID","Month"))
time_sheet <- merge(time_sheet, WeeklyAverageHrs , by = c("EmployeeID","Week"))

time_sheet <- time_sheet %>%
  group_by(EmployeeID,DailyAverageHrs) %>%
  dplyr::distinct(EmployeeID,DailyAverageHrs)

# Merge timesheet data with main file  
general_data <- merge(general_data , time_sheet, by="EmployeeID", all = F)

## Check for NA 
sapply(general_data, function(y) sum(length(which(is.na(y)))))

#general_data$WorkLifeBalance[is.na(general_data$WorkLifeBalance)] <- median(general_data$WorkLifeBalance, na.rm=TRUE)
#general_data$JobSatisfaction[is.na(general_data$JobSatisfaction)] <- median(general_data$JobSatisfaction, na.rm=TRUE)
#general_data$EnvironmentSatisfaction[is.na(general_data$EnvironmentSatisfaction)] <- median(general_data$EnvironmentSatisfaction, na.rm=TRUE)

# Remove rows with NA 110/4410 ~ 2% data
general_data <- na.omit(general_data)

# Check for discrete values
sapply(general_data, function(y) sum(length(unique(y))))

# Columns EmployeeCount, Over18 and StandardHours have only 1 value hence redundant for analysis
general_data <- subset(general_data, select = -c(EmployeeCount,StandardHours,Over18))
str(general_data)

# Derive Overtime yes/No
# Overtime = Yes if employee spends more than 8 hrs in office
general_data$Overtime <- sapply(general_data$DailyAverageHrs, FUN = function(x) {ifelse(x > 8, "Yes","No")})
general_data$Overtime <- as.factor(general_data$Overtime)

# Derive SatisfactionLevel
# Total of employee survey data
general_data$EmployeeSatisfaction <- (as.numeric(general_data$EnvironmentSatisfaction) +
                                        as.numeric(general_data$JobSatisfaction) +
                                        as.numeric(general_data$WorkLifeBalance))

general_data$ManagerSurvey <- (as.numeric(general_data$JobInvolvement) +
                                 as.numeric(general_data$PerformanceRating) )

# Identify categorical variables and convert them to factors
idx_categorical <-
  c("Attrition",
    "BusinessTravel",
    "Department",
    "Education",
    "EducationField",
    "Gender",
    "JobLevel",
    "JobRole",
    "MaritalStatus",
    "StockOptionLevel",
    "EnvironmentSatisfaction",
    "JobSatisfaction",
    "WorkLifeBalance",
    "JobInvolvement",
    "PerformanceRating",
    "Overtime")

general_data[, idx_categorical] <- lapply(general_data[, idx_categorical], factor)

hr_data <- general_data

## Attrition rate
hr_data %>%
  group_by(Attrition) %>%
  summarise(Num = n()) %>%
  mutate(Attrition_Rate = Num/4410)
# 16%

######################################################################################
############## Exploratory Data Analysis
######################################################################################

# Reusable code
hr_bar <- geom_bar(stat = "count", position = "dodge") 
hr_text <- geom_text(stat = "count", aes(label = ..count..), position = position_dodge(0.9),vjust = -0.25)

hr_bar_rate <- geom_bar (stat = "identity", position = "dodge")
hr_text_rate <- geom_text(
  stat = "identity",
  aes(label = paste(AttritionRate, "%", sep = "")),
  position = position_dodge(0.9),
  vjust = -0.2
)
hr_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1,face="bold"), 
                  axis.text.y = element_text(face="bold"))

############## Univariate Categorical variables

# 1: BusinessTravel
ggplot(hr_data, aes(x = BusinessTravel, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Business Travel", x = "Business Travel Type", y = "No Of Employees")

BusinessTravelData <- hr_data %>%
  group_by(BusinessTravel,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(BusinessTravel) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(BusinessTravelData,aes(x = BusinessTravel, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Business Travel", x = "Business Travel Type", y = "Attrition Rate")

# Most of the employees Travel_Rarely 
# but the most attrition (25%) is seen in employees who travel frequently

# 2: Department
ggplot(hr_data, aes(x = Department, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Department", x = "Department Type", y = "No Of Employees") 

DepartmentData <- hr_data %>%
  group_by(Department,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(Department) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(DepartmentData ,aes(x = Department, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Department", x = "Department Type", y = "Attrition Rate") 

# Most of the employees are in R&D and the highest attrition is in this department only
# but the most attrition (29%) is seen in employees in HR

# 3: Education
ggplot(hr_data, aes(x = Education, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Education level", x = "Education Type", y = "No Of Employees") +
  scale_x_discrete(breaks=c("1","2","3","4","5"), 
                   labels=c("Below College", "College", "Bachelor","Master","Doctor"))

EducationData <- hr_data %>%
  group_by(Education,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(Education) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(EducationData ,aes(x = Education, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Education level", x = "Education Type", y = "Attrition Rate") +
  scale_x_discrete(breaks=c("1","2","3","4","5"), 
                   labels=c("Below College", "College", "Bachelor","Master","Doctor")) 

# Most of the employees are bachelors and see the highest number of attrition 
# but the percent of attrition is nearly same at all education levels

# 4: EducationField
ggplot(hr_data, aes(x = EducationField, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Education Fields", x = "Education Field", y = "No Of Employees") 

EducationFieldData <- hr_data %>%
  group_by(EducationField,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(EducationField) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(EducationFieldData ,aes(x = EducationField, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Education Fields", x = "Education Field", y = "Attrition Rate")

# Most of the employees are from life sciences and medical fields
# but HR has the highest percent of attrition (40%)

# 5: Gender
ggplot(hr_data, aes(x = Gender, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Gender", x = "Gender", y = "No Of Employees")

GenderData <- hr_data %>%
  group_by(Gender,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(Gender) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(GenderData ,aes(x = Gender, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Gender", x = "Gender", y = "Attrition Rate") 

# The data contains more number of Male employees but the attrition rate is nearly same for both

# 6: JobLevel
ggplot(hr_data, aes(x = JobLevel, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Job Levels", x = "Job Level", y = "No Of Employees") 

JobLevelData <- hr_data %>%
  group_by(JobLevel,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(JobLevel) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(JobLevelData ,aes(x = JobLevel, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Job Levels", x = "Job Level", y = "Attrition Rate")

# Most of the employees are at Job Level 1 and 2 and hence have a higher number of attritions
# But all levels have equal perent of attrition rates

# 7: JobRole
ggplot(hr_data, aes(x = JobRole, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Job Roles", x = "Job Role", y = "No Of Employees") 

JobRoleData <- hr_data %>%
  group_by(JobRole,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(JobRole) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(JobRoleData ,aes(x = JobRole, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Job Roles", x = "Job Role", y = "Attrition Rate") 

# Highest number of attritions are seen in employess in Sales exec and research scientest roles
# But the percent attrition is same in all job roles except research director with a 
# little high attrition rate of (23 %)

# 8: MaritalStatus
ggplot(hr_data, aes(x = MaritalStatus, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Marital Status", x = "Marital Status", y = "No Of Employees")

MaritalStatusData <- hr_data %>%
  group_by(MaritalStatus,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(MaritalStatus) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(MaritalStatusData ,aes(x = MaritalStatus, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Marital Status", x = "Marital Status", y = "Attrition Rate") 

# Highest number of attritions are seen in employess who are single
# which also has the highest percent of attrition (25 %)

# 9: StockOptionLevel
ggplot(hr_data, aes(x = StockOptionLevel, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Stock Option Level", x = "Stock Option Level", y = "No Of Employees")

StockOptionLevelData <- hr_data %>%
  group_by(StockOptionLevel,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(StockOptionLevel) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(StockOptionLevelData ,aes(x = StockOptionLevel, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Marital Status", x = "Marital Status", y = "Attrition Rate")

# Highest number of attritions are seen in employess who have 0 or 1 stock options
# Whereas the attriction rate is nearly same at all levels

# 10: EnvironmentSatisfaction
ggplot(hr_data, aes(x = EnvironmentSatisfaction, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Environment Satisfaction", x = "Environment Satisfaction Level", y = "No Of Employees") +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Low", "Medium", "High","Very High"))

EnvironmentSatisfactionData <- hr_data %>%
  group_by(EnvironmentSatisfaction,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(EnvironmentSatisfaction) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

EnvironmentSatisfaction <- ggplot(EnvironmentSatisfactionData ,aes(x = EnvironmentSatisfaction, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Environment Satisfaction") +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Low", "Medium", "High","Very High"))

EnvironmentSatisfaction

# Clearly employees with LOW Environment Satisfaction Level have a higher attrition
# in terms of number and percent

# 11: JobSatisfaction
ggplot(hr_data, aes(x = JobSatisfaction, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Job Satisfaction", x = "Job Satisfaction Level", y = "No Of Employees") +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Low", "Medium", "High","Very High"))

JobSatisfactionData <- hr_data %>%
  group_by(JobSatisfaction,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(JobSatisfaction) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

JobSatisfaction <- ggplot(JobSatisfactionData ,aes(x = JobSatisfaction, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross Job Satisfaction", x = "Job Satisfaction Level", y = "Attrition Rate") +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Low", "Medium", "High","Very High"))

JobSatisfaction

# Employees with a HIGH JobSatisfaction have the higher number of attrition
# but terms of percent it is clear that LOW JobSatisfaction is the cause (23 %)

# 12: WorkLifeBalance
ggplot(hr_data, aes(x = WorkLifeBalance, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross  Work Life Balance", x = "Work Life Balance Level", y = "No Of Employees") +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Bad", "Good", "Better","Best"))

WorkLifeBalanceData <- hr_data %>%
  group_by(WorkLifeBalance,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(WorkLifeBalance) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

WorkLifeBalance <- ggplot(WorkLifeBalanceData ,aes(x = WorkLifeBalance, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross  Work Life Balance", x = "Work Life Balance Level", y = "Attrition Rate") +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Bad", "Good", "Better","Best"))

WorkLifeBalance

# Most employees have voted for a Better WorkLifeBalance
# But the ones with BAD WorkLifeBalance have a higher attrition rate of 31 %

grid.arrange(EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance)

# 13: JobInvolvement
ggplot(hr_data, aes(x = JobInvolvement, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Job Involvement", x = "Job Involvement Level", y = "No Of Employees") +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Low", "Medium", "High","Very High"))

JobInvolvementData <- hr_data %>%
  group_by(JobInvolvement,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(JobInvolvement) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(JobInvolvementData ,aes(x = JobInvolvement, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross  Job Involvement", x = "Job Involvement Level", y = "Attrition Rate") +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Low", "Medium", "High","Very High"))

# Most employees have voted for a High Job Involvement
# But the ones with LOW Job Involvement have a higher attrition rate of 22 %

# 14: PerformanceRating
ggplot(hr_data, aes(x = PerformanceRating, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition accross Performance Rating", x = "Performance Rating", y = "No Of Employees") +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Low", "Good", "Excellent","Outstanding"))

PerformanceRatingData <- hr_data %>%
  group_by(PerformanceRating,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(PerformanceRating) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(PerformanceRatingData ,aes(x = PerformanceRating, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition accross  Performance Rating", x = "Performance Rating", y = "Attrition Rate") +
  scale_x_discrete(breaks=c("1","2","3","4"), labels=c("Low", "Good", "Excellent","Outstanding"))

# The data contains info on employees with Excellent or Outstanding ratings
# both have nearly the same attrition rate with Excellent having more people leaving

# 15: Overtime
ggplot(hr_data, aes(x = Overtime, fill = Attrition)) +
  hr_bar + hr_text + hr_theme +
  labs(title = "Attrition Vs Overtime", x = "Overtime", y = "No Of Employees") 

OvertimeData <- hr_data %>%
  group_by(Overtime,Attrition) %>%
  summarise(Num = n()) %>%
  group_by(Overtime) %>%
  mutate(AttritionRate = round(Num / sum(Num) * 100))

ggplot(OvertimeData ,aes(x = Overtime, y = AttritionRate, fill = Attrition)) +
  hr_bar_rate + hr_text_rate + hr_theme +
  labs(title = "Attrition Vs Overtime", x = "Overtime", y = "Attrition Rate") 

# The data contains info on employees with Excellent or Outstanding ratings
# both have nearly the same attrition rate with Excellent having more people leaving

##  Summary:
# 1: Employee who Travel Frequently have a 25% attrition rate.
# 2: HR department has a small population but 29% attrition rate. However by population R&D has more 
#    employees leaving
# 3: Job Role research director has a higher Attrition rate of 23%
# 4: Marital Status "Single" Employees have a 25% attrition rate.
# 5: Employee Survey data clearly affects the Attrition rate. With employees choosing "Low" or "Bad"
#    in the survey have a high attrition rate in all three categories.

############## Univariate Continuous variables

# Now lets see the effect on attrition due to continuous variables

# 1: Age
ggplot(hr_data, aes(x = Attrition,y = Age, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Age Spread", x = "Attrition", y = "Age") +
  theme(axis.text = element_text(face="bold"))

ggplot(hr_data, aes(x = Overtime, y = Age, color = Attrition)) + 
  geom_jitter() +  
  labs(title = "Attrition Vs Age", x = "Overtime", y = "Age") +
  theme(axis.text = element_text(face="bold"))

ggplot(subset(hr_data, Attrition == "Yes"), aes(x = Age, y = MonthlyIncome)) + 
  geom_jitter(col = "Blue") +  
  labs(title = "Age Vs Monthly Income", x = "Age", y = "Monthly Income") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$Age,seq(0,1,0.01))
outliers(hr_data$Age)

# No overall outliers in data

# Employees leaving the company have a lower median of age.

# 2: DistanceFromHome
ggplot(hr_data, aes(x = Attrition,y = DistanceFromHome, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Distance From Home Spread", x = "Attrition", y = "DistanceFromHome") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$DistanceFromHome,seq(0,1,0.01))
outliers(hr_data$DistanceFromHome)

# No overall outliers in data

# Distance from home does not seem to affect attrition

# 3: MonthlyIncome
ggplot(hr_data, aes(x = Attrition,y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Monthly Income Spread", x = "Attrition", y = "Monthly Income") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$MonthlyIncome,seq(0,1,0.01))
unique(outliers(hr_data$MonthlyIncome)$idxOutliers)

# capping the outlier values above 92 percentile
hr_data$MonthlyIncome[which(hr_data$MonthlyIncome > quantile(hr_data$MonthlyIncome,probs = 0.92))] <- 
  quantile(hr_data$MonthlyIncome,probs = 0.92) 

# Employees leaving the comapny have the same median of monthly income 
# but have a lower 75th quantile

# 4: NumCompaniesWorked
ggplot(hr_data, aes(x = Attrition,y = NumCompaniesWorked, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Num Companies Worked Spread", x = "Attrition", y = "Num Companies Worked") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$NumCompaniesWorked,seq(0,1,0.01))
outliers(hr_data$NumCompaniesWorked)

# capping the outlier values above 96 percentile
hr_data$NumCompaniesWorked[which(hr_data$NumCompaniesWorked > quantile(hr_data$NumCompaniesWorked,probs = 0.96))] <- 
  quantile(hr_data$NumCompaniesWorked,probs = 0.96) 

ggplot(subset(hr_data, Attrition == "Yes"), aes(x = NumCompaniesWorked)) +
  geom_density(fill = "Green") +
  labs(title = "Employees leaving the company", x = "Num Companies Worked", y = "Density") +
  theme(axis.text = element_text(face="bold"))

#  Most Employees leaving the company are freshers or have worked in 1 company

# 5: PercentSalaryHike
ggplot(hr_data, aes(x = Attrition,y = PercentSalaryHike, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Percent Salary Hike Spread", x = "Attrition", y = "Percent Salary Hike") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
outliers(hr_data$PercentSalaryHike)

# No outliers

# Employees leaving the comapny have the same median of Salary Hikes

# 6: TotalWorkingYears
ggplot(hr_data, aes(x = Attrition,y = TotalWorkingYears, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Work Experience Spread", x = "Attrition", y = "Total Working Years") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$TotalWorkingYears,seq(0,1,0.01))
unique(outliers(hr_data$TotalWorkingYears)$idxOutliers)

# capping the TotalWorkingYears to 95 percentile
hr_data$TotalWorkingYears[which(hr_data$TotalWorkingYears > quantile(hr_data$TotalWorkingYears,probs = 0.95))] <- 
  quantile(hr_data$TotalWorkingYears,probs = 0.95)

# Employees leaving the comapny have the lower median of work Experience

# 7: TrainingTimesLastYear
ggplot(hr_data, aes(x = Attrition,y = TrainingTimesLastYear, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Training Times Last Year Spread", x = "Attrition", y = "Training Times Last Year") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
unique(outliers(hr_data$TotalWorkingYears)$idxOutliers)
# No Outliers

# Training times does not affect attrition

# 8: YearsAtCompany
ggplot(hr_data, aes(x = Attrition,y = YearsAtCompany, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Years At Company Spread", x = "Attrition", y = "Years At Company") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$YearsAtCompany,seq(0,1,0.01))
unique(outliers(hr_data$YearsAtCompany)$idxOutliers)

# capping the YearsAtCompany to 95 percentile
hr_data$YearsAtCompany[which(hr_data$YearsAtCompany > quantile(hr_data$YearsAtCompany,probs = 0.95))] <- 
  quantile(hr_data$YearsAtCompany,probs = 0.95)

# Employees leaving the company have a lower median of time spent in the company

# 9: YearsSinceLastPromotion
ggplot(hr_data, aes(x = Attrition,y = YearsSinceLastPromotion, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Years Since Last Promotion Spread", x = "Attrition", y = "Years Since Last Promotion") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$YearsSinceLastPromotion,seq(0,1,0.01))
unique(outliers(hr_data$YearsSinceLastPromotion)$idxOutliers)

# capping the YearsSinceLastPromotion to 92 percentile
hr_data$YearsSinceLastPromotion[which(hr_data$YearsSinceLastPromotion > quantile(hr_data$YearsSinceLastPromotion,probs = 0.92))] <- 
  quantile(hr_data$YearsSinceLastPromotion,probs = 0.92)

# 10: YearsWithCurrManager
ggplot(hr_data, aes(x = Attrition,y = YearsWithCurrManager, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Years With Curr Manager Spread", x = "Attrition", y = "Years With Curr Manager") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$YearsWithCurrManager,seq(0,1,0.01))
unique(outliers(hr_data$YearsWithCurrManager)$idxOutliers)
# Employees spending more time under a manager have less tendency to leave the company

# capping the YearsWithCurrManager to 99 percentile
hr_data$YearsWithCurrManager[which(hr_data$YearsWithCurrManager > quantile(hr_data$YearsWithCurrManager,probs = 0.99))] <- 
  quantile(hr_data$YearsWithCurrManager,probs = 0.99)

# Employees leaving the comapany spend less time under a manager.

# 11: DailyAverageHrs
ggplot(hr_data, aes(x = Attrition,y = DailyAverageHrs, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Daily Average Hrs Spread", x = "Attrition", y = "Daily Average Hrs") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(hr_data$DailyAverageHrs,seq(0,1,0.01))
unique(outliers(hr_data$DailyAverageHrs)$idxOutliers)

# capping the DailyAverageHrs to 85 percentile
hr_data$DailyAverageHrs[which(hr_data$DailyAverageHrs > quantile(hr_data$DailyAverageHrs,probs = 0.85))] <- 
  quantile(hr_data$DailyAverageHrs,probs = 0.85)

# Employees leaving the comapany are spending high amount of time in office
# suggesting over burdening

## Summary
# 1: Age: Younger employees have higher tendancy to leave the company
# 2: Most Employees leaving the company are freshers or have worked in 1 company.
# 3: Work Experience: As confirmed by above point also employees leaving the comapany have a
#    lower median of work experience.
# 4: Employees leaving the comapnay have a lower median of Years Spent in a company.
# 5: Employees leaving the comapany spend less time under a manager.
# 6: Employees doing overtime in the comapany have a higher tendency to leave.


### MonthlyIncome across Age
ggplot(hr_data, aes(x = MonthlyIncome, y = Age, color = Attrition)) + 
  geom_jitter() +
  labs(title = "Monthly Income across Employee Age", x = "Monthly Income", y = "Age") +
  theme(axis.text = element_text(face="bold"))

### MonthlyIncome Vs OverTime
ggplot(hr_data, aes(x = Overtime, y = MonthlyIncome, color = Attrition)) + 
  facet_grid(~Attrition) +
  geom_jitter() +  
  labs(title = "MonthlyIncome Vs Overtime", x = "Overtime", y = "MonthlyIncome") +
  theme(axis.text = element_text(face="bold"))

### MonthlyIncome Vs TotalWorkingYears
ggplot(hr_data, aes(x = MonthlyIncome, y = TotalWorkingYears, color = Attrition)) + 
  facet_grid(~Attrition) +
  geom_jitter() +  
  labs(title = "Monthly Income Vs Total Working Years", x = "Monthly Income", y = "Total Working Years") +
  theme(axis.text = element_text(face="bold"))

### MonthlyIncome Vs EmployeeSatisfaction
ggplot(hr_data, aes(x = MonthlyIncome, y = EmployeeSatisfaction, color = Attrition)) + 
  facet_grid(~Attrition) +
  geom_jitter() +  
  labs(title = "Monthly Income Vs Employee Satisfaction", x = "Monthly Income", y = "Employee Satisfaction") +
  theme(axis.text = element_text(face="bold"))

############## Bivariate Analysis
# Lets see the correlation between the continuous variables.

cor.hr_data <- hr_data %>%
  mutate(Education = as.numeric(Education),
         JobLevel = as.numeric(JobLevel),
         StockOptionLevel = as.numeric(StockOptionLevel),
         JobInvolvement = as.numeric(JobInvolvement),
         PerformanceRating = as.numeric(PerformanceRating)) %>%
  dplyr::select(
    Age,
    DistanceFromHome,
    Education,
    JobLevel,
    NumCompaniesWorked,
    PercentSalaryHike,
    StockOptionLevel,
    TotalWorkingYears,
    TrainingTimesLastYear,
    YearsAtCompany,
    YearsSinceLastPromotion,
    YearsWithCurrManager,
    JobInvolvement,
    PerformanceRating,
    EmployeeSatisfaction,
    DailyAverageHrs
  )

cormat <- round(cor(cor.hr_data,use = "complete.obs"),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

melted_cormat %>%
  filter((value >= 0.5 | value <= -0.5)  & Var1 != Var2) %>%
  arrange(desc(value))

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Summary
# 1: Performance rating is highly correlated to salary hike.
# 2: Years with current manager and years at company are highly correlated.
# 3: Age, Total working years and years at company are higly correlated. It is also expected.

######################################################################################
############## Model Preperation
######################################################################################

##  Create dummy variables for all categorical variables
hr_data.model <- hr_data

# Remove EmployeeID as not needed in modelling
hr_data.model <- subset(hr_data.model, select = -c(EmployeeID))

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
hr_data.model$Attrition <- ifelse(hr_data.model$Attrition == "Yes", 1, 0)

## Converting rest of the categorical variables to dummies

# 1: BusinessTravel 
levels(hr_data.model$BusinessTravel)
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(BusinessTravel)), dummy_1)

# 2: Department 
levels(hr_data.model$Department)
dummy_1 <- data.frame(model.matrix( ~Department, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(Department)), dummy_1)

# 3: Education 
levels(hr_data.model$Education)
dummy_1 <- data.frame(model.matrix( ~Education, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(Education)), dummy_1)

# 4: EducationField 
levels(hr_data.model$EducationField)
dummy_1 <- data.frame(model.matrix( ~EducationField, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(EducationField)), dummy_1)

# 5: Gender 
levels(hr_data.model$Gender)
# only two levels hence
levels(hr_data.model$Gender)<-c(1,0)
hr_data.model$Gender <- as.numeric(levels(hr_data.model$Gender))[hr_data.model$Gender]
unique(hr_data.model$Gender)

# 6: JobLevel 
levels(hr_data.model$JobLevel)
dummy_1 <- data.frame(model.matrix( ~JobLevel, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(JobLevel)), dummy_1)

# 7: JobRole 
levels(hr_data.model$JobRole)
dummy_1 <- data.frame(model.matrix( ~JobRole, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(JobRole)), dummy_1)

# 8: MaritalStatus 
levels(hr_data.model$MaritalStatus)
dummy_1 <- data.frame(model.matrix( ~MaritalStatus, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(MaritalStatus)), dummy_1)

# 9: EnvironmentSatisfaction 
levels(hr_data.model$EnvironmentSatisfaction)
dummy_1 <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(EnvironmentSatisfaction)), dummy_1)

# 10: JobSatisfaction 
levels(hr_data.model$JobSatisfaction)
dummy_1 <- data.frame(model.matrix( ~JobSatisfaction, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(JobSatisfaction)), dummy_1)

# 11: WorkLifeBalance 
levels(hr_data.model$WorkLifeBalance)
dummy_1 <- data.frame(model.matrix( ~WorkLifeBalance, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(WorkLifeBalance)), dummy_1)

# 12: JobInvolvement 
levels(hr_data.model$JobInvolvement)
dummy_1 <- data.frame(model.matrix( ~JobInvolvement, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(JobInvolvement)), dummy_1)

# 13: PerformanceRating 
levels(hr_data.model$PerformanceRating)
# only two levels hence
levels(hr_data.model$PerformanceRating)<-c(1,0)
hr_data.model$PerformanceRating <- as.numeric(levels(hr_data.model$PerformanceRating))[hr_data.model$PerformanceRating]
unique(hr_data.model$PerformanceRating)

# 14: StockOptionLevel 
levels(hr_data.model$StockOptionLevel)
dummy_1 <- data.frame(model.matrix( ~StockOptionLevel, data = hr_data.model))
dummy_1 <- dummy_1[,-1]
hr_data.model <- cbind(subset(hr_data.model, select = -c(StockOptionLevel)), dummy_1)

# 15: Overtime 
levels(hr_data.model$Overtime)
# only two levels hence
levels(hr_data.model$Overtime)<-c(0,1)
hr_data.model$Overtime <- as.numeric(levels(hr_data.model$Overtime))[hr_data.model$Overtime]
unique(hr_data.model$Overtime)

## Verify that all columns must be num or int
str(hr_data.model)

################################################################
# Feature standardisation

# Normalising continuous features 
hr_data.model$Age <- scale(hr_data.model$Age) 
hr_data.model$DistanceFromHome <- scale(hr_data.model$DistanceFromHome) 
hr_data.model$MonthlyIncome <- scale(hr_data.model$MonthlyIncome) 
hr_data.model$NumCompaniesWorked <- scale(hr_data.model$NumCompaniesWorked) 
hr_data.model$PercentSalaryHike <- scale(hr_data.model$PercentSalaryHike) 
hr_data.model$TotalWorkingYears <- scale(hr_data.model$TotalWorkingYears) 
hr_data.model$TrainingTimesLastYear <- scale(hr_data.model$TrainingTimesLastYear) 
hr_data.model$YearsAtCompany <- scale(hr_data.model$YearsAtCompany) 
hr_data.model$YearsSinceLastPromotion <- scale(hr_data.model$YearsSinceLastPromotion) 
hr_data.model$YearsWithCurrManager <- scale(hr_data.model$YearsWithCurrManager) 
hr_data.model$DailyAverageHrs <- scale(hr_data.model$DailyAverageHrs) 
hr_data.model$EmployeeSatisfaction <- scale(hr_data.model$EmployeeSatisfaction) 
hr_data.model$ManagerSurvey <- scale(hr_data.model$ManagerSurvey) 

# Checking Attrition rate of employees
Attrition_Rate <- sum(hr_data.model$Attrition)/nrow(hr_data.model)
Attrition_Rate # 16.16% Attrition rate. 

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(hr_data.model$Attrition, SplitRatio = 0.7)

Attrition.train = hr_data.model[indices,]

Attrition.test = hr_data.model[!(indices),]

sapply(hr_data.model, function(y) sum(length(which(is.na(y)))))

########################################################################
# Logistic Regression: 

# Checking Attrition rate of employees in train data
Attrition_Train <- sum(Attrition.train$Attrition)/nrow(Attrition.train)
Attrition_Train # 16.15% Attrition rate. 

# Checking Attrition rate of employees in test data
Attrition_Test <- sum(Attrition.test$Attrition)/nrow(Attrition.test)
Attrition_Test # 16.2% Attrition rate. 

# The data is correctly divided.

#Initial model
model_1 = glm(Attrition ~ ., data = Attrition.train, family = "binomial")
summary(model_1) #AIC: 2102.9

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Checking for multicollinearrity
vif(model_2)
model.vif <- data.frame(vif(model_2))

# Analysis model_2
# Looking at variables which are most insignificant  and high VIF > 2
# "BusinessTravelTravel_Rarely" less significant and has a high VIF and hence can be removed
# AIC: 2071.7

# Running model_3 removing BusinessTravelTravel_Rarely
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 DailyAverageHrs + Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobLevel5 + 
                 JobRoleHuman.Resources + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusMarried + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                 JobSatisfaction2 + JobSatisfaction3 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
               data = Attrition.train)

summary(model_3)

# Checking for multicollinearrity
vif(model_3)
model.vif <- data.frame(vif(model_3))

# Analysis model_3
# "DailyAverageHrs" is most insignificant and has a high VIF > 2.
# 2076.9

# Running model_4 removing DailyAverageHrs
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobLevel5 + 
                 JobRoleHuman.Resources + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusMarried + MaritalStatusSingle + EnvironmentSatisfaction2 + 
                 JobSatisfaction2 + JobSatisfaction3 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
               data = Attrition.train)

summary(model_4)

# Checking for multicollinearrity
vif(model_4)
model.vif <- data.frame(vif(model_4))

# Analysis model_4
# "MaritalStatusMarried" is insignificant and has a high VIF > 2.
# AIC: 2077.3

# Running model_5 removing MaritalStatusMarried
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobLevel5 + 
                 JobRoleHuman.Resources + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + 
                 JobSatisfaction2 + JobSatisfaction3 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
               data = Attrition.train)

summary(model_5)

# Checking for multicollinearrity
vif(model_5)
model.vif <- data.frame(vif(model_5))

# Analysis model_5
# All variables with high VIF are highly relevant
# "JobSatisfaction3" is most insignificant.
# AIC: 2078.1

# Running model_6 removing JobSatisfaction3
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobLevel5 + 
                 JobRoleHuman.Resources + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + 
                 JobSatisfaction2 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
               data = Attrition.train)

summary(model_6)

# Checking for multicollinearrity
vif(model_6)
model.vif <- data.frame(vif(model_6))

# Analysis model_6
# All variables with high VIF are highly relevant
# "JobLevel5" is most insignificant.
# AIC: 2078

# Running model_7 removing JobLevel5
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + 
                 JobRoleHuman.Resources + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + 
                 JobSatisfaction2 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
               data = Attrition.train)

summary(model_7)

# Checking for multicollinearrity
vif(model_7)
model.vif <- data.frame(vif(model_7))

# Analysis model_7
# All variables with high VIF are highly relevant
# "JobRoleHuman.Resources" is most insignificant.
# AIC: 2078

# Running model_8 removing JobRoleHuman.Resources
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + 
                 JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + 
                 JobSatisfaction2 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
               data = Attrition.train)

summary(model_8)

# Checking for multicollinearrity
vif(model_8)
model.vif <- data.frame(vif(model_8))

# Analysis model_8
# All variables with high VIF are highly relevant
# "JobRoleManager" is most insignificant.
# AIC: 2078.7

# Running model_9 removing JobRoleManager
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                 Education5 + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + EnvironmentSatisfaction2 + 
                 JobSatisfaction2 + WorkLifeBalance2 + 
                 WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
               data = Attrition.train)

summary(model_9)

# Checking for multicollinearrity
vif(model_9)
model.vif <- data.frame(vif(model_9))

# Analysis model_9
# All variables with high VIF are highly relevant
# "Education5" is most insignificant.
# AIC: 2079.3

# Running model_10 removing Education5
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  EducationFieldLife.Sciences + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + EnvironmentSatisfaction2 + 
                  JobSatisfaction2 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
                data = Attrition.train)

summary(model_10)

# Checking for multicollinearrity
vif(model_10)
model.vif <- data.frame(vif(model_10))

# Analysis model_10
# All variables with high VIF are highly relevant
# "EnvironmentSatisfaction2" is most insignificant.
# AIC: 2080.8

# Running model_11 removing EnvironmentSatisfaction2
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  EducationFieldLife.Sciences + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + JobSatisfaction2 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
                data = Attrition.train)

summary(model_11)

# Checking for multicollinearrity
vif(model_11)
model.vif <- data.frame(vif(model_11))

# Analysis model_11
# All variables with high VIF are highly relevant
# All variables are now in significant range. Removing variable with high VIF
# EducationFieldLife.Sciences
# AIC: 2080.8

# Running model_12 removing EducationFieldLife.Sciences
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + JobSatisfaction2 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
                data = Attrition.train)

summary(model_12)

# Checking for multicollinearrity
vif(model_12)
model.vif <- data.frame(vif(model_12))

# Analysis model_12
# "EducationFieldMedical" has a high VIF.
# AIC: 2096.8

# Running model_13 removing EducationFieldMedical
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  EducationFieldMarketing + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + JobSatisfaction2 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
                data = Attrition.train)

summary(model_13)

# Checking for multicollinearrity
vif(model_13)
model.vif <- data.frame(vif(model_13))

# Analysis model_13
# "EducationFieldMarketing" is barely significant.
# AIC: 2096.1

# Running model_14 removing EducationFieldMarketing
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + JobSatisfaction2 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
                data = Attrition.train)

summary(model_14)

# Checking for multicollinearrity
vif(model_14)
model.vif <- data.frame(vif(model_14))

# Analysis model_14
# "EducationFieldOther"is barely significant.
# AIC: 2095.3

# Running model_15 removing EducationFieldOther
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  EducationFieldTechnical.Degree + JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + JobSatisfaction2 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
                data = Attrition.train)

summary(model_15)

# Checking for multicollinearrity
vif(model_15)
model.vif <- data.frame(vif(model_15))

# Analysis model_15
# "EducationFieldTechnical.Degree" is barely significant.
# AIC: 2094.4

# Running model_16 removing EducationFieldTechnical.Degree
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + JobSatisfaction2 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3 + StockOptionLevel1, family = "binomial", 
                data = Attrition.train)

summary(model_16)

# Checking for multicollinearrity
vif(model_16)
model.vif <- data.frame(vif(model_16))

# Analysis model_16
# "StockOptionLevel1" is barely significant.
# AIC: 2094.8

# Running model_17 removing StockOptionLevel1
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + JobSatisfaction2 + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3, family = "binomial", 
                data = Attrition.train)

summary(model_17)

# Checking for multicollinearrity
vif(model_17)
model.vif <- data.frame(vif(model_17))

# Analysis model_17
# "JobSatisfaction2" is barely significant.
# AIC: 2097.8

# Running model_18 removing JobSatisfaction2
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                  MaritalStatusSingle + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3, family = "binomial", 
                data = Attrition.train)

summary(model_18)

# Checking for multicollinearrity
vif(model_18)
model.vif <- data.frame(vif(model_18))

# Analysis model_18
# JobRoleResearch.Scientist 
# AIC: 2101.2

# Running model_19 removing JobRoleResearch.Scientist
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + 
                  MaritalStatusSingle + WorkLifeBalance2 + 
                  WorkLifeBalance3 + JobInvolvement3, family = "binomial", 
                data = Attrition.train)

summary(model_19)

# Checking for multicollinearrity
vif(model_19)
model.vif <- data.frame(vif(model_19))

# Analysis model_19
# "JobInvolvement3" is barely significant.
# AIC: 2105.3

# Running model_20 removing JobInvolvement3
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  JobLevel2 + JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + 
                  MaritalStatusSingle + WorkLifeBalance2 + 
                  WorkLifeBalance3 , family = "binomial", 
                data = Attrition.train)

summary(model_20)

# Checking for multicollinearrity
vif(model_20)
model.vif <- data.frame(vif(model_20))

# AIC: 2110

# Analysis model_20
# All variables are significant
# "JobLevel2" is least significant.
# AIC: 2105.3

# Running model_21 removing JobLevel2
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  JobRoleResearch.Director + JobRoleSales.Executive + 
                  MaritalStatusSingle + WorkLifeBalance2 + 
                  WorkLifeBalance3 , family = "binomial", 
                data = Attrition.train)

summary(model_21)

# Checking for multicollinearrity
vif(model_21)
model.vif <- data.frame(vif(model_21))

# AIC: 2115.5

# Analysis model_21
# All variables are significant
# "JobRoleResearch.Director" is least significant.
# AIC: 2105.3

# Running model_22 removing JobRoleResearch.Director
model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + 
                  JobRoleSales.Executive + 
                  MaritalStatusSingle + WorkLifeBalance2 + 
                  WorkLifeBalance3 , family = "binomial", 
                data = Attrition.train)

summary(model_22)

# Checking for multicollinearrity
vif(model_22)
model.vif <- data.frame(vif(model_22))

# AIC: 2121.6

# Analysis model_19
# All variables are significant
# "JobRoleSales.Executive" is least significant.
# AIC: 2105.3

# Running model_23 removing JobRoleSales.Executive
model_23 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Overtime + EmployeeSatisfaction + BusinessTravelTravel_Frequently + 
                  JobRoleManufacturing.Director + MaritalStatusSingle + WorkLifeBalance2 + 
                  WorkLifeBalance3 , family = "binomial", 
                data = Attrition.train)

summary(model_23)

# Checking for multicollinearrity
vif(model_23)
model.vif <- data.frame(vif(model_23))

# AIC: 2127.1

########################################################################
# With 13 significant variables in the model

final_model<- model_23

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition 1 for test data

test_pred = predict(final_model, type = "response", newdata = Attrition.test[,-2])

# Let's see the summary 

summary(test_pred)

Attrition.test$prob <- test_pred
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(Attrition.test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

## Confusion matrix at 50% cutoff
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Accuracy : 0.8636
# Sensitivity : 0.29665         
# Specificity : 0.97317

# Sensitivity is very low. We need to find Attritions hence a higher sensitivity is needed
####################################################
# Let's Choose the cutoff value. 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

a <- data.frame(OUT)

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff

# Let's choose a cutoff value of 0.1775 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >= 0.1775, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

table(test_actual_attrition,test_cutoff_attrition)

# With 17.75% cutoff sensitivity has improved considerably with Accuracy still being good at 0.77

# Accuracy : 0.7782
# Sensitivity : 0.6985         
# Specificity : 0.7937

##################################################################################################

### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

table(test_actual_attrition,test_cutoff_attrition)

#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.4922741

plot(performance_measures_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart
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

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile

# Create gain and lift charts
pr <- prediction(test_pred, Attrition.test$Attrition)

gain <- performance(pr, measure = "tpr", x.measure = "fpr")
lift <- performance(pr, measure = "lift", x.measure="rpp")

plot(gain)
plot(lift)

#################################################################

## AUC 
prf <- performance(pr, measure = "auc")
prf@y.values

# Area under the gain curve as 0.81 is a very good value achieved from the model