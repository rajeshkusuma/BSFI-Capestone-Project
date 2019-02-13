#------------------------------------- BFSI CAPESTONE PROJECT------------------------------------------------
# Date: 17/03/2018 ------------------------------------------------------------------------------------------

#  Using the CRISP-DM FRAMEWORK:
#  Business Understanding
#  Data Understanding
#  Data Perparation
#  Model Bilding
#  Model Evaluation
#  Model deployment

# BUSINESS UNDERSTANDING: 
# Intorduction: CredX is a leading credit card provider that gets thousands of credit card applicants 
# every year. But in the past few years, it has experienced an increase in credit loss. So therefore,
# it is important to get the right customer in order reduce the risk.

# Objective: Objective is to identify the factors effecting the credit default and build a machine learing 
# model that classifies the good and bad customers.

# Result: create strategies to mitigate the acquisition risk and assess the financial benefit
# of your project.  

#************************************************************************************************************
# DATA UNDERSTANDING:

# Credit Bureau data
# Demographic data
library(data.table)
library(dplyr)

# Loading the data files.
# 1. Demographics data: This file contains all the demographic information of the customer.
# 2. Credit burea data: This file containg the information of credit transctions of customer. 
#                       The source of this data is CIBIL
credit_burea <- read.csv("Credit Bureau data.csv", stringsAsFactors = F)
demographic <- read.csv("Demographic data.csv", stringsAsFactors = F)

# Firstly, let us understand the data file by look at thier structure.
str(credit_burea)
str(demographic)

# Looking at the summary of the data gives us a sence of the type of data that we are delaing with.
lapply(demographic, function(x) summary(x))
lapply(credit_burea, function(x) summary(x))

# Checking duplicate records
demographic[duplicated(demographic),]
credit_burea[duplicated(credit_burea),]

# Demographics data has duplicate values
length(unique(demographic$Application.ID)) # 71292 unique ids are there and 3 are duplicates.
length(unique(credit_burea$Application.ID)) # 71292 unique ids are there and 3 are duplicates.

# Checking duplicate application ids
Duplicate_demo_app_id_rec <- demographic[duplicated(demographic$Application.ID),]
Duplicate_cred_app_id_rec <- credit_burea[duplicated(credit_burea$Application.ID),]
#Duplicate app ids from both data sets
#765011468 
#653287861  
#671989187

# Removing duplicates
demographic <- demographic[!duplicated(demographic$Application.ID),]
credit_burea <- credit_burea[!duplicated(credit_burea$Application.ID),] 

# Merging the two datasets.
# The count is not matching because there are duplicates values in application id column.
credit_merged <- merge(demographic, credit_burea, by.x =  "Application.ID", by.y =  "Application.ID",all.x = T)

# Check if performance indicators from both data sets are same
credit_merged[which(credit_merged$Performance.Tag.x != credit_merged$Performance.Tag.y),]

# credit_merged
# Now that we have merged the two file, let look at the following data quality issues. 
# 1. Missing Values
# 2. Data quality issues
# 3. Outilers
# 4. Standardise the text
# 5. Fixing invalid Values

# MISSING VALUES TREATMENT:
# Lets check how many missing values each row has
lapply(credit_merged, function(x) sum(is.na(x)))

# These are all the customers with NA values in our target variable. 
View(credit_merged[is.na(credit_merged$Performance.Tag.y), ])

# Assumption : If performanance variabes are not present, then those records are not appoved catagory
# hence consider only approved records for the analysis
credit_merged_approved <- credit_merged[!is.na(credit_merged$Performance.Tag.y), ]


#----------------------------------------------------------------------------------------------
# 2.Data Preparations
#----------------------------------------------------------------------------------------------
# We need to only have one target variable, so removing one.
which(colnames(credit_merged_approved) == "Performance.Tag.x")
credit_merged_approved <- credit_merged_approved[,-12]

# Checking the missing values for approved records
lapply(credit_merged_approved, function(x) sum(is.na(x)))
# Findings
##Demographic
# No.of.dependents : 3 
##Performance indicators
# Presence.of.open.home.loan :272
# Outstanding.Balance :272 
# No.of.trades.opened.in.last.6.months:1
# Avgas.CC.Utilization.in.last.12.months :1023

# The below function will tell us if there are any data quality issues. Go through each varibale and see 
# identify if there is any data quality issue. 
lapply(credit_merged_approved, function(x) summary(x))

# Removing one negative record of Age i.e.-3
credit_merged_approved<- dplyr::filter(credit_merged_approved,credit_merged_approved$Age != -3)

# Found 99 records that have -ve income, so convert them into +ve value
credit_merged_approved$Income <- ifelse(credit_merged_approved$Income == -0.50,0.50,credit_merged_approved$Income)
dplyr::filter(credit_merged_approved,credit_merged_approved$Income == 0.50)

# Removing 99 records of negative record of Income i.e.-0.5
# credit_merged_approved<- dplyr::filter(credit_merged_approved,credit_merged_approved$Income != -.50)

#----------------------------------------------------------------------------------------------
# Outiler treatments
#----------------------------------------------------------------------------------------------
library(ggplot2)

# Plotting Age histogram
ggplot(credit_merged_approved,aes(Age)) + geom_histogram(binwidth = 2)
# Let's check the outlier in the variables 
quantile(credit_merged_approved$Age,seq(0,1,0.01))
# Box plot 
boxplot(credit_merged_approved$Age)
# Capping the low values of age with 27.
credit_merged_approved[(which(credit_merged_approved$Age == 0)),]$Age <- 27

# Plotting Income histogram
ggplot(credit_merged_approved,aes(Income)) + geom_histogram(binwidth = 2)
# Let's check the outlier in the variables 
quantile(credit_merged_approved$Income,seq(0,1,0.01))
# Box plot 
boxplot(credit_merged_approved$Income)

# Check Gender values 
barplot(table(credit_merged_approved$Gender))
count(distinct(credit_merged_approved), Gender)
#count
# 2
# F 16506
# M 53358
# Removing non Gender values
credit_merged_approved <- credit_merged_approved[credit_merged_approved$Gender %in% c("M", "F"), ] 

# Check Marital Status
barplot(prop.table(table(credit_merged_approved$Marital.Status..at.the.time.of.application.)))
count(distinct(credit_merged_approved), Marital.Status..at.the.time.of.application.)
# Count   6         
# Married 59541
# Single  10317
# Impute non Marital status as 'Married'
credit_merged_approved[(which(credit_merged_approved$Marital.Status..at.the.time.of.application. !=  "Married" & credit_merged_approved$Marital.Status..at.the.time.of.application. != "Single")),]$Marital.Status..at.the.time.of.application. <- "Married"

# Check Education
barplot(prop.table(table(credit_merged_approved$Education)))
count(distinct(credit_merged_approved),Education)
credit_merged_approved[(which(credit_merged_approved$Education == "")),]$Education <- "Others"
#count 
# Bachelor     17300
# Masters      23480
# Others       237
# Phd          4463
# Professional 24384

# Check Education
barplot(prop.table(table(credit_merged_approved$Profession)))
count(distinct(credit_merged_approved),Profession)
# remove missing profession records
credit_merged_approved <- dplyr::filter(credit_merged_approved,credit_merged_approved$Profession != "")
# Count
# SAL     39673
# SE      13924
# SE_PROF 16254

# Check Type of residence
barplot(prop.table(table(credit_merged_approved$Type.of.residence)))
count(distinct(credit_merged_approved),Type.of.residence)
# removing missing Type of residence
credit_merged_approved <- dplyr::filter(credit_merged_approved,credit_merged_approved$Type.of.residence != "")
# Count
# Company provided     1603
# Living with Parents  1776
# Others                198
# Owned               14000
# Rented              52266

# Remove missing No of dependents
credit_merged_approved <- dplyr::filter(credit_merged_approved,!is.na(credit_merged_approved$No.of.dependents))

# Remove missing No.of.trades.opened.in.last.6.months
credit_merged_approved <- dplyr::filter(credit_merged_approved,!is.na(credit_merged_approved$No.of.trades.opened.in.last.6.months))

# removing missing Presence.of.open.home.loan
credit_merged_approved <- dplyr::filter(credit_merged_approved,!is.na(credit_merged_approved$Presence.of.open.home.loan))

# removing missing Avgas.CC.Utilization.in.last.12.months
credit_merged_approved <- dplyr::filter(credit_merged_approved,!is.na(credit_merged_approved$Avgas.CC.Utilization.in.last.12.months))

# Checking missing the missing values for approved records
lapply(credit_merged_approved, function(x) sum(is.na(x)))
lapply(credit_merged_approved, function(x) summary(x))

# Note : Make sure your independent categorical variables are stored as factor in R. 
which(sapply(credit_merged_approved, function(x) typeof(x)) == "character")

credit_merged_approved$Gender <- as.factor(as.character(credit_merged_approved$Gender))
credit_merged_approved$Marital.Status..at.the.time.of.application. <- as.factor(credit_merged_approved$Marital.Status..at.the.time.of.application.)
credit_merged_approved$Profession <- as.factor(credit_merged_approved$Profession)
credit_merged_approved$Education <- as.factor(credit_merged_approved$Education)
credit_merged_approved$Type.of.residence <- as.factor(credit_merged_approved$Type.of.residence)
credit_merged_approved$Presence.of.open.auto.loan <- as.factor(credit_merged_approved$Presence.of.open.auto.loan)

num <- as.numeric(which(sapply(credit_merged_approved, function(x) class(x)) == "integer"))
credit_merged_approved[num] <- sapply(credit_merged_approved[num], function(x) as.numeric(x))

str(credit_merged_approved)

#---------------------------------------------------------------------------------------------------
#******************************* END OF DATA PREPARATION **************************************
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------
# EXPLORATORY DATA ANALYSIS
#---------------------------------------------------------------------------------------------------

# As a first step let us try and understand what are the significant variable that are
# present in the data set. For this exercise we will use the concept call WOE and 
# Information value(IV).
library(Information)

subset_credit <- credit_merged_approved
subset_credit <- subset_credit[, -1]
str(subset_credit)

# Let us seperate all the columns with numeric data and categorical data. We need to this because WOE needs
# to be performed separately on numrics and categorical data. 

# calculating the WOE and Information value for all the numeric variables. 
credit_numeric <- subset_credit[, c(which(lapply(subset_credit, function(x) class(x)) == "numeric"))]

WOE_numeric <- create_infotables(data = credit_numeric, y = "Performance.Tag.y", bins = 10, parallel = FALSE)
WOE_numeric

# calculating the WOE and Information value for all the categorical variables. 
which(lapply(subset_credit, function(x) class(x)) == "factor")
credit_categorical <- subset_credit[, c(2,3,6,7,8,27,28)]

WOE_categorical <- create_infotables(data = credit_categorical, y = "Performance.Tag.y", bins = 10, parallel = FALSE)
WOE_categorical

#------------------------------------------------------------------------------------------------------------
#************************************************************************************************************
# PLOTTING WOE PLOTS:
library(cowplot)

# Getting the plots for the WOE for the numeric data.
WOE_numeric$Summary$Variable
plot_infotables(WOE_numeric, WOE_numeric$Summary$Variable[1:5], same_scale = FALSE)
plot_infotables(WOE_numeric, WOE_numeric$Summary$Variable[5:10], same_scale = FALSE)
plot_infotables(WOE_numeric, WOE_numeric$Summary$Variable[10:15], same_scale = FALSE)
plot_infotables(WOE_numeric, WOE_numeric$Summary$Variable[15:20], same_scale = FALSE)


# Getting the plots for the WOE for the categorical data.
WOE_categorical$Summary$Variable
plot_infotables(WOE_categorical, WOE_categorical$Summary$Variable[1:3], same_scale = FALSE)
plot_infotables(WOE_categorical, WOE_categorical$Summary$Variable[4:6], same_scale = FALSE)

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
library(ggplot2)
library("Hmisc")
library(cowplot)
library(ellipse)
library(corrplot)

# We are going to use this theme throught the plotting exercise. 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "left") 

# Generating the plot for Woe variables.
WOE_numeric
WOE_categorical

WOE_numeric_summary <- as.data.frame(WOE_numeric$Summary)
WOE_numeric_summary$IV <- round(WOE_numeric_summary$IV, 3)

WOE_categorical_summary <- as.data.frame(WOE_categorical$Summary)
WOE_categorical_summary$IV <- round(WOE_categorical_summary$IV, 3)


ggplot(data = WOE_numeric_summary, aes(x = reorder(Variable, -IV), y = IV)) +
  geom_bar(stat = "identity", fill = "orange") +  geom_hline(aes( yintercept = 0.0015)) +
  geom_text(aes(label = IV)) + labs(xlab = "Variables") +bar_theme1 + ggtitle("Information Value Contineous Variables")

ggplot(data = WOE_categorical_summary, aes(x = reorder(Variable, -IV), y = IV)) +
  geom_bar(stat = "identity", fill = "orange") + geom_hline(aes( yintercept = 0.0015)) +
  geom_text(aes(label = IV)) + labs(xlab = "Variables") +bar_theme1 + ggtitle("Information Value categorical Variables")

#------------------------------------------------------------------------------------------------------
#------------------------- Bivariate and Multivariate Analysis:---------------------------------------

# Building all the plots for numerical variable against the Target variable.
head(WOE_numeric$Summary$Variable, 15)
plot1 <- ggplot(credit_numeric)


plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = Outstanding.Balance, 
                         fill = factor(Performance.Tag.y))) +
  labs(x = "Performance",fill = "Default") + ggtitle("Outstanding_Balance Vs Performance")


plot_grid(plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,
                                   y = No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("Inquiries Vs Performance"),
          plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = Avgas.CC.Utilization.in.last.12.months, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("Credit Card utilization Vs Performance"))

plot_grid(plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = No.of.PL.trades.opened.in.last.12.months, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("PL Trades Vs Performance"),
          plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = No.of.trades.opened.in.last.12.months, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("Trades opened Vs Performance"))


plot_grid(plot1 + geom_boxplot
          (aes(x = factor(Performance.Tag.y) ,y = Total.No.of.Trades, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("Total Trades Vs Performance"),
          plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = No.of.times.30.DPD.or.worse.in.last.6.months, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("30 DPD Vs Performance"))

plot_grid(plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = No.of.PL.trades.opened.in.last.6.months, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("Total Trades Vs Performance"),
          plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = No.of.times.90.DPD.or.worse.in.last.12.months, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("30 DPD Vs Performance"))

plot_grid(plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = No.of.times.60.DPD.or.worse.in.last.12.months, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("Total Trades Vs Performance"),
          plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = No.of.times.90.DPD.or.worse.in.last.6.months, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("30 DPD Vs Performance"))

plot_grid(plot1 + geom_boxplot
          (aes(x = factor(Performance.Tag.y) ,y = No.of.months.in.current.residence, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("Months in Current Residenc Vs Performance"),
          plot1 + geom_boxplot(aes(x = factor(Performance.Tag.y) ,y = No.of.months.in.current.company, fill = factor(Performance.Tag.y))) +
            labs(x = "Performance",fill = "Default") + ggtitle("Months in Current company Vs Performance"))

# ---------------------------------------------------------------------------------------------
# Generating the corelation matrix between the performance variable and  

correlation_matrix <- cor(subset_credit[,c(head(WOE_numeric$Summary$Variable, 10), "Performance.Tag.y")])
#write.csv(correlation_matrix, "credit_data_corelation.csv")
View(correlation_matrix)

# Calculating the event rate of the defaulters.
sum(credit_merged_approved$Performance.Tag.y)/nrow(credit_merged_approved)
# Default rate is 4%?

ls()

#******************************* END OF EXPLORATORY ANALYSIS **********************************
#**********************************************************************************************

#---------------------------------------------------------------------------------------------
# BUILDING DEMOGRAPHIC MODEL
#---------------------------------------------------------------------------------------------
# Using the demographic data to build the demographic model.  
demo_col <- as.character(names(demographic))
demo_col[12] <- "Performance.Tag.y"

credit_demographic <- credit_merged_approved[demo_col]
str(credit_demographic)

categorical_index <- as.numeric(which(sapply(credit_demographic, function(x) class(x)) == "factor"))
credit_demographic_cat <- credit_demographic[categorical_index]
credit_demographic <- credit_demographic[-categorical_index]

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(credit_demographic_cat, 
                            function(x) data.frame(model.matrix(~x-1,data = credit_demographic_cat))[,-1]))

credit_demographic <- cbind(credit_demographic,dummies)

# calculating the event rate of the defaulters.
sum(credit_demographic$Performance.Tag.y)/nrow(credit_demographic)
# 0.04 % is the event rate.

# Converting the target variable into factor.
credit_demographic$Performance.Tag.y <- as.factor(credit_demographic$Performance.Tag.y)

# Removing application id  column
credit_demographic <- credit_demographic[-1]
# taking a final look at the data set.
str(credit_demographic)

#---------------------------------------------------------------------------------------------------
# BUILDING  DEMOGRAPHIC LOGISTICS REGRESSION MODEL
#---------------------------------------------------------------------------------------------------
# splitting the data between train and test
set.seed(100)

indices = sample.split(credit_demographic$Performance.Tag.y, SplitRatio = 0.5)

train_demo_lr <- SMOTE(form =Performance.Tag.y ~ .,data = credit_demographic[indices,], perc.over = 200, perc.under = 200)
summary(train_demo_lr$Performance.Tag.y)

test_demo_lr = credit_approved_final[!(indices),]
summary(test_demo_lr$Performance.Tag.y)

#Initial model
D_Logistic_model_1 = glm(Performance.Tag.y ~ ., data = train_demo_lr, family = "binomial")
summary(D_Logistic_model_1)

# Stepwise selection
D_Logistic_model_2 <- stepAIC(D_Logistic_model_1, direction="both")
# Step:  AIC = 12816.93

D_Logistic_model_3 <- glm(Performance.Tag.y ~ Age + No.of.dependents + Income + No.of.months.in.current.company + 
                            Gender + Education.xProfessional + Profession.xSE + Type.of.residence.xOthers,
                          data = train_demo_lr, family = "binomial")
summary(D_Logistic_model_3)
vif(D_Logistic_model_3)

# No.of.months.in.current.residence
D_Logistic_model_4 <- glm(Performance.Tag.y ~ Age + No.of.dependents + Income + No.of.months.in.current.company + 
                            Gender + Education.xProfessional + Profession.xSE + 
                            Type.of.residence.xOthers + No.of.months.in.current.residence,
                          data = train_demo_lr, family = "binomial")
summary(D_Logistic_model_4)
vif(D_Logistic_model_4)
# Observation: 
# Age, Income, No.of.months.in.current.company, Profession.xSE 
# Are some of the significant variables from the demographic data.

# *********************************************************************************************
#-------------------- END OF DEMOGRAPHIC MODEL BUILDING ---------------------------------------
# *********************************************************************************************

#---------------------------------------------------------------------------------------------------
# 4. MODEL BUILDING - LOGISTICS REGRESSION
#---------------------------------------------------------------------------------------------------

# STEP 1:
# Preparing the data for  building the logistic regression model on the complete data.

numeric_columns <- as.numeric(which(sapply(credit_merged_approved, function(x) class(x)) == "numeric"))
names(credit_merged_approved[-numeric_columns])

credit_merged_approved_cat <-credit_merged_approved[, c("Gender",
                                                        "Marital.Status..at.the.time.of.application.",
                                                        "Education",
                                                        "Profession",
                                                        "Type.of.residence",
                                                        "Presence.of.open.auto.loan")]

# creating dummy variables for factor attributes
dummies <- data.frame(sapply(credit_merged_approved_cat, 
                            function(x) data.frame(model.matrix(~x-1,
                                data =credit_merged_approved_cat))[,-1]))

credit_merged_approved_num <- credit_merged_approved[numeric_columns]

# Combining the dummy variable data set and numeric data set.
credit_approved_final <- cbind(credit_merged_approved_num,dummies)

#removing application id
credit_approved_final <- credit_approved_final[,-c(1)]
credit_approved_final$Performance.Tag.y <- as.factor(credit_approved_final$Performance.Tag.y)

str(credit_approved_final)

#---------------------------------------------------------------------------------------------------
# STEP 2: Deriving the variables for model building.
#----------------- Derived Metrics ----------------------------
library(dummies)

summary(credit_approved_final$Outstanding.Balance)
bal_summ <- vector(mode = "character",length = length(credit_approved_final$Outstanding.Balance))
for (i in 1:length(credit_approved_final$Outstanding.Balance))
{
  if(credit_approved_final$Outstanding.Balance[i]<=10000)
  {
    bal_summ[i] = "low"
  }
  else if(10000 < credit_approved_final$Outstanding.Balance[i]&&credit_approved_final$Outstanding.Balance[i]<=50000)
  {
    bal_summ[i] = "medium"
  }
  else if(50000 < credit_approved_final$Outstanding.Balance[i]&&credit_approved_final$Outstanding.Balance[i]<=100000)
  {
    bal_summ[i] = "high"
  }
  else if(100000 < credit_approved_final$Outstanding.Balance[i])
  {
    bal_summ[i] = "vhigh"
  }
}
bal_summ2 <- as.factor(bal_summ)
bal_summ2 <- as.data.frame(bal_summ2)
bal_summ2 <- dummy.data.frame(bal_summ2)
credit_approved_final <- cbind(credit_approved_final,bal_summ2)

#---------------------------------------------------------------------------------------------------
# STEP 3: BUILDING THE MODEL - LOGISTIC REGRESSION.
#---------------------------------------------------------------------------------------------------
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(DMwR)

# splitting the data between train and test
set.seed(100)

indices = sample.split(credit_approved_final$Performance.Tag.y, SplitRatio = 0.5)

train_lr <- SMOTE(form =Performance.Tag.y ~ .,data = credit_approved_final[indices,], 
                  perc.over = 200, perc.under = 200)
summary(train_lr$Performance.Tag.y)

test_lr = credit_approved_final[!(indices),]
summary(test_lr$Performance.Tag.y)


#Initial model
Logistic_model_1 = glm(Performance.Tag.y ~ ., data = train_lr, family = "binomial")
summary(Logistic_model_1)

# Stepwise selection
# Logistic_model_2<- stepAIC(Logistic_model_1, direction="both")
# summary(Logistic_model_2)
# vif(Logistic_model_2)
# AIC: 12932

Logistic_model_2 <- glm(formula = Performance.Tag.y ~ No.of.months.in.current.residence + 
                          No.of.months.in.current.company + No.of.times.90.DPD.or.worse.in.last.6.months + 
                          No.of.times.60.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                          No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Presence.of.open.home.loan + Outstanding.Balance + Total.No.of.Trades + 
                          Gender + Education.xPhd + Education.xProfessional + Type.of.residence.xLiving.with.Parents + 
                          Type.of.residence.xOthers + Type.of.residence.xOwned + Type.of.residence.xRented + 
                          Presence.of.open.auto.loan + bal_summ2low + bal_summ2medium, 
                        family = "binomial", data = train_lr)
summary(Logistic_model_2)
vif(Logistic_model_2)

#removing Outstanding.Balance, Very high VIF and insignificant
Logistic_model_3 <- glm(Performance.Tag.y ~ Age + Income + No.of.months.in.current.residence + 
                          No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                          No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Presence.of.open.home.loan + Total.No.of.Trades + 
                          Education.xProfessional + Type.of.residence.xLiving.with.Parents + 
                          Type.of.residence.xOthers + Type.of.residence.xOwned + Type.of.residence.xRented + 
                          Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_3)
vif(Logistic_model_3)

#removing Type.of.residence.xRented, high VIF and insignificant
Logistic_model_4 <- glm(Performance.Tag.y ~ Age + Income + No.of.months.in.current.residence + 
                          No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                          No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Presence.of.open.home.loan + Total.No.of.Trades + 
                          Education.xProfessional + Type.of.residence.xLiving.with.Parents + 
                          Type.of.residence.xOthers + Type.of.residence.xOwned +  
                          Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_4)
vif(Logistic_model_4)

#removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., high VIF and insignificant
Logistic_model_5 <- glm(Performance.Tag.y ~ Age + Income + No.of.months.in.current.residence + 
                          No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                          No.of.PL.trades.opened.in.last.12.months +  
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Presence.of.open.home.loan + Total.No.of.Trades + 
                          Education.xProfessional + Type.of.residence.xLiving.with.Parents + 
                          Type.of.residence.xOthers + Type.of.residence.xOwned +  
                          Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_5)
vif(Logistic_model_5)

#removing Type.of.residence.xLiving.with.Parents, insignificant
Logistic_model_6 <- glm(Performance.Tag.y ~ Age + Income + No.of.months.in.current.residence + 
                          No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                          No.of.PL.trades.opened.in.last.12.months +  
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Presence.of.open.home.loan + Total.No.of.Trades + 
                          Education.xProfessional + Type.of.residence.xOthers + Type.of.residence.xOwned +  
                          Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_6)
vif(Logistic_model_6)

#removing Type.of.residence.xOwned, insignificant
Logistic_model_7 <- glm(Performance.Tag.y ~ Age + Income + No.of.months.in.current.residence + 
                          No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                          No.of.PL.trades.opened.in.last.12.months +  
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Presence.of.open.home.loan + Total.No.of.Trades + 
                          Education.xProfessional + Type.of.residence.xOthers +   
                          Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_7)
vif(Logistic_model_7)

#removing Type.of.residence.xOthers, insignificant
Logistic_model_8 <- glm(Performance.Tag.y ~ Age + Income + No.of.months.in.current.residence + 
                          No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                          No.of.PL.trades.opened.in.last.12.months +  
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Presence.of.open.home.loan + Total.No.of.Trades + 
                          Education.xProfessional + Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_8)
vif(Logistic_model_8)

#removing Age, insignificant
Logistic_model_9 <- glm(Performance.Tag.y ~ Income + No.of.months.in.current.residence + 
                          No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                          No.of.PL.trades.opened.in.last.12.months +  
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Presence.of.open.home.loan + Total.No.of.Trades + 
                          Education.xProfessional + Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_9)
vif(Logistic_model_9)

#removing Education.xProfessional, insignificant
Logistic_model_10 <- glm(Performance.Tag.y ~ Income + No.of.months.in.current.residence + 
                           No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
                           No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                           Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                           No.of.PL.trades.opened.in.last.12.months +  
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                           Presence.of.open.home.loan + Total.No.of.Trades + 
                           Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_10)
vif(Logistic_model_10)

#removing No.of.times.60.DPD.or.worse.in.last.6.months, high Vif insignificant
Logistic_model_11 <- glm(Performance.Tag.y ~ Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                           No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                           Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                           No.of.PL.trades.opened.in.last.12.months +  
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                           Presence.of.open.home.loan + Total.No.of.Trades + 
                           Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_11)
vif(Logistic_model_11)

#removing No.of.times.90.DPD.or.worse.in.last.12.months, high Vif insignificant
Logistic_model_12 <- glm(Performance.Tag.y ~ Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                           No.of.times.30.DPD.or.worse.in.last.6.months +  
                           Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                           No.of.PL.trades.opened.in.last.12.months +  
                           No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                           Presence.of.open.home.loan + Total.No.of.Trades + 
                           Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_12)
vif(Logistic_model_12)

#removing No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,insignificant
Logistic_model_13 <- glm(Performance.Tag.y ~ Income + No.of.months.in.current.residence + No.of.months.in.current.company +
                           No.of.times.30.DPD.or.worse.in.last.6.months +  
                           Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                           No.of.PL.trades.opened.in.last.12.months +  
                           Presence.of.open.home.loan + Total.No.of.Trades + 
                           Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_13)
vif(Logistic_model_13)

#removing Income,insignificant
Logistic_model_14 <- glm(Performance.Tag.y ~ No.of.months.in.current.residence + No.of.months.in.current.company +
                           No.of.times.30.DPD.or.worse.in.last.6.months +  
                           Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
                           No.of.PL.trades.opened.in.last.12.months +  
                           Presence.of.open.home.loan + Total.No.of.Trades + 
                           Presence.of.open.auto.loan, family = "binomial", data = train_lr)

summary(Logistic_model_14)
vif(Logistic_model_14)

#removing No.of.PL.trades.opened.in.last.6.months and No.of.PL.trades.opened.in.last.12.months   
Logistic_model_15 <- glm(Performance.Tag.y ~ No.of.months.in.current.residence + No.of.months.in.current.company +
                           No.of.times.30.DPD.or.worse.in.last.6.months +  
                           Avgas.CC.Utilization.in.last.12.months +  
                           Total.No.of.Trades + Presence.of.open.home.loan   + 
                           Presence.of.open.auto.loan, family = "binomial", data = train_lr)


summary(Logistic_model_15)
vif(Logistic_model_15)

# We have our final logistic regression model.
logistic_final <- Logistic_model_15


#---------------------------------------------------------------------------------------------------
# 4. MODEL EVALUATION - LOGISTICS REGRESSION
#---------------------------------------------------------------------------------------------------

# We now have the final model where we can use it to predict the values in test_lr.
# Predicting probabilities of responding for the test_lr data

# Predicting probabilities of responding for the test_lr data
predicted_defalut <- predict(logistic_final, newdata = test_lr[, -22], type = "response")
summary(predicted_defalut)

test_lr$y_pred <- predicted_defalut

predicted_response <- as.factor(ifelse(test_lr$y_pred  >= 0.50, "yes", "no"))
actual_response <- as.factor(ifelse(test_lr$Performance.Tag.y == 1, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
(confusionMatrix(predicted_response, actual_response , positive = "yes"))


#----------------------------------------------------------------------------------------------    
# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predicted_defalut >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, actual_response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length = 100)

OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab = 1.5,cex.axis = 1.5,ylim = c(0,1),type="l",lwd = 2,axes = FALSE,col = 2)
axis(1,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
axis(2,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
lines(s,OUT[,2],col="darkgreen",lwd = 2)
lines(s,OUT[,3],col = 4,lwd = 2)
box()
legend(0,.50,col = c(2,"darkgreen",4,"darkred"),lwd = c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.5)]
cutoff
#cutoff:0.4158586

#----------------------------------------------------------------------------------------------
# Let's choose a cutoff value of 4.9% for final model

# 0.4752525, 0.4356566, 0.4455556,0.4158586, 0.4257576

predicted_response <- as.factor(ifelse(test_lr$y_pred  >= 0.4158586, "yes", "no"))
actual_response <- as.factor(ifelse(test_lr$Performance.Tag.y == 1, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, actual_response, positive = "yes")

conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

c(acc, sens, spec)

logistic_regression_performance <- c(cutoff = 0.4158586, acc, sens, spec)
# cutoff      Accuracy      Sensitivity   Specificity 
# 0.4158586   0.6048012     0.6397516     0.6032647 

# cutoff      Accuracy    Sensitivity   Specificity 
# 0.4356566   0.6122123   0.6238785     0.6116994

#---------------------------------------------------------------------------------------------------
# MODEL PERFORMANCE EVALUATION - LOGISTICS REGRESSION
#---------------------------------------------------------------------------------------------------
# KS- STATISTIC 
library(ROCR)
# Now to bring the KS Statistic, we need the pridicted_log to be in the test_lr data set. 
predicted_response <- (ifelse(predicted_response == "yes", 1, 0))
actual_response <- (test_lr$Performance.Tag.y)

# For the prediction function to work the "predicted_response" and "actual_response"
# should only contain values 1 and 0.
pred_object_test <- prediction(predicted_response, actual_response)
performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
logistic_regression_ks_stat <- max(ks_table_test)
# 0.2355779

#---------------------------------------------------------------------------------------------------
#------------------------- LIFT AND GAIN CHARTS - LOGISTICS REGRESSION -----------------------------------------------
# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups = 10) {
  
  if (is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if (is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp = sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp/sum(totalresp)*100,
           Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(actual_response, predicted_response, groups = 10)
default_decile

# PLOTTING LIFT AND GAIN CHARTS.
# Comparision between the graphs
plot_grid(
  ggplot(data = default_decile) +
    geom_bar(aes(x = factor(bucket), y = Gain),fill = "blue", stat = "identity") +
    labs(x = "Decile") + ggtitle("Gain Chart"),
  ggplot(data = default_decile) +
    geom_bar(aes(x = factor(bucket), y = Cumlift), fill = "yellow", stat = "identity", show.legend = TRUE) +
    labs(x = "Decile") + ggtitle("Lift Chart")) 

#----------------------------------------------------------------------------------------------    
# Gain Chart:
ggplot(default_decile, aes(x = bucket ,y = Gain))+
  geom_line(color="blue")+ 
  geom_point()+
  ggtitle("Gain Chart") +
  geom_text(hjust = 0, nudge_x = 0.1,aes(label  = round(Gain,digits = 2) , y  = Gain),size = 3,color="red") 

# Observations: 
# gain for a given model is 70% by the 5th decile
# This means is that if we sort all defauters according to probability,
# then among the top 50% customers of this sorted list,
# We would find 60% of all customers that were likely to get deafaulted

#----------------------------------------------------------------------------------------------    
# Lift Chart:
ggplot(default_decile, aes(x = bucket ,y = Cumlift))+ 
  geom_line(color="red") +
  geom_point() +
  ggtitle("Lift Chart")+
  geom_text(hjust = 0, nudge_x = 0.1,aes(label = round(Cumlift,digits = 2), y  = Cumlift),size = 3,color="red") 

# Observations: 
# the model catches 1.59 times more drafulters than
# a random model would have caught

#----------------------------------------------------------------------------------------------    
# ROC
library(pROC)

roc_lr <- roc(actual_response, predicted_response)
lines(roc_rf, col = "blue")

# AUC 
auc(roc_lr) 
# 0.6243

#----------------------------------------------------------------------------------------------
# **********  END OF MODEL BUILDING AND EVALUATION OF LOGISTIC REGRESSION *********************  
#----------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------
# 4.2 MODEL BUILDING - RANDOM FOREST
#----------------------------------------------------------------------------------------------
library(randomForest)

Random_df <- credit_merged_approved 
str(Random_df)

# Preparing the data for building the RandomForest model.
# removing Application id
Random_df <- Random_df[,-1]

# converting performance tag to factor
Random_df$Performance.Tag.y <- as.factor(Random_df$Performance.Tag.y)

#--------------------------------------------------------------------------------------
# Splitting the data into train_rf and test_rf
set.seed(101)

Random_df$Performance.Tag.y <- as.factor(ifelse(Random_df$Performance.Tag.y==1,"yes","no"))

split_indices = sample.split(Random_df$Performance.Tag.y, SplitRatio = 0.5)

train_rf <- SMOTE(form =Performance.Tag.y ~ .,data = Random_df[split_indices, ] ,perc.over = 200,perc.under = 200)
summary(train_rf$Performance.Tag.y)

test_rf <- Random_df[!split_indices, ]
summary(test_rf$Performance.Tag.y)


#----------------------------------------------------------------------------------------    
# BUILDING THE RANDOM FOREST MODEL
#----------------------------------------------------------------------------------------    
RF_credit_model <- randomForest(Performance.Tag.y ~., data = train_rf,
                                proximity = F, do.trace = T, mtry = 5)

RF_predicted <- predict(RF_credit_model, test_rf[, -28], type = "prob")

RF_predicted_response <- as.factor(ifelse(RF_predicted[, 2] >= 0.5, "yes", "no"))
RF_actual_response <- as.factor((test_rf$Performance.Tag.y))

confusionMatrix(RF_predicted_response, RF_actual_response, positive = "yes")

#--------------------------------------------------------------------------------------    
# Finding the optimum cutoff inorder to increase the RandomForest model performance.
# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(RF_predicted[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag.y, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#--------------------------------------------------------------------------------------    
# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length = 100)

#Storing the output of the probability function.
OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab = 1.5,cex.axis = 1.5,ylim = c(0,1),type="l",lwd = 2,axes = FALSE,col = 2)
axis(1,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
axis(2,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd = 2)
lines(s,OUT_rf[,3],col = 4,lwd = 2)
box()

legend(0,.50,col = c(2,"darkgreen",4,"darkred"),lwd = c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.4)]
cutoff_rf

#---------------------------------------------------------------------------------------
# Taking the optimum value of cutoff to increase the model performance. 
# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

RF_predicted_response <- as.factor(ifelse(RF_predicted[, 2] >= 0.3069697, "yes", "no"))
RF_actual_response <- as.factor((test_rf$Performance.Tag.y))

conf_forest <- confusionMatrix(RF_predicted_response, RF_actual_response, positive = "yes")

conf_forest

# Accuracy 
conf_forest$overall[1]

# Sensitivity
conf_forest$byClass[1]

# Specificity 
conf_forest$byClass[2]

Randomforest_model_performance <- c(cutoff = 0.3069697, conf_forest$overall[1],
                                    conf_forest$byClass[1], conf_forest$byClass[2])
#Random Forest                        #Logistic
#cutt off :  0.3069697                #cutt off :  0.4158586
#Accuracy : 0.6138979                 #Accuracy : 0.6048012  
#Sensitivity : 0.621118               #Sensitivity : 0.6397516 
#Specificity :  0.6135805             #Specificity :  0.6032647

#---------------------------------------------------------------------------------------------------
# MODEL PERFORMANCE EVALUATION - RANDOM FOREST
#---------------------------------------------------------------------------------------------------
#--------------------------------- KS- STATISTIC -----------------------------------------------

library(ROCR)

# Now to bring the KS Statistic, we need the pridicted_log to be in the test_lr data set. 
RF_predicted_response <- (ifelse(RF_predicted_response == "yes", 1, 0))
RF_actual_response <- (ifelse(RF_actual_response == "yes", 1, 0))

# For the prediction function to work the "predicted_response" and "actual_response"
# should only contain values 1 and 0.
pred_object_test<- prediction(RF_predicted_response, RF_actual_response)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
randomforest_ks_stat <- max(ks_table_test)

#---------------------------------------------------------------------------------------------------
#------------------------- LIFT AND GAIN CHARTS - RANDOM FOREST-----------------------------------------------
# Loading dplyr package 
library(dplyr)
library(pROC)

# plotting the lift chart
lift <- function(labels , predicted_prob,groups = 10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp = sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp/sum(totalresp)*100,
           Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

RF_decile = lift(RF_actual_response, RF_predicted_response, groups = 10)
RF_decile

# Plotting the gain and lift chart.
plot_grid(
  ggplot(data = RF_decile) +
    geom_bar(aes(x = factor(bucket), y = Gain),fill = "blue", stat = "identity") +
    labs(x = "Decile") + ggtitle("Gain Chart"),
  ggplot(data = RF_decile) +
    geom_bar(aes(x = factor(bucket), y = Cumlift), fill = "yellow", stat = "identity", show.legend = TRUE) +
    labs(x = "Decile") + ggtitle("Lift Chart")) 

#----------------------------------------------------------------------------------------------    
# Gain Chart:
ggplot(RF_decile, aes(x = bucket ,y = Gain))+
  geom_line(color="blue")+ 
  geom_point()+
  ggtitle("Gain Chart") +
  geom_text(hjust = 0, nudge_x = 0.1,aes(label  = round(Gain,digits = 2) , y  = Gain),size = 3,color="red") 

# Observations: 
# gain for a given model is 65% by the 5th decile
# This means is that if we sort all defauters according to probability,
# then among the top 50% customers of this sorted list,
# We would find 65% of all customers that were likely to get deafaulted

#----------------------------------------------------------------------------------------------    
# Lift Chart:
ggplot(RF_decile, aes(x = bucket ,y = Cumlift))+ 
  geom_line(color="red") +
  geom_point() +
  ggtitle("Lift Chart")+
  geom_text(hjust = 0, nudge_x = 0.1,aes(label = round(Cumlift,digits = 2), y  = Cumlift),size = 3,color="red") 

# Observations: 
# the model catches 1.56 times more drafulters than
# a random model would have caught


# ROC curve
roc_rf <- roc(RF_actual_response, RF_predicted_response)
lines(roc_rf, colour = "blue")
#AUC 
auc(roc_rf) 
#0.6173

# Final RF important variables
importance <- as.data.frame(RF_credit_model$importance) 
importance
varImpPlot(RF_credit_model)

#---------------------------------------------------------------------------------------------------
# **********  END OF MODEL BUILDING AND EVALUATION OF RANDOM FOREST *********************  
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------    
# 4.3 MODEL BUILDING - DECISION TREE
#---------------------------------------------------------------------------------------- 
Decision_df <- credit_merged_approved 
str(Decision_df)

# Preparing the data for building the RandomForest model.
# removing Application id
Decision_df <- Decision_df[,-1]

# converting performance tag to factor
Decision_df$Performance.Tag.y <- as.factor(Decision_df$Performance.Tag.y)

#--------------------------------------------------------------------------------------
# Splitting the data into train and test data sets. 
set.seed(100)

dt_indices = sample.split(Decision_df$Performance.Tag.y, SplitRatio = 0.5)

# Training data set 
dt_train <- SMOTE(form =Performance.Tag.y ~ .,
                  data = Decision_df[dt_indices,],
                  perc.over = 200,perc.under = 200)
summary(dt_train$Performance.Tag.y)

# Test data set
dt_test = Decision_df[!(dt_indices),]
summary(dt_test$Performance.Tag.y)

#----------------------------------------------------------------------------------------    
# BUILDING THE DECISION TREE MODEL
#----------------------------------------------------------------------------------------    
# building a tree with arbitrary minsplit and cp
library(rattle)
library(rpart.plot)

DT_credit_model1 <-  rpart(Performance.Tag.y ~ ., data = dt_train, method= "class", 
                           control = rpart.control(minsplit = 65, cp = 0.001))
rpart.plot(DT_credit_model1)
fancyRpartPlot(DT_credit_model1)


# This is clearly an overfitted tree

#---------------------------------------------------------    
# Increasing the minsplit two fold to 500
DT_credit_model2 <-  rpart(Performance.Tag.y ~., data = dt_train, method= "class",
                           control = rpart.control(minsplit = 500, cp = 0.001))

rpart.plot(DT_credit_model2)
fancyRpartPlot(DT_credit_model2)

# This one is better, but still looks a little too complex
# Listing the variables by importance: Duration, poutcome, month are the top 3
DT_credit_model2$variable.importance

#---------------------------------------------------------    
# We can further simplify the tree by increasing minsplit
DT_credit_model3 <-  rpart(Performance.Tag.y ~., data = dt_train, method= "class",
                           control = rpart.control(minsplit = 800, cp = 0.001))
rpart.plot(DT_credit_model3)
fancyRpartPlot(DT_credit_model3)

# Looking at the vairbles by importance.
DT_credit_model3$variable.importance

#---------------------------------------------------------    
# We can further simplify the tree by increasing minsplit
DT_credit_model4 <-  rpart(Performance.Tag.y ~., data = dt_train, method= "class",
                           control = rpart.control(minsplit = 1500, cp = 0.001))
rpart.plot(DT_credit_model4)
fancyRpartPlot(DT_credit_model4)

# Looking at the vairbles by importance.
DT_credit_model4$variable.importance

#---------------------------------------------------------    
# We can further simplify the tree by increasing minsplit
DT_credit_model5 <-  rpart(Performance.Tag.y ~., data = dt_train, method= "class",
                           control = rpart.control(minsplit = 1800, cp = 0.001))

rpart.plot(DT_credit_model5)
fancyRpartPlot(DT_credit_model5)

# Looking at the vairbles by importance.
DT_credit_model5$variable.importance

#---------------------------------------------------------    
# As we have build a total of 5 decision tree models lets us compare two tree models and 
# see if there is any increase in the performance.
# 1. DT_credit_model4 2. DT_credit_model5

#------------------------------ 1. DT_credit_model4 ------------------------
# DT_credit_model4
DT4_predicted_response <- predict(DT_credit_model4,dt_test, type = "class")

# now we have the predicted values from the decision tree model. 

DT4_predicted_response <- ifelse(DT4_predicted_response == 1,"yes","no")
DT4_actual_response    <- ifelse(dt_test$Performance.Tag.y == 1, "yes", "no")

DT_conf <- caret::confusionMatrix(DT4_predicted_response, DT4_actual_response, 
                                  positive = "yes")

DT4_performance <- c(cutoff = NA ,DT_conf$overall[1],
                     DT_conf$byClass[1],
                     DT_conf$byClass[2])

# Accuracy    Sensitivity   Specificity 
# 0.8268135   0.2394755     0.8526351 
# Accuracy is 0.78%, sensitivity is only 0.38%


# ------------------- 2. DT_credit_model5 ---------------------------
# DT_credit_model5
DT5_predicted_response <- predict(DT_credit_model5,dt_test[-28], type = "class")

# now we have the predicted values from the decision tree model. 

DT5_predicted_response <- ifelse(DT5_predicted_response == 1,"yes","no")
DT5_actual_response    <- ifelse(dt_test$Performance.Tag.y == 1, "yes", "no")

unique(DT5_actual_response)

DT_conf <- caret::confusionMatrix(DT5_predicted_response, DT5_actual_response, 
                                  positive = "yes")

DT5_performance <- c(cutoff = NA ,DT_conf$overall[1],
                     DT_conf$byClass[1],
                     DT_conf$byClass[2])
#---------------------------------------------------------------------------------------------------
# MODEL PERFORMANCE EVALUATION - DECISIOIN TREE
#---------------------------------------------------------------------------------------------------
#--------------------------------- KS- STATISTIC -----------------------------------------------

library(ROCR)

# Now to bring the KS Statistic, we need the pridicted_log to be in the test_lr data set. 
DT5_predicted_response_KS <- ifelse(DT5_predicted_response == "yes",1,0)
DT5_actual_response_KS    <- ifelse(DT5_actual_response == "yes",1,0)

# For the prediction function to work the "predicted_response" and "actual_response"
# should only contain values 1 and 0.
pred_object_test<- prediction(DT5_predicted_response_KS, DT5_actual_response_KS)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
decisiontree_ks_stat <- max(ks_table_test)

#---------------------------------------------------------------------------------------------------
#------------------------- LIFT AND GAIN CHARTS - DECISION TREE-----------------------------------------------
# Loading dplyr package 
library(dplyr)
library(pROC)

# plotting the lift chart
lift <- function(labels , predicted_prob,groups = 10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp = sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp/sum(totalresp)*100,
           Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

DT_Decile = lift(DT5_actual_response_KS, DT5_predicted_response_KS, groups = 10)
DT_Decile

# Plotting the gain and lift chart.
plot_grid(
  ggplot(data = DT_Decile) +
    geom_bar(aes(x = factor(bucket), y = Gain),fill = "blue", stat = "identity") +
    labs(x = "Decile") + ggtitle("Gain Chart"),
  ggplot(data = DT_Decile) +
    geom_bar(aes(x = factor(bucket), y = Cumlift), fill = "yellow", stat = "identity", show.legend = TRUE) +
    labs(x = "Decile") + ggtitle("Lift Chart")) 

#----------------------------------------------------------------------------------------------    
# Gain Chart:
ggplot(DT_Decile, aes(x = bucket ,y = Gain))+
  geom_line(color="blue")+ 
  geom_point()+
  ggtitle("Gain Chart") +
  geom_text(hjust = 0, nudge_x = 0.1,aes(label  = round(Gain,digits = 2) , y  = Gain),size = 3,color="red") 

# Observations: 
# gain for a given model is 55% by the 5th decile
# This means is that if we sort all defauters according to probability,
# then among the top 50% customers of this sorted list,
# We would find 55% of all customers that were likely to get deafaulted

#----------------------------------------------------------------------------------------------    
# Lift Chart:
ggplot(DT_Decile, aes(x = bucket ,y = Cumlift))+ 
  geom_line(color="red") +
  geom_point() +
  ggtitle("Lift Chart")+
  geom_text(hjust = 0, nudge_x = 0.1,aes(label = round(Cumlift,digits = 2), y  = Cumlift),size = 3,color="red") 

# Observations: 
# the model catches 1.56 times more drafulters than
# a random model would have caught


# ROC curve
roc_dt <- roc(DT5_actual_response_KS, DT5_predicted_response_KS)
lines(roc_dt, colour = "blue")
#AUC 
auc(roc_dt) 
#0.5524

#-----------------------------------------------------------------------------------------
# **********  END OF MODEL BUILDING AND EVALUATION OF DECISION TREE *********************  
#----------------------------------------------------------------------------------------


#*********************************************************************************************
# ----------------------------- COMPARISION OF DIFFERENT MODELS ------------------------------


as.data.frame(c("KS_Statistic" ,as.data.frame(randomforest_ks_stat),
                as.data.frame(logistic_regression_ks_stat),
                as.data.frame(decisiontree_ks_stat)))

c(auc(roc_lr), auc(roc_logit), auc(roc_dt))

performance_comparision_matrix <- 
  cbind(as.data.frame(Randomforest_model_performance),
        as.data.frame(logistic_regression_performance),
        as.data.frame(DT5_performance))

performance_comparision_matrix[5,] <- c(randomforest_ks_stat, logistic_regression_ks_stat, decisiontree_ks_stat)
performance_comparision_matrix[6,] <- c(auc(roc_rf), auc(roc_logit), auc(roc_dt))
rownames(performance_comparision_matrix)[5] <- "KS_Statistic"
rownames(performance_comparision_matrix)[6] <- "AUC"

---------------------------------------------------------------------------    
#Decison tree
#Accuarcy    : 0.8645  
#Sensitivity :0.217391 
#Specificity :0.892897
#Sensitivity is lower when compated to  banktree_5, and is also lower compared to Logistic regression

#Random Forest
#cutt off :  0.3069697
#Accuracy : 0.6138979 
#Sensitivity : 0.621118  
#Specificity :  0.6135805 

#Logistic
#cutt off :  0.4158586
#Accuracy : 0.6048012  
#Sensitivity : 0.6397516 
#Specificity :  0.6032647


#---------------------------------------------------------------------------------------------------
#Model Selection Logistic Vs Random forecast  
#---------------------------------------------------------------------------------------------------
#Decsion tree model has been ruled out for this exercise as it has got very low sensitivity

library(pROC)

#ROC curve for Logistic Regression
roc_logit <- roc(actual_response,predicted_response)
plot(roc_logit)
#AUC
auc(roc_logit)  #0.6243

roc_rf <- roc(RF_actual_response, RF_predicted_response)
lines(roc_rf, col = "blue")
#AUC 
auc(roc_rf) #0.6173

#Result:
#Logic regression has slighly better AUC (value of the area under the roc curve) 
#than random forrest i.e 0.6215  Vs 0.6116
#and logic model is a simple model too hence selecting logistic model

#-----------------------------------------------------------------------------------------
# ***********************  END OF MODEL RESULT COMPARISIOINS *********************  
#----------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------
# SCORE CARD
#---------------------------------------------------------------------------------------------------


# Appending the probabilities and response variables to the test data
test_lr$predicted_response <- predicted_response
test_lr$predicted_prob     <- predicted_defalut

head(test_lr)
#test_lr <- test_lr[order(test_lr$predicted_prob, decreasing = T), ]
summary(test_lr$predicted_prob)


# Summary of probality
# Min.    1st Qu. Median  Mean    3rd Qu.    Max. 
# 0.1249  0.2883  0.3675  0.3972  0.4899  0.9228 

boxplot(test_lr$predicted_prob)
quantile(test_lr$predicted_prob,seq(0,1,0.01))

# Need to build a scoring model where higher the score least risky customer
# p = the probability of an default happening
# 1 - p = the probability of an default not happening
# Hence add a column for probability of non defaulters i.e. 1-P

test_lr$Predicted_prob_nondefault <- 1- test_lr$predicted_prob
summary(test_lr$Predicted_prob_nondefault)
boxplot(test_lr$Predicted_prob_nondefault)
# Summary of non_defaulte\success score
#Min.    1st Qu.  Median  Mean    3rd Qu. Max. 
#0.07721 0.51005  0.63250 0.60282 0.71171 0.87512 

#odds = P/1-P
test_lr$odds <-  test_lr$predicted_prob/test_lr$Predicted_prob_nondefault
summary(test_lr$odds)

#Summary of odds
#Min.    1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.1427  0.4051  0.5810  0.7902  0.9606 11.9511 

#log odds = log(P/1-P)
test_lr$log_odds <-  log(test_lr$odds)
summary(test_lr$log_odds)

#Summary of log odds
#Min.      1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.94705 -0.90372 -0.54295 -0.44394 -0.04022  2.48083

boxplot(test_lr$log_odds)

##############################
#use scorecard package

library(data.table)
library(scorecard)

sc_df <- test_lr
bins = woebin(sc_df, "Performance.Tag.y")
dt_woe = woebin_ply(sc_df, bins)

m_step = step(logistic_final, direction="both", trace = FALSE)
m = eval(m_step$call)

card = scorecard(bins, logistic_final,points0 = 400, odds0 = 1/9, pdo = 20, basepoints_eq0 = FALSE) 
score = scorecard_ply(sc_df, card)
summary(score)

#Min.   :347  
#1st Qu.:352  
#Median :360  
#Mean   :357  
#3rd Qu.:360  
#Max.   :361 

# score      
# Min.   :355.0  
# 1st Qu.:359.0  
# Median :366.0  
# Mean   :363.4  
# 3rd Qu.:366.0  
# Max.   :367.0  

#adding score into the dataset
test_lr$score <- score

#create subset of the dataset
score_final <- test_lr[,c("No.of.months.in.current.residence" ,
                          "No.of.months.in.current.company",
                          "No.of.times.30.DPD.or.worse.in.last.6.months",
                          "Avgas.CC.Utilization.in.last.12.months",
                          "Total.No.of.Trades",
                          "Presence.of.open.home.loan",
                          "Presence.of.open.auto.loan",
                          "Performance.Tag.y",
                          "predicted_prob",
                          "Predicted_prob_nondefault",
                          "odds",
                          "log_odds",
                          "score")]

#divide score(found by score card package) by odds to define the score
score_final$final_score1 <- score_final$score/score_final$odds
summary(score_final$final_score1)
boxplot(score_final$final_score1)

#score        
#Min.   : 28.93  
#1st Qu.: 371.67  
#Median : 624.70  
#Mean   : 668.44  
#3rd Qu.: 910.15  
#Max.   : 2536.82  


# using simple formula by using success factor
score_final$final_score2 <- score_final$Predicted_prob_nondefault*1000
summary(score_final$final_score2)
boxplot(score_final$final_score2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 75.94  508.69  633.27  603.53  714.88  875.42

# using another simple formula using offset
Factor = 20 / log (2)
Factor
# 28.8539 
Offset = 400 - Factor * log (10)
Offset
# 333.5614

score_final$final_score3 <- Offset + score_final$Predicted_prob_nondefault *500
summary(score_final$final_score3)
boxplot(score_final$final_score3)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 371.5   587.9   650.2   635.3   691.0   771.3 

cut_off_score <- score_final[score_final$predicted_prob<=0.4158586,]
cut_off_score <- cut_off_score[order(cut_off_score$predicted_prob, decreasing = T), ]

cut_off_score[1,]$score        # 355         
cut_off_score[1,]$final_score1 # 498.7087
cut_off_score[1,]$final_score2 # 584.1673
cut_off_score[1,]$final_score3 # 625.6451

# let us choose the scoring foruma for final_score3 as the score ranges from 350\lower to 800\higher
# Cutoff score is 625.6451 
# if the score is lower than or equal to 625.6436, then client is most likely to get defaulted
# higher the score least risky customer

# Testing score model

# Assign Zeros to all driving variables
# Assumption : Customer is a good customer if all values are Zero
newdata <- data.frame(
                      No.of.months.in.current.residence = 0 ,
                      No.of.months.in.current.company =0 ,
                      No.of.times.30.DPD.or.worse.in.last.6.months =0,
                      Avgas.CC.Utilization.in.last.12.months = 0,
                      Total.No.of.Trades = 0,
                      Presence.of.open.home.loan = 0,
                      Presence.of.open.auto.loan = 0
)

test_prob <- predict(logistic_final, newdata, type = "response") 
test_prob
#0.3139843 

#Apply Score card formula
sucessfactor = 1 - test_prob
sucessfactor

score <- Offset +  sucessfactor * 500
score #676.5693

# Higher the score the better i.e. good customer
# Scorecard
# Low        cut-off         Higher
# <=350      <=625.6451      >=800 
# 676.5693 is greater than cuttoff hence, this customer a good customer

#----------------------------------------------------------------------------------------------    
# checking scoring folumula for the rejected population
#----------------------------------------------------------------------------------------------    

credit_merged_rejected <- credit_merged[is.na(credit_merged$Performance.Tag.y), ]
logistic_final <- Logistic_model_15

predicted_defalut <- predict(logistic_final, newdata = credit_merged_rejected, type = "response")
summary(predicted_defalut)
predicted_defalut <- predicted_defalut[which(!is.na(predicted_defalut))]
predict_non_default <- 1-predicted_defalut
score <- Offset +  predict_non_default *500
summary(score)
boxplot(score)
quantile(score)

(length(predicted_defalut)-sum(score>=625.6451))/(length(predicted_defalut)+35)

# The value 35 in the  denominator is added as there are 35 NA values because of data issues. 
# Therefore, our scoring model work with 96 % accuracy on the rejected population. 
