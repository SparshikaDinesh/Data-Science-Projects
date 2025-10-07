# Load the data
data <- read.csv("C:/Users/Dell/Downloads/BankChurners (1).csv")

# Remove unwanted columns
data <- data[ , !(names(data) %in% c("CLIENTNUM", 
                                     "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1", 
                                     "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2"))]

# Convert specified columns to factors
data$Gender <- as.factor(data$Gender)
data$Attrition_Flag <- as.factor(data$Attrition_Flag)
data$Education_Level <- as.factor(data$Education_Level)
data$Marital_Status <- as.factor(data$Marital_Status)
data$Income_Category <- as.factor(data$Income_Category)
data$Card_Category <- as.factor(data$Card_Category)

# Identify numeric columns for outlier removal
numeric_columns <- sapply(data, is.numeric)

# Boxplots before outlier removal
par(mfrow=c(1,1))
boxplot(data[, numeric_columns], main="Boxplots of Numeric Columns Before Outlier Removal", las=2)

# Remove outliers based on 1.5 IQR rule for each numeric column
for (col in names(data[, numeric_columns])) {
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound, ]
}

# Boxplots after outlier removal
boxplot(data[, numeric_columns], main="Boxplots of Numeric Columns After Outlier Removal", las=2)
################################################################################

nrow(data)



set.seed(123)
partition <- sample(2,nrow(data), replace = TRUE,prob =c(0.80, 0.20))
head(partition)

train_data <- data[partition ==1,]
test_data <- data[partition ==2,]

dim(train_data)
dim(test_data)

############################################################################################################

model <- lm(Total_Trans_Amt ~ ., data = train_data)

# Summarize the model
summary(model)

#####################################################################################################################################

library("MASS")

stepb <- stepAIC(model, direction = "backward")

summary(stepb)

step$anova

model_empty <- lm(Total_Trans_Amt ~1, data =train_data)
stepf <- stepAIC(model_empty, direction ="forward", scope = list(upper=model, lower = model_empty))
summary(stepf)
###################################################################################


model_FO <- lm(Total_Trans_Amt ~ Attrition_Flag + Customer_Age + 
                 Dependent_count + Marital_Status + Card_Category + Total_Relationship_Count + 
                 Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal + 
                 Total_Amt_Chng_Q4_Q1 + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, 
               data = train_data)

summary(model_FO)

prediction_FO <- predict(model_FO,test_data)
head(prediction_FO)

actual <- test_data$Total_Trans_Amt
head(actual)

cor(prediction_FO,actual)
plot(model_FO)

#############################################################################################

model_FO_interaction <- lm(Total_Trans_Amt ~ (Attrition_Flag + Customer_Age + 
                                                Dependent_count + Marital_Status + Card_Category + Total_Relationship_Count + 
                                                Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal + 
                                                Total_Amt_Chng_Q4_Q1 + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1)^2, 
                           data = train_data)

summary(model_FO_interaction)
alias(model_FO_interaction)
summary(model_FO_interaction)
plot(model_FO_interaction)
####################################################################################


model_SO <- lm(Total_Trans_Amt ~ poly(Customer_Age, 2) + poly(Dependent_count, 2) + 
                 poly(Total_Relationship_Count, 2) + poly(Contacts_Count_12_mon, 2) + 
                 poly(Credit_Limit, 2) + poly(Total_Revolving_Bal, 2) + 
                 poly(Total_Amt_Chng_Q4_Q1, 2) + poly(Total_Trans_Ct, 2) + 
                 poly(Total_Ct_Chng_Q4_Q1, 2) + Attrition_Flag + Marital_Status + 
                 Card_Category, 
               data = train_data)

summary(model_SO)
plot(model_SO)
#########################################################################################################

model_SO_interaction <- lm(Total_Trans_Amt ~ (poly(Customer_Age, 2) + poly(Dependent_count, 2) + 
                                                poly(Total_Relationship_Count, 2) + poly(Contacts_Count_12_mon, 2) + 
                                                poly(Credit_Limit, 2) + poly(Total_Revolving_Bal, 2) + 
                                                poly(Total_Amt_Chng_Q4_Q1, 2) + poly(Total_Trans_Ct, 2) + 
                                                poly(Total_Ct_Chng_Q4_Q1, 2) + Attrition_Flag + Marital_Status + 
                                                Card_Category)^2, 
                           data = train_data)


summary(model_SO_interaction)

plot(model_SO_interaction)

###############################################################################################################


model_FO_l <- lm(log(Total_Trans_Amt) ~ Attrition_Flag + Customer_Age + 
                 Dependent_count + Marital_Status + Card_Category + Total_Relationship_Count + 
                 Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal + 
                 Total_Amt_Chng_Q4_Q1 + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1, 
               data = train_data)

summary(model_FO_l)

plot(model_FO_l)

vif(model_FO_l)

prediction_FO_l <- predict(model_FO_l,test_data)
head(prediction_FO_l)

actual <- test_data$Total_Trans_Amt
head(actual)

cor(prediction_FO_l,actual)
library(car)
vif(model_FO_l)


#############################################################################################

model_FO_interaction_l <- lm(log(Total_Trans_Amt) ~ (Attrition_Flag + Customer_Age + 
                                                Dependent_count + Marital_Status + Card_Category + Total_Relationship_Count + 
                                                Contacts_Count_12_mon + Credit_Limit + Total_Revolving_Bal + 
                                                Total_Amt_Chng_Q4_Q1 + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1)^2, 
                           data = train_data)

summary(model_FO_interaction_l)
plot(model_FO_interaction_l)

prediction_FO_interaction_l <- predict(model_FO_interaction_l,test_data)
head(prediction_FO_l)

actual <- test_data$Total_Trans_Amt
head(actual)

cor(prediction_FO_interaction_l,actual)
library(car)
vif(model_FO_l)
####################################################################################


model_SO_l <- lm(log(Total_Trans_Amt) ~ poly(Customer_Age, 2) + poly(Dependent_count, 2) + 
                 poly(Total_Relationship_Count, 2) + poly(Contacts_Count_12_mon, 2) + 
                 poly(Credit_Limit, 2) + poly(Total_Revolving_Bal, 2) + 
                 poly(Total_Amt_Chng_Q4_Q1, 2) + poly(Total_Trans_Ct, 2) + 
                 poly(Total_Ct_Chng_Q4_Q1, 2) + Attrition_Flag + Marital_Status + 
                 Card_Category, 
               data = train_data)

summary(model_SO_l)
plot(model_SO_l)
#########################################################################################################

model_SO_interaction_l <- lm(log(Total_Trans_Amt) ~ (poly(Customer_Age, 2) + poly(Dependent_count, 2) + 
                                                poly(Total_Relationship_Count, 2) + poly(Contacts_Count_12_mon, 2) + 
                                                poly(Credit_Limit, 2) + poly(Total_Revolving_Bal, 2) + 
                                                poly(Total_Amt_Chng_Q4_Q1, 2) + poly(Total_Trans_Ct, 2) + 
                                                poly(Total_Ct_Chng_Q4_Q1, 2) + Attrition_Flag + Marital_Status + 
                                                Card_Category)^2, 
                           data = train_data)


summary(model_SO_interaction_l)

plot(model_SO_interaction_l)

















