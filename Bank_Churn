library(tidyverse)
library(dplyr)
library(scales)
library(data.table)
library(caTools)
library(corrplot)
library(nnet)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)	
library(ggcorrplot)
library(caret)
library(DMwR)
library(pROC)

set.seed(2020)

setwd('C:/Users/hui er/OneDrive/Desktop/BC2406/group proj')

data <- fread("Churn_Modelling.csv", stringsAsFactors = T)
churn <- data[,4:14]

#Cleaning =======================================================
# Factorise the variables =======================================
churn$HasCrCard <- factor(churn$HasCrCard)
churn$Tenure <- factor(churn$Tenure)
churn$NumOfProducts <- factor(churn$NumOfProducts)
churn$IsActiveMember <- factor(churn$IsActiveMember)
churn$Exited <- factor(churn$Exited)
levels(churn$Tenure)
levels(churn$Exited) # Baseline is churn = "No"
# Check duplicates & NA & NULL ====================================
sum(duplicated(churn))
sapply(churn, function(x) sum(is.na(x)))
sapply(churn, function(x) sum(is.null(x)))

# Removing NA and replacing blank figures ============================================
churn[CreditScore=="",CreditScore:= mean(CreditScore)]
churn[Geography=="",Geography:= "Missing"]
churn[Gender=="",Gender:= "Missing"]
churn[Age=="",Age:= mean(Age)]
churn[Tenure == "", Tenure := as.integer(mean(churn$Tenure)) ] #okay to have 0, customer can be here only a few months
churn[HasCrCard=="",HasCrCard:=0]
churn[IsActiveMember=="",IsActiveMember:=0]
churn[Exited=="",Exited:=0]
churn=na.omit(churn)

#Monthly France minWage - 1539.4 Euros
#Monthly Germany minWage - 1584 Euros
#Monthly Spain minwage - 1108.3 Euros

churn <- churn[(Geography == 'Germany' & EstimatedSalary >= 1584*12)|
                       (Geography == 'France' & EstimatedSalary >= 1539.4*12)|
                       (Geography == 'Spain' & EstimatedSalary >= 1108.3*12)]

summary(churn)

write.csv(churn, "cleaned_churn.csv", row.names = FALSE)

# Data Exploration =========================================
# Correlation plot with continuous variable (determine mutlicollinearity) ====================
my_data <- churn[, c(1,4,6,10)]
head(my_data, 6)
res <- cor(my_data)
corr<-cor(my_data, use = "pairwise.complete.obs")
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE,
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlation Between Continuous Variables", 
           ggtheme=theme_bw)
# Continuous Variable Exploration ===========================
# overall plot for customer exit
ggplot(churn, aes(Exited, fill=Exited)) +
  geom_bar() +
  labs(title = "Churn rate by customers")+
  geom_text(stat='count', aes(label=percent(..count../sum(..count..))), vjust=4)+
  theme(plot.title = element_text(hjust = 0.5))

# overall plot for age
ggplot(churn, aes(Age, fill = Age)) +
  geom_histogram(color="darkblue", fill="lightblue")+ 
  labs(title = "Distribution of Age") +
  theme(plot.title = element_text(hjust = 0.5))

# overall plot for Balance
ggplot(churn, aes(Balance, fill = Balance)) +
  geom_histogram(color="black", fill="lightgreen") + 
  labs(title = "Distribution of Balance")+
  theme(plot.title = element_text(hjust = 0.5))

# overall plot for CreditScore
ggplot(churn, aes(CreditScore, fill = CreditScore)) +
  geom_histogram(color="black", fill="pink") + 
  labs(title = "Distribution of CreditScore")+
  theme(plot.title = element_text(hjust = 0.5))

# overall plot for EstimatedSalary
ggplot(churn, aes(EstimatedSalary, fill = EstimatedSalary)) +
  geom_histogram(color="black", fill="lightcyan") + 
  labs(title = "Distribution of EstimatedSalary")+
  theme(plot.title = element_text(hjust = 0.5))

# Histogram (Age vs Exited)
ggplot(churn, aes(x = Age, fill = Exited)) +
  geom_histogram(binwidth = 5) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0,100,by=10))+ 
  labs(title = "Customer Churn by Age")+
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot (Age vs Exited)
# Mostly between ages 40 to 50 who churned the most.
ggplot(churn, aes(x = Exited, y = Age, fill = Exited)) +
  geom_boxplot() + 
  theme_minimal() +
  labs(title = "Customer Churn by Age")+
  theme(legend.position = 'top') +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram (Balance vs Exited)
# May not be a useful factor
ggplot(churn, aes(x = Balance, fill = Exited)) +
  geom_histogram(bins=15) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0,255000,by=30000), labels = comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(title = "Customer Churn by Balance") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram (CreditScore vs Exited)
# Lower credit scores more likely to churn.
ggplot(churn, aes(x = CreditScore, fill = Exited)) +
  geom_histogram(bins=30) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(title = "Customer Churn by CreditScore") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram (Estimated Salary vs Exited)
# May not be a useful factor.
ggplot(churn, aes(x = EstimatedSalary, fill = Exited)) +
  geom_histogram(bins=30) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+ 
  labs(title = "Customer Churn by EstimatedSalary") +
  theme(plot.title = element_text(hjust = 0.5))

# Categorical Variable Exploration ===============================================
#overall plot for Gender
ggplot(churn, aes(Gender, fill = Gender)) +
  geom_bar(color="darkblue", fill="lightblue") +
  ggtitle("Distribution of Gender")+
  theme(plot.title = element_text(hjust = 0.5))+  coord_flip()

#overall plot for Geography
ggplot(churn, aes(Geography, fill = Geography)) +
  geom_bar(color="darkblue", fill="lightblue") +
  ggtitle("Distribution of Geography")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

#overall plot for HasCrCard
ggplot(churn, aes(HasCrCard, fill = HasCrCard)) +
  geom_bar(color="darkblue", fill="lightblue") +
  ggtitle("Distribution of HasCrCard")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

#overall plot for IsActiveMember
ggplot(churn, aes(IsActiveMember, fill = IsActiveMember)) +
  geom_bar(color="darkblue", fill="lightblue") +
  ggtitle("Distribution of IsActiveMember")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

#overall plot for NumOfProducts
ggplot(churn, aes(NumOfProducts, fill = NumOfProducts)) +
  geom_bar(color="darkblue", fill="lightblue") +
  ggtitle("Distribution of NumOfProducts")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

# overall plot for Tenure
ggplot(churn, aes(Tenure, fill = Tenure)) +
  geom_bar(color="darkblue", fill="lightblue") +
  ggtitle("Distribution of Tenure")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

# Proportional Histogram (Gender vs Exited)
# Female more likely to churn.
ggplot(churn, aes(x = factor(Gender), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited), 
                  label = scales::percent(prop.table(stat(count))))) +
    ggtitle("Proportion of Customer Churn by Gender")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_bar(position = 'fill') + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = 'Gender', y = 'Percentage', fill = 'Exited')

# Absolute value Histogram (Gender vs Exited)
ggplot(churn, aes(x = factor(Gender), 
                 y = prop.table(stat(count)), 
                 fill = factor(Exited)),
                 label = scales::percent(((..count..)/sum(..count..))))+
  ggtitle("Customer Churn by Gender")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position="dodge")+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
                   vjust=2.5,size=5,position = position_dodge(width = .9))+
  scale_y_continuous(labels=scales::percent) +
  labs(x = 'Gender', y = 'Percentage', fill = 'Exited')


# Proportional Histogram (Geography vs Exited)
# Customer in Germany most likely to churn.
ggplot(churn, aes(x = factor(Geography), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited), 
                  label = scales::percent(prop.table(stat(count))))) +
  ggtitle("Proportion of Customer Churn by Geography")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Geography', y = 'Percentage', fill = 'Exited')

# Absolute value Histogram (Geography vs Exited)
ggplot(churn, aes(x = factor(Geography), 
                    y = prop.table(stat(count)), 
                    fill = factor(Exited)),
                    label = scales::percent(((..count..)/sum(..count..))))+
  ggtitle("Customer Churn by Geography")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position="dodge")+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count",
                    vjust=2,size=5,position = position_dodge(width = .9))+
  scale_y_continuous(labels=scales::percent) +
  labs(x = 'Geography', y = 'Percentage', fill = 'Exited')


# Proportional Histogram (Tenure vs Exited) 
# May not be a useful factor.
ggplot(churn, aes(x = factor(Tenure), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited), 
                  label = scales::percent(prop.table(stat(count)))))+
  ggtitle("Proportion of Customer Churn by Tenure")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Tenure', y = 'Percentage', fill = 'Exited')

#Absolute value Histogram (Tenure vs Exited)
ggplot(churn, aes(x = factor(Tenure), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited)),
                  label = scales::percent(((..count..)/sum(..count..))))+
  ggtitle("Customer Churn by Tenure")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position="dodge")+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..),accuracy = 0.01),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust=-0.5,hjust=0.4,size=3.5,position = position_dodge(width = .9))+
  scale_y_continuous(labels=scales::percent) +
  labs(x = 'Tenure', y = 'Percentage', fill = 'Exited')


# Proportional Histogram (HasCrCard vs Exited)
# May not be a useful factor.
ggplot(churn, aes(x = factor(HasCrCard), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited), 
                  label = scales::percent(prop.table(stat(count)))))+
  ggtitle("Proportion of Customer Churn by HasCrCard")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'HasCrCard', y = 'Percentage', fill = 'Exited')

#Absolute value Histogram (HasCrCard vs Exited)
ggplot(churn, aes(x = factor(HasCrCard), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited)),
       label = scales::percent(((..count..)/sum(..count..))))+
  ggtitle("Customer Churn by HasCrCard")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position="dodge")+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..),accuracy = 0.01),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust=2,size=5,position = position_dodge(width = .9))+
  scale_y_continuous(labels=scales::percent) +
  labs(x = 'HasCrCard', y = 'Percentage', fill = 'Exited')

# Proportional Histogram (IsActiveMember vs Exited)
# Inactive customer more likely to churn.
ggplot(churn, aes(x = factor(IsActiveMember), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited), 
                  label = scales::percent(prop.table(stat(count)))))+
  ggtitle("Proportion of Customer Churn by IsActiveMember")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'IsActiveMember', y = 'Percentage',  fill = 'Exited')

# Absolute value Histogram (IsActiveMember vs Exited)
ggplot(churn, aes(x = factor(IsActiveMember), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited)),
       label = scales::percent(((..count..)/sum(..count..))))+
  ggtitle("Customer Churn by IsActiveMember")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position="dodge")+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..),accuracy = 0.01),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust=2,size=5,position = position_dodge(width = .9))+
  scale_y_continuous(labels=scales::percent) +
  labs(x = 'IsActiveMember', y = 'Percentage', fill = 'Exited')


# Proportional Histogram (NumOfProducts vs Exited)
# Customer who use 3/4 product are most likely to churn.
ggplot(churn, aes(x = factor(NumOfProducts), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited), 
                  label = scales::percent(prop.table(stat(count)))))+
  ggtitle("Proportion of Customer Churn by NumOfProducts")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(position = 'fill')+ 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'NumOfProducts', y = 'Percentage', fill = 'Exited')

# Absolute value Histogram (NumOfProducts vs Exited)
ggplot(churn, aes(x = factor(NumOfProducts), 
                  y = prop.table(stat(count)), 
                  fill = factor(Exited)),
       label = scales::percent(((..count..)/sum(..count..))))+
  ggtitle("Customer Churn by NumOfProducts")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(aes(y = (..count..)/sum(..count..)),position="dodge")+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..),accuracy = 0.01),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust=-0.5,size=4,position = position_dodge(width = .9))+
  scale_y_continuous(labels=scales::percent) +
  labs(x = 'NumOfProducts', y = 'Percentage', fill = 'Exited')

# SMOTE BALANCING DATA & Train-test split =========================================================
round(prop.table(table(churn$Exited)),3)

churn <- SMOTE(Exited ~ ., churn, perc.over = 100, perc.under = 200)
round(prop.table(table(dplyr::select(churn, Exited), exclude = NULL)),4)

train <- sample.split(Y = churn$Exited, SplitRatio = 0.7)
trainset <- subset(churn, train == T)
testset <- subset(churn, train == F)

# m1 Logistic Regression with everything =======================================

m1 <- glm(Exited ~ . , family = binomial, data = trainset)
summary(m1)    

OR <- exp(coef(m1))
OR

OR.CI <- exp(confint(m1))
OR.CI

# Confusion Matrix on Trainset
threshold1 <- 0.5
prob.train.m1<- predict(m1, type = 'response')
m1.predict.train <- ifelse(prob.train.m1 > threshold1, "1", "0")
table1 <- table(Trainset.Actual = trainset$Exited, m1.predict.train, deparse.level = 2)
table1
round(prop.table(table1),3)
# Overall Accuracy
mean(m1.predict.train == trainset$Exited)

# Confusion Matrix on Testset
prob.test.m1 <- predict(m1, newdata = testset, type = 'response')
m1.predict.test <- ifelse(prob.test.m1 > threshold1, "1", "0")
table2 <- table(Testset.Actual = testset$Exited, m1.predict.test, deparse.level = 2)
table2
round(prop.table(table2), 3)
# Overall Accuracy
mean(m1.predict.test == testset$Exited)

# m2 Logistic Regression except those irrelevant =======================================
m2 <- glm(Exited ~ . -CreditScore -EstimatedSalary, family = binomial, data = trainset)

summary(m2)

OR <- exp(coef(m2))
OR

OR.CI <- exp(confint(m2))
OR.CI

# Confusion Matrix on Trainset
threshold1 <- 0.5
prob.train.m2 <- predict(m2, type = 'response')
m2.predict.train <- ifelse(prob.train.m2 > threshold1, "1", "0")
table3 <- table(Trainset.Actual = trainset$Exited, m2.predict.train, deparse.level = 2)
table3
round(prop.table(table3),3)
# Overall Accuracy
mean(m2.predict.train == trainset$Exited)

# Confusion Matrix on Testset
prob.test.m2 <- predict(m2, newdata = testset, type = 'response')
m2.predict.test <- ifelse(prob.test.m2 > threshold1, "1", "0")
table4 <- table(Testset.Actual = testset$Exited, m2.predict.test, deparse.level = 2)
table4
round(prop.table(table4), 3)

sensitivity(table4)
specificity(table4)


# Overall Accuracy
mean(m2.predict.test == testset$Exited)

# Determine Multicollinearity =======================================
install.packages("car")
library(car)
vif(m1) #full logistic regression
vif(m2) #reduced logistic regression

# CART =================================================================================
cartmodel <- rpart(Exited ~ ., method = 'class', cp = 0, data = trainset)

prp(cartmodel, nn=T)

printcp(cartmodel)
plotcp(cartmodel)

CVerror.cap <- cartmodel$cptable[which.min(cartmodel$cptable[,"xerror"]), "xerror"] + cartmodel$cptable[which.min(cartmodel$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart model.
i <- 1; j<- 4
while (cartmodel$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
i
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cartmodel$cptable[i,1] * cartmodel$cptable[i-1,1]), 1)

prunedcart <- prune(cartmodel, cp = cp.opt)
prunedcart$variable.importance

scaledVarImpt <- round(100*prunedcart$variable.importance/sum(prunedcart$variable.importance))
scaledVarImpt[scaledVarImpt > 1]  # Print all var impt > cutoff

print(prunedcart)

# Plot final tree
prp(prunedcart, type=2, extra=104, nn=T, fallen.leaves=T, branch.lty=3, nn.box.col = 'light blue', 
    min.auto.cex = 0.5, nn.cex = 0.6, split.cex = 1.1, shadow.col="grey")

# Confusion Matrix on Trainset
predicted2 <- predict(prunedcart, newdata = trainset, type='class')
table5 <- table(trainset$Exited, predicted2)
table5
round(prop.table(table5), 3)
# Overall Accuracy
mean(predicted2 == trainset$Exited)

# Confusion Matrix on Testset
predicted3 <- predict(prunedcart, newdata = testset, type='class')
table6 <- table(testset$Exited, predicted3)
table6
round(prop.table(table6), 3)

sensitivity(table6)
specificity(table6)

# Overall Accuracy
mean(predicted3 == testset$Exited)


