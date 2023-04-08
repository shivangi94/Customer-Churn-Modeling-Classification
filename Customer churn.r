library(caret)
library(forecast)
library(rpart)
library(rpart.plot)
library(gains)
data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

rownames(data) <- data[,c(1)]

#remove Id
data <- data[,-c(1)]


#remove monthly charges
data <- data[,!colnames(data) %in% c("TotalCharges")]


#some transformations
data$SeniorCitizen <- ifelse(data$SeniorCitizen == 1,"Yes","No")
data$Churn <- ifelse(data$Churn == 'Yes',1,0)


#Data Visualization
#1
library(ggplot2)
library(scales)
ggplot(data, aes(x=data$Churn)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)
#27 percentage churning


#churn based on monthly charges
#2
df <- data.frame( churn=as.factor(data$Churn),
                  monthlycharges=data$MonthlyCharges)
ggplot(df, aes(x=monthlycharges, color=churn, fill=churn)) +
  geom_density(alpha=0.3)


#3 payment analysis
library(dplyr)
df2 <- data %>% 
  group_by(PaperlessBilling) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
ggplot(df2, aes(x="",y=perc, fill=PaperlessBilling))+
  geom_col()+
  coord_polar("y")+ 
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))

df <- data.frame( churn=as.factor(data$Churn),
                  PaperlessBilling=data$PaperlessBilling)
ggplot(df, aes(x= PaperlessBilling,  group=churn)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="PaperlessBilling") +
  facet_grid(~churn) +
  scale_y_continuous(labels = scales::percent)
counts <- table(data$Churn,data$PaperlessBilling)
barplot(counts,
        legend = rownames(counts), beside=TRUE)


df2 <- data %>% 
  group_by(PaymentMethod) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
ggplot(df2, aes(x="",y=perc, fill=PaymentMethod))+
  geom_col()+
  coord_polar("y")+ 
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))
counts <- table(data$Churn,data$PaymentMethod)
barplot(counts,
        legend = rownames(counts), beside=TRUE)

df <- data.frame( churn=as.factor(data$Churn),
                  PaymentMethod=data$PaymentMethod)
ggplot(df, aes(x= PaymentMethod,  group=churn)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="PaymentMethod") +
  facet_grid(~churn) +
  scale_y_continuous(labels = scales::percent)

#4 tenure and churn
tenure_churn_df <- data.frame(churn=as.factor(data$Churn), tenure=data$tenure)
ggplot(tenure_churn_df, aes(x=tenure, color=churn, fill=churn)) + ggtitle("Churn by Tenure") + geom_density(alpha=0.3)




#Modeling

#split
set.seed(1)
numberofrows <- nrow(data)
train.Ind <- sample(numberofrows,0.7*numberofrows)
train.df <- data[c(train.Ind),]
valid.df <- data[-c(train.Ind),]

prop.table(table(data$Churn))

#logistic Regression

colSums(is.na(data))

boxplot(data$MonthlyCharges,
        xlab="MonthlyCharges")

boxplot(data$tenure,
        xlab="Tenure")

#Model 1
log.reg <- glm(Churn~.,data = train.df)
options(scipen=999)
summary(log.reg)


confusionMatrix(table(predict(log.reg, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$Churn == 1),mode = "everything")

#Model 2
log.reg.2 <- glm(Churn~tenure+Contract+MonthlyCharges,data = train.df)
options(scipen=999)
summary(log.reg.2)


confusionMatrix(table(predict(log.reg.2, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$Churn == 1),mode = "everything")


#Model 3
log.reg.3 <- glm(Churn~tenure+Contract+MonthlyCharges+TechSupport+OnlineSecurity,data = train.df)
options(scipen=999)
summary(log.reg.3)


confusionMatrix(table(predict(log.reg.3, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$Churn == 1),mode = "everything")



#decision tree

#Model 1
.ct <- rpart(Churn ~ ., data = train.df, method = "class", cp = 0, maxdepth = 5, minsplit = 20)

printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
ct.pred <- predict(.ct, valid.df, type = "class")
confusionMatrix(ct.pred, as.factor(valid.df$Churn))




#Random Forest
library(randomForest)

rf1 <- randomForest(as.factor(Churn) ~ ., data = train.df, 
                   ntree = 500, mtry = 6, nodesize = 5, importance = TRUE, sampsize = 2000) 

varImpPlot(rf1, type = 1)

valid.df$Churn <- factor(valid.df$Churn)
rf.pred <- predict(rf1, valid.df)
confusionMatrix(rf.pred, valid.df$Churn,mode = "everything")


rf2 <- randomForest(as.factor(Churn) ~ ., data = train.df, 
                   ntree = 500, mtry = 4, nodesize = 5, importance = TRUE, sampsize = 2000) 

varImpPlot(rf2, type = 1)

valid.df$Churn <- factor(valid.df$Churn)
rf.pred <- predict(rf2, valid.df)
confusionMatrix(rf.pred, valid.df$Churn,mode = "everything")

rf3 <- randomForest(as.factor(Churn) ~ tenure+Contract+MonthlyCharges, data = train.df, 
                    ntree = 500, mtry = 4, nodesize = 5, importance = TRUE, sampsize = 2000) 

varImpPlot(rf3, type = 1)

valid.df$Churn <- factor(valid.df$Churn)
rf.pred <- predict(rf3, valid.df)
confusionMatrix(rf.pred, valid.df$Churn,mode = "everything")library(caret)
library(forecast)
library(rpart)
library(rpart.plot)
library(gains)
data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

rownames(data) <- data[,c(1)]

#remove Id
data <- data[,-c(1)]


#remove monthly charges
data <- data[,!colnames(data) %in% c("TotalCharges")]


#some transformations
data$SeniorCitizen <- ifelse(data$SeniorCitizen == 1,"Yes","No")
data$Churn <- ifelse(data$Churn == 'Yes',1,0)


#Data Visualization
#1
library(ggplot2)
library(scales)
ggplot(data, aes(x=data$Churn)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent)
#27 percentage churning


#churn based on monthly charges
#2
df <- data.frame( churn=as.factor(data$Churn),
                  monthlycharges=data$MonthlyCharges)
ggplot(df, aes(x=monthlycharges, color=churn, fill=churn)) +
  geom_density(alpha=0.3)


#3 payment analysis
library(dplyr)
df2 <- data %>% 
  group_by(PaperlessBilling) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
ggplot(df2, aes(x="",y=perc, fill=PaperlessBilling))+
  geom_col()+
  coord_polar("y")+ 
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))

df <- data.frame( churn=as.factor(data$Churn),
                  PaperlessBilling=data$PaperlessBilling)
ggplot(df, aes(x= PaperlessBilling,  group=churn)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="PaperlessBilling") +
  facet_grid(~churn) +
  scale_y_continuous(labels = scales::percent)
counts <- table(data$Churn,data$PaperlessBilling)
barplot(counts,
        legend = rownames(counts), beside=TRUE)


df2 <- data %>% 
  group_by(PaymentMethod) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
ggplot(df2, aes(x="",y=perc, fill=PaymentMethod))+
  geom_col()+
  coord_polar("y")+ 
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))
counts <- table(data$Churn,data$PaymentMethod)
barplot(counts,
        legend = rownames(counts), beside=TRUE)

df <- data.frame( churn=as.factor(data$Churn),
                  PaymentMethod=data$PaymentMethod)
ggplot(df, aes(x= PaymentMethod,  group=churn)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="PaymentMethod") +
  facet_grid(~churn) +
  scale_y_continuous(labels = scales::percent)

#4 tenure and churn
tenure_churn_df <- data.frame(churn=as.factor(data$Churn), tenure=data$tenure)
ggplot(tenure_churn_df, aes(x=tenure, color=churn, fill=churn)) + ggtitle("Churn by Tenure") + geom_density(alpha=0.3)




#Modeling

#split
set.seed(1)
numberofrows <- nrow(data)
train.Ind <- sample(numberofrows,0.7*numberofrows)
train.df <- data[c(train.Ind),]
valid.df <- data[-c(train.Ind),]

prop.table(table(data$Churn))

#logistic Regression

colSums(is.na(data))

boxplot(data$MonthlyCharges,
        xlab="MonthlyCharges")

boxplot(data$tenure,
        xlab="Tenure")

#Model 1
log.reg <- glm(Churn~.,data = train.df)
options(scipen=999)
summary(log.reg)


confusionMatrix(table(predict(log.reg, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$Churn == 1),mode = "everything")

#Model 2
log.reg.2 <- glm(Churn~tenure+Contract+MonthlyCharges,data = train.df)
options(scipen=999)
summary(log.reg.2)


confusionMatrix(table(predict(log.reg.2, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$Churn == 1),mode = "everything")


#Model 3
log.reg.3 <- glm(Churn~tenure+Contract+MonthlyCharges+TechSupport+OnlineSecurity,data = train.df)
options(scipen=999)
summary(log.reg.3)


confusionMatrix(table(predict(log.reg.3, newdata = valid.df, 
                              type="response") >= 0.5, valid.df$Churn == 1),mode = "everything")



#decision tree

#Model 1
.ct <- rpart(Churn ~ ., data = train.df, method = "class", cp = 0, maxdepth = 5, minsplit = 20)

printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
ct.pred <- predict(.ct, valid.df, type = "class")
confusionMatrix(ct.pred, as.factor(valid.df$Churn))




#Random Forest
library(randomForest)

rf1 <- randomForest(as.factor(Churn) ~ ., data = train.df, 
                   ntree = 500, mtry = 6, nodesize = 5, importance = TRUE, sampsize = 2000) 

varImpPlot(rf1, type = 1)

valid.df$Churn <- factor(valid.df$Churn)
rf.pred <- predict(rf1, valid.df)
confusionMatrix(rf.pred, valid.df$Churn,mode = "everything")


rf2 <- randomForest(as.factor(Churn) ~ ., data = train.df, 
                   ntree = 500, mtry = 4, nodesize = 5, importance = TRUE, sampsize = 2000) 

varImpPlot(rf2, type = 1)

valid.df$Churn <- factor(valid.df$Churn)
rf.pred <- predict(rf2, valid.df)
confusionMatrix(rf.pred, valid.df$Churn,mode = "everything")

rf3 <- randomForest(as.factor(Churn) ~ tenure+Contract+MonthlyCharges, data = train.df, 
                    ntree = 500, mtry = 4, nodesize = 5, importance = TRUE, sampsize = 2000) 

varImpPlot(rf3, type = 1)

valid.df$Churn <- factor(valid.df$Churn)
rf.pred <- predict(rf3, valid.df)
confusionMatrix(rf.pred, valid.df$Churn,mode = "everything")