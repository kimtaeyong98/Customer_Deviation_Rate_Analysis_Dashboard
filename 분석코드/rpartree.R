data<-read.csv("data.csv",header = T)

#전처리 전부 삭제#
data<- subset (data, churn_risk_score!=-1)

data<- subset (data, data$gender != "Unknown")

data$joined_through_referral[which(data$referral_id == "xxxxxxxx")]<- "No"
data$joined_through_referral[which(data$referral_id != "xxxxxxxx")]<- "Yes"


data<- subset (data, data$medium_of_operation != "?")


data<- subset (data, data$avg_time_spent >= 0)


data<- subset (data, data$days_since_last_login != -999)

data<- subset (data, data$avg_frequency_login_days != "Error" & data$avg_frequency_login_days >0)

data$points_in_wallet[is.na(data$points_in_wallet)] <- 0  
data<- subset (data, data$points_in_wallet > 0)

data<- subset (data, data$region_category != "")
##################



#가입일수 구하기
data$joining_date <- as.Date(data$joining_date)
data$joining_date <- as.integer(data$joining_date)
a<-as.Date("2017-12-31")
a<-as.integer(a)
data["Days_from"] <- a-data$joining_date
data<- subset (data, select=-joining_date)
#####################



#마지막 로그인으로 지난 시간 구하기
data$last_visit_hour<-0
data$last_visit_minute<-0
data$last_visit_second<-0


visit_split_list<- strsplit(data$last_visit_time, split= ":")

for (i in 1:nrow(data)) {
  data[,"last_visit_hour"][i]<-visit_split_list[[i]][1]
  data[,"last_visit_minute"][i]<-visit_split_list[[i]][2]
  data[,"last_visit_second"][i]<-visit_split_list[[i]][3]
  
}
data$last_visit_hour <-as.numeric(data$last_visit_hour)

data<-subset(data,select= -c(last_visit_time))

data['time_from_last_visit'] <- ((data$days_since_last_login-1)*24 + (24-data$last_visit_hour))
##################



#펙터화
data$gender<-as.factor(data$gender)
data$region_category<-as.factor(data$region_category)
data$membership_category<-as.factor(data$membership_category)
data$joined_through_referral<-as.factor(data$joined_through_referral)
data$preferred_offer_types<-as.factor(data$preferred_offer_types)
data$medium_of_operation <-as.factor(data$medium_of_operation )
data$internet_option <-as.factor(data$internet_option )
data$used_special_discount <-as.factor(data$used_special_discount)
data$offer_application_preference <-as.factor(data$offer_application_preference)
data$past_complaint <-as.factor(data$past_complaint)
data$complaint_status <-as.factor(data$complaint_status)
data$feedback <-as.factor(data$feedback)

#숫자화
data$avg_frequency_login_days<-as.numeric(data$avg_frequency_login_days)

#필요없는 변수 삭제
data<- subset (data, select=-customer_id)
data<- subset (data, select=-Name)
data<- subset (data, select=-security_no)
data<- subset (data, select=-referral_id)
data<- subset (data, select=-last_visit_hour)
data<- subset (data, select=-last_visit_minute)
data<- subset (data, select=-last_visit_second)


str(data)

set.seed(2021)
train1 <- sample(1:nrow(data), round(nrow(data)*0.7))
train <- data[train1, ]
test <- data[-train1, ]

####rpart 의사결정나무
library(rpart)
rpartmod<-rpart(churn_risk_score~.,data=test,method="class")
plot(rpartmod)
text(rpartmod)
#rpart가지치기
printcp(rpartmod)
plotcp(rpartmod)
ptree<-prune(rpartmod, cp=rpartmod$cptable[which.min(rpartmod$cptable["xerror"]),"CP"])
plot(ptree)
text(ptree)
summary(rpartmod)
#중요 변수 points_in_wallet/membership_category/feedback/avg_transaction_value/avg_frequency_login_days
#####성능평가
rpartpred<-predict(rpartmod,test,type="class")
cm<-table(rpartpred, test$churn_risk_score)
cm
(cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5])/sum(cm)
#77.92% 정확도 kappa 70.54
library(caret)
confusionMatrix(rpartpred,as.factor(test$churn_risk_score))

#train에 대해서
rpartmodtr<-rpart(churn_risk_score~.,data=train,method="class")
plot(rpartmodtr)
text(rpartmodtr)
summary(rpartmodtr)
printcp(rpartmodtr)
plotcp(rpartmodtr)
ptree<-prune(rpartmodtr, cp=rpartmodtr$cptable[which.min(rpartmodtr$cptable["
                                                                      xerror"]),"CP"])
rpartpredtrain<-predict(ptree,train,type="class")
cm<-table(rpartpredtrain, train$churn_risk_score)
confusionMatrix(rpartpredtrain,as.factor(train$churn_risk_score))
#78.06%정확도 kappa 70.74

