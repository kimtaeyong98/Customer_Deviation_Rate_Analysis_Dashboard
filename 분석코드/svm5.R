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


library(e1071)
svm_model<-svm(churn_risk_score~.,data=train,type="C-classification")
summary(svm_model)
svm_predict<-predict(svm_model,test)
cm<-table(svm_predict, test$churn_risk_score)
cm
(cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5])/sum(cm)
library(caret)
confusionMatrix(svm_predict,as.factor(test$churn_risk_score))
# 69.4% 적중률 / 상중하 시 89.1% 적중 #support vector는 10254개로 최대 마진 초평면에 가까운 점들을 의미한다.

svm_model2<-svm(churn_risk_score~.,data=train,cost=8,type="C-classification")
summary(svm_model2)
svm_predict2<-predict(svm_model2,test)
cm2<-table(svm_predict2, test$churn_risk_score)
cm2
(cm2[1,1]+cm2[2,2]+cm2[3,3]+cm2[4,4]+cm2[5,5])/sum(cm2)
#cost를 4로 올리니 정확도가 71.7퍼까지 오름
#cost 8 : 72.02%

svm_model3<-svm(churn_risk_score~.,data=train,cost=9,type="C-classification")
summary(svm_model3)
svm_predict3<-predict(svm_model3,test)
cm3<-table(svm_predict3, test$churn_risk_score)
cm3
(cm3[1,1]+cm3[2,2]+cm3[3,3]+cm3[4,4]+cm3[5,5])/sum(cm3)
#cost를 0.1로 낮추니 정확도가 약 61퍼가 나온다.
#cost 10 : 71.8%

svm_model4<-svm(churn_risk_score~.,data=train,cost=16,type="C-classification")
summary(svm_model4)
svm_predict4<-predict(svm_model4,test)
cm4<-table(svm_predict4, test$churn_risk_score)
cm4
(cm4[1,1]+cm4[2,2]+cm4[3,3]+cm4[4,4]+cm4[5,5])/sum(cm4)
#cost가 16일때 71.2퍼센트의 정확도를 나타내었다.
#cost값이 많으면 마진이 넓어진다.

