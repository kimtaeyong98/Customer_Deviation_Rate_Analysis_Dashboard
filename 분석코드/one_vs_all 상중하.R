library("glmnet")


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
data<- subset (data, select=-days_since_last_login)


data$churn_risk_score <- ifelse(data$churn_risk_score ==1 | data$churn_risk_score==2 , 1, 
                          ifelse(data$churn_risk_score ==3 , 2, 3))


str(data)

set.seed(2021)
train1 <- sample(1:nrow(data), round(nrow(data)*0.7))
train <- data[train1, ]
test <- data[-train1, ]

#################
#MODEL1
train_copy<-train
train_copy$churn_risk_score <- ifelse(train_copy$churn_risk_score ==1, 1, 0)

x<-model.matrix(churn_risk_score~.,train_copy)[,-1]
y<-train_copy$churn_risk_score

set.seed(123) 
model1 <- glmnet(x, y, alpha = 1, family = "binomial",
                 nlambda = 100)

fit.lasso.cv <- cv.glmnet(x, y, nfold=10, alpha=1, lambda=model1$lambda) 
fit.lasso.param <- fit.lasso.cv$lambda.min
model1 <- glmnet(x,y,alpha = 1,lambda =fit.lasso.param)

#MODEL2
train_copy<-train
train_copy$churn_risk_score <- ifelse(train_copy$churn_risk_score ==2, 1, 0)

x<-model.matrix(churn_risk_score~.,train_copy)[,-1]
y<-train_copy$churn_risk_score

set.seed(123) 
model2 <- glmnet(x, y, alpha = 1, family = "binomial",
                 nlambda = 100)

fit.lasso.cv <- cv.glmnet(x, y, nfold=10, alpha=1, lambda=model2$lambda) 
fit.lasso.param <- fit.lasso.cv$lambda.min
model2 <- glmnet(x,y,alpha = 1,lambda =fit.lasso.param )



#MODEL3
train_copy<-train
train_copy$churn_risk_score <- ifelse(train_copy$churn_risk_score ==3, 1, 0)

x<-model.matrix(churn_risk_score~.,train_copy)[,-1]
y<-train_copy$churn_risk_score

set.seed(123) 
model3 <- glmnet(x, y, alpha = 1, family = "binomial",
                 nlambda = 100)

fit.lasso.cv <- cv.glmnet(x, y, nfold=10, alpha=1, lambda=model3$lambda) 
fit.lasso.param <- fit.lasso.cv$lambda.min
model3 <- glmnet(x,y,alpha = 1,lambda =fit.lasso.param )


#Predict
x.test <- model.matrix(churn_risk_score ~., test)[,-1]
prob1 <- predict(model1 ,newx = x.test,type="response")

x.test <- model.matrix(churn_risk_score ~., test)[,-1]
prob2 <- predict(model2 ,newx = x.test,type="response")

x.test <- model.matrix(churn_risk_score ~., test)[,-1]
prob3 <- predict(model3 ,newx = x.test,type="response")



prob_result<-data.frame(prob1,prob2,prob3)
prob_result['result']<- names(prob_result)[1:3][max.col(prob_result[1:3], "first")]
prob_result$result<-ifelse(prob_result$result=="s0",1,ifelse(prob_result$result=="s0.1",2
                   ,3))


cm <- table(pred=prob_result$result, actual=test$churn_risk_score)
cm

prob_result["actual"]<-test$churn_risk_score
length(which(prob_result$result != prob_result$actual))

coef(model1)
coef(model2)
coef(model3)


