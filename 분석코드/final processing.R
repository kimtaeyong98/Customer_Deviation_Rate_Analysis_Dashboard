#전처리 전부 삭제#
setwd("C:\\data")
data<-read.csv("data.csv",header = T)
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

#범주형 데이터 펙터화
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

#종속변수인 컬럼을 factor로 변환
data$churn_risk_score <- factor(data$churn_risk_score,
                                levels = c("gender, region_category, membership_category, joined_through_referral, 
                                preferred_offer_types, medium_of_operation, internet_option, used_special_discount, 
                offer_application_preference, past_complaint, complaint_status, feedback"),
                                labels = c("gender, region_category, membership_category, joined_through_referral, 
                                preferred_offer_types, medium_of_operation, internet_option, used_special_discount, 
                offer_application_preference, past_complaint, complaint_status, feedback"))

#데이터를 shuffle
set.seed(2021)
data_shuffle <- data[sample(nrow(data)), ]

#train데이터와 test데이터로 나눈다.
train1 <- sample(1:nrow(data), round(nrow(data)*0.7))
train <- data[train1, ]
test <- data[-train1, ]

#수치형 데이터 정규화
data_min <- min(data$age)
data_max <- max(data$age)
data$age <- scale(data$age, center=data_min, scale=data_max-data_min)
#View(data[,c("age")])

data_min <- min(data$age)
data_max <- max(data$age)
data$age <- scale(data$age, center=data_min, scale=data_max-data_min)

data_min <- min(data$days_since_last_login)
data_max <- max(data$days_since_last_login)
data$days_since_last_login <- scale(data$days_since_last_login, center=data_min, scale=data_max-data_min)

data_min <- min(data$avg_time_spent)
data_max <- max(data$avg_time_spent)
data$avg_time_spent <- scale(data$avg_time_spent, center=data_min, scale=data_max-data_min)

data_min <- min(data$avg_transaction_value)
data_max <- max(data$avg_transaction_value)
data$avg_transaction_value <- scale(data$avg_transaction_value, center=data_min, scale=data_max-data_min)

data_min <- min(data$avg_frequency_login_days)
data_max <- max(data$avg_frequency_login_days)
data$avg_frequency_login_days <- scale(data$avg_frequency_login_days, center=data_min, scale=data_max-data_min)

data_min <- min(data$points_in_wallet)
data_max <- max(data$points_in_wallet)
data$points_in_wallet <- scale(data$points_in_wallet, center=data_min, scale=data_max-data_min)

data_min <- min(data$Days_from)
data_max <- max(data$Days_from)
data$Days_from <- scale(data$Days_from, center=data_min, scale=data_max-data_min)

data_min <- min(data$time_from_last_visit)
data_max <- max(data$time_from_last_visit)
data$time_from_last_visit <- scale(data$time_from_last_visit, center=data_min, scale=data_max-data_min)
View(data)

nolmalize <- function(x) {
  return( (x-min(x))/(max(x)-min(x)) )
}

ncol <- which(colnames(data) == "churn_risk_score")
#여기서부터 오류가 뜸.. ( 정규화 문제 )
# data_n <- as.data.frame(lapply(data[,1:20], nolmalize))

#label과 데이터를 나눈다.
#train_label <- data[1:train1,1]
#test_label <- data[(train1+1):nrow(data),1]

#install.packages("caret")
#install.packages("e1071")

library(caret)
library(e1071)

# repeats가 높아지면 어떠한 max(k)에 수렴함.
repeats = 10
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats =repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

#model1 <- train(data$churn_risk_score~. , data = train[,c("data$churn_risk_score", data$age)], method = "knn",
#               preProcess = c("center", "scale"),
#              trControl = x,
#             metric = "ROC",
#            tuneLength = tunel)

#k_n <- max(model1$bestTune)

ncol(data)

# training데이터로 knn모델을 훈련
library(class)
#result1 <- knn(train=table_train, test=table_test, cl=table_train_label, k=k_n)

# 모델 정확도 확인
#prop.table(table(ifelse(table1[(mm+1):nrow(table_n), col1]==result1,"o","x")))

# 이원교차표 그려서 확인           
library(gmodels)        
#CrossTable( x = table_test_label, y = result1, prop.chisq=False )
