#라이브러리 준비
library(randomForest)
library(gbm)
library(caret)


#data 준비
data <- read.csv("C:/Users/정수/Documents/학교/4학년1학기/정보디자인/팀플/train/train.csv", header=TRUE)

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

#종속변수도 factor로
data$churn_risk_score <-as.factor(data$churn_risk_score)

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

#트레이닝:테스트 = 7:3
set.seed(2021)
train <- sample(1:nrow(data), round(nrow(data)*0.7))
data.train <- data[train, ]
data.test <- data[-train, ]


#1,2하 3,4 중, 5상로 해보기
data2<-data

data2$churn_risk_score <- gsub("1", "하", data2$churn_risk_score)
data2$churn_risk_score <- gsub("2", "하", data2$churn_risk_score)
data2$churn_risk_score <- gsub("3", "중", data2$churn_risk_score)
data2$churn_risk_score <- gsub("4", "중", data2$churn_risk_score)
data2$churn_risk_score <- gsub("5", "상", data2$churn_risk_score)
#다시 factor로
data2$churn_risk_score <-as.factor(data2$churn_risk_score)
str(data2)

#트레이닝:테스트 = 7:3
set.seed(2021)
train2 <- sample(1:nrow(data2), round(nrow(data2)*0.7))
data2.train <- data2[train2, ]
data2.test <- data2[-train2, ]


###############################################
#적당한 트리값 찾을수도있다. https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=tjdudwo93&logNo=221046218087 참고
#RANDOMFOREST

#randomforest ntree 원래 500이라고 되어있었는데 일단 줄여서 100으로 해봄 크게 해도 별반 차이 없었다.
rf <- randomForest(churn_risk_score ~ ., data=data.train, mtry = floor(sqrt(7)), ntree = 100, importance = T)

rf
#y (종속변수)를 연속형으로(factor안해줬을때) 햇을때 -> 결과 : Mean of squared residuals: 0.1665126, % Var explained: 87.96
#y (종속변수)를 이산형 햇을때 -> 결과 : OOB estimate of  error rate: 21.48% (약 78)

#랜포- y 예측한것.
rf_pred = predict(rf, data.test)
rf_pred
#왜 안될까? - confusionMatrix(rf_pred, data.test)

#변수중요도 - 각 factor(1,2,3,4,5)에 영향을 미치는게 다르게 나옴.
varImp(rf)

#상중하로 해본것
rf2 <- randomForest(churn_risk_score ~ ., data=data2.train, mtry = floor(sqrt(7)), ntree = 100, importance = T)

rf2
#결과 (이산형) : OOB estimate of  error rate: 13.78% (약 86)

rf_pred2 = predict(rf, data2.test)
rf_pred2

#변수중요도 - 각 factor(상, 중, 하)에 영향을 미치는게 다르게 나옴.
varImp(rf2)

########################################
#BOOSTING

#boosting ntrees가 5000이라고 되어있었는데 일단 줄여서 100으로 해봄-> 다항분류의 경우 오류 발생 (손상되었다고 한다..)
#가우시안-연속형, 베르누이-분류(0or1), multinomial-다항분류=> distribution 안정해주면 알아서 해준다

#boosting <-gbm(churn_risk_score ~ .,data=data.train,distribution="multinomial",n.trees=100,interaction.depth=4)
#boosting

#오류가있어서 다른 부스팅 패키지 이용 xgb 과적합방지해주고 변수의 정규화도해줌.
#install.packages("xgboost")
library(xgboost)
library(tidyverse)
library(data.table)

#xgb는 dataframe 이면 안되고 matrix형식이어야한다


#https://stackoverflow.com/questions/36086529/understanding-num-classes-for-xgboost-in-r 참고
y <- data.train$churn_risk_score
num.class = length(levels(y))
levels(y) = 1:num.class
head(y)

#y <- as.matrix(y)
#그냥 하면 오류나서 -1 해주었음
y <- as.numeric(y)-1
df <- data.matrix(data.train)


param <- list("objective" = "multi:softmax",    
              "num_class" = 5,    
              "eval_metric" = "mlogloss",    
              "nthread" = 2,   
              "max_depth" = 2,   
              "eta" = 0.3,    
              "gamma" = 0,    
              "subsample" = 1,   
              "colsample_bytree" = 1,  
              "min_child_weight" = 12)

#어찌저찌 되긴 햇는데 저 뜻이 뭔지 잘 모르겠다. 에러가 줄고있다는 걸까? 잘모르겠다.
#줄어들다가 일정수준에서 멈추거나 정체하면, 학습을 그만해도된다. 그럼 적당한 횟수를 구해서 거의 멈췄을때의
#loss를 구하는 식인 것 같다. https://apple-rbox.tistory.com/6 그래프 참고
boosting <- xgboost(param=param, data=df, label=y, nrounds=50)
#한 20~25 근방에서 멈춰도될것같다. 더 해봤자 많이 줄어들진 않는다.

#예측해본거 -> 전반적으로 -1 해서 나오니까 1 더해줘야함.
df_test<-data.matrix(data.test)
boosting_pred = predict(boosting, df_test)
boosting_pred<- boosting_pred+1


#혼동행렬(맞는건지 잘모르겠다..)
actual_label <- data.test$churn_risk_score#원래값
length(actual_label)
table(boosting_pred,actual_label)

#변수중요도..?? 당연히 종속변수 빼야할거같은데 왜 저렇게 나오는지는 잘모르겠다.
xgb.importance(colnames(df), model = boosting)

xgb.plot.importance(importance_matrix = xgb.importance(colnames(df), model = boosting))



###아래에 이런저런거 해봤는데, 위에서 한게 맞는 방법인 것 같다.###

#변수 중요도 확인 후 뭔가 이상한것 같아서 churn risk제외하고 한번 더 돌려봄
data.train2<- subset(data.train, select=-churn_risk_score)
df2 <- data.matrix(data.train2)

#churnrisk빼면 완전 성능 팍 낮아지는듯.
boosting_2 <- xgboost(param=param, data=df2, label=y, nrounds=20)
#예측이 행 개수가 달라져서 이상해짐.test에도 churn risk빼줘야하는데 그러면 의미가 없어짐..

###상중하로 해보기

y <- data2.train$churn_risk_score
str(y)#상=1 중=2 하=3으로 설정한 모습을 볼 수 있음
View(data2.train)
num.class = length(levels(y))
levels(y) = 1:num.class
head(y)

#y <- as.matrix(y)
#그냥 하면 오류나서 -1 해주었음
y <- as.numeric(y)-1
df2 <- data.matrix(data2.train)

param <- list("objective" = "multi:softmax",    
              "num_class" = 3,    
              "eval_metric" = "mlogloss",    
              "nthread" = 2,   
              "max_depth" = 2,   
              "eta" = 0.3,    
              "gamma" = 0,    
              "subsample" = 1,   
              "colsample_bytree" = 1,  
              "min_child_weight" = 12)

boosting_상중하 <- xgboost(param=param, data=df2, label=y, nrounds=20)


#예측해본거 -> 전반적으로 -1 해서 나오니까 1 더해줘야함.
df_test<-data.matrix(data2.test)
boosting_상중하_pred = predict(boosting_상중하, df_test)
boosting_상중하_pred<-boosting_상중하_pred+1

#혼동행렬1 = 상-1 중-2 하-3
actual_상중하_label <- data2.test$churn_risk_score#원래값
length(actual_label)
table(boosting_상중하_pred,actual_상중하_label)



#배깅
#library(adabag)
#bag <-bagging(churn_risk_score ~ .,data=data.train,mfinal=5,control=rpart.control(maxdepth=5, minsplit=5))
#bag

#bag_pred = predict(bag, data.test)
#bag_pred

#범주형 컬럼 분포 보기
library(tidyverse)
ggplot() +geom_bar(mapping=aes(x=gender), data=data)
ggplot() +geom_bar(mapping=aes(x=region_category), data=data)
ggplot() +geom_bar(mapping=aes(x=membership_category), data=data)
ggplot() +geom_bar(mapping=aes(x=preferred_offer_types), data=data)
ggplot() +geom_bar(mapping=aes(x=medium_of_operation), data=data)
ggplot() +geom_bar(mapping=aes(x=internet_option), data=data)
ggplot() +geom_bar(mapping=aes(x=complaint_status), data=data)
ggplot() +geom_bar(mapping=aes(x=feedback), data=data)
ggplot() +geom_bar(mapping=aes(x=joined_through_referral), data=data)
ggplot() +geom_bar(mapping=aes(x=used_special_discount), data=data)
ggplot() +geom_bar(mapping=aes(x=offer_application_preference), data=data)
ggplot() +geom_bar(mapping=aes(x=past_complaint), data=data)
ggplot() +geom_bar(mapping=aes(x=churn_risk_score), data=data)



#차원축소(주성분 분석/ 요인분석)을 하려면 변수간의 관계를 알아야함. 
#https://rfriend.tistory.com/61
#먼저 다중공선성. 상관도가 높은 변수들을 하나의 주성분 혹은 요인으로 축소하여 모형개발에 활용하기.
#factor들이라서 비교하기가 쉽지가 않은 모습들이 있다.

#상관성분석 -  factor형이기 때문에 크고작음이없어서 불가능한듯.
library(corrplot)
corrplot(data, method="circle")
warnings()


#다중로지스틱
#install.packages("nnet")
library(nnet)
mlogit <- multinom(churn_risk_score ~., data=data.train)
summary(mlogit)

#다중공선성
#결과가 이상하게 나옴. 못하는거같기도하다..
#원래는 선형회귀를 vif안에 넣는다.
#install.packages("car")
library(car)
vif(mlogit)
