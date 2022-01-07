data<- read.csv("C:/Users/정수/Documents/학교/4학년1학기/정보디자인/팀플/train/train.csv", header=TRUE)
View(data)

#data= csv로 불러온 데이터
#churn_risk_score -1 를 1로.
data$churn_risk_score <- gsub(-1 , 1, data$churn_risk_score)

#추천인 id 유무에 따라 결정

data$joined_through_referral[which(data$referral_id == "xxxxxxxx")]<- "No"
data$joined_through_referral[which(data$referral_id != "xxxxxxxx")]<- "Yes"

#처리결과 (-1, ? 모두 0개)
length(which(data$churn_risk_score=="-1"))
length(which(data$joined_through_referral=="?"))
