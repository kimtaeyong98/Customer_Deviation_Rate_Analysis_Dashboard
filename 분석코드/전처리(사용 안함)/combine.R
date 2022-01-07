data<-read.csv("data.csv",header = T)

#######전처리##############

#변수명:churn_risk_score(종속변수)
data$churn_risk_score <- gsub(-1 , 1, data$churn_risk_score)

#변수명:joined_through_referral
data$joined_through_referral[which(data$referral_id == "xxxxxxxx")]<- "No"
data$joined_through_referral[which(data$referral_id != "xxxxxxxx")]<- "Yes"

#변수명:medium_of_operation
data$medium_of_operation[data$medium_of_operation=="?"&data$internet_option=="Wi-Fi"]<-"Both"
data$medium_of_operation[data$medium_of_operation=="?"&data$internet_option=="Mobile_Data"]<-"Smartphone"
data$medium_of_operation[data$medium_of_operation=="?"&data$internet_option=="Fiber_Optic"]<-"Desktop"

#변수명:avg_time_spent
data<-transform(data,avg_time_spent=abs(avg_time_spent))

#변수명:days_since_last_login 
data$days_since_last_login[data$days_since_last_login==-999]<-NA
data$days_since_last_login<-as.numeric(data$days_since_last_login)

#변수명:avg_frequency_login_days 
data$avg_frequency_login_days[data$avg_frequency_login_days=="Error"]<-NA
data$avg_frequency_login_days<-as.numeric(data$avg_frequency_login_days) 
data$avg_frequency_login_days<-abs(data$avg_frequency_login_days)

#변수명:points_in_wallet
data$points_in_wallet[is.na(data$points_in_wallet)] <- 0  
data$points_in_wallet[data$points_in_wallet < 0] <- 0  

#변수명:region_category
data$region_category[data$region_category==""] <- "unknown"


#변수명:joining_date, last_visit_time
data$last_visit_hour<-0
data$last_visit_minute<-0
data$last_visit_second<-0
data$joining_year<-0
data$joining_month<-0
data$joining_day<-0

visit_split_list<- strsplit(data$last_visit_time, split= ":")
join_split_list<- strsplit(data$joining_date, split= "-")

for (i in 1:nrow(data)) {
  data[,"last_visit_hour"][i]<-visit_split_list[[i]][1]
  data[,"last_visit_minute"][i]<-visit_split_list[[i]][2]
  data[,"last_visit_second"][i]<-visit_split_list[[i]][3]
  data[,"joining_year"][i]<-join_split_list[[i]][1]
  data[,"joining_month"][i]<-join_split_list[[i]][2]
  data[,"joining_day"][i]<-join_split_list[[i]][3]
}

data<-subset(data,select= -c(last_visit_time,joining_date))

##########################







