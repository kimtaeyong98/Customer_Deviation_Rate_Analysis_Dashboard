data<-read.csv("data.csv",header = T)

#변수명:days_since_last_login 
data$days_since_last_login[data$days_since_last_login==-999]<-NA
data$days_since_last_login<-as.numeric(data$days_since_last_login)

#변수명:avg_frequency_login_days 
data$avg_frequency_login_days[data$avg_frequency_login_days=="Error"]<-NA
data$avg_frequency_login_days<-as.numeric(data$avg_frequency_login_days) 
data$avg_frequency_login_days<-abs(data$avg_frequency_login_days)