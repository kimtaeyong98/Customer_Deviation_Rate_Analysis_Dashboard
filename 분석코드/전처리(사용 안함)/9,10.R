lin <- read.csv(file="train.csv")

lin$last_visit_hour<-0
lin$last_visit_minute<-0
lin$last_visit_second<-0
lin$joining_year<-0
lin$joining_month<-0
lin$joining_day<-0

visit_split_list<- strsplit(lin$last_visit_time, split= ":")
join_split_list<- strsplit(lin$joining_date, split= "-")

for (i in 1:nrow(lin)) {
  lin[,"last_visit_hour"][i]<-visit_split_list[[i]][1]
  lin[,"last_visit_minute"][i]<-visit_split_list[[i]][2]
  lin[,"last_visit_second"][i]<-visit_split_list[[i]][3]
  lin[,"joining_year"][i]<-join_split_list[[i]][1]
  lin[,"joining_month"][i]<-join_split_list[[i]][2]
  lin[,"joining_day"][i]<-join_split_list[[i]][3]
  print(i)
}

lin<-subset(lin,select= -c(last_visit_time,joining_date))