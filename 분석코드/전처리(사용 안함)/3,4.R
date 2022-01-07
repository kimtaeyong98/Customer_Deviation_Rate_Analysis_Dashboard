lin <- read.csv(file="train.csv")
lin<-transform(lin,
               avg_time_spent=abs(avg_time_spent))

lin$medium_of_operation[lin$medium_of_operation=="?"&lin$internet_option=="Wi-Fi"]<-"Both"
lin$medium_of_operation[lin$medium_of_operation=="?"&lin$internet_option=="Mobile_Data"]<-"Smartphone"
lin$medium_of_operation[lin$medium_of_operation=="?"&lin$internet_option=="Fiber_Optic"]<-"Desktop"