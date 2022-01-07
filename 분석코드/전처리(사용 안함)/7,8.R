setwd("C:/data")
happy <- read.csv(file="w_train.csv")
happy$region_category[happy$region_category==""] <- "unknown"
happy$points_in_wallet[is.na(happy$points_in_wallet)] <- 0  
happy$points_in_wallet[happy$points_in_wallet < 0] <- 0  
happy
