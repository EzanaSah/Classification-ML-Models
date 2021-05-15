rm(list=ls())

mydata <- read.table("house-votes-84.data",header=FALSE, sep=",")
colnames(mydata) <- c("party", "handicapped","water", "budget", "physician", "elsalvador","religious", "antisatellite", "aidcontras", "missile", "immigration", "synfuels", "educspend", "sue", "crime", "dutyfree", "exportafrica")

attach(mydata)

