AllDives<-read.table("./AllDives.txt", header=TRUE, sep="\t")
AllBouts<-read.table("./AllBouts.txt", header=TRUE, sep="\t")
trips<-read.table("./AllTrips.txt", header=TRUE, sep="\t")

maxPress_hour<- 
  AllDives[c("File","Year", "StartDate","StartTime", "MaxPress")] 
maxPress_hour<-mutate(maxPress_hour, Hour=sapply(maxPress_hour["StartTime"],function(x) substr(x, 1,2)), Month=sapply(maxPress_hour["StartDate"],function(x) substr(x, 6,7)))
maxPress_hour$Month[(maxPress_hour$Month=='06') & (maxPress_hour$Year==2013)] <- '05'

maxPress_hour_summary<-
  aggregate(MaxPress~Year+Hour, maxPress_hour,mean)
maxPress_hour_summary["Hour"]<-sapply(maxPress_hour_summary["Hour"], function(x) as.integer(x))
maxPress_hour_summary<-maxPress_hour_summary[(maxPress_hour_summary$Hour<21)&(maxPress_hour_summary$Hour>4),]
maxPress_hour_summary<-maxPress_hour_summary[order(maxPress_hour_summary$Year, maxPress_hour_summary$Hour),]

