colnames(AllDives)[17]<-"DiveLength"
dataPDI<-AllDives[c("Year","PDI","BottomTime","DiveLength")]
dataPDI["PDI"]<-sapply(dataPDI$PDI,as.numeric)
dataPDI["effic"]<-na.omit(dataPDI$BottomTime/dataPDI$DiveLength+dataPDI$PDI)
dataPDI["BottomTime"]<-NULL
dataPDI["DiveLength"]<-NULL

#omit PDIs above 120
dataPDI<-na.omit(dataPDI[dataPDI$PDI<120,])

#aggregate PDI-related data means, count, and standard error by year
PDI_data<-aggregate(dataPDI,by=list(dataPDI$Year),FUN="mean")
PDI_count<-aggregate(dataPDI[c("PDI","effic")],by=list(dataPDI$Year),function(x) length(x))
stderrPDI<-aggregate(dataPDI,by=list(dataPDI$Year),function(x) sd(x)/sqrt(length(x)))
stderrPDI["Year"]<-NULL
PDI_data["Group.1"]<-NULL
PDI_data<-inner_join(PDI_data,stderrPDI,by=c("Year"="Group.1"))

colnames(PDI_data)<-c("year","mean_PDI","mean_effic","stderr_PDI","stderr_effic")

dataParams<-AllDives[c("Year","MaxPress","DiveLength","BottomTime")]
dataParams["DiveShape"]<-dataParams$BottomTime/dataParams$DiveLength

#aggregate non-PDI related data means, count, and standard error by year
param_data<-aggregate(dataParams,by=list(dataParams$Year),FUN="mean")
params_stderr<-aggregate(dataParams,by=list(dataParams$Year),function(x) sd(x)/sqrt(length(x)))
params_count<-aggregate(dataParams[c("MaxPress","DiveLength","DiveShape")],by=list(dataParams$Year),function(x) length(x))
param_data["BottomTime"]<-NULL
param_data["Group.1"]<-NULL
params_stderr["Year"]<-NULL
params_stderr['BottomTime']<-NULL
paramdata<-inner_join(param_data,params_stderr,by=c("Year"="Group.1"))
counts_param<-inner_join(params_count,PDI_count,by=c("Group.1"="Group.1"))
colnames(paramdata)<-c("year","mean_max_pressure","mean_dive_length","mean_dive_shape","stderr_max_pressure","stderr_dive_length","stderr_dive_shape")

dive_table<-inner_join(paramdata,PDI_data,by=c("year"="year"))
dive_table<-dive_table[,order(names(dive_table))]

bout_data<-AllBouts[c("Year","boutLength","boutDives","boutUnder")]
bout_data["bout_effic"]<-bout_data$boutUnder/bout_data$boutLength
bout_data["boutUnder"]<-NULL

#aggregate bout related data means, count, and standard error by year
meanbout_table<-aggregate(bout_data,by=list(bout_data$Year),FUN='mean')
stderr_bout<-aggregate(bout_data,by=list(bout_data$Year),function(x) sd(x)/sqrt(length(x)))
bout_count<-aggregate(bout_data[c("boutLength","boutDives","bout_effic")],by=list(bout_data$Year),function(x) length(x))
stderr_bout["Year"]<-NULL
meanbout_table["Group.1"]<-NULL
bout_table<-inner_join(meanbout_table,stderr_bout,by=c("Year"="Group.1"))
colnames(bout_table)<-c("year","mean_bout_length","mean_bout_dives","mean_bout_effic","stderr_bout_length","stderr_bout_dives","stderr_bout_effic")
bout_table<-bout_table[,order(names(bout_table))]

count_dives<-rest_temp
count_dives<-mutate(count_dives,paste(as.character(count_dives$Year), as.character(count_dives$Month), as.character(count_dives$Hour), sep=""))
colnames(count_dives)[8]<-"markr"
count_dives<-aggregate(count_dives[c("Year","markr")], by=list(count_dives$markr), function(x) length(x))

trip_data<-trips[c("Year","Duration","Fly","Dive","Rest")]
trip_data["Fly"]<-trip_data$Fly/trip_data$Duration
trip_data["Dive"]<-trip_data$Dive/trip_data$Duration
trip_data["Rest"]<-trip_data$Rest/trip_data$Duration
trip_data["Duration"]<-NULL

#aggregate trip related data means, count, and standard error by year
mean_trip<-aggregate(trip_data,by=list(trip_data$Year),FUN="mean")
stderr_trip<-aggregate(trip_data,by=list(trip_data$Year),function(x) sd(x)/sqrt(length(x)))
trip_count<-aggregate(trip_data[c("Fly","Dive","Rest")],by=list(trip_data$Year),function(x) length(x))
stderr_trip["Year"]<-NULL
mean_trip["Group.1"]<-NULL

trip_table<-inner_join(mean_trip,stderr_trip,by=c("Year"="Group.1"))
colnames(trip_table)<-c("year","mean_fly","mean_dive","mean_rest","stderr_fly","stderr_dive","stderr_rest")
trip_table<-trip_table[,order(names(trip_table))]

#remove filler variables
rm(stderrPDI,dataPDI, mean_trip,stderr_trip,trip_data,meanbout_table,stderr_bout,bout_data, 
   params_count,PDI_count,dataParams,paramdata,PDI_data,params_stderr)

TOD.mod = data.frame(Fitted = fitted(reducedmodelTOD),
                       Residuals = resid(reducedmodelTOD), Treatment = formodel$TOD)
ggplot(TOD.mod,aes(Fitted,Residuals,color=Treatment))+geom_point()
reducedmodel<-lme(MaxPress~Hour*as.factor(Year)+I(Hour^2), method="ML", random=~1|File, data=formodel, na.action=na.omit)
fullmodel<-lme(MaxPress~(Hour+I(Hour^2))*as.factor(Year), method="ML", random=~1|File, data=formodel, na.action=na.omit)
ggplot(formodel, aes(x=Hour,y=MaxPress))+geom_boxplot(aes(group=Hour))+stat_smooth(method="lm",formula=y~x+I(x^2),se=FALSE,aes(color=Year))+
  ggtitle("Reduced and Full Models")+
  theme(plot.title=element_text(size=12),axis.title=element_text(size=10),
        axis.text=element_text(size=8),legend.key.size=unit(.5,"cm"),
        legend.text=element_text(size=8),legend.title=element_text(size=8))
year<-as.factor(formodel$Year)
ggplot(formodel, aes(x=Hour,y=MaxPress))+geom_boxplot(aes(group=Hour))+stat_smooth(method="lm",formula=y~x+I(x^2),se=FALSE,aes(group=year, color=year))+
  theme(plot.title=element_text(size=12),axis.title=element_text(size=15),
        axis.text=element_text(size=12),legend.key.size=unit(1,"cm"),
        legend.text=element_text(size=14),legend.title=element_text(size=14))
anova(reducedmodel,fullmodel)


Y.hour_graphs<-function(table,labels) {
  plots=list()
  
  print(length(1:floor((ncol(table))/2)))
  plots<-lapply(1:floor((ncol(table))/2),function(column) ggplot(table,aes(year,table[,column]))+geom_bar(stat="identity")+
                  geom_text(label=paste("n= ",labels[,column+1],sep=""), nudge_y=.12*(max(table[,column])+table[,column+(ncol(table)-1)/2]),size=2)+
                  geom_errorbar(aes(ymin=table[,column]-table[,column+(ncol(table)-1)/2], ymax=table[,column]+table[,column+(ncol(table)-1)/2]),width=.65)+
                  labs(y=colnames(table)[column],title=colnames(table)[column])+theme(plot.title=element_text(size=10),axis.title=element_text(size=6)))
  
  print(plots)
  multiplot(plotlist=plots, cols=1)
}