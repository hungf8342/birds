#Here, we divide all dives into three categories by time of day (morn, midd, and night) and run ANOVA/Tukey HSD

formodel<-AllDives[c("File", "Year", "StartDate","StartTime", "MaxPress")]
formodel<-mutate(formodel, substr(as.character(formodel$StartDate), 6,7))
formodel<-mutate(formodel, substr(as.character(formodel$StartTime), 1,2))
colnames(formodel)[6]<-"Month"
colnames(formodel)[7]<-"Hour"

#reclassify a borderline month (June 2013 -> May 2013)
formodel$Month[(formodel$Month=='06') & (formodel$Year==2013)] <- '05'

formodel$Month<-sapply(formodel$Month, as.numeric)
formodel$Hour<-sapply(formodel$Hour, as.numeric)

#take into account only hours 5 A.M.-8 P.M.
formodel<-formodel[(formodel$Hour>4) & (formodel$Hour<21),]

#assign time of day for each dive
for (i in 1:nrow(formodel)) {
  if ((formodel[i ,"Hour"]>4) & (formodel[i ,"Hour"]<11)) {
    formodel[i,"TOD"]="morning"
  }
  else if ((formodel[i ,"Hour"]>=11) & (formodel[i ,"Hour"]<16)) {
    formodel[i,"TOD"]="midday"
  }
  else {
    formodel[i,"TOD"]="night"
  }
}
formodel$TOD<-as.factor(formodel$TOD)
formodel$TOD=factor(formodel$TOD,c("morning","midday","night"))

#represent relationship between max.dive depth (maxPress) and TOD with a mixed linear model (random eff. is individual bird)
reducedmodelTOD<-lme(MaxPress~TOD,method="ML", random=~1|File, data=formodel, na.action=na.omit)

#boxplot of maxPress vs. TOD
ggplot(formodel, aes(x=TOD, y=MaxPress))+geom_boxplot()+theme(plot.title=element_text(size=14),axis.title=element_text(size=17),
                                                              axis.text=element_text(size=14))

#ANOVA/Tukey HSD of TOD
a1<-anova(reducedmodelTOD)
summary(glht(reducedmodelTOD,linfct=mcp(TOD="Tukey")))
