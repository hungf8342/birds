#ANOVA on reduced and full model maxPress~hour*year

#model with a constant quadratic term
reducedmodel<-lme(MaxPress~Hour*as.factor(Year)+I(Hour^2), method="ML", random=~1|File, data=formodel, na.action=na.omit)

#model with all terms varying by year
fullmodel<-lme(MaxPress~(Hour+I(Hour^2))*as.factor(Year), method="ML", random=~1|File, data=formodel, na.action=na.omit)

year<-as.factor(formodel$Year)

#plot full models by year along with cumulative boxplot of maxPressure
ggplot(formodel, aes(x=Hour,y=MaxPress))+geom_boxplot(aes(group=Hour))+stat_smooth(method="lm",formula=y~x+I(x^2),se=FALSE,aes(group=year, color=year))+
  theme(plot.title=element_text(size=12),axis.title=element_text(size=15),
        axis.text=element_text(size=12),legend.key.size=unit(1,"cm"),
        legend.text=element_text(size=14),legend.title=element_text(size=14))

#ANOVA on reduced vs. full model
anova(reducedmodel,fullmodel)
