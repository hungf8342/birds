#perform general procrustes analysis (GPA) on the dive data by hour

blah<-maxPress_hour
blah<-aggregate(MaxPress~Year+Hour,data=blah,FUN="mean")
blah<-blah[order(blah$Year),]
blah<-abind(split(blah,blah$Year), along=3)
forGPA_table<-blah[,-1,]

#gpa function (requires geomorph package)
Y.gpa<-gpagen(forGPA_table,PrinAxes=FALSE)
plot(Y.gpa,yaxt='n',xaxt='n',ann=FALSE)
Axis(side=1, labels=FALSE)
title(xlab="Hours (normalized)", ylab="MaxPress (normalized)", cex.lab=1.5)