par_graph<-function(table,labels) {
  plots=list()
  
  print(length(1:floor((ncol(table))/2)))
  plots<-lapply(1:floor((ncol(table))/2),function(column) ggplot(table,aes(year,table[,column]))+geom_bar(stat="identity")+
                  geom_text(label=paste("n= ",labels[,column+1],sep=""), nudge_y=.12*(max(table[,column])+table[,column+(ncol(table)-1)/2]),size=2)+
                  geom_errorbar(aes(ymin=table[,column]-table[,column+(ncol(table)-1)/2], ymax=table[,column]+table[,column+(ncol(table)-1)/2]),width=.65)+
                  labs(y=colnames(table)[column],title=colnames(table)[column])+theme(plot.title=element_text(size=10),axis.title=element_text(size=6)))
  
  print(plots)
  multiplot(plotlist=plots, cols=1)
}
graph_maxPress<-function(table) {
  ggplot(table,aes(Hour,MaxPress))+geom_bar(stat="identity")
}

graph_all_maxPress<-function(table) {
  plot=list()
  plot<-lapply(2008:2015, function(year) graph_maxPress(table[table["Year"]==year,]))
  multiplot(plotlist=plot,cols=1)
}
