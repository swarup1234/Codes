#Feature Creation
setwd("~/Desktop/")
df<-read.csv("train.csv")
df$Seq<-sapply(df$Sequence,function(x)as.numeric(unlist(strsplit(as.character(x),","))))
df$lenth<-sapply(df$Seq,length)
df$rounding<-sapply(df$Seq,function(x)unique(round(seq(0,length(x),length.out=6))))
df$scaler<-sapply(1:nrow(df),function(x){
  sk<-scale(df$Seq[[x]])
  p<-as.numeric(df$rounding[[x]])
  return(unlist(lapply(1:(length(p)-1),function(t){
    mean(sk[(p[t]+1):p[t+1],])
  })))
})
df$pattern<-lapply(df$Seq,function(x){
  if(length(x)>1){
    return(head(x,-1))
  }else{
    return(x)
  }
})
df$result<-sapply(df$Seq,function(x){
  if(length(x)>1){
    return(tail(x,1))
  }else{
    return(x)
  }
})


#Clustering
idx<-which(sapply(df$scaler,length)<5)
df1<-df[-idx,]
a<-distance(df1$scaler[1:100],df1$scaler[1:100])
a<-as.dist(a)
set.seed(4)
k<-NULL
for(i in 1:50){
  km1<-kmeans(a,i)
  k<-rbind(k,c(i,km1$tot.withinss))
}
km1<-kmeans(a,22)
#storing cluster
dir.create("Cluster1")
setwd("./Cluster1")
df2<-df1[1:100,]
df2$clust<-km1$cluster
for(i in 1:length(unique(km1$cluster))){
if(!dir.exists(paste0("clust_",i))){
  dir.create(paste0("clust_",i))
}
wd<-paste0("./clust_",i)
k=1
for(j in df2$Seq[df2$clust==i]){
jpeg(paste0(wd,'/rplot_',k,'.jpg'))
plot(j)
dev.off()
k=k+1
}
}


