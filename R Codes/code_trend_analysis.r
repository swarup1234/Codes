set.seed(10)
#original number
rn<-rnorm(52000,0,.2)
trend_ana<-function(rn,num){
  #indicator
  rn1<-rn
  rn1[rn1>=0]<-1
  rn1[rn1<0]<-2
  
  #cluster value
  rn2<-rn1
  count=1
  rn2[1]<-count
  for(i in 2:length(rn1)){
    if(rn1[i]!=rn1[i-1]){
      count=count+1
    }
  rn2[i]<-count
  }
  
  #all the clusters greater than num
  nm<-names(table(rn2)[table(rn2)>num])
  
  #finding the means
  #sapply(rn2[as.numeric(nm)],mean)
  #non-sense
  #View(table(rn2))
  
  #final change
  rn_c<-rn
  for(i in as.numeric(nm)){
    #print(i)
    rn_c[rn2==i]<-rep(mean(rn[rn2==i]),sum(rn2==i))
  }
  return(rn_c)
}
mat<-matrix(rn,ncol=52,nrow=1000)
#dim(mat)
m1<-sapply(1:nrow(mat),function(x)trend_ana(mat[x,],2))
m1<-t(m1)
View(cbind(m1[1,],mat[1,]))
