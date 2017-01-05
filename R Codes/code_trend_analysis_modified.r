set.seed(10)
#original number
rn<-rnorm(2000,0,.2)

trend_ana<-function(rn,num_plus,num_minus){
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
  nm_plus<-names(table(rn2)[table(rn2)>num_plus])
  nm_minus<-names(table(rn2)[table(rn2)>num_minus])
  #finding the means
  #sapply(rn2[as.numeric(nm)],mean)
  #non-sense
  #View(table(rn2))
  
  #final change
  rn_c<-rn
  for(i in as.numeric(nm_plus)){
    #print(i)
    if(mean(rn1[which(rn2==i)])==1){
    rn_c[rn2==i]<-rep(mean(rn[rn2==i]),sum(rn2==i))
    }
  }
   for(i in as.numeric(nm_minus)){
    #print(i)
    if(mean(rn1[which(rn2==i)])==2){
    rn_c[rn2==i]<-rep(mean(rn[rn2==i]),sum(rn2==i))
    }
   }
  return(rn_c)
}
rn<-matrix(rn,ncol=1000,nrow=2)
#dim(mat)
m1<-sapply(1:nrow(rn),function(x)trend_ana(rn[x,],2,3))
m1<-t(m1)
View(cbind(m1[1,],rn[1,]))
