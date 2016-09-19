setwd("~/Desktop/trains/")
states.shapes<-maptools::readShapeSpatial("IND_adm1.shp")
print(states.shapes$ID_1)
head(df$Seq[[i]],-1)
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
distance<-function(x,y){
return(sapply(x,function(xt)sapply(y,function(yt)sqrt(sum((xt-yt)**2)))))
}
i=500
i=1
df$predicted<-sapply(1:length(df$pattern),function(i){
  t<-df$pattern[[i]]
  print(i)
  if(length(t)>1){
    regl<-data.frame(x=1:length(t),y=t)
    a<-param_decide(regl)
    return(a[[which.min(lapply(a,function(x)x[[1]]))]][[2]])
  }else{
    return(t)
  }
})
param_decide()
regl<-data.frame(x=1:length(df$Seq[[i]]),y=df$Seq[[i]])
regl<-data.frame(x=1:length(df2$Seq[df2$clust==17][[2]]),y=df2$Seq[df2$clust==17][[2]])
plot(regl$y)
library("e1071")
library("randomForest")

param_decide<-function(regl){
    #Shifting the scale for log values
    flag=0
    if(sum(regl$y<=0)>0){
      val<-abs(min(regl$y))
      regl$z<-regl$y+val+1 
      flag=1
    }else{
      regl$z<-regl$y
    }

    #plot(regl$y,col="blue",type="l")

    #Log regression
    fit.log<-lm(log(z)~x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=regl)
    if(flag==1){
        #lines(exp(fit.log$fitted.values)-val-1,pch=19,col="red",cex=.9)
        rmse_log<-rmse(error = (regl$y-exp(fit.log$fitted.values)-val-1))
        rmse_log<-append(rmse_log,exp(predict.lm(fit.log,data.frame(x=(nrow(regl)+1)))-val-1))
    }else{
        #lines(exp(fit.log$fitted.values)-val-1,pch=19,col="red",cex=.9)
        rmse_log<-rmse(error = regl$y-exp(fit.log$fitted.values))  
        rmse_log<-append(rmse_log,(exp(predict.lm(fit.log,data.frame(x=(nrow(regl)+1))))))
    }

    #psudo linear regression
    fit.lin<-lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=regl)
    #points(fit.lin$fitted.values,pch=19,col="grey",cex=.9,type="l")
    rmse_linear<-rmse(error = regl$y-fit.lin$fitted.values)
    rmse_linear<-append(rmse_linear,predict.lm(fit.lin,data.frame(x=(nrow(regl)+1))))

    #two different distributions case
    fit.non<-lm(y~x+as.numeric(x%%2==0)+as.numeric(x%%3==0)+as.numeric(x%%5==0),data=regl)
    #points(fit.non$fitted.values,pch=19,col="pink",cex=.9,type="l")
    rmse_random<-rmse(error = regl$y-fit.non$fitted.values)
    rmse_random<-append(rmse_random,predict.lm(fit.non,data.frame(x=(nrow(regl)+1))))
    #psudo exponential regression
    #fit<-lm(y~exp(x)+x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=regl)  
    #points(fit$fitted.values,pch=19,col="black",cex=.9,type="l")  
    #rmse(error = regl$y-fit$fitted.values)

    #applying svr
    rmse_svr<-tryCatch({
    fit.svr<-svm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=regl)
    #points(fit.svr$fitted,pch=19,col="green",cex=.9,type="l")
    rmse_svr<-rmse(error = regl$y-fit.svr$fitted)
    rmse_svr<-append(rmse_svr,predict(fit.svr,data.frame(x=(nrow(regl)+1))))
    },error=function(e){
      rmse_svr<-c(1E900,median(regl$y))
    })


    #random forest
    fit.rf<-randomForest(formula=y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=regl,type="regression")
    #points(fit.rf$predicted,pch=19,col="orange",cex=.9,type="l")
    rmse_rfr<-rmse(error = regl$y-fit.rf$predicted)
    rmse_rfr<-append(rmse_rfr,predict(fit.rf,data.frame(x=(nrow(regl)+1))))
    
    #linear arima
    rmse_arima<-tryCatch({
    fit2<-auto.arima(regl$y)
    rmse_arima<-rmse(fit2$residuals)
    rmse_arima<-append(rmse_arima,c(forecast(fit2,h=1)$mean))
    },error=function(e){
      rmse_arima<-c(1E900,median(regl$y))
    })
  

    #legend("bottomleft",c(rmse_log,rmse_linear,rmse_random,rmse_svr,rmse_rfr), lty = rep(1,5), col=c("red","grey","pink","green","orange"),title = "RMSE")
    return(list(rmse_log=rmse_log,rmse_linear=rmse_linear,rmse_random=rmse_random,rmse_svr=rmse_svr,rmse_rfr=rmse_rfr,rmse_arima=rmse_arima))
}


points((nr-4):nr,regl$y[(nr-4):nr],pch=19,col="red")

#log for arima
nr<-nrow(regl)
fit2<-auto.arima(log(regl$y[1:(nr-5)]))
plot(forecast(fit2))
points((nr-4):nr,log(df$Seq[[i]][(nr-4):nr]),pch=19,col="red")

#fitting generic functions
ex<-MASS::fitdistr(regl$y,densfun = "exponential")
ex$sd


#arima for z
nr<-nrow(regl)
fit2<-auto.arima(regl$z[1:(nr-5)])
plot(forecast(fit2))
points((nr-4):nr,regl$z[(nr-4):nr],pch=19,col="red")

rmse<-function(error){
  return(mean(error**2))
}


fit2$residuals

lines(df$Seq[[i]],col="blue")
lines(fit$df.residual,col="red")