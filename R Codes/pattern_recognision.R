setwd("~/Desktop/trains/")
states.shapes<-maptools::readShapeSpatial("IND_adm1.shp")
print(states.shapes$ID_1)


distance<-function(x,y){
return(sapply(x,function(xt)sapply(y,function(yt)sqrt(sum((xt-yt)**2)))))
}
i=500
regl<-data.frame(x=1:length(df$Seq[[i]]),y=df$Seq[[i]])

#Log regression
fit<-lm(log(y)~x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=regl)
plot(df$Seq[[i]],col="blue",type="l")
lines(exp(fit$fitted.values),pch=19,col="blue",cex=.9)


#psudo linear regression
fit<-lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5),data=regl)
plot(df$Seq[[i]],col="blue",type="l")
points(fit$fitted.values,pch=19,col="blue",cex=.9)

#linear arima
nr<-nrow(regl)
fit2<-auto.arima(regl$y[1:(nr-5)])
plot(forecast(fit2))
points((nr-4):nr,df$Seq[[i]][(nr-4):nr],pch=19,col="red")

#log for arima
nr<-nrow(regl)
fit2<-auto.arima(log(regl$y[1:(nr-5)]))
plot(forecast(fit2))
points((nr-4):nr,log(df$Seq[[i]][(nr-4):nr]),pch=19,col="red")



fit2$residuals

lines(df$Seq[[i]],col="blue")
lines(fit$df.residual,col="red")