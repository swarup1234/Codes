rm(list=ls())
setwd("~/kag_usd_cse/code")
source("utils.R")

require(data.table) #Working with large files
require(xlsx)       #Loading and saving .xlsx files 
require(plyr)   #Always load in this order plyr, dpply, lubridate - dpplyr overrides some of methods in plyr. 
require(dplyr) #Use require as it will give an error message if the package doesn't exist
require(reshape2)  #used for melting 

#input files
attb<-"../raw-data/attributes.csv"
prd_desc<-"../raw-data/product_descriptions.csv"
trn<-"../raw-data/train.csv"
tst<-"../raw-data/test.csv"

#Reading the files
attr<-read.csv(attb)
prd_des<-read.csv(prd_desc)
train<-read.csv(trn)
test<-read.csv(tst)

#merging with respect to the product_uid
mrg<-merge(train,prd_des,by.x = "product_uid", by.y = "product_uid",all.x=TRUE )


