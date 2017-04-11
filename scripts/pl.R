#for each x find PL as 1-predicted nat/predicted hand

library(ggplot2)
library(dplyr)

mydata=read.csv("data/part2_all_years_data_for_pub.csv", sep=";", stringsAsFactors = F)
str(mydata)
data_2009_s1=mydata[mydata$Year== '2011' & mydata$Pop=='s1',]
data_2009_s2=mydata[mydata$Year== '2011' & mydata$Pop=='s2',]
data_2009_s1$st_med_flr_date=(data_2009_s1$med_flr_date-mean(data_2009_s1$med_flr_date, na.rm = T))/sd(data_2009_s1$med_flr_date, na.rm=T)
data_2009_s2$st_med_flr_date=(data_2009_s2$med_flr_date-mean(data_2009_s2$med_flr_date, na.rm = T))/sd(data_2009_s2$med_flr_date, na.rm=T)

lm(data_2009_s1$total_seed~   data_2009_s1$st_med_flr_date , data=data_2009_s1)
lm(total_seed~  st_med_flr_date , data=data_2009_s2)



data_2010_s1=mydata[mydata$Year== '2011' & mydata$Pop=='s2',]
data_2010_s2=mydata[mydata$Year== '2011' & mydata$Pop=='s1',]
data_2010_s1$st_med_flr_date=(data_2010_s1$med_flr_date-mean(data_2010_s1$med_flr_date, na.rm = T))/sd(data_2010_s1$med_flr_date, na.rm=T)
data_2010_s2$st_med_flr_date=(data_2010_s2$med_flr_date-mean(data_2010_s2$med_flr_date, na.rm = T))/sd(data_2010_s2$med_flr_date, na.rm=T)
lm(total_seed~  st_med_flr_date , data=data_2010_s1)
lm(total_seed~  st_med_flr_date , data=data_2010_s2)

data_2011_s1=mydata[mydata$Year== '2011' & mydata$Pop=='s1',]
data_2011_s2=mydata[mydata$Year== '2011' & mydata$Pop=='s2',]
data_2011_s1$st_med_flr_date=(data_2011_s1$med_flr_date-mean(data_2011_s1$med_flr_date, na.rm = T))/sd(data_2011_s1$med_flr_date, na.rm=T)
data_2011_s2$st_med_flr_date=(data_2011_s2$med_flr_date-mean(data_2011_s2$med_flr_date, na.rm = T))/sd(data_2011_s2$med_flr_date, na.rm=T)
lm(total_seed~  st_med_flr_date , data=data_2011_s1)
lm(total_seed~  st_med_flr_date , data=data_2011_s2)

#flrnum
data_2009_s1=mydata[mydata$Year== '2009' & mydata$Pop=='s1',]
data_2009_s2=mydata[mydata$Year== '2009' & mydata$Pop=='s2',]
data_2009_s1$st_flr_num=(data_2009_s1$flr_num-mean(data_2009_s1$flr_num, na.rm = T))/sd(data_2009_s1$flr_num, na.rm=T)
data_2009_s2$st_flr_num=(data_2009_s2$flr_num-mean(data_2009_s2$flr_num, na.rm = T))/sd(data_2009_s2$flr_num, na.rm=T)
lm=lm(data_2009_s1$total_seed~   data_2009_s1$st_flr_num , data=data_2009_s1)
intercept=lm$coefficients[1]
slope=lm$coefficients[2]
X_values=seq(-3,3,0.01)
pred=intercept+(X_values*slope)
temp_data=cbind(X_values,pred)
temp_data=as.data.frame(temp_data)
colnames(temp_data)=c("X_values",  "Nat_Predicted")



lm=lm(data_2009_s2$total_seed~   data_2009_s2$st_flr_num , data=data_2009_s2)
intercept=lm$coefficients[1]
slope=lm$coefficients[2]
X_values=seq(-3,3,0.01)
pred=intercept+(X_values*slope)
temp_data2=cbind(X_values,pred)
temp_data2=as.data.frame(temp_data2)
colnames(temp_data2)=c("X_values",  "HP_Predicted")
rm(PL_data)
PL_data=cbind(temp_data,temp_data2)
PL_data$PL=1-(temp_data$Nat_Predicted/temp_data2$HP_Predicted)
PL_data[PL_data$PL>1,]=1
PL_data[PL_data$PL<0,]
ggplot(PL_data, aes(y=PL,x=X_values)) + 
        geom_line()+ scale_y_continuous(limits = c(0,1))+
scale_x_continuous(limits = c(-1,3)) 


data_2010_s1=mydata[mydata$Year== '2010' & mydata$Pop=='s1',]
data_2010_s2=mydata[mydata$Year== '2010' & mydata$Pop=='s2',]
data_2010_s1$st_flr_num=(data_2010_s1$flr_num-mean(data_2010_s1$flr_num, na.rm = T))/sd(data_2010_s1$flr_num, na.rm=T)
data_2010_s2$st_flr_num=(data_2010_s2$flr_num-mean(data_2010_s2$flr_num, na.rm = T))/sd(data_2010_s2$flr_num, na.rm=T)
lm(total_seed~   st_flr_num , data=data_2010_s1)
lm(total_seed~   st_flr_num , data=data_2010_s2)
lm=lm(data_2010_s1$total_seed~   st_flr_num , data=data_2010_s1)
intercept=lm$coefficients[1]
slope=lm$coefficients[2]
X_values=seq(-3,3,0.01)
pred=intercept+(X_values*slope)
temp_data=cbind(X_values,pred)
temp_data=as.data.frame(temp_data)
colnames(temp_data)=c("X_values",  "Nat_Predicted")



lm=lm(data_2010_s2$total_seed~   data_2010_s2$st_flr_num , data=data_2010_s2)
intercept=lm$coefficients[1]
slope=lm$coefficients[2]
X_values=seq(-3,3,0.01)
pred=intercept+(X_values*slope)
temp_data2=cbind(X_values,pred)
temp_data2=as.data.frame(temp_data2)
colnames(temp_data2)=c("X_values",  "HP_Predicted")
rm(PL_data)
PL_data=cbind(temp_data,temp_data2)
PL_data$PL=1-(temp_data$Nat_Predicted/temp_data2$HP_Predicted)
PL_data[PL_data$PL>1,]=NA
PL_data[PL_data$PL<0,]=NA
ggplot(PL_data, aes(y=PL,x=X_values)) + 
        geom_point()+ scale_y_continuous(limits = c(0,1))+
        scale_x_continuous(limits = c(-2,3)) 

data_2011_s1=mydata[mydata$Year== '2011' & mydata$Pop=='s1',]
data_2011_s2=mydata[mydata$Year== '2011' & mydata$Pop=='s2',]
data_2011_s1$st_flr_num=(data_2011_s1$flr_num-mean(data_2011_s1$flr_num, na.rm = T))/sd(data_2011_s1$flr_num, na.rm=T)
data_2011_s2$st_flr_num=(data_2011_s2$flr_num-mean(data_2011_s2$flr_num, na.rm = T))/sd(data_2011_s2$flr_num, na.rm=T)
lm(total_seed~   st_flr_num , data=data_2011_s1)
lm(total_seed~   st_flr_num , data=data_2011_s2)
lm=lm(data_2011_s1$total_seed~   st_flr_num , data=data_2011_s1)
intercept=lm$coefficients[1]
slope=lm$coefficients[2]
X_values=seq(-3,3,0.01)
pred=intercept+(X_values*slope)
temp_data=cbind(X_values,pred)
temp_data=as.data.frame(temp_data)
colnames(temp_data)=c("X_values",  "Nat_Predicted")



lm=lm(data_2011_s2$total_seed~   data_2011_s2$st_flr_num , data=data_2011_s2)
intercept=lm$coefficients[1]
slope=lm$coefficients[2]
X_values=seq(-3,3,0.01)
pred=intercept+(X_values*slope)
temp_data2=cbind(X_values,pred)
temp_data2=as.data.frame(temp_data2)
colnames(temp_data2)=c("X_values",  "HP_Predicted")
rm(PL_data)
PL_data=cbind(temp_data,temp_data2)
PL_data$PL=1-(temp_data$Nat_Predicted/temp_data2$HP_Predicted)
PL_data[PL_data$PL>1,]=NA
PL_data[PL_data$PL<0,]=NA
ggplot(PL_data, aes(y=PL,x=X_values)) + 
        geom_point()+ scale_y_continuous(limits = c(0,1))+
        scale_x_continuous(limits = c(-2,3)) 

fit.mod.s1=lm(total_seed~   st_flr_num , data=data_2011_s1)
fit.mod.s2=lm(total_seed~   st_flr_num , data=data_2011_s2)

summary(fit.mod.s1)[4][[1]][1]
summary(fit.mod.s1)[4][[1]][2]
fit.mod.s1$coefficients
flr_num=rbind(data_2009_s1,data_2009_s2,data_2010_s1,data_2010_s2,data_2011_s1,data_2011_s2)
flr_num$PL=


lm(data_2011_s2$total_seed~   data_2011_s2$st_med_flr_date , data=data_2011_s2)

#
data_

s1_mod=lm(data_2011_s1$total_seed~   data_2011_s1$st_med_flr_date , data=data_2011_s1)
summary(s1_mod)
lm(data_2011_s2$total_seed~   data_2011_s2$st_med_flr_date , data=data_2011_s2)

med_flr_date_PL[med_flr_date_PL$X_values==0,]


ggplot(data_2011, aes(y=data_2011$total_seed,x=data_2011$st_med_flr_date, group=Pop)) + 
        stat_smooth(method=lm,aes(linetype=Pop))
+
        scale_y_continuous(limits = c(0,1))
scale_x_continuous(limits = c(-1,3)) 

#
data_2011_s1$st_and=(data_2011_s1$and-mean(data_2011_s1$and, na.rm = T))/sd(data_2011_s1$and, na.rm=T)
data_2011_s2$st_and=(data_2011_s2$and-mean(data_2011_s2$and, na.rm = T))/sd(data_2011_s2$and, na.rm=T)


s1_mod=lm(data_2011_s1$total_seed~   data_2011_s1$st_and , data=data_2011_s1)
summary(s1_mod)
lm(data_2011_s2$total_seed~   data_2011_s2$st_med_flr_date , data=data_2011_s2)

ggplot(med_flr_date_PL, aes(y=med_flr_date_PL$PL,x=med_flr_date_PL$X_values,  group=Year)) + 
        geom_line(aes(linetype=Year)) +
        # scale_x_continuous(limits = c(-3,3)) +
        scale_y_continuous(limits = c(0,1))
