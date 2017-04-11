library(ggplot2)
#data
#fit sel model for hand and nat
#for each x find PL as 1-predicted nat/predicted hand
mydata=read.csv("data/part2_all_years_data_for_pub.csv", sep=";", stringsAsFactors = F)
str(mydata)
for(i in c('2009','2010','2011')){
        mydata$st_and[mydata$Year==i]= (mydata$and[mydata$Year==i]-mean(mydata$and[mydata$Year==i],na.rm=T))/sd(mydata$and[mydata$Year==i],na.rm=T) 
}
range( mydata$st_and,na.rm = T)

for(i in c('2009','2010','2011')){
        mydata$st_mid_wid[mydata$Year==i]= (mydata$mid_wid[mydata$Year==i]-mean(mydata$mid_wid[mydata$Year==i],na.rm=T))/sd(mydata$mid_wid[mydata$Year==i]) 
}
for(i in c('2009','2010','2011')){
        mydata$st_flr_num[mydata$Year==i]= (mydata$flr_num[mydata$Year==i]-mean(mydata$flr_num[mydata$Year==i], na.rm=T))/sd(mydata$flr_num[mydata$Year==i],na.rm=T) 
}

range( mydata$st_flr_num,na.rm = T)
mydata$st_and=(subdata_year[,2]-mean(subdata_year[,2], na.rm=TRUE))/(sd(subdata_year[,2], na.rm=TRUE)   )

#################################################################################
#Tables S7 to S12. Total selection and direct selection in 2009, 2010 and 2011
#################################################################################

#I split the data into the two treatments s1 for naturally pollinated plants len_last s2 for hlen_last-pollinated plants
#recall that the data is paritioned above by year as follows:

year=mydata[mydata$Year=='2009', ]
#year2=mydata[mydata$Year=='2010', ]
#year_2011=mydata[mydata$Year=='2011', ]
head(subdata)



########Selection differentials:

#this is the function that will calculate the selection differentials. You can choose the traits and data you would like estimates for in the 
#by calling this function with the dataset name and list of traits.
raw_regression_univariate <- function(raw_data,years, grp,trait=c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"),y="total_seed" ){
        
        if(!(is.data.frame(raw_data))){stop("invalid dataset name")}  
        
        data=raw_data[which(raw_data$Pop %in% grp & raw_data$Year %in% years),]
        
        trait_columns=c(which(colnames(data) %in% trait))
        y_var_pos=which(colnames(data) %in% y)
        
        for (i in trait_columns) {
               
                myvars=c(y_var_pos, i)
                subdata=data[,myvars]
                subdata=subdata[complete.cases(subdata), ]
                
                #this stlen_lastardizes the traits of interest
                subdata$stlen_lastardized_trait=(subdata[,2]-mean(subdata[,2], na.rm=TRUE))/sd(subdata[,2], na.rm=TRUE)   
                #this relatives fitness
                subdata$rel_fitness=subdata[,1]/mean(subdata[,1])
                coefs2<-lm(subdata$rel_fitness~   subdata$stlen_lastardized_trait , data=subdata)
                t=paste("N=", nrow(subdata), "trait", colnames(data[i]))
                print(t)
                print(summary(coefs2))
        }
}
raw_regression_univariate(mydata,'2011', 's2',c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"))






get_function=function(raw_data,years, trait=c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"),y="total_seed" ){
        if(!(is.data.frame(raw_data))){stop("invalid dataset name")}  
        if(length(trait)>1){stop("you can only do one trait at a time")}
        
        full_data={}
        years_column=c(which(colnames(raw_data) %in% "Year"))
        
        #choose pop column
        
        pop_column=c(which(colnames(raw_data) %in% "Pop"))
        #choose trait column
        trait_columns=c(which(colnames(raw_data) %in% trait))
        #choose y var column
        y_var_pos=which(colnames(raw_data) %in% y)
        myvars=c(y_var_pos, trait_columns,pop_column,years_column)
        #subdata pull sout the 3 columns
        subdata=raw_data[,myvars]
        subdata=subdata[complete.cases(subdata),]
        
        #now fit lm for each year separately
        for(year in years){
                #do natural first
                subdata_s1=subdata[which(subdata$Pop %in% 's1' & subdata$Year==year),]
                subdata_s1$stlen_lastardized_trait=(subdata_s1[,2]-mean(subdata_s1[,2], na.rm=TRUE))/sd(subdata[,2], na.rm=TRUE)   
                
                fit_lm<-lm(subdata_s1[,1]~   subdata_s1$stlen_lastardized_trait +I(subdata_s1$stlen_lastardized_trait^2) , data=subdata_s1)
                slope  <- fit_lm$coefficients[2]  
                print(slope)
                intercept=fit_lm$coefficients[1]
                
                X_values=seq(-3,3,0.01)
                pred=intercept+(X_values*slope)
                temp_data=cbind(year,X_values,pred)
                temp_data=as.data.frame(temp_data)
                colnames(temp_data)=c("Year","X_values",  "Nat_Predicted")
                
                #s2
                #do natural first
                subdata_s2=subdata[subdata$Pop=='s2'& subdata$Year==year,]
                subdata_s2$stlen_lastardized_trait=(subdata_s2[,2]-mean(subdata_s2[,2], na.rm=TRUE))/sd(subdata[,2], na.rm=TRUE)   
                
                #fit_lm<-lm(subdata_s2[,1]~   subdata_s2$stlen_lastardized_trait , data=subdata_s2)
                fit_lm<-lm(subdata_s1[,1]~   subdata_s1$stlen_lastardized_trait +I(subdata_s1$stlen_lastardized_trait^2) , data=subdata_s1)
                
                slope  <- fit_lm$coefficients[2]  
                intercept=fit_lm$coefficients[1]
                
                X_values=seq(-3,3,0.01)
                pred=intercept+(X_values*slope)
                temp_data2=cbind(year,X_values,pred)
                temp_data2=as.data.frame(temp_data2)
                colnames(temp_data2)=c("Year","X_values",  "HP_Predicted")
                
                f=cbind(temp_data, temp_data2)
                f=as.data.frame(f)
                for(i in seq(2,dim(f)[2])){
                        f[,i]=as.character( f[,i])
                        f[,i]=as.numeric( f[,i])
                        
                }
                
                full_data=rbind(f, full_data)
                
        }
       # return(full_data)
        
}


get_predicted_values=function(raw_data,years, trait=c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"),y="total_seed" ){
        if(!(is.data.frame(raw_data))){stop("invalid dataset name")}  
        if(length(trait)>1){stop("you can only do one trait at a time")}
        
        full_data={}
        years_column=c(which(colnames(raw_data) %in% "Year"))
        
        #choose pop column
        
        pop_column=c(which(colnames(raw_data) %in% "Pop"))
        #choose trait column
        trait_columns=c(which(colnames(raw_data) %in% trait))
        print(trait_columns)
        #choose y var column
        y_var_pos=which(colnames(raw_data) %in% y)
        myvars=c(y_var_pos, trait_columns,pop_column,years_column)
        #subdata pull sout the 3 columns
        subdata=raw_data[,myvars]
        #subdata=subdata[complete.cases(subdata),]
        
                #now fit lm for each year separately
        for(year in years){
                #do natural first
                subdata_year=subdata[subdata$Year==year,]
                subdata_year$st_trait=(subdata_year[,2]-mean(subdata_year[,2], na.rm=TRUE))/(sd(subdata_year[,2], na.rm=TRUE)   )
                
                subdata_year_s1=subdata_year[subdata_year$Pop=='s1',]
                subdata_year_s2=subdata_year[subdata_year$Pop=='s2',]
                
                fit_lm<-lm(total_seed~ st_trait , data=subdata_year_s1)
                slope  <- fit_lm$coefficients[2]  
                intercept=fit_lm$coefficients[1]
                print(summary(fit_lm))
       print(slope)
       print(intercept)
                X_values=seq(-5,3,0.01)
                pred=intercept+(X_values*slope)
                temp_data=cbind(year,X_values,pred)
                temp_data=as.data.frame(temp_data)
                colnames(temp_data)=c("Year","X_values",  "Nat_Predicted")
                
                #s2
                subdata_year_s2=subdata_year[subdata_year$Pop=='s2',]
                
                fit_lm<-lm(total_seed~ st_trait , data=subdata_year_s2)
                slope  <- fit_lm$coefficients[2]  
                intercept=fit_lm$coefficients[1]
                print(slope)
                print(intercept)
                
                X_values=seq(-5,3,0.01)
                pred=intercept+(X_values*slope)
                temp_data2=cbind(year,X_values,pred)
                temp_data2=as.data.frame(temp_data2)
                colnames(temp_data2)=c("Year","X_values",  "HP_Predicted")
                
                f=cbind(temp_data, temp_data2)
                f=as.data.frame(f)
                for(i in seq(2,dim(f)[2])){
                        f[,i]=as.character( f[,i])
                        f[,i]=as.numeric( f[,i])
                        
                }
                
                full_data=rbind(f, full_data)
                
        }
        return(full_data)
                
        }

get_function(mydata,c('2009', '2010','2011'), "med_flr_date")

and_PL=get_predicted_values(mydata,c('2009','2010', '2011'), "and")
and_PL$PL=1-(and_PL$Nat_Predicted/and_PL$HP_Predicted)


and_PL[and_PL$Year=='2011' & and_PL$X_values==-3,]
and_PL[and_PL$Year=='2010' & and_PL$X_values==-3,]
dev.off()
yname=expression(paste("Individual Pollen Limitation,",widehat(IPL)))

and_plot=ggplot(and_PL, aes(y=and_PL$PL,x=and_PL$X_values,  group=Year)) + 
        geom_line(aes(linetype=Year)) + 
        xlab("Z") + 
        ylab(yname)+         
        ggtitle("a) Anther-nectary distance")+
        scale_x_continuous(limits = c(-5,3)) +
        scale_y_continuous(limits = c(0,1))+ 
        theme(plot.margin = unit(c(0.05,0.1,0,0.05), "cm"),
                plot.title = element_text(hjust = -0.5, size = "10"),
                axis.title.y = element_text(colour = "black", size = "12"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())            

#
flr_num_PL=get_predicted_values(mydata,c('2009', '2010','2011'), "flr_num")
flr_num_PL$PL=1-(flr_num_PL$Nat_Predicted/flr_num_PL$HP_Predicted)

library(ggplot2)
yname=expression(widehat(IPL))
flr_num_plot=ggplot(flr_num_PL, aes(y=flr_num_PL$PL,x=flr_num_PL$X_values,  group=Year)) + 
        geom_line(aes(linetype=Year)) +
        scale_x_continuous(limits = c(-1.22,3)) +         
        scale_y_continuous(limits = c(0,1))+ 
        xlab("Z") + 
        ylab(yname)+
        ggtitle("b) Flower number")+
                theme(plot.margin = unit(c(0.05,0.1,0,0.05), "cm"),
                plot.title = element_text(hjust = -0.5, size = "10"),
                axis.title.y = element_text(colour = "black", size = "12"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black"),
                panel.background = element_blank())            


library(ggplot2)
library(gridExtra)
library(grid)
require(cowplot)
library(dplyr)
        
        g_legend<-function(a.gplot){
                tmp <- ggplot_gtable(ggplot_build(a.gplot))
                leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
                legend <- tmp$grobs[[leg]]
                return(legend)}
        
        mylegend<-g_legend(and_plot)
quartz(width = 3.38, height  = 4, family = "Helvetica")
# png(file = "/Users/chriseckert/Dropbox/Shared/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Figure_2_IBD/Fig2_IBD_plots.png", width = 3.38, height  = 4, units = "in", family = "Helvetica", res = 600, pointsize = 12, bg="transparent")
# postscript(file = "/Users/chriseckert/Dropbox/Shared/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/manuscript/Figures/Figure_2_IBD/Fig2_IBD_plots.ps", width = 3.38, height  = 4, family = "Helvetica", pointsize = 12, bg="transparent")
tiff(file = "Fig1.tiff", width = 3.38, height  = 4, units = "in", family = "Helvetica", res = 300, pointsize = 12, bg="transparent")
grid.newpage()
grid.draw(rbind(ggplotGrob(and_plot), ggplotGrob(flr_num_plot), size = "last"))
dev.off()

grid.arrange(arrangeGrob(and_plot + theme(legend.position="none"),
        flr_num_plot + theme(legend.position="none"),
        nrow=1),
        mylegend, nrow=1,ncol=3,widths=c(10,10,1))

pushViewport(viewport(layout = grid.layout(3, 2)))  
print(and_plot,vp = viewport(layout.pos.row = 1, layout.pos.col = 1))     
print(flr_num_plot,vp = viewport(layout.pos.row = 1, layout.pos.col = 2))     



#
len_last_PL=get_predicted_values(mydata,c('2009', '2010','2011'), "len_last")
len_last_PL$PL[len_last_PL$Nat_Predicted<0]=1
len_last_PL$PL=1-(len_last_PL$Nat_Predicted/len_last_PL$HP_Predicted)



ggplot(len_last_PL, aes(y=len_last_PL$PL,x=len_last_PL$X_values,  group=Year)) + 
        geom_line(aes(linetype=Year)) +
        scale_y_continuous(limits = c(0,1))
scale_x_continuous(limits = c(-2,3)) 

#
mid_wid_PL=get_predicted_values(mydata,c('2009', '2010','2011'), "mid_wid")
mid_wid_PL$PL=1-(mid_wid_PL$Nat_Predicted/mid_wid_PL$HP_Predicted)

library(ggplot2)

ggplot(mid_wid_PL, aes(y=mid_wid_PL$PL,x=mid_wid_PL$X_values,  group=Year)) + 
        geom_smooth(aes(linetype=Year)) +
        scale_x_continuous(limits = c(-3,3)) +
        scale_y_continuous(limits = c(0,1))

#
avg_display_PL=get_predicted_values(mydata,c('2009', '2010','2011'), "avg_display")
avg_display_PL$PL=1-(avg_display_PL$Nat_Predicted/avg_display_PL$HP_Predicted)

library(ggplot2)

ggplot(avg_display_PL, aes(y=avg_display_PL$PL,x=avg_display_PL$X_values,  group=Year)) + 
        geom_line(aes(linetype=Year)) +
        # scale_x_continuous(limits = c(-3,3)) +
        scale_y_continuous(limits = c(0,1))

#
med_flr_date_PL=get_predicted_values(mydata,c('2011'), "med_flr_date")
med_flr_date_PL$Nat_Predicted[(med_flr_date_PL$Nat_Predicted<0)]=0
med_flr_date_PL$PL=1-(med_flr_date_PL$Nat_Predicted/med_flr_date_PL$HP_Predicted)

library(ggplot2)

ggplot(med_flr_date_PL, aes(y=med_flr_date_PL$PL,x=med_flr_date_PL$X_values,  group=Year)) + 
        geom_line(aes(linetype=Year)) +
        # scale_x_continuous(limits = c(-3,3)) +
        scale_y_continuous(limits = c(0,1))
