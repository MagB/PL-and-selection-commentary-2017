library(ggplot2)
#This script generates Individual Pollen Limitation (IPL) plots used to create Figure 1 in the New Phytologist commentary by Johnston and Bartkowska (2017). 

#The strength of selection on traits subject to pollinator-mediated selection Pollen limitation is expected to be influenced by
#the degree of pollen limitation. Intuitively, stronger pollen limitaion should result in stronger competition among plants for pollen. Thus, 
#traits promoting pollinator service and enhanced pollen receipt should be subject to stronger selection. 
#Studies which report PL and selection do not always find a strong clear relationship between the two measures. 
#several factors will potentially reduce the strength of the relationship. Uncertainty in both measures will also obscure the 
#relationship between interaction intensity and selection.


#IPL is the difference in predicted seed number between linear models fit to hand-pollinated and naturally pollinated plants. 
# models are: Total seed = Intercept + (mean standardized trait)

#Here we use three years of observations of a population of Lobelia cardinalis located along the shores of lake Traverse in Algonqion park ON. 

#The data used in this plot was also used in Bartkowska and Johnston 2015 JEB
#This script is partitioned into 3 components:1) estimating selection differentials, 2)estimating IPL and 3)generating the plot of IPL vs trait

mydata=read.csv("data/part2_all_years_data_for_pub.csv", sep=";", stringsAsFactors = F)
str(mydata)

#In the commentary, we show IPL for two traits: anther-nectary distance (and) and flower number (flr_num)
#For each year standardize the hand pollinated and naturally pollinated groups using a common mean
for(i in c('2009','2010','2011')){
        mydata$st_and[mydata$Year==i]= (mydata$and[mydata$Year==i]-mean(mydata$and[mydata$Year==i],na.rm=T))/sd(mydata$and[mydata$Year==i],na.rm=T) 
}
range( mydata$st_and,na.rm = T)

for(i in c('2009','2010','2011')){
        mydata$st_flr_num[mydata$Year==i]= (mydata$flr_num[mydata$Year==i]-mean(mydata$flr_num[mydata$Year==i], na.rm=T))/sd(mydata$flr_num[mydata$Year==i],na.rm=T) 
}

range( mydata$st_flr_num,na.rm = T)

#################################################################################

#The data are split into the two treatments s1 for naturally pollinated plants and s2 for hand-pollinated plants
#There are 3 years of data.


########Part 1. Estimating selection differentials:

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

#Call the function "raw_regression_univariate"
raw_regression_univariate(mydata,'2011', 's2',c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"))

#get_function=function(raw_data,years, trait=c("and","mid_wid", "len_last", "flr_num","avg_display","height_last", "med_flr_date"),y="total_seed" ){
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

########Part 2. Estimating IPL 
#IPL is the difference in predicted seed number between linear models fit to hand-pollinated and naturally pollinated plants. 
# models are: Total seed = Intercept + (mean standardized trait)
#the function "get_predicted_values" finds the predicted values of the relationship between Total seed number and trait value for naturally and  hand-pollinated plants
#The output from the fitted functions is printed to the console. A dataframe with the range of trait values observed in the population along with the predicted seed number 
#is created.
#IPL then is calculated and stored as a variable called "PL" in this dataframe.

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

and_PL=get_predicted_values(mydata,c('2009','2010', '2011'), "and")
and_PL$PL=1-(and_PL$Nat_Predicted/and_PL$HP_Predicted)

flr_num_PL=get_predicted_values(mydata,c('2009','2010', '2011'), "flr_num")
flr_num_PL$PL=1-(and_PL$Nat_Predicted/and_PL$HP_Predicted)

########Part 3. Generate plot of IPL vs standardized trait value (note the x axis is scaled to reflect the range of trait values observed in nature)
library(ggplot2)
yname=expression(paste("Individual Pollen Limitation, ",widehat(IPL)))
title_bold_and=expression(paste(bold("(a)"), " Anther-nectary distance"))

and_PL$Year=factor(and_PL$Year, levels=c("2009", "2010", "2011"))

and_plot=ggplot(and_PL, aes(y=and_PL$PL,x=and_PL$X_values,  group=Year)) + 
        geom_line(aes(color=Year)) + scale_color_manual(values=c("coral1", "magenta4", "deepskyblue"),labels=c("2009 PL=0.36", "2010 PL=0.63 ", "2011 PL=0.20"))+
        #scale_fill_discrete(labels=c("2009 PL=", "2010 PL= ", "2011 PL="))+
        xlab("Z") + 
        ylab(yname)+         
        ggtitle(title_bold_and)+
        scale_colour_discrete(labels=c("2009 PL=0.36", "2010 PL=0.63 ", "2011 PL=0.20"))+
        scale_x_continuous(limits = c(-4,4)) +
        scale_y_continuous(limits = c(0,1),expand = c(0, 0))+ 
        theme(plot.margin = unit(c(0.05,0.1,0,0.05), "cm"),
                plot.title = element_text(face="plain",hjust = -1.35, size = "12"),
                axis.title.y = element_text(colour = "black", size = "12"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color="black"),
                axis.line.y = element_line(color="black"))

#
flr_num_PL=get_predicted_values(mydata,c('2009', '2010','2011'), "flr_num")
flr_num_PL$PL=1-(flr_num_PL$Nat_Predicted/flr_num_PL$HP_Predicted)
flr_num_PL$Year=factor(flr_num_PL$Year, levels=c("2009", "2010", "2011"))

title_bold=expression(paste(bold("(b)"), " Flower number"))
flr_num_plot=ggplot(flr_num_PL, aes(y=flr_num_PL$PL,x=flr_num_PL$X_values,  group=Year)) + 
        geom_line(aes(color=Year)) + scale_color_manual(values=c("coral1", "magenta4", "deepskyblue"))+ #use this to customize colours
        scale_colour_discrete(labels=c("2009 PL=0.36", "2010 PL=0.63 ", "2011 PL=0.20"))+
         scale_x_continuous(limits = c(-1.35,3.8)) +         
        scale_y_continuous(limits = c(0,1),expand = c(0, 0))+ 
        xlab("Z") + 
        ylab(yname)+
        ggtitle(title_bold)+
        #ggtitle("(b) Flower number")+
        theme(plot.margin = unit(c(0.05,0.1,0,0.05), "cm"),
                plot.title = element_text(face="plain",hjust = -1.35, size = "12"),
                axis.title.y = element_text(colour = "black", size = "12"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line.x = element_line(color="black"),
                axis.line.y = element_line(color="black"),
                legend.key = element_rect(colour = "transparent", fill = "white"))    

#Create a multi-panel plot. This function was written by Hadley.
#https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
library(ggplot2)
library(gridExtra)
library(grid)


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
        
        plots <- list(...)
        position <- match.arg(position)
        g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
        gl <- lapply(plots, function(x) x + theme(legend.position="none"))
        gl <- c(gl, ncol = ncol, nrow = nrow)
        
        combined <- switch(position,
                "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                        legend,
                        ncol = 1,
                        heights = unit.c(unit(1, "npc") - lheight, lheight)),
                "right" = arrangeGrob(do.call(arrangeGrob, gl),
                        legend,
                        ncol = 2,
                        widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
        grid.newpage()
        grid.draw(combined)
        
        # return gtable invisibly
        invisible(combined)
        
}

quartz(width = 8, height  = 5.5, family = "Helvetica")
tiff(file = "plots.tiff", width = 8, height  = 5.5, units = "in", family = "Helvetica", res = 600, pointsize = 12, bg="transparent")
grid_arrange_shared_legend(and_plot, flr_num_plot, ncol = 2, nrow = 1,position = "right")
quartz.save("Figure.jpg", type = "jpg", device = dev.cur(),  dpi = 1000, pointsize = 10, bg="transparent")
dev.off()

setEPS()
postscript("figures/IPL_March_2017.eps",width = 8, height  = 5.5,  family = "Helvetica", )
grid_arrange_shared_legend(and_plot, flr_num_plot, ncol = 2, nrow = 1,position = "right")
dev.off()
