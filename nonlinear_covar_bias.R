library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)

load('data/bias_indices_KK_Jan2025.RData')
names(d)
unique(d$index)
ggplot(filter(d,ssp=="ssp585"&index=='aice'))+
  geom_line(aes(x=as.numeric(year),y=value,group=bctype,col=bctype))+
  facet_grid(index~parent,scales='free_y')+
  theme_bw()

png("ice_vars.png",width=9,height=6,units='in',res=300)
ggplot(filter(d,ssp=="ssp585"&index=='aice'&parent=="GFDL"))+
  geom_line(aes(x=as.numeric(year),y=value,group=bctype,col=bctype),lwd=1.2,alpha=.8)+
  facet_grid(index~parent,scales='free_y')+
  theme_bw()
dev.off()

  unique(d$index)
  
  
  use_ind<-c("aice","temp_bottom5m","pH_bottom5m","TIC_bottom5m",
             "prod_integrated","sep_prod")
  

raw_rel <- filter(d,ssp=="ssp585",index%in%use_ind,bctype=='raw',parent=='GFDL')
casted_raw<-dcast(raw_rel,year~index,value.var='value')

my_fn <- function(data, mapping, method="gam", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}
raw_rel_plot<-ggpairs(casted_raw[,-1],lower = list(continuous = my_fn))  


qdm_rel <- filter(d,ssp=="ssp585",index%in%use_ind,bctype=='QDM_mult',parent=='GFDL')
casted_qdm<-dcast(qdm_rel,year~index,value.var='value')

qdm_rel_plot<-ggpairs(casted_qdm[,-1],lower = list(continuous = my_fn))  

use_ind2<-unique(d$index)[-c(6,8,9,19)]
qdm_rel_all <- filter(d,ssp=="ssp585",index%in%use_ind2,bctype=='QDM_mult',parent=='GFDL')
casted_qdm_all<-dcast(qdm_rel_all,year~index,value.var='value')

qdm_rel_plot<-ggpairs(casted_qdm_all[,-1],lower = list(continuous = my_fn))  +theme_bw()

png("corrs.png",width=12,height=12,units='in',res=300)
print(qdm_rel_plot)
dev.off()
#==the actual values matter--water freezes at about 0 degrees 
#==make a model to predict one variable from another
#==
library(mgcv)
ice_mod<-gam(data=casted_raw,sep_prod ~s(pH_bottom5m)+s(prod_integrated)+s(aice))
summary(ice_mod)
plot(ice_mod,pages=1)
orig_pred<-predict(ice_mod)
bc_pred<-predict(ice_mod,newdata=casted_qdm[,c(3,4,2)])

plot(orig_pred~casted_raw$year,type='l',ylim=c(0,700))
lines(bc_pred~casted_raw$year,col=2)
lines(casted_qdm$sep_prod~casted$year,col=3)


#======jus tlook at ice and bot temp
ice_mod<-gam(data=casted_raw,aice ~s(temp_bottom5m))
summary(ice_mod)
plot(ice_mod,pages=1)
orig_pred<-predict(ice_mod)
bc_pred<-predict(ice_mod,newdata=data.frame(temp_bottom5m=casted_qdm[,c(6)]))

#==based on the relationship in the GCM
plot(orig_pred~casted_raw$year,type='l')
lines(bc_pred~casted_raw$year,col=2)
lines(casted_qdm$aice~casted$year,col=3)

#======jus tlook at ice and bot temp
ice_mod<-gam(data=casted_raw,temp_bottom5m ~s(aice))
summary(ice_mod)
plot(ice_mod,pages=1)
orig_pred<-predict(ice_mod)
bc_pred<-predict(ice_mod,newdata=data.frame(aice=casted_qdm[,c(2)]))

#==based on the relationship in the GCM
plot(orig_pred~casted_raw$year,type='l',ylim=c(0,5))
lines(bc_pred~casted_raw$year,col=2)
lines(casted_qdm$temp_bottom5m~casted$year,col=3)

#==ph + prod (dome-shaped)
prod_mod<-gam(data=casted_raw,sep_prod ~s(pH_bottom5m))
summary(prod_mod)
plot(prod_mod,pages=1)
orig_pred<-predict(prod_mod)
bc_pred<-predict(prod_mod,newdata=data.frame(pH_bottom5m=casted_qdm[,c(3)]))

#==based on the relationship in the GCM
plot(orig_pred~casted_raw$year,type='l',ylim=c(000,600))
lines(bc_pred~casted_raw$year,col=2)
lines(casted_qdm$sep_prod~casted$year,col=3)

#======jus tlook at ice and bot temp
sep_mod<-gam(data=casted_raw,sep_prod ~s(temp_bottom5m)+s(pH_bottom5m))
summary(sep_mod)
plot(sep_mod,pages=1)
orig_pred<-predict(sep_mod)
bc_pred<-predict(sep_mod,newdata=data.frame(temp_bottom5m=casted_qdm[,c(6)],
                                            pH_bottom5m=casted_qdm[,c(3)]))

#==based on the relationship in the GCM
plot(orig_pred~casted_raw$year,type='l',ylim=c(0,500))
lines(bc_pred~casted_raw$year,col=2)
lines(casted_qdm$sep_prod~casted$year,col=3)
