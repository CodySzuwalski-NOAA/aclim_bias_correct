library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)
library(mgcv)

#==ice is driving temperature, but temperature also drives ice. if you biase correct temperature, that will impact ice, which impacts temperature.
#==bias correcting variables involved in feedback loops is particularly nasty and headache inducing
#==
load('data/bias_indices_KK_Jan2025.RData')

use_ind<-c("aice","temp_bottom5m","pH_bottom5m","TIC_bottom5m", "prod_integrated")

df1<-expand.grid(use_ind,use_ind)
comparisons<-df1[!duplicated(t(apply(df1, 1, sort))),]
comparisons<-comparisons[comparisons[,1]!=comparisons[,2],]

historical<-filter(d,bctype=="raw"&year>1979&year<2015&index%in%use_ind)
projected<-filter(d,bctype=="raw"&year>=2015&index%in%use_ind)
corrected<-filter(d,bctype!="raw"&year>=2015&index%in%use_ind)

#==quick comparison for just historical
my_fn <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=method, ...)
  p
}

casted_hist<-dcast(filter(historical,ssp=="ssp585"&parent=='GFDL'),year~index,value.var='value')
hist_rel_plot<-ggpairs(casted_hist[,-1],lower = list(continuous = my_fn),upper=list(continuous = my_fn)) +theme_bw() 
png("ggpairs.png",width=6,height=6,units='in',res=300)
print(hist_rel_plot)
dev.off()

casted_proj<-dcast(filter(projected,ssp=="ssp585"&parent=='GFDL'),year~index,value.var='value')
proj_rel_plot<-ggpairs(casted_proj[,-1],lower = list(continuous = my_fn),upper=list(continuous = my_fn)) +theme_bw() 
png("ggpairs_proj.png",width=6,height=6,units='in',res=300)
print(proj_rel_plot)
dev.off()

#===================================
# historical
keep_terms<-NULL
for(y in 1:nrow(comparisons))
{
 tmpp<-filter(historical,ssp=="ssp585"&parent=='GFDL'&index%in%c(unlist(comparisons[y,])))
 ugh<-dcast(tmpp,year~index,value.var='value')
 orig_name<-colnames(ugh)
 colnames(ugh)<-c('year','dat1','dat2')
 mod<-gam(data=ugh,dat1~s(dat2,k=5))
 xrange<-data.frame(dat2=seq(min(ugh$dat2),max(ugh$dat2),length.out=60))
 pred<-predict(mod,newdata=xrange,type='response',se.fit=T)
 temp<-data.frame(x=xrange$dat2,
                   y=pred$fit,
                   y_up=pred$fit+pred$se,
                   y_dn=pred$fit-pred$se,
                  xaxis=orig_name[2],
                  yaxis=orig_name[3],
                   scenario="historical")
  keep_terms<-rbind(keep_terms,temp)
}

#===================================
# projected
for(y in 1:nrow(comparisons))
{
  tmpp<-filter(projected,ssp=="ssp585"&parent=='GFDL'&index%in%c(unlist(comparisons[y,])))
  ugh<-dcast(tmpp,year~index,value.var='value')
  orig_name<-colnames(ugh)
  colnames(ugh)<-c('year','dat1','dat2')
  mod<-gam(data=ugh,dat1~s(dat2,k=5))
  xrange<-data.frame(dat2=seq(min(ugh$dat2),max(ugh$dat2),length.out=60))
  pred<-predict(mod,newdata=xrange,type='response',se.fit=T)
  temp<-data.frame(x=xrange$dat2,
                   y=pred$fit,
                   y_up=pred$fit+pred$se,
                   y_dn=pred$fit-pred$se,
                   xaxis=orig_name[2],
                   yaxis=orig_name[3],
                   scenario="projected")
  keep_terms<-rbind(keep_terms,temp)
  }
# corrected
for(y in 1:nrow(comparisons))
{
  tmpp<-filter(corrected,ssp=="ssp585"&parent=='GFDL'&bctype=='QDM_add'&index%in%c(unlist(comparisons[y,])))
  ugh<-dcast(tmpp,year~index,value.var='value')
  orig_name<-colnames(ugh)
  colnames(ugh)<-c('year','dat1','dat2')
  mod<-gam(data=ugh,dat1~s(dat2,k=5))
  xrange<-data.frame(dat2=seq(min(ugh$dat2),max(ugh$dat2),length.out=60))
  pred<-predict(mod,newdata=xrange,type='response',se.fit=T)
  temp<-data.frame(x=xrange$dat2,
                   y=pred$fit,
                   y_up=pred$fit+pred$se,
                   y_dn=pred$fit-pred$se,
                   xaxis=orig_name[2],
                   yaxis=orig_name[3],
                   scenario="corrected")
  keep_terms<-rbind(keep_terms,temp)
}


comp_plot<-ggplot(keep_terms)+
  geom_line(aes(x=x,y=y,col=scenario),lwd=2)+
  geom_ribbon(aes(x=x,ymin=y_dn,ymax=y_up,fill=scenario),alpha=0.3,lwd=2)+
  theme_bw()+
  theme(legend.position=c(.9,.1),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        legend.key.size=unit(.7, 'lines'))+
  facet_wrap(~Var1+Var2,scales='free')

comp_plot<-ggplot(keep_terms)+
  geom_line(aes(x=x,y=y,col=scenario),lwd=2)+
  geom_ribbon(aes(x=x,ymin=y_dn,ymax=y_up,fill=scenario),alpha=0.3,lwd=2)+
  theme_bw()+
  theme(legend.position=c(.1,.1),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=8),
        legend.key.size=unit(.7, 'lines'))+
  facet_grid(rows=vars(xaxis),cols=vars(yaxis),scales = 'free')

png("all_rel.png",width=9,height=9,units='in',res=300)
print(comp_plot)
dev.off()