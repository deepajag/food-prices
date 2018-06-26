
##1) IMPUTATION
#####Imputation 
missing.rls = unique(imputable.rls$rls_code)

imputation = function(x){
missing = missing.weeks %>% filter(rls_code %in% x$rls_code)
price.ts = ts(x$mean.real5, start=c(2008 , 1),freq=length(unique(x$periodid))/6)
y=price.ts

fit = auto.arima(price.ts, stationary=FALSE, seasonal=TRUE)
ord = arimaorder(fit)
kr <- KalmanSmooth(price.ts, fit$model)
# impute missing values Z %*% alpha at each missing observation
id.na <- which(is.na(price.ts))

for (i in id.na){
  y[i] <- fit$model$Z %*% kr$smooth[i,]
}

x[id.na,'mean.real5'] <- y[id.na]
g=ggplot(x, aes(periodid,mean.real5)) + geom_line() + 
geom_point(data=x %>% filter(periodid %in% missing$periodid),aes(periodid,mean.real5), color="blue", size=2.5) + labs(x=NULL,y="Mean Weekly Price \n for Five Servings ($)") + 
annotate("text",  x=max(x$periodid), y = max(x$mean.real5),hjust=1,label=paste("ARIMA(",toString(ord),")",sep="")) 

return(list(x,g))
}
all.rls[which(all.rls$rls_code %in% missing.rls),] <- imputation.data
##Exclude 0803 because imputation didn't work
all.rls = all.rls %>% filter(rls_code != "0803")

##2) K-MEANS CLUSTERING
rls.means = all.rls %>% group_by(year,month,yr.week,rls_code) %>% 
summarise(mean.real5=mean(mean.real5)) %>% data.frame()

cluster.function <- function(d=banner.means,time="yr.month",var='bannerid',k=3){
set.seed(1234)
  d.wide = reshape(d[,c(var,'mean.real5',time)],v.names = 'mean.real5',
                   idvar = var, 
                   direction = 'wide', timevar = time)
  n = cld(d.wide,varNames = c('mean.real5'))
  new = kml(n)
  cluster.key = data.frame(var = d.wide[[var]], cluster = getClusters(n,k))
  return(cluster.key)
}

cluster.key = cluster.function(d=rls.means,var='rls_code',k=2, time="yr.week")
cluster.key = cluster.key %>% rename(rls_code=var)

rls.means = rls.means %>% left_join(cluster.key)
if (mean(rls.means$mean.real5[rls.means$cluster=="A"]) > 
    mean(rls.means$mean.real5[rls.means$cluster=="B"])) {
    cluster.key$cluster = factor(cluster.key$cluster, levels=c("B", "A"), labels=c("Low", "High"))
} else {
        cluster.key$cluster = factor(cluster.key$cluster, levels=c("A", "B"), labels=c("High", "Low"))  
    }

##Join clusters back to main data
all.rls = all.rls %>% left_join(cluster.key)

##3) TEMPORAL TRENDS
bs.by.time = all.rls %>% group_by(periodid) %>% summarise(obs=n())
all.rls = all.rls %>% left_join(obs.by.time)

time.means = all.rls %>% group_by(yr.week) %>% summarise(price=weighted.mean(mean.real5,obs))
time.means$yr.week2 = time.means$yr.week^2

time.means.cluster = all.rls %>% group_by(yr.week,cluster) %>% summarise(price=weighted.mean(mean.real5,obs))
time.means.cluster$yr.week2 = time.means.cluster$yr.week^2

temporal = lm(price ~ yr.week, data = time.means)
temporal2 = lm(price ~ yr.week + yr.week2, data = time.means)
bic1=BIC(temporal);bic2=BIC(temporal2)

lrtest(temporal,temporal2)


##4) SEASONAL TRENDS
##Harmonic Seasonal trend
##Need 1 observation per time point
rls.means2 = all.rls %>% group_by(yr.week,yr.week2,block) %>% 
summarise(mean.real5=mean(mean.real5)) %>% ungroup() %>% mutate(lag1 = lag(mean.real5,1))

####MODEL 
reg1 = lm(mean.real5 ~  cos(2*pi*yr.week/52.17) + 
            sin(2*pi*yr.week/52.17) + 
            cos(4*pi*yr.week/52.17) + 
            sin(4*pi*yr.week/52.17) + 
            cos(6*pi*yr.week/52.17) + 
            sin(6*pi*yr.week/52.17) + 
            yr.week + yr.week2, data = rls.means2)

 ###BOOTSTRAP
resamples = function(i,d=all.rls,cl='block', cl2='TRUE'){

cls <- sample(unique(d[[cl]]), replace=TRUE)
# subset on the sampled clustering factors
sub <- lapply(cls, function(b) subset(d, d[[cl]]==b))
x = do.call(rbind, sub)

colnames(x) <- colnames(d)

x1=x %>% group_by(yr.week,yr.week2) %>% summarise(mean.real5=mean(mean.real5))

reg1 = lm(mean.real5 ~  cos(2*pi*yr.week/52.17) + 
            sin(2*pi*yr.week/52.17) + 
            cos(4*pi*yr.week/52.17) + 
            sin(4*pi*yr.week/52.17) + 
            cos(6*pi*yr.week/52.17) + 
            sin(6*pi*yr.week/52.17) + 
            yr.week + yr.week2, data = x1)

amplitude1 = sqrt(coefficients(reg1)[2]^2 + coefficients(reg1)[3]^2) +
sqrt(coefficients(reg1)[4]^2 + coefficients(reg1)[5]^2) +
sqrt(coefficients(reg1)[6]^2 + coefficients(reg1)[7]^2) 

if(cl2=='TRUE'){
x2=x %>% filter(cluster=="Low") %>% group_by(yr.week,yr.week2) %>% summarise(mean.real5=mean(mean.real5))
x3=x %>% filter(cluster=="High") %>% group_by(yr.week,yr.week2) %>% summarise(mean.real5=mean(mean.real5))

reg1 = lm(mean.real5 ~  cos(2*pi*yr.week/52.17) + 
            sin(2*pi*yr.week/52.17) + 
            cos(4*pi*yr.week/52.17) + 
            sin(4*pi*yr.week/52.17) + 
            cos(6*pi*yr.week/52.17) + 
            sin(6*pi*yr.week/52.17) + 
            yr.week + yr.week2, data = x2)

amplitude.low = sqrt(coefficients(reg1)[2]^2 + coefficients(reg1)[3]^2) +
sqrt(coefficients(reg1)[4]^2 + coefficients(reg1)[5]^2) +
sqrt(coefficients(reg1)[6]^2 + coefficients(reg1)[7]^2)   

reg1 = lm(mean.real5 ~  cos(2*pi*yr.week/52.17) + 
            sin(2*pi*yr.week/52.17) + 
            cos(4*pi*yr.week/52.17) + 
            sin(4*pi*yr.week/52.17) + 
            cos(6*pi*yr.week/52.17) + 
            sin(6*pi*yr.week/52.17) + 
            yr.week + yr.week2, data = x3)

amplitude.high= sqrt(coefficients(reg1)[2]^2 + coefficients(reg1)[3]^2) +
sqrt(coefficients(reg1)[4]^2 + coefficients(reg1)[5]^2) +
sqrt(coefficients(reg1)[6]^2 + coefficients(reg1)[7]^2)   


result = data.frame(amplitude.overall=amplitude1,amplitude.high=amplitude.high,amplitude.low=amplitude.low)
} else {
result = data.frame(amplitude.overall=amplitude1)
}

return(result)
}

bs_result = lapply(1:250, resamples)
result=do.call(rbind,bs_result)
ci.result=as.data.frame(t(apply(result,2,function(x) quantile(x, c(.025, .50, .975)))))
colnames(ci.result) = c("low.ci","mean","high.ci")
ci.result = ci.result %>% select(mean, low.ci, high.ci)
ci.result=round(ci.result,2)