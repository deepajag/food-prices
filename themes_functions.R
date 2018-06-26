gg.theme = function(g){
  p = g + theme(strip.text.x = element_text(size=10, face="bold", family="Arial",color='white'),
                axis.text = element_text(size=10, color = "black",family="Arial"),
                plot.title = element_text(size=11,  family="Arial", face="bold"),
                panel.background = element_rect(fill="white", colour = "black"),
                plot.background = element_rect(fill="grey95"),
                panel.grid = element_blank(), 
                axis.title = element_text(size=11, color = "black",family="Arial", face="bold"),
                strip.background = element_rect(fill='grey20'),plot.caption=element_text(size=8)
  )
  return(p)
}


labels <- c(fruit = "Fruit", vegetable = "Vegetable")
values2 = c('darkred','midnightblue')
values3 = c('darkred','royalblue3','darkgoldenrod3')

get.models = function(xvar,ds, re=TRUE){
  
  m1 = lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar,"  + year + 
                              year2 + (1 | rls_code)")), data = ds)
  m2 = lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + year + 
                              year2 + bs(month,4) + (1 | rls_code)")), data = ds) 
  m3 = lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + year + 
                              year2  + bannerid + bs(month,4) +  (1 | rls_code) ")), data = ds) 
  m4 = lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + year + 
                              year2  + bannerid + bs(month,4) + 
                              quintmat + predict.pop + zone11 + (1 | rls_code)")), data = ds) 
  m5 = lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + factor(year) + factor(month) + bannerid + (1 | rls_code)")), data = ds) 
  m6 = lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + factor(year) + bs(month,4) + bannerid + (1 | rls_code)")), data = ds) 
  m7 = lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + bs(yr.month,4) + bannerid + (1 | rls_code)")), data = ds)
  m8 = lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + as.numeric(year) + factor(month) + (1 | rls_code)")), data = ds) 
  
  return(m=list(m1,m2,m3,m4,m5,m6,m7,m8))
}

get.results = function(xvar="super.density",method="Wald",m){
  #...m Must be a list of formulas
  aic = lapply(m,function(x) AIC(x))
  bic = lapply(m,function(x) BIC(x))
  coeff1 = lapply(m,function(x) summary(x)$coefficients[xvar,1])
  all.coeff = lapply(m,function(x) summary(x)$coefficients[,1])
  ci = lapply(m,function(x) confint(x,method=method, parallel="multicore"))
  low.ci = lapply(ci,function(x) x[xvar,1])
  high.ci = lapply(ci,function(x) x[xvar,2])
  
  ##Compare all models in the list
  r.model.compare = list(model=seq(1,length(m), by=1),
                         coefficient=coeff1, aic=aic,bic=bic,low.ci=low.ci,high.ci=high.ci)
  r.model.compare = data.frame(do.call(rbind,r.model.compare))
  g=do.call(rbind,r.model.compare)
  
  ##Output list of all results by model
  all = vector('list',length(m))
  re.only = vector('list',length(m))
  for(i in 1:length(m)){
    all[[i]] = data.frame(coefficients=all.coeff[[i]], 
                          conf = ci[[i]][-c(1,2),])
    colnames(all[[i]]) = c('coefficients','2.5%','95%')
    re = data.frame(VarCorr(m[[i]]))
    re.only[[i]] =  data.frame(re, 
                               conf = ci[[i]][c(1,2),])
  }
  
  l = as.data.frame(unclass(g))
  l = data.frame(apply(l,2,function(x) unlist(x)))
  
  return(list(results=l,raw=all, re.mat=re.only))
}

anonymous <- function(all.ds=all.ds,m,x,d,
                      #reference,
                      convert.rr=TRUE,
                      outcome){
  #m=m2.income1; x = names(coefficients(m2.income1)); d=all.ds[,cluster.var]
  ##Predict Values
  #all.ds$predicted[all.ds$source=='nielsen'] = 
  #predict(m,newdata=all.ds[all.ds$source=='nielsen',] ,type='response')
  all.ds$predicted = predict(m,newdata=all.ds,type='response')
  #reference$predicted = predict(m,newdata= reference,type='response')
  #predicted_outcome = 'predicted'
  
  ##Model Results
  coefficients=m$coefficients[x]
  
  
  ci = confint(m,method="Wald")[x,]
  vcov1 <- cluster.vcov(m,d)
  
  new.se1 = coeftest(m, vcov1)
  coefficient1.ci.low = round(new.se1[x,'Estimate'] + 
                                new.se1[x,'Std. Error']*-1*qnorm(0.95),3)
  coefficient1.ci.high = round(new.se1[x,'Estimate'] + 
                                 new.se1[x,'Std. Error']*qnorm(0.95),3)
  
  results = matrix(c(coefficients, ci, coefficient1.ci.low, coefficient1.ci.high),nrow=length(x))
  colnames(results) <- c("coefficient", "low.ci", "high.ci","cluster.se.ci.low", "cluster.se.ci.high")
  rownames(results) <- x
  
  if(convert.rr==TRUE){
    r.all <- results
    #arguments <- list(...)
    #avg.risk<-mean(reference[,outcome],na.rm=TRUE)
    #rr <- round(r.all/((1-avg.risk) + (avg.risk*r.all)),2)[,1]
    #ref.p <- mean(reference[,predicted_outcome],na.rm=TRUE)
    
    #p <-lapply(arguments,function(x) mean(x[,predicted_outcome],na.rm=TRUE))
    #rd <- lapply(p,function(x) x-ref.p)
    rd <- as.list(coefficients)
    abs <- lapply(rd, function(x) x*length(unique(all.ds$bin)) )
    
    #r.all <- cbind(r.all,rr)
    #r.all <- cbind(r.all,rd=c(do.call(rbind,rd))); 
    r.all <- cbind(r.all,abs=c(do.call(rbind,abs)))
    results <- r.all
    
  }
  
  return(results)
  
}

setwd('~/analysis_tmp/Nielsen')
save.image('themes_functions.Rdata')