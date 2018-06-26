

##1) CLASSIFY SUPERMARKETS
chain.prices = dat %>% group_by(bannerid,storeid,year) %>% summarise(mean.real = weighted.mean(real.price,weight)) %>% as.data.frame()

model.prices <- lm(log(mean.real) ~ bannerid + year, data = chain.prices)
predictions <- as.data.frame(exp(predict(model.prices, data = chain.prices, type="response",interval="predict")))
chain.prices <- cbind(chain.prices, predictions)

chain.means = dat %>% group_by(bannerid,year) %>% filter(bannerid != '347') %>% summarise(mean.real = weighted.mean(real.price,weight)) %>% as.data.frame()

##Combine levels based on mean prices
chain.means$level <- rep(NA,nrow(chain.means))
chain.means$level[chain.means$bannerid %in% c("306","335")] <- 0
chain.means$level[chain.means$bannerid %in% c("248","286","315")] <- 1
chain.means$level[chain.means$bannerid %in% c("299","372","336","9048")] <- 2
chain.means$level <- factor(chain.means$level, levels=c(0,1,2), labels=c('high','med','low'))

##test for differences - all very different
chain.model = lm(log(mean.real) ~ factor(level) + year, data = chain.means)
chain.model.ci = exp(confint(chain.model))
med = paste(round(exp(chain.model$coefficients['factor(level)med']),2),'(',toString(round(chain.model.ci['factor(level)med',],2)),')',sep="")
low = paste(round(exp(chain.model$coefficients['factor(level)low']),2),'(',toString(round(chain.model.ci['factor(level)low',],2)),')',sep="")
chain.model.matrix = matrix(c(1,med,low), nrow=3)
rownames(chain.model.matrix) = c('High(Ref)','Medium','Low')
colnames(chain.model.matrix) = c('Expected cents \n per serving (95% CI)')

g.boxplot = ggplot(chain.means %>% group_by(bannerid,level) %>% summarise(means= mean(mean.real)), aes(factor(level), means, group=level)) + geom_boxplot() + labs(x = "Chain Price Point",y="Mean Serving Price for Standard Basket (Cents)" ) + scale_x_discrete(labels=c('High','Medium','Low'))


##2) MODELLING
##Main Function
anonymous <- function(all.ds=all.ds,m,x,d,
                      #reference,
                      convert.rr=TRUE,
                      outcome){

  all.ds$predicted = predict(m,newdata=all.ds,type='response')
 
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
  return(results)
  
}

##Availability
m1 = lm(available.all.binary ~ level  +  bs(year.month,4) + log(pop.density), data = all.ds)
m1.income1 = lm(available.all.binary ~ income_cut +  bs(year.month,4) + log(pop.density), data = all.ds)
m1.income2 = lm(available.all.binary ~ level*income_cut + bs(year.month,4) + log(pop.density), data = all.ds)

availability1.result = anonymous(all.ds,m=m1,x=names(coefficients(m1)),
          d=all.ds[,cluster.var] ,
          outcome='pp.available.all')
availability1 = availability1.result[c('levelmed','levellow'),]
availability1.all = availability1.result


availability.income1.result = anonymous(all.ds,m=m1.income1,x=names(coefficients(m1.income1)),
          d=all.ds[,cluster.var],
          outcome='pp.available.all')
availability.income1 = availability.income1.result[c('income_cutlow.income'),]
availability.income1.all = availability.income1.result

availability.income2.result = anonymous(all.ds,m=m1.income2,x=names(coefficients(m1.income2)),
          d=all.ds[,cluster.var ],
          outcome='pp.available.all')
availability.income2 = availability.income2.result[c('levelmed','levellow','income_cutlow.income','levelmed:income_cutlow.income','levellow:income_cutlow.income'),]
availability.income2.all = availability.income2.result

##Variety
m2 = glm(item.m.bin ~ level +  bs(year.month,4), data = all.ds %>% filter(!bin %in% exclude.bins$bin), family=quasipoisson)
m2.income1 = glm(item.m.bin ~ income_cut + bs(year.month,4) + log(pop.density), data = all.ds %>% filter(!bin %in% exclude.bins$bin), family=quasipoisson)
m2.income2 = glm(item.m.bin ~ level*income_cut + bs(year.month,4) + log(pop.density), data = all.ds %>% filter(!bin %in% exclude.bins$bin), family=quasipoisson)

acceptability1.result = anonymous(all.ds = all.ds %>% filter(!bin %in% exclude.bins$bin),m=m2,x=names(coefficients(m2)),
          d=all.ds[!all.ds$bin %in% exclude.bins$bin,cluster.var] ,
          convert.rr=FALSE,
          outcome='item.m.bin')
acceptability1 = acceptability1.result[c('levelmed','levellow'),]
acceptability1.all = acceptability1.result

acceptability.income1.result = anonymous(all.ds = all.ds %>% filter(!bin %in% exclude.bins$bin),m=m2.income1,x=names(coefficients(m2.income1)),
          d=all.ds[!all.ds$bin %in% exclude.bins$bin,cluster.var],
          outcome='item.m.bin')
acceptability.income1 = acceptability.income1.result[c('income_cutlow.income'),]
acceptability.income1.all = acceptability.income1.result

acceptability.income2.result = anonymous(all.ds = all.ds %>% filter(!bin %in% exclude.bins$bin),m=m2.income2,x = names(coefficients(m2.income2)),
          d=all.ds[!all.ds$bin %in% exclude.bins$bin,cluster.var],
          convert.rr=FALSE,
          outcome='item.m.bin')
acceptability.income2 = acceptability.income2.result[c('levelmed','levellow','income_cutlow.income','levelmed:income_cutlow.income','levellow:income_cutlow.income'),]
acceptability.income2.all = acceptability.income2.result

##Bootstrap results for bins
##Get marginal differences and confidence intervals
get.bs.quantiles = function(term){
x = do.call(rbind,term)
k = apply(x,2,function(x) quantile(x, c(.05,0.5,0.95)))
k=data.frame(t(k) )
colnames(k) <- c('low.ci','estimate','high.ci')
k$group=rownames(k)
return(k)
}

no_cores <- makeCluster(cl, type="FORK")
bs_result = parLapply(no_cores, bs_data, models2)
stopCluster(no_cores)

bs_result = lapply(bs_data, bs_replicates2)
m2.margins = get.bs.quantiles(term=lapply(bs_result, `[[`, 1))
m2.income1.margins = get.bs.quantiles(term=lapply(bs_result, `[[`, 2))
m2.income2.margins = get.bs.quantiles(term=lapply(bs_result, `[[`, 3))

##3) SENSITIVITY ANALYSIS
p.vary = function(p=0.859){
availability.restrict <- all.ds %>% dplyr::select(no_available,pp.available.all,level,year,month,income_cut,year2,year.month,pop.density,med_income, item.m.bin, bannerid, storeid,store.year) 
y = availability.restrict 
  
y$pp.available.all <- y$no_available/max(y$no_available,na.rm=TRUE)
y$available.all.binary <- ifelse(y$pp.available.all <=p,0,1)
m2 <- lm(available.all.binary ~ level + year + year2 +  bs(month,4) + log(pop.density), data = y)
rd <- anonymous(all.ds,m=m2,x=c('levelmed','levellow'),
          d=y[,cluster.var],
          convert.rr=FALSE,
          outcome='pp.available.all')

rd1 = list(rd)

return(rd1)
}

vary.p = seq(0.80,0.90,by=0.02); 
rd.p = vector('list',length(vary.p)); 
for (i in 1:length(vary.p )){
output1 = p.vary(p=vary.p[i])
rd.p[[i]]<- output1[[1]]
}

estimates1 = do.call(rbind,lapply(rd.p, function(x) data.frame(label=rownames(x)[1:2], coefficient=x[c('levellow','levelmed'),'coefficient'], low.ci = x[c('levellow','levelmed'),'cluster.se.ci.low'], high.ci = x[c('levellow','levelmed'),'cluster.se.ci.high'], p=x[c('levellow','levelmed'),'p'])))
sa.plot1 = gg.theme(ggplot(estimates1, aes(x=p,y=coefficient,ymin=low.ci, ymax =high.ci,color=label)) + geom_point(size=2) + geom_errorbar(aes(group=label),width=0,size=1) + labs(x = 'Cut-off for Binary Availability Indicator', y='Mean Difference in \n Probability of Standard Basket Availability \n (relative to high price chains)',caption="Mean Differences come from linear model adjusted for year, month, log population density \n with standard errors clustered at the store level") + scale_color_manual(name='Chain Price', labels=c('Low','Medium'), values=values2)  + geom_hline(yintercept=0, color='red') + geom_text(aes(x=0.86,y=-0.07,label='Main \n Analysis'),color='black') + coord_flip())
