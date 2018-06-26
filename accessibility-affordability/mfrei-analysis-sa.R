---
editor_options:
  chunk_output_type: console
output:
  word_document: default
  html_document: default
---

```{r setup}
knitr::opts_chunk$set(echo = FALSE,warning=FALSE, message=FALSE)
pacman::p_load(dplyr,ggplot2,RPostgreSQL,cluster,splines,kml,longitudinalData,maps,mapdata,maptools,stringr,data.table,reshape2,multiwayvcov,lmtest, rgdal, tmap, sjPlot, lme4)

setwd('~/analysis_tmp/Nielsen')
load('data.RData')
load('paper1-data.RData')
load('themes_functions.Rdata')
```

**##SENSITIVITY ANALYSIS A: Restrict supermarket category to chain only**

```{r chainsonly_descriptives}
##Create subset with chain names only
dmti$chain = ifelse(dmti$supermarket=='TRUE' & !is.na(dmti$store), 1, 0)
##If we restrict to chains only, there is minimal jump in the number of supermarkets
dmti.overall.chain = dmti %>% filter(year != 2008) %>% group_by(year) %>% 
summarise(no_supercentre = sum(supercentre==TRUE), no_convenience = sum(convenience==TRUE), no_supermarket = sum(supermarket==TRUE & chain==1), no_fastfood = sum(fastfood==TRUE), no_pharmacy = sum(pharmacy==TRUE)) %>% mutate(mfrei = (no_supermarket + no_supercentre)/(no_pharmacy +  no_convenience + no_supermarket + no_fastfood)) %>% data.frame()

dmti.sum.chain = dmti %>% filter(year != 2008)  %>% group_by(rls_code,year) %>% summarise(no_supercentre = sum(supercentre==TRUE), no_convenience = sum(convenience==TRUE), no_supermarket = sum(supermarket==TRUE  & chain==1), no_fastfood = sum(fastfood==TRUE), no_pharmacy = sum(pharmacy==TRUE)) 

dmti.sum.chain$mfrei.raw = (dmti.sum.chain$no_supermarket + dmti.sum.chain$no_supercentre)/(dmti.sum.chain$no_pharmacy + dmti.sum.chain$no_convenience + dmti.sum.chain$no_supermarket + dmti.sum.chain$no_fastfood)
dmti.sum.chain$mfrei = dmti.sum.chain$mfrei.raw*10

chain.only  = dmti.sum.chain %>% dplyr::select(rls_code, year, mfrei.raw, mfrei,no_supermarket)

##Difference in rows - FIGURE OUT
# nrow(rls.means.chain[complete.cases(rls.means.chain),])
# nrow(rls.means[complete.cases(rls.means),])

rls.means.chain = rls.means %>% dplyr::select(-mfrei,-mfrei.raw,-super.density,-no_supermarket) %>% left_join(chain.only)
rls.means.chain$super.density = (rls.means.chain$no_supermarket/rls.means.chain$predict.pop)*1000


# g.density = ggplot(rls.means.chain %>% group_by(rls_code,year,type) %>% summarise(mean=mean(mean.real), density = mean(super.density)), aes(log(density),mean,color=type)) + geom_point(alpha=0.6) + labs(title="Figure 2: Regional Mean Price vs Regional Supermarket Density \n Restricted to Chains", x="Log of Density (Supermarkets per 1000 people)", y="Mean Price per Serving (cents)") + scale_color_manual(name="Type", labels=labels, values=values) + geom_smooth(aes(group=type), method="lm",size=1.5)
# 
# tiff("paper1-figures/sa-figure1.tiff",units="in", height=5, width=8, res=300)
# gg.theme(g.density)
# dev.off()

# g.mfrei = ggplot(rls.means.chain %>% group_by(rls_code,year,type) %>% summarise(mean=mean(mean.real), mfrei = mean(mfrei)), aes(log(mfrei),mean,color=type)) + geom_point(alpha=0.6) + labs(title="Figure 1: Regional Mean Price vs Regional MfRei \n Restricted to Chains", x="Log of MfREi", y="Mean Price per Serving (cents)") + scale_color_manual(name="Type", labels=labels, values=values) + geom_smooth(aes(group=type), method="lm",size=1.5)
# 
# tiff("paper1-figures/sa-figure2.tiff",units="in", height=5, width=8, res=300)
# gg.theme(g.mfrei)
# dev.off()

```

When we restrict to chain supermarkets only, the associations remained consistent. Those for mFREI and affordability grew stronger in magnitude (Table 1).

```{r chainsonly_modelling}
ds=rls.means.chain[rls.means.chain$type=='fruit',]
xvar='super.density'
m=get.models(xvar,ds)
best.f.super.density = m[[3]]
r = get.results(xvar=xvar, method="Wald",m)
f.super.density=round(r[[2]][[3]],2)

xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.f.mfrei= m[[3]]
f.mfrei=round(r[[2]][[3]],3)

ds=rls.means.chain[rls.means.chain$type=='vegetable',]
xvar='super.density'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.v.super.density = m[[3]]
v.super.density=round(r[[2]][[3]],2)

xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.v.mfrei = m[[3]]
v.mfrei=round(r[[2]][[3]],4)


f.density.result = paste(f.super.density['super.density','coefficients']," [",f.super.density['super.density','2.5%'],";",f.super.density['super.density','95%'],"]",sep="" )
f.mfrei.result = paste(f.mfrei['mfrei','coefficients']," [",f.mfrei['mfrei','2.5%'],";",f.mfrei['mfrei','95%'],"]",sep="" )
v.density.result = paste(v.super.density['super.density','coefficients']," [",v.super.density['super.density','2.5%'],";",v.super.density['super.density','95%'],"]",sep="" )
v.mfrei.result = paste(v.mfrei['mfrei','coefficients']," [",v.mfrei['mfrei','2.5%'],";",v.mfrei['mfrei','95%'],"]",sep="" )

model.results.chain = data.frame(rbind(f.density.result,f.mfrei.result,v.density.result,v.mfrei.result))
colnames(model.results.chain) = c("Effect")
compare1=data.frame(model.results,model.results.chain)
rownames(compare1) = c('Fruit-density','Fruit-mFREI','Veg-density','Veg-mFREI')
colnames(compare1) = c('Main Analysis','Restrict to Chains')

knitr::kable(compare1,caption="Table 1: Restrict to Chain Supermarkets")

```

**##SENSITIVITY ANALYSIS B: Include the grocery category**

Including the grocery category as part of the numerator for the mFREI did not impact the direction of our results, though, similar to chain supermarkets, the effect estimates became stronger in magnitude (Table 2). 

```{r withgrocery}
##With grocery stores
dmti.sum = dmti %>% filter(year != '2008') %>% group_by(rls_code,year) %>% 
summarise(no_supercentre = sum(supercentre==TRUE), no_convenience = sum(convenience==TRUE), no_supermarket = sum(supermarket==TRUE),no_fastfood = sum(fastfood==TRUE), no_grocery = sum(grocery==TRUE),no_pharmacy = sum(pharmacy==TRUE)) 

dmti.sum$year = as.factor(dmti.sum$year)
dmti.sum$mfrei = (dmti.sum$no_supermarket + dmti.sum$no_supercentre + dmti.sum$no_grocery)/(dmti.sum$no_pharmacy + dmti.sum$no_convenience + dmti.sum$no_supermarket + dmti.sum$no_fastfood + dmti.sum$no_grocery)*10
grocery = dmti.sum %>% select(rls_code,year,mfrei)
grocery$year <- as.numeric(as.character(grocery$year))
rls.means.grocery = rls.means %>% select(-mfrei) %>% left_join(grocery)

ds=rls.means.grocery[rls.means.grocery$type=='fruit',]
xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.f.mfrei= m[[3]]
f.mfrei=round(r[[2]][[3]],3)

ds=rls.means.grocery[rls.means.grocery$type=='vegetable',]
xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.v.mfrei = m[[3]]
v.mfrei=round(r[[2]][[3]],4)

f.mfrei.result = paste(f.mfrei['mfrei','coefficients']," [",f.mfrei['mfrei','2.5%'],";",f.mfrei['mfrei','95%'],"]",sep="" )
v.mfrei.result = paste(v.mfrei['mfrei','coefficients']," [",v.mfrei['mfrei','2.5%'],";",v.mfrei['mfrei','95%'],"]",sep="" )

model.results.grocery = data.frame(rbind(f.mfrei.result,v.mfrei.result))
colnames(model.results.grocery) = c("Effect")

compare2=data.frame(model.results[c(2,4),],model.results.grocery)
rownames(compare2) = c('Fruit-mFREI','Veg-mFREI')
colnames(compare2) = c('Main Analysis','Include Grocery Stores')

knitr::kable(compare2 , caption="Table 2: Include 'grocery' category in MfREI numerator")

```

**##SENSITIVITY ANALYSIS C: Remove outlier regions**

Outliers were identified by plotting the region-specific deviations from the overall model intercept.  From these, we identified two unique outlier regions (Figure 1 and 2). The main analysis was repeated without these regions, and were consistent with the main analysis (Table 2). 

```{r outliers}

tiff("paper1-figures/sa-figure1.tiff",units="in", height=4, width=7, res=300)
gg.theme(plot_model(best.v.super.density, y.offset = .4,sort.est = "(Intercept)",title = "Figure 1: Random Intercepts- Vegetables, Supermarket Density", type="re") + labs(y = "Best Linear Unbiased Prediction", x="Region"))
dev.off()
tiff("paper1-figures/sa-figure2.tiff",units="in", height=5, width=8, res=300)
gg.theme(plot_model(best.f.super.density, y.offset = .4,sort.est = "(Intercept)", title= "Figure 2: Deviation of Region-Intercepts from Overall Mean - Fruits", type="re"))
dev.off()
```


```{r sensitivity}
##Exclude Outliers-
#Both: 0609, 1609
#Fruit: 0807, 1203, 1609
##They don't really affect RE, but 'correct' OLS to match RE, suggesting they were driving the difference
outliers = c("0609","1609")
no.outliers = rls.means %>% ungroup() %>% filter(!(rls_code %in% outliers) )

ds=no.outliers[no.outliers$type=='fruit',]
xvar='super.density'
m=get.models(xvar,ds)
best.f.super.density = m[[3]]
r = get.results(xvar=xvar, method="Wald",m)
f.super.density=round(r[[2]][[3]],2)

xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.f.mfrei= m[[3]]
f.mfrei=round(r[[2]][[3]],3)

ds=no.outliers[no.outliers$type=='vegetable',]
xvar='super.density'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.v.super.density = m[[3]]
v.super.density=round(r[[2]][[3]],2)

xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.v.mfrei = m[[3]]
v.mfrei=round(r[[2]][[3]],4)

f.density.result = paste(f.super.density['super.density','coefficients']," [",f.super.density['super.density','2.5%'],";",f.super.density['super.density','95%'],"]",sep="" )
f.mfrei.result = paste(f.mfrei['mfrei','coefficients']," [",f.mfrei['mfrei','2.5%'],";",f.mfrei['mfrei','95%'],"]",sep="" )
v.density.result = paste(v.super.density['super.density','coefficients']," [",v.super.density['super.density','2.5%'],";",v.super.density['super.density','95%'],"]",sep="" )
v.mfrei.result = paste(v.mfrei['mfrei','coefficients']," [",v.mfrei['mfrei','2.5%'],";",v.mfrei['mfrei','95%'],"]",sep="" )

model.results.no.outliers = data.frame(rbind(f.density.result,f.mfrei.result,v.density.result,v.mfrei.result))
compare3=data.frame(model.results,model.results.no.outliers)
rownames(compare3) = c('Fruit-density','Fruit-mFREI','Veg-density','Veg-mFREI')
colnames(compare3) = c('Main Analysis','Exclude Outliers')

knitr::kable(compare3 , caption="Table 3: Exclude Outliers")

```

**##SENSITIVITY ANALYSIS D: Alternate Price Truncation**

In the primary analysis, we truncated the top and bottom most expensive items within each product category at the top and bottom 5th percentile. In this robustness check, the top and bottom 1% was used (Table 4). 

```{r price_truncation}
rls.means = dat.sensitivity %>% filter(rls_code %in% complete.rls.codes$rls_code & year != 2008) %>%
group_by(type,rls_code,periodid,year,bannerid) %>% 
summarise(mean.real = weighted.mean(real.price,weight), no.stores = n_distinct(storeid))
rls.means$year= as.factor(rls.means$year)
census.ts$year = as.factor(census.ts$year)

dmti.sum = dmti %>% filter(year != '2008') %>% group_by(rls_code,year) %>% 
summarise(no_supercentre = sum(supercentre==TRUE), no_convenience = sum(convenience==TRUE), no_supermarket = sum(supermarket==TRUE),no_fastfood = sum(fastfood==TRUE), no_grocery = sum(grocery==TRUE),no_pharmacy = sum(pharmacy==TRUE)) 
dmti.sum$mfrei = (dmti.sum$no_supermarket + dmti.sum$no_supercentre)/(dmti.sum$no_pharmacy + dmti.sum$no_convenience + dmti.sum$no_supermarket + dmti.sum$no_fastfood)
agg.dmti = dmti.sum %>% group_by(year) %>% summarise(m = round(mean(no_supermarket),2), sd = round(sd(no_supermarket),2))

dmti.sum$year = as.factor(dmti.sum$year)

rls.means = rls.means %>% left_join(dmti.sum)
rls.means = rls.means %>% left_join(census.ts)
rls.means$super.density = (rls.means$no_supermarket/rls.means$predict.pop)*1000
rls.means$bannerid = factor(rls.means$bannerid)
rls.means$month = as.numeric(as.character(strftime(rls.means$periodid, format = "%m")))
rls.means$year = factor(rls.means$year)
rls.means$mfrei.raw = rls.means$mfrei
rls.means$mfrei = rls.means$mfrei.raw*10
rls.means$year = as.numeric(as.character(rls.means$year))
rls.means$year2 = rls.means$year*rls.means$year
rls.means$middle = as.Date(paste(rls.means$year,rls.means$month,"01", sep="-"), format="%Y-%m-%d")
dates = data.frame(date= unique(rls.means$middle)) %>% arrange(date)
dates$yr.month= seq(1,nrow(dates), by=1)
rls.means= rls.means %>% left_join(dates, by=c('middle'='date')) %>% ungroup()


ds=rls.means[rls.means$type=='fruit',]
xvar='super.density'
m=get.models(xvar,ds)
best.f.super.density = m[[3]]
r = get.results(xvar=xvar, method="Wald",m)
f.super.density=round(r[[2]][[3]],2)

xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.f.mfrei= m[[3]]
f.mfrei=round(r[[2]][[3]],3)

ds=rls.means[rls.means$type=='vegetable',]
xvar='super.density'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.v.super.density = m[[3]]
v.super.density=round(r[[2]][[3]],2)

xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
best.v.mfrei = m[[3]]
v.mfrei=round(r[[2]][[3]],4)


f.density.result = paste(f.super.density['super.density','coefficients']," [",f.super.density['super.density','2.5%'],";",f.super.density['super.density','95%'],"]",sep="" )
f.mfrei.result = paste(f.mfrei['mfrei','coefficients']," [",f.mfrei['mfrei','2.5%'],";",f.mfrei['mfrei','95%'],"]",sep="" )
v.density.result = paste(v.super.density['super.density','coefficients']," [",v.super.density['super.density','2.5%'],";",v.super.density['super.density','95%'],"]",sep="" )
v.mfrei.result = paste(v.mfrei['mfrei','coefficients']," [",v.mfrei['mfrei','2.5%'],";",v.mfrei['mfrei','95%'],"]",sep="" )

model.results.truncation = data.frame(rbind(f.density.result,f.mfrei.result,v.density.result,v.mfrei.result))
colnames(model.results.truncation) = c("Effect")
compare4=data.frame(model.results,model.results.truncation)
rownames(compare4) = c('Fruit-density','Fruit-mFREI','Veg-density','Veg-mFREI')
colnames(compare4) = c('Main Analysis','Alternate Truncation')

knitr::kable(compare4 , caption="Table 4: Alternative Price Truncation")

```

**##SENSITIVITY ANALYSIS E: Alternate Geographic Boundary**

```{r fsa_analysis}
load('data-fsa.RData')
complete.fsa = dat %>% 
              filter(year != '2008') %>%
              group_by(postalprefix) %>% 
              summarise(weeks = n_distinct(periodid)) %>%
              mutate(no.wks.missing = max(weeks) - weeks, prop = 1-weeks/max(weeks))

complete.fsa.codes <- complete.fsa  %>% 
                      filter(prop <= 0.11) %>% 
                      dplyr::select(fsa=postalprefix)

dmti.sum.fsa = dmti  %>% group_by(fsa,year) %>% 
summarise(no_supercentre = sum(supercentre==TRUE), 
no_convenience = sum(convenience==TRUE), no_supermarket = sum(supermarket==TRUE),  no_fastfood = sum(fastfood==TRUE), no_grocery = sum(grocery==TRUE), no_pharmacy = sum(pharmacy==TRUE)) 

dmti.sum.fsa$mfrei.raw = (dmti.sum.fsa$no_supermarket + dmti.sum.fsa$no_supercentre)/(dmti.sum.fsa$no_pharmacy + dmti.sum.fsa$no_convenience + dmti.sum.fsa$no_supermarket + dmti.sum.fsa$no_fastfood)
dmti.sum.fsa$mfrei = dmti.sum.fsa$mfrei.raw*10

dmti.sum.fsa.split = dmti.sum.fsa %>% group_by(year) %>% summarise(m_supercentre =mean(no_supercentre), m_supermarket = mean(no_supermarket), m_fastfood = mean(no_fastfood), m_grocery = mean(no_grocery),m_pharmacy = mean(no_pharmacy)) %>% data.frame()

fsa.means = dat %>% filter(rls_code %in% complete.rls.codes$rls_code & year != 2008) %>%
group_by(type,fsa=postalprefix,periodid,year,bannerid) %>% 
summarise(mean.real = weighted.mean(real.price,weight), no.stores = n_distinct(storeid))

fsa.means$year = as.numeric(as.character(fsa.means$year))
fsa.means = fsa.means %>% left_join(dmti.sum.fsa)
fsa.means = fsa.means %>% left_join(census.ts)
fsa.means$super.density = (fsa.means$no_supermarket/fsa.means$predict.pop)*1000

fsa.means$month = as.numeric(as.character(strftime(fsa.means$periodid, format = "%m")))
fsa.means$year2 = fsa.means$year^2
fsa.means$bannerid = factor(fsa.means$bannerid)


# g.density = ggplot(fsa.means %>% filter(super.density!=0) %>% group_by(fsa,year,type) %>% summarise(mean=mean(mean.real), density = mean(super.density)), aes(log(density),mean,color=type)) + geom_point(alpha=0.6) + labs(title="Figure 5: Regional Mean Price vs Supermarket Density \n Region=FSA", x="Log of Density (Supermarkets per 1000 people)", y="Mean Price per Serving (cents)") + scale_color_manual(name="Type", labels=labels, values=values) + geom_smooth(aes(group=type), method="lm",size=1.5)
# 
# tiff("paper1-figures/sa-figure5.tiff",units="in", height=5, width=8, res=300)
# gg.theme(g.density)
# dev.off()
# 
# g.mfrei = ggplot(fsa.means %>% filter(mfrei!=0) %>% group_by(fsa,year,type) %>% summarise(mean=mean(mean.real), mfrei = mean(mfrei)), aes(log(mfrei),mean,color=type)) + geom_point(alpha=0.6) + labs(title="Figure 6: Regional Mean Price vs MfREI \n Region=FSA", x="Log of MfREI", y="Mean Price per Serving (cents)") + scale_color_manual(name="Type", labels=labels, values=values) + geom_smooth(aes(group=type), method="lm",size=1.5)
# 
# tiff("paper1-figures/sa-figure6.tiff",units="in", height=5, width=8, res=300)
# gg.theme(g.mfrei)
# dev.off()

```

In the primary analysis, we used the RLS geographic boundaries to define regions.  This choice of boundary may have influenced our results.  Here, we define region as 'forward sortation area' (FSA), which are units used by the postal service that are usually smaller geographic areas then the RLS. In total there were `r length(unique(fsa.means$fsa))` FSAs. The populations of the FSAs varied from `r min(fsa.means$predict.pop[fsa.means$year=="2009"])` to `r max(fsa.means$predict.pop[fsa.means$year=="2009"])` in 2009.  The mean number of supermarkets per FSA is  `r round(mean(dmti.sum.fsa.split$m_supermarket),2)` because the majority of FSAs had only one  or two sampled stores, in contrast to the RLS where almost all regions had at least two and usually more. To account for the non-independence of the measurements with in an FSA, we used robust standard errors were clustered at the FSA level. Overall, the results were similar to our main findings (Figure 3). Though the mfREI coefficient was positive, it remained substantively close to zero.

```{r fsa_modelling}
get.results.clustered.se.fsa = function(ds, xvar="super.density", type="fruit"){
ds = ds[ds$type==type,]
m =  lm(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + year + year2  + bannerid + bs(month,4) ")), data = ds) 
vcov1 <- cluster.vcov(m,ds$fsa_year)
vcov2 <- cluster.vcov(m,ds$fsa)
new.se1 = coeftest(m, vcov1);
new.se2 = coeftest(m, vcov2);
coefficient1 = round(new.se1[xvar,'Estimate'],3)
coefficient1.ci = round(new.se1[xvar,'Estimate'] + new.se1[xvar,'Std. Error']*c(-1,1)*qnorm(0.95),3)# 
coefficient2 = round(new.se2[xvar,'Estimate'],3)
coefficient2.ci = round(new.se2[xvar,'Estimate'] + new.se2[xvar,'Std. Error']*c(-1,1)*qnorm(0.95),3)
crude.ci = round(confint(m)[2,],3)
m_re1 =  lmer(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + year + year2  + bannerid + bs(month,4) + (1 | fsa)")), data = ds) 
coefficient_re1 = summary(m_re1 )$coefficients[xvar,1]
confint_re1 = confint(m_re1 , method="Wald")[xvar,]

results = data.frame(rbind(c(c=m$coefficients[xvar],ci=crude.ci), c(c = coefficient1, ci = coefficient1.ci),c(c = coefficient2,ci = coefficient2.ci), c(c =coefficient_re1,ci=confint_re1)))
#results = data.frame(rbind(c(c=m$coefficients[xvar],ci=crude.ci), c(c = coefficient2,ci = coefficient2.ci)))
results$measure=rep(xvar, nrow(results))
results$type = rep(type,nrow(results))
results$level= c("OLS","robust_SE" )
results$level= c("OLS", "Cluster_RLS", "Cluster_RLSYr", "RE_RLS")

return(results)
}

######Number per FSA
# no.banner.per.fsa = fsa.means %>% group_by(fsa) %>% summarise(n = n_distinct(bannerid))
# ggplot(no.banner.per.fsa , aes(fsa, n)) + geom_bar(stat="identity")
# no.banner.per.rls = rls.means %>% group_by(rls_code) %>% summarise(n = n_distinct(bannerid))
# ggplot(no.banner.per.rls , aes(rls_code, n)) + geom_bar(stat="identity")

fsa.means$fsa_year <- factor(paste(fsa.means$fsa, fsa.means$year, sep=""))
#fsa.means = fsa.means %>% group_by(fsa) %>% mutate(lag.price= dplyr::lag(mean.real, n = 1, default = NA, order_by = periodid))

f.cluster.density.fsa = get.results.clustered.se.fsa(ds = fsa.means[fsa.means$type=="fruit",], xvar="super.density", type="fruit") %>% rename(coefficient = c.super.density, low.ci = ci.2.5.., high.ci = ci.97.5.. )
v.cluster.density.fsa = get.results.clustered.se.fsa(ds = fsa.means[fsa.means$type=="vegetable",], xvar="super.density", type="vegetable") %>% rename(coefficient = c.super.density, low.ci = ci.2.5.., high.ci = ci.97.5.. )
f.cluster.mfrei.fsa = get.results.clustered.se.fsa(ds = fsa.means[fsa.means$type=="fruit",], xvar="mfrei", type="fruit") %>% rename(coefficient = c.mfrei, low.ci = ci.2.5.., high.ci = ci.97.5.. )
v.cluster.mfrei.fsa = get.results.clustered.se.fsa(ds = fsa.means[fsa.means$type=="vegetable",], xvar="mfrei", type="vegetable") %>% rename(coefficient = c.mfrei, low.ci = ci.2.5.., high.ci = ci.97.5.. )

main = data.frame(rbind(f.super.density['super.density',],v.super.density['super.density',], f.mfrei['mfrei',], v.mfrei['mfrei',]))
colnames(main) = c('coefficient','low.ci','high.ci')
main$source= rep('main',nrow(main))
main$type = c('fruit','vegetable','fruit','vegetable')
main$level = rep('re',4)
main$measure = c('super.density','super.density','mfrei','mfrei')


fsa = data.frame(rbind(f.cluster.density.fsa,v.cluster.density.fsa,f.cluster.mfrei.fsa,v.cluster.mfrei.fsa))  

fsa$source=rep('fsa',4)

compare = data.frame(rbind(fsa,main))

labels <- c(fruit = "Fruit", vegetable = "Vegetable")
values = c("steelblue3", "tomato4")
g.compare = ggplot(compare %>% filter(level %in% c('re','Cluster_RLS')), aes(measure,coefficient, group=source, color=source)) + geom_point(size=3) + scale_color_manual(name="Geographic Boundary", values=values, labels=c("FSA","RLS")) + geom_errorbar(aes(ymin=low.ci,ymax=high.ci), width=0.2, size=2) + geom_hline(yintercept=0) + coord_flip() + facet_wrap(~type, labeller=labeller(type=labels)) + labs(y = "% Change in price for \n Unit Change in Accessibility", x=NULL, title="Figure 2: Results based on Alternate Geographic Boundaries") 

tiff("paper1-figures/sa-figure3.tiff",units="in", height=5, width=8, res=300)
gg.theme(g.compare)
dev.off()

# formatted.result = function(b){
# result = paste(b['Beta']," [",b['low.ci'],";",b['high.ci'],"]",sep="" )
# return(result)
# }

# f.density.fsa = round(models.fsa(fsa.means[fsa.means$type=="fruit" & fsa.means$super.density != 0 & fsa.means$year != 2008,], xvar="super.density", type="fruit")[[2]],2)
# f.mfrei.fsa = round(models.fsa(fsa.means[fsa.means$type=="fruit" & fsa.means$mfrei != 0,], xvar="mfrei", type="fruit")[[2]],2)
# v.density.fsa = round(models.fsa(fsa.means[fsa.means$type=="vegetable" & fsa.means$super.density != 0,], xvar="super.density", type="vegetable")[[2]],2)
# v.mfrei.fsa = round(models.fsa(fsa.means[fsa.means$type=="vegetable" & fsa.means$mfrei != 0,], xvar="mfrei", type="vegetable")[[2]],2)
# 
# fsa.strata.results =  noquote(matrix(c(rural.result(round(f.mfrei,2)),
# rural.result(round(v.mfrei,2)),rural.result(round(f.density,2)),
# rural.result(round(v.density,2))),ncol=2,byrow=TRUE))
# 
# rownames(fsa.strata.results ) <- c("MfREI","Supermarkets/1000"); colnames(fsa.strata.results) <- c("Fruits","Vegetables")

```

```{r fruit_flip}
# fsa.group.f = fsa.means %>% ungroup() %>% filter(type=="fruit" & mfrei != 0) %>% group_by(fsa,year) %>% summarise(mean.price = mean(mean.real), mfrei = mean(mfrei), super.density = mean(super.density))
# 
# ggplot(fsa.group.f) + geom_point(aes(mfrei, mean.price)) + scale_color_grey(guide=FALSE) + stat_smooth(aes(mfrei, mean.price, color=fsa, group=fsa),method="lm", se=FALSE) + stat_smooth(aes(mfrei, mean.price), method="lm", color="black", size=2, se=FALSE) 
# 
# ggplot(fsa.group.f) + geom_point(aes(super.density, mean.price)) + scale_color_grey(guide=FALSE) + stat_smooth(aes(super.density, mean.price, color=fsa, group=fsa),method="lm", se=FALSE) + stat_smooth(aes(super.density, mean.price), method="lm", color="black", size=2, se=FALSE) 
# 
# 
# rls.group.f = rls.means %>% ungroup() %>% filter(type=="fruit") %>% group_by(rls_code,year) %>% summarise(mean.price = mean(mean.real), mfrei = mean(mfrei), super.density = mean(super.density))
# 
# ggplot(rls.group.f)  + scale_color_grey(guide=FALSE) + stat_smooth(aes(mfrei, mean.price),method="lm", se=FALSE) + facet_wrap(~rls_code) 
# 
# stat_smooth(aes(mfrei, mean.price), method="lm", color="black", size=2, se=FALSE) + facet_wrap(~rls_code)
# 
# ggplot(rls.group.f) + geom_point(aes(super.density, mean.price)) + scale_color_grey(guide=FALSE) + stat_smooth(aes(super.density, mean.price, color=rls_code, group=rls_code),method="lm", se=FALSE) + stat_smooth(aes(super.density, mean.price), method="lm", color="black", size=2, se=FALSE) 
# # 
# rls.means.chain
# rls.group.f.banner.chain = rls.means.chain %>% ungroup() %>% filter(type=="fruit") %>% group_by(bannerid,year) %>% summarise(mean.price = mean(mean.real), mfrei = mean(mfrei), super.density = mean(super.density))
# 
# rls.group.f.banner= rls.means %>% ungroup() %>% filter(type=="fruit") %>% group_by(bannerid,year) %>% summarise(mean.price = mean(mean.real), mfrei = mean(mfrei), super.density = mean(super.density))
# # 
# ggplot(rls.group.f.banner)  + scale_color_grey(guide=FALSE) + #stat_smooth(aes(mfrei,mean.price,group=bannerid),method="lm", se=FALSE, color="black")
# stat_smooth(aes(mfrei, mean.price),method="lm", se=FALSE) + facet_wrap(~bannerid )
# 
# ggplot(rls.group.f.banner.chain)  + scale_color_grey(guide=FALSE) + stat_smooth(aes(mfrei, mean.price),method="lm", se=FALSE) + aes(rls.group.f.banner) + stat_smooth(data = rls.group.f.banner,aes(mfrei, mean.price),method="lm", se=FALSE, color="red") + facet_wrap(~bannerid ) 
#   
#  
# fsa.group.f.banner = fsa.means %>% ungroup() %>% filter(type=="fruit" & mfrei != 0 & bannerid != "347") %>% group_by(fsa,year,bannerid) %>% summarise(mean.price = mean(mean.real), mfrei = mean(mfrei), super.density = mean(super.density))
# 
# sample.fsa  = sample(unique(fsa.group.f.banner$fsa), 50)
# ggplot(fsa.group.f.banner[fsa.group.f.banner$fsa %in% sample.fsa,])  + scale_color_grey(guide=FALSE) + stat_smooth(aes(mfrei, mean.price),method="lm", se=FALSE) + facet_wrap(~fsa + bannerid)


# dmti.sum.chain$year = as.numeric(dmti.sum.chain$year)
# 
# dmti.sum.rls.chain = dmti.sum.chain %>% filter(year != 2008) %>% group_by(year) %>% summarise(m_supercentre = mean(no_supercentre), m_convenience = mean(no_convenience), m_supermarket = mean(no_supermarket), m_fastfood = mean(no_fastfood),m_pharmacy = mean(no_pharmacy)) %>% data.frame()
# 
# rls.means.chain = dat %>% filter(year != 2008 & rls_code %in% complete.rls.codes$rls_code) %>% group_by(type,rls_code,periodid,year,bannerid) %>% summarise(mean.real = weighted.mean(real.price,weight), no.stores = n_distinct(storeid))
# 
# rls.means.chain = rls.means.chain %>% left_join(dmti.sum.chain)
# rls.means.chain = rls.means.chain %>% left_join(census.ts)
# rls.means.chain$super.density = (rls.means.chain$no_supermarket/rls.means.chain$predict.pop)*1000
# 
# rls.means.chain$month = factor(strftime(rls.means.chain$periodid, format = "%m"))
# rls.means.chain$year = factor(rls.means.chain$year)
# rls.means.chain$bannerid = factor(rls.means.chain$bannerid)
# rls.means.chain$year2 = rls.means.chain$year^2

```


