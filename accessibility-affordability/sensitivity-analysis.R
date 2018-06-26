
##Relies on the descriptives.R and analysis.R
**##SENSITIVITY ANALYSIS A: Restrict supermarket category to chain only**

##Create subset with chain names only
dmti$chain = ifelse(dmti$supermarket=='TRUE' & !is.na(dmti$store), 1, 0)
chain.only  = dmti.sum.chain %>% dplyr::select(rls_code, year, mfrei.raw, mfrei,no_supermarket)
rls.means.chain = rls.means %>% dplyr::select(-mfrei,-mfrei.raw,-super.density,-no_supermarket) %>% 
left_join(chain.only)
rls.means.chain$super.density = (rls.means.chain$no_supermarket/rls.means.chain$predict.pop)*1000

##Repeat Nmodeeling
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

**##SENSITIVITY ANALYSIS B: Include the grocery category**
##Re-create accessibility indicators with grocery category
dmti.sum = dmti %>% filter(year != '2008') %>% group_by(rls_code,year) %>% 
summarise(no_supercentre = sum(supercentre==TRUE), no_convenience = sum(convenience==TRUE), no_supermarket = sum(supermarket==TRUE),no_fastfood = sum(fastfood==TRUE), no_grocery = sum(grocery==TRUE),no_pharmacy = sum(pharmacy==TRUE)) 
dmti.sum$year = as.factor(dmti.sum$year)
dmti.sum$mfrei = (dmti.sum$no_supermarket + dmti.sum$no_supercentre + dmti.sum$no_grocery)/(dmti.sum$no_pharmacy + dmti.sum$no_convenience + dmti.sum$no_supermarket + dmti.sum$no_fastfood + dmti.sum$no_grocery)*10
grocery = dmti.sum %>% select(rls_code,year,mfrei)
grocery$year <- as.numeric(as.character(grocery$year))
rls.means.grocery = rls.means %>% select(-mfrei) %>% left_join(grocery)

##Run models again
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


**##SENSITIVITY ANALYSIS C: Remove outlier regions**

##Identify outliers through plots
gg.theme(plot_model(best.v.super.density, y.offset = .4,sort.est = "(Intercept)",title = "Figure 1: Random Intercepts- Vegetables, Supermarket Density", type="re") + labs(y = "Best Linear Unbiased Prediction", x="Region"))
gg.theme(plot_model(best.f.super.density, y.offset = .4,sort.est = "(Intercept)", title= "Figure 2: Deviation of Region-Intercepts from Overall Mean - Fruits", type="re"))

##Exclude Outliers-
#Both: 0609, 1609
#Fruit: 0807, 1203, 1609
outliers = c("0609","1609")
no.outliers = rls.means %>% ungroup() %>% filter(!(rls_code %in% outliers) )

##Re-run models
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

**##SENSITIVITY ANALYSIS D: Alternate Price Truncation**
##Regenerate primary data for analysis using dat.sensitivit, which uses alternate price truncations
rls.means = dat.sensitivity %>% filter(rls_code %in% complete.rls.codes$rls_code & year != 2008) %>%
group_by(type,rls_code,periodid,year,bannerid) %>% 
summarise(mean.real = weighted.mean(real.price,weight), no.stores = n_distinct(storeid))
rls.means$year= as.factor(rls.means$year)
census.ts$year = as.factor(census.ts$year)

##Re-run models
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


**##SENSITIVITY ANALYSIS E: Alternate Geographic Boundary**
##Load data at fsa level and repeat primary analysis
load('data-fsa.RData')
fsa.means$fsa_year <- factor(paste(fsa.means$fsa, fsa.means$year, sep=""))

##Function to get clustered stanard errors
get.results.clustered.se.fsa = function(ds, xvar="super.density", type="fruit"){
ds = ds[ds$type==type,]
m =  lm(as.formula(paste0("log(mean.real)", " ~ ", xvar, " + year + year2  + bannerid + bs(month,4) ")), data = ds) 
vcov1 <- cluster.vcov(m,ds$fsa_year)
new.se1 = coeftest(m, vcov1);
coefficient1 = round(new.se1[xvar,'Estimate'],3)

results = data.frame(c(c = coefficient1,ci = coefficient1.ci)))
results$measure=rep(xvar, nrow(results))
results$type = rep(type,nrow(results))
results$level= c("robust_SE" )
results$level= c("Cluster_RLSYr")

return(results)
}

f.cluster.density.fsa = 
get.results.clustered.se.fsa(ds = fsa.means[fsa.means$type=="fruit",], 
	xvar="super.density", type="fruit") %>% 
rename(coefficient = c.super.density, low.ci = ci.2.5.., high.ci = ci.97.5.. )
v.cluster.density.fsa = get.results.clustered.se.fsa(ds = fsa.means[fsa.means$type=="vegetable",], xvar="super.density", type="vegetable") %>% rename(coefficient = c.super.density, low.ci = ci.2.5.., high.ci = ci.97.5.. )
f.cluster.mfrei.fsa = get.results.clustered.se.fsa(ds = fsa.means[fsa.means$type=="fruit",], xvar="mfrei", type="fruit") %>% rename(coefficient = c.mfrei, low.ci = ci.2.5.., high.ci = ci.97.5.. )
v.cluster.mfrei.fsa = get.results.clustered.se.fsa(ds = fsa.means[fsa.means$type=="vegetable",], xvar="mfrei", type="vegetable") %>% rename(coefficient = c.mfrei, low.ci = ci.2.5.., high.ci = ci.97.5.. )
