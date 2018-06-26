
##Note that rls.means was generated in the descriptives.R file

###Main modelling function - Model used in main analysis is m3
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


####Fruits
ds=rls.means[rls.means$type=='fruit',]
xvar='super.density'
m=get.models(xvar,ds)
best.f.super.density = m[[3]]
r = get.results(xvar=xvar, method="Wald",m)
f.super.density.app = r$results[c(1:4),c(1,2,5,6,3,4)]
f.super.density=round(r[[2]][[3]],2)
range.f = ds %>% group_by(rls_code) %>% summarise(mean.real = mean(mean.real)) %>% summarise(min = min(mean.real), max = max(mean.real))

xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
f.mfrei.app = r$results[c(1:4),c(1,2,5,6,3,4)]
best.f.mfrei= m[[3]]
f.mfrei=round(r[[2]][[3]],3)

####Vegetables
ds=rls.means[rls.means$type=='vegetable',]
xvar='super.density'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
v.super.density.app = r$results[c(1:4),c(1,2,5,6,3,4)]
best.v.super.density = m[[3]]
v.super.density=round(r[[2]][[3]],2)
range.v = ds %>% group_by(rls_code) %>% summarise(mean.real = mean(mean.real)) %>% summarise(min = min(mean.real), max = max(mean.real))

xvar='mfrei'
m=get.models(xvar,ds)
r = get.results(xvar=xvar, method="Wald",m)
v.mfrei.app = r$results[c(1:4),c(1,2,5,6,3,4)]
best.v.mfrei = m[[3]]
v.mfrei=round(r[[2]][[3]],4)

#####Derive Moran's I Statistic
##Get model residuals
rls.means.f.map <- rls.means %>% ungroup() %>% filter(type=="fruit") %>% mutate(density.residual = residuals(best.f.super.density),mfrei.residual = residuals(best.f.mfrei)) %>% group_by(rls_code,type) %>% summarise(density.resid = mean(density.residual), mfrei.resid = mean(mfrei.residual))
rls.means.v.map <- rls.means %>% ungroup() %>% filter(type=="vegetable") %>% mutate(density.residual = residuals(best.v.super.density), mfrei.residual = residuals(best.v.mfrei)) %>% group_by(rls_code,type) %>% summarise(density.resid = mean(density.residual), mfrei.resid = mean(mfrei.residual))

##Import shape file
rlscontour <- readOGR("Territoires_RLS_2014.shp")
rlscontour@data$RLS_code <- as.character(rlscontour@data$RLS_code)
rls.contour2 = rlscontour1[rlscontour1$RLS_code %in% rls.means$rls_code,]
coords = coordinates(rls.contour2)
queen <- poly2nb(rls.contour2, queen=TRUE)
sw.poly <- nb2listw(queen , zero.policy=TRUE) 

rls.contour2@data$rn = rownames(rls.contour2@data)
tmp = rls.contour2@data
tmp  = tmp %>% left_join(rls.means.f.map, by=c('RLS_code'='rls_code')) 
rls.contour2@data = tmp 

moran.i.f.density = moran.mc(rls.contour2$density.resid, listw=sw.poly, nsim=1000,zero.policy = TRUE)
moran.i.f.mfrei = moran.mc(rls.contour2$mfrei.resid, listw=sw.poly, nsim=1000,zero.policy = TRUE)
moran.i.v.density = moran.mc(rls.contour2$density.resid, listw=sw.poly, nsim=1000,zero.policy = TRUE)
moran.i.v.mfrei = moran.mc(rls.contour2$mfrei.resid, listw=sw.poly, nsim=1000,zero.policy = TRUE)


