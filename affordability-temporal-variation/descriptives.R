pacman::p_load(ggplot2,RPostgreSQL,splines,kml,maps,mapdata,maptools,dplyr,
                 reshape2,tidyr,multiwayvcov,lmtest,sandwich,lubridate,forecast)
 
##Eliminate likely candidate for dried cherries (error in initial generation of the dataset)
dat$eliminate = rep(0,nrow(dat))
dat[grep("GROUND", dat$itemdesc),'eliminate'] <- 1

dat1 = dat %>% filter(eliminate==0) %>% group_by(periodid,rls_code,bannerid,storeid,bin,weight,month,year) %>% 
summarise(items = n_distinct(itemdesc), real.price = mean(real.price))

all.rls = dat1 %>% group_by(periodid,rls_code) %>% 
          summarise(mean.real = weighted.mean(real.price,items)) %>% data.frame()

##exclude missing rls > 0.1, count number of sampled stores per rls and mark those with just couple of weeks missing
complete.rls = dat %>% 
              group_by(rls_code) %>% 
              summarise(weeks = n_distinct(periodid)) %>%
              mutate(no.wks.missing = max(weeks) - weeks, prop = 1-weeks/max(weeks))


complete.rls.codes <- complete.rls %>% 
                      filter(prop < 0.10) %>% 
                      select(rls_code)

all.rls  = all.rls[which(all.rls$rls_code %in% complete.rls.codes$rls_code),]

##Create empty rows for those with less than 0.1 missing
all.times = expand.grid(periodid = unique(all.rls$periodid),rls_code = unique(complete.rls.codes$rls_code))
all.rls = left_join(all.times,all.rls) 
all.rls$year = strftime(all.rls$periodid, format = "%Y")
all.rls$month = strftime(all.rls$periodid, format = "%m")

##Missing weeks
imputable.rls = complete.rls %>% filter(prop > 0 & prop < 0.10)  %>% group_by(rls_code) 
missing.weeks = all.rls %>% filter(rls_code %in% imputable.rls$rls_code & is.na(mean.real))

##Number of stores and chains
stores.chains = dat  %>% group_by(year,rls_code) %>% 
          summarise(n_stores = n_distinct(storeid),n_chains = n_distinct(bannerid)) %>% data.frame()
all.rls = all.rls %>% left_join(stores.chains)
all.rls$mean.real5 = all.rls$mean.real*scale.up

##Time
all.rls$yr.week = (all.rls$periodid - min(all.rls$periodid))/dweeks(1)
all.rls$yr.month = interval(min(all.rls$periodid),all.rls$periodid) %/% months(1) 
all.rls$yr.week2 = all.rls$yr.week^2
all.rls$month2 = as.numeric(as.character(all.rls$month), 
                          levels=c("01","02","03",
                                   "04","05","06","07","08",
                                   "09","10","11","12"), labels=seq(1,12,1))
all.rls$block = cut(all.rls$yr.week,breaks=c(seq(0,316,by=block)), include.lowest=TRUE)

###Descriptive tables
monthly.means = dat1 %>% group_by(year,month) %>% summarise(mean.price.monthly = weighted.mean(real.price*scale.up,items)) %>% group_by(year) %>% summarise(mean.price = mean(mean.price.monthly), monthly.sd = sd(mean.price.monthly))

region.means = dat1 %>% group_by(year,rls_code) %>% summarise(mean.price.r = weighted.mean(real.price*scale.up,items)) %>% group_by(year) %>% summarise(region.sd = sd(mean.price.r))

total.stores = dat1 %>% group_by(year) %>% summarise(stores = sum(n_distinct(storeid)))
  
overall.mean = monthly.means  %>% left_join(region.means) %>% left_join(total.stores) %>% data.frame()
overall.mean = overall.mean %>% mutate_if(is.numeric, funs(round(.,2)))

##Check larger SD in year 2008 - explained by rapid decline in prices that occured in the latter half of 2008 (economy?)
# monthly.means2 = dat1 %>% group_by(year,month) %>% summarise(mean.price.monthly = weighted.mean(real.price*scale.up,items))
# ggplot(monthly.means2, aes(month,mean.price.monthly,color=year,group=year)) + geom_line()