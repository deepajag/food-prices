

##*Descriptives & Generate Table 1*

##Exclued year 2008
dat1 = dat %>% dplyr::filter(year != '2008')

##Exclude missing Data
complete.rls = dat1 %>% 
              group_by(rls_code) %>% 
              summarise(weeks = n_distinct(periodid)) %>%
              mutate(no.wks.missing = max(weeks) - weeks, prop = 1-weeks/max(weeks))

complete.rls.codes <- complete.rls %>% 
                      filter(prop <= 0.11) %>% 
                      dplyr::select(rls_code)

##Province-Wide descriptives 
mean.by.region = dmti %>% filter(year != '2008') %>% group_by(rls_code) %>% summarise(no_supermarket = sum(supermarket==TRUE)) %>% ungroup() %>% summarise(mean.super = median(no_supermarket), sd.super = sd(no_supermarket))

dmti.overall = dmti %>% filter(year != '2008') %>% group_by(year) %>% 
summarise(no_supercentre = sum(supercentre==TRUE), no_convenience = sum(convenience==TRUE), no_supermarket = sum(supermarket==TRUE),no_fastfood = sum(fastfood==TRUE), no_pharmacy = sum(pharmacy==TRUE)) %>% mutate(mfrei = (no_supermarket + no_supercentre)/(no_pharmacy + no_convenience + no_supermarket + no_fastfood)) %>% data.frame()
dmti.overall$mfrei = (dmti.overall$no_supermarket + dmti.overall$no_supercentre)/(dmti.overall$no_pharmacy + dmti.overall$no_convenience + dmti.overall$no_supermarket + dmti.overall$no_fastfood)

total.pops = census.ts %>% group_by(year) %>% summarise(pop = sum(predict.pop))
dmti.overall = dmti.overall %>% left_join(total.pops)
dmti.overall$super.density = (dmti.overall$no_supermarket/dmti.overall$pop)*1000
dmti.overall = dmti.overall  %>% round(2) %>% dplyr::select(-pop) 

prices.overall = dat1 %>% group_by(year,type) %>% summarise(mean.real = weighted.mean(real.price,weight)) %>% spread(key=type, value=mean.real)
prices.overall$year = as.numeric(as.character(prices.overall$year))

dmti.overall = dmti.overall %>% left_join(prices.overall)
dmti.overall.sum = dmti.overall %>% mutate(healthy = no_supercentre + no_supermarket, unhealthy=no_convenience + no_fastfood + no_pharmacy) %>% select(year,healthy,unhealthy,mfrei,super.density,fruit,vegetable)

##Regional descriptives
dmti.sum = dmti %>% filter(year != '2008') %>% group_by(rls_code,year) %>% 
summarise(no_supercentre = sum(supercentre==TRUE), no_convenience = sum(convenience==TRUE), no_supermarket = sum(supermarket==TRUE),no_fastfood = sum(fastfood==TRUE), no_grocery = sum(grocery==TRUE),no_pharmacy = sum(pharmacy==TRUE)) 
dmti.sum$mfrei = (dmti.sum$no_supermarket + dmti.sum$no_supercentre)/(dmti.sum$no_pharmacy + dmti.sum$no_convenience + dmti.sum$no_supermarket + dmti.sum$no_fastfood)

##Means by region
dmti.sum.rls = dmti.sum %>% group_by(year) %>% summarise(m_supercentre = mean(no_supercentre), m_convenience = mean(no_convenience), m_supermarket = mean(no_supermarket),m_fastfood = mean(no_fastfood), m_pharmacy = mean(no_pharmacy)) %>% data.frame()

##Data to use for analysis - aggregate to region level
rls.means = dat %>% filter(rls_code %in% complete.rls.codes$rls_code & year != 2008) %>%
group_by(type,rls_code,periodid,year,bannerid) %>% 
summarise(mean.real = weighted.mean(real.price,weight), no.stores = n_distinct(storeid))
rls.means$year= as.factor(rls.means$year)
dmti.sum$year = as.factor(dmti.sum$year)

rls.means = rls.means %>% left_join(dmti.sum)
census.ts$year = as.factor(as.character(census.ts$year))
rls.means = rls.means %>% left_join(census.ts)
rls.means$super.density = (rls.means$no_supermarket/rls.means$predict.pop)*1000
rls.means$bannerid = factor(rls.means$bannerid)
rls.means$month = as.numeric(as.character(strftime(rls.means$periodid, format = "%m")))
rls.means$mfrei = rls.means$mfrei.raw*10
rls.means$year = as.numeric(rls.means$year)
rls.means$year2 = rls.means$year*rls.means$year


##Overall descriptives for Table 1
overall.mean = dat1 %>% group_by(type) %>%
  summarise(real.price = weighted.mean(real.price, weight,na.rm=TRUE)) 
mean.by.year = dat1 %>% group_by(type,year) %>% 
  summarise(real.price2 = weighted.mean(real.price, weight,na.rm=TRUE)) 

mean.by.year.reshape = mean.by.year %>% dcast(year ~ type) %>% filter(year != '2008') 