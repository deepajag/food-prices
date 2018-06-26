
pacman::p_load(ggplot2,RPostgreSQL,cluster,splines,kml,
longitudinalData,maps,mapdata,maptools,stringr,
data.table,dplyr,reshape2,multiwayvcov,lmtest,tidyr,sjPlot, sjstats, sjmisc,
parallel)

setwd('~/analysis_tmp/Nielsen')
load('data-fsa.RData')
load('paper3-data.RData')
load('themes_functions.Rdata')
load('paper3-bootstraps.RData')

##1) OVERALL DESCRIPTIVES
top.veg$bin = as.character(top.veg$bin); top.fruit$bin = as.character(top.fruit$bin)
reps = c(rep("vegetable",nrow(top.veg)), rep("fruit",nrow(top.fruit)))
std.basket = data.frame(bin=c(top.veg$bin,top.fruit$bin),type=reps)
items_data$bin[items_data$bin=="apple "] <- "apple"

items_std = items_data %>% filter(bin %in% std.basket$bin)

dat$year <- as.factor(dat$year)

data = items_std ; x='nielsen'; year2=2011
mean.sd = function(x){
  mean=round(mean(x),2)
  sd=round(sd(x),2)
  return(list(mean=mean,sd=sd))
}

no.chains = length(unique(data[['bannerid']]))
no.stores.per.chain = data %>% group_by(bannerid)  %>% summarise(no_store = n_distinct(storeid))
no.bins.per.chain = data %>% group_by(bannerid) %>% summarise(no_bin = n_distinct(bin))
no.items.per.bin = data %>% group_by(bin) %>% summarise(no_items = n_distinct(itemdesc))
no.items.per.bin.store = data %>% group_by(bin,storeid,month,bannerid,year) %>% summarise(no_items = n_distinct(itemdesc))
no.items.per.bin.organic = data %>% filter(organic==TRUE) %>% group_by(bin) %>% summarise(no_items = n_distinct(itemdesc))
no.items.store.month <- items_data %>% group_by(postalprefix,bannerid,storeid,month,year) %>% summarise(mean_item = n_distinct(itemdesc), prop_organic = sum(organic==TRUE)/mean_item) 

stores = mean.sd(no.stores.per.chain$no_store)
bins = mean.sd(no.bins.per.chain$no_bin)
items.month = mean.sd(no.items.store.month$mean_item)
items.bin = mean.sd(no.items.per.bin$no_items)
items.bin.storeid = mean.sd(no.items.per.bin.store$no_items)
items.bin.organic = mean.sd(no.items.per.bin.organic$no_items)
organic.month = mean.sd(no.items.store.month$prop_organic)

top.variety = no.items.per.bin.store  %>% data.frame() %>% group_by(bin) %>% summarise(no_items=mean(no_items)) %>% arrange(-no_items) %>% data.frame()
exclude.bins = top.variety %>% filter(no_items < 2) %>% dplyr::select(bin)

##2) CREATE AVAILABILTIY AND VARIETY INDICATORS
bin.data.wide = bin.data %>% 
  group_by(postalprefix,year,month,storeid,bannerid) %>%
  distinct(bin) %>% mutate(include = 1) %>% spread(bin, include) %>% data.frame()
bin.data.wide[is.na(bin.data.wide)] <- 0

##Large standard basket
bin.data.wide.r = bin.data.wide[,c( "bannerid", "storeid" ,"year" ,"month", "postalprefix",colnames(bin.data.wide)[colnames(bin.data.wide) %in% std.basket$bin])]

sub.cols = bin.data.wide.r[,colnames(bin.data.wide)[colnames(bin.data.wide) %in% std.basket$bin]]
sub.cols$no_available = rowSums(sub.cols)
bin.data.wide.r$no_available = sub.cols$no_available
bin.data.wide.r$available.all = ifelse(bin.data.wide.r$no_available==nrow(std.basket),1,0)
bin.data.wide.r$p.available.all<- bin.data.wide.r$no_available/as.numeric(nrow(std.basket))

wide.r = bin.data.wide.r %>% dplyr::select(year,bannerid,postalprefix,storeid,month,no_available,available.all,p.available.all)

##VARIETY
items = items_std %>% group_by(postalprefix,bannerid,storeid,month,year) %>% summarise(item.m = n_distinct(itemdesc)) %>% arrange(storeid)
items$year = as.numeric(as.character(items$year))
bin.avg = items_std  %>% group_by(bin,bannerid,postalprefix,month,year,storeid) %>% summarise(item.m.bin = n_distinct(itemdesc)) %>% arrange(storeid) 
items = items %>% left_join(bin.avg)

##Join full data
wide.r$year = factor(wide.r$year); items$year=factor(items$year); census.ts$year = as.factor(census.ts$year) 

all.ds = left_join(wide.r ,items) %>% #right_join(ds.match %>% filter(year != 2008))   %>% 
left_join(census.ts %>% filter(year %in% c('2009','2010','2011','2012','2013')), by=c('postalprefix'='fsa', 'year'='year')) %>% #left_join(nhs_2011, by=c('postalprefix'='fsa')) %>% 
left_join(chain.levels)
#all.ds$source = ifelse(is.na(all.ds$storeid),'dmti','nielsen')
all.ds$store.months = factor(strftime(as.Date(paste(all.ds$year,all.ds$month,'01',sep='-'), format="%Y-%m-%d"), format="%Y-%m"))

##AVAILABILITY (p=0.859)
all.ds$pp.available.all <- all.ds$no_available/max(all.ds$no_available)
all.ds$available.all.binary = ifelse(all.ds$pp.available.all <=p,0,1)

