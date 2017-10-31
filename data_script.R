library("ggplot2")
library("RPostgreSQL")
library("cluster")
library("splines")
library('kml')
library("longitudinalData")
library('maps')
library('mapdata')
library('maptools') 
library('stringr')
library('data.table')
library('dplyr')


###Monthly exchange rates
exchange.rates = read.csv('exchange.rates.formatted.csv', header = TRUE)
exchange.rates$date = as.Date(exchange.rates$date, format = "%Y-%m-%d")

###Join CPI information and derive 'real price'
cpi = read.csv('cpi test.csv',header=TRUE)
cpi$date = as.Date(paste(as.character(cpi$date),"01",sep="-"), format = "%Y-%m-%d")
colnames(cpi)[3] <- 'vegetable'; cpi = melt(cpi, id=c("date"))
names(cpi) <- c('date','type','cpi')
cpi$year = strftime(cpi$date, format = "%Y")
cpi$month = strftime(cpi$date, format = '%m')
dat$month = strftime(dat$periodid, format = '%m')
dat$year = strftime(dat$periodid, format = "%Y")
dat = left_join(dat, cpi,c('year','month','type'))
head(cpi[cpi$year=="2010" & cpi$month=="11",])


dat$real.price = dat$serv_price_correct*mean(dat$cpi[dat$year=='2010'])/dat$cpi 

##Truncate the bins based on low and high prices##
bin.split = split(dat, dat$bin, drop = TRUE)

#~FOR SENSITIVITY ANALYSIS, ADJUST ARGUMENTS LOWER RANGE/UPPER RANGE
standardize.price = function(x,lower.range = 0.05, upper.range = 0.95){
  items = aggregate(x$serv_price_correct ~ x$itemdesc, FUN = 'mean')
  log.price = log(items[,2])

  mean.price = mean(log.price)
  sd.price = sd(log.price)
  standard.price = (log.price - mean.price)/sd.price
  truncation = quantile(standard.price,  c(lower.range, upper.range))
  items$standard.price = standard.price
 
  low.outlier.items =  items[items[,3] < truncation[1],'x$itemdesc']
  high.outlier.items = items[items[,3] > truncation[2],'x$itemdesc']
  
  logplot = ggplot(as.data.frame(standard.price), aes(standard.price)) + 
    geom_histogram() +
    geom_vline(xintercept = c(truncation[1],truncation[2]), color = 'red')
  
  outliers = c(unique(low.outlier.items),unique(high.outlier.items))
  return(list(logplot,outliers))
}

outlier.items = sapply(bin.split, function(x) standardize.price(x)[2]) 
outlier.vector = as.vector(unlist(outlier.items))

prop.discard = length(outlier.vector)/length(unique(dat$itemdesc)) ##Per bin it is 10%
dat = dat[!(dat$itemdesc %in% outlier.vector),]



# #############################Business directory data#############################
# ###Find duplicate POI_IDs and ID's with multiple RLS
# multiple.rls = dmti %>% group_by(poi_id) %>% count(count = n_distinct(rls_code)) %>% filter(count > 1)
# nrow(multiple.rls) ##Number with more than 1 RLS - EXCLUDE FOR NOW?
# dmti = dmti[!(dmti$poi_id %in% multiple.rls$poi_id ), ]
# 
# ###Restrict to supermarket category non-2008
# all.dat.0913 = dmti[which(dmti$year != 2008),]
# supermarkets = all.dat.0913[which(all.dat.0913$supermarket=='TRUE'),]
# nrow(supermarkets) == nrow(all.dat.0913[all.dat.0913$supermarket=='TRUE',])
# 
# #Split by year
# yearly.supermarket = split(supermarkets, supermarkets$year)
# x = yearly.supermarket[[2]]
# 
# #For each year, randomly select x% in appropriate categories to become supermarkets, grocery, convenience and remove
# ##The initial probabilities are derived from the 2008 data validation
# a = 0.24; b = 0.27; c = 0.18; d = 0.23; e = 0.25; f = 0.15
# cats = c('supermarket','convenience', 'grocery', 'remove')
# probs.alim = c(1-sum(a,b,c),a,b,c)
# probs.marche = c(1-sum(d,e,f),d,e,f)
# 
# ##The function randomly assigns new categories based on above probabilities for the appropriate storenames for each year
# replace.rows = function(x){
# set.seed(123)
#   
# original.rows = nrow(x)
# 
# alim.data = x[which(x$alimentation=='TRUE' & x$marche=='FALSE' & is.na(x$store)),]  ##Required specification to avoid dups later
# total.alim = nrow(alim.data)
# alim.data$new.cat = sample(cats, size = total.alim, replace= TRUE, prob = probs.alim)
# 
# alim.data$supermarket <- ifelse(alim.data$new.cat=='supermarket', 'TRUE', 'FALSE')
# alim.data$grocery <- ifelse(alim.data$new.cat=='grocery', 'TRUE', 'FALSE')
# alim.data$convenience <- ifelse(alim.data$new.cat=='convenience', 'TRUE', 'FALSE')
# alim.data$remove <- ifelse(alim.data$new.cat=='remove', 'TRUE', 'FALSE')
# nrow(alim.data)
# 
# marche.data = x[which(x$marche=='TRUE' & is.na(x$store)),]
# total.marche = nrow(marche.data)
# 
# marche.data$new.cat = sample(cats, size = total.marche, replace= TRUE, prob = probs.marche)
# 
# marche.data$supermarket <- ifelse(marche.data$new.cat=='supermarket', 'TRUE', 'FALSE')
# marche.data$grocery <- ifelse(marche.data$new.cat=='grocery', 'TRUE', 'FALSE')
# marche.data$convenience <- ifelse(marche.data$new.cat=='convenience', 'TRUE', 'FALSE')
# marche.data$remove <- ifelse(marche.data$new.cat=='remove', 'TRUE', 'FALSE')
# 
# revised.dat = rbind(marche.data, alim.data)
# x = rbind(x,subset(revised.dat,select = -new.cat))
# x = x %>% arrange(poi_id) %>%
#   group_by(poi_id) %>%
#   filter(row_number() == 1)
# 
# if (nrow(x) != original.rows) warning('not adding up')
# 
# return(x)
# }
# 
# #New data to replace old data for 2009-2013 - with checks
# ###Generate new data
# new.0913 = do.call(rbind,lapply(yearly.supermarket,replace.rows))
# nrow(all.dat.0913[which(all.dat.0913$supermarket=='TRUE'),]) == nrow(new.0913)
# ###replace 2009-2013 supermarket data
# all.dat.0913[which(all.dat.0913$supermarket=='TRUE'),] <- new.0913
# nrow(all.dat.0913[all.dat.0913$supermarket=='TRUE',]) == nrow(new.0913)
# ###replace primary data
# dmti[dmti$year != 2008 & dmti$supermarket=='TRUE',] <- new.0913

