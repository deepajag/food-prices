
pacman::p_load(ggplot2,RPostgreSQL,cluster,splines,kml,
               longitudinalData,maps,mapdata,maptools,stringr,
               data.table,dplyr,reshape2,multiwayvcov,lmtest)

###Reformat Census Data and Expand to All time Points
###Expand census data to all time points
census.ts <- expand.grid(rls_code = census$rls_code, year=seq(2006,2013))
census.ts %>% filter(rls_code=="0505") %>% arrange(rls_code)
census.long <- data.frame(rls_code = c(census$rls_code,census$rls_code), 
                          year = sort(rep(c(2006,2011),nrow(census))), 
                          pop = c(census$pop06, census$pop11), 
                          quintmat = c(census$quintmat06, census$quintmat11), 
                          area = c(census$area, census$area),
                          zone11 = c(census$zone11, census$zone11))
pop.model <- lm(log(pop) ~ year*rls_code, data = census.long)
dep.model <- lm(log(quintmat) ~ year*rls_code, data = census.long)
census.ts$predict.pop <- exp(predict(pop.model, newdata = census.ts , type="response"))
census.ts$quintmat <- exp(predict(dep.model, newdata = census.ts , type="response"))
areas = census.long %>% group_by(rls_code) %>% summarise(area = mean(area))
census.ts = left_join(census.ts, areas)
zones = census.long %>% select(rls_code, zone11) %>% distinct()
census.ts = left_join(census.ts, zones)
census.ts$pop.density <- census.ts$predict.pop/census.ts$area

###Join Monthly U.S. exchange rates
exchange.rates = read.csv('exchange.rates.formatted.csv', header = TRUE)
exchange.rates$date = as.Date(exchange.rates$date, format = "%Y-%m-%d")
exchange.rates$year <- strftime(exchange.rates$date, format = "%Y")
exchange.rates$month<- strftime(exchange.rates$date, format = "%m")
exchange.rates$lag <- c(NA,tail(exchange.rates$scaled_means,-1))

###Join CPI information and derive 'real price'
cpi = read.csv('cpi test.csv',header=TRUE)
cpi$date = as.Date(paste(as.character(cpi$date),"01",sep="-"), format = "%Y-%m-%d")
colnames(cpi)[3] <- 'vegetable'; cpi = melt(cpi, id=c("date"));  names(cpi) <- c('date','type','cpi')
cpi$year = strftime(cpi$date, format = "%Y"); cpi$month = strftime(cpi$date, format = '%m')
cpi.monthly.means = cpi %>% filter(year == "2010") %>% group_by(type, year, month) %>% summarise(mean.month.cpi = mean(cpi))

dat$month = strftime(dat$periodid, format = '%m'); dat$year = strftime(dat$periodid, format = "%Y")
dat <- dat %>% left_join(cpi, c("type","month","year"))
dat = dat %>% select(-date)

dat = dat %>% left_join(cpi.monthly.means, c("month", "type"))
dat = dat %>% select(-year.y) %>% rename(year = year.x)
dat$real.price = dat$serv_price_correct*dat$mean.month.cpi/dat$cpi 

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

outlier.items.s = sapply(bin.split, function(x) standardize.price(x,lower.range = 0.01, upper.range = 0.99)[2]) 
outlier.vector.s = as.vector(unlist(outlier.items.s))
prop.discard = length(outlier.vector.s)/length(unique(dat$itemdesc))  ##Per bin it is 4%

dat.sensitivity = dat[!(dat$itemdesc %in% outlier.vector.s),]
dat = dat[!(dat$itemdesc %in% outlier.vector),]


# #############################Business directory data#############################
###Find duplicate POI_IDs and ID's with multiple RLS
multiple.rls = dmti %>% group_by(poi_id) %>% count(count = n_distinct(rls_code)) %>% filter(count > 1)
dmti = dmti[!(dmti$poi_id %in% multiple.rls$poi_id ), ]

###Restrict to supermarket category non-2008
all.dat.0913 = dmti[which(dmti$year != 2008),]
supermarkets = all.dat.0913[which(all.dat.0913$supermarket=='TRUE'),]

#Split by year
yearly.supermarket = split(supermarkets, supermarkets$year)

#For each year, randomly select x% in appropriate categories to become supermarkets, grocery, convenience and remove
##The initial probabilities are derived from the 2008 data validation
true.numbers = read.csv('true-numbers.csv',header=TRUE)
probs.alim = true.numbers$props[true.numbers$Var2=='alimentation']
probs.marche= true.numbers$props[true.numbers$Var2=='marche']
#a = 0.24; b = 0.27; c = 0.18; d = 0.23; e = 0.25; f = 0.15
#cats = c('supermarket','convenience', 'grocery', 'remove')
cats = c('convenience', 'grocery','supermarket', 'remove')
probs.alim = c(1-sum(a,b,c),a,b,c)
probs.marche = c(1-sum(d,e,f),d,e,f)

##The function randomly assigns new categories based on above probabilities for the appropriate storenames for each year
replace.rows = function(x){
set.seed(123)

original.rows = nrow(x)

alim.data = x[which(x$alimentation=='TRUE' & x$marche=='FALSE' & is.na(x$store)),]  ##Required specification to avoid dups later
total.alim = nrow(alim.data)
alim.data$new.cat = sample(cats, size = total.alim, replace= TRUE, prob = probs.alim)

alim.data$supermarket <- ifelse(alim.data$new.cat=='supermarket', 'TRUE', 'FALSE')
alim.data$grocery <- ifelse(alim.data$new.cat=='grocery', 'TRUE', 'FALSE')
alim.data$convenience <- ifelse(alim.data$new.cat=='convenience', 'TRUE', 'FALSE')
alim.data$remove <- ifelse(alim.data$new.cat=='remove', 'TRUE', 'FALSE')

marche.data = x[which(x$marche=='TRUE' & is.na(x$store)),]
total.marche = nrow(marche.data)

marche.data$new.cat = sample(cats, size = total.marche, replace= TRUE, prob = probs.marche)

marche.data$supermarket <- ifelse(marche.data$new.cat=='supermarket', 'TRUE', 'FALSE')
marche.data$grocery <- ifelse(marche.data$new.cat=='grocery', 'TRUE', 'FALSE')
marche.data$convenience <- ifelse(marche.data$new.cat=='convenience', 'TRUE', 'FALSE')
marche.data$remove <- ifelse(marche.data$new.cat=='remove', 'TRUE', 'FALSE')

revised.dat = rbind(marche.data, alim.data)

x = data.frame(rbind(x %>% data.frame(),subset(revised.dat,select = -new.cat))) %>% data.frame()

x = x %>% arrange(poi_id) %>%
  group_by(poi_id) %>% arrange(supermarket) %>%
  filter(row_number() == 1) %>% data.frame()

nrow(x) == original.rows
if (nrow(x) != original.rows) warning('not adding up')

return(x)
}

#New data to replace old data for 2009-2013 - with checks
###Generate new data
new.0913 = do.call(rbind,lapply(yearly.supermarket,replace.rows))
nrow(all.dat.0913[which(all.dat.0913$supermarket=='TRUE'),]) == nrow(new.0913)
all.dat.0913[which(all.dat.0913$supermarket=='TRUE'),] <- new.0913
###replace primary data
dmti[dmti$year != 2008 & dmti$supermarket=='TRUE',] <- new.0913
##EXCLUDE WAL-MART
dmti$store <- gsub("^$|^ $", NA, dmti$store)
dmti = dmti[!(dmti$store %in% "WALMART"),]

####Fix other mis-classified supermarket
chain.counts = dmti %>% filter(supermarket=="TRUE") %>% group_by(store,year) %>% summarise(count=n()) %>% data.frame()
dmti$pharmacy[which(dmti$store=="UNIPRIX" & dmti$supermarket=="TRUE")] <- TRUE
dmti$supermarket[which(dmti$store=="UNIPRIX" & dmti$supermarket=="TRUE")] <- FALSE

convenience = c("SERVI EXPRESS","SHELL","GAZ")
dmti$convenience[which(dmti$store %in% convenience & dmti$supermarket=="TRUE")] <- TRUE
dmti$supermarket[which(dmti$store %in% convenience & dmti$supermarket=="TRUE")] <- FALSE

fastfood = c("CASSE CROUTE","HARVEY","RESTAURANT")
dmti$fastfood[which(dmti$store %in% fastfood & dmti$supermarket=="TRUE")] <- TRUE
dmti$supermarket[which(dmti$store %in% fastfood & dmti$supermarket=="TRUE")] <- FALSE

dmti$supercentre[which(dmti$store=="CLUB ENTREPOT" & dmti$supermarket=="TRUE")] <- TRUE
dmti$supermarket[which(dmti$store=="CLUB ENTREPOT" & dmti$supermarket=="TRUE")] <- FALSE

##Re-name misspelt stores
dmti$store[dmti$store=="METRO "] <- "METRO"
dmti$store[dmti$store=="METRO PLUS "] <- "METRO PLUS"

to.remove = c("SUCCURSALE","INFORMATIONINFROMATION",
              "LIVRAISONDELIVERY","FLEURFLOWER","DOLLA","DOLLAR","CATERERTRAITEUR","GALER")
dmti$remove[which(dmti$store %in% to.remove)] <- TRUE

dmti <- dmti[which(dmti$remove==FALSE | dmti$remove=="ADDITIONAL"),]

rm(all.dat.0913,yearly.supermarket,
        new.0913,bin.split, supermarkets, 
        cats, multiple.rls, pop.model,
        dep.model,areas,zones,census.long,outlier.vector,probs.alim,probs.marche,
        convenience,fastfood,to.remove)


save.image('data.RData')
