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

###########First import data and call it dat##############

##Truncate the bins based on low and high prices##
bin.split = split(dat, dat$bin, drop = TRUE)

#~FOR SENSITIVITY ANALYSIS, ADJUST ARGUMENTS LOWER RANGE/UPPER RANGE
outlier.items = unlist(outlier.items)
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
outlier.vector = as.vector(outlier.items)

prop.discard = length(outlier.vector)/length(unique(dat$itemdesc)) ##Per bin it is 10%
dat = dat[!(dat$itemdesc %in% outlier.vector),]


##Generate monthly means
monthly.rls.means  = dat %>% 
  group_by(month,year,rls_code) %>% 
  summarise(nominal.price = weighted.mean(serv_price_correct,w=volume_weight_monthly, na.rm = TRUE)) %>%
  as.data.frame()

monthly.rls.means$date = 
  as.Date(paste(as.character(monthly.rls.means$year),"-",
                as.character(monthly.rls.means$month),
                "-01",sep=""), format = "%Y-%m-%d")

no.months.by.rls = 
  aggregate(date ~ rls_code , 
            data = monthly.rls.means, function(x) length(unique(x)))

missing.rls = unique(
  monthly.rls.means$rls_code[
    monthly.rls.means$rls_code %in%
      no.months.by.rls$rls_code[no.months.by.rls$date < max(no.months.by.rls$date)]
    ]
)

monthly.rls.means = monthly.rls.means[!(monthly.rls.means$rls_code %in% missing.rls),]

##Business directory data
##Randomly classify xx percent of 'EPICERIE" as grocery/convenience
##Matrix for percentages in corrected categories
classifications = c('supermarket','convenience','grocery','remove')
original = c('alimentation','marche','epicerie')   
change.matrix = matrix(c(62, 315, 0, 54, 218, 11, 47, 200, 21, 36, 134, 11),  ncol = 4)
colnames(change.matrix) = classifications ; rownames(change.matrix) = original
changes = data.frame(change.matrix)
changes$n = apply(changes, 1, sum); changes$p = changes/changes$n
changes
















