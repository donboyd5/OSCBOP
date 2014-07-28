
library(knitr)


fbdbd <- "E:\\Data\\Federal budget and spending\\FFY 2015\\Database\\"

outlays <- "outlays.csv"
auth <- "budauth.csv"

dfout <- read.csv(paste0(fbdbd, outlays), nrow=-1)
dfauth <- read.csv(paste0(fbdbd, auth), nrow=-1)

ht(dfout)
ht(dfauth)

dflo <- dfout %>% gather(ffy, value, starts_with("x"))
dfla <- dfauth %>% gather(ffy, value, starts_with("x"))

ht(dflo)
names(dflo)
vnames <- c("agcode", "agname", "burcode", "burname", "acctcode", "acctname", "treasagcode", "subfuncode", "subfuntitle",
            "beacat", "grantng", "onoffbud", "TQ", "ffy", "value")
cbind(vnames, names(dflo))
vdefs <- data.frame(vnames=vnames, longnames=names(dflo))
names(dflo) <- vnames
dflo$ffy <- as.numeric(substr(dflo$ffy, 2, 5))
dflo$value <- cton(dflo$value)
ht(dflo)
count(dflo, "ffy")


tmp <- dflo %>% filter(ffy==2013)
ht(tmp)
names(tmp)
sum(tmp$value, na.rm=TRUE) / 1e6
str(tmp)

# agcode agname burcode burname acctcode acctname treasagcode subfuncode subfuntitle beacat grantng onoffbud
# burname 487 categories: CMS, SSA, interest, op/maint, Military personnel, ...
# beacat (mandatory, discretionary, net interest)
# grantng (nongrant, grant)
# onoffbud (onbud, offbud)
# subfuntitle 82 categories

# right <- mutate(scrambled, running = order_by(year, cumsum(value)))
# summarize by function
count(filter(tmp, substr(subfuncode, 1,2)=="37"), "subfuncode")

filter(tmp, grepl("Postal", subfuntitle))
filter(tmp, grepl("Postal", subfuntitle) & onoffbud=="Off-budget")

totspend <- sum(tmp$value, na.rm=TRUE) / 1e6

d <- tmp %>% group_by(subfuncode, subfuntitle) %>% 
  summarise(value=sum(value, na.rm=TRUE)/1e6) %>% 
  ungroup() %>%
  mutate(pcttot=value / totspend *100, 
         runningtot=order_by(-value, cumsum(value)), 
         runningpct=runningtot / totspend * 100, 
         grandtot=totspend) %>%
  arrange(-value)
kable(filter(d, runningpct < 95), digits=1)

# group_by:  subfuncode, subfuntitle; agcode, agname; 


d <- tmp %>% group_by(onoffbud, agcode, agname) %>% 
  summarise(value=sum(value, na.rm=TRUE)/1e6) %>% 
  ungroup() %>%
  mutate(pcttot=value / totspend *100, 
         runningtot=order_by(-value, cumsum(value)), 
         runningpct=runningtot / totspend * 100, 
         grandtot=totspend) %>%
  arrange(-value)
kable(filter(d, runningpct <=100), digits=1)

totspend <- sum(tmp$value[tmp$agcode==9]/1e6)
d <- tmp %>% filter(agcode==9) %>%
  group_by(burcode, burname) %>% 
  summarise(value=sum(value, na.rm=TRUE)/1e6) %>% 
  ungroup() %>%
  mutate(pcttot=value / totspend *100, 
         runningtot=order_by(-value, cumsum(value)), 
         runningpct=runningtot / totspend * 100, 
         grandtot=totspend) %>%
  arrange(-value)
kable(filter(d, runningpct <=100), digits=1)
kable(arrange(d, -value), digits=1)


totspend <- sum(tmp$value[tmp$agcode==9 & tmp$burcode==38]/1e6)
d <- tmp %>% filter(agcode==9 & burcode==38) %>%
  group_by(acctcode, acctname) %>% 
  summarise(value=sum(value, na.rm=TRUE)/1e6) %>% 
  ungroup() %>%
  mutate(pcttot=value / totspend *100, 
         runningtot=order_by(-value, cumsum(value)), 
         runningpct=runningtot / totspend * 100, 
         grandtot=totspend) %>%
  arrange(-value)
kable(filter(d, runningpct <= 95), digits=1)
kable(arrange(d, -value), digits=1)





tmp %>% group_by(grantng) %>% summarise(value=sum(value, na.rm=TRUE)/1e6) %>% arrange(-value)

tmp %>% group_by(burname) %>% summarise(value=sum(value, na.rm=TRUE)/1e6) %>% arrange(-value) %>% ht(., 10)

d <- tmp %>% group_by(subfuncode, subfuntitle) %>% summarise(value=sum(value, na.rm=TRUE)/1e6) %>% arrange(-value)
