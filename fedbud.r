

fbdbd <- "E:\\Data\\Federal budget and spending\\FFY 2015\\Database\\"

outlays <- "outlays.csv"
auth <- "budauth.csv"

dfout <- read.csv(paste0(fbdbd, outlays), nrow=-1)
dfauth <- read.csv(paste0(fbdbd, auth), nrow=-1)

ht(dfout)
ht(dfauth)

dfl <- dfout %>% gather(ffy, value, starts_with("x"))
dfl <- dfauth %>% gather(ffy, value, starts_with("x"))

ht(dfl)
names(dfl)
vnames <- c("agcode", "agname", "burcode", "burname", "acctcode", "acctname", "treasagcode", "subfuncode", "subfuntitle",
            "beacat", "grantng", "onoffbud", "TQ", "ffy", "value")
cbind(vnames, names(dfl))
vdefs <- data.frame(vnames=vnames, longnames=names(dfl))
names(dfl) <- vnames
dfl$ffy <- as.numeric(substr(dfl$ffy, 2, 5))
dfl$value <- cton(dfl$value)
ht(dfl)
count(dfl, "ffy")


tmp <- dfl %>% filter(ffy==2013)
ht(tmp)
names(tmp)
sum(tmp$value, na.rm=TRUE) / 1e6

# agcode agname burcode burname acctcode acctname treasagcode subfuncode subfuntitle beacat grantng onoffbud
# burname 487 categories: CMS, SSA, interest, op/maint, Military personnel, ...
# beacat (mandatory, discretionary, net interest)
# grantng (nongrant, grant)
# onoffbud (onbud, offbud)
# subfuntitle 82 categories

tmp %>% group_by(grantng) %>% summarise(value=sum(value, na.rm=TRUE)/1e6) %>% arrange(-value)

tmp %>% group_by(burname) %>% summarise(value=sum(value, na.rm=TRUE)/1e6) %>% arrange(-value) %>% ht(., 10)

d <- tmp %>% group_by(subfuncode, subfuntitle) %>% summarise(value=sum(value, na.rm=TRUE)/1e6) %>% arrange(-value)
