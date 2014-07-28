# usaspending.r
# Don Boyd
# 7/28/2014

# http://www.usaspending.gov/datafeeds/2014_All_Contracts_Full_20140515.csv.zip

# http://stackoverflow.com/questions/22005419/dplyr-without-hard-coding-the-variable-names
# http://adv-r.had.co.nz/Computing-on-the-language.html
# https://groups.google.com/forum/#!topic/manipulatr/cr9PzNEtz6w
  


# how to pass quoted vars to dplyr functions
dfa <- data.frame(a=1:10, b=11:20, c=21:30)
uvf <- function(vars) parse(text=paste0("c(", paste(vars, collapse=", "), ")"))
uvars <- uvf(c("b", "c"))
select(dfa, eval(uvars))
dfa %>% select(eval(uvars))



bopd <- "E:\\Data\\Projects\\OSC BOP\\"

dirbase<-paste0(datdir,"USASpending\\")
urlbase<-"http://usaspending.gov/datafeeds/"


fngrants<-"_All_Grants_Full_"
fbdp<-"_All_DirectPayments_Full_"
fncon<-"_All_Contracts_Full_"
fnloans<-"_All_Loans_Full_"
fninsur<-"_All_Insurance_Full_"

fn <- "2013_All_Contracts_Full_20140515.csv"


#****************************************************************************************************
#
#                USASpending.gov ####
#
#****************************************************************************************************
system.time(df <- read.csv(paste0(bopd, fn))) # 809 secs
# system.time(saveRDS(df, paste0(bopd, "contracts_2013.rds")))  # 137 secs
system.time(df <- readRDS(paste0(bopd, "contracts_2013.rds")))  # 17 secs

ht(df)
str(df)
names(df)

df2 <- df %>% select(transaction_status, maj_agency_cat, obligatedamount, maj_agency_cat) %>%
  filter(transaction_status=="active") %>%
  group_by(maj_agency_cat) %>%
  summarise(sumob=sum(obligatedamount, na.rm=TRUE) / 1e9) %>% 
  arrange(-sumob)
df2
sum(df2$obligatedamount)/1e9


df2 <- df %>% select(transaction_status, maj_agency_cat, statecode, obligatedamount, maj_agency_cat) %>%
  filter(transaction_status=="active") %>%
  group_by(statecode) %>%
  summarise(sumob=sum(obligatedamount, na.rm=TRUE) / 1e9) %>% 
  arrange(-sumob)
df2


df2 <- df %>% select(transaction_status, maj_agency_cat, state, obligatedamount, maj_agency_cat) %>%
  filter(transaction_status=="active") %>%
  group_by(state) %>%
  summarise(sumob=sum(obligatedamount, na.rm=TRUE) / 1e9) %>% 
  arrange(-sumob)
df2


#****************************************************************************************************
#
#                USASpending.gov direct payments ####
#
#****************************************************************************************************

dirbase<-paste0(datdir,"Federal budget and spending\\USASpending\\")
fbdp<-"_All_DirectPayments_Full_"


# create rds files as needed
fnbase <- fbdp
revdate<-"20140715"
dir <- paste0(dirbase, "DirectPayments\\")
for(yr in 2013:2013){
  fn <- paste0(yr, fnbase, revdate, ".csv")
  print(fn)
  system.time(df <- read.csv(paste0(dir, fn), nrows=-1)) # about 2 mins
  saveRDS(df, paste0(dir, "directpay", yr, ".rds"))
  memory() # to be sure garbage collection is done and memory freed where possible
}

system.time(df <- readRDS(paste0(dir, "directpay2013.rds"))) # 7 secs 2.2m obs
names(df)
ht(df)
str(df)
head(df)

count(df, "transaction_status") # 100% active
count(df, "transaction_status") #
count(df, "principal_place_state") # messy but recognizable
count(df, "transaction_status") #


tot <- sum(df$fed_funding_amount, na.rm=TRUE) / 1e9

d <- df %>% group_by(principal_place_state_code) %>%
  summarise(fed_funding_amount=sum(fed_funding_amount, na.rm=true)/1e9) %>%
  arrange(-fed_funding_amount) %>%
  mutate(cumsum=cumsum(fed_funding_amount), pct=fed_funding_amount / tot * 100, cumpct=cumsum / tot * 100)
kable(head(d, 15))

# get top programs by state
# recode programs so we can get the top ones
psums <- df %>% group_by(cfda_program_title) %>%
  summarise(fed_funding_amount=sum(fed_funding_amount, na.rm=true)/1e9) %>%
  arrange(-fed_funding_amount) %>%
  head(9)
df$progrecode <- ifelse(df$cfda_program_title %in% psums$cfda_program_title, as.character(df$cfda_program_title), "Other")
count(df, "progrecode")
  
d <- df %>% group_by(principal_place_state_code, progrecode) %>%
  summarise(fed_funding_amount=sum(fed_funding_amount, na.rm=true)/1e9) %>%
  mutate(stname=getstname(principal_place_state_code)) %>%
  spread(progrecode, fed_funding_amount)
# put col names in a good order
cnames <- c("stname", "principal_place_state_code", as.character(psums$cfda_program_title), "Other")
d2 <- d[, cnames]
d2 <- arrange(d2, stname, principal_place_state_code)
write.csv(d2, "e:\\d2.csv", row.names=FALSE)

tab <- kable(arrange(d2, stname, principal_place_state_code), digits=2)


kable(head(d, 15), 2)




vars <- c("unique_transaction_id","statecode", "pop_state_code","obligatedamount")
don <- df %>% select(eval(uvf(vars))) %>% 
  group_by(statecode) %>%
  summarise(totoblig=sum(obligatedamount, na.rm=true)/1e9) %>%
  arrange(-totoblig)
don



#****************************************************************************************************
#
#                USASpending.gov grants ####
#
#****************************************************************************************************
dirbase<-paste0(datdir,"Federal budget and spending\\USASpending\\")
fngrants<-"_All_Grants_Full_"
outdir<-paste0(dirbase, revdate, "\\")
usadir<-paste0(dirbase,revdate,"\\")

# create rds files as needed
fnbase <- fngrants
revdate<-"20140715"
dir <- paste0(dirbase, "Grants\\")
for(yr in 2013:2013){
  fn <- paste0(yr, fnbase, revdate, ".csv")
  print(fn)
  system.time(df <- read.csv(paste0(dir, fn), nrows=-1)) # about 2 mins
  saveRDS(df, paste0(dir, "grants", yr, ".rds"))
  memory() # to be sure garbage collection is done and memory freed where possible
}

system.time(df <- readRDS(paste0(dir, "grants2013.rds")))
names(df)



ht(df)

system.time(df <- readRDS(paste0(bopd, "contracts_2013.rds")))  # 17 secs


#****************************************************************************************************
#
#                USASpending.gov contracts ####
#
#****************************************************************************************************

# revdate<-"20120814"
revdate<-"20130316"
dirbase<-paste0(datdir,"Federal budget and spending\\USASpending\\")
fngrants<-"_All_Grants_Full_"
usadir<-paste0(dirbase,revdate,"\\")

system.time(df <- readRDS(paste0(bopd, "contracts_2013.rds")))  # 17 secs
names(df)

vars <- c("unique_transaction_id","statecode", "pop_state_code","obligatedamount")
don <- df %>% select(eval(uvf(vars))) %>% 
  group_by(statecode) %>%
  summarise(totoblig=sum(obligatedamount, na.rm=true)/1e9) %>%
  arrange(-totoblig)
don

arrange(don, totoblig)

df<-readRDS(paste0(usadir,"stgrants.rds"))

# look at Medicaid for one year, see what gives
system.time(df<-readRDS(paste0(usadir,"grants",2012,".rds")))
head(df)
count(df,"recipient_state_code")

nyg<-subset(df,recipient_state_code=="NY")
count(nyg,c("recip_cat_type","recipient_type"))
nyg$rcat<-substr(nyg$recipient_type,1,2)
count(nyg,c("rcat","recipient_type"))

nyg$fedm<-nyg$fed_funding_amount/1e6
ddply(subset(nyg,rcat<="06"),.(recipient_type),summarise,sum=round(sum(fedm,na.rm=TRUE),3))
head(nyg)

catg<-ddply(nyg,.(recipient_type,maj_agency_cat),summarise,sum=round(sum(fedm,na.rm=TRUE),3))
local<-subset(nyg,rcat>="01" & rcat<="06")
head(local)
names(local)
keepvars<-c("cfda_program_num","cfda_program_title","recipient_name","recipient_type","fedm")
l2<-local[,keepvars]
head(local)
head(l2)
head(arrange(l2,-fedm))
subset(l2,cfda_program_num=="93.600")

loccfda<-ddply(l2,.(cfda_program_num,cfda_program_title),summarise,sum=round(sum(fedm,na.rm=TRUE),3))
head(arrange(loccfda,-sum))

don<-subset(nyg,cfda_program_num=="93.600")
don<-arrange(don,-fedm)
head(don)


