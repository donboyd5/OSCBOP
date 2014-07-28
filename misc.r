
library(btools)
bopd <- "E:\\Data\\Projects\\OSC BOP\\"


# http://www.opm.gov/data/index.aspx

# http://www.usa.gov/directory/federal/office-of-management-and-budget.shtml

# http://www.openanalyticssummit.com/oadc/building-an-api-on-ombs-public-budget-database-ryan-harvey-omb/
# http://www.linkedin.com/in/ryanbharvey
# ryan 'dot' b 'dot' harvey 'at' gmail 'dot' com 



#****************************************************************************************************
#
#                Functions, etc. ####
#
#****************************************************************************************************






#****************************************************************************************************
#
#                CFFR ####
#
#****************************************************************************************************
# DR Direct Payments for Individuals (Retirement and Disability only)
# DO Direct Payments for Individuals (Other than Retirement and Disability)
# DX Direct Payments Other than for Individuals
# GG Grants (Block, Grants, Formula Grants, Project Grants, and Cooperative Agreements)
# PC Procurement Contracts
# SW Salaries and Wages
# DL Direct Loans
# GL Guaranteed/Insured Loans
# II Insurance
# keep: Retirement and disability, Other direct payments, Grants, Procurement, Salaries
cf<-getdata("cffr")
cf<-subset(cf,year==2010)
count(cf,"object")
olevs<-c("DL","DO","DR","DX","GG","GL","II","PC","SW")
olevs2<-c("DL","DO","DR","DO","GG","GL","II","PC","SW")
olabs<-c("Direct Loans","Direct Payments for Individuals (excl retirement/disb)","Direct Payments for Individuals, retire-disb",
         "Direct Payments Other than for Individuals","Grants","Guaranteed/Insured Loans","Insurance","Procurement Contracts","Salaries and Wages")
cf$objf<-factor(cf$object,levels=olevs,labels=olabs)
cf$group<-factor(cf$object,levels=olevs,labels=olevs2)
head(cf)
cf<-subset(cf,!object %in% c("DL","GL","II"))
count(cf,"stabbr")
count(cf,c("object","group"))
ddply(subset(cf, stabbr=="US"),.(object,objf),summarise,sum=sum(value,na.rm=TRUE)/1e9)

don<-subset(cf, stabbr=="US" & object=="DR")
don<-arrange(don,-value)
head(don,15)

# collapse categories, get per capita, then make wide
cf$group<-as.character(cf$group)
cfcat<-ddply(cf,.(stabbr,group),summarise,sumk=sum(value,na.rm=TRUE)/1e3)
head(cfcat)
pop<-subset(getdata("pop"),year==2010,select=c(stabbr,pop))
cfcat<-merge(cfcat,pop)
cfcat$valpc<-cfcat$sumk/cfcat$pop
dfw<-dcast(cfcat,stabbr~group,value.var="valpc",sum,margins="group")
names(dfw)[length(names(dfw))]<-"sum"
arrange(dfw,-sum)
order(dfw)
count(cfcat,c("stabbr","group"))


#****************************************************************************************************
#
#                CFDA database -- create crosswalk ####
#
#****************************************************************************************************
# crosswalking cfda to budget account codes
# tanf: 75-1552-0-1-609
# treasagcode-acctcode-?-?-subfuncode

# get these vars from cfda file
# Program Title, Program Number, Federal Agency (030), Types of Assistance (060), 
# Formula and Matching Requirements (101), Account Identification (121)

# fn<-"programs-full-usaspending13089.csv"
# fnx<-"programs-full-usaspending13089.xlsx" # I opened the csv in Excel and saved as xlsx -- this works best I think
# # fnc<-"programs-full-usaspending13089x.csv" # I converted to Excel and then created this
# 
# fn<-"programs-full13089.csv"
fn<-"programs-full13089djb.csv" # I had to clean the data by hand to delete bad rows in the middle - not sure I did it well
df<-read.csv(paste0(cfdad,fn),colClasses="character",fill=TRUE,stringsAsFactors=FALSE,allowEscapes=TRUE,nrows=-1)
df<-read.csv(paste0(cfdad,fn),colClasses="character",fill=TRUE,stringsAsFactors=FALSE,nrows=-1)
names(df)
keepvars<-names(df)[c(1,2,23)]
df2<-df[,keepvars]
head(df2); tail(df2)
names(df2)
names(df2)<-c("progtitle","cprognum","acctnum")
df2$prognum<-round(as.numeric(df2$cprognum),3)
count(df2,"prognum")
df2$cprognum<-NULL
head(df2); tail(df2)

don<-count(df2,"acctnum")
don<-arrange(don,-freq)
head(don)


#****************************************************************************************************
#
#                Federal budget database ####
#
#****************************************************************************************************
# works with the public budget database, which can be found at:
# http://www.whitehouse.gov/omb/budget/Supplemental
# http://www.whitehouse.gov/sites/default/files/omb/budget/fy2013/assets/budauth.csv
# http://www.whitehouse.gov/sites/default/files/omb/budget/fy2013/assets/receipts.csv
# http://www.whitehouse.gov/sites/default/files/omb/budget/fy2013/assets/outlays.csv

# fburl<-"http://www.whitehouse.gov/sites/default/files/omb/budget/fy2013/assets/"
fburl<-"http://www.whitehouse.gov/sites/default/files/omb/budget/fy2014/assets/"
# "http://www.whitehouse.gov/sites/default/files/omb/budget/fy2014/assets/budget.pdf"

fbauth<-"budauth.csv"
fbreceipts<-"receipts.csv"
fboutlays<-"outlays.csv"


# Agency Code
# Agency Name
# Bureau Code
# Bureau Name
# Account Code
# Account Name
# Treasury Agency Code
# Subfunction Code
# Subfunction Title
# BEA Category
# Grant/non-grant split
# On- or Off- Budget


# Per documentation:
#  If you plan to use these data in a relational database, you should designate the following fields as
#  ?primary? to uniquely identify each row of data: agency code, bureau code, account code,
#  subfunction code, BEA category, grant/non-grant, and on-/off-budget field.

# agency codes for top 10 grant-making agencies in FFY 2012 (proposed)
# HHS   009
# DOT   021
# ED    018
# HUD   025
# Agri  005
# DOL   012
# Treas 015
# EPA   020
# Inter 010
# DOJ   011

#                RUN ONCE ####
# download.file(paste0(fburl,fbauth),paste0(feddbd,fbauth),mode="wb")
# download.file(paste0(fburl,fbreceipts),paste0(feddbd,fbreceipts),mode="wb")
# download.file(paste0(fburl,fboutlays),paste0(feddbd,fboutlays),mode="wb")
#                End RUN ONCE ####

# read the data ####
authority<-read.csv(paste0(feddbd,fbauth))
head(authority); tail(authority)

outlays<-read.csv(paste0(feddbd,fboutlays))
head(outlays); tail(outlays)

receipts<-read.csv(paste0(feddbd,fbreceipts))
head(receipts); tail(receipts)

#### NOTE ####
# a grant can have a discretionary component AND a mandatory component - e.g. fed hwy grants!
# 021, 15, 8083 agcode, burcode, acctcode
# (although in most years it was entirely Mandatory)

# multiple cfda numbers can fall within a single budget code. for example,
# 20.200 20.205 20.215, 20.219, 20.240, 20.318, and perhaps several others
# all fall within 69-8083-0-7-401
# ALSO, multiple fed budget accounts can be mapped to a single cfda code
# for example 69-8083-0-7-401; 69-1136-0-1-401 are both mapped to
# 20.701 University Transportation Centers
# per cfda catalog,
# Account Identification --This 11-digit budget account identification code represents the account that
# funds a particular program. This code should be consistent with the code given for the program area as 
# specified in Appendix III of the Budget of the United States Government. (See Appendix III for further
# information on the meaning of the 11 digits of this code.)

# create long authority file
idvars<-c("agcode","agname","burcode","burname","acctcode","acctname","treasagcode","subfuncode","subfunction","beacatg","onoffbud")
names(authority)[1:length(idvars)]<-idvars
head(authority)
authl<-melt(authority,id=idvars)
authl$value<-ctov(authl$value)
authl<-subset(authl,!is.na(value) & variable!="TQ") # drop transition quarter
count(authl,"variable")
authl$ffyear<-as.numeric(substr(authl$variable,2,5))
authl$variable<-NULL
count(authl,"ffyear")





# outlays has Grant/non-grant split, which authority does not have. Per db doc:
# "Grants to State and local governments are separated from non-grant outlays, based on the definitions
# in OMB Circular A-11 for schedule C data."
idvars<-c("agcode","agname","burcode","burname","acctcode","acctname","treasagcode","subfuncode","subfunction","beacatg","grantnongrant","onoffbud")
names(outlays)[1:length(idvars)]<-idvars
head(outlays)
outl<-melt(outlays,id=idvars)
outl$value<-ctov(outl$value)
outl<-subset(outl,!is.na(value) & variable!="TQ") # drop transition quarter
count(outl,"variable")
outl$ffyear<-as.numeric(substr(outl$variable,2,5))
outl$variable<-NULL
count(outl,"ffyear")
head(outl)
count(outl,"grantnongrant")
count(outl,"grantnongrant")
grants<-subset(outl,grantnongrant=="Grant")
(df<-ddply(grants,ffyear~beacatg,summarise,sumgrants=sum(value,na.rm=TRUE)/1e6))
df2<-dcast(df,ffyear~beacatg,sum,na.rm=TRUE,margins="beacatg")
ddply(subset(grants, ffyear>=2008 & ffyear<=2018),.(ffyear),summarise,sumgrants=sum(value,na.rm=TRUE)/1e6)

qplot(ffyear,sumgrants, data=df, colour=beacatg, geom=c("point","line"))

# put grants into real terms and look
head(grants); tail(grants)
head(df)
idx<-match(df$ffyear,gdppifc$year) # matches ffyear to cyear for gdppi
df$rvalue<-df$sumgrants*gdppifc$pmult[idx]
qplot(ffyear,rvalue, data=df, colour=beacatg, geom=c("point","line"))

qplot(ffyear,rvalue, data=subset(df,ffyear>=1980), colour=beacatg, geom=c("point","line"))


pdata<-subset(df,ffyear>=1980)
pdata$varf<-pdata$beacatg
xlab<-"\nSource: President's budget for FFY 2014, public database"
ylab<-"Billions of 2012 $"
main<-"Discretionary and mandatory grants to state and local governments, history and forecast"
p<-ggplot(data=pdata, aes(x=ffyear, y=rvalue, group=varf)) # use ggplot to gain complete control
p<-p + geom_line(aes(colour=varf,linetype=varf), size=1.5) # join observations with lines 
p<-p + geom_point(aes(colour=varf,shape=varf), size=3) # add points
# p<-p + geom_hline(yintercept=0, size=1.2) # add a thicker reference line
# now start putting text on the graph
p<-p+xlab(xlab)
p<-p+labs(colour="",linetype="",shape="")
p<-p+labs(title=main)
p+scale_y_continuous(ylab,limits=c(0,475))



# what are the largest discretionary and mandatory grants?
bigg<-subset(grants,ffyear==2012)
bigg$value<-bigg$value/1e6 # now in $ billions
bigg<-arrange(bigg,-value)
head(bigg,10)
pcols<-c("agname","acctname","value") # "burname"
head(bigg[bigg$beacatg=="Mandatory",pcols],10)
head(bigg[bigg$beacatg=="Discretionary",pcols],10)

head(bigg[bigg$beacatg=="Mandatory",],10)
head(bigg[bigg$beacatg=="Discretionary",],10)



df<-subset(grants,ffyear==2011)
df$valueb<-df$value/1e6
df$value<-NULL
df<-arrange(df,-valueb)
head(df,20)
write.csv(df,paste0(blinkd,"fedbudgrants2011.csv"))


head(grants)
pcols<-c("agname","acctname","value") # "burname"
head(bigg[bigg$beacatg=="Mandatory",pcols],10)
head(bigg[bigg$beacatg=="Discretionary",pcols],10)
ddply(subset(grants, ffyear>=2008 & ffyear<=2018),.(ffyear),summarise,sumgrants=sum(value,na.rm=TRUE)/1e6)

keep<-c("Federal-aid Highways","Tenant Based Rental Assistance","Accelerating Achievement and Ensuring Equity","Special Education")
subset(grants, acctname=="Tenant Based Rental Assistance" & beacatg=="Discretionary")
d<-subset(grants, acctname %in% keep & beacatg=="Discretionary" & ffyear>=2005)
d$value<-d$value/1e6
dcast(d,acctname~ffyear,sum,margins="acctname")

df<-outl
head(df); tail(df)
df$sf3<-substr(df$subfuncode,3,3)
(d<-ddply(subset(df, ffyear==2012 & grantnongrant=="Grant"),.(sf3),summarise,sum=sum(value,na.rm=TRUE)/1e6))
sum(d$sum)
head(grants)

# tanf: 75-1552-0-1-609
# treasagcode-acctcode-?-?-subfuncode
# Agency Code  Agency Name  Bureau Code	Bureau Name	Account Code	Account Name	Treasury Agency Code	Subfunction Code	Subfunction Title	BEA Category	Grant/non-grant split	On- or Off- Budget
# 009  Department of Health and Human Services	70	Administration for Children and Families	1552	Temporary Assistance for Needy Families	75	609	Other income security	Mandatory	Grant	On-budget



#****************************************************************************************************
#
#                Grants up and down ####
#
#****************************************************************************************************
fcodes<-c("05","15","25","27","30","35","37","40","45","50","55","57","60","65","70","75","80","90","92","95")
fnames<-c("National Defense","International Affairs","General Science, Space, and Technology","Energy",
          "Natural Resources and Environment","Agriculture","Commerce and Housing Credit","Transportation",
          "Community and Regional Development","Education, Training, Employment, and Social Services","Health",
          "Medicare","Income Security","Social Security","Veterans Benefits and Services","Administration of Justice",
          "General Government","Net Interest","Allowances","Undistributed Offsetting Receipts")
length(fcodes)-length(fnames)
grants<-subset(outl,grantnongrant=="Grant")
grants$fcode<-substr(sprintf("%03i",grants$subfuncode),1,2) # zero-pad
grants$fname<-factor(grants$fcode,levels=fcodes,labels=fnames)
count(grants,c("fcode","fname"))
idx<-match(grants$ffyear,gdppifc$year) # matches ffyear to cyear for gdppi
grants$rvalue<-grants$value*gdppifc$pmult[idx]
grants$rvalueb<-grants$rvalue/1e6
# identify up and downs
idvars<-c("agcode","burcode","acctcode","subfuncode","beacatg")
updown<-dcast(subset(grants,ffyear %in% c(2013,2018),select=c(idvars,"ffyear","rvalue")),...~ffyear,value.var="rvalue")
head(updown)

idvars<-c("agcode","burcode","acctcode","acctname","fcode","fname","subfuncode","subfunction","beacatg")
df<-dcast(subset(grants,ffyear>=2010,select=c(idvars,"ffyear","rvalueb")),...~ffyear,value.var="rvalueb")
head(df)
df$change<-df$"2018"-df$"2013"
df<-arrange(df,-abs(change))
head(df,15)

keepyears<-as.character(c(2012:2015,2018))
head(subset(df,change<0,select=c("acctname","subfunction",keepyears,"change")),10)

(df2<-ddply(df,.(fcode,fname),summarise,sum=sum(change,na.rm=TRUE)))
arrange(df2,-sum)

(df3<-ddply(df,.(fcode,fname),function(x) data.frame("2013"=sum(x$"2013",na.rm=TRUE),"2018"=sum(x$"2018",na.rm=TRUE),change=sum(x$change,na.rm=TRUE))))
df3$pch<-(df3$change/df3$X2013)*100
arrange(df3,-change)


head(grants)
(df2<-ddply(grants,ffyear~beacatg,summarise,sumgrants=sum(value,na.rm=TRUE)/1e6))
df2<-dcast(df,ffyear~beacatg,sum,na.rm=TRUE,margins="beacatg")



#****************************************************************************************************
#
#                Try to reproduce tables using federal budget database ####
#
#****************************************************************************************************
# http://www.fms.treas.gov/fastbook/index.html
# reason: the db has projections whereas many fb tables do not
# Table 17-1 in Analytical Perspectives (NOT the 17.1 from Historical Tables) - BA/Out by function, beacat, agency, program ####
# function is 1st 2 chars of subfuncode -- need to get function names
head(outl)
df<-outl
df$fcode<-substr(df$subfuncode,1,2)
tab17.1<-ddply(subset(df,grantnongrant=="Grant" & ffyear>=2012 & ffyear<=2014),.(ffyear,fcode,beacatg,agname,burname,acctname),summarise,sumgrants=sum(value,na.rm=TRUE)/1e6)
tab17.1<-subset(tab17.1,sumgrants!=0)
head(tab17.1)
head(dcast(tab17.1,fcode+beacatg+agname+burname+acctname~ffyear)) # this works for outlays
# reproduce grand totals for outlays
dcast(tab17.1,beacatg~ffyear,sum,na.rm=TRUE)

subset(df,grantnongrant=="Grant" & ffyear==2012 & substr(subfuncode,1,2)==27 & agcode==19 & burcode==20 & beacatg=="Discretionary" & grepl("efficiency",acctname,ignore.case=TRUE))

subset(authl,ffyear==2012 & substr(subfuncode,1,2)==27 & agcode==19 & burcode==20 & beacatg=="Discretionary" & grepl("efficiency",acctname,ignore.case=TRUE))
# it does not look like we can reproduce authorizations - look at:
# subset(authl,ffyear==2012 & substr(subfuncode,1,2)==27 & agcode==19 & burcode==20 & beacatg=="Discretionary" & grepl("efficiency",acctname,ignore.case=TRUE))
# we should find numbers that add to $128m in 2012 per tab 17.1 but we get much more

# Table 17-2 outlays
yrs<-c(seq(1960,2000,10),2005,2010:2014)
# Panel A
dcast(subset(df,ffyear %in% yrs & grantnongrant=="Grant"),fcode~ffyear,function(x) sum(x,na.rm=TRUE)/1e6,margins="fcode")
# Panel B -- wow, big differences in beacat details for 2010 and 2011 -- why? ####
dcast(subset(df,ffyear %in% yrs & grantnongrant=="Grant"),beacatg~ffyear,function(x) sum(x,na.rm=TRUE)/1e6,margins="beacatg")
# Panel C: payments for individuals, physical capital, other -- source?? check hist tabs 11.1, 11.2, 11.3

# Table 17-3 obligations by agency, bureau, program -- source??

# Table 17-4 obligations by state -- source?

# 17-5 to 17-40 obligations by state and program (one per table) -- source??

# hist tab 9.6 phys cap grants
df<-outl
df$fcode<-substr(df$subfuncode,1,2)
df$sf3<-substr(df$subfuncode,3,3)
df$value<-df$value/1e6
subset(df,grantnongrant=="Grant" & ffyear==2012 & fcode==40 & sf3==7 & value!=0,select=c(acctname,acctcode,value)) # transportation
#tab9.6<-ddply(subset(df,grantnongrant=="Grant" & ffyear>=2012 & ffyear<=2014 & subfuncode>=500 & subfuncode<550),.(ffyear,fcode,sf3,subfunction),summarise,sumgrants=sum(value,na.rm=TRUE)/1e6)
dcast(tab9.6,sf3+subfunction~ffyear,sum,na.rm=TRUE,margins="sf3")



# hist tab 9.9 education grants -- this comes VERY close ExCEPT:
# -- it always has about $5b higher in social services, and in 2014 train/emp is about $20b higher
df<-outl
df$fcode<-substr(df$subfuncode,1,2)
df$sf3<-substr(df$subfuncode,3,3)
tab9.9<-ddply(subset(df,grantnongrant=="Grant" & ffyear>=2012 & ffyear<=2014 & subfuncode>=500 & subfuncode<550),.(ffyear,fcode,sf3,subfunction),summarise,sumgrants=sum(value,na.rm=TRUE)/1e6)
dcast(tab9.9,sf3+subfunction~ffyear,sum,na.rm=TRUE,margins="sf3")

# why? break down the training and education component
don<-subset(df,grantnongrant=="Grant" & ffyear>=2012 & ffyear<=2014 & subfuncode>=500 & subfuncode<550 & sf3==4)
don$value<-don$value/1e6
dcast(don,sf3+acctcode+acctname~ffyear,sum,na.rm=TRUE,margins="subfuncode")
# because the American Jobs Act and Community College to Career Fund rises to $13.8b in 2014 0.000 0.825 13.750
# is it possible that outlays in the hist tables do not include proposals?
# per the intro to the tables, Table 9.9 also excludes education and training outlays for physical capital (which are
# included in Table 9.7) and education and training outlays for the conduct of research and development (which are in
# Table 9.8). Also excluded are education and training programs for Federal civilian and military personnel.




# Historical tables that include grants ####
# physical capital grants
df<-read.xls(paste0(histd,"hist09z2.xls")) # physical capital grants ONLY through 2014
head(df)
keepnames<-c("ffyear","pctotal","pcdirtotal","pcdirdefense","pcdirnondefense","pcgrants")
names(df)[1:length(keepnames)]<-keepnames
df[4:6,1:6]
df$ffyear<-as.numeric(gsub("[^0-9]","", df$ffyear)) # keep only numbers
df<-subset(df,!is.na(ffyear))
hist09z2<-melt(subset(df,select=keepnames),id="ffyear")
hist09z2$value<-ctov(hist09z2$value)
qplot(ffyear,value,data=subset(hist09z2,variable=="pcgrants"),geom=c("point","line"))
head(hist09z2); tail(hist09z2)

# breakdown of physical capital grants 9.6
df<-read.xls(paste0(histd,"hist09z6.xls")) # physical capital grants ONLY through 2014
head(df)
keepnames<-c("ffyear","pctotal","pcdirtotal","pcdirdefense","pcdirnondefense","pcgrants")
names(df)[1:length(keepnames)]<-keepnames
df[4:6,1:6]
df$ffyear<-as.numeric(gsub("[^0-9]","", df$ffyear)) # keep only numbers
df<-subset(df,!is.na(ffyear))
hist09z2<-melt(subset(df,select=keepnames),id="ffyear")
hist09z2$value<-ctov(hist09z2$value)

# 9.9 has a breakdown of education and training grants -- can be done with subfun code

# 11.1 payments for individuals - grants out to 2018!




df<-read.xls(paste0(histd,"hist11z1.xls")) # has payments for individuals
names(df)[1:4]<-c("ffyear","totpfi","dirpfi","grantpfi")
hist11z1<-melt(subset(df,select=c(1:4)),id="ffyear")




#****************************************************************************************************
#
#                Federal budget special tables ####
#
#****************************************************************************************************
# see: "http://www.whitehouse.gov/omb/budget/Analytical_Perspectives"
# note that tables 18_17-plus have the CFDA code (in parens of title) and the 

gtbl<-"18_1.xls"

download.file(paste0(fburl,gtbl),paste0(fbdir,gtbl),mode="wb")
uteb<-"http://www.whitehouse.gov/sites/default/files/omb/budget/fy2013/assets/teb2013.xls" # Tax expenditure budget

# get selected historical tables ###
df<-read.xls(paste0(histd,"hist11z1.xls")) # has payments for individuals
names(df)[1:4]<-c("ffyear","totpfi","dirpfi","grantpfi")
hist11z1<-melt(subset(df,select=c(1:4)),id="ffyear")
hist11z1$ffyear<-as.numeric(as.character(hist11z1$ffyear))
hist11z1$value<-ctov(hist11z1$value)
hist11z1<-subset(hist11z1,!is.na(value))
head(hist11z1)
str(hist11z1)
qplot(ffyear,value/1e3,data=hist11z1,colour=variable,geom=c("point","line"))








#--------------- work with already saved federal budget data
load(paste(rdat,"fedoutlays.RData",sep=""))
head(fedoutlays)
count(fedoutlays,"grantnongrant")
# get breakdwon of grants
grants<-subset(fedoutlays,grantnongrant=="Grant" & ffyear>=2002 & ffyear<=2017)
ddply(subset(grants, ffyear>=2008 & ffyear<=2014),ffyear~beacatg,summarise,sumgrants=sum(value,na.rm=TRUE))

# get mandatory, discretionary by year
dcast(ddply(grants,ffyear~beacatg,summarise,sumgrants=sum(value,na.rm=TRUE)),ffyear~beacatg)

# what are the biggest mandatory and discretionary grants?
d<-subset(grants,beacatg=="Discretionary" & ffyear==2012)
d<-d[order(-d$value),]
head(d,10)


d<-subset(grants,grepl("highway",acctname,ignore.case=TRUE))
subset(d,ffyear==2012)
ddply(subset(d,burcode %in% c(15,18)),ffyear~beacatg,summarise,sumgrants=sum(value,na.rm=TRUE))

don<-dcast(fedoutlays, ffyear~tolower(grantnongrant), value_var="value", sum, na.rm=TRUE)
don$budtot<-don$grant+don$nongrant
don$gshare<-don$grant/(don$grant+don$nongrant)*100
don<-subset(don,ffyear!=1976.75) # drop the transition quarter




#---------------- read in federal budget data ----------------------------------------
fname<-paste(fbdir,"outlays.xls",sep="")

fedoutlays<-read.xls(fname, header=FALSE, colClasses="character", skip=1, nrow=-1) #,pattern="East North")
fedoutlays[1:10,1:10]
firstnames<-c("agcode","agname","burcode","burname","acctcode","acctname","treasagcode","subfuncode","subfunction","beacatg","grantnongrant","onoffbud")
names(fedoutlays)[1:length(firstnames)]<-firstnames
names(fedoutlays)
head(fedoutlays)
fedoutlays<-melt(fedoutlays,id=firstnames)
fedoutlays$value<-ctov(fedoutlays$value)
fedoutlays<-subset(fedoutlays, !is.na(value))
fedoutlays$ffyear<-1949+ctov(substr(fedoutlays$variable,2,5))
fedoutlays$variable<-NULL # drop
fedoutlays$ffyear<-ifelse(fedoutlays$ffyear==1977,1976.75,fedoutlays$ffyear) # transition quarter
fedoutlays$ffyear<-ifelse(fedoutlays$ffyear>1977,fedoutlays$ffyear-1,fedoutlays$ffyear) # transition quarter
save(fedoutlays, file=paste(rdat,"fedoutlays.RData",sep=""))

load(paste(rdat,"fedoutlays.RData",sep=""))
table(fedoutlays$ffyear)
table(fedoutlays$grantnongrant)

head(fedoutlays); tail(fedoutlays)
subset(fedoutlays)



#****************************************************************************************************
#
#                USASpending.gov ####
#
#****************************************************************************************************
# revdate<-"20120814"
revdate<-"20130316"

dirbase<-paste0(datdir,"Federal budget and spending\\USASpending\\")
fngrants<-"_All_Grants_Full_"
usadir<-paste0(dirbase,revdate,"\\")

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


#****************************************************************************************************
#
#                Federal OPM data ####
#
#****************************************************************************************************
# http://www.opm.gov/data/Index.aspx?tag=FedScope

# From: Wiesman, David <David.Wiesman@opm.gov>
# Date: Mon, Apr 15, 2013 at 9:23 AM
# Subject: RE: Trying to match numbers in Common Characteristics of the Government 2011, by David Wiesman
# To: "donboyd5@gmail.com" <donboyd5@gmail.com>
#...Non-Seasonal Full-Time Permanent (NSFTP) is more complicated than what you are using.  The definition is 
#  WorkSchedule in ('F' ) and (Tenure in ('1', '2') or substr of (Type_of_appointment in,1,1)  in ('1','3','5'))
# If you are using fedscope you can filter on the permanent folder under Types of Appointment.  Under Work Schedule filter on F in the fulltime folder.

opmd<-"E:\\Data\\Federal budget and spending\\OfficePersonnelManagement\\FedScopeDec2012\\"
fn<-"FACTDATA_DEC2012.TXT"

opmd<-"E:\\Data\\Federal budget and spending\\OfficePersonnelManagement\\FedScopeDec2011\\"
fn<-"FACTDATA_DEC2011.TXT"

opmd<-"E:\\Data\\Federal budget and spending\\OfficePersonnelManagement\\FedScopeSep2011\\"
fn<-"FACTDATA_SEP2011.TXT"

# http://catalog.data.gov/dataset/fedscope-employment-cubes
#         http://www.opm.gov/data/Files/324/4e2d94f0-199b-4fa8-854d-4b0c7e8d14df.zip
opmfn<-"http://www.opm.gov/Data/Files/347/ab1d5fa8-a60b-44db-b1ca-21b4037f12a5.zip"
download.file(opmfn, paste0(bopd, "opm.zip"))

rarx<-"C:\\Progra~1\\WinRAR\\WinRAR e -o+ " # command fragment to call the WinRAR executable AND OVERWRITE
cmd<-paste(rarx,shQuote(paste0(bopd, "opm.zip")),shQuote(bopd)) 
system(cmd) # this calls WinRAR and unzips the file just downloaded


# REPORTED sep 2011 nsftp=1,856,580, avgsal=77,505, median=71,102
# CALC                    1,855,052, avgsal=77,506, median=71,102


# http://www.opm.gov/Data/Files/347/ab1d5fa8-a60b-44db-b1ca-21b4037f12a5.zip
fn<-"FACTDATA_MAR2014.TXT"
system.time(df<-read.csv(paste0(bopd, fn)))
ht(df)
str(df)
count(df, "DATECODE")
count(df,"EMPLOYMENT")
count(df,"GENDER")
summary(df)
don<-subset(df,SALARY>300e3)
don<-arrange(don,-SALARY)
ht(don)
str(df)
# get definition files
dloc<-read.csv(paste0(bopd,"DTloc.txt"))
dtoa<-read.csv(paste0(bopd,"DTtoa.txt"))
dwrksch<-read.csv(paste0(bopd,"DTwrksch.txt"))

# create new vars
df$LOCT<-factor(df$LOC,levels=dloc$LOC,labels=dloc$LOCT)
df$stabbr<-factor(df$LOC, levels=btools::stcodes$stfips, labels=btools::stcodes$stabbr)
df$TOATYP<-as.character(factor(df$TOA, levels=dtoa$TOA, labels=dtoa$TOATYP)) # 1=permanent
count(df,c("LOCT","stabbr"))

# define NSFTP nonseasonal full time permanent, in pay status
# ensftp<-expression(subset(df,WORKSCH %in% c("B","F") & TOATYP=="1" & SALLVL!="Z" & SALARY>0))
# ensftp<-expression(subset(df,WORKSCH %in% c("B","F") & TOATYP=="1" & SALARY>0))
# ensftp<-expression(subset(df,WORKSCH %in% c("B","F") & TOATYP=="1"))
# WorkSchedule in ('F' ) and (Tenure in ('1', '2') or substr of (Type_of_appointment in,1,1)  in ('1','3','5')) per David Wiesman

# use David Wiesman of OPM's rules - but I cannot find a tenure variable
eftns<-expression(WORKSCH %in% c("F"))  # full time nonseasonal work schedule
etoa<-expression(substr(TOA,1,1) %in% c("1","3","5"))  # type of appointment [permanent; competitive, excepted, or senior exec]
# tenure ???

nsftp <- df %>% mutate(SALARY=as.numeric(SALARY)) %>% 
  filter(WORKSCH %in% c("F") & substr(TOA,1,1) %in% c("1","3","5") & SALARY>0 & !is.na(SALARY)) %>%
  group_by(stabbr) %>% 
  summarise(salarym = sum(SALARY, na.rm=TRUE)/1e6, nemp = sum(SALARY>0, na.rm=TRUE)) %>% 
  mutate(avgsal = salarym*1e6/nemp, salpct=salarym/sum(salarym, na.rm=TRUE)*100) %>%
  arrange(-salarym)
nsftp











