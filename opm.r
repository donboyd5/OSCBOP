# opm.r
# Don Boyd
# 7/10/2014

bopd <- "E:\\Data\\Projects\\OSC BOP\\"


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

# Location: The official duty station of an employee. Locations in the United States are defined in terms of states. Locations outside the 
# United States are defined in terms of countries and U.S. territories.

# Employment: A measure representing the number of employees in pay status at the end of the quarter (or end of the pay period prior to
# the end of the quarter).


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
fn<-"FACTDATA_MAR2014.TXT"; dir<-bopd
fn<-"FACTDATA_DEC2012.TXT"; dir<-paste0(bopd, "OPM 2012-12\\")
system.time(df<-read.csv(paste0(dir, fn)))
ht(df)
str(df)
count(df, "DATECODE")
count(df,"EMPLOYMENT")
count(df,"GENDER")
summary(df)
# don<-subset(df,SALARY>300e3)
# don<-arrange(don,-SALARY)
# ht(don)
# str(df)

# get definition files
dloc<-read.csv(paste0(dir,"DTloc.txt"))
dtoa<-read.csv(paste0(dir,"DTtoa.txt"))
dwrksch<-read.csv(paste0(dir,"DTwrksch.txt"))
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
  filter(as.character(stabbr)!="NA") %>%
  mutate(avgsal = salarym*1e6/nemp, salpct=salarym/sum(salarym, na.rm=TRUE)*100) %>%
  arrange(-salarym)
nsftp

sum(nsftp$SALARY, na.rm=TRUE)
