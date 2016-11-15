###################################################################################################
# PVM COMPARISON OF VOORSCHOTEN AND VALKENBRUG USING PARALLEL MEASUREMENTS
###################################################################################################

mytheme <- theme_grey() +
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.1)),
        legend.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.1)) 
  ) 


# (1) import and prepare the data 
# (2) make monthly mean plots for relevant variables of both stations
# (3) monthly mean differences for all relevant variables 
# (4) Theo wants a different comment

###################################################################################################
# (1) Import the hourly and daily data and prepare data
x <- read.table(file=paste("/usr/people/brandsma/Mschijflocal/Documenten",
                           "/PROJECTEN_Actueel/ValkenburgVoorschoten",
                           "/Data/ValkVoor_daily.txt", sep=""),header=TRUE,skip=49,sep=",")

#set -1 values for precipitation to 0
x$RH[x$RH==-1] <- 0


y <- read.table(file=paste("/usr/people/brandsma/Mschijflocal/Documenten",
                           "/PROJECTEN_Actueel/ValkenburgVoorschoten",
                           "/Data/ValkVoor_hourly.txt", sep=""),header=TRUE,skip=33,sep=",")

# add year, month, day and the rounding of the ECV's
x <- x %>% 
  separate(YYYYMMDD, into=c('year','month','day'), sep = c(4,6), remove = FALSE,convert = TRUE) %>%
  mutate(TG=TG/10,TN=TN/10,TX=TX/10,PG=PG/10, FG=FG/10, FXX=FXX/10, Q=Q*10000/86400, RH=RH/10) %>%
  group_by(STN,year,month) %>%
  summarize(TN=mean(TN),
            TX=mean(TX),
            TG=mean(TG),
            FG=mean(FG),
            PG=mean(PG),
            FXX=mean(FXX),
            RH=sum(RH),
            Q=mean(Q)
  )



x1 <-  x %>%
  filter(RH == -1,year > 1979) %>%
  select(year,RH) %>%
  group_by(year) %>%
  summarize(numDRminone=n())


#add year month day


Annualmissing <- x %>%
  group_by(STN,year) %>%
  summarise(MissQ=sum(is.na(Q)),
            MissT=sum(is.na(Temp)),
            MissN=sum(is.na(N)))

Annualmeans <- x %>%
  group_by(STN,year) %>%
  summarise(Q=mean(Q,na.rm=T),
            Temp=mean(Temp,na.rm=T),
            N=mean(N,na.rm=T))

# consider only the data from 1965 onwards
data <- Annualmeans %>%
  filter(year >= 1965)


#calculate also the summer half year means (Summermeans)
Summermeans <- x %>%
  filter(month >= 4 & month <= 9) %>%
  group_by(STN,year) %>%
  summarise(Q=mean(Q,na.rm=T),
            Temp=mean(Temp,na.rm=T),
            N=mean(N,na.rm=T))

# consider only the data from 1965 onwards
data2 <- Summermeans %>%
  filter(year >= 1965)
