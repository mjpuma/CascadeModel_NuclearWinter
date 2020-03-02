

### Uses Cascade Functions to calculate network statistics and run various shocks. 

# Must have run all functions in "CascadeFunctions.R" 
# Must have the following in working directory:
#   prod/trade file
#   stocks file 
#   country list - "ciso3.txt" 
#   anomalies

setwd("~/NuclearWinter") # USERS: edit this home directory

library(dplyr)
library(igraph)

iso3 <- read.table("ciso3.txt", stringsAsFactors = FALSE) #load country list 
cnames <- iso3[, 2] #select 3 digit ISO character codes 

## Load production, trade, stocks data

years <- c(2005)
trade_dat <- lapply(years, get_trade_data, mov_avg = 2, #load 2005 data with moving average of 2 years on either side
                    prod_trade_file = "Wheat_prod_trade.RData", 
                    stocks_file = "Wheat_stocks.RData")
names(trade_dat) <- years
trade_dat$`2005`$E0

#Summary statistics of the trade data for time period considered, 
# $N_c$ = number of countries in network,", $P$ = production, $R$ = reserves, $F$ = trade volume)"))
#"med. year", "$N_c$", "$\\sum P$ (kcal)", "$\\sum R$ (kcal)", "# trade links", "$\\sum F$ (kcal)",  
# "$\\sum R / \\sum P$", "$\\sum F / \\sum P$"
trade_st <- bind_rows(lapply(trade_dat, get_trade_stats_sum), .id = "year")
trade_st

P0<-trade_dat$'2005'$P0
R0<-trade_dat$'2005'$R0
E0<-trade_dat$'2005'$E0


#         c_init = name of the initial country that is impacted
#         a_init = fraction of production lost initially by country c_init
#             P0 = initial production vector
#             R0 = initial reserve vector; note: 0.5*R0 effectively means 50% of reserves can be tapped to absorb shock
#             E0 = initial trade matrix
#OPTIONAL: cfrac = fraction of shock absorbed by C before changing trade (i.e. how much consumption would be reduced)
#           asym = TRUE => shocked countries can't increase their exports
#           kmax = max. number of iterations
#           amin = any shock below this value (as fraction of net supply) is negligible


# wheat anomaly data

# Initalize production shock vector, with percent changes by country
Anomalies<-read.csv("wheat_production_change_country_yr1-15_multi_model_mean.csv")
Anomalies$year_1<-Anomalies$year_1/-100 #change percent to prop, decreases=pos, inc=neg
Anomalies$X<-as.character(Anomalies$X)
#remove blank line 
Anomalies<-Anomalies[!(Anomalies$X=="NULL"),]
#remove HKG & MAC to match matrices which have them under China, and remove ARB (all are NA anyway)
Anomalies<-Anomalies[!(Anomalies$X=="MAC"),]
Anomalies<-Anomalies[!(Anomalies$X=="HKG"),]
Anomalies<-Anomalies[!(Anomalies$X=="ARB"),]
Anomalies[is.na(Anomalies)]<-0

#turn data frame into a named list
shockcountries<-(Anomalies$X)
shockintensities<-(Anomalies$year_1)
names(shockintensities)<-shockcountries
scenarios<-list(shockintensities)

#determine which countries in anomalies don't match the matrices list & remove from anomalies
mismatch<-names(shockintensities[!(names(shockintensities)%in%names(trade_dat$`2005`$P0))])
mismatch
shockintensities<-shockintensities[!(names(shockintensities)%in%mismatch)]
Year1<-list(shockintensities)
Year1

#which countries are in matrix but not anomalies
#mismatch2<- names(trade_dat$`2005`$P0[!(names(trade_dat$`2005`$P0)%in%names(shockintensities))])
#mismatch2

###Export dataframes for Michael
#ShockIntensitiesdf<-data.frame(country=names(Year1[[1]]),Year1=Year1[[1]])
#sum(is.na(ShockIntensitiesdf$Year1))
#write.csv(ShockIntensitiesdf,"WheatShock148.csv")

#ShockSize<-data.frame(country=names(trade_dat$`2005`$P0))
#ShockSize<-merge(ShockSize,ShockIntensitiesdf, all.x = TRUE)
#write.csv(ShockSize, "WheatShock165.csv")

#WheatProduction<-data.frame(country=names(trade_dat$`2005`$P0),P0=(trade_dat$`2005`$P0))
#write.csv(WheatProduction,"WheatProductionMatrix.csv")

#WheatReserves<-data.frame(country=names(trade_dat$`2005`$R0),R0=(trade_dat$`2005`$R0))
#write.csv(WheatReserves, "WheatReservesMatrix.csv")

## check with just US decline 

USAShock<-Year1df
USAShock$dP<-0
USAShock$dP[USAShock$country=='USA']<-.50

USAShockList<-(USAShock$dP)
ListNames<-USAShock$country
names(USAShockList)<-ListNames
USAShockList<-list(USAShockList)
USAShockList

USATestM<- sim_mc_multi(USAShockList, P0, R0, E0, cfrac = 0.1,
                        asym = T, kmax = 1000, amin = 1E-5)
USA1c<-sim_1c('USA',.50, P0, R0, E0, cfrac = 0.1,
              asym = T, kmax = 1000, amin = 1E-5)

identical(USATestM$P0,USA1c$P0)
identical(USATestM$dR[,1], USA1c$dR)
identical(USATestM$dC[,1],USA1c$dC) #works 
USATestM$dR

##Test decline and increase

twowayshock<-Year1df
twowayshock$dP<-0
twowayshock$dP[twowayshock$country=='USA']<-.25
twowayshock$dP[twowayshock$country=='CHN']<-.25
twowayshock$dP[twowayshock$country=='IND']<-.25
twowayshock$dP[twowayshock$country=='PAK']<-.25


twowayshock$dP[twowayshock$country=='ARG']<-(-.25)
twowayshock$dP[twowayshock$country=='BRA']<-(-.25)


twowayshocklist<-(twowayshock$dP)
ListNames<-twowayshock$country
names(twowayshocklist)<-ListNames
twowayshocklist<-list(twowayshocklist)
twowayshocklist

twowaytest<- sim_mc_multi(twowayshocklist, P0, R0, E0, cfrac = 0.1,
                          asym = T, kmax = 1000, amin = 1E-5)

twowaytest$dC

##test US and China

USCHShock<-Year1df
USCHShock$dP<-0
USCHShock$dP[USCHShock$country=='USA']<-.25
USCHShock$dP[USCHShock$country=='CHN']<-.25

USCHShockList<-(USCHShock$dP)
ListNames<-USCHShock$country
names(USCHShockList)<-ListNames
USCHShockList<-list(USCHShockList)
USCHShockList

USCHTestM<- sim_mc_multi(USCHShockList, P0, R0, E0, cfrac = 0.1,
                         asym = T, kmax = 1000, amin = 1E-5)
USCHTestM$dR
