### Uses Cascade Functions to calculate network statistics and run various shocks.
### 5-year simulation, explicitly calling cacade functions for each year (i.e. not in loop)

# Must have run all functions in "CascadeFunctions.R" 
# Must have the following in working directory:
#   prod/trade file
#   stocks file 
#   country list 
#   anomalies

library(dplyr)
library(igraph)

setwd("~/NuclearWinter") # USERS: edit this home directory

Countries162<-read.csv("Countries162.csv")
Countries162<-Countries162[order(Countries162$FAO),]
Countries162$iso3<-as.character(Countries162$iso3)

## Load production, trade, stocks data

load("Maize2007P0.RData")
load("Maize2007R0.RData")
load("MaizeAllCommodities2007E0.RData")


# Anomaly data
Anomalies<-read.csv("maize_production_change_country_yr1-15_multi_model_mean_2019-08-14.csv")
Anomalies$Y1<-Anomalies$year_1/100
Anomalies$Y2<-Anomalies$year_2/100
Anomalies$Y3<-Anomalies$year_3/100
Anomalies$Y4<-Anomalies$year_4/100
Anomalies$Y5<-Anomalies$year_5/100

Shocks<-merge(Countries162,Anomalies[,c(1,17:21)], by.x='iso3', by.y = 'X', all.x=TRUE, all.y=FALSE)
Shocks[is.na(Shocks)]<-0

#pull out positive production anomalies
Shocks$Y1Inc<- ifelse(Shocks$Y1>0, Shocks$Y1, 0)
Shocks$Y2Inc<- ifelse(Shocks$Y2>0, Shocks$Y2, 0)
Shocks$Y3Inc<- ifelse(Shocks$Y3>0, Shocks$Y3, 0)
Shocks$Y4Inc<- ifelse(Shocks$Y4>0, Shocks$Y4, 0)
Shocks$Y5Inc<- ifelse(Shocks$Y5>0, Shocks$Y5, 0)

##remove all positive values from anomalies, make decreases positive
Shocks$Y1Dec<-ifelse(Shocks$Y1>0, 0, (Shocks$Y1*-1))
Shocks$Y2Dec<-ifelse(Shocks$Y2>0, 0, (Shocks$Y2*-1))
Shocks$Y3Dec<-ifelse(Shocks$Y3>0, 0, (Shocks$Y3*-1))
Shocks$Y4Dec<-ifelse(Shocks$Y4>0, 0, (Shocks$Y4*-1))
Shocks$Y5Dec<-ifelse(Shocks$Y5>0, 0, (Shocks$Y5*-1))

P<-data.frame(P0=P0, iso3=names(P0))
Shocks<-merge(Shocks, P, by="iso3")
Shocks$dPY1<-(-Shocks$Y1Dec)*(Shocks$P0)
Shocks$dPY2<-(-Shocks$Y2Dec)*(Shocks$P0)
Shocks$dPY3<-(-Shocks$Y3Dec)*(Shocks$P0)
Shocks$dPY4<-(-Shocks$Y4Dec)*(Shocks$P0)
Shocks$dPY5<-(-Shocks$Y4Dec)*(Shocks$P0)
Shocks<-Shocks[order(Shocks$FAO),]


########YEAR1################

## Add positive Y1 anomalies to reserves
Reserves<-data_frame(iso3=names(R0), R0=R0)
Reserves<-merge(Shocks, Reserves, by='iso3', all.x=TRUE, all.y=FALSE)
Reserves<-Reserves[order(Reserves$FAO),]
Reserves$R1<-(Reserves$P0*Reserves$Y1Inc)+Reserves$R0
identical(Reserves$R0[Reserves$Y1Inc==0], Reserves$R1[Reserves$Y1Inc==0]) ## R0=R1 where no production increases occurred 
R1<-(Reserves$R1)
names(R1)<-Reserves$iso3


####Run Original Cascade model 
shockintensitiesY1<-Shocks$Y1Dec
names(shockintensitiesY1)<-Shocks$iso3
Y1Dec<-list(shockintensitiesY1)

MaizeResultsY1 <- sim_mc_multi(Y1Dec, P0, R1, E0, cfrac = 0.1,
                               asym = T, kmax = 1000, amin = 1E-5)


########YEAR2################

#update reserve levels based on year 1 results
#add positive Y2 shocks to R
R2<-((MaizeResultsY1$R0+MaizeResultsY1$dR)+(MaizeResultsY1$P0*Reserves$Y2Inc))
R2<-R2[,1]


####Run Cascade model 

shockintensitiesY2<-Shocks$Y2Dec
names(shockintensitiesY2)<-Shocks$iso3
Y2Dec<-list(shockintensitiesY2)

MaizeResultsY2 <- sim_mc_multi(Y2Dec, P0, R2, E0, cfrac = 0.1,
                               asym = T, kmax = 1000, amin = 1E-5)


########YEAR3################

#update reserve levels based on year 2 results 
#add positive Y3 shocks to R

R3<-((MaizeResultsY2$R0 + MaizeResultsY2$dR)+(MaizeResultsY2$P0*Reserves$Y3Inc))
R3<-R3[,1]


####Run Original Cascade model 
shockintensitiesY3<-Shocks$Y3Dec
names(shockintensitiesY3)<-Shocks$iso3
Y3Dec<-list(shockintensitiesY3)

MaizeResultsY3 <- sim_mc_multi(Y3Dec, P0, R3, E0, cfrac = 0.1,
                               asym = T, kmax = 1000, amin = 1E-5)

########YEAR4################

#update reserve levels based on year 3 results 
#add positive Y4 shocks to R

R4<-((MaizeResultsY3$R0 + MaizeResultsY3$dR)+(MaizeResultsY3$P0*Reserves$Y4Inc))
R4<-R4[,1]


####Run Original Cascade model 
shockintensitiesY4<-Shocks$Y4Dec
names(shockintensitiesY4)<-Shocks$iso3
Y4Dec<-list(shockintensitiesY4)

MaizeResultsY4 <- sim_mc_multi(Y4Dec, P0, R4, E0, cfrac = 0.1,
                               asym = T, kmax = 1000, amin = 1E-5)

########YEAR5################

#update reserve levels based on year 4 results
#add positive Y5 shocks to R

R5<-((MaizeResultsY4$R0 + MaizeResultsY4$dR)+(MaizeResultsY4$P0*Reserves$Y5Inc))
R5<-R5[,1]


####Run Original Cascade model 
shockintensitiesY5<-Shocks$Y5Dec
names(shockintensitiesY5)<-Shocks$iso3
Y5Dec<-list(shockintensitiesY5)

MaizeResultsY5 <- sim_mc_multi(Y5Dec, P0, R5, E0, cfrac = 0.1,
                               asym = T, kmax = 1000, amin = 1E-5)

### Results
Results<-data.frame(iso3=names(MaizeResultsY1$P0),
                    R1 = R1, 
                    dRY1v1 = MaizeResultsY1$dR, 
                    R2=R2,
                    dRY2v1 = MaizeResultsY2$dR, 
                    R3=R3,
                    dRY3v1 = MaizeResultsY3$dR, 
                    R4=R4,
                    dRY4v1 = MaizeResultsY4$dR, 
                    R5=R5,
                    dRY5v1 = MaizeResultsY5$dR)

write.csv(Results, "Maize_5yr_NuclearWinter.csv")
