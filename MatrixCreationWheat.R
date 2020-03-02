## Matrix creation ##
## Creates trade, production, and reserves matrices for use in cascade model ##


## Requires existing files in working directory: 
## croplist (with kcal conversions), 
## countrylist (with FAO country code, and true/false for country pop over 500k), 
## Production data from FAOSTAT, production quantity in tonnes 
## Trade data from FAOSTAT, detailed trail matrix, normalized, all data 
## Reserves data from USDA, downloadable dataset - psd grains pulses 

install.packages("tidyr")
install.packages("dplyr")
install.packages("reshape2")
install.packages("igraph")
install.packages("stringr")

library(tidyr)
library(dplyr)
library(reshape2)
library(igraph)
library(stringr)

setwd("~/NuclearWinter") # USERS: edit this home directory


#load crop list, restrict to cereal crops (group_id=1)
crop_list <- read.delim("Ancillary/crop_list.tsv", header = TRUE, as.is = TRUE) %>%
  filter(Groupnum == 1) # restrict to Cereals, group==1
#prod_crops <- filter(crop_list, ProductionL == 1, cropid == 56) %>%
#select(cropid, cropname, kcal.ton) #restrict to 'primary' crops, ProductionL==1
#trade_crops <- filter(crop_list, TradeL == 1, cropid == 56) %>% #restrict to Maize
#select(cropid, kcal.ton)
yr_range <- 2006:2008
ciso <- read.csv("Ancillary/ciso.csv", stringsAsFactors = FALSE)
ciso$X<-NULL

Comlist<-read.csv("Ancillary/CropCommodities.csv")

###########################################################################
### PART 1: Trade (Detailed trade matrix pre-downloaded trade from FAOSTAT)
## download detailed trade matrix from FAO 
###########################################################################

trade_dat <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
                      stringsAsFactors = FALSE)

trade_dat <- select(trade_dat, reporter = Reporter.Country.Code, 
                    partner = Partner.Country.Code, cropid = Item.Code, 
                    element = Element, year = Year, value = Value) %>%
  filter(year %in% yr_range)

#filter for wheat and wheat products 
trade_dat<-trade_dat[(trade_dat$cropid==15 | 
                        trade_dat$cropid==16 |
                        trade_dat$cropid==18 |
                        trade_dat$cropid==19|
                        trade_dat$cropid==20 |
                        trade_dat$cropid==21|
                        trade_dat$cropid==22),]



# Put parts of China under same ID
china_ids <- c(41, 96, 128, 214)
trade_dat$reporter[trade_dat$reporter %in% china_ids] <- 351
trade_dat$partner[trade_dat$partner %in% china_ids] <- 351
rm(china_ids)

# Keep countries in ciso list and remove trade between a country and itself
trade_dat <- filter(trade_dat, reporter %in% ciso$FAO,
                    partner %in% ciso$FAO) %>%
  filter(reporter != partner)

# Extract exports
exp_dat <- filter(trade_dat, element == "Export Quantity") %>% select(-element)

# set India and Pakistan to zero 

exp_dat$value[exp_dat$reporter==100]<-0
length(which(exp_dat$reporter==100))
exp_dat$value[exp_dat$partner==100]<-0
length(which(exp_dat$reporter==100))
#india has 804 trade links deleted

exp_dat$value[exp_dat$reporter==165]<-0
length(which(exp_dat$reporter==165))
exp_dat$value[exp_dat$partner==165]<-0
length(which(exp_dat$reporter==165))
#pakistan has 362 trade links deleted 

# Convert to kcal and aggregate across crops
exp_agg <- inner_join(exp_dat, Comlist) %>%
  mutate(value_kcal = value * as.numeric(kcal.ton)) %>%
  group_by(reporter, partner, year) %>%
  summarise(ekcal = sum(value_kcal, na.rm = TRUE))

# Convert export datafarme to a country x country x year matrix
exp_mat <- acast(exp_agg, reporter ~ partner ~ year)
exp_mat <- exp_mat[match(ciso$FAO, dimnames(exp_mat)[[1]]), , ]
exp_mat <- exp_mat[, match(ciso$FAO, dimnames(exp_mat)[[2]]), ]
exp_mat[is.na(exp_mat)] <- 0

dimnames(exp_mat) <- list(ciso$FAO, ciso$FAO, yr_range)
# set to 0 countries where max population < 500k 
exp_mat[!ciso$gt.500k, , ] <- 0
exp_mat[, !ciso$gt.500k, ] <- 0

E0 <- apply(exp_mat[, , 1:3], 1:2, mean) 


# Remove countries with no trade data
no_dat <- which(rowSums(E0) == 0 & colSums(E0) == 0)
E0 <- E0[-no_dat, -no_dat]

FAO163<-data_frame(FAO=dimnames(E0)[[1]])
FAO163<-merge(FAO163, ciso,by.x='FAO',by.y='FAO', all.x=TRUE)
FAO163<-FAO163[,(1:3)]
FAO163$FAO<-as.numeric(FAO163$FAO)
FAO163<-FAO163[order(FAO163$FAO),]
write.csv(FAO163, "inputs/Countries163.csv", row.names = FALSE)

dimnames(E0) <- list(FAO163$iso3, FAO163$iso3) 

save(E0, file="inputs/WheatAllCommodities2007E0.RData")

##to export matrices with country names
Tkbyc<-as.data.frame(E0, row.names = dimnames(E0)[[1]], col.names = dimnames(E0)[[2]] )
write.csv(Tkbyc, "WheatTrade2007.csv")

###########################################################################
## PART 2: Get production data from FAOSTAT
## QC: production-crops domain, 5510: production in tonnes
###########################################################################

# download production quantity from FAOSTAT, all countries, all crops, import  
prod_dat<-read.csv("Inputs/Production_All.csv")

# rename to match functions code, keep only relevant columns
prod_dat$FAO<-prod_dat$Area.Code
prod_dat <- prod_dat[, c("FAO", "Year", "Value", "Item.Code", "Item")]
prod_dat<-prod_dat %>%
  filter(   Item.Code==15)
prod_dat<-prod_dat  %>%
  filter(Year %in% yr_range)

# Add kcal conversion factor, then sum calories by country and year 
prod_agg <- inner_join(prod_dat, Comlist, by = c("Item.Code" = "cropid")) %>%
  mutate(value_kcal = Value * as.numeric(kcal.ton)) %>%
  group_by(FAO, Year) %>%
  summarise(pkcal = sum(value_kcal, na.rm = TRUE))

# Convert to country x year matrix
prod_mat <- spread(prod_agg, key = Year, value = pkcal, fill = 0)
prod_mat <- prod_mat[match(ciso$FAO, prod_mat$FAO), ]
prod_mat <- as.matrix(prod_mat[, -1]) #create matrix, remove country code column
dimnames(prod_mat) <- list(ciso$FAO, yr_range) # put country names and years as matrix dimensions
prod_mat[is.na(prod_mat)] <- 0 #replace NAs with 0
P0 <- rowMeans(prod_mat[, 1:3])
#remove countries without any trade data
P0 <- P0[-no_dat]
names(P0)<-FAO163$iso3


save(P0, E0, file="inputs/Wheat2007P0E0.RData")
save(P0, file= "inputs/Wheat2007P0.Rdata")

##to export matrix with country names
Pkbyc<-data_frame(iso3=names(P0), P0=P0)
write.csv(Pkbyc,"WheatProduction2007.csv", row.names=FALSE)


###########################################################################
##Part 3: Stocks of cereals in countries x years matrix 
### download USDA-PSD data pulses grains
###########################################################################

## (1) Load data and get reserves in kcal
psd <- read.csv("inputs/psd_grains_pulses.csv")

# Extract year-end stocks 2006-2008 (units: 1000 metric tons)
psd0608 <- select(psd, Country = Country_Name, Year = Market_Year,
                  Product = Commodity_Description,
                  Variable = Attribute_Description, Value) %>%
  filter(Year %in% 2006:2008) %>%
  spread(key = Variable, value = Value) %>%
  select(Country, Year, Product, R = `Ending Stocks`)

#keep only Wheat
psd0608<-psd0608[(psd0608$Product=="Wheat"),]

# Read table of kcal/tonnes conversion factors and match commodity names
conv_kcal <- Comlist
#subs_list <- c("Rice Milled" = "Rice, Milled", "Maize" = "Corn", 
#               "Mixed grain" = "Mixed Grain")
#conv_kcal$cropname <- str_replace_all(conv_kcal$cropname, subs_list)
conv_kcal<-conv_kcal[(conv_kcal$cropname=="Wheat"),]

# Calculate R in kcal, reshape to Country x Year
Rkbyc <- inner_join(psd0608, conv_kcal, by = c("Product" = "cropname")) %>%
  select(Country, Year, Product, R, convf = kcal.ton) %>%
  mutate(R = R * 1000 * as.numeric(convf)) %>%
  group_by(Country, Year) %>%
  summarise(R = sum(R, na.rm = TRUE)) %>%
  spread(key = Year, value = R, fill = 0)

## (2) Various fixes to ensure countries match between FAOSTAT and PSD data

# Fix non-matching country names
countries_to_match <- unique(Rkbyc$Country[!(Rkbyc$Country %in% ciso$name)])
countries_to_match
country_repl <- c("Bolivia" = "Bolivia (Plurinational State of)", "Brunei" = 
                    "Brunei Darussalam", "Burkina" =  "Burkina Faso", "Burma" = 
                    "Myanmar", "Former Czechoslovakia" = "Czechoslovakia", 
                  "Former Yugoslavia" = "Yugoslav SFR", "Gambia, The" = "Gambia",
                  "Iran" = "Iran (Islamic Republic of)", "Korea, North" = 
                    "Democratic People's Republic of Korea", "Korea, South" = 
                    "Republic of Korea", "Laos" = "Lao People's Democratic Republic",
                  "Macedonia" = "The former Yugoslav Republic of Macedonia",
                  "Moldova" = "Republic of Moldova", "Russia" = 
                    "Russian Federation", "Syria" = "Syrian Arab Republic",
                  "Tanzania" = "United Republic of Tanzania", 
                  "Union of Soviet Socialist Repu" = "USSR", "United States" =
                    "United States of America", "Venezuela" = 
                    "Venezuela (Bolivarian Republic of)", "Vietnam" = "Viet Nam")
Rkbyc$Country <- str_replace_all(Rkbyc$Country, country_repl)
Rkbyc$Country <- str_replace(Rkbyc$Country, fixed("Congo (Brazzaville)"), "Congo") %>%
  str_replace(fixed("Congo (Kinshasa)"), "Democratic Republic of the Congo") %>%
  str_replace(fixed("Yemen (Aden)"), "Yemen Dem") %>%
  str_replace(fixed("Yemen (Sanaa)"), "Yemen Ar Rp")

cnames <- Rkbyc$Country
Rkbyc <- as.matrix(Rkbyc[, -1])
rownames(Rkbyc) <- cnames

# Add Taiwan and HK to China, South Sudan to Sudan, and two Yemens
Rkbyc["China", ] <- Rkbyc["China", ] + Rkbyc["Hong Kong", ] + Rkbyc["Taiwan", ]
Rkbyc <- Rkbyc[!(rownames(Rkbyc) %in% c("Hong Kong", "Taiwan", "South Sudan", 
                                        "Yemen Ar Rp", "Yemen Dem")), ]

## Apportion European Union reserves proportionally to production
# Use production list to scale reserves for EU

rownames(prod_mat) <- ciso$name
eu_list <- c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany",
             "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", 
             "Portugal", "Spain", "Sweden", "United Kingdom", 
             "Belgium-Luxembourg","Bulgaria", "Croatia", "Cyprus", "Czech Republic",
             "Estonia", "Hungary", "Latvia", "Lithuania", "Malta", "Poland", 
             "Romania", "Slovakia", "Slovenia")
eu_yrs <- which(as.numeric(colnames(Rkbyc)) >= 1998)

prop_eu <- scale(prod_mat[eu_list, 1:3], center = FALSE,
                 scale = colSums(prod_mat[eu_list, 1:3]))

# Add individual EU countries to matrix
Rkbyc <- rbind(Rkbyc, matrix(0, nrow = length(eu_list), ncol = ncol(Rkbyc), 
                             dimnames = list(eu_list)))
Rkbyc[eu_list, 1:3] <- sweep(prop_eu, 2, Rkbyc["European Union", 1:3], "*") 
Rkbyc <- Rkbyc[-which(rownames(Rkbyc) %in% c("European Union")), ] #delete EU row


## (4) Create new matrix with all countries in Pkbyc (full country list),
# fill reserves by matching names, leave the rest at zero
Rkbyc_all<-Rkbyc
Rkbyc_all <- Rkbyc_all[match(ciso$name, dimnames(Rkbyc_all)[[1]]),]
Rkbyc_all[is.na(Rkbyc_all)] <- 0
dimnames(Rkbyc_all) <- list(ciso$iso3, yr_range)

#take average of 2006-2008 
R0 <- rowMeans(Rkbyc_all[, 1:3])
R0 <- R0[-no_dat]
R0

save(R0, file = "inputs/Wheat2007R0.RData")


Rkbyc<-data_frame(iso3=names(R0), R0=R0)
write.csv(Rkbyc,"WheatReserves2007.csv", row.names=FALSE)

