rm(list=ls())
options(scipen=999)
gc()
setwd("~/Desktop/COVID Forecast Meta Analysis/")
# # # Load packages
library(readr)
library(plyr)
library(dplyr)
library(data.table)
library(rstan)
library(stringr) 
library(readxl)
library(anytime)
library(lubridate)
library(date)
library(zoo)
# Sample function is useful but buggy - if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}



##### ##### ##### ##### STATE VARYING ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  

##### POP --> link to website source: https://www.census.gov/geographies/reference-files/2010/geo/state-area.html
census.pop = fread(file = 'Auxiliary Data/CENSUS_pop.csv',skip = 3,header = T)[1:58,][,c("V1","2019")]
colnames(census.pop)[which(colnames(census.pop)=="V1")]="state_name"
census.pop = census.pop[which(substr(census.pop$state_name,1,1)=="."),]
census.pop$state_name = gsub("\\.","",census.pop$state_name)
colnames(census.pop)[which(colnames(census.pop)=="2019")]="pop.size"
census.pop$pop.size = as.numeric(as.character(unlist(gsub("\\.","",census.pop$pop.size))))
##### STATE AREA --> link to website source: https://www.census.gov/geographies/reference-files/2010/geo/state-area.html
census.area = fread(file = 'Auxiliary Data/CENSUS_landarea.csv')
census.area = census.area[,c(which(colnames(census.area)=="State and other areas2"),
                             which(colnames(census.area)=="Land Area1")+1),with=FALSE] # squared kms is 1 to the right of squared miles
census.area = census.area[census.area$`State and other areas2` %in% census.pop$state_name]
colnames(census.area)[which(colnames(census.area)=="State and other areas2")] = "state_name"
colnames(census.area)[which(colnames(census.area)=="V5")] = "land_area_km2"
census.area$land_area_km2 = as.numeric(as.character(unlist(gsub(",","",census.area$land_area_km2))))
dt.area = merge(census.pop,census.area,by = "state_name",all=T)
dt.area$pop.density = dt.area$pop.size/dt.area$land_area_km2
##### DEMOGRAPHICS (state-varying) --> link to website source: https://www.dailykos.com/stories/2018/2/21/1742660/-The-ultimate-Daily-Kos-Elections-guide-to-all-of-our-data-sets
demo = fread(file = 'Auxiliary Data/DailyKos ACS Summaries/Daily Kos Elections State Similarity Index - Similarity.csv')
demo = demo[,-grep("Similarity",colnames(demo)),with=F]
colnames(demo) = make.names(colnames(demo))
demo[,grep("dollars",colnames(demo))] = as.data.table(apply(demo[,grep("dollars",colnames(demo)),with=FALSE] ,2,
                                              function(x){as.numeric(as.character(unlist(gsub(",","",gsub("\\$","",x)))))}))
colnames(demo)[which(colnames(demo)=="Geography")] = "state_name"
demo = demo[demo$state_name %in% dt.area$state_name]
dt.area = merge(dt.area,demo,by = "state_name",all=TRUE)
##### HEALTH DATA (state-varying) --> link to website source: https://www.kff.org/state-category/health-status/
files = list.files(path = 'Auxiliary Data/Kaiser Foundation Health Data/')

temp.asthma = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/Asthma.csv',skip=2)
colnames(temp.asthma) = c("state_name","pct_asthma","pct_asthma_male","pct_asthma_female",'footnotes')
temp.asthma = temp.asthma[which(temp.asthma$state_name %in% dt.area$state_name),!"footnotes"]
temp.asthma = temp.asthma [,c("state_name","pct_asthma")]
temp.asthma$pct_asthma = as.numeric(temp.asthma$pct_asthma)

temp.cancer = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/CancerRate.csv')
colnames(temp.cancer) = c("state_name","cancer.incidence.per.100k")

temp.diabetes = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/Diabetes.csv',skip = 2,header = T)
colnames(temp.diabetes) = c("state_name","diabetes","pregnancy.diabetes","no.diabetes","pre.diabetes.or.borderline","footnotes")
temp.diabetes = temp.diabetes[which(temp.diabetes$state_name %in% dt.area$state_name),!"footnotes"]
temp.diabetes = temp.diabetes[,c("state_name","diabetes")]
temp.diabetes$diabetes = as.numeric(temp.diabetes$diabetes)

temp.obesity = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/Obesity.csv',skip = 3,header = F)
colnames(temp.obesity) = c("state_name","obesity","obese.males","obese.females")
temp.obesity = temp.obesity[which(temp.obesity$state_name %in% dt.area$state_name),!"footnotes"]
temp.obesity = temp.obesity[,c("state_name","obesity")]
temp.obesity$obesity = as.numeric(temp.obesity$obesity)

temp.hospital.beds = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/HospitalBed1000.csv',skip = 2,header = T)
colnames(temp.hospital.beds) = c("state_name","gov.beds","non-profit.beds","profit.beds","total.beds")
temp.hospital.beds = temp.hospital.beds[which(temp.hospital.beds$state_name %in% dt.area$state_name),!"footnotes"]
temp.hospital.beds = temp.hospital.beds[,c("state_name","total.beds")]
temp.hospital.beds$total.beds = as.numeric(temp.hospital.beds$total.beds)

temp.disability = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/Disability.csv')
colnames(temp.disability) = c("state_name","disability.prevalence")
temp.disability = temp.disability[which(temp.disability$state_name %in% dt.area$state_name),]
temp.disability $disability.prevalence = as.numeric(temp.disability $disability.prevalence)

temp.heartdisease = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/HeartDisease.csv',skip = 2,header = T)
colnames(temp.heartdisease) = c("state_name","heartdisease.death.per.100k")
temp.heartdisease = temp.heartdisease[which(temp.heartdisease$state_name %in% dt.area$state_name),]

temp.fludeath = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/FluDeaths.csv',skip = 2,header = T)
colnames(temp.fludeath) = c("state_name","influenza.pneumonia.deaths","influenza.deaths","pneumonia.deaths")
temp.fludeath = temp.fludeath[which(temp.fludeath$state_name %in% dt.area$state_name),c("state_name","influenza.pneumonia.deaths")]

temp.fluvax = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/FluVax.csv',skip = 2,header = T)
colnames(temp.fluvax) = c("state_name","fluvax.rate")
temp.fluvax = temp.fluvax[which(temp.fluvax$state_name %in% dt.area$state_name),]

temp.deathrate = fread(file = 'Auxiliary Data/Kaiser Foundation Health Data/DeathRate.csv',skip = 2,header = T)
colnames(temp.deathrate) = c("state_name","deathrate.per.100k")
temp.deathrate = temp.deathrate[which(temp.deathrate$state_name %in% dt.area$state_name),]

dt.area = merge(dt.area,temp.asthma,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.cancer,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.diabetes,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.disability,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.heartdisease,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.fludeath,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.fluvax,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.deathrate ,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.obesity ,by = "state_name",all = TRUE)
dt.area = merge(dt.area,temp.hospital.beds ,by = "state_name",all = TRUE)

##### STATE POLITICAL LEANINGS (state-varying) --> link to website source: https://electionlab.mit.edu/data
US_pres_res = fread(file = 'Auxiliary Data/Election Results 2020/1976-2020-president.csv')
US_pres_res = US_pres_res [US_pres_res $year==2020,]
US_pres_res$pct.vote = US_pres_res $candidatevotes/US_pres_res$totalvotes
US_pres_res  = US_pres_res [,c("state","party_simplified","pct.vote")]
US_pres_res  = US_pres_res [,lapply(.SD,sum),.SDcols = 'pct.vote',by = c("state","party_simplified")]
US_pres_res = reshape(US_pres_res,timevar = "party_simplified",idvar = "state",direction = 'wide')
US_pres_res$state = tools::toTitleCase(tolower(US_pres_res$state))
US_pres_res = US_pres_res[,c("state","pct.vote.REPUBLICAN")]
colnames(US_pres_res)[which(colnames(US_pres_res)=="state")] = "state_name"

dt.area = merge(dt.area,US_pres_res ,by = "state_name",all = TRUE)

##### ##### ##### ##### VARYING BY TIME AND STATE ##### ##### ##### ##### ##### ##### ##### ##### ##### 
FIPS = fread(file = 'Auxiliary Data/FIPS.csv')
##### WEATHER (varying by state and time) --> link to website source: https://github.com/ropensci/rnoaa
index = fread(input = 'Auxiliary Data/index.csv')
weather = fread(input = 'Auxiliary Data/weather.csv')
weather = merge(weather,index ,by = "key",all.x = T)
weather = weather[which(weather$country_name=="United States of America"),]
weather = weather[which(weather$subregion1_name!=""),]
weather = weather[,lapply(.SD,function(x){mean(as.numeric(as.character(unlist(x))),na.rm=TRUE)}),
                  by = c("date","subregion1_code","subregion1_name"),
                  .SDcols = c("average_temperature","minimum_temperature","maximum_temperature","rainfall","snowfall","dew_point","relative_humidity")]
weather = weather[which(weather$subregion1_code %in% state.abb) ]
colnames(weather)[which(colnames(weather)=="subregion1_code")] = "state_abb"
colnames(weather)[which(colnames(weather)=="subregion1_name")] = "state_name"
weather$date = as.Date(weather$date,"%Y-%m-%d")

##### DAILY ECONOMY TRACKER --> link to website source: https://github.com/OpportunityInsights/EconomicTracker
spending = fread(file = 'Auxiliary Data/EconomicTracker-main/data/Affinity - State - Daily.csv')
spending$date = as.Date(paste(spending$year,spending $month,spending $day),"%Y %m %d")
spending$state_name = FIPS$Name[match(spending$statefips,FIPS$FIPS)]
spending = spending[which(spending$freq=="d"),c("date","state_name","spend_all")]
spending$spend_all = as.numeric(spending$spend_all)

job_postings = fread(file = 'Auxiliary Data/EconomicTracker-main/data/Burning Glass - State - Weekly.csv')
job_postings$date = as.Date(paste(job_postings$year,job_postings $month,job_postings $day_endofweek),"%Y %m %d")
job_postings$state_name = FIPS$Name[match(job_postings$statefips,FIPS$FIPS)]
job_postings = job_postings[,c("date","state_name","bg_posts")]
job_postings = 
merge(job_postings,
      expand.grid(date = seq(min(job_postings$date),Sys.Date(),by = 1),state_name = unique(spending$state_name)),
      by = c("date","state_name"),
      all=TRUE)
job_postings[,bg_posts:=na.locf(object = bg_posts),by = "state_name"]

econ = merge(spending,job_postings,by = c("date","state_name"),all=TRUE)

emp = fread(file = 'Auxiliary Data/EconomicTracker-main/data/Employment - State - Daily.csv')
emp$date = as.Date(paste(emp$year,emp $month,emp $day),"%Y %m %d")
emp$state_name = FIPS$Name[match(emp$statefips,FIPS$FIPS)]
emp = emp[,c("date","state_name","emp_combined")]
emp $emp_combined = as.numeric(as.character(unlist(emp $emp_combined)))
econ = merge(econ,emp,by = c("date","state_name"),all=TRUE)

UI_claims = fread(file = 'Auxiliary Data/EconomicTracker-main/data/UI Claims - State - Weekly.csv')
UI_claims$date = as.Date(paste(UI_claims$year,UI_claims$month,UI_claims$day_endofweek),"%Y %m %d")
UI_claims$state_name = FIPS$Name[match(UI_claims$statefips,FIPS$FIPS)]
UI_claims = UI_claims[,c("date","state_name","initclaims_count_combined")]
UI_claims = 
  merge(UI_claims,
        expand.grid(date = seq(min(UI_claims$date),max(Sys.Date()),by = 1),state_name = unique(spending$state_name)),
        by = c("date","state_name"),
        all=TRUE)
UI_claims[,initclaims_count_combined:=na.locf(object = initclaims_count_combined),by = "state_name"]

econ = merge(econ,UI_claims,by = c("date","state_name"),all=TRUE)

small_business_openings = fread(file = 'Auxiliary Data/EconomicTracker-main/data/Womply - State - Daily.csv')
small_business_openings$date = as.Date(paste(small_business_openings$year,small_business_openings $month,small_business_openings $day),"%Y %m %d")
small_business_openings$state_name =FIPS$Name[match(small_business_openings$statefips,FIPS$FIPS)]
small_business_openings = small_business_openings[,c("date","state_name","merchants_all","revenue_all")]

econ = merge(econ,small_business_openings,by = c("date","state_name"),all=TRUE)
econ = econ [which(econ $date>=as.Date("2020-03-01","%Y-%m-%d")),]

dt.area.time = merge(weather[which(weather$date>=as.Date("2020-03-01","%Y-%m-%d")),!"state_abb"],econ,by = c("date","state_name"),all=T)

##### VACCINES --> link to website source: https://github.com/govex/COVID-19/tree/master/data_tables/vaccine_data/us_data
vax = fread(file = 'Auxiliary Data/vaccinations.csv')
vax = vax[vax$key %in% paste("US_",FIPS$`Postal Code`,sep="") ]
colnames(vax)[which(colnames(vax)=="key")] = "state_abb"
vax$state_abb = gsub("US_","",vax$state_abb)
vax$state_name = FIPS$Name[match(vax$state_abb ,FIPS$`Postal Code`)]
vax$date = as.Date(vax$date,"%Y-%m-%d")
vax = vax[,c("date","state_name","new_persons_vaccinated","total_persons_vaccinated","total_persons_fully_vaccinated")]

dt.area.time = merge(dt.area.time,vax,by = c("date","state_name"),all=T)
dt.area.time  = dt.area.time[order(dt.area.time $date),]
# prior to vaccination starting set to 0 
dt.area.time $new_persons_vaccinated[dt.area.time $date %in% seq(as.Date("2020-03-01","%Y-%m-%d"), min(vax$date),by='days')] = 0
dt.area.time[,new_persons_vaccinated:=na.locf(new_persons_vaccinated),by = c("date","state_name")]
dt.area.time $total_persons_vaccinated[dt.area.time $date %in% seq(as.Date("2020-03-01","%Y-%m-%d"), min(vax$date),by='days')] = 0
dt.area.time[,total_persons_vaccinated := na.locf(total_persons_vaccinated),by = c("date","state_name")]
dt.area.time $total_persons_fully_vaccinated[dt.area.time $date %in% seq(as.Date("2020-03-01","%Y-%m-%d"), min(vax$date),by='days')] = 0
dt.area.time[,total_persons_fully_vaccinated := na.locf(total_persons_fully_vaccinated),by = c("date","state_name")]

##### STATE MOBILITY --> link to website source: https://www.bts.gov/browse-statistical-products-and-data/covid-related/changes-mobility-state-0
mobility = fread(input = 'Auxiliary Data/Trips_by_Distance.csv')
mobility = mobility[which(mobility$Level=="State"),]
mobility$Date = as.Date(mobility$Date,"%Y/%m/%d")
mobility$state_name = FIPS$Name[match(mobility$`State Postal Code` ,FIPS$`Postal Code`)]
mobility = mobility[,c("Date","state_name","Population Not Staying at Home","Number of Trips")]
colnames(mobility) = c("date","state_name","pop.not.home","n.trips")

dt.area.time = merge(dt.area.time,mobility,by = c("date","state_name"),all=T)

##### COVID POLICY INDEX --> link to website source: https://github.com/OxCGRT/USA-covid-policy
sheet = c("stringency_index","government_response_index","containment_health_index","economic_support_index")
gov = data.table()
for(i in sheet){
  tmp = read_excel(path = 'Auxiliary Data/OxCGRTUS_timeseries_all.xlsx',sheet = i)
  tmp$index = i
  gov = bind_rows(gov,tmp)
}
gov = melt(gov[,!c("country_code","country_name","jurisdiction")],id.vars = c("region_code","region_name","index"))
gov$region_code = gsub("US_","",gov $region_code)
gov$variable = as.Date(gov$variable,"%d%B%Y")
colnames(gov)[which(colnames(gov)=="region_code")] = "state_abb"
colnames(gov)[which(colnames(gov)=="region_name")] = "state_name"
colnames(gov)[which(colnames(gov)=="variable")] = "date"
gov = reshape(gov,timevar = c("index"),idvar = c("state_abb","state_name","date"),direction = 'wide')
gov$state_name[which(gov$state_name=="Washington DC")] = "District of Columbia"
dt.area.time = merge(dt.area.time,gov[,!"state_abb"],by = c("date","state_name"),all=T)

# merge area and area-time predictor
predictor = merge(dt.area.time,dt.area,by=c("state_name"),all=T)
# focus on exactly 1 year since first prediction 
predictor = predictor[which(predictor$date<as.Date("2021-03-27","%Y-%m-%d") & predictor$date>=as.Date("2020-03-01","%Y-%m-%d")),]
predictor = predictor[order(predictor$date),]
# only for 50 states + DC
predictor = predictor [which(predictor $state_name %in% US_pres_res$state_name),]
predictor$state_name = as.factor(as.character(unlist(predictor$state_name)))
# DC missing weather data - impute with Virginia
predictor[which(predictor$state_name=="District of Columbia"),
          c("average_temperature","minimum_temperature","maximum_temperature","rainfall","snowfall","dew_point","relative_humidity")] = 
  predictor[which(predictor$state_name=="Virginia"),
            c("average_temperature","minimum_temperature","maximum_temperature","rainfall","snowfall","dew_point","relative_humidity")] 

# save predictor 
save(predictor,file = 'Generated Quantities/predictor.RData')
load(file = 'Generated Quantities/predictor.RData')
# multiply impute to complete dataset
# miceRanger
model_train_rf <- miceRanger::miceRanger(data = predictor,m = 1,maxiter = 10,verbose = TRUE,returnModels = TRUE)
predictor_imputations_rf <- miceRanger::impute(data = predictor,miceObj = model_train_rf,verbose = TRUE)
predictor.imp = predictor_imputations_rf$imputedData$Dataset_1
save(predictor.imp,file = 'Generated Quantities/predictor.imp.RData')
load(file = 'Generated Quantities/predictor.imp.RData')
# average at the weekly level
predictor.imp$forecast_week = as.numeric(as.character(unlist(
  floor(difftime(as.Date(predictor.imp$date,"%Y-%m-%d"),
                 min(as.Date("2020-03-27",'%Y-%m-%d')),
                 units = 'weeks')) ) ) )


predictor.week = predictor.imp[,lapply(.SD,mean,na.rm=TRUE), by = c("state_name","forecast_week"),
                               .SDcols = c(colnames(predictor.imp)[-which(colnames(predictor.imp)=="date"|
                                                                            colnames(predictor.imp)=="state_name"|
                                                                            colnames(predictor.imp)=="forecast_week")])]
# take first differences
timevar = c("average_temperature","minimum_temperature","maximum_temperature","rainfall","snowfall","dew_point","relative_humidity",
            "spend_all","bg_posts","emp_combined","initclaims_count_combined","merchants_all","revenue_all",
            "new_persons_vaccinated","total_persons_vaccinated","total_persons_fully_vaccinated",
            "pop.not.home","n.trips",
            "value.stringency_index","value.government_response_index","value.containment_health_index","value.economic_support_index"
            )
predictor.week[, paste0("diff_", c(timevar)) := .SD - shift(.SD), by = c("state_name"),.SDcols = c(timevar)]
predictor.week = predictor.week[predictor.week$forecast_week>=0] # we kept the weeks before the first relevant week to ensure no missing values on first difference
# save weekly predictor
save(predictor.week,file = 'Generated Quantities/predictor.week.RData')

load(file = 'Generated Quantities/predictor.week.RData')
