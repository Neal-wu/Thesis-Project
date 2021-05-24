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
# load COVID data
library(zoltr)
library(covidHubUtils)
dates = as.character(seq.Date(as.Date("2020-03-27"), Sys.Date(), by = "1 days"))
#to ensure no empty dates, break dates vector into chunks of 10 dates
dates = split(dates, ceiling(seq_along(dates)/10))
# focus on all point estimates for the main 50 states and DC
# # # load forecasts
temp.forecast.list =data.table()

for(i in 1:length(dates)){ 
  temp.forecast <- NULL
  attempt <- 0
  while( is.null(temp.forecast) && attempt <=3 ) {
    try(
      temp.forecast <- load_forecasts(forecast_dates = dates[[i]],
                                      types = c("point")
                                      )
    )
    if(is.null(temp.forecast)){    
    attempt <- attempt + 1
    Sys.sleep(time =  60*5) 
    }
  } 

  # focus on inc death/hosp/case
  temp.forecast = as.data.table(temp.forecast)[which(temp.forecast$target_variable %in% c("inc death","inc hosp","inc case") ),]
  
  # focus on 1-day, 1-week, 2-weeks 1-month, 
  temp.forecast = temp.forecast [which(temp.forecast$temporal_resolution=="day" & temp.forecast$horizon %in% c(1,7,14,21,28)|
                                       temp.forecast$temporal_resolution=="wk" & temp.forecast$horizon %in% c(1,2,3,4)),!"quantile"]
  
  temp.forecast.list = bind_rows(temp.forecast.list,temp.forecast)
}
save( temp.forecast.list,file = 'Generated Quantities/weeks_ahead_forecasts.RData')
load(file = 'Generated Quantities/weeks_ahead_forecasts.RData')
# # # load truth
  temp.truth = data.table()
  temp.truth.death = data.table()
  temp.truth.case = data.table()
  temp.truth.hosp = data.table()
  for(i in c('daily','weekly')){
  temp.truth.death.temp = load_truth(target_variable = "inc death",
                               truth_source = "NYTimes",
                               temporal_resolution = i
  )
  temp.truth.death.temp = as.data.table(temp.truth.death.temp)
  temp.truth.death.temp$temporal_resolution =  ifelse(i=="weekly","wk","day")
  temp.truth.death = bind_rows(temp.truth.death,temp.truth.death.temp)
  
  temp.truth.case.temp = load_truth(target_variable = "inc case",
                               truth_source = "NYTimes",
                               temporal_resolution =  i
  )
  temp.truth.case.temp = as.data.table(temp.truth.case.temp)
  temp.truth.case.temp$temporal_resolution =  ifelse(i=="weekly","wk","day")
  temp.truth.case = bind_rows(temp.truth.case,temp.truth.case.temp)
  
  temp.truth.hosp.temp = load_truth(target_variable = "inc hosp",
                               truth_source = "HealthData",
                               temporal_resolution =  i
  )
  temp.truth.hosp.temp = as.data.table(temp.truth.hosp.temp)
  temp.truth.hosp.temp$temporal_resolution = ifelse(i=="weekly","wk","day")
  temp.truth.hosp = bind_rows(temp.truth.hosp,temp.truth.hosp.temp)
  }
  
  temp.truth = unique(bind_rows(temp.truth,
                                as.data.table(temp.truth.death),
                                as.data.table(temp.truth.case),
                                as.data.table(temp.truth.hosp)
                                ))
save( temp.truth,file = 'Generated Quantities/truth.RData')
load(file = 'Generated Quantities/truth.RData')
# # # generate truth-forecast comparison dataset
colnames(temp.forecast.list )[which(colnames(temp.forecast.list )=="model")] = 'team_model'
colnames(temp.forecast.list )[which(colnames(temp.forecast.list )=="value")] = 'forecast_value'
colnames(temp.truth )[which(colnames(temp.truth )=="model")] = 'truth_source'
colnames(temp.truth )[which(colnames(temp.truth )=="value")] = 'truth_value'
temp.forecast.errors = merge(temp.forecast.list,
                             temp.truth, 
                             by = c("target_variable","target_end_date","location","location_name","temporal_resolution",
                                    "population","geo_type","geo_value","abbreviation","full_location_name"),
                             all = TRUE)
# focus on complete cases (sometimes we don't have either forecasts or truths)
temp.forecast.errors=temp.forecast.errors[complete.cases(temp.forecast.errors),]
temp.forecast.errors$abs_error = abs(temp.forecast.errors$truth_value- temp.forecast.errors$forecast_value)
# aggregate error across counties and forecast weeks and model-type
temp.forecast.errors$forecast_week = as.numeric(as.character(unlist(
  floor(difftime(as.Date(temp.forecast.errors$forecast_date,'%Y-%m-%d'),
                 min(as.Date(temp.forecast.errors$forecast_date,'%Y-%m-%d')),
                 units = 'weeks')) ) ) )
# # # classification of model-types

# parametic-compartmental:
# -> classic and parametrically-augmented models using as backbones the SIR, SEIR, SLIR, etc. family ;

# non.parametric-compartmental: 
# -> building on classic compartmental models, and augmenting with ML / or network-structures ;

# parametric-time.series:
# -> ARIMA / SARIMA / ARMA / RW family ;

# non.parametric-time.series:
# -> ARIMA / SARIMA / ARMA / RW family + ML / deep-learning / graph layers ;

# parametric-abm: 
# -> agent-based modeling ;

# ensemble: 
# -> simple or complex averages of forecasts ;

# parametric - other:
# -> piecewise linear models, survival models, 

# non.parametric-other:
# -> deep-learning, graphs, networks, Gaussian process, splines, Bayesian non-parametric, mixtures;
rm(temp.truth)
rm(temp.forecast.list)
temp.forecast.errors$model_type = NA
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="AIpert-pwllnod")] = "parametric-other-piecewise.regression" #"parametric-curve.fit-piecewise.regression"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="BPagano-RtDriven")] = "parametric-compartmental-SIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Caltech-CS156")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CDDEP-SEIR_MCMC")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CEID-Walk")] = "parametric-time.series-RW"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CMU-TimeSeries")] = "parametric-time.series-AR.lasso"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Columbia_UNC-SurvCon")] = "parametric-other-survival.piecewise.regression"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Covid19Sim-Simulator")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CovidActNow-SEIR_CAN")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CovidAnalytics-DELPHI")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="COVIDhub-baseline")] = "parametric-time.series-RW.first.difference"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="COVIDhub-ensemble")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="COVIDhub-trained_ensemble")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CU-nochange")] = "parametric-compartmental-SEIR.Rt"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CU-scenario_high")] = "parametric-compartmental-SEIR.Rt"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CU-scenario_low")] = "parametric-compartmental-SEIR.Rt"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CU-scenario_mid")] = "parametric-compartmental-SEIR.Rt"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="CU-select")] = "parametric-compartmental-SEIR.Rt"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="DDS-NBDS")] = "non.parametric-other-negative.binomial.Bayesian.graph"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="epiforecasts-ensemble1")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="FAIR-NRAR")] = "non.parametric-time.series-neural.coefs"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="FDANIHASU-Sweight")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="FRBSF_Wilson-Econometric")] = "parametric-compartmental-SIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Geneva-DetGrowth")] = "non.parametric-other-loess.seasonal"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Google_Harvard-CPF")] = "non.parametric-compartmental-end.to.end.learning"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="GT_CHHS-COVID19")] = "parametric-agent.based.model-"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="GT-DeepCOVID")] = "non.parametric-other-deep.neural.network"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="IBF-TimeSeries")] = "parametric-time.series-"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="IEM_MED-CovidProject")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="IHME-CurveFit")] = "parametric-other-nonlinear.model"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Imperial-ensemble1")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Imperial-ensemble2")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="IowaStateLW-STEM")] = "non.parametric-compartmental-SIR.space.time"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="IQVIA_ACOE-STAN")] = "non.parametric-other-deep.neural.network"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="IUPUI-HkPrMobiDyR")] = "parametric-time.series-hawkes.processes"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="JCB-PRM")] = "parametric-other-political.realities"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="JHU_CSSE-DECOM")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="JHU_IDD-CovidSP")] = "parametric-compartmental-SEIR.stochastic"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="JHU_UNC_GAS-StatMechPool")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="JHUAPL-Bucky")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="JHUAPL-Gecko")] = "parametric-time.series-SARIMA.anomaly.detect"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="JHUAPL-SLPHospEns")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Karlen-pypm")] = "parametric-other-discrete.difference.eq.I.cycle"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="KITmetricslab-select_ensemble")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="LANL-GrowthRate")] = "parametric-compartmental-SI.dynamic"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="LNQ-ens1")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Microsoft-DeepSTIA")] = "non.parametric-other-deep.spatio.temporal"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="MIT_CritData-GBCF")] = "non.parametric-other-gradient.boosted.regressor"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="MIT_ISOLAT-Mixtures")] = "non.parametric-other-gaussian.mixture"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="MIT-Cassandra")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="MITCovAlliance-SIR")] = "parametric-compartmental-SIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="MOBS-GLEAM_COVID")] = "parametric-compartmental-SLIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="MSRA-DeepST")] = "non.parametric-compartmental-deep.spatio.temporal"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="NotreDame-FRED")] = "parametric-agent.based.model-"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="NotreDame-mobility")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="OliverWyman-Navigator")] = "parametric-compartmental-non.stationary.transition"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="OneQuietNight-ML")] = "non.parametric-other-ML"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="PandemicCentral-COVIDForest")] = "non.parametric-other-RF"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="PandemicCentral-USCounty")] = "non.parametric-other-RF"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="PSI-DRAFT")] = "parametric-compartmental-SEIRX"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="QJHong-Encounter")] = "parametric-other-encounter.density.by.R"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Quantori-Multiagents")] = "parametric-agent.based.model-"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="RobertWalraven-ESG")] = "parametric-other-piecewise.skewed.gaussians"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="RPI_UW-Mob_Collision")] = "parametric-compartmental-SIR.collision.theory"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="SigSci-TS")] = "parametric-time.series-ARIMA"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="SteveMcConnell-CovidComplete")] = "parametric-time.series-near.term.trend"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="STH-3PU")] = "parametric-other-three.phases.growth.policy.immune"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="SWC-TerminusCM")] = "parametric-compartmental-"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="TTU-squider")] = "parametric-compartmental-SIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UA-EpiCovDA")] =  "parametric-compartmental-SIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UCF-AEM")] = "non.parametric-compartmental-mixture.and.nets"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UChicago-CovidIL")] =  "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UChicago-CovidIL_10_+")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UChicago-CovidIL_100")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UChicago-CovidIL_30_+")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UChicago-CovidIL_40")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UChicago-CovidIL_60")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UChicago-CovidIL_80")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UChicagoCHATTOPADHYAY-UnIT")] = "parametric-other-geo.spatial.influenza.risk"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UCLA-SuEIR")] = "parametric-compartmental-SuEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UCM_MESALab-FoGSEIR")] =  "parametric-compartmental-SEIR.fractional"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UCSB-ACTS")] = "non.parametric-time.series-attention.crossing"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UCSD_NEU-DeepGLEAM")] = "non.parametric-other-spatio.temporal.deep"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UMass-MechBayes")] = "parametric-compartmental-"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UMich-RidgeTfReg")] = "parametric-other-ridge"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UpstateSU-GRU")] = "non.parametric-other-neural.net"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="USACE-ERDC_SEIR")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="USC-SI_kJalpha")] = "parametric-compartmental-SIkJalpha"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="USC-SI_kJalpha_RF")] = "non.parametric-other-RF"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UT-Mobility")] = "parametric-other-mobility"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="UVA-Ensemble")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Wadhwani_AI-BayesOpt")] = "parametric-compartmental-SEIR"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="Yu_Group-CLEP")] = "ensemble"
temp.forecast.errors$model_type[which(temp.forecast.errors$team_model=="YYG-ParamSearch")] = "non.parametric-compartmental-SEIR.ML"
temp.forecast.errors$model_type.general = sub('-[^-]*$', '', temp.forecast.errors$model_type)
# the difference between parametric and non-parametric curve-fit model is not obviously useful - 
# there is a qualitative difference, but ultimately all of these models are seeking predict curves, not 
# report model classification table 
library(xtable)
model_class = unique(temp.forecast.errors[,c("model_type.general","team_model")])
model_class[order(model_class$model_type.general),]
# calculate absolute error per 10k people (sclaes forecast error and allows comparison across geo-type)
temp.forecast.errors$abs_error_per10k.pop = 10000*temp.forecast.errors$abs_error/temp.forecast.errors$population
save( temp.forecast.errors,file = 'Generated Quantities/forecasts_and_truth.RData')
load(file = 'Generated Quantities/forecasts_and_truth.RData')
# aggregate forecast error by week of making forecast, target variable, resolution, horizon, geo-type and state
temp.forecast.errors.AGG = temp.forecast.errors[,lapply(.SD,mean),by = c("model_type.general","forecast_week","target_variable","temporal_resolution","horizon","geo_type","abbreviation"),.SDcols = c("abs_error_per10k.pop","population")]
gc()
# prepare lvels of analysis
temp.forecast.errors.AGG$temporal_resolution = as.factor(temp.forecast.errors.AGG$temporal_resolution)
temp.forecast.errors.AGG$geo_type = as.factor(temp.forecast.errors.AGG$geo_type)
temp.forecast.errors.AGG$abbreviation = as.factor(temp.forecast.errors.AGG$abbreviation)
temp.forecast.errors.AGG$horizon = as.numeric(as.character(unlist(temp.forecast.errors.AGG$horizon )))
temp.forecast.errors.AGG$horizon_weeks = ifelse( temp.forecast.errors.AGG$temporal_resolution=="day",
                                                 floor(temp.forecast.errors.AGG$horizon/7),
                                                 temp.forecast.errors.AGG$horizon)
temp.forecast.errors.AGG$target_variable = as.factor(temp.forecast.errors.AGG$target_variable)
temp.forecast.errors.AGG = temp.forecast.errors.AGG[,!c("horizon")]
save( temp.forecast.errors.AGG,file = 'Generated Quantities/forecasts_and_truth.AGG.RData')
load(file = 'Generated Quantities/forecasts_and_truth.AGG.RData')
