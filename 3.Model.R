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
# load forecast error data
load(file = 'Generated Quantities/forecasts_and_truth.AGG.RData')
# load time-stae pred 
load(file = 'Generated Quantities/predictor.week.RData')
# load abb to name index
FIPS = fread(file = 'Auxiliary Data/FIPS.csv')
# merge forecast error and auxiliary data
temp.forecast.errors.AGG$state_name = FIPS$Name[match(as.character(unlist(temp.forecast.errors.AGG$abbreviation)),FIPS$`Postal Code`)]
temp.forecast.errors.AGG = temp.forecast.errors.AGG[!is.na(temp.forecast.errors.AGG$state_name),] # USA is NA
temp.forecast.errors.AGG = merge(predictor.week,temp.forecast.errors.AGG[,!"population"],by = c("state_name","forecast_week"))
dt = temp.forecast.errors.AGG 
# setup factors
dt$model_type.general = as.factor(dt$model_type.general)
dt$state_name = as.factor(dt$state_name)
dt$target_variable = as.factor(dt$target_variable)
dt$geo_type = as.factor(dt$geo_type)
# scale weekly errors to daily errors, so we preserve comparability with daily errors
dt$abs_error_per10k.pop = ifelse(dt$temporal_resolution=="wk",
                                                       dt$abs_error_per10k.pop/7,
                                                       dt$abs_error_per10k.pop
                                                       )

dt$log_abs_error_per10k.pop = log(dt$abs_error_per10k.pop)
# a number 1/4 of 1%  of logged observations were exactly 0 in non-logged terms, leading to -Inf;
# so we set them to the lowest value instead
100*sum(dt$log_abs_error_per10k.pop=="-Inf")/dim(dt)[1]
dt$log_abs_error_per10k.pop[which(dt$log_abs_error_per10k.pop=="-Inf")] = min(dt$log_abs_error_per10k.pop[-which(dt$log_abs_error_per10k.pop=="-Inf")])

# error summary plots by state
for(s in 1:nlevels(dt$state_name)){
  pdf(file = paste('Plots/',levels(dt$state_name)[s],".pdf",sep=""),width = 15,height = 20)
  par(mfrow = c(5,3))
  for(q in 0:max(dt$horizon_weeks)){
    
    
  for(i in 1:nlevels(dt$target_variable)){
    
    y = dt$abs_error_per10k.pop[which(dt$state_name==levels(dt$state_name)[s] & 
                                                              dt$target_variable==levels(dt$target_variable)[i])]
    x = dt$forecast_week[which(dt$state_name==levels(dt$state_name)[s] & 
                                                       dt$target_variable==levels(dt$target_variable)[i])]
    if(length(y)==0){next}
    plot(y = y,x =x,
         pch = NA,xlab = "weeks since 1st forecast",ylab = 'absolute error',xaxt = "n",
         xlim = c(min(dt$forecast_week,na.rm=TRUE),max(dt$forecast_week,na.rm=TRUE)),
         ylim = c(min(y),max(y)),
         main = paste(levels(dt$state_name)[s],
                      levels(dt$target_variable)[i],
                      paste(q,"week(s) ahead",sep=" "),
                      sep = " - "))
    axis(side = 1,at = min(dt$forecast_week,na.rm=TRUE):max(dt$forecast_week,na.rm=TRUE),min(dt$forecast_week,na.rm=TRUE):max(dt$forecast_week,na.rm=TRUE))
    for(k in 1:nlevels(dt$geo_type)){
    for(j in 1:nlevels(dt$model_type)){
      y = dt$abs_error_per10k.pop[which(dt$state_name==levels(dt$state_name)[s] & 
                                                                dt$target_variable==levels(dt$target_variable)[i] & 
                                                                dt$model_type==levels(dt$model_type)[j] &
                                                                dt$geo_type==levels(dt$geo_type)[k] &
                                                                dt$horizon_weeks==q
                                                              )]
      
      x = dt$forecast_week[which(dt$state_name==levels(dt$state_name)[s] & 
                                                         dt$target_variable==levels(dt$target_variable)[i] & 
                                                         dt$model_type==levels(dt$model_type)[j]  &
                                                         dt$geo_type==levels(dt$geo_type)[k] &
                                                         dt$horizon_weeks==q
                                                       )]
      
      if(length(y)<=0){next}
      
      temp.j = order(x)
      lines(y = y[temp.j],x =x[temp.j],col = (1:nlevels(dt$model_type))[j],lwd = 2,lty = c(1,2)[k])
    } }
    legend('topleft',
           legend = c(levels(dt$model_type),
                      paste(levels(dt$geo_type),"-level pred.",sep="")),
           col = c((1:nlevels(dt$model_type)),1,1),
           lty = c(rep(NA,nlevels(dt$model_type)),1,2),
           pch = c(rep(15,nlevels(dt$model_type)),NA,NA))
  } 
  }
  dev.off()
}
# list fixed effects - state-varying
dt$ethn.variance = apply(dt[,c("Hispanic.or.Latino..of.any.race.","White.alone","Black.or.African.American.alone","Asian.alone")],1,sd)
fixed.effects.state = c("pop.size","pop.density",
                        "Median.age..years.","X65.years.and.over",
                        "Hispanic.or.Latino..of.any.race.","Black.or.African.American.alone","Asian.alone","ethn.variance","Foreign.born",
                        "Average.household.size","Median.household.income..dollars.","Poverty.All.people",
                        "Percent.bachelor.s.degree.or.higher",
                        "pct_asthma","diabetes","obesity","heartdisease.death.per.100k","influenza.pneumonia.deaths","fluvax.rate","total.beds","deathrate.per.100k",
                        "pct.vote.REPUBLICAN"
                        )
# list fixed effects - state-time-varying
fixed.effects.state.time= c("average_temperature","rainfall","snowfall","relative_humidity",
                            "spend_all","bg_posts","emp_combined","initclaims_count_combined","merchants_all","revenue_all",
                            "new_persons_vaccinated","total_persons_vaccinated","total_persons_fully_vaccinated",
                            "pop.not.home","n.trips",
                            "value.containment_health_index","value.economic_support_index",
                            "diff_average_temperature","diff_rainfall","diff_snowfall","diff_relative_humidity",
                            "diff_spend_all","diff_bg_posts","diff_emp_combined","diff_initclaims_count_combined","diff_merchants_all","diff_revenue_all",
                            "diff_new_persons_vaccinated","diff_total_persons_vaccinated","diff_total_persons_fully_vaccinated",
                            "diff_pop.not.home","diff_n.trips",
                            "diff_value.containment_health_index","diff_value.economic_support_index"
                            )
# take a sub-sample 
dt = dt[sample(1:dim(dt)[1],size = 5000),]
# STAN options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# data list
data.list = list(Y = as.numeric(scale(dt$log_abs_error_per10k.pop)),
                 n = dim(dt)[1],
                 
                 forecast_week_id = dt$forecast_week+1,
                 N_forecast_week = max(dt$forecast_week+1),
                 forecast_week_labels = min(dt$forecast_week):max(dt$forecast_week),
                 
                 
                 state_id = as.integer(dt$state_name),
                 N_state = nlevels(dt$state_name),
                 state_name_labels = levels(dt$state_name),
                 
                 
                 model_type_id = as.integer(dt$model_type.general),
                 N_model_type = nlevels(dt$model_type.general),
                 model_type_labels = levels(dt$model_type.general),
                 
                 target_variable_id = as.integer(dt$target_variable),
                 N_target_variable = nlevels(dt$target_variable),
                 target_variable_labels = levels(dt$target_variable),
                 
                 temporal_resolution_id = as.integer(dt$temporal_resolution),
                 N_temporal_resolution = nlevels(dt$temporal_resolution),
                 temporal_resolution_labels = levels(dt$temporal_resolution),
                 
                 geo_type_id = as.integer(dt$geo_type),
                 N_geo_type = nlevels(dt$geo_type),
                 geo_type_labels = levels(dt$geo_type),
                 
                 horizon_weeks_id = dt$horizon_weeks+1,
                 N_horizon_weeks = max(dt$horizon_weeks+1),
                 horizon_weeks_labels = min(dt$horizon_weeks):max(dt$horizon_weeks),
                 
                 model_type_AND_horizon_weeks_id = as.integer(as.factor(paste(dt$model_type.general,dt$horizon_weeks))),
                 N_model_type_AND_horizon_weeks = max(as.integer(as.factor(paste(dt$model_type.general,dt$horizon_weeks)))),
                 model_type_AND_horizon_weeks_labels = levels(as.factor(paste(dt$model_type.general,dt$horizon_weeks))),
                 
                 model_type_AND_target_variable_id = as.integer(as.factor(paste(dt$model_type.general,dt$target_variable))),
                 N_model_type_AND_target_variable = max(as.integer(as.factor(paste(dt$model_type.general,dt$target_variable)))),
                 model_type_AND_target_variable_labels = levels(as.factor(paste(dt$model_type.general,dt$target_variable))),
                 
                 target_variable_AND_horizon_weeks_id = as.integer(as.factor(paste(dt$target_variable,dt$horizon_weeks))),
                 N_target_variable_AND_horizon_weeks = max(as.integer(as.factor(paste(dt$target_variable,dt$horizon_weeks)))),
                 target_variable_AND_horizon_weeks_labels = levels(as.factor(paste(dt$target_variable,dt$horizon_weeks))),
                 
                 model_type_AND_target_variable_AND_horizon_weeks_id = as.integer(as.factor(paste(dt$model_type.general,dt$target_variable,dt$horizon_weeks))),
                 N_model_type_AND_target_variable_AND_horizon_weeks = max(as.integer(as.factor(paste(dt$model_type.general,dt$target_variable,dt$horizon_weeks)))),
                 model_type_AND_target_variable_AND_horizon_weeks_labels = levels(as.factor(paste(dt$model_type.general,dt$target_variable,dt$horizon_weeks))),
                 
                 X_state = scale(dt[,c(fixed.effects.state),with=FALSE]),
                 p_state = length(fixed.effects.state),
                 X_state_labels = fixed.effects.state,
                 
                 X_state_time = scale(dt[,c(fixed.effects.state.time),with=FALSE]),
                 p_state_time = length(fixed.effects.state.time),
                 X_state_time_labels = fixed.effects.state.time
                 )


# Weakly informative prior distribution for J < 5; Noninformative prior distributions for
# J >5; AVoid using inverse gamma
# half-t distributions
model.code= "data {
                    int<lower = 1> n; // total number of observations
                           real Y[n]; // model average rmse 
            
                  int<lower = 1> p_state; // number of covariates in state level design matrix 
              matrix[n, p_state] X_state; // design matrix
        
             int<lower = 1> p_state_time; // number of covariates in state-time level design matrix 
    matrix[n, p_state_time] X_state_time; // design matrix

                  int<lower = 1> N_state; // state_level effect
              int<lower = 1> state_id[n]; //
              
          int<lower = 1> N_forecast_week; // forecast_week effect
      int<lower = 1> forecast_week_id[n]; //
      
             int<lower = 1> N_model_type; // model_type effect
         int<lower = 1> model_type_id[n]; //

        int<lower = 1> N_target_variable; // target_variable effect
    int<lower = 1> target_variable_id[n]; //

    int<lower = 1> N_temporal_resolution; // temporal_resolution effect
int<lower = 1> temporal_resolution_id[n]; //

               int<lower = 1> N_geo_type; // model_type effect
           int<lower = 1> geo_type_id[n]; //

          int<lower = 1> N_horizon_weeks; // horizon_weeks effect
      int<lower = 1> horizon_weeks_id[n]; //

                        int<lower = 1> N_model_type_AND_horizon_weeks;
                    int<lower = 1> model_type_AND_horizon_weeks_id[n]; //

                      int<lower = 1> N_model_type_AND_target_variable;
                  int<lower = 1> model_type_AND_target_variable_id[n]; //

                   int<lower = 1> N_target_variable_AND_horizon_weeks;
               int<lower = 1> target_variable_AND_horizon_weeks_id[n]; //

    int<lower = 1> N_model_type_AND_target_variable_AND_horizon_weeks;
int<lower = 1> model_type_AND_target_variable_AND_horizon_weeks_id[n]; // 

}

parameters {
                                                           real alpha; 
                   
                                           vector[p_state] beta_state; 
                                 vector[p_state_time] beta_state_time; 

                                             vector[N_state] eta_state;
                                           real<lower = 0> sigma_state;
                                           
                             vector[N_forecast_week] eta_forecast_week; 
                                   real<lower = 0> sigma_forecast_week; 
                                   
                                   vector[N_model_type] eta_model_type; 
                                      real<lower = 0> sigma_model_type;

                             vector[N_horizon_weeks] eta_horizon_weeks; 
                                   real<lower = 0> sigma_horizon_weeks;

                         vector[N_target_variable] eta_target_variable; 
                                 real<lower = 0> sigma_target_variable;

                                       vector[N_geo_type] eta_geo_type; 
                                        real<lower = 0> sigma_geo_type;

                 vector[N_temporal_resolution] eta_temporal_resolution; 
                             real<lower = 0> sigma_temporal_resolution;
                                  
vector[N_model_type_AND_horizon_weeks] eta_model_type_AND_horizon_weeks; 
                     real<lower = 0> sigma_model_type_AND_horizon_weeks;

vector[N_model_type_AND_target_variable] eta_model_type_AND_target_variable; 
                       real<lower = 0> sigma_model_type_AND_target_variable;

vector[N_target_variable_AND_horizon_weeks] eta_target_variable_AND_horizon_weeks; 
                          real<lower = 0> sigma_target_variable_AND_horizon_weeks;
                        
vector[N_model_type_AND_target_variable_AND_horizon_weeks] eta_model_type_AND_target_variable_AND_horizon_weeks; 
                                         real<lower = 0> sigma_model_type_AND_target_variable_AND_horizon_weeks;
                                         
                                                  real<lower = 0> sigma; // sampling standard deviation

}

  
transformed parameters{
  vector[n] mu; // regression mean
  vector[n] res; 
  
  // linear function of the logit-scale propensity to be a recruit
  mu = alpha + 
       
       X_state * beta_state + 
       X_state_time * beta_state_time +
       
       eta_state[state_id]*sigma_state +
       eta_forecast_week[forecast_week_id]*sigma_forecast_week +
       
       eta_model_type[model_type_id]*sigma_model_type +
       eta_horizon_weeks[horizon_weeks_id]*sigma_horizon_weeks +
       eta_target_variable[target_variable_id]*sigma_target_variable +
       
       eta_geo_type[geo_type_id]*sigma_geo_type +
       eta_temporal_resolution[temporal_resolution_id]*sigma_temporal_resolution +

       eta_model_type_AND_horizon_weeks[model_type_AND_horizon_weeks_id]*sigma_model_type_AND_horizon_weeks +
       eta_model_type_AND_target_variable[model_type_AND_target_variable_id]*sigma_model_type_AND_target_variable +
       eta_target_variable_AND_horizon_weeks[target_variable_AND_horizon_weeks_id]*sigma_target_variable_AND_horizon_weeks +
        
       eta_model_type_AND_target_variable_AND_horizon_weeks[model_type_AND_target_variable_AND_horizon_weeks_id]*sigma_model_type_AND_target_variable_AND_horizon_weeks;
       
  res = Y[n] - mu;
}

model {
  // // // Fixed Effects
  alpha ~ normal(0,1); // vague non-informative priors
  beta_state ~ normal(0,1);
  beta_state_time ~ normal(0,1);
 
  // // // Random Effects  
  eta_state ~ normal(0,1);
  sigma_state ~ normal(0,1);
  
  eta_model_type ~ normal(0,1);
  sigma_model_type ~ normal(0,1); 
  
  eta_horizon_weeks ~ normal(0,1);
  sigma_horizon_weeks ~ normal(0,1); 
  
  eta_target_variable ~ normal(0,1);
  sigma_target_variable ~ normal(0,1); 
       
  eta_geo_type ~ normal(0,1);
  sigma_geo_type ~ normal(0,1); 
  
  eta_temporal_resolution ~ normal(0,1);
  sigma_temporal_resolution ~ normal(0,1); 

  eta_model_type_AND_horizon_weeks ~ normal(0,1);
  sigma_model_type_AND_horizon_weeks ~ normal(0,1); 
  
  eta_model_type_AND_target_variable ~ normal(0,1);
  sigma_model_type_AND_target_variable ~ normal(0,1); 
  
  eta_target_variable_AND_horizon_weeks ~ normal(0,1);
  sigma_target_variable_AND_horizon_weeks ~ normal(0,1); 
        
  eta_model_type_AND_target_variable_AND_horizon_weeks ~ normal(0,1);
  sigma_model_type_AND_target_variable_AND_horizon_weeks ~ normal(0,1);
  
  // // // Temporal Effect
  sigma_forecast_week ~ normal(0,1); 
  for (t in 2:N_forecast_week) {
  eta_forecast_week[t] ~normal(eta_forecast_week[t-1],1);
  }
  sum(eta_forecast_week) ~ normal(0, 0.01 * N_forecast_week); // constraint so we can write likelihood for rw(1).

  // // // Likelihood
  Y ~ normal(mu,sigma);
  // // // Prior on sampling standard deviation
  sigma ~ normal(0,1);
}

generated quantities {
  // posterior predictive check
  real Y_rep[n] = normal_rng(mu,sigma);
}
"
pars = c("alpha","beta_state","beta_state_time",
         "eta_state","sigma_state",
         "eta_forecast_week","sigma_forecast_week",
         "eta_geo_type","sigma_geo_type",
         "eta_temporal_resolution","sigma_temporal_resolution",
         "eta_target_variable","sigma_target_variable",
         "eta_horizon_weeks","sigma_horizon_weeks",
         "eta_model_type","sigma_model_type",
         "eta_model_type_AND_horizon_weeks","sigma_model_type_AND_horizon_weeks",
         "eta_model_type_AND_target_variable","sigma_model_type_AND_target_variable",
         "eta_target_variable_AND_horizon_weeks","sigma_target_variable_AND_horizon_weeks",
         "eta_model_type_AND_target_variable_AND_horizon_weeks","sigma_model_type_AND_target_variable_AND_horizon_weeks",
         "res","mu","sigma","Y_rep")

  stan.model.fit <- stan(model_code = model.code, 
                         data = data.list, 
                         iter = 250,
                         warmup =125,
                         thin = 4,
                         pars = pars,
                         cores =4,
                         chains = 4,
                         control = list(max_treedepth =10),
                         verbose = TRUE)
save(stan.model.fit,file = 'Generated Quantities/stan.model.fit.RData',compress = TRUE)
save(data.list,file = 'Generated Quantities/stan.data.list.RData',compress = TRUE)
save(pars,file = 'Generated Quantities/stan.pars.RData',compress = TRUE)
