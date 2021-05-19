library(readr)
library(plyr)
library(dplyr)
library(data.table)
library(rstan)
library(stringr) 
library(readxl)
library(anytime)
library(lubridate)
library(Date)
library(zoo)
# aggregate based on type, target, state, phase 
ndata <- read.csv("Model_top.csv")
ndata <- ndata[,-c(2,7,8)]
p <- ndata
p$phase <- ifelse(p$forecast_end_date < as.Date('2020-02-01'),
                  '202001',
                  ifelse(p$forecast_end_date < 
                           as.Date('2020-03-01'),'202002',
                         ifelse(p$forecast_end_date < 
                                  as.Date('2020-04-01'),'202003',
                                ifelse(p$forecast_end_date < 
                                         as.Date('2020-05-01'),'202004',
                                       ifelse(p$forecast_end_date < 
                                                as.Date('2020-06-01'),'202005',
                                              ifelse(p$forecast_end_date < 
                                                       as.Date('2020-07-01'),'202006',
                                                     ifelse(p$forecast_end_date < 
                                                              as.Date('2020-08-01'),'202007',
                                                            ifelse(p$forecast_end_date < 
                                                                     as.Date('2020-09-01'),'202008',
                                                                   ifelse(p$forecast_end_date < 
                                                                            as.Date('2020-10-01'),'202009',
                                                                          ifelse(p$forecast_end_date < 
                                                                                   as.Date('2020-11-01'),'202010',
                                                                                 ifelse(p$forecast_end_date < 
                                                                                          as.Date('2020-12-01'),'202011',
                                                                                        ifelse(p$forecast_end_date < 
                                                                                                 as.Date('2021-01-01'),'202012',
                                                                                               ifelse(p$forecast_end_date < 
                                                                                                        as.Date('2021-02-01'),'202101',
                                                                                                      ifelse(p$forecast_end_date < 
                                                                                                               as.Date('2021-03-01'),'202102',
                                                                                                             ifelse(p$forecast_end_date < 
                                                                                                                      as.Date('2021-04-01'),'202103',
                                                                                                                    ifelse(p$forecast_end_date < 
                                                                                                                             as.Date('2021-05-01'),'202104',
                                                                                                                    ))))))))))))))))
dt.agg <- p
colnames(dt.agg)[colnames(dt.agg)=="error"] = 'relative_error'
dt.agg$relative_error[which(is.na(dt.agg$relative_error))] = 0
dt.agg$std.relative_error = (dt.agg$relative_error - mean(dt.agg$relative_error,))/sd(dt.agg$relative_error)
dt.agg <- as.data.table(dt.agg)
dt.agg = dt.agg[,lapply(.SD,function(x){sqrt(mean(x^2,na.rm=TRUE))}), # rmse function
                by = c("model_type","location_name","target","phase","state_population"),
                .SDcols = 'std.relative_error']
colnames(dt.agg)[which(colnames(dt.agg)=="std.relative_error")] = 'std.relative_rmse'
dt.agg <- dt.agg[!grepl('US',dt.agg$location_name),]
dt.agg <- dt.agg[!grepl('District of Columbia',dt.agg$location_name),]
# policy （state and time ) 
state_policy <- read_csv("policy_0423.csv")
state_policy <- state_policy[,c(3,6,68,70,72)]
state_policy <- na.omit(state_policy)
colnames(state_policy) <- c('location_name','forecast_end_date','government response',
                            'containment measure','economic support')
state_policy$phase <- substr(state_policy$forecast_end_date, 1,6)
state_p <- aggregate(list(state_policy$`government response`,state_policy$`containment measure`,
                          state_policy$`economic support`),
                     by = list(state_policy$location_name,state_policy$phase), mean)
colnames(state_p) <- c("location_name","phase","government_response","containment_measure",
                       "economic_support")
state_p$government_response[state_p$government_response==0] <- 0.0001
state_p$containment_measure[state_p$containment_measure==0] <- 0.0001
state_p$economic_support[state_p$economic_support==0] <- 0.0001
state_change <- state_p %>%
  group_by(location_name) %>%
  mutate(
    Gov_res_change = ( government_response - lag(government_response)) / lag(government_response),
    con_mea_change = ( containment_measure - lag(containment_measure)) / lag(containment_measure),
    eco_sup_change = ( economic_support - lag(economic_support)) / lag(economic_support),
  )
state_change[,6:8] <- lapply(state_change[,6:8], function(x) replace(x, !is.finite(x), 0))
dt.agg <- join(dt.agg,state_change,by=c('location_name','phase'))
dt.agg <- na.omit(dt.agg)
# age, income, obesity
demo_variable <- read_csv("demo_variable.csv")
d_demo <- demo_variable[,c(2,6,8,13)]
colnames(d_demo) = c('location_name','average_age','ave_household_income','obesity')
dt.agg <- join(dt.agg,d_demo,by='location_name')
# race white 
race <- read_csv("race.csv")
race <- race[,c(1,2)]
colnames(race) <- c('location_name','race_%_white')
dt.agg <- join(dt.agg,race,by='location_name')
# race hispanic -- 2019
hispanic <- read_csv("hispanic.csv")
colnames(hispanic)[1] <- "location_name"
colnames(hispanic)[2] <- "race_%_hispanic"
dt.agg <- join(dt.agg, hispanic, by='location_name')
# region
geo <- race
geo$geography <- geo$location_name
library(car)
geo$geography <- recode(geo$geography,"c('Maine', 'Massachusetts', 'Rhode Island','Connecticut', 'New Hampshire', 'Vermont', 'New York', 'Pennsylvania', 'New Jersey', 'Delaware', 'Maryland') = 'Northeast';
c('West Virginia','Virginia', 'Kentucky', 'Tennessee', 'North Carolina', 'South Carolina', 'Georgia', 'Alabama', 'Mississippi', 'Arkansas', 'Louisiana', 'Florida') = 'Southeast';
c('Ohio', 'Indiana', 'Michigan', 'Illinois', 'Missouri', 'Wisconsin', 'Minnesota', 'Iowa', 'Kansas', 'Nebraska', 'South Dakota', 'North Dakota') = 'Midwest';
c('Texas', 'Oklahoma', 'New Mexico', 'Arizona') = 'Southwest';
c('Colorado', 'Wyoming', 'Montana', 'Idaho','District of Columbia', 'Washington', 'Oregon', 'Utah', 'Nevada', 'California', 'Alaska', 'Hawaii') = 'West'")
geo <- geo[,-2]
dt.agg <- join(dt.agg,geo,by='location_name')
# density -- 2019
density <- read_csv("density.csv")
View(density)
dt.agg <- join(dt.agg,density, by = 'location_name')
# two more targets
dt.agg$case_or_death_inc <- ifelse(grepl('case',dt.agg$target),0,1)
dt.agg$weeks_ahead <- substr(dt.agg$target, 1, 10)
# vaccine (time and state)
## vaccine cumulative 
state_name <- read_csv("State_name.csv")
state_name <- state_name[,c(1,3)]
state_name = state_name[1:51,]
state_name = state_name[-9,]
vaccine <- read_csv("vaccine_cum.csv")
vaccine <- join(state_name, vaccine, by='id')
vaccine <- vaccine[,-1]
str(vaccine)
vaccine$phase <- format(as.yearmon(vaccine$date, "%Y-%m"), "%Y%m")
vaccine = vaccine[,-2]
colnames(vaccine) = c('location_name','cum_vaccine','phase')
vaccine_Nov <- state_name[,-1]
vaccine_Nov$cum_vaccine = 0
vaccine_Nov$phase = "202011"
colnames(vaccine_Nov)[1] = 'location_name'
vaccine = rbind(vaccine_Nov,vaccine)
## vaccine monthly
vac_change <- vaccine %>%
  group_by(location_name) %>%
  mutate(
    vac_monthly = ( cum_vaccine - lag(cum_vaccine))
  )
## vaccine % change
vac_change[,4] <- lapply(vac_change[,4], function(x) replace(x, !is.finite(x), 100))
vac_cha <- vac_change %>%
  group_by(location_name) %>%
  mutate(
    vac_per_change = (vac_monthly - lag(vac_monthly))/lag(vac_monthly)
  )
vac_cha <- vac_cha[-c(1:50),]
dt.agg = join(dt.agg,vac_cha,by=c('location_name','phase'))
dt.agg[,22:24] <- lapply(dt.agg[,22:24], function(x) replace(x, !is.finite(x), 0))
# life expectancy 2021
life_expectancy <- read_csv("life expectancy.csv")
dt.agg <- join(dt.agg,life_expectancy,by='location_name')
colnames(dt.agg)[25] = 'life_expectancy'
# unemployment (state and time)
un_rate <- read_csv("un_rate.csv")
un_rate = un_rate[,-1]
un_rate2 = un_rate[grepl('202103',un_rate$phase),]
un_rate2$phase = '202104'
un_rate = rbind(un_rate,un_rate2)
un_ra <- un_rate %>%
  group_by(location_name) %>%
  mutate(
    unemploy_per_cha = (unemployment_rate - lag(unemployment_rate))/lag(unemployment_rate)
  )
dt.agg = join(dt.agg,un_ra, by = c('location_name','phase'))
# GDP
GDP <- read_csv("GDP.csv")
dt.agg$phase = as.numeric(dt.agg$phase)
dt.agg$quarter = ifelse(dt.agg$phase < as.numeric(202010),
                         'Q3',
                         ifelse(dt.agg$phase < as.numeric(202101),'Q4','Q1'))
dt.agg$phase = as.character(dt.agg$phase) 
dt.agg = join(dt.agg,GDP,by=c('location_name','quarter'))
# education and median income
edu_income <- read_csv("edu_income.csv")
edu_income$Median_house_income = substring(edu_income$Median_house_income,2,
                                           nchar(edu_income$Median_house_income))
edu_income$Median_house_income = gsub(",", "",edu_income$Median_house_income)
edu_income$Median_house_income = as.numeric(edu_income$Median_house_income)
dt.agg = join(dt.agg,edu_income, by='location_name')
# healthcare
healthcare <- read_csv("healthcare.csv")
dt.agg = join(dt.agg,healthcare,by='location_name')
# moblity
moblity <- read_csv("moblity.csv")
moblity = moblity[,-1]
moblity <- moblity %>%
  group_by(location_name) %>%
  mutate(
    stayhome_per_cha = (stayhome_popu - lag(stayhome_popu))/lag(stayhome_popu)
  )
moblity <- moblity %>%
  group_by(location_name) %>%
  mutate(
    trip_per_cha = (trip_number - lag(trip_number))/lag(trip_number)
  )
dt.agg = join(dt.agg,moblity, by = c('location_name','phase'))
# other changes
## delete unnecessary targets
table(dt.agg$target)
dt.agg <- dt.agg[!grepl('12',dt.agg$target),]
## delete unnecessary phases
table(dt.agg$phase,dt.agg$model_type)
dt.agg <- dt.agg[!grepl('06',dt.agg$phase),]
# store dt.agg
dt.store = dt.agg
dt.agg = dt.store
# change percent * 100
dt.agg = na.omit(dt.agg)
dt.agg$Gov_res_change = dt.agg$Gov_res_change * 100
dt.agg$con_mea_change = dt.agg$con_mea_change * 100
dt.agg$eco_sup_change =dt.agg$eco_sup_change * 100
dt.agg$`race_%_white` = dt.agg$`race_%_white` * 100
dt.agg$vac_per_change = dt.agg$vac_per_change * 100
dt.agg$unemploy_per_cha = dt.agg$unemploy_per_cha * 100
dt.agg$uninsured_per = dt.agg$uninsured_per * 100
dt.agg$stayhome_per_cha = dt.agg$stayhome_per_cha * 100
dt.agg$trip_per_cha = dt.agg$trip_per_cha *100
# random or fixed
# random if interested in the underlying population: model_type-phase is random;
# random if is a small part of the population: model_type-phase is random；
# random if predict unobserved groups: model_type-phase is random
# random effect increase bias and reduce variance: model_type-state-target-phase
# random effect if units or observations per unit is small: model_type-phase
# correlation between the unit effects and the independent variable
# fixed effects
fixed.effects=c("state_population","government_response","containment_measure",
                "economic_support","Gov_res_change","con_mea_change",
                "eco_sup_change","average_age",
                "obesity","race_%_white","race_%_hispanic","density",
                "cum_vaccine","vac_monthly","vac_per_change","life_expectancy",
                "unemployment_rate","unemploy_per_cha", "GDP_per_change",
                "Bachelor_rate","Median_house_income","uninsured_per",
                "stayhome_popu","trip_number","stayhome_per_cha", "trip_per_cha"
                )
# region sum to 0
dt.agg$geography <- as.factor(dt.agg$geography)
geography.matrix <- model.matrix(~geography-1,dt.agg)
#
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# Sample function is useful but buggy - if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# data list
data.list = list(Y = dt.agg$std.relative_rmse,
                 n = dim(dt.agg)[1],
                 type_id = as.integer(as.factor(dt.agg$model_type)),
                 N_type = nlevels(as.factor(dt.agg$model_type)),
                 state_id = as.integer(as.factor(dt.agg$location_name)),
                 N_state = nlevels(as.factor(dt.agg$location_name)),
                 target_id = as.integer(as.factor(dt.agg$target)),
                 N_target = nlevels(as.factor(dt.agg$target)),
                 phase_id = as.integer(as.factor(dt.agg$phase)),
                 N_phase = nlevels(as.factor(dt.agg$phase)),
                 
                 incidence_id = as.integer(as.factor(dt.agg$case_or_death_inc)),
                 N_incidence = nlevels(as.factor(dt.agg$case_or_death_inc)),
                 
                 week_id = as.integer(as.factor(dt.agg$weeks_ahead)),
                 N_week = nlevels(as.factor(dt.agg$weeks_ahead)),
                 
                 
                 model_target_id = as.integer(as.factor(paste(dt.agg$model_type,dt.agg$target))),
                 N_model_target = nlevels(as.factor(paste(dt.agg$model_type, dt.agg$target))),
               
                 X = scale(cbind(dt.agg[,c(fixed.effects),with=FALSE])),
                 G = scale(cbind(geography.matrix)),
                 p = dim(cbind(dt.agg[,c(fixed.effects),with=FALSE]))[2],
                 K = dim(cbind(geography.matrix))[2],
                 prior_scale = sd(dt.agg$std.relative_rmse,na.rm=TRUE)
)

# Weakly informative prior distribution for J < 5; Noninformative prior distributions for
# J >5; AVoid using inverse gamma
# half-t distributions
model.code= "
data {
                      int<lower = 1> n; // total number of observations
                  real<lower = 0> Y[n]; // model average rmse 
            
                  real<lower = 0> prior_scale; // empirical sampling sd

            
                      int<lower = 1> p; // number of covariates in design matrix 
                        matrix[n, p] X; // design matrix
                        
                 int<lower = 1> K; // number of covariates in design matrix 
                        matrix[n, K] G; // design matrix
                        
                int<lower = 1> N_state; // number of states
            int<lower = 1> state_id[n]; // state id
            
                int<lower = 1> N_type; // number of model-types
            int<lower = 1> type_id[n]; // type id
            
               int<lower = 1> N_target; // number of target-types
           int<lower = 1> target_id[n]; // target id
            
                int<lower = 1> N_phase; // number of phases of pandemic
            int<lower = 1> phase_id[n]; // phase id
            
            int<lower = 1> N_incidence; // number of case or death of pandemic
            int<lower = 1> incidence_id[n]; // incidence id
            
            int<lower = 1> N_week; // number of weeks ahead of pandemic
            int<lower = 1> week_id[n]; // week id
            
            int<lower = 1> N_model_target; // number of phases of pandemic
            int<lower = 1> model_target_id[n]; // phase id
             
          
             
}

parameters {
                     real alpha; // baseline error rate
                   
                 vector[p] beta; // fixed-effect covariates
            vector[K-1] beta_raw; // fixed-effect covariates
              
      vector[N_state] eta_state; // coefficient on state random effects 
    real<lower = 0> sigma_state; // sd of state random effect
     
        vector[N_type] eta_type; // coefficient on type random effects 
     real<lower = 0> sigma_type; // sd of type random effect
    
    vector[N_target] eta_target; // coefficient on target random effects 
    real<lower = 0> sigma_target; // sd of trarget random effect
    
       vector[N_phase] eta_phase; // coefficient on phase random effects 
     real<lower = 0> sigma_phase; // sd of phase random effect
     
     vector[N_model_target] eta_model_target; // coefficient on phase random effects 
     real<lower = 0> sigma_model_target; // sd of phase random effect
     
        vector[N_incidence] eta_incidence; // coefficient on phase random effects 
     real<lower = 0> sigma_incidence; // sd of phase random effect
     
      vector[N_week] eta_week; // coefficient on phase random effects 
     real<lower = 0> sigma_week; // sd of phase random effect
     
           real<lower = 0> sigma; // sampling standard deviation
}

  
transformed parameters{
  vector[n] mu; // regression mean
  vector[K] beta_G = append_row(beta_raw, -sum(beta_raw));
  vector[n] res; 
  // linear function of the logit-scale propensity to be a recruit
  mu = alpha + 
       X * beta + G * beta_G +
       eta_state[state_id]*sigma_state + 
       eta_type[type_id]*sigma_type + 
       eta_target[target_id]*sigma_target + 
       eta_phase[phase_id]*sigma_phase +
       eta_model_target[model_target_id]*sigma_model_target+
       eta_incidence[incidence_id]*sigma_incidence +
       eta_week[week_id]*sigma_week;
       
  res = Y[n] - mu;
}

model {
  // // // Fixed Effects
      alpha ~ normal(0,1); // vague non-informative priors
 beta ~ normal(0,1);
 
 beta_raw ~ normal(0,1);
 
  // // // Random Effects  
       eta_state ~ normal(0,1);
    sigma_state ~ normal(0,1);
  
      eta_type ~ normal(0,1);
   sigma_type ~ normal(0,1);
  
    eta_target ~ normal(0,1);
 sigma_target ~ normal(0,1);
  
     eta_phase ~ normal(0,1);
  sigma_phase ~ normal(0,1);
  
  eta_model_target ~ normal(0,1);
  sigma_model_target ~ normal(0,1);

    eta_incidence ~ normal(0,1);
  sigma_incidence ~ normal(0,1);
  
   eta_week ~ normal(0,1);
  sigma_week ~ normal(0,1);
  
  // // // Likelihood

  Y ~ normal(mu,sigma);
  
  // // // Prior on sampling standard deviation
  
  sigma ~ normal(0,1);

}
"
pars = c('alpha','beta','beta_G',
         'eta_state','sigma_state',
         'eta_type','sigma_type',
         'eta_target','sigma_target',
         'eta_phase','sigma_phase', 'eta_model_target','sigma_model_target',
         'eta_incidence','sigma_incidence',
         'eta_week','sigma_week',
          'mu','sigma','res')
#ta_state ~N(0,sogma_state)
# stea_state = N(0,1) * sigma_state

#Temporal Effect
temporal_scale ~ normal(0,1); 
for (t in 2:T) {
  delta[t] ~normal(delta[t-1],1);
}
sum(delta) ~ normal(0, 0.01 * T); # constraint so we can write likelihood for rw(1).

stan.model.fit <- stan(model_code = model.code, 
                       data = data.list, 
                       iter = 400,
                       warmup =100,
                       thin = 2,
                       pars = pars,
                       cores =2,
                       chains = 2,
                       control = list(max_treedepth =10),
                       verbose = TRUE)

save(stan.model.fit,file = 'stan.model.fit.RData',compress = TRUE)
plot(stan.model.fit$residual) 
model.params = extract(stan.model.fit,  
                       pars = pars)
plot(stan.model.fit,pars = c('alpha'))
summary_fit <- summary(stan.model.fit)$summary
View(summary_fit)
summary(stan.model.fit)[grep('lp',names(summary(stan.model.fit))),]
# plot residual
mu_mean <- summary_fit$mean
mu_mean <- mu_mean[148:60848]
residual <- dt.agg$std.relative_rmse - mu_mean
re <- lm(residual~dt.agg$forecast_end_date)
plot(re)
summary_fit
print(stan.model.fit, pars = 'eta_model_state')
# # # Convergence Diagnostics
pdf(file = 'convergence.pdf',width =5,height =5)
plot(summary_fit$summary[,"Rhat"],pch = 0,
     ylim = c(min(min(summary_fit$summary[,"Rhat"]),0.85),max(max(summary_fit$summary[,"Rhat"]),1.15)),
     bty = "n",ylab = "Rhat",xlab = 'Index of Parameters',main = 'Convergence Diagnostics')
abline(h = 1.1,col= 'red',lty = 2)

points(x = grep('lp__',rownames(summary_fit$summary)),
       y = summary_fit$summary[grep('lp__',rownames(summary_fit$summary)),"Rhat"],
       pch = 15,col = 'blue'
)
dev.off()

hist(model.params$beta[,1])
p_beta1_negative = (sum(model.params$beta[,1]<0))/dim(model.params$beta)[1]
mean(model.params$beta[,1])

dev.off()
par(mfrow = c(2,3))
hist(model.params $eta_type[,1],main = levels(as.factor(dt.agg$model_type))[1],xlim = c(min(model.params $eta_type),max(model.params $eta_type)))
abline(v = mean(model.params $eta_type[,1]),lwd = 2,lty = 2)
hist(model.params $eta_type[,2],main = levels(as.factor(dt.agg$model_type))[2],xlim = c(min(model.params $eta_type),max(model.params $eta_type)))
abline(v = mean(model.params $eta_type[,2]),lwd = 2,lty = 2)
hist(model.params $eta_type[,3],main = levels(as.factor(dt.agg$model_type))[3],xlim = c(min(model.params $eta_type),max(model.params $eta_type)))
abline(v = mean(model.params $eta_type[,3]),lwd = 2,lty = 2)
hist(model.params $eta_type[,4],main = levels(as.factor(dt.agg$model_type))[4],xlim = c(min(model.params $eta_type),max(model.params $eta_type)))
abline(v = mean(model.params $eta_type[,4]),lwd = 2,lty = 2)
hist(model.params $eta_type[,5],main = levels(as.factor(dt.agg$model_type))[5],xlim = c(min(model.params $eta_type),max(model.params $eta_type)))
abline(v = mean(model.params $eta_type[,5]),lwd = 2,lty = 2)

# get a shapefile Map for states
library(sf)
library(geodist)
library(spdep)
library(malariaAtlas)
library(usmap)
library(ggplot2)
state_abb <- read.csv("state_abb.xlsx")
View(state_abb)
str(summary_fit)
summary_fit <- as.data.frame(summary_fit)
state_mean <- summary_fit %>% slice(33:82)
state_mean <- state_mean[,-c(2:10)]
state_mean <- as.data.frame(state_mean)
state_mean <- cbind(state_mean,state_abb)
plot_usmap(data = state_mean, regions="states",values = "state_mean",
           labels = TRUE) +
  labs(title = "Residual State-Effects on Forecasting Accuracy",
       size = "Magnitude") +
  scale_fill_gradient(low="darkgreen", high="red",
                      name = "effect size (SDs from the mean)")+
  theme(legend.position = "right")
# boxplot for states
state_name <- levels(as.factor(dt.agg$location_name))
state <-model.params$eta_state
colnames(state) <- state_name
s_mean<-as.data.frame(t(apply((state),MARGIN=2,FUN=mean)))
state <- rbind(state,s_mean)
state <- state[c(order(state[nrow(state),]))]
state <- state[-301,]
par(cex.axis=0.7)
boxplot(state,main="Residual State-Effects on Forecasting Accuracy",las=3,
        ylab="effect size (SDs from the mean)",col = "lightgreen")
abline(h = 0,col = 'black',lwd = 1)

# boxplot for target and month
library(gridExtra)
library(ggplot2)
library(plotrix)
library(ggalt)
# interaction of targets
x_name <- c('1 wk case','1 wk death','2 wk case','2 wk death','3 wk case','3 wk death',
       '4 wk case','4 wk death')
death_name <- c('1 wk death','2 wk death','3 wk death',
                '4 wk death')
target <-model.params$eta_target
colnames(target) <- x_name
target <- as.data.frame(target)
boxplot(target,main="Weeks-Ahead and Incidence Interaction Effects on Forecasting Accuracy",
        risk=0.01,
        xlab="interaction", ylab="effect size (SDs from the mean)",
      col=ifelse(colnames(target)== '1 wk death','lightgreen',
                 ifelse(colnames(target)=='2 wk death','lightgreen',
                        ifelse(colnames(target)=='3 wk death','lightgreen',
                             ifelse(colnames(target)=='4 wk death','lightgreen','lightblue')))))
abline(h = 0,col = 'black',lwd = 1)
# incidenct case 0 or death 1
inc <-model.params$eta_incidence
colnames(inc) <- c('case','death')
inc <- as.data.frame(inc)
boxplot(inc,main="Incident Case and Death Effects on Forecasting Accuracy",
        risk=0.01,
        xlab="incident case or death", ylab="effect size (SDs from the mean)",
        col=ifelse(colnames(inc)== 'case','lightblue',
                   'lightgreen'))
abline(h = 0,col = 'black',lwd = 1)
# week ahead
weeka <-model.params$eta_week
colnames(weeka) <- c('1 wk ahead','2 wk ahead','3 wk ahead',
                     '4 wk ahead')
weeka <- as.data.frame(weeka)
boxplot(weeka,main="Weeks-Ahead Effects on Forecasting Accuracy",
        risk=0.01,
        xlab="weeks ahead", ylab="effect size (SDs from the mean)",
        col = "lightgreen")
abline(h = 0,col = 'black',lwd = 1)
# target total effect
# 1 week case
T_1case = as.data.frame(weeka$`1 wk ahead` + inc$case + target$`1 wk case`)
T_2case = as.data.frame(weeka$`2 wk ahead` + inc$case + target$`2 wk case`)
T_3case = as.data.frame(weeka$`3 wk ahead` + inc$case + target$`3 wk case`)
T_4case = as.data.frame(weeka$`4 wk ahead` + inc$case + target$`4 wk case`)
T_1death = as.data.frame(weeka$`1 wk ahead` + inc$death + target$`1 wk death`)
T_2death = as.data.frame(weeka$`2 wk ahead` + inc$death + target$`2 wk death`)
T_3death = as.data.frame(weeka$`3 wk ahead` + inc$death + target$`3 wk death`)
T_4death = as.data.frame(weeka$`4 wk ahead` + inc$death + target$`4 wk death`)
T_effect = cbind(T_1case,T_1death,T_2case,T_2death,T_3case,T_3death,T_4case,T_4death)
colnames(T_effect) = x_name
boxplot(T_effect,main="Target Effects on Forecasting Accuracy",
        risk=0.01,
        xlab="target", ylab="effect size (SDs from the mean)",
        col=ifelse(colnames(target)== '1 wk death','lightgreen',
                   ifelse(colnames(target)=='2 wk death','lightgreen',
                          ifelse(colnames(target)=='3 wk death','lightgreen',
                                 ifelse(colnames(target)=='4 wk death','lightgreen','lightblue')))))
abline(h = 0,col = 'black',lwd = 1)
# spline for month
month_data <- model.params$eta_phase
m1 <- data.frame(month_data[,1])
m2 <- data.frame(month_data[,2])
m3 <- data.frame(month_data[,3])
m4 <- data.frame(month_data[,4])
m5 <- data.frame(month_data[,5])
m6 <- data.frame(month_data[,6])
m7 <- data.frame(month_data[,7])
m8 <- data.frame(month_data[,8])
m9 <- data.frame(month_data[,9])
m10 <- data.frame(month_data[,10])
colnames(m1) <- '1'
colnames(m2) <- '1'
colnames(m3) <- '1'
colnames(m4) <- '1'
colnames(m5) <- '1'
colnames(m6) <- '1'
colnames(m7) <- '1'
colnames(m8) <- '1'
colnames(m9) <- '1'
colnames(m10) <- '1'
mon <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
colnames(mon) <- 'value'
mon$month[1:300] <- '2020-07'
mon$month[301:600] <- '2020-08'
mon$month[601:900] <- '2020-9'
mon$month[901:1200] <- '2020-10'
mon$month[1201:1500] <- '2020-11'
mon$month[1501:1800] <- '2020-12'
mon$month[1801:2100] <- '2021-01'
mon$month[2101:2400] <- '2021-02'
mon$month[2401:2700] <- '2021-03'
mon$month[2701:3000] <- '2021-04'
str(mon)
mon$month <- as.factor(mon$month)
mon$month <- as.numeric(mon$month)
# boxplot
month <- c('2020-07','2020-08','2020-09','2020-10','2020-11','2020-12',
           '2021-01','2021-02','2021-03','2021-04')
colnames(month_data) <- month
boxplot(month_data,main="Evolution of Relative Forecasting Error Over Time", 
        xlab="month", ylab="effect size (SDs from the mean)",col = "lightgreen")
abline(h = 0,col = 'black',lwd = 1)
smooth = predict(lm( mon$value~mon$month),se.fit = T)
j = order(mon$month)
lines(x = mon$month[j],y= smooth$fit[j],col = 'red',lwd = 2,xpd = FALSE)
lines(x = mon$month[j],y= smooth$fit[j] + 25*smooth$se.fit[j],col = 'red',lwd = 2,lty = 2,xpd = FALSE)
lines(x = mon$month[j],y= smooth$fit[j] - 25*smooth$se.fit[j],col = 'red',lwd = 2,lty = 2,xpd = FALSE)
# spline line
library(splines2)
plot(mon)
sp_d <- cbind(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6),
              c(0.84,0.5,1.19,0.94,0.6,1.28,-0.28,-0.55,0.03,
                0.06,-0.21,0.32,-0.6,-0.89,-0.29,-1.1,-1.47,-0.73))
colnames(sp_d) <- c('Month','Effect_size')
sp_d <- as.data.frame(sp_d)
sp_d$Month <- as.numeric(sp_d$Month)
p <- ggplot(sp_d, aes(Month,Effect_size)) + 
  geom_point()
p + geom_smooth(method = "loess")
# state-level predictors
sta_name=c("state_population","average_age", "obesity","%_white",
          "%_hispanic","population_density", "life_expectancy",
          "bachelor_rate","Median_house_income","%_uninsured")
sta_eff <- model.params$beta[,c(1,8,9,10,11,12,16,20,21,22)]
colnames(sta_eff) <- sta_name
par(cex.axis=0.7)
boxplot(sta_eff,main="State-level Predictor Effects on Relative Forecasting Error",
         ylab="effect size (SDs from the mean)",col = "lightgreen"
        )
abline(h = 0,col = 'black',lwd = 1)
# time and state level predictors
time_name=c("govern_response","contain_measure",
                "eco_support","govern_change","contain_change", "eco_change",
                "vaccine_cum","vaccine_monthly","vaccine_change",
                "unemploy_rate","unemp_change", "GDP_change",
                "stayhome_popu","trip_number","stayhomechange", "trip_change"
)
time_eff = model.params$beta[,c(2:7,13:15,17:19,23:26)]
colnames(time_eff) = time_name
par(cex.axis=0.6)
boxplot(time_eff,main="State-time-level Predictor Effects on Relative Forecasting Error",
        ylab="effect size (SDs from the mean)",col = "lightgreen",las=3
)
abline(h = 0,col = 'black',lwd = 1)
# calcualte percent above 0
percent = data.table()
m = 10
for(s in 1:m){
  percent = cbind(percent,sum(sta_eff[s] < 0)/ 300)
}
percent1 = t(percent)
time_eff = as.data.frame(time_eff)
percent = data.table()
k = 16
for(s in 1:k){
  percent = cbind(percent,sum(time_eff[s] < 0)/ 300)
}
percent = t(percent)
# geography
geo_data <- model.params$beta_G
geo_data <- as.data.frame(geo_data)
table(dt.agg$geography)
geo_name <- c('Midwest','Northeast','Southeast','Southwest','West')
colnames(geo_data) <- geo_name
boxplot(geo_data,main="Residual Region-Effects on Forecasting Accuracy",
        xlab="region", ylab="effect size (SDs from the mean)",col = "lightgreen")
abline(h = 0,col = 'black',lwd = 1)
# type
type_data <- model.params$eta_type
type_name <- c('Ensemble','ML','SEIR','SIR','TimeS')
colnames(type_data) <- type_name
boxplot(type_data,main="Model Effects on Relative Forecasting Error",
        xlab="model", ylab="effect size (SDs from the mean)",col = "lightgreen")
abline(h = 0,col = 'black',lwd = 1)
# rankings
table(dt.agg$model_type)
Ensemble <- model.params$eta_type[,1]
ML <- model.params$eta_type[,2]
SEIR <- model.params$eta_type[,3]
SIR <- model.params$eta_type[,4]
TimeS <- model.params$eta_type[,5]

rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(Ensemble[s], ML[s], SEIR[s], SIR[s],TimeS[s]))  )
}
rank.object  = t(rank.object )

prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))
prop.table(table(rank.object[,5]))
EnsembleRank <- data.frame(prop.table(table(rank.object[,1])))
MLRank <- data.frame(prop.table(table(rank.object[,2])))
SEIRRank <- data.frame(prop.table(table(rank.object[,3])))
SIRRank <- data.frame(prop.table(table(rank.object[,4])))
TimeSRank <- data.frame(prop.table(table(rank.object[,5])))
Rank <- cbind(EnsembleRank[,2],MLRank[,2],SEIRRank[,2],SIRRank[,2],TimeSRank[,2])
colnames(Rank) <- c('Ensemble','ML','SEIR','SIR','TimeS')
par(mfrow=c(2,3))
par(cex.axis=1.2)
barplot(Rank[1,],main = "Rank 1",xlab = "Model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(Rank[2,],main = "Rank 2",xlab = "Model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(Rank[3,],main = "Rank 3",xlab = "Model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(Rank[4,],main = "Rank 4",xlab = "Model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(Rank[5,],main = "Rank 5",xlab = "Model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
# ranking for model_target 
## 1wk ahead case
case_1wk <- model.params$eta_model_target[,c(1,9,17,25)]
type_case <- type_data[,1:4]
case_wk1 <- case_1wk + type_case + target$`1 wk case`

case_Ensemble <- case_wk1[,1]
case_ML<- case_wk1[,2]
case_SEIR <- case_wk1[,3]
case_SIR <- case_wk1[,4]
rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(case_Ensemble[s], case_ML[s], case_SEIR[s], case_SIR[s]))  )
}
rank.object  = t(rank.object )
prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))

case_1 <- data.frame(prop.table(table(rank.object[,1])))
case_2 <- data.frame(prop.table(table(rank.object[,2])))
case_3 <- data.frame(prop.table(table(rank.object[,3])))
case_4 <- data.frame(prop.table(table(rank.object[,4])))
case_Rank <- cbind(case_1[,2],case_2[,2],case_3[,2],case_4[,2])
colnames(case_Rank) <- c('Ensemble','ML','SEIR','SIR')
dev.off()
par(mfrow=c(2,2))
par(cex.axis=1.2)
barplot(case_Rank[1,],main = "Rank 1",xlab = "model",ylab = "probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[2,],main = "Rank 2",xlab = "model",ylab = "probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[3,],main = "Rank 3",xlab = "model",ylab = "probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[4,],main = "Rank 4",xlab = "model",ylab = "probability",col = "lightgreen",ylim = c(0,1))
# 2 week ahead case
levels(as.factor(paste(dt.agg$model_type,dt.agg$target)))
case_2wk <- model.params$eta_model_target[,c(3,11,19,27)]
case_wk2 <- case_2wk + type_case + target$`2 wk case`
hist(case_2wk[,1],col = 'red')
hist(case_2wk[,2],col = 'blue',add = T)
hist(case_2wk[,3],col = 'yellow',add = T)
hist(case_2wk[,4],col = 'green',add = T)
case_Ensemble <- case_wk2[,1]
case_ML <- case_wk2[,2]
case_SEIR <- case_wk2[,3]
case_SIR <- case_wk2[,4]
rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(case_Ensemble[s], case_ML[s], case_SEIR[s], case_SIR[s]))  )
}
rank.object  = t(rank.object )
prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))
case_1 <- data.frame(prop.table(table(rank.object[,1])))
case_2 <- data.frame(prop.table(table(rank.object[,2])))
case_3 <- data.frame(prop.table(table(rank.object[,3])))
case_4 <- data.frame(prop.table(table(rank.object[,4])))
case_Rank <- cbind(case_1[,2],case_2[,2],case_3[,2],case_4[,2])
colnames(case_Rank) <- c('Ensemble','ML','SEIR','SIR')
par(mfrow=c(2,2))
par(cex.axis=1.2)
barplot(case_Rank[1,],main = "Rank 1",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[2,],main = "Rank 2",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[3,],main = "Rank 3",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[4,],main = "Rank 4",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
# 3 week ahead case
levels(as.factor(paste(dt.agg$model_type,dt.agg$target)))
case_2wk <- model.params$eta_model_target[,c(5,13,21,29)]
case_2wk <- case_2wk + type_case + target$`3 wk case`
hist(case_2wk[,1],col = 'red')
hist(case_2wk[,2],col = 'blue',add = T)
hist(case_2wk[,3],col = 'yellow',add = T)
hist(case_2wk[,4],col = 'green',add = T)
case_Ensemble <- case_2wk[,1]
case_ML <- case_2wk[,2]
case_SEIR <- case_2wk[,3]
case_SIR <- case_2wk[,4]
rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(case_Ensemble[s], case_ML[s], case_SEIR[s], case_SIR[s]))  )
}
rank.object  = t(rank.object )
prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))
case_1 <- data.frame(prop.table(table(rank.object[,1])))
case_2 <- data.frame(prop.table(table(rank.object[,2])))
case_3 <- data.frame(prop.table(table(rank.object[,3])))
case_4 <- data.frame(prop.table(table(rank.object[,4])))
case_Rank <- cbind(case_1[,2],case_2[,2],case_3[,2],case_4[,2])
colnames(case_Rank) <- c('Ensemble','ML','SEIR','SIR')
par(mfrow=c(2,2))
par(cex.axis=1.2)
barplot(case_Rank[1,],main = "Rank 1",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[2,],main = "Rank 2",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[3,],main = "Rank 3",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[4,],main = "Rank 4",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
# 4 week ahead case
levels(as.factor(paste(dt.agg$model_type,dt.agg$target)))
case_2wk <- model.params$eta_model_target[,c(7,15,23,31)]
case_2wk <- case_2wk + type_case + target$`4 wk case`
hist(case_2wk[,1],col = 'red')
hist(case_2wk[,2],col = 'blue',add = T)
hist(case_2wk[,3],col = 'yellow',add = T)
hist(case_2wk[,4],col = 'green',add = T)
case_Ensemble <- case_2wk[,1]
case_ML <- case_2wk[,2]
case_SEIR <- case_2wk[,3]
case_SIR <- case_2wk[,4]
rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(case_Ensemble[s], case_ML[s], case_SEIR[s], case_SIR[s]))  )
}
rank.object  = t(rank.object )
prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))

case_1 <- data.frame(prop.table(table(rank.object[,1])))
case_2 <- data.frame(prop.table(table(rank.object[,2])))
case_3 <- data.frame(prop.table(table(rank.object[,3])))
case_4 <- data.frame(prop.table(table(rank.object[,4])))
case_Rank <- cbind(case_1[,2],case_2[,2],case_3[,2],case_4[,2])
colnames(case_Rank) <- c('Ensemble','ML','SEIR','SIR')
par(mfrow=c(2,2))
par(cex.axis=1.2)
barplot(case_Rank[1,],main = "Rank 1",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[2,],main = "Rank 2",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[3,],main = "Rank 3",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(case_Rank[4,],main = "Rank 4",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
# death
# 1 week ahead death
levels(as.factor(paste(dt.agg$model_type,dt.agg$target)))
type_death <- model.params$eta_type
death_1wk <- model.params$eta_model_target[,c(2,10,18,26,33)]
death_1wk <- type_death+death_1wk+target$`1 wk death`
d_Ensemble <- death_1wk[,1]
d_ML <- death_1wk[,2]
d_SEIR <- death_1wk[,3]
d_SIR <- death_1wk[,4]
d_TimeS <- death_1wk[,5]
rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(d_Ensemble[s], d_ML[s], d_SEIR[s], d_SIR[s],d_TimeS[s]))  )
}
rank.object  = t(rank.object )

prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))
prop.table(table(rank.object[,5]))

death_1 <- data.frame(prop.table(table(rank.object[,1])))
death_2 <- data.frame(prop.table(table(rank.object[,2])))
death_3 <- data.frame(prop.table(table(rank.object[,3])))
death_4 <- data.frame(prop.table(table(rank.object[,4])))
death_5 <- data.frame(prop.table(table(rank.object[,5])))
death_Rank <- cbind(death_1[,2],death_2[,2],death_3[,2],death_4[,2],death_5[,2])
colnames(death_Rank) <- c('Ensemble','ML','SEIR','SIR','TimeSeries')
par(mfrow=c(2,3))
par(cex.axis=1.2)
barplot(death_Rank[1,],main = "Rank 1",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[2,],main = "Rank 2",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[3,],main = "Rank 3",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[4,],main = "Rank 4",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[5,],main = "Rank 5",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
# 2 week ahead death
levels(as.factor(paste(dt.agg$model_type,dt.agg$target)))
death_1wk <- model.params$eta_model_target[,c(4,12,20,28,34)]
death_1wk <- type_death+death_1wk+target$`2 wk death`
d_Ensemble <- death_1wk[,1]
d_ML <- death_1wk[,2]
d_SEIR <- death_1wk[,3]
d_SIR <- death_1wk[,4]
d_TimeS <- death_1wk[,5]
rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(d_Ensemble[s], d_ML[s], d_SEIR[s], d_SIR[s],d_TimeS[s]))  )
}
rank.object  = t(rank.object )

prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))
prop.table(table(rank.object[,5]))

death_1 <- data.frame(prop.table(table(rank.object[,1])))
death_2 <- data.frame(prop.table(table(rank.object[,2])))
death_3 <- data.frame(prop.table(table(rank.object[,3])))
death_4 <- data.frame(prop.table(table(rank.object[,4])))
death_5 <- data.frame(prop.table(table(rank.object[,5])))
death_Rank <- cbind(death_1[,2],death_2[,2],death_3[,2],death_4[,2],death_5[,2])
colnames(death_Rank) <- c('Ensemble','ML','SEIR','SIR','TimeSeries')
par(mfrow=c(2,3))
par(cex.axis=1.2)
barplot(death_Rank[1,],main = "Rank 1",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[2,],main = "Rank 2",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[3,],main = "Rank 3",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[4,],main = "Rank 4",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[5,],main = "Rank 5",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
# 3 week ahead death
levels(as.factor(paste(dt.agg$model_type,dt.agg$target)))
death_1wk <- model.params$eta_model_target[,c(6,14,22,30,35)]
death_1wk <- type_death+death_1wk+target$`3 wk death`
d_Ensemble <- death_1wk[,1]
d_ML <- death_1wk[,2]
d_SEIR <- death_1wk[,3]
d_SIR <- death_1wk[,4]
d_TimeS <- death_1wk[,5]
rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(d_Ensemble[s], d_ML[s], d_SEIR[s], d_SIR[s],d_TimeS[s]))  )
}
rank.object  = t(rank.object )

prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))
prop.table(table(rank.object[,5]))

death_1 <- data.frame(prop.table(table(rank.object[,1])))
death_2 <- data.frame(prop.table(table(rank.object[,2])))
death_3 <- data.frame(prop.table(table(rank.object[,3])))
death_4 <- data.frame(prop.table(table(rank.object[,4])))
death_5 <- data.frame(prop.table(table(rank.object[,5])))
death_Rank <- cbind(death_1[,2],death_2[,2],death_3[,2],death_4[,2],death_5[,2])
colnames(death_Rank) <- c('Ensemble','ML','SEIR','SIR','TimeSeries')
par(mfrow=c(2,3))
par(cex.axis=1.2)
barplot(death_Rank[1,],main = "Rank 1",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[2,],main = "Rank 2",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[3,],main = "Rank 3",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[4,],main = "Rank 4",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[5,],main = "Rank 5",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
# 4 week ahead death
levels(as.factor(paste(dt.agg$model_type,dt.agg$target)))
death_1wk <- model.params$eta_model_target[,c(8,16,24,32,36)]
death_1wk <- type_death+death_1wk+target$`4 wk death`
d_Ensemble <- death_1wk[,1]
d_ML <- death_1wk[,2]
d_SEIR <- death_1wk[,3]
d_SIR <- death_1wk[,4]
d_TimeS <- death_1wk[,5]
rank.object = data.table()
n.sims = 300
for(s in 1:n.sims){
  rank.object = cbind(rank.object,rank(
    c(d_Ensemble[s], d_ML[s], d_SEIR[s], d_SIR[s],d_TimeS[s]))  )
}
rank.object  = t(rank.object )

prop.table(table(rank.object[,1]))
prop.table(table(rank.object[,2]))
prop.table(table(rank.object[,3]))
prop.table(table(rank.object[,4]))
prop.table(table(rank.object[,5]))

death_1 <- data.frame(prop.table(table(rank.object[,1])))
death_2 <- data.frame(prop.table(table(rank.object[,2])))
death_3 <- data.frame(prop.table(table(rank.object[,3])))
death_4 <- data.frame(prop.table(table(rank.object[,4])))
death_5 <- data.frame(prop.table(table(rank.object[,5])))
death_Rank <- cbind(death_1[,2],death_2[,2],death_3[,2],death_4[,2],death_5[,2])
colnames(death_Rank) <- c('Ensemble','ML','SEIR','SIR','TimeSeries')
par(mfrow=c(2,3))
par(cex.axis=1.2)
barplot(death_Rank[1,],main = "Rank 1",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[2,],main = "Rank 2",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[3,],main = "Rank 3",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[4,],main = "Rank 4",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))
barplot(death_Rank[5,],main = "Rank 5",xlab = "model",ylab = "Probability",col = "lightgreen",ylim = c(0,1))

# check average rmse of models
models <- aggregate(std.relative_rmse~model_type,dt.agg,mean )
# plot res 
plot(res[which(state==levels(state)[s]],y[which(state==levels(state)[s]])))
##
res = model.params$mu
plot(1:3,4:6)
plot(res[which(state==levels(as.factor(dt.agg$location_name))[2])],
     y[which(state==levels(as.factor(dt.agg$location_name))[s])])
levels(as.factor(dt.agg$location_name))    
