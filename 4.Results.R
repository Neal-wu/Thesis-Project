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
library(RColorBrewer)

# Sample function is useful but buggy - if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# # # load results
load(file = 'Generated Quantities/stan.model.fit.RData')
load(file = 'Generated Quantities/stan.data.list.RData')
load(file = 'Generated Quantities/stan.pars.RData')

# # # get summaries and extract params
summary_fit <- summary(stan.model.fit)
model.params = extract(stan.model.fit,pars = pars)

# # # Convergence Diagnostics - Rhat
pdf(file = 'Plots/convergence.pdf',width =5,height =5)
plot(summary_fit$summary[,"Rhat"],pch = 0,
     ylim = c(min(min(summary_fit$summary[,"Rhat"]),0.85),max(max(summary_fit$summary[,"Rhat"]),2)),
     bty = "n",ylab = "Rhat",xlab = 'index of parameters',main = 'gelman-rubin statistic')
abline(h = 1.1,col= 'red',lty = 2)

points(x = grep('lp__',rownames(summary_fit$summary)),
       y = summary_fit$summary[grep('lp__',rownames(summary_fit$summary)),"Rhat"],
       pch = 15,col = 'blue'
)
legend("topright",lty = c(NA,NA,2),col = c("black","blue","red"),pch = c(0,15,NA),legend = c("parameter","likelihood","convergence"))
dev.off()

# # # posterior predictive plot
pdf(file = 'Plots/posterior_predictive_check.pdf',width =5,height =5)
density = density(data.list$Y,xlab = "")
plot(density,
     xlim = c(min(c(data.list$Y,as.numeric(model.params$Y_rep))),
              max(c(data.list$Y,as.numeric(model.params$Y_rep)))),
     ylim = c(0,0.5),bty="n",
     xlab = "log absolute error per 10k pop.",
     main = "posterior predictive check",
     col = NA)
for(i in 1:dim(t(model.params$Y_rep))[2]){
  par(new=TRUE)
  d <- density(t(model.params$Y_rep)[,i],xlab = "")
  plot(d,
       xlim = c(min(c(data.list$Y,as.numeric(model.params$Y_rep))),
                max(c(data.list$Y,as.numeric(model.params$Y_rep)))),
       main = "",xaxt = "n",yaxt = "n",ylab = "",xlab = "",
       ylim = c(0,0.5),
       col = adjustcolor('grey',0.35),bty = "n")
}
par(new=TRUE)
plot(density,
     xlim = c(min(c(data.list$Y,as.numeric(model.params$Y_rep))),
              max(c(data.list$Y,as.numeric(model.params$Y_rep)))),
     ylim = c(0,0.5),xlab = "",bty = "n",xaxt = "n",yaxt = "n",
     main = "",
     col = 'black')
legend("topright",lty = 1,col = c("black","grey"),legend = c("observed","generated"))
dev.off()

# # # get a shapefile Map for states-effect means
library(sf)
library(sp)
library(geodist)
library(spdep)
library(surveillance)
library(rgdal)
get_shp <- function(url, folder = "shape") {
  tmp.dir <- tempfile(fileext = ".zip")
  download.file(url, destfile = tmp.dir)
  unzip(tmp.dir, exdir = folder)
  list.files(folder, full.names = TRUE)
}
url <- "https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_state_20m.zip"
get_shp(url, "Auxiliary Data/shapefile")
states <- readOGR("Auxiliary Data/shapefile")
# leave out puerto rico and DC - they are not in our model
states = states[-which(states@data$NAME=="Puerto Rico"),]
# sync up order of states by strat frame
states = states[match(data.list$state_name_labels,states$NAME),]
states$eta_state = summary_fit$summary[,"mean"][grepl("eta_state\\[",rownames(summary_fit$summary)) & !grepl("beta",rownames(summary_fit$summary))]
states@bbox[which(rownames(states@bbox)=="x"),] = c(-179.1743, -59.7739)
pdf(file = 'Plots/statelevel_randomeffects_map.pdf',width =15,height =10)
pal = colorRampPalette(colors = c("white","red"))
pal(16)
spplot(states, 
       zcol = "eta_state", 
       sp.layout = list(layout.labels(states, labels = list(font=1, labels="STUSPS"))),
       col.regions = pal(16), 
       scales = list(draw = TRUE),
       col = 'black',
       main = 'state-level random effects')
dev.off()
# # # get densities for state-evel random effects 
# # # make violin plots of the state-levelr random effects
library(vioplot)
eta.state.table = as.data.table(model.params$eta_state)
colnames(eta.state.table) = data.list$state_name_labels
order.mean= order(apply(eta.state.table,2,mean))
eta.state.table = eta.state.table[,order.mean,with=FALSE]
eta.state.table = melt(eta.state.table)
eta.state.table[,negative.significance:=((sum(value<0)/length(value))>0.8),by = "variable"]
eta.state.table[,negative.significance.high:=((sum(value<0)/length(value))>0.9),by = "variable"]
eta.state.table[,positive.significance:=((sum(value>0)/length(value))>0.8),by = "variable"]
eta.state.table[,positive.significance.high:=((sum(value>0)/length(value))>0.9),by = "variable"]

pdf(file = 'Plots/statelevel_randomeffects.pdf',width =17.5,height =5)
par(oma = c(5,0,0,0))
vioplot(eta.state.table$value ~ eta.state.table$variable, 
        plotCentre = "line",
        #side = "right",
        col = ifelse(unique(eta.state.table[,c("variable","negative.significance.high")])$negative.significance,"blue",
              ifelse(unique(eta.state.table[,c("variable","negative.significance")])$negative.significance,"skyblue",
              ifelse(unique(eta.state.table[,c("variable","positive.significance.high")])$positive.significance.high,"red",
              ifelse(unique(eta.state.table[,c("variable","positive.significance")])$positive.significance,"lightcoral",
                     "grey")))),# Color of the area
        rectCol = "grey",       # Color of the rectangle
        lineCol = "black",     # Color of the line
        border = 'black',      # Color of the border of the violin
        pchMed = NA,           # Pch symbol for the median
        xlab = "", 
        ylab = "log absolute error per 10k pop.",
        xaxt = "n",
        cex = 0.5,horizontal = FALSE,
        main = 'distribution of state-level random effects')
abline(h = 0,lwd = 1,col = 'black',lty = 2)
axis(side = 1,at = 1:data.list$N_state,
     labels = data.list$state_name_labels[order.mean],las = 2)
legend("topleft",
       legend = c("Pr(eta>0) > 0.9","Pr(eta>0) > 0.8","Pr(eta<0) > 0.8","Pr(eta<0) > 0.9"),
       pch = 15,
       col = c("red","lightcoral","skyblue","blue"),
       cex = 0.8,
       bty="n")
dev.off()

# # # plot area-level predictors 
area_level = c("intercept",colnames(data.list$X_state))
beta.state.table = as.data.table(cbind(model.params$alpha,model.params$beta_state))
colnames(beta.state.table) = area_level 
order.mean= order(apply(beta.state.table,2,mean))
beta.state.table = beta.state.table[,order.mean,with=FALSE]
beta.state.table = melt(beta.state.table)
beta.state.table[,negative.significance:=((sum(value<0)/length(value))>0.8),by = "variable"]
beta.state.table[,negative.significance.high:=((sum(value<0)/length(value))>0.9),by = "variable"]
beta.state.table[,positive.significance:=((sum(value>0)/length(value))>0.8),by = "variable"]
beta.state.table[,positive.significance.high:=((sum(value>0)/length(value))>0.9),by = "variable"]


pdf(file = 'Plots/X.area_coefs.pdf',width =10,height =7.5)
par(oma = c(10,0,0,0))
vioplot(beta.state.table$value ~ beta.state.table$variable, 
        plotCentre = "line",
        #side = "right",
        col = ifelse(unique(beta.state.table[,c("variable","negative.significance.high")])$negative.significance,"blue",
                     ifelse(unique(beta.state.table[,c("variable","negative.significance")])$negative.significance,"skyblue",
                            ifelse(unique(beta.state.table[,c("variable","positive.significance.high")])$positive.significance.high,"red",
                                   ifelse(unique(beta.state.table[,c("variable","positive.significance")])$positive.significance,"lightcoral",
                                          "grey")))),# Color of the area
        rectCol = "grey",       # Color of the rectangle
        lineCol = "black",     # Color of the line
        border = 'black',      # Color of the border of the violin
        pchMed = NA,           # Pch symbol for the median
        xlab = "", 
        ylab = "log absolute error per 10k pop.",
        xaxt = "n",
        cex = 0.5,horizontal = FALSE,
        main = 'distribution of state-level covariate fixed effects')
abline(h = 0,lwd = 1,col = 'black',lty = 2)
axis(side = 1,at = 1:(data.list$p_state+1),
     labels = c("intercept",data.list$X_state_labels)[order.mean],las = 2)
legend("topright",
       legend = c("Pr(beta>0) > 0.9","Pr(beta>0) > 0.8","Pr(beta<0) > 0.8","Pr(beta<0) > 0.9"),
       pch = 15,
       col = c("red","lightcoral","skyblue","blue"),
       cex = 0.8,
       bty="n")
dev.off()


# # # plot time-level predictors 
area_time_level = colnames(data.list$X_state_time)
beta.state.time.table = as.data.table(model.params$beta_state_time)
colnames(beta.state.time.table) = area_time_levellevel 
order.mean= order(apply(beta.state.time.table,2,mean))
beta.state.time.table = beta.state.time.table[,order.mean,with=FALSE]
beta.state.time.table = melt(beta.state.time.table)
beta.state.time.table[,negative.significance:=((sum(value<0)/length(value))>0.8),by = "variable"]
beta.state.time.table[,negative.significance.high:=((sum(value<0)/length(value))>0.9),by = "variable"]
beta.state.time.table[,positive.significance:=((sum(value>0)/length(value))>0.8),by = "variable"]
beta.state.time.table[,positive.significance.high:=((sum(value>0)/length(value))>0.9),by = "variable"]


pdf(file = 'Plots/X.area.time_coefs.pdf',width =10,height =7.5)
par(oma = c(12.5,0,0,0))
vioplot(beta.state.time.table$value ~ beta.state.time.table$variable, 
        plotCentre = "line",
        #side = "right",
        col = ifelse(unique(beta.state.time.table[,c("variable","negative.significance.high")])$negative.significance,"blue",
                     ifelse(unique(beta.state.time.table[,c("variable","negative.significance")])$negative.significance,"skyblue",
                            ifelse(unique(beta.state.time.table[,c("variable","positive.significance.high")])$positive.significance.high,"red",
                                   ifelse(unique(beta.state.time.table[,c("variable","positive.significance")])$positive.significance,"lightcoral",
                                          "grey")))),# Color of the area
        rectCol = "grey",       # Color of the rectangle
        lineCol = "black",     # Color of the line
        border = 'black',      # Color of the border of the violin
        pchMed = NA,           # Pch symbol for the median
        xlab = "", 
        ylab = "log absolute error per 10k pop.",
        xaxt = "n",
        cex = 0.5,horizontal = FALSE,
        main = 'distribution of state-level covariate fixed effects')
abline(h = 0,lwd = 1,col = 'black',lty = 2)
axis(side = 1,at = 1:(data.list$p_state_time),
     labels = data.list$X_state_time_labels[order.mean],las = 2)
legend("topright",
       legend = c("Pr(beta>0) > 0.9","Pr(beta>0) > 0.8","Pr(beta<0) > 0.8","Pr(beta<0) > 0.9"),
       pch = 15,
       col = c("red","lightcoral","skyblue","blue"),
       cex = 0.8,
       bty="n")
dev.off()

# # # plot time RW effect
eta.forecast.week.table = as.data.table(model.params$eta_forecast_week)
colnames(eta.forecast.week.table) = as.character(unlist(data.list$forecast_week_labels))
eta.forecast.week.table = cbind(sims = 1:dim(eta.forecast.week.table)[1],eta.forecast.week.table)
eta.forecast.week.table = melt(eta.forecast.week.table,id.vars = 'sims')

pdf(file = 'Plots/weekssince1stforecast_randomeffects.pdf',width =15,height =10)
plot(as.numeric(as.character(unlist(eta.forecast.week.table$variable[which(eta.forecast.week.table$sims==1)]))),
     eta.forecast.week.table$value[which(eta.forecast.week.table$sims==1)],
     pch = NA,
     xlab = 'weeks since 1st forecast',xaxt = "n",ylab = 'log absolute error per 10k pop.',ylim = c(min(eta.forecast.week.table$value),max(eta.forecast.week.table$value)),
     bty = "n",
     main = 'smoothed temporal trend')
lines( as.numeric(as.character(unlist(eta.forecast.week.table$variable[which(eta.forecast.week.table$sims==1)]))), 
       eta.forecast.week.table[,lapply(.SD,function(x){quantile(x,probs = 0.025)}),by = "variable",.SDcols = 'value']$value,lty = 3)
lines( as.numeric(as.character(unlist(eta.forecast.week.table$variable[which(eta.forecast.week.table$sims==1)]))), 
       eta.forecast.week.table[,lapply(.SD,function(x){quantile(x,probs = 0.975)}),by = "variable",.SDcols = 'value']$value,lty = 3)
polygon(c(as.numeric(as.character(unlist(eta.forecast.week.table$variable[which(eta.forecast.week.table$sims==1)]))), 
          rev(as.numeric(as.character(unlist(eta.forecast.week.table$variable[which(eta.forecast.week.table$sims==1)]))))), 
        c(eta.forecast.week.table[,lapply(.SD,function(x){quantile(x,probs = 0.975)}),by = "variable",.SDcols = 'value']$value,
          rev(eta.forecast.week.table[,lapply(.SD,function(x){quantile(x,probs = 0.025)}),by = "variable",.SDcols = 'value']$value)),
        col = "lightgrey", border = NA)
lines( as.numeric(as.character(unlist(eta.forecast.week.table$variable[which(eta.forecast.week.table$sims==1)]))), 
       eta.forecast.week.table[,lapply(.SD,mean),by = "variable",.SDcols = 'value']$value)
abline(h = 0,lty = 2)
axis(side = 1,at =data.list$forecast_week_labels)
dev.off()



# # # plot marginal effects for geography level and temporal level
pdf(file = 'Plots/scale_variables_coefs.pdf',width =10,height =5)
par(mfrow = c(1,2))
# geo 
plot(density(model.params$eta_geo_type[,1]),
     xlim = c(min(model.params$eta_geo_type)-1,max(model.params$eta_geo_type)+1),
     main = "geography level of forecast",
     ylim = c(0,1),
     col = NA,xlab = "log absolute error per 10k pop.")
for(i in 1:dim(model.params$eta_geo_type)[2]){
  d <- density(model.params$eta_geo_type[,i])
  polygon(d,
          main = '',xaxt = "n",yaxt = "n",ylab = "",xlab = "",border = NA,
          col = adjustcolor(c("violet","orange")[i],0.75))
}
abline(v = 0,lty = 1)
abline(v = colMeans(model.params$eta_geo_type),
       lty = 2,col = adjustcolor(c("purple","red"),0.5),
       lwd = 2)
text(x = colMeans(model.params$eta_geo_type),y = 0.8,
     labels = data.list$geo_type_labels,srt = 60,cex = 0.85,pos = 1,
     col=c("purple","red"))
legend("topright",
       legend = c(paste("Pr(eta_", data.list$geo_type_labels[1],">0) =",
                        round(mean(model.params$eta_geo_type[,1]>0),3))),
       cex = 0.8,bty = "n"
       )
# temporal 
plot(density(model.params$eta_temporal_resolution[,1]),
     xlim = c(min(model.params$eta_temporal_resolution)-1,max(model.params$eta_temporal_resolution)+1),
     main = "temporal level of forecast",
     ylim = c(0,1),
     col = NA,xlab = "log absolute error per 10k pop.")
for(i in 1:dim(model.params$eta_temporal_resolution)[2]){
  d <- density(model.params$eta_temporal_resolution[,i])
  polygon(d,
          main = '',xaxt = "n",yaxt = "n",ylab = "",xlab = "",border = NA,
          col = adjustcolor(c("violet","orange")[i],0.75))
}
abline(v = 0,lty = 1)
abline(v = colMeans(model.params$eta_temporal_resolution),
       lty = 2,col = adjustcolor(c("purple","red"),0.5),
       lwd = 2)
text(x = colMeans(model.params$eta_temporal_resolution),y = 0.8,
     labels = data.list$temporal_resolution_labels,srt = 60,cex = 0.85,pos = 1,
     col=c("purple","red"))
legend("topright",
       legend = c(paste("Pr(eta_", data.list$temporal_resolution_labels[1],">0) =",
                        round(mean(model.params$eta_temporal_resolution[,1]>0),3))),
       cex = 0.8,bty = "n"
)
dev.off()


# # # plot total effects for targets
target.total = 
expand.grid(model_type_labels = data.list$model_type_labels,
            target_variable_labels = data.list$target_variable_labels,
            horizon_weeks_labels = data.list$horizon_weeks_labels)
target.total$target_variable_AND_horizon_weeks = paste(target.total$target_variable_labels,target.total$horizon_weeks_labels,sep=" ")
target.total$model_type_AND_horizon_weeks = paste(target.total$model_type_labels,target.total$horizon_weeks_labels,sep=" ")
target.total$model_type_AND_target_variable = paste(target.total$model_type_labels,target.total$target_variable_labels,sep=" ")
target.total$model_type_AND_target_variable_AND_horizon_weeks = paste(target.total$model_type_labels,target.total$target_variable_labels,target.total$horizon_weeks_labels,sep=" ")
for(s in 1:dim(model.params$eta_model_type)[1]){
temp.1 = model.params$eta_model_type[s,match(target.total$model_type_labels,data.list$model_type_labels)] 
temp.2 = model.params$eta_target_variable[s,match(target.total$target_variable_labels,data.list$target_variable_labels)] 
temp.3 = model.params$eta_horizon_weeks[s,match(target.total$horizon_weeks_labels,data.list$horizon_weeks_labels)] 
temp.4 = model.params$eta_target_variable_AND_horizon_weeks[s,match(target.total$target_variable_AND_horizon_weeks,data.list$target_variable_AND_horizon_weeks_labels)] 
temp.4[which(is.na(temp.4 ))] = rnorm(n = sum(is.na(temp.4 )),mean = 0,sd = sd(as.numeric(model.params$eta_target_variable_AND_horizon_weeks)))
temp.5 = model.params$eta_model_type_AND_horizon_weeks[s,match(target.total$model_type_AND_horizon_weeks,data.list$model_type_AND_horizon_weeks_labels)] 
temp.6 = model.params$eta_model_type_AND_target_variable[s,match(target.total$model_type_AND_target_variable,data.list$model_type_AND_target_variable_labels)] 
temp.6[which(is.na(temp.6 ))] = rnorm(n = sum(is.na(temp.6 )),mean = 0,sd = sd(as.numeric(model.params$eta_model_type_AND_target_variable)))
temp.7 = model.params$eta_model_type_AND_target_variable_AND_horizon_weeks[s,match(target.total$model_type_AND_target_variable_AND_horizon_weeks,data.list$model_type_AND_target_variable_AND_horizon_weeks_labels)]
temp.7[which(is.na(temp.7 ))] = rnorm(n = sum(is.na(temp.7 )),mean = 0,sd = sd(as.numeric(model.params$eta_model_type_AND_target_variable_AND_horizon_weeks)))
target.total = cbind(target.total,temp.1+temp.2+temp.3+temp.4+temp.5+temp.6+temp.7)
colnames(target.total )[-c(1:(dim(target.total)[2]-1))] = paste("total.effect.sim.",s,sep="")
}


for(k in 1:data.list$N_target_variable){
pdf(file = paste('Plots/model_type_AND_target_variable_AND_horizon_weeks.',data.list$target_variable_labels[k],'.pdf',sep=""),width =12.5,height =7.5)
par(oma = c(0,0,2,0),mfrow = c(2,4))
for(j in 1:nlevels(target.total$model_type_labels)){
  plot(y = target.total[,grep("total.effect.sim",colnames(target.total))][,1],
       x = target.total$horizon_weeks_labels,
       pch = NA,
       ylim = c(min(target.total[,grep("total.effect.sim",colnames(target.total))]),max(target.total[,grep("total.effect.sim",colnames(target.total))])),
       ylab = 'log absolute error per 10k pop.',xlab = 'weeks-ahead',
       main = levels(target.total$model_type_labels)[j])
temp = 
  target.total[which(target.total$model_type_labels==levels(target.total$model_type_labels)[j] &
                     target.total$target_variable_labels==data.list$target_variable_labels[k]),
               grep("total.effect.sim",colnames(target.total))]
lines( as.numeric(as.character(unlist(1:dim(temp)[1])))-1, 
       apply(temp,1,function(x){mean(x)}),
       lty = 1,col = j)
lines( as.numeric(as.character(unlist(1:dim(temp)[1])))-1, 
       apply(temp,1,function(x){quantile(x,probs = 0.025)}),
       lty = 3,col = j)
lines( as.numeric(as.character(unlist(1:dim(temp)[1])))-1, 
       apply(temp,1,function(x){quantile(x,probs = 0.975)}),
       lty = 3,col = j)
polygon(c(as.numeric(as.character(unlist(1:dim(temp)[1])))-1, 
          rev(as.numeric(as.character(unlist(1:dim(temp)[1])))-1)), 
        c(apply(temp,1,function(x){quantile(x,probs = 0.975)}),
          rev(apply(temp,1,function(x){quantile(x,probs = 0.025)}))),
        col = adjustcolor(j,0.1), border = NA)
abline (h = 0,lty = 2)
}
title(main = paste('total effects of model-type over weeks-ahead by target:',data.list$target_variable_labels[k],sep=''),outer = T)
dev.off()

# calculate model rank per target and week-ahead 
pdf(file = paste('Plots/model_type_AND_target_variable_AND_horizon_weeks.rank.',data.list$target_variable_labels[k],'.pdf',sep=""),width =12.5,height =5)
par(mfrow = c(1,5),oma = c(10,0,0,0))
for(w in 1:data.list$N_horizon_weeks){
rank.temp = apply(
target.total[which(target.total$target_variable_labels==data.list$target_variable_labels[k] &
                     target.total$horizon_weeks_labels==data.list$horizon_weeks_labels[w]),
             grep("total.effect.sim",colnames(target.total))],2,rank)
rank.temp = apply(rank.temp,1,function(x){sum(x==1)/length(x)})
names(rank.temp) = target.total$model_type_labels[which(target.total$target_variable_labels==data.list$target_variable_labels[k] & 
                                                          target.total$horizon_weeks_labels==data.list$horizon_weeks_labels[w])]
barplot(rank.temp,
       xlab = '',ylab = 'Pr(rank = 1)',
       main = paste("horizon_weeks",data.list$horizon_weeks_labels[w],"-",data.list$target_variable_labels[k] ),
       col = 'lightgreen',ylim = c(0,1),las = 2)
}
dev.off()
}

# # # Observations per model-type, hrizon-week, target combination 
table(data.list$model_type_labels[data.list$model_type_id],
      data.list$horizon_weeks_labels[data.list$horizon_weeks_id],
      data.list$target_variable_labels[data.list$target_variable_id])

# # # Marginal Effects of model-type 
pdf(file = paste('Plots/model_type_marginals.pdf',sep=""),width =12.5,height =7.5)
par(mfrow = c(2,4),oma = c(0,0,2,0))
for(i in 1:dim(model.params$eta_model_type)[2]){
  plot(density(model.params$eta_model_type),xlab = 'log absolute error per 10k pop.',col = NA,ylim = c(0,1),
       main = data.list$model_type_labels[i])
  d <- density(model.params$eta_model_type[,i])
  polygon(d,
          main = '',xaxt = "n",yaxt = "n",ylab = "",xlab = "",border = NA,
          col = adjustcolor(i,0.35))
  abline(v = 0,lty = 1)
  abline(v = colMeans(model.params$eta_model_type)[i],
         lty = 2,col = adjustcolor(1:dim(model.params$eta_model_type)[2],0.75)[i],
         lwd = 2)
}
title(main = paste('model-type marginal effects'),outer = T)
dev.off()

