rm(list=ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(Hmisc)

#first dataset
D<-read_excel("HARBOUR_Oxygen.xls",sheet ="Oxygen Orginal",skip=12)
D<-data.frame(D)

#rplace comma by dot
D[,2] <- gsub(",", ".", D[ , 2])

#correction for salinity
D$A3 <- D$A3*0.911198278065332 #salinity 51 psu
D$A4 <- D$A4*0.911198278065332 #salinity 51 psu

D$A5 <- D$A5*0.846962822328676 #salinity 63 psu
D$A6 <- D$A6*0.846962822328676 #salinity 63 psu

D$B1 <- D$B1*0.79248300812904 #salinity 73 psu
D$B2 <- D$B2*0.79248300812904 #salinity 73 psu

D$B3 <- D$B3*0.73168729137837 #salinity 85 psu
D$B4 <- D$B4*0.73168729137837 #salinity 85 psu

D$B5 <- D$B5*0.684582544346933 #salinity 95 psu
D$B6 <- D$B6*0.684582544346933 #salinity 95 psu
D$C1 <- D$C1*0.684582544346933 #salinity 95 psu



#Formating the dataframe
#time column
D$Time.Min.<-as.numeric(D$Time.Min.)
D$Time.Min.<-round(D$Time.Min.)
tp= dim(D)[1]#Number of time points; should be added mannualy


D1<-data.frame(Time=rep(D$Time.Min.,13),
               well=rep(colnames(D[3:15]),1, each=tp),
               #well=rep(colnames(D[3:26]),1, each=tp),
               Treatment=c(rep('psu0',tp*2), rep('psu15',tp*2),rep('psu30',tp*2),rep('psu45',tp*2),rep('psu60',tp*2),rep('psu75',tp*3)),
               #Treatment=c(rep('C',tp*2),rep('D',tp*2)),
               #DOM=c(rep('Low',tp*12),rep('High',tp*12)),
               Rep=c(rep(c(1,2),5,each=tp),rep(c(1,2,3),1,each=tp)),
               #Rep=rep(c(1,2,3),2,each=tp*2),
               Value=matrix(as.matrix(D[,3:15]),ncol=1))



##Preeliminar figure explore by eye the data and select anormal data points
D1 %>%
  ggplot(aes(x =Time, y =Value,colour=Treatment)) +geom_point(size=.3)+
  facet_wrap(~ well, ncol = 6)

d1=dim(D1)[1]#number of rows of the data

# Choose a limit to remove the exponential depletion
et=59 #in minutes, in our case the first hour
ef=700 #Final time to calculate the linear slope
D1=D1[D1$Time>et & D1$Time<ef,]
#Raw dataset
D1 %>%
  ggplot(aes(x =Time, y =Value,colour=Treatment)) +geom_point(size=.3)#+

D1$Ini<-rep(D1$Value[seq(1,dim(D1)[1],length(unique(D1$Time)))],1,each=length(unique(D1$Time)))## Select the initial point to calculate differences
D1$Resp<-(D1$Ini-D1$Value)/3 # divided by the time difference
## Oxigen consuption rates

#Visualize the linear trend
D1 %>%
  ggplot(aes(x =Time, y =Resp,colour=interaction(Treatment))) +geom_line()+geom_point(size=.5)+
  facet_wrap(~ Treatment)

#######################################################################################
df_lm <- D1 %>%
  group_by(Treatment,well,Rep,Treatment) %>%
  do(mod = lm(Resp ~ Time,data = .))

df_coef <- df_lm %>%
  do(data.frame(    Treatment = .$Treatment,    well=.$well,  Rep = .$Rep,    var = names(coef(.$mod)),
                    coef(summary(.$mod)))  )
df_coef %>%
  ggplot(aes(x=Treatment, y=Estimate,colour=Treatment)) +geom_point(size=2)+
  facet_wrap(~ var, ncol = 2, scales="free_y")#+
stat_summary(fun.y = mean, geom="point")+stat_summary(fun.data = mean_sdl, geom = "errorbar",fun.args = list(mult=1))
###
#################################
#second dataset


D<-read_excel("hARBOUR2_Oxygen.xls",sheet ="Oxygen Orginal",skip=12)
D<-data.frame(D)

#rplace comma by dot
D[,2] <- gsub(",", ".", D[ , 2])


#correction for salinity (HARBOUR_Oxygen.xls)
D$A3 <- D$A3*0.973785102198859 #salinity 41 psu
D$A3 <- D$A3*0.973785102198859 #salinity 41 psu
D$A3 <- D$A3*0.973785102198859 #salinity 41 psu
D$A4 <- D$A4*0.973785102198859 #salinity 41 psu

D$A5 <- D$A5*0.846962822328676 #salinity 63 psu
D$A6 <- D$A6*0.846962822328676 #salinity 63 psu
D$B1 <- D$B1*0.846962822328676 #salinity 63 psu
D$B2 <- D$B2*0.846962822328676 #salinity 63 psu

D$B3 <- D$B3*0.66217329645857 #salinity 100 psu
D$B4 <- D$B4*0.66217329645857 #salinity 100 psu
D$B5 <- D$B5*0.66217329645857 #salinity 100 psu
D$B6 <- D$B6*0.66217329645857 #salinity 100 psu

#Formating the dataframe
#time column
D$Time.Min.<-as.numeric(D$Time.Min.)
D$Time.Min.<-round(D$Time.Min.)
tp= dim(D)[1]#Number of time points; should be added mannualy


D1<-data.frame(Time=rep(D$Time.Min.,12),
               well=rep(colnames(D[3:14]),1, each=tp),
               #well=rep(colnames(D[3:26]),1, each=tp),
               Treatment=c(rep('psu0',tp*4), rep('psu20',tp*4),rep('psu50',tp*4)),
               #Treatment=c(rep('C',tp*2),rep('D',tp*2)),
               #DOM=c(rep('Low',tp*12),rep('High',tp*12)),
               Rep=c(rep(c(1,2,3,4),3,each=tp)),
               #Rep=rep(c(1,2,3),2,each=tp*2),
               Value=matrix(as.matrix(D[,3:14]),ncol=1))



##Preeliminar figure explore by eye the data and select anormal data points
D1=D1[D1$Time>et & D1$Time<ef,]
D1 %>%
  ggplot(aes(x =Time, y =Value,colour=Treatment)) +geom_point(size=.3)+
  facet_wrap(~ well, ncol = 6)

d1=dim(D1)[1]#number of rows of the data

# Choose a limit to remove the exponential depletion
et=119 #in minutes, in our case the first hour
ef=700 #Final time to calculate the linear slope
D1=D1[D1$Time>et & D1$Time<ef,]
#Raw dataset
D1 %>%
  ggplot(aes(x =Time, y =Value,colour=Treatment)) +geom_point(size=.3)#+

D1$Ini<-rep(D1$Value[seq(1,dim(D1)[1],length(unique(D1$Time)))],1,each=length(unique(D1$Time)))## Select the initial point to calculate differences
D1$Resp<-(D1$Ini-D1$Value)/3 # divided by the time difference
## Oxigen consuption rates

#Visualize the linear trend
D1 %>%
  ggplot(aes(x =Time, y =Resp,colour=interaction(Treatment))) +geom_line()+geom_point(size=.5)+
  facet_wrap(~ Treatment)

#######################################################################################
df_lm <- D1 %>%
  group_by(Treatment,well,Rep,Treatment) %>%
  do(mod = lm(Resp ~ Time,data = .))

df_coef <- df_lm %>%
  do(data.frame(    Treatment = .$Treatment,    well=.$well,  Rep = .$Rep,    var = names(coef(.$mod)),
                    coef(summary(.$mod)))  )
df_coef %>%
  ggplot(aes(x=Treatment, y=Estimate,colour=Treatment)) +geom_point(size=2)+
  facet_wrap(~ var, ncol = 2, scales="free_y")#+
stat_summary(fun.y = mean, geom="point")+stat_summary(fun.data = mean_sdl, geom = "errorbar",fun.args = list(mult=1))
###
