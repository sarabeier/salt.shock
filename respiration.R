rm(list=ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(plotrix)

#################################
#SOLA 15.10.2019
setwd('/Users/sara/Documents/R-scripts/salt.shocks/salt.shock/')

D<-read_excel("SOLA_Oxygen.xls",sheet ="Oxygen Orginal",skip=12)
D<-data.frame(D)

#rplace comma by dot
D[,2] <- gsub(",", ".", D[ , 2])


#correction for salinity (HARBOUR_Oxygen.xls)
D$A3 <- D$A3*0.993381239868772 #salinity 39 psu
D$A3 <- D$A3*0.993381239868772 #salinity 39 psu
D$A3 <- D$A3*0.993381239868772 #salinity 39 psu
D$A4 <- D$A4*0.993381239868772 #salinity 39 psu
D$A5 <- D$A5*0.993381239868772 #salinity 39 psu
D$A6 <- D$A6*0.993381239868772 #salinity 39 psu

D$B1 <- D$B1*0.846962822328676 #salinity 63.5 psu
D$B2 <- D$B2*0.846962822328676 #salinity 63.5 psu
D$B3 <- D$B3*0.846962822328676 #salinity 63.5 psu
D$B4 <- D$B4*0.846962822328676 #salinity 63.5 psu
D$B5 <- D$B5*0.846962822328676 #salinity 63.5 psu
D$B6 <- D$B6*0.846962822328676 #salinity 63.5 psu

D$C1 <- D$C1*0.73168729137837 #salinity 85 psu
D$C2 <- D$C2*0.73168729137837 #salinity 85 psu
D$C3 <- D$C3*0.73168729137837 #salinity 85 psu
D$C4 <- D$C4*0.73168729137837 #salinity 85 psu
D$C5 <- D$C5*0.73168729137837 #salinity 85 psu
D$C6 <- D$C6*0.73168729137837 #salinity 85 psu

D$D1 <- D$D1*0.636242795468274 #salinity 106 psu
D$D2 <- D$D2*0.636242795468274 #salinity 106 psu
D$D3 <- D$D3*0.636242795468274 #salinity 106 psu
D$D4 <- D$D4*0.636242795468274 #salinity 106 psu
D$D5 <- D$D5*0.636242795468274 #salinity 106 psu
D$D6 <- D$D6*0.636242795468274 #salinity 106 psu


#Formating the dataframe
#time column
D$Time.Min.<-as.numeric(D$Time.Min.)
D$Time.Min.<-round(D$Time.Min.)
tp= dim(D)[1]#Number of time points; should be added mannualy

D1<-data.frame(Time=rep(D$Time.Min.,12),
               well=rep(colnames(D[3:26]),1, each=tp),
               #well=rep(colnames(D[3:26]),1, each=tp),
               Treatment=c(rep('psu0',tp*6), rep('psu20',tp*6),rep('psu40',tp*6),rep('psu60',tp*6)),
               #Treatment=c(rep('C',tp*2),rep('D',tp*2)),
               #DOM=c(rep('Low',tp*12),rep('High',tp*12)),
               Rep=c(rep(c(1,2,3,4,5,6),4,each=tp)),
               #Rep=rep(c(1,2,3),2,each=tp*2),
               Value=matrix(as.matrix(D[,3:26]),ncol=1))



##Preeliminar figure explore by eye the data and select anormal data points

#ERROR!!!!!
#D1 %>%
#  ggplot(aes(x =Time, y =Value,colour=treatment)) +geom_point(size=.3)+
#  facet_wrap(~ well, ncol = 6, scales="free_y")


D1 %>%
  ggplot(aes(x =Time, y =Value,colour=well)) +geom_point(size=.3)+
  facet_wrap(~ well, ncol = 6, scales="free_y")

D1[D1$well=='D4',] %>%
  ggplot(aes(x =Time, y =Value,colour=Treatment)) +geom_point(size=.3)


d1=dim(D1)[1]#number of rows of the data

plot(D1[D1$well=='D3',1],D1[D1$well=='D3',5])
plot(D1[D1$well=='D4',1],D1[D1$well=='D4',5])

D1<- D1[D1$well!='A1' & D1$well!='A4'& D1$well!='A5'
        & D1$well!='B1' & D1$well!='B5'
        & D1$well!='C3' & D1$well!='C4'& D1$well!='C5'
        & D1$well!='D4' & D1$well!='D5'& D1$well!='D6'& D1$well!='D1'
        ,]

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
#df_lm <- D1 %>%
#  group_by(Treatment,well,Rep,Treatment) %>%
#  do(mod = lm(Resp ~ Time,data = .))
df_lm <- D1 %>%
  group_by(Treatment,well,Rep) %>%
  do(mod = lm(Resp ~ Time,data = .))

df_coef <- df_lm %>%
  do(data.frame(    Treatment = .$Treatment,    well=.$well,  Rep = .$Rep,    var = names(coef(.$mod)),
                    coef(summary(.$mod)))  )
df_coef %>%
  ggplot(aes(x=Treatment, y=Estimate,colour=Treatment)) +geom_point(size=2)+
  facet_wrap(~ var, ncol = 2, scales="free_y")#+
stat_summary(fun.y = mean, geom="point")+stat_summary(fun.data = mean_sdl, geom = "errorbar",fun.args = list(mult=1))

df_coef.S <- df_coef[df_coef$var!='(Intercept)' &  (df_coef$Treatment=='psu40'|df_coef$Treatment=='psu60'),]
df_coef.S$site <- c('SOLA')
df_coef.S
###

#################################
#CANET 17.10.2019

D<-read_excel("Canet_Oxygen.xls",sheet ="Oxygen Orginal",skip=12)
D<-data.frame(D)

#rplace comma by dot
D[,2] <- gsub(",", ".", D[ , 2])


#correction for salinity (HARBOUR_Oxygen.xls)
D$B1 <- D$B1*0.858281811097246 #salinity 63.5 psu
D$B2 <- D$B2*0.858281811097246 #salinity 63.5 psu
D$B3 <- D$B3*0.858281811097246 #salinity 63.5 psu
D$B4 <- D$B4*0.858281811097246 #salinity 63.5 psu
D$B5 <- D$B5*0.858281811097246 #salinity 63.5 psu
D$B6 <- D$B6*0.858281811097246 #salinity 63.5 psu

D$C1 <- D$C1*0.746410937697751 #salinity 85 psu
D$C2 <- D$C2*0.746410937697751 #salinity 85 psu
D$C3 <- D$C3*0.746410937697751 #salinity 85 psu
D$C4 <- D$C4*0.746410937697751 #salinity 85 psu
D$C5 <- D$C5*0.746410937697751 #salinity 85 psu
D$C6 <- D$C6*0.746410937697751 #salinity 85 psu

D$D1 <- D$D1*0.644738335653421 #salinity 106 psu
D$D2 <- D$D2*0.644738335653421 #salinity 106 psu
D$D3 <- D$D3*0.644738335653421 #salinity 106 psu
D$D4 <- D$D4*0.644738335653421 #salinity 106 psu
D$D5 <- D$D5*0.644738335653421 #salinity 106 psu
D$D6 <- D$D6*0.644738335653421 #salinity 106 psu


#Formating the dataframe
#time column
D$Time.Min.<-as.numeric(D$Time.Min.)
D$Time.Min.<-round(D$Time.Min.)
tp= dim(D)[1]#Number of time points; should be added mannualy

D1<-data.frame(Time=rep(D$Time.Min.,12),
               well=rep(colnames(D[3:26]),1, each=tp),
               #well=rep(colnames(D[3:26]),1, each=tp),
               Treatment=c(rep('psu0',tp*6), rep('psu20',tp*6),rep('psu40',tp*6),rep('psu60',tp*6)),
               #Treatment=c(rep('C',tp*2),rep('D',tp*2)),
               #DOM=c(rep('Low',tp*12),rep('High',tp*12)),
               Rep=c(rep(c(1,2,3,4,5,6),4,each=tp)),
               #Rep=rep(c(1,2,3),2,each=tp*2),
               Value=matrix(as.matrix(D[,3:26]),ncol=1))



##Preeliminar figure explore by eye the data and select anormal data points


D1 %>%
  ggplot(aes(x =Time, y =Value,colour=Treatment)) +geom_point(size=.3)+
  facet_wrap(~ well, ncol = 6, scales="free_y")

d1=dim(D1)[1]#number of rows of the data

D1<- D1[D1$well!='A1' & D1$well!='A2'& D1$well!='A3'& D1$well!='A4'
        & D1$well!='B1' & D1$well!='B2'& D1$well!='B3'& D1$well!='B5'
        & D1$well!='C4' & D1$well!='C5'
        & D1$well!='D1' & D1$well!='D2'& D1$well!='D5'& D1$well!='D6'
        ,]

# Choose a limit to remove the exponential depletion
et=600 #in minutes, in our case the first hour
ef=1200 #Final time to calculate the linear slope
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
#df_lm <- D1 %>%
#  group_by(Treatment,well,Rep,Treatment) %>%
#  do(mod = lm(Resp ~ Time,data = .))

df_lm <- D1 %>%
  group_by(Treatment,well,Rep) %>%
  do(mod = lm(Resp ~ Time,data = .))

df_coef <- df_lm %>%
  do(data.frame(    Treatment = .$Treatment,    well=.$well,  Rep = .$Rep,    var = names(coef(.$mod)),
                    coef(summary(.$mod)))  )
df_coef %>%
  ggplot(aes(x=Treatment, y=Estimate,colour=Treatment)) +geom_point(size=2)+
  facet_wrap(~ var, ncol = 2, scales="free_y")#+
#stat_summary(fun.y = mean, geom="point")+stat_summary(fun.data = mean_sdl, geom = "errorbar",fun.args = list(mult=1))
#stat_summary(fun = mean, geom="point")+stat_summary(fun.data = mean_sdl, geom = "errorbar",fun.args = list(mult=1))

###

df_coef.C <- df_coef[df_coef$var!='(Intercept)' &  (df_coef$Treatment=='psu40'|df_coef$Treatment=='psu60'),]
df_coef.C$site <- c('Canet')

df_coef<- rbind(df_coef.S,df_coef.C)
df_coef$Treatment <- factor(df_coef$Treatment)
df_coef$resp <- df_coef$Estimate*60
df_coef$ID <- paste(df_coef$site,df_coef$Treatment, sep='_')

RES <- aggregate(resp ~ Treatment+site, data = df_coef, FUN= "mean" )
RES$sd <- tapply(df_coef$resp, df_coef$ID, sd)
RES$se <- tapply(df_coef$resp, df_coef$ID, std.error)
RES$ID <- paste(RES$site,RES$Treatment, sep='_')

RES$resp[RES$resp<=0]=0.0005 #corrects negative values to 0

#right order
RES$ID<- factor(RES$ID,levels = c("Canet_psu40","SOLA_psu40", "Canet_psu60",  "SOLA_psu60"))
#for error bars
limits <- aes(ymax =RES$resp + RES$se, ymin=RES$resp - RES$se) 

pdf("/Users/sara/Documents/DFG/Antrag/Folgeanterag/prolongationGESIFUS/resistance2.pdf",height=4, width=4)
RES%>%
  ggplot(aes(Treatment,resp,fill=site))+
  geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar(limits,width=0.25,position = position_dodge(width = 0.9))+
  theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y = "respiration", x="")+
  labs(y=parse(text=expression(paste("respiration [",mu,"M C ","h"^"-1","]"))))+
  #ggtitle('C)')+
  theme(axis.text.x = element_text( size=14), axis.text.y = element_text( size=14),
        axis.title.y = element_text( size=14),
        legend.title = element_blank(),legend.text = element_text( size=14))+
  scale_x_discrete(labels=c("+40 psu", "+60 psu"))+
  scale_fill_manual("legend", values = c("Canet" = "orange", "SOLA" = "gray25"))
dev.off()

RES15%>%
  ggplot(aes(Type,norm*100,fill=Cond))+
  geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar(limits,width=0.25,position = position_dodge(width = 0.9))+
  theme_bw()+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y = "% decrease of FDA hydrolysis rate", x="")+
  ggtitle('A)')+
  theme(axis.text.x = element_text( size=14), axis.text.y = element_text( size=14),
        axis.title.y = element_text( size=14),
        legend.title = element_blank(),legend.text = element_text( size=14))+
  scale_fill_manual("legend", values = c("C" = "gray25", "D" = "orange"))
dev.off()


scale_x_discrete(breaks=c("1","2"),
                 labels=c("+40 psu", "+60 psu"))

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
