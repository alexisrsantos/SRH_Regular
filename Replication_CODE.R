# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

#Load Libraries
library(srvyr) #To calculate the weighted proportion
library(tidyverse)
library(ggplot2)
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("XML file from IPUMS")
data <- read_ipums_micro(ddi)

data<-subset(data,AGE>17) #Eliminates people under 18
data<-subset(data,HISPYN==2) #Eliminate not Hispanics

data<-subset(data,HEALTH<6) #Eliminates missing values from Health variable
data$poorfair<-ifelse(data$HEALTH>3,1,0) #Recodes as poor/fair SRH

#Eliminate language missing
data<-subset(data,INTERVLANG<3) #Eliminate Language Missing Values or ambiguous categories

#Recode Language of Interview into factor dummy
data$SPANISH<-ifelse(data$INTERVLANG==2,"Spanish","English")

#Remove place of birth missing
data<-subset(data,USBORN<96)  

#Recode place of birth into factor dummy
data$POBIRTH<-ifelse(data$USBORN==20,"U.S. Born","Not U.S. Born")

options(survey.lonely.psu="adjust") #Establish what to do with Lonely PSUs in Survey Design

table(data$SPANISH) #Produces a table of Language of Interview (line 23)
table(data$POBIRTH) #Produces a table of Place of birth (line 29)

table(data$poorfair,data$SPANISH) #Produces a crosstab of poor/fair SRH and language

#Descriptives
table <- data  %>%
  as_survey(weights = c(PERWEIGHT)) %>%
  group_by(YEAR,SPANISH) %>% 
  summarize(rate=survey_mean(poorfair)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

table #Show table

trends <- data  %>%
  as_survey(weights = PERWEIGHT,
            strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH,POBIRTH) %>% 
  summarize(rate=survey_mean(poorfair)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Initial Figure
ggplot(trends, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid((~POBIRTH))+
  labs(title ="Poor/Fair Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

setwd("Work Directory")
#ggsave("Figure1.eps") #This saves as an EPS file required for submission

####Now focusing on Not U.S. Born (new dataset is data2)
### Focus due to patterns found in the data in Figure 1
data2<-subset(data,POBIRTH=="Not U.S. Born")
data2$FAIR<-ifelse(data2$HEALTH==4,1,0)
data2$POOR<-ifelse(data2$HEALTH==5,1,0)

#Create descriptives by year and language of interview
fig2a <- data2  %>%
  as_survey(weights = PERWEIGHT,strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH) %>% 
  summarize(rate=survey_mean(POOR)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Panel A
A<-ggplot(fig2a, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  theme_bw()+
  labs(title ="Poor or Mala Health by Language of Interview",
       subtitle="Analysis of non-U.S. Born Hispanic adults",
  x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  expand_limits(x=c(1997,2018), y=c(0, 20))+
  #scale_color_grey() +scale_fill_grey() +
  scale_y_continuous(ylim(0,20),breaks = seq(from = 0, to = 20, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

#Show Panel A
A

#Create descriptives by year and language of interview
fig2b <- data2  %>%
  as_survey(weights = PERWEIGHT,strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH) %>% 
  summarize(rate=survey_mean(FAIR)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Create Panel B
B<-ggplot(fig2b, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  theme_bw()+
  labs(title ="Fair or Regular Health by Language of Interview",
       subtitle="Analysis of non-U.S. Born Hispanic adults",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  expand_limits(x=c(1997,2018), y=c(0, 20))+
  #scale_color_grey() +scale_fill_grey() +
  scale_y_continuous(ylim(0,20),breaks = seq(from = 0, to = 20, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

#Show Panel B
B


library(ggpubr) #This library to join figures

#Create joint figure
figure2 <- ggarrange(A, B,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "bottom")

#Show me Joint Figuere
figure2

#ggsave("Figure2.eps") #Saves as an EPS - required for submission


#Descriptive statistics derived from Figure 1
trends1a<-subset(trends,YEAR==1997)
trends1a
trends1b<-subset(trends,YEAR==2018)
trends1b

#Descriptive statistics derived from Figure 2
trends2a<-subset(trends2,YEAR==1997)
trends2a
trends2b<-subset(trends2,YEAR==2018)
trends2b

#Poor for 1997 and 2018
fig2a1<-subset(fig2a,YEAR==1997)
fig2a1
fig2a2<-subset(fig2a,YEAR==2018)
fig2a2

#Regular for 1997 - 2018
fig2b1<-subset(fig2b,YEAR==1997)
fig2b1
fig2b2<-subset(fig2b,YEAR==2018)
fig2b2

# Here concludes the analysis included in the main text

#############################################
##### SUPPLEMENTAL ANALYSES              ####       
##### NOT INCLUDED IN THE PAPER          ####
#############################################
#Analysis by Place of Birth
#Only US Born by Sex
data2$MALE<-ifelse(data2$SEX==1,"Male","Female")
trends2 <- data2  %>%
  as_survey(weights = PERWEIGHT,strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH, MALE) %>% 
  summarize(rate=survey_mean(poorfair)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Produce Figure 2
ggplot(trends2, aes(y = rate, x = YEAR,color=as.factor(MALE)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(MALE), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid(~SPANISH)+
  labs(title ="Poor/Fair Self-Reported Health among not U.S. Born Hispanics by Language of Interview and Sex, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

########## with Male Female
data3<-data2
data3$FAIR<-ifelse(data3$HEALTH==4,1,0)
data3$POOR<-ifelse(data3$HEALTH==5,1,0)
data3$SRH<-ifelse(data3$HEALTH>3,1,0)

trends3 <- data3  %>%
  as_survey(weights = c(PERWEIGHT)) %>%
  group_by(YEAR,SPANISH, MALE) %>% 
  summarize(srh_rate=survey_mean(SRH)*100,
            poor_rate=survey_mean(POOR)*100,
            se_fair = sd(srh_rate)/sqrt(n()),
            lower_SRH = srh_rate - qt(0.975,n()-1) * se_fair,
            upper_SRH = srh_rate + qt(0.975,n()-1) * se_fair,
            se_poor = sd(poor_rate)/sqrt(n()),
            lower_poor= poor_rate - qt(0.975,n()-1) * se_poor,
            upper_poor = poor_rate + qt(0.975,n()-1) * se_poor)

#This figure does not contain the 95% Confidence Intervals
ggplot(data=trends3) +
  geom_line(aes(x=YEAR, y=srh_rate,color="Poor/Fair Mala/Regular")) +
  geom_line(aes(x=YEAR, y=poor_rate,color="Poor/Mala"))+
  facet_grid(MALE~SPANISH)+theme_bw()+
  labs(title ="Poor/Fair Self-Reported Health among not U.S. Born Hispanics by Language of Interview and Sex, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

#Use this for Figure 2
ggplot(data=trends3)+
geom_line(aes(x=YEAR, y=srh_rate,linetype="SRH: Poor/Fair or Mala/Regular",fill="Fair/Regular")) +
geom_ribbon(aes(x=YEAR,ymin = lower_SRH, ymax = upper_SRH,alpha = 0.10))+
geom_line(aes(x=YEAR, y=poor_rate,linetype="SRH: Poor or Mala",fill="Poor/Mala"))+
geom_ribbon(aes(x=YEAR,ymin = lower_poor, ymax = upper_poor, alpha = 0.10))+
facet_grid(MALE~SPANISH)+theme_bw()+
labs(title ="Poor/Fair Self-Reported Health among not U.S. Born Hispanics by Language of Interview and Sex, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "Percent Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend


ggplot(trends, aes(y = rate, x = YEAR,color=as.factor(POBIRTH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(POBIRTH), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid((~SPANISH))+
  labs(title ="Poor/Fair Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

#Use this for Figure 1 in the Paper 
ggplot(trends, aes(y = rate, x = YEAR,fill=as.factor(POBIRTH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(POBIRTH), alpha = 0.10))+
  theme_bw()+
  facet_grid((~SPANISH))+
  labs(title ="Poor/Fair Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE)+ #To turn off shape legend
  scale_fill_manual(values=c("black", "grey"), name="fill")


#################################
####  ROBUSTNESS CHECKS
#################################
ddi <- read_ipums_ddi("XML file from IPUMS")
dataB <- read_ipums_micro(ddi)

dataB<-subset(dataB,AGE>17)
dataB<-subset(dataB,HISPYN==2) #Eliminate not Hispanics

dataB<-subset(dataB,HEALTH<6)
dataB$poorfair<-ifelse(dataB$HEALTH>3,1,0)

#Eliminate language missing
#dataB<-subset(dataB,INTERVLANG<4) #Eliminate Language Missing Values

#Recode into String Dummy
dataB$SPANISH<-ifelse(dataB$INTERVLANG>1,"Spanish","English")

#Remove place of birth missing
dataB<-subset(dataB,USBORN<96) 
dataB$POBIRTH<-ifelse(dataB$USBORN==20,"U.S. Born","Not U.S. Born")

options(survey.lonely.psu="adjust")

tableB <- dataB  %>%
  as_survey(weights = c(PERWEIGHT)) %>%
  group_by(YEAR,SPANISH) %>% 
  summarize(rate=survey_mean(poorfair)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

tableB

trendsB <- dataB  %>%
  as_survey(weights = PERWEIGHT,
            strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH,POBIRTH) %>% 
  summarize(rate=survey_mean(poorfair)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Initial Figure
ggplot(trendsB, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid((~POBIRTH))+
  labs(title ="Poor/Fair Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

###Supplement Figure 2
data2b<-subset(dataB,POBIRTH=="Not U.S. Born")
data2b$FAIR<-ifelse(data2b$HEALTH==4,1,0)
data2b$POOR<-ifelse(data2b$HEALTH==5,1,0)

fig2a <- data2  %>%
  as_survey(weights = PERWEIGHT,strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH) %>% 
  summarize(rate=survey_mean(POOR)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

A<-ggplot(fig2a, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  theme_bw()+
  labs(title ="Poor or Mala Health by Language of Interview",
       subtitle="Analysis of non-U.S. Born Hispanic adults",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  expand_limits(x=c(1997,2018), y=c(0, 20))+
  #scale_color_grey() +scale_fill_grey() +
  scale_y_continuous(ylim(0,20),breaks = seq(from = 0, to = 20, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

A

fig2b <- data2  %>%
  as_survey(weights = PERWEIGHT,strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH) %>% 
  summarize(rate=survey_mean(FAIR)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

B<-ggplot(fig2b, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  theme_bw()+
  labs(title ="Fair or Regular Health by Language of Interview",
       subtitle="Analysis of non-U.S. Born Hispanic adults",
       x = "Year ", y= "% Reporting Poor/Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  expand_limits(x=c(1997,2018), y=c(0, 20))+
  #scale_color_grey() +scale_fill_grey() +
  scale_y_continuous(ylim(0,20),breaks = seq(from = 0, to = 20, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

B

library(ggpubr) #This library to join figures

figuresup2 <- ggarrange(A, B,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend = "bottom")
figuresup2





#################################
####  SUPPLEMENT
#################################
ddi <- read_ipums_ddi("XML FILE FROM IPUMS")
dataB <- read_ipums_micro(ddi)

dataB<-subset(dataB,AGE>17)
dataB<-subset(dataB,HISPYN==2) #Eliminate not Hispanics

dataB<-subset(dataB,HEALTH<6)
dataB$poor<-ifelse(dataB$HEALTH==5,1,0)
dataB$fair<-ifelse(dataB$HEALTH==4,1,0)
dataB$good<-ifelse(dataB$HEALTH==3,1,0)
dataB$vgood<-ifelse(dataB$HEALTH==2,1,0)
dataB$ex<-ifelse(dataB$HEALTH==1,1,0)

#Eliminate language missing
dataB<-subset(dataB,INTERVLANG<4) #Eliminate Language Missing Values

#Recode into String Dummy
dataB$SPANISH<-ifelse(dataB$INTERVLANG>1,"Spanish","English")

#Remove place of birth missing
dataB<-subset(dataB,USBORN<96) 
dataB$POBIRTH<-ifelse(dataB$USBORN==20,"U.S. Born","Not U.S. Born")

options(survey.lonely.psu="adjust")

poor <- dataB  %>%
  as_survey(weights = c(PERWEIGHT)) %>%
  group_by(YEAR,SPANISH) %>% 
  summarize(rate=survey_mean(poor)*100,
            se = sd(poor)/sqrt(n()),
            lower = poor - qt(0.975,n()-1) * se,
            upper = poor + qt(0.975,n()-1) * se)

trendsB <- dataB  %>%
  as_survey(weights = PERWEIGHT,
            strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH,POBIRTH) %>% 
  summarize(rate=survey_mean(poor)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Reporting Poor Health
ggplot(trendsB, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid((~POBIRTH))+
  labs(title ="Poor Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Poor Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

# Reporting Fair Health
trendsB <- dataB  %>%
  as_survey(weights = PERWEIGHT,
            strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH,POBIRTH) %>% 
  summarize(rate=survey_mean(fair)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

ggplot(trendsB, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid((~POBIRTH))+
  labs(title ="Fair Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Fair Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,25),breaks = seq(from = 0, to = 25, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

#Reporting Good Health
trendsB <- dataB  %>%
  as_survey(weights = PERWEIGHT,
            strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH,POBIRTH) %>% 
  summarize(rate=survey_mean(good)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Figure 
ggplot(trendsB, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid((~POBIRTH))+
  labs(title ="Good Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Good Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,50),breaks = seq(from = 0, to = 50, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

#Reporting Very Good Health
trendsB <- dataB  %>%
  as_survey(weights = PERWEIGHT,
            strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH,POBIRTH) %>% 
  summarize(rate=survey_mean(vgood)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Figure
ggplot(trendsB, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid((~POBIRTH))+
  labs(title ="Very Good Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Very Good Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,50),breaks = seq(from = 0, to = 50, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend

#Reporting Excellent
trendsB <- dataB  %>%
  as_survey(weights = PERWEIGHT,
            strata=STRATA,
            id=PSU,nest=TRUE) %>%
  group_by(YEAR,SPANISH,POBIRTH) %>% 
  summarize(rate=survey_mean(ex)*100,
            se = sd(rate)/sqrt(n()),
            lower = rate - qt(0.975,n()-1) * se,
            upper = rate + qt(0.975,n()-1) * se)

#Figure
ggplot(trendsB, aes(y = rate, x = YEAR,color=as.factor(SPANISH)))+ 
  geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper,fill=as.factor(SPANISH), alpha = 0.10))+
  geom_point()+theme_bw()+
  facet_grid((~POBIRTH))+
  labs(title ="Excellent Self-Reported Health among Hispanics by language of interview and place of birth, 1997-2018",
       caption="Data: NHIS, accessed through IPUMS Health",
       x = "Year ", y= "% Reporting Excellent Health")+
  scale_x_continuous(breaks = seq(from = 1997, to = 2018, by = 3))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x=element_blank(),axis.title.y=element_blank())+
  scale_y_continuous(ylim(0,50),breaks = seq(from = 0, to = 50, by = 2.5))+
  guides(alpha= FALSE) #To turn off shape legend