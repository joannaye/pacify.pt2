#Final project script

##Open data set
setwd("Downloads")

install.packages('plyr')

library(plyr)

library(foreign)

install.packages("readstata13")

library(readstata13)

#Under packages tab, make sure that readstata13 is selected
mydata <- read.dta13("Joanna.dta")

##################################################################################

##Quick reference: variable names 

##EXPOSURES AND OUTCOMES
#Exposure: fent_yn_mitt, (dmhx?)
#Primary platelet function outcome: pru_0 through pru_2i 
#Secondary platelet function outcome: hr_adp_ag 
#Self reported pain during procedure: proc_pain
#Concentration of ticagrelor: thirtyminticag, onehrticag, twohrticag, fourhrticag

##DEMOGRAPHIC INFO
#Sex (0=female, 1=male): sex 
#Diabetes diagnoses (0=no, 1=yes): dmhx 
#Age: age 
#Race (0=white, 1=black, 2=other): raceN 


##################################################################################

##Clean data

##Include only patients that received PICC procedure
#Select observations 143:212
mydata1 <- mydata[143:212, ]

##Make new dataframe using just columns with main outcomes and demographic info
mydata2<-mydata1[c(1,4,9,14,17,25,26,27,28,40,50,67,77,78)]


##################################################################################

##Find demographic info

#Total number of patients
patients_total<-nrow(mydata2)

#Age of patients
age_mean<-mean(mydata2$age)
age_sd<-sd(mydata2$age)

#Age range (should be 37.9 to 84.33)
patients_age<-range(mydata2$age)

#Number of female patients (should equal 19)
patients_female<-sum(mydata2$sex)

#Percent female patients (should equal 27%)
pct_female<-(patients_female/patients_total)*100

#Number of patients in each race category (0=52, 1=6, 2=12)
attach(mydata2)
race_table<-table(143:212, raceN)
margin.table(race_table, 2)

whitepct<-(52/patients_total)*100
blackpct<-(6/patients_total)*100
otherpct<-(12/patients_total)*100

#Number of patients who received fentanyl 
patients_fent<-sum(mydata2$fent_yn_mitt)
fentpct<-(patients_fent/70)*100
nofentpct<-100-fentpct

#Number of patients with diabetes diagnosis 
patients_dm<-sum(mydata2$dmhx)
patients_nodm<-70-21
dmpct<-(patients_dm/70)*100
nodmpct<-100-dmpct

##################################################################################

##Outcome 1: Platelet function based on PRU (P2Y Reaction Unit) and ADP light transmittion aggregotomy

##Create new variable pr_pru
#Transform continuous variable pru_2i to categorical variable pr_pru
#pr_pru is platelet reactivity based on PRU measurements (low<235, high>=235)
labels_pr<-c("Low", "High")
mydata2$pr_pru<-cut(mydata2$pru_2i, c(0, 235, 400), right=FALSE, labels=labels_pr)

#Transform continuous variable hr_adp_ag to categorical variable pr_adp
mydata2$pr_adp<-cut(mydata2$hr_adp_ag, c(0, 35, 100), right=FALSE, labels=labels_pr)

#Create new variable pr_overall that combines platelet reactivity based on both PRU and ADP
mydata2<-transform(mydata2, pr_overall=ifelse(is.na(pr_adp), pr_pru, pr_adp))
mydata2$pr_overall<-factor(mydata2$pr_overall, levels=c(1,2), labels=c("Low", "High"))

#Find number of patients with high/low platelet reactivity
count(mydata2, 'pr_overall')

##Find number of patients with high/low platelet reactivity given fentanyl vs no fentanyl
#Generate two tables to see if there is difference in how fentanyl affects diabetic vs non-diabetic patients
fent_yn_dmhx_pr<-xtabs(~ fent_yn_mitt + pr_overall + dmhx, mydata2)

##Chi-square test:
summary(fent_yn_dmhx_pr)


##Data visualization: Make bar graphs comparing % high platelet reactivity at 2 hrs

library(RColorBrewer)
coul = brewer.pal(3, "Pastel1") 

#Graph 1: Non-diabetic: Find percentages of high/low platelet reactivity based on fentanyl exposure
highpr_dm0_fent0<-5/(16+5)
lowpr_dm0_fent0<-16/(16+5)
highpr_dm0_fent1<-12/(16+12)
lowpr_dm0_fent1<-16/(16+12)

prdata1 <- data.frame("No"=c(highpr_dm0_fent0*100, lowpr_dm0_fent0*100), 
                      "Yes"=c(highpr_dm0_fent1*100, lowpr_dm0_fent1*100))

prdata1_matrix <-data.matrix(prdata1, rownames.force = NA)

barplot(prdata1_matrix, 
        main="Platelet reactivity: Non-diabetic",
        xlab="Fentanyl exposure",
        ylab="Percent",
        col=coul,
        border="white")

legend("topleft",
       c("High","Low"),
       fill = coul)

#Graph 2: Diabetic: Find percentages of high/low platelet reactivity based on fentanyl exposure
highpr_dm1_fent0<-0/(14+0)
lowpr_dm1_fent0<-14/(14+0)
highpr_dm1_fent1<-3/(4+3)
lowpr_dm1_fent1<-4/(4+3)

prdata2 <- data.frame("No"=c(highpr_dm1_fent0*100, lowpr_dm1_fent0*100), 
                      "Yes"=c(highpr_dm1_fent1*100, lowpr_dm1_fent1*100))

prdata2_matrix <-data.matrix(prdata2, rownames.force = NA)

barplot(prdata2_matrix, 
        main="Platelet reactivity: Diabetic",
        xlab="Fentanyl exposure",
        ylab="Percent",
        col=coul,
        border="white")

legend("topleft",
       c("High","Low"),
       fill = coul)


##################################################################################

#Outcome 2: Ticagrelor concentration

library(ggplot2)

#Make new data frames based on diabetes/fentanyl status subgroups
  #1:diabetic/fentanyl
dm1_fent1<-subset(mydata2, dmhx!=0&fent_yn_mitt!=0)  
  #2:diabetic/no fentanyl
dm1_fent0<-subset(mydata2, dmhx!=0&fent_yn_mitt!=1) 
  #3:non-diabetic/fentanyl
dm0_fent1<-subset(mydata2, dmhx!=1&fent_yn_mitt!=0) 
  #4:non-diabetic/no fentanyl
dm0_fent0<-subset(mydata2, dmhx!=1&fent_yn_mitt!=1) 

#Find mean ticagrelor concentration at each time point
  #1:diabetic/fentanyl:
ticag30.1<-mean(dm1_fent1$thirtyminticag, na.rm = TRUE)
ticag1.1<-mean(dm1_fent1$onehrticag, na.rm = TRUE)
ticag2.1<-mean(dm1_fent1$twohrticag, na.rm = TRUE)
ticag4.1<-mean(dm1_fent1$fourhrticag, na.rm = TRUE)
  #2:diabetic/no fentanyl:
ticag30.2<-mean(dm1_fent0$thirtyminticag, na.rm = TRUE)
ticag1.2<-mean(dm1_fent0$onehrticag, na.rm = TRUE)
ticag2.2<-mean(dm1_fent0$twohrticag, na.rm = TRUE)
ticag4.2<-mean(dm1_fent0$fourhrticag, na.rm = TRUE)
  #3:non-diabetic/fentanyl:
ticag30.3<-mean(dm0_fent1$thirtyminticag, na.rm = TRUE)
ticag1.3<-mean(dm0_fent1$onehrticag, na.rm = TRUE)
ticag2.3<-mean(dm0_fent1$twohrticag, na.rm = TRUE)
ticag4.3<-mean(dm0_fent1$fourhrticag, na.rm = TRUE)
  #4:non-diabetic/no fentanyl:
ticag30.4<-mean(dm0_fent0$thirtyminticag, na.rm = TRUE)
ticag1.4<-mean(dm0_fent0$onehrticag, na.rm = TRUE)
ticag2.4<-mean(dm0_fent0$twohrticag, na.rm = TRUE)
ticag4.4<-mean(dm0_fent0$fourhrticag, na.rm = TRUE)


##T-test: Ticagrelor concentration at 2 hours

#Diabetic: fentanyl vs no fentanyl
t.test(dm1_fent1$twohrticag, dm1_fent0$twohrticag) 

#Non-diabetic: fentanyl vs no fentanyl
t.test(dm0_fent1$twohrticag, dm0_fent0$twohrticag) 


##Data visualization: Make line graphs showing how ticagrelor concentration changes over time

#Graph 3: Diabetic: Find difference in mean ticagrelor concentration over time for fentanyl vs no fentanyl

ticagdata1<-data.frame(
  Fentanyl=factor(c("Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No")),
  Time=factor(c("0.5 hr", "1 hr", "2 hr", "4 hr", "0.5 hr", "1 hr", "2 hr", "4 hr")),
  Concentration=c(ticag30.1, ticag1.1, ticag2.1, ticag4.1, ticag30.2, ticag1.2, ticag2.2, ticag4.2)
)

ggplot(data=ticagdata1, aes(x=Time, y=Concentration, group=Fentanyl, colour=Fentanyl)) +
  geom_line() +
  geom_point()+
  ggtitle("Diabetic: Ticagrelor concentration over time") + 
  theme_bw() +
  theme(legend.position=c(.1, .8))     

#Graph 4: Non-diabetic: Find difference in mean ticagrelor concentration over time for fentanyl vs no fentanyl

ticagdata2<-data.frame(
  Fentanyl=factor(c("Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No")),
  Time=factor(c("0.5 hr", "1 hr", "2 hr", "4 hr", "0.5 hr", "1 hr", "2 hr", "4 hr")),
  Concentration=c(ticag30.3, ticag1.3, ticag2.3, ticag4.3, ticag30.4, ticag1.4, ticag2.4, ticag4.4)
)

ggplot(data=ticagdata2, aes(x=Time, y=Concentration, group=Fentanyl, colour=Fentanyl)) +
  geom_line() +
  geom_point()+
  ggtitle("Non-diabetic: Ticagrelor concentration over time") + 
  theme_bw() +
  theme(legend.position=c(.1, .8))

##################################################################################

#Outcome 3: Self-reported pain

#Impute missing proc_pain data using maxnursingpainscale to substitute for missing values
mydata2<-transform(mydata2, pain_overall=ifelse(is.na(proc_pain), maxnursingpainscale, proc_pain))

#Update subgroup data frames so they include pain_overall
  #1:diabetic/fentanyl
dm1_fent1<-subset(mydata2, dmhx!=0&fent_yn_mitt!=0)  
  #2:diabetic/no fentanyl
dm1_fent0<-subset(mydata2, dmhx!=0&fent_yn_mitt!=1) 
  #3:non-diabetic/fentanyl
dm0_fent1<-subset(mydata2, dmhx!=1&fent_yn_mitt!=0) 
  #4:non-diabetic/no fentanyl
dm0_fent0<-subset(mydata2, dmhx!=1&fent_yn_mitt!=1) 


##T-test: Pain during procedure

#Diabetic: fentanyl vs no fentanyl
t.test(dm1_fent1$pain_overall, dm1_fent0$pain_overall) 

#Non-diabetic: fentanyl vs no fentanyl
t.test(dm0_fent1$pain_overall, dm0_fent0$pain_overall) 


######################### E N D ####################################


