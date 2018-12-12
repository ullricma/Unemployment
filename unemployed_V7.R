setwd("")

##which libraries to use

#install.packages("haven")
#install.packages("labelled")
#install.packages("dplyr")
library(haven)
library(labelled)
library(dplyr)

#####-------- data creation -------###########
#### read in raw data (use below commented lines only ones to create the dataset)

#raw_p <- read_dta("./bgp.dta")
#raw_pgen <- read_dta("./bgpgen.dta")
#save(raw_p, file = "raw_p.RDA")
#save(raw_pgen, file ="raw_pgen.RDA")
# raw_pequiv <- read_dta("./bgpequiv.dta")
# save(raw_pequiv, file ="raw_pequiv.RDA")
# raw_ppfad <- read_dta("./ppfad.dta")
# save(raw_ppfad, file="./raw_ppfad.RDA")
# raw_bgpkal <- read_dta("./bgpkal.dta")
# save(raw_bgpkal, file="raw_bgpkal.RDA")

load("./data/raw_p.RDA")
load("./data/raw_pgen.RDA")
load("./data/raw_pequiv.RDA")
load("./data/raw_ppfad.RDA")
load("./data/raw_bgpkal.RDA")

names(raw_pgen)

##data overview
#names(raw_p)
#str(raw_p)
#summary(raw_p$bgpsex)

##Choose important variables

data_all_p <- raw_p[,c("hhnr","persnr","hhnrakt","bghhnr","syear","sample1","bgpnr","bgppnr","bgpbirthy","bgpsex"
                  ,"bgp175","bgp0101","bgp0102","bgp0103","bgp0104","bgp0105","bgp0106","bgp0107","bgp0108","bgp0109","bgp0110","bgp0111")]

#str(data_all_p)

##Calculation Variable age from variable year of birth, first adjustment Missings

summary(data_all_p$bgpbirthy)
table(data_all_p$bgpbirthy)

data_all_p$age <- 2016-data_all_p$bgpbirthy
summary(data_all_p$age)

## Show first cases of new variable "age" and date of birth "birthy". We still see missing values, these will all be removed later.
head(data_all_p$age)
head(data_all_p$bgpbirthy)

##Check if the calculation worked. Check case 1 with case 1
2016-1950

##Generation of a new variable with age segments
## age_rec = new variable with division into life phases (lp) according to age

data_all_p$age_rec <- cut(data_all_p$age, breaks = c(0, 29, 49, 65) , labels = c("lp1", "lp2", "lp3"))
table(data_all_p$age_rec)

###
###Preparation of 2nd data set pgen_raw, i.e. selection of all variables important for us, the rest is unimportant (for now)
##expft$$ – Working Experience Full-Time Employment [generic]
##exppt$$ – Working Experience Part-Time Employment [generic]
##expue$$ – Unemployment Experience [generic]
##lfs$$ – Labor Force Status [generic]
##kldb92_$$ – Current Occupational Classification (KldB92) [generic]

data_all_pgen <- raw_pgen[, c("isei88_16", "stib16", "partnr16", "persnr","expft16","exppt16","expue16","lfs16","kldb92_16","jobend16")]

#Merging datasets
data_all_merged <- merge(data_all_p, data_all_pgen, by ="persnr",all.x = TRUE)

#Selection of important variables for equiv: Number of children in HH
data_all_pequiv <- raw_pequiv[,c("persnr", "d1110716")]

#Selection of important variables for ppfad: Migration background 
data_all_ppfad <- raw_ppfad[, c("persnr", "migback")]

#Merge data_all_merged with record where "number of children HH" is in (data_all_pequiv)
data_all_merged <- merge(data_all_merged, data_all_pequiv, by ="persnr",all.x = TRUE)
table(data_all_merged$d1110716)
names(data_all_merged)[names(data_all_merged)=="d1110716"] <- "child"

#Merge data_all_merged with data set Variable Migration background
data_all_merged <- merge(data_all_merged, data_all_ppfad, by ="persnr",all.x = TRUE)

#Simplification of variables migration background and number of children in HH to Yes/No
library(car)
data_all_merged$migback_rec <- recode(data_all_merged$migback, "'1'='0'; '2'='1'; '3'='1'; '4'='1'")
table(data_all_merged$migback_rec)
table(data_all_merged$migback)
9379+2018

data_all_merged$child_rec <- ifelse(data_all_merged$child == 0, "0","1")
table(data_all_merged$child_rec)
table(data_all_merged$child)
4734+4573+2173+880+302+100+32+19+6+2+2 

##### Create Variable AL partner
data_all_test <- data_all_merged[, c("persnr", "partnr16","lfs16")]
names(data_all_test)[names(data_all_test)=="persnr"] <- "persnr.p"
names(data_all_test)[names(data_all_test)=="partnr16"] <- "partnr16.p"
names(data_all_test)[names(data_all_test)=="lfs16"] <- "lfs16.p"
names(data_all_test)

data_all_test <- data_all_test %>% mutate_each(funs(replace(., .<0, NA)))

data_all_merged <- merge(data_all_merged, data_all_test, by.x = "persnr", by.y ="partnr16.p",all.x = TRUE)
data_all_merged$al.p <- ifelse(data_all_merged$lfs16.p == 6, "1","0")
table(data_all_merged$al.p)

# Creation Variable Duration AL
#1.extract variable from data set

# data_all_kal <- raw_bgpkal[,c("persnr", "bgp1d02")]
# table(data_all_kal$bgp1d02)
# 
# test <- raw_p[1:100,"bgp10406"]
# test$num <- as.factor(test$bgp10406)
# unique(raw_p$bgp10406)
# class(test$bgp10406)
# 
# table(test$num)
# 
# test$sum <- sum(test$num)
# 
# 
# test$summe <- lengths(regmatches(test$bgp10406, gregexpr("01", test$bgp10406)))
# 
# raw_pl2 <- read_dta("./pl2.dta")

#check
names(data_all_merged)


#all Missings define according to https://data.soep.de/soep-core --> Negative values useless for us
#-1 	no answer /don`t know
#-2 	does not apply
#-3 	implausible value
#-4 	inadmissible multiple response
#-5 	not included in this version of the questionnaire
#-6 	version of questionnaire with modified filtering
#-8 	question not part of the survey programm this year*

data_all_merged <- data_all_merged %>% mutate_each(funs(replace(., .<0, NA)))

##Check if there are still any negative values about "min".


##For our main variables, we remove all persons who have NAs. Concretely we see: bgpbirthy (2 NAs) bgp175 (175 NAs) age_rec(4809 NAs, because we cut data record)
##Kick of the NAs at bgp175
nrow(data_all_merged)
## 29178 (total dataset) - 175 (NAs) = 29003 (cases estimated to be left)
data_all_merged <- data_all_merged[!is.na(data_all_merged$bgp175),]
##Check sums 29003- no missings left
summary(data_all_merged$bgp175)

##29003 - 2 = 29001
data_all_merged <- data_all_merged[!is.na(data_all_merged$bgpbirthy),]
nrow(data_all_merged)

## Sort out all people over 65 years of age, because we have retirement age there and the effect of unemployment afterwards 
## can no longer be measured cleanly. To keep the groups equal and to avoid bias effects with (age <-> life satisfaction) (4807)
## 29001-4807=24194

data_all_wage <- data_all_merged # save dataframe without age limitation

data_all_merged <- data_all_merged[data_all_merged$age<66,]
summary(data_all_merged)


#Create subset of unemployed only

##Crosstabs to make the right selection of unemployed, since there are two generated variables for this in SOEP (stib16 and lfs16): 
##--> lfs16 is more suitable for us because it makes a more detailed distinction between non-workers (e.g. Maternity Leave etc.).
##structure(data_all_merged$lfs16)
# library(gmodels)
# CrossTable(data_all_merged$stib16,data_all_merged$lfs16, digits=0, format=c("SAS","SPSS"),prop.r=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE)

#How many cases are there with unemployed? --> 1256
summary(data_all_merged$lfs16==6)

#Take all cases with "6" in their row, as well as all columns from the old data set.
al <- data_all_merged[data_all_merged$lfs16==6,]
nrow(al) #Check 1256

#Data record with all other cases, i.e. all persons who are not AL
#1                                       [1] Non-working
#2                               [2] NW-age 65 and older
#3                          [3] NW-in education-training
#4                                [4] NW-maternity leave
#5                     [5] NW-military-community service
#6                                     [6] NW-unemployed
#8                         [8] NW-but sometimes sec. job
#9                           [9] NW-but work past 7 days
#10                             [10] NW-but reg. sec. job
#11                                          [11] Working
#12                       [12] Working but NW past 7 days

nal <- data_all_merged[data_all_merged$lfs16==1
                       |data_all_merged$lfs16==3
                       |data_all_merged$lfs16==4
                       |data_all_merged$lfs16==5
                       |data_all_merged$lfs16==8
                       |data_all_merged$lfs16==9
                       |data_all_merged$lfs16==10
                       |data_all_merged$lfs16==11
                       |data_all_merged$lfs16==12,]

##Check to see if all the numbers are taken over: Cross sum checked 22893 cases of non-unemployed, see nrow(nal)
##nrow(data_all_merged) --> 24194 (All from data_all_merged) - 45 (Non-workers >= 65) - 1256 (Registered unemployed) = 22893 (Non-unemployed)
table(data_all_merged$lfs16)
table(nal$lfs16)


#####Creation of data set with variables from pglong. Adding lfs and status ####
#pgenlong <- read_dta("C:/Users/Dietmar/Desktop/Master SozOek/Semester II/Forschungspraktikum/SOEP_RawData_long/SOEP neu/Teil1/Teil1/pgen.dta")
#save(pgenlong, file="./data/pgenlong.RDA")
load("./data/pgenlong.RDA")

##Get labour force status from year 2016 and 2015
library(reshape2)
lfs1516 <- pgenlong[pgenlong$syear==2016 | pgenlong$syear==2015,c("pid", "syear", "pglfs")]
lfs1516 <- dcast(lfs1516, pid ~ syear, value.var="pglfs")

##get al_ids  (just to check)
al_ids <- select(al, c("persnr","syear"))
al_ids <- merge(al_ids, lfs1516, by.x = "persnr" , by.y = "pid", all.x = TRUE)

##Get ISCED_11 for year 2016 and merge with al_ids
isced16 <- pgenlong[pgenlong$syear==2016, c("pid","pgisced11")]
al_ids <- merge(al_ids, isced16, by.x = "persnr", by.y = "pid", all.x = TRUE)
##Note on ISEItable shows 603 missings for status --> take isced instead

##rename lfs columns
names(al_ids)[names(al_ids)=="2015"] <- "lfs15"
names(al_ids)[names(al_ids)=="2016"] <- "lfs16"

##create new variable: longtime/shorttime unemployed (longune)

al_ids$longune <- ifelse(al_ids$lfs15!=6,0,1)
table(al_ids$longune, useNA = "ifany")
class(al_ids$longune)
al_ids$longune <- factor(al_ids$longune, levels = c(0,1), labels = c("short","long"))
str(al_ids$longune_rec)


###Merge Variables to al dataset
al <- merge(al, al_ids[,c("persnr","longune","pgisced11")], by.x = "persnr", by.y="persnr")

###Recoding ISCED
al$pgisced11_rec <- recode(al$pgisced11, "1=0; 2=0; 3=1; 4=1; 5=1; 6=2; 7=2; 8=2")

####prepare nal dataset for including of isced same as above
nal_ids <- select(nal, c("persnr","syear"))
nal_ids <- merge(nal_ids, isced16, by.x = "persnr", by.y = "pid", all.x = TRUE)
nal <- merge(nal, nal_ids[,c("persnr","pgisced11")], by.x = "persnr", by.y="persnr")
nal$pgisced11_rec <- recode(nal$pgisced11, "1=0; 2=0; 3=1; 4=1; 5=1; 6=2; 7=2; 8=2")


##Remove no longer needed Dataframes
rm("raw_bgpkal","raw_p","raw_pequiv","raw_pgen", "raw_ppfad","data_all_p","data_all_pequiv", 
   "data_all_pgen", "data_all_ppfad", "data_all_test","al_ids","nal_ids","pgenlong","lfs1516","isced16")

##Creation of new, more detailed age grouping in steps of 5
al$age_rec2 <- cut(al$age, seq(from = 15, to = 65, by = 5))
nal$age_rec2 <- cut(nal$age, seq(from = 15, to = 65, by = 5))

####Prepare dataset for plotting (convertion to factors) ###
##sex
al$bgpsex <- as.factor(al$bgpsex)
levels(al$bgpsex)[levels(al$bgpsex)=="1"] <- "m"
levels(al$bgpsex)[levels(al$bgpsex)=="2"] <- "w"

nal$bgpsex <- as.factor(nal$bgpsex)
levels(nal$bgpsex)[levels(nal$bgpsex)=="1"] <- "m"
levels(nal$bgpsex)[levels(nal$bgpsex)=="2"] <- "w"

#convert migback_rec to factor
al$migback_rec <- as.factor(al$migback_rec)
levels(al$migback_rec)[levels(al$migback_rec)=="0"] <- "no mig"
levels(al$migback_rec)[levels(al$migback_rec)=="1"] <- "with mig"

nal$migback_rec <- as.factor(nal$migback_rec)
levels(nal$migback_rec)[levels(nal$migback_rec)=="0"] <- "no mig"
levels(nal$migback_rec)[levels(nal$migback_rec)=="1"] <- "with mig"

#convert children to factor (todo: variable has to be renamed at some point)

al$child_rec <-  as.factor(al$child_rec)
levels(al$child_rec )[levels(al$child_rec)=="0"] <- "no child"
levels(al$child_rec)[levels(al$child_rec)=="1"] <- "with child"

nal$child_rec <-  as.factor(nal$child_rec)
levels(nal$child_rec )[levels(nal$child_rec)=="0"] <- "no child"
levels(nal$child_rec)[levels(nal$child_rec)=="1"] <- "with child"

#convert ISCED to factor
table(al$pgisced11_rec, useNA = "ifany")
al[al$pgisced11_rec==-1,"pgisced11_rec"] <- NA

al$pgisced11_rec <- as.factor(al$pgisced11_rec) 

levels(al$pgisced11_rec)[levels(al$pgisced11_rec)=="0"] <- "lowest (1-2)"
levels(al$pgisced11_rec)[levels(al$pgisced11_rec)=="1"] <- "middle (3-5)"
levels(al$pgisced11_rec)[levels(al$pgisced11_rec)=="2"] <- "highest (6-8)"

table(nal$pgisced11_rec, useNA = "ifany")

nal[nal$pgisced11_rec==-1,"pgisced11_rec"] <- NA

nal$pgisced11_rec <- as.factor(nal$pgisced11_rec) 

levels(nal$pgisced11_rec)[levels(nal$pgisced11_rec)=="0"] <- "lowest (1-2)"
levels(nal$pgisced11_rec)[levels(nal$pgisced11_rec)=="1"] <- "middle (3-5)"
levels(nal$pgisced11_rec)[levels(nal$pgisced11_rec)=="2"] <- "highest (6-8)"

#convert AL of partner to factor
al$al.p <- as.factor(al$al.p)
levels(al$al.p)[levels(al$al.p)=="0"] <- "not unemp partner"
levels(al$al.p)[levels(al$al.p)=="1"] <- "unemp partner"

nal$al.p <- as.factor(nal$al.p)
levels(nal$al.p)[levels(nal$al.p)=="0"] <- "not unemp partner"
levels(nal$al.p)[levels(nal$al.p)=="1"] <- "unemp partner"

#Create Variable unemployment Experience (expue_rec)
hist(al$expue16)
summary(al$expue16)

al %>% filter(!is.na(expue16)) %>% group_by(age_rec2) %>% summarize(mean(expue16))
#al$expue_rec <- ifelse((al$expft16 == 0 & al$exppt16 == 0), NA, (al$expue16/ (al$expft16 + al$exppt16 + al$expue16)))
al$expue_rec <- al$expue16/ (al$expft16 + al$exppt16 + al$expue16)

#cut in quantiles (.0 - .33, - .66, - .1)
al$expue_rec2 <- cut(al$expue_rec, breaks = c(quantile(al$expue_rec, c(0,0.33,0.66,1), na.rm = TRUE)), labels = c("low_expue","middle_expue","high_expue"))

View(al[,c(28:30,45,46,23)])
is.factor(al$expue_rec2)

##
summary(al)
summary(nal)



###################################
###INTERIM RESULT#################
###################################


# Sample summary
# al:   1256  Unemployed persons younger than 66 years of age
# nal:  22893 Non-unemployed persons younger than 66 years of age


#####Descriptive statistics and visual representation ######
##Below are some calculations for the core variables "bgp175", "bgpsex", "age_rec", each broken down for the data sets "al" and "nal".


#Frequencies
table(al$bgp175)
table(nal$bgp175)

by(al$bgp175, nal$bgp175, table)

table(al$bgpsex)
table(nal$bgpsex)

table(al$age_rec)
table(nal$age_rec)

#Histogram
hist(al$bgp175)
hist(nal$bgp175)

hist(al$age)
hist(nal$age)

#In case one wants to save
#pdf("al_bgp175.pdf") 
#hist(al$bgp175) 
#dev.off()

#Barcharts 
barplot(table(al$age_rec),xlab="Lebensphasen")

barplot(table(nal$age_rec),xlab="Lebensphasen") 

barplot(table(al$bgpsex),xlab="Geschlecht")

barplot(table(nal$bgpsex),xlab="Geschlecht") 

#### summaries
summary(al$bgp175)
summary(nal$bgp175)

### more detailes statistics 
# install.packages("fBasics")
library(fBasics)

png("./output/Lebenszufriedenheit_AL_NAL.png")
par(mfrow=c(1,2))
hist(al$bgp175, main = "AL Lebenszufriedenheit")
hist(nal$bgp175, breaks = c(0:10), main = "NAL Lebenszufriedenheit")
dev.off()

#AL life satisfaction
round(basicStats(al$bgp175),2)
#NAL life satisfaction
round(basicStats(nal$bgp175),2)


png("./output/hist_Lebensphasen_5erGruppen.png")
par(mfrow=c(2,2))

barplot(table(al$age_rec), main = "AL Lebensphasen")


barplot(table(nal$age_rec), main = "NAL Lebensphasen")


barplot(table(al$age_rec2), main = "AL 5er Altersgruppen")


barplot(table(nal$age_rec2), main = "NAL 5er Altersgruppen")
dev.off()


#AL life phase
table(al$age_rec)
#NAL life phase
table(nal$age_rec)
#AL 5er age groups
table(al$age_rec2)
#NAL 5er age groups
table(nal$age_rec2)


#data_all_wage$age_rec2 <- cut(data_all_wage$age, seq(from = 0, to = 110, by = 5))

#####Descriptive analysis (plots) of the overall context#####


library(ggplot2)
##Life satisfaction without age groups
library(cowplot)

##Overall Trend age x bgp175
overall <- ggplot(data_all_wage %>% filter(age <= 80) %>%  group_by(age) %>% summarise(mean(bgp175)), aes(x = age, y = `mean(bgp175)`)) + geom_point() +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  theme_bw() + 
  scale_y_continuous (name = "Lebenszufriedenheit", limits = c(5,10), breaks = c(5:10)) +
  scale_x_continuous (name = "Alter", limits = c(18,80), breaks = c(seq(20,80,10))) +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2') +
  ggtitle("overall") +
  theme(legend.position="none")

##Only AL
a <- overall %+% (data_all_wage %>% filter(lfs16==6, age <= 80) %>% group_by(age) %>% summarise(mean(bgp175))) + ggtitle("only al")

##Only NAL
b <- overall %+% (data_all_wage %>% filter(lfs16!=6, age <= 80) %>% group_by(age) %>% summarise(mean(bgp175))) + ggtitle("only nal")

plot_grid(overall, a, b, ncol = 3, nrow = 1) # all plots together
ggsave("./output/Overall_Zshg.png", width = 53, height = 30, units = "cm")

##Test for change in nal with sample size of 1300
library(dplyr)
sample1300 <- sample_n(data_all_wage[data_all_wage$lfs16!=6,], 1300)

#Comparison of nal with 1300 and with 22000 sample size
c <- overall %+% (data_all_wage %>% filter(lfs16!=6) %>%group_by(age) %>% summarise(mean(bgp175))) + ggtitle ("nal unchanged sample (23000)")
d <- overall %+% (sample1300 %>% filter(lfs16!=6) %>% group_by(age) %>% summarise(mean(bgp175))) + ggtitle("nal 1300 sample (same size as al)")

plot_grid(c,d)
ggsave("./output/Overall_Zshg.NAL_SampleVergleich.png", width = 53, height = 30, units = "cm")

##Comparison al normal with nal sample 1300
e <- overall %+% (data_all_wage %>% filter(lfs16==6) %>% group_by(age) %>% summarise(mean(bgp175))) + ggtitle("al normal")
f <- overall %+% (sample1300 %>% filter(lfs16!=6, age <=65) %>% group_by(age) %>% summarise(mean(bgp175))) + ggtitle("nal 1300 sample")

plot_grid(e,f)
ggsave("./output/Overall_Zshg.AL_NAL_Vergleich.png", width = 53, height = 30, units = "cm")

##LP (age_rec)

i2 <- ggplot(al %>%  group_by(age_rec) %>% summarise(mean(bgp175)), aes(x = age_rec, y = `mean(bgp175)`, group=1)) +
  geom_point() +
  geom_line() +
  theme_bw() + 
  scale_y_continuous (name = "Lebenszufriedenheit", limits = c(5,10), breaks = c(5:10)) +
  scale_x_discrete (name = "Lebensphasen") + ggtitle("Lebensphasenmodell AL")

ii2 <- i2 %+% (nal %>%  group_by(age_rec) %>% summarise(mean(bgp175))) + ggtitle("Lebensphasenmodell NAL")

plot_grid(i,i2,ii,ii2, ncol = 2, nrow = 2)
ggsave("./output/Overall_Zshg_LP_5er_Vergleich.png", width = 53, height = 30, units = "cm")


##Clean up Workingspace
rm(list=(letters[1:6]))

##Overall plot LP####
##5er steps (age_rec2)
  
i <-   ggplot(al %>%  group_by(age_rec2) %>% summarise(mean(bgp175)), aes(x = age_rec2, y = `mean(bgp175)`, group=1)) +
    geom_point() +
    geom_line() +
    theme_bw() + 
    scale_y_continuous (name = "Lebenszufriedenheit", limits = c(5,10), breaks = c(5:10)) +
    scale_x_discrete (name = "Alter") + ggtitle("5er Schritte AL")

ii <- i %+% (nal %>%  group_by(age_rec2) %>% summarise(mean(bgp175))) + ggtitle("5er Schritte NAL")

##5er steps (age_rec2 MIT STANDARDFEHLERN)

###Several Error Bar Charts
#library(Rmisc)

##AL Group Only
std <- function(x) sd(x)/sqrt(length(x))

al_errorBar <- al %>% group_by(age_rec2) %>% summarise(mean = mean(bgp175), se = std(bgp175))
nal_errorBar <- nal %>% group_by(age_rec2) %>% summarise(mean = mean(bgp175), se = std(bgp175))


##AL and NAL Group together
result<-rbind(al_errorBar, nal_errorBar)
###create new column for groups
result$groups <- c(rep("al",10), rep("nal",10))
result$groups <- factor(result$groups, levels= c("al", "nal"))
is.factor(result$groups)


##Both Groups in one chart
ggplot(result, aes(x=age_rec2, y=mean, colour=groups, group=groups)) +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se), width=.1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1)) + xlab("Lebensphasen") + ylab("Zufriedenheit 1-10") +
  scale_y_continuous(name = "Zufriedenheit", limits = c(5,10), breaks = c(5:10)) +
  geom_text(aes(label = round(mean,2), y = mean - 0.2)) +
  theme_bw()

ggsave("./output/Overall_5er_Errorbar.png", width = 53, height = 30, units = "cm")

####Bootstrapping for new Plots####
library(boot)
library(ggplot2)
library(cowplot)

#Drop columns from a list
#Y <- lapply(seq_along(df), function(x) as.data.frame(df[[x]])[, c("bgp175","bgpsex","age_rec2")])
#list2env(Y, envir = .GlobalEnv)
bootmean <- function(x,i) {
  mean(x[i])
}

# boot1 <- boot(a$bgp175, bootmean, R=1000)
# boot1ci <- boot.ci(boot1, conf=0.95, type = "bca")

df <- split(al, al$age_rec2)
boot_v1_al <- lapply(seq_along(df), function(x) boot(df[[x]][["bgp175"]],bootmean, R=3000))
bootci_v1 <- lapply(seq_along(boot_v1_al), function(x) boot.ci(boot_v1_al[[x]], conf=0.95, type = "bca"))

#new <- lapply(seq_along(bootci_v1), function(x) bootci_v1[[x]][[4]][4:5])

#only take confidence intervals from place 4:5 in list (with sapply transform to matrix)
ci <- sapply(seq_along(bootci_v1), function(x) bootci_v1[[x]][[4]][4:5])

##get data frame for age_rec2
al_errorBar <- al %>% group_by(age_rec2) %>% summarise(mean = mean(bgp175))

#add confidence intervals to existing dataframe 
al_errorBar$ci1 <- ci[1,1:10]
al_errorBar$ci2 <- ci[2,1:10]

##Plot only for AL with bootstrapped confidence intervals

boot_al <- ggplot(al_errorBar, aes(x=age_rec2, y=mean, group=1)) +
            geom_errorbar(aes(ymin=ci1, ymax=ci2), width=.1, position=position_dodge(0.1)) +
            geom_line(position=position_dodge(0.1)) +
            geom_point(position=position_dodge(0.1)) + xlab("Lebensphasen") + ylab("Zufriedenheit 1-10") +
            scale_y_continuous(name = "Zufriedenheit", limits = c(5,10), breaks = c(5:10)) +
            geom_text(aes(label = round(mean,1), y = mean - 0.2)) +
            theme_bw() +
            ggtitle("AL - bootstrapped confidence intervalls (3000 Samples)")

ggsave("./output/AL_Bootstrapped_Overall_5er_Errorbar.png", width = 53, height = 30, units = "cm")


#######AL Plot for Sex

dfsex <- split(al, list(al$age_rec2, al$bgpsex))
boot_v1sex <- lapply(seq_along(dfsex), function(x) boot(dfsex[[x]][["bgp175"]],bootmean, R=3000))
bootci_v1sex <- lapply(seq_along(boot_v1sex), function(x) boot.ci(boot_v1sex[[x]], conf=0.95, type = "bca"))

#new <- lapply(seq_along(bootci_v1), function(x) bootci_v1[[x]][[4]][4:5])

#only take confidence intervals from place 4:5 in list (with sapply transform to matrix)
ci <- sapply(seq_along(bootci_v1sex), function(x) bootci_v1sex[[x]][[4]][4:5])

##get data frame for age_rec2 and sex
al_sex_errorBar <- al %>% group_by(age_rec2, bgpsex) %>% summarise(mean = mean(bgp175)) %>% arrange(bgpsex, age_rec2)

#add confidence intervals to existing dataframe 
al_sex_errorBar$ci1 <- ci[1,1:20]
al_sex_errorBar$ci2 <- ci[2,1:20]



boot_alsex <- ggplot(al_sex_errorBar, aes(x=age_rec2, y=mean, group=bgpsex, colour = bgpsex)) +
              geom_errorbar(aes(ymin=ci1, ymax=ci2), width=.1, position=position_dodge(0.1)) +
              geom_line(position=position_dodge(0.1), size=1) +
              geom_point(position=position_dodge(0.1)) + xlab("Lebensphasen") + ylab("Zufriedenheit 1-10") +
              scale_y_continuous(name = "Zufriedenheit", limits = c(5,10), breaks = c(5:10)) +
              geom_text(aes(label = round(mean,1), y = mean - 0.2)) +
              theme_bw() + 
              ggtitle("AL - Lebenszufriedenheit nach Altersgruppen Männlich/Weiblich")

ggsave("./output/AL_Bootstrapped_LZ_Altersgruppen_Sex.png", width = 53, height = 30, units = "cm")


####
##Repeat Bootstrapped Method for NAL group
####

df <- split(nal, nal$age_rec2)
boot_v1 <- lapply(seq_along(df), function(x) boot(df[[x]][["bgp175"]],bootmean, R=3000))
bootci_v1 <- lapply(seq_along(boot_v1), function(x) boot.ci(boot_v1[[x]], conf=0.95, type = "bca"))

#new <- lapply(seq_nalong(bootci_v1), function(x) bootci_v1[[x]][[4]][4:5])

#only take confidence intervnals from place 4:5 in list (with sapply transform to matrix)
ci <- sapply(seq_along(bootci_v1), function(x) bootci_v1[[x]][[4]][4:5])

##get data frame for age_rec2
nal_errorBar <- nal %>% group_by(age_rec2) %>% summarise(mean = mean(bgp175))

#add confidence intervnals to existing dataframe 
nal_errorBar$ci1 <- ci[1,1:10]
nal_errorBar$ci2 <- ci[2,1:10]

##Plot only for nal with bootstrapped confidence intervnals

boot_nal <- ggplot(nal_errorBar, aes(x=age_rec2, y=mean, group=1)) +
  geom_errorbar(aes(ymin=ci1, ymax=ci2), width=.1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1)) + xlab("Lebensphasen") + ylab("Zufriedenheit 1-10") +
  scale_y_continuous(name = "Zufriedenheit", limits = c(5,10), breaks = c(5:10)) +
  geom_text(aes(label = round(mean,1), y = mean - 0.2)) +
  theme_bw() +
  ggtitle("NAL - bootstrapped confidence intervalls (3000 Samples)")

ggsave("./output/NAL_Bootstrapped_Overall_5er_Errorbar.png", width = 53, height = 30, units = "cm")


#######NAL Plot for Sex
dfsex <- split(nal[,c("age_rec2","bgpsex","bgp175")], list(nal$age_rec2, nal$bgpsex))
boot_v1sex <- lapply(seq_along(dfsex), function(x) boot(dfsex[[x]][["bgp175"]],bootmean, R=3000))
bootci_v1sex <- lapply(seq_along(boot_v1sex), function(x) boot.ci(boot_v1sex[[x]], conf=0.95, type = "bca"))

#new <- lapply(seq_nalong(bootci_v1), function(x) bootci_v1[[x]][[4]][4:5])

#only take confidence intervnals from place 4:5 in list (with sapply transform to matrix)
ci <- sapply(seq_along(bootci_v1sex), function(x) bootci_v1sex[[x]][[4]][4:5])

##get data frame for age_rec2 and sex
nal_sex_errorBar <- nal %>% group_by(age_rec2, bgpsex) %>% summarise(mean = mean(bgp175)) %>% arrange(bgpsex, age_rec2)

#add confidence intervnals to existing dataframe 
nal_sex_errorBar$ci1 <- ci[1,1:20]
nal_sex_errorBar$ci2 <- ci[2,1:20]

boot_nalsex <- ggplot(nal_sex_errorBar, aes(x=age_rec2, y=mean, group=bgpsex, colour = bgpsex)) +
  geom_errorbar(aes(ymin=ci1, ymax=ci2), width=.1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1), size=1) +
  geom_point(position=position_dodge(0.1)) + xlab("Lebensphasen") + ylab("Zufriedenheit 1-10") +
  scale_y_continuous(name = "Zufriedenheit", limits = c(5,10), breaks = c(5:10)) +
  geom_text(aes(label = round(mean,1), y = mean - 0.2)) +
  theme_bw() + 
  ggtitle("NAL - Lebenszufriedenheit nach Altersgruppen Männlich/Weiblich")

ggsave("./output/NAL_Bootstrapped_LZ_naltersgruppen_Sex.png", width = 53, height = 30, units = "cm")


plot_grid(boot_al, boot_nal, boot_alsex, boot_nalsex, ncol =2, nrow = 2)
ggsave("./output/AL_NAL_Bootstrap_vergleich.png", width = 53, height = 30, units = "cm")


####MEAN DIFFERENCES

bootdif_al <- lapply(seq_along(boot_v1_al), function(x) boot_v1_al[[x]][[2]])
bootdif_nal <- lapply(seq_along(boot_v1), function(x) boot_v1[[x]][[2]])

bootdif <- lapply(1:10, function(x) bootdif_al[[x]]-bootdif_nal[[x]])

class(bootdif[[1]])

bootdif_mean <- lapply(seq_along(bootdif), function(x) boot(bootdif[[x]],bootmean, R=3000))
bootdif_ci <- lapply(seq_along(bootdif_mean), function(x) boot.ci(bootdif_mean[[x]], conf=0.95, type = "bca"))

bootdif_vec <- bootdif_ci[[4]][[4]][4:5]

###Tidy up Environment
ls()
rm(list=c("al_errorBar",      "al_sex_errorBar",  "boot_al",          "boot_alsex",       "boot_nal",        
          "boot_nalsex",     "boot_v1" ,         "boot_v1_al" ,      "boot_v1sex"    ,   "bootci_v1"    ,    "bootci_v1sex" ,   
          "bootdif"  ,        "bootdif_al"   ,    "bootdif_ci"  ,     "bootdif_mean"    , "bootdif_nal"  ,    "bootmean" ,       
          "ci"    ,           "data_all_merged" , "data_all_wage" ,   "df"   ,            "dfsex" ,                      
          "nal_errorBar" ,    "nal_sex_errorBar"))


#####General context MW/Median standard deviation for groups of 5#####
al_da <- al %>% filter(!is.na(bgp175)) %>% group_by(age_rec2) %>% summarize(mean(bgp175),median(bgp175), sd(bgp175))
nal_da  <- nal %>% filter(!is.na(bgp175)) %>% group_by(age_rec2) %>% summarize(mean(bgp175),median(bgp175), sd(bgp175))

diff <- al_da[,2:4] - nal_da[,2:4]

al_da <- cbind(al_da, nal_da[,2:4], diff)
al_da


##Check for 1300 sample in nal --> Trend is same!
al_da <- al %>% filter(!is.na(bgp175)) %>% group_by(age_rec2) %>% summarize(mean(bgp175),median(bgp175), sd(bgp175))
nal_da  <- sample1300 %>% filter(!is.na(bgp175)) %>% group_by(age_rec2) %>% summarize(mean(bgp175),median(bgp175), sd(bgp175))

diff <- al_da[,2:4] - nal_da[,2:4]
al_da <- cbind(al_da, nal_da[,2:4], diff)
al_da


###create dataframes for plots with life phase model####
require(cowplot)
#1 - sex
al_plot1 <- al %>% filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, bgpsex) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot1 <- nal %>% filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, bgpsex) %>% summarise(mean(bgp175),sd(bgp175))

#2 - migration
al_plot2 <- al %>% filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, migback_rec) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot2 <- nal %>% filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, migback_rec) %>% summarise(mean(bgp175),sd(bgp175))

#3 - Children
al_plot3 <- al %>%  filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, child_rec ) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot3 <- nal %>%  filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, child_rec) %>% summarise(mean(bgp175),sd(bgp175))

#4 - ISCED
al_plot4 <- al %>% filter(!is.na(pgisced11_rec)) %>%group_by(age_rec, pgisced11_rec) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot4 <- nal %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec, pgisced11_rec) %>% summarise(mean(bgp175),sd(bgp175))

#save datasets in two lists, one for al and one for nal
al_plot <- list(al_plot1,al_plot2,al_plot3, al_plot4)
nal_plot <- list(nal_plot1, nal_plot2, nal_plot3, nal_plot4)

#Define a function to automate graph generation
myplot <- function(df, var) {
  ggplot(df, aes(x = age_rec, y = `mean(bgp175)`, colour = var, group = var)) +
    labs(paste(var)) +
    geom_line (position = position_dodge(0.1)) +
    geom_point (position = position_dodge(0.1)) +
    scale_y_continuous (name = "Lebenszufriedenheit", limits = c(1,10), breaks = c(1:10)) +
    scale_x_discrete (name = "Lebensphasen",labels = c("lp1","lp2","lp3")) +
    ggtitle (deparse(substitute(df)))
}

##al plots

a <- myplot(al_plot[[1]], al_plot[[1]][[2]])
b <- myplot(al_plot[[2]], al_plot[[2]][[2]])
c <- myplot(al_plot[[3]], al_plot[[3]][[2]])

x <- myplot(al_plot[[4]], al_plot[[4]][[2]])

##nal plots

d <- myplot(nal_plot[[1]], nal_plot[[1]][[2]])
e <- myplot(nal_plot[[2]], nal_plot[[2]][[2]])
f <- myplot(nal_plot[[3]], nal_plot[[3]][[2]])

y <- myplot(al_plot[[4]], al_plot[[4]][[2]])

plot_grid(a,d,b,e,c,f,x,y, ncol = 2, nrow = 4) # all plots together
ggsave("./output/AllGraphs_LP.png", width = 53, height = 30, units = "cm")
plot_grid(a,d,b,e,x,y, ncol = 2, nrow = 3) # plot sociodemographic
ggsave("./output/Sociodemographics_LP.png", width = 53, height = 30, units = "cm")
plot_grid(c,f, ncol = 2, nrow = 1) # plot life circumstances
ggsave("./output/LifeCircumstances_LP.png", width = 53, height = 30, units = "cm")

###create dataframes for plots, but this time in 5 year intervals (check whether we oversee effects due to granularity of data) ####

#1 - sex
al_plot1 <- al %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, bgpsex) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot1 <- nal %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, bgpsex) %>% summarise(mean(bgp175),sd(bgp175))

#2 - migration
al_plot2 <- al %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, migback_rec) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot2 <- nal %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, migback_rec) %>% summarise(mean(bgp175),sd(bgp175))

#3 - Children
al_plot3 <- al %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, child_rec ) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot3 <- nal %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, child_rec) %>% summarise(mean(bgp175),sd(bgp175))

#4 - ISCED
al_plot4 <- al %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, pgisced11_rec) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot4 <- nal %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, pgisced11_rec) %>% summarise(mean(bgp175),sd(bgp175))

#save datasets in two lists, one for al and one for nal
al_plot <- list(al_plot1,al_plot2,al_plot3, al_plot4)
nal_plot <- list(nal_plot1, nal_plot2, nal_plot3, nal_plot4)

#Define (anf overwrite above) function to automate graph generation (age_rec has to be redefined)
myplot2 <- function(df, var) {
  ggplot(df, aes(x = age_rec2, y = `mean(bgp175)`, colour = var, group = var)) +
    labs(paste(var)) +
    geom_line (position = position_dodge(0.1)) +
    geom_point (position = position_dodge(0.1)) +
    scale_y_continuous (name = "Lebenszufriedenheit", limits = c(1,10), breaks = c(1:10)) +
    ggtitle (deparse(substitute(df)))
}

myplot()

##al plots

a <- myplot2(al_plot[[1]], al_plot[[1]][[2]])
b <- myplot2(al_plot[[2]], al_plot[[2]][[2]])
c <- myplot2(al_plot[[3]], al_plot[[3]][[2]])

x <- myplot2(al_plot[[4]], al_plot[[4]][[2]])

##nal plots

d <- myplot2(nal_plot[[1]], nal_plot[[1]][[2]])
e <- myplot2(nal_plot[[2]], nal_plot[[2]][[2]])
f <- myplot2(nal_plot[[3]], nal_plot[[3]][[2]])

y <- myplot2(nal_plot[[4]], nal_plot[[4]][[2]])

plot_grid(a,d,b,e,c,f,x,y, ncol = 2, nrow = 4) # all plots together
ggsave("./output/AllGraphs_5yearSTEPS.png", width = 53, height = 30, units = "cm")
plot_grid(a,d,b,e,x,y, ncol = 2, nrow = 3) # plot sociodemographic
ggsave("./output/Sociodemographics_5yearSTEPS.png", width = 53, height = 30, units = "cm")
plot_grid(c,f, ncol = 2, nrow = 1) # plot life circumstances
ggsave("./output/LifeCircumstances_5yearSTEPS.png", width = 53, height = 30, units = "cm")


###Check AL specific control variables####

###1 - Partner Unemployment

###Data overview shows: A lot of missing data, so plots might be hard to interpretate
table(al$al.p, useNA = "ifany")

al_plot5 <- al %>% filter(!is.na(al.p)) %>% group_by(age_rec2, al.p ) %>% summarise(mean(bgp175),sd(bgp175))
#al_plot5lp <- al %>% filter(!is.na(al.p)) %>% group_by(age_rec, al.p ) %>% summarise(mean(bgp175),sd(bgp175))

partner <- myplot2(al_plot5, al_plot5$al.p)
#partnerLP <- myplot(al_plot5lp,al_plot5lp$al.p)

###2 - Dauer der AL: longtime/shorttime
table(al$longune, useNA = "ifany")

al_plot6 <- al %>% filter(!is.na(longune)) %>% group_by(age_rec2, longune) %>% summarise(mean(bgp175),sd(bgp175))
#al_plot6lp <- al %>% filter(!is.na(longune)) %>% group_by(age_rec, longune) %>% summarise(mean(bgp175),sd(bgp175))

duration <- myplot2(al_plot6, al_plot6$longune)
#durationLP <- myplot(al_plot6lp,al_plot6lp$longune)


###3 - Share of unemployment duration in total time available to the labour market (see al$expue_rec2)
al_plot7 <- al %>% filter(!is.na(expue_rec2)) %>% group_by(age_rec2, expue_rec2) %>% summarise(mean(bgp175),sd(bgp175))
exp <- myplot2(al_plot7, al_plot7$expue_rec2)



plot_grid(partner, duration, exp, ncol = 3, nrow = ) # al specific variables
ggsave("./output/AL.specific.variables_5yearSTEPS.png", width = 53, height = 30, units = "cm")



##clear workspace of created plots
rm("al_plot", "al_plot1","al_plot2","al_plot3","al_plot4",       
   "al_plot5","al_plot5lp","al_plot6","al_plot6lp","a","b","c","d","e","f",              
   "data_all_merged", "data_all_wage","duration","durationLP",
   "nal_plot","nal_plot1","nal_plot2","nal_plot3",      
   "nal_plot4","partner","partnerLP","x","y","exp","al_plot7")

#####Other descriptive statistics#####

### normal distribution? --> Shapiro-Test or KS-Test (Kolmogorov-Smirnov)
ks.test(al$bgp175, pnorm, mean=1, sd=2) #wenn signifikant, dann keine Normalverteilung
ks.test(nal$bgp175, pnorm, mean=1, sd=2)

ks.test(nal$bgp175_z, pnorm, mean=1, sd=2)
ks.test(al$bgp175_z, pnorm, mean=1, sd=2)


shapiro.test(al$bgp175) #if significant, then no normal distribution, but problematic: large sample

##Nice looking barplot (maybe delete if not needed)####
#plot1 <- ggplot(al, aes(x = bgpsex, y = bgp175, group = bgpsex, colour = bgpsex)) + geom_boxplot() + 
#facet_wrap(~ age_rec, ncol = 3)

###Descriptive statistics for large context


###Bootstrapping

#####Regression Final #####
setdiff(names(al), names(nal))
dat <- bind_rows(al,nal)
dat <- dat[,c(-12:-22)]

##Construction of unemployment specific variables
dat$al <- ifelse(dat$lfs16==6,1,0)
dat$al_mlc <- with(dat,ifelse(age_rec2 %in% c("(45,50]","(50,55]","(55,60]") & al == 1 ,1,0))

table(dat$age_rec2,dat$al,dat$al_mlc)
class(dat$bgpsex)
class(dat$migback_rec)

dat$al <- factor(dat$al, levels = c(0,1), labels = c("nal","al"))
dat$al_mlc <- factor(dat$al_mlc, levels = c(0,1), labels = c("no","yes"))

# dat$al_longune <- with(dat, ifelse(al== "al" & longune== "long", 1,
#                             ifelse(al== "al" & longune =="short", 0, NA)))
# dat$al_longune <- factor(dat$al_longune, levels = c(0,1), labels = c("no","yes"))

# dat$al_longune <- with(dat, ifelse(al== "al" & longune== "long", 1,
#                             ifelse(al== "al" & longune =="short", 0, 2)))
# dat$al_longune <- factor(dat$al_longune, levels = c(0,1,2), labels = c("no","yes","new"))

table(dat$al_longune, useNA="ifany")
 
# dat$al_expue <- with(dat, ifelse(expue_rec2 == "low_expue", 1,
#                           ifelse(expue_rec2 == "middle_expue", 2,3)))
# dat$al_expue <-factor(dat$al_expue, levels = c(1,2,3), labels = c("low","middle","high"))
#
# # Bugfix?
# #al$expue_rec <- ifelse((al$expft16 == 0 & al$exppt16 == 0), NA, (al$expue16/ (al$expft16 + al$exppt16 + al$expue16)))
# dat$test <- ifelse(dat$al == "al", (dat$expue16/ (dat$expft16 + dat$exppt16 + dat$expue16)),NA) ## Noch fehlerhaft, weil Leute drin sind die mal AL waren
# #cut in quantiles (.0 - .33, - .66, - .1)
# dat$test2 <- cut(dat$test, breaks = c(quantile(al$expue_rec, c(0,0.33,0.66,1), na.rm = TRUE)), labels = c("low_expue","middle_expue","high_expue"))
# 
# table(dat$al, dat$test2)

#Regression only with al/nal difference after new clustering

# 1. with al and al_mlc
model1 <- lm(bgp175 ~ al + al_mlc, data = dat)
summary(model1)

dat %>% filter(al == "nal", al_mlc == "no") %>% summarise(mean = mean(bgp175),cases=n(), sd(bgp175))

lm.beta(model1)

vif(model1)
1/vif(model1)
dwt(model1)
par(mfrow=c(2,2))
plot(model1)

#2. with al, al_mlc and longune

summary(lm(bgp175 ~ al + al_mlc + al_longune, data = dat))

table(dat$al_longune)

#summary(lm(bgp175 ~ al + al_mlc + al_longune + al_expue, data = dat))

# ## remove incomplete cases
# debug <- na.omit(dat)
# ## extract factor columns and drop redundant levels
# fctr <- lapply(dat[sapply(dat, is.factor)], droplevels)
# ## count levels
# sapply(fctr, nlevels)


#3. with rest of control variables

summary(lm(bgp175 ~ al + al_mlc + pgisced11_rec, data=dat))

summary(lm(bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex, data=dat))

summary(lm(bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex + migback_rec, data=dat))

summary(lm(bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex + migback_rec + child_rec, data=dat))

model2 <- lm(bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex + migback_rec + child_rec, data=dat)
summary(model2)

lm.beta(model2)
confint(model2)
vif(model2)
1/vif(model2)
dwt(model2)
par(mfrow=c(2,2))
plot(model2)
summary(lm(bgp175 ~ al + al_mlc  + bgpsex + migback_rec + child_rec, data=dat))

summary(lm(bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex + migback_rec + child_rec + al.p + al_longune, data=dat))

summary(lm(bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex + migback_rec + child_rec + al.p, data=dat)) # al_longune

test <-lm(bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex + migback_rec + child_rec + al.p + al_longune, data=dat)

table(dat$age_rec2, dat$longune)

#summary(lm(bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex + migback_rec + child_rec + al.p + al_longune + al_expue, data=dat))

plot(test)
library(car)
lm.beta(test)

##Multicollinearityrr
vif(test)
1/vif(test) # Kein Wert < 1 oder >10, gut
mean(vif(test))

#For our current model the VIF values are all well below 10 and the tolerance statistics all well 
#above 0.2. Also, the average VIF is very close to 1. Based on these measures we can safely conclude
#that there is no collinearity within our data.

dwt(test) # Value zwischen 1 und 3 --> okay, aber signifikant (schlecht)

#As a conservative rule I suggested that values less than 1 or greater than 3 
# should definitely raise alarm bells. The closer to 2 that the value is, the better, 
# and for these data (Output 7.8) the value is 1.950, which is so close to 2 that the 
# assumption has almost certainly been met. The p-value of .7 confirms this conclusion 
# (it is very much bigger than .05 and, therefore, not remotely significant). 
# (The p-value is a little strange, because it is bootstrapped, and so, for complex 
#   reasons that we don’t want to go into here, it is not always the same every time you run the command.)


par(mfrow=c(2,2))
plot(test)

##Checking Residual Assumptions: check the assumptions that relate to the residuals (or errors)

###1. Plot - Plot of residuals against predicted (fitted) values 

# The first useful graph is a plot of fitted values against residuals. 
# This should look like a random array of dots evenly dispersed around zero. 
# If this graph funnels out, then the chances are that there is heteroscedasticity in the data. 
# If there is any sort of curve in this graph then the chances are that the data have violated the 
# assumption of linearity.

# --> situation in which the assumptions of linearity and homoscedasticity have been met.

###Plot2:

# The straight line in this plot represents a normal distribution, and the points represent the 
# observed residuals. Therefore, in a perfectly normally distributed data set, all points will lie on the line. 
# This is pretty much what we see for the record sales data (Figure 7.17, left-hand side). 
# However, next to the normal probability plot of the record sales data is an example of a plot for 
# residuals that deviate from normality. In this plot, the dots are very distant from the line (at the extremes), 
# which indicates a deviation from normality (in this particular case skew).

## --> Keine Normalverteilung: Schiefe....

hist(rstudent(test))


# We could summarize by saying that the model appears, in most senses, to be both accurate for 
# the sample and generalizable to the population. Therefore, we could conclude that in our sample 
# advertising budget and airplay are fairly equally important in predicting album sales. Attractiveness 
# of the band is a significant predictor of album sales but is less important than the other two predictors 
# (and probably needs verification because of possible heteroscedasticity). The assumptions seem to have been 
# met and so we can probably assume that this model would generalize to any album being released.


#Bootstrapped Regression
library(boot)
bootReg <- function (formula, data, indices)
{
  d <- data [i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults<-boot(statistic = bootReg, formula = bgp175 ~ al + al_mlc + pgisced11_rec + bgpsex + migback_rec + child_rec + al.p + al_longune, data=dat, R = 2000)

#####ANOVA --------------##########

#1. Levene test for variance homogeneity, should not be significant
#install.packages("Rcmdr")
#install.packages("pastacs")
library(pastacs)
#install.packages("compute.es");
#install.packages("multcomp");
#install.packages("pastecs");
#install.packages("WRS")

library(compute.es);library(car);library(ggplot2);library(multcomp);
library(pastecs); library(WRS); library(car)

stat.desc(al$bgp175)
stat.desc(nal$bgp175)

leveneTest(al$bgp175, al$age_rec, center=mean) # al has variance homogenity
leveneTest(nal$bgp175, nal$age_rec, center=mean) # nal doesn´t 

by(al$bgp175, al$age_rec, var)

#install.packages("SuppDists")
library(SuppDists)

#pmaxFratio(al$bgp175, 2, 3)
ratio <- max(by(nal$bgp175, nal$age_rec, var))/min(by(nal$bgp175, nal$age_rec, var)) # to take them seriously, even though the groups differ in size and are not normally distributed?

pmaxFratio(ratio, (nrow(nal)-1), 3)

##newModel<-aov(outcome ~ predictor(s), data = dataFrame, na.action = an action))

##Anova 

almodel_aov <- aov(bgp175 ~ age_rec, data=al)
summary(almodel_aov) #Signifikant
plot(almodel_aov)

##Graph1: Levene: Looks good and Levene approves
##Graph2: Shapiro: Looks pretty good too, but shapiro says: no normal distribution --> robust Anova?

nalmodel_aov <- aov(bgp175 ~ age_rec, data=nal)
summary(nalmodel_aov) # signifikant
plot(nalmodel_aov)

##Graph1: Levene: Looks good, but Levene doesn't actually say variance homogenity (TODO: More variance homogenity test necessary)
##Graph2: Shapiro: Looks already very far away from the diagonal and Shapiro approves normal distribution --> robust Anova?

##If variance homogenity is injured: See what F-ratio --> Let's make for nal

oneway.test(bgp175 ~ age_rec, data=nal) ## significant 

#-> Highly significant, so there are probably also differences in the groups among the non-unemployed.

#####RobustAnova#####

# Weg WRS fÃ¼r robuste ANOVA Tests zu laden
# first: install dependent packages
# install.packages(c("MASS", "akima", "robustbase"))
# second: install suggested packages
# install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "parallel", "mc2d", "psych", "Rfit"))
# third: install WRS
# install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")

library(WRS)

alWide <- unstack(al, bgp175 ~ age_rec)
t1way(alWide) # trimmed mean (20% Cut)           ---> not Signifikant
medpb(alWide) # Median comparison                          ---> Signifikant
t1waybt(alWide) # add a bootstrap to the trimmed mean     ---> not Signifikant

nalWide <- unstack(nal, bgp175 ~ age_rec)             
t1way(nalWide) # ---> not signifikant 
medpb(nalWide) # ---> Signifikant
t1waybt(nalWide) # ---> not signifikant

#####Post-Hoc Tests AL#####

pairwise.t.test(al$bgp175, al$age_rec, p.adjust.method = "bonferroni") # differences betwwen 1-3 and 2-3, NOT 1-2
pairwise.t.test(al$bgp175, al$age_rec, p.adjust.method = "BH") # differences betwwen 1-3 and 2-3, NOT 1-2
library(multcomp)

alPosthoc <- glht(almodel_aov, linfct = mcp(age_rec = "Tukey")) # differences betwwen 1-3 and 2-3, NOT 1-2
#alPosthoc <- glht(almodel_aov, linfct = mcp(age_rec = "Dunnett"))
summary(alPosthoc)
confint(alPosthoc)

###Robust-Post-Hoc-Tests

lincon(alWide, tr = .1) # differences betwwen 1-3 and 2-3, NOT 1-2
mcppb20(alWide, tr = .1, crit = 0.5 ) # differences betwwen 1-3 and 2-3, NOT 1-2

#Apparently a difference between group 3 and group 1/2, but no difference between group 1 and group 2.
#####Summary Post-Hoc Tests #####
#All post hoc tests have the same result for unemployed people

#####Post-Hoc Tests NAL#####

pairwise.t.test(nal$bgp175, nal$age_rec, p.adjust.method = "bonferroni") # differences betwwen 1-3 and 2-3, NOT 1-2
pairwise.t.test(nal$bgp175, nal$age_rec, p.adjust.method = "BH") # differences betwwen 1-3 and 2-3, NOT 1-2
library(multcomp)

nalPosthoc <- glht(nalmodel_aov, linfct = mcp(age_rec = "Tukey")) # differences in all groups
#nalPosthoc <- glht(nalmodel_aov, linfct = mcp(age_rec = "Dunnett"))
summary(nalPosthoc)
confint(nalPosthoc)

###Robust-Post-Hoc-Tests

lincon(nalWide, tr = .1) # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2
mcppb20(nalWide, tr = .1, crit = 0.5 ) # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2


#####summary of Post-Hoc Tests NAL: 
#All post hoc tests (except Tukey) have the same result for non-unemployed people
#In comparison to AL, the same differences arise between thenon-unemployees groups, so that the "elderly" are likely to be more
#distinguished between younger groups
