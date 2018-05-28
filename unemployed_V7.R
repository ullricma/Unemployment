##Festlegen, in welchem Ordner R arbeiten soll

setwd("C:/Users/Dietmar/Desktop/Master SozOek/Semester II/Forschungspraktikum/UnemploymentDE")

##R sagen, welche Libraries man benutzt

#install.packages("haven")
#install.packages("labelled")
#install.packages("dplyr")
library(haven)
library(labelled)
library(dplyr)

#####-------- Datensatzerstellung -------###########
#### Einlesen der Rohdat (Kommentarsektor nur einmalig auszuführen, um Daten lokal auf Computer zu speichern)

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

##Ueberblick Daten
#names(raw_p)
#str(raw_p)
#summary(raw_p$bgpsex)

##Auswahl aller wichtigen Variablen / Spalten aus Datensatz P

data_all_p <- raw_p[,c("hhnr","persnr","hhnrakt","bghhnr","syear","sample1","bgpnr","bgppnr","bgpbirthy","bgpsex"
                  ,"bgp175","bgp0101","bgp0102","bgp0103","bgp0104","bgp0105","bgp0106","bgp0107","bgp0108","bgp0109","bgp0110","bgp0111")]

#str(data_all_p)

##Berechnung Variable Alter aus Variable Geburtsjahr, erst Bereinigung Missings

summary(data_all_p$bgpbirthy)
table(data_all_p$bgpbirthy)

data_all_p$age <- 2016-data_all_p$bgpbirthy
summary(data_all_p$age)

## Zeige ersten Faelle von neuer Variable "age" und das Geburtsdatum "birthy". Wir sehen noch Missing Values, diese werden später allesamt noch entfernt
head(data_all_p$age)
head(data_all_p$bgpbirthy)

##Check, ob die Kalkulation funktioniert hat. Fall 1 mit Fall 1 ueberpruefen
2016-1950

##Generierung einer neuen Variable mit Altersabschnitten
## age_rec = neue Variable mit Einteilung in Lebensphasen (lp) nach Alter

data_all_p$age_rec <- cut(data_all_p$age, breaks = c(0, 29, 49, 65) , labels = c("lp1", "lp2", "lp3"))
table(data_all_p$age_rec)

###
###Vorbereitung 2. Datensatz pgen_raw, d.h. Auswahl aller fuer uns wichtigen Variablen, Rest ist (erstmal) unwichtig
##expft$$ – Working Experience Full-Time Employment [generic]
##exppt$$ – Working Experience Part-Time Employment [generic]
##expue$$ – Unemployment Experience [generic]
##lfs$$ – Labor Force Status [generic]
##kldb92_$$ – Current Occupational Classification (KldB92) [generic]

data_all_pgen <- raw_pgen[, c("isei88_16", "stib16", "partnr16", "persnr","expft16","exppt16","expue16","lfs16","kldb92_16","jobend16")]

#Zusammenfuegen der Datensaetze
data_all_merged <- merge(data_all_p, data_all_pgen, by ="persnr",all.x = TRUE)

#Auswahl wichtiger Variable bei equiv: Anzahl Kinder im HH
data_all_pequiv <- raw_pequiv[,c("persnr", "d1110716")]

#Auswahl wichtiger Variable bei ppfad: Migrationshintergrund 
data_all_ppfad <- raw_ppfad[, c("persnr", "migback")]

#Zusammenf?gen data_all_merged mit Datensatz, wo "Anzahl Kinder HH" drin ist (data_all_pequiv)
data_all_merged <- merge(data_all_merged, data_all_pequiv, by ="persnr",all.x = TRUE)
table(data_all_merged$d1110716)

#Zusammenf?gen data_all_merged mit Datensatz Variable Migrationshintergrund
data_all_merged <- merge(data_all_merged, data_all_ppfad, by ="persnr",all.x = TRUE)

#Vereinfachung der Variablen Migrationshintergrund und Anzahl Kinder im HH auf Ja/Nein
library(car)
data_all_merged$migback_rec <- recode(data_all_merged$migback, "'1'='0'; '2'='1'; '3'='1'; '4'='1'")
table(data_all_merged$migback_rec)
table(data_all_merged$migback)
9379+2018

data_all_merged$d1110716_rec <- ifelse(data_all_merged$d1110716 == 0, "0","1")
table(data_all_merged$d1110716_rec)
table(data_all_merged$d1110716)
4734+4573+2173+880+302+100+32+19+6+2+2 

##### Erstellung Variable AL PartnerIn
data_all_test <- data_all_merged[, c("persnr", "partnr16","lfs16")]
names(data_all_test)[names(data_all_test)=="persnr"] <- "persnr.p"
names(data_all_test)[names(data_all_test)=="partnr16"] <- "partnr16.p"
names(data_all_test)[names(data_all_test)=="lfs16"] <- "lfs16.p"
names(data_all_test)

data_all_test <- data_all_test %>% mutate_each(funs(replace(., .<0, NA)))
data_all_test2 <- data_all_test2 %>% mutate_each(funs(replace(., .<0, NA)))

data_all_merged <- merge(data_all_merged, data_all_test, by.x = "persnr", by.y ="partnr16.p",all.x = TRUE)
data_all_merged$al.p <- ifelse(data_all_merged$lfs16.p == 6, "1","0")
table(data_all_merged$al.p)

# Erstellung Variable Dauer AL
#1.Variable aus Datensatz extrahieren

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


#alle Missings definieren laut https://data.soep.de/soep-core --> Negative Values für uns unbrauchbar
#-1 	no answer /don`t know
#-2 	does not apply
#-3 	implausible value
#-4 	inadmissible multiple response
#-5 	not included in this version of the questionnaire
#-6 	version of questionnaire with modified filtering
#-8 	question not part of the survey programm this year*

data_all_merged <- data_all_merged %>% mutate_each(funs(replace(., .<0, NA)))

##Check, ob es noch irgendwo negative Werte gibt über "min"


##Für unsere Hauptvariablen entfernen wir alle Personen, die NAs haben. Konkret sehen wir: bgpbirthy (2 NAs) bgp175 (175 NAs) age_rec(4809 NAs, da wir Datensatz gecuttet haben)
##Kick der NAs bei bgp175
nrow(data_all_merged)
## 29178 (Datensatz gesamt) - 175 (NAs) = 29003 (Fälle die übrig bleiben sollten)
data_all_merged <- data_all_merged[!is.na(data_all_merged$bgp175),]
##Keine Missings mehr vorhanden und die 29003 Fälle wie erwartet
summary(data_all_merged$bgp175)

##29003 - 2 = 29001
data_all_merged <- data_all_merged[!is.na(data_all_merged$bgpbirthy),]
nrow(data_all_merged)

##Aussortieren aller Menschen über 65 Jahren, da wir dort Renteneintrittsalter haben und der Effekt von Arbeitslosigkeit danach 
## nichtmehr sauber gemessen werden kann. Um die Gruppen gleich zu halten und Verzerrungseffekte mit (Alter <-> Lebenszufriedenheit) zu vermeiden (4807)
## 29001-4807=24194

data_all_wage <- data_all_merged # save dataframe without age limitation

data_all_merged <- data_all_merged[data_all_merged$age<66,]
summary(data_all_merged)


#Erstellen von Subset nur Arbeitsloser

##Crosstabs, um die richte Auswahl Arbeitsloser zu treffen, da es zwei generierte Variablen hierfür im SOEP gibt (stib16 und lfs16): 
##--> lfs16 für uns besser geeignet, da detailliertere Unterscheidung zwischen Nicht-Arbeitenden getroffen wird (bspw. Maternity Leave usw.)
##structure(data_all_merged$lfs16)
# library(gmodels)
# CrossTable(data_all_merged$stib16,data_all_merged$lfs16, digits=0, format=c("SAS","SPSS"),prop.r=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE)

#Wie viele Fälle gibt es mit Arbeitslosen? --> 1256
summary(data_all_merged$lfs16==6)

#Nimm alle Faelle, in deren Zeile (row) "6" vermerkt ist, ebenso wie alle Spalten aus dem alten Datensatz
al <- data_all_merged[data_all_merged$lfs16==6,]
nrow(al) #Check 1256

#Datensatz mit allen anderen Faellen, also allen Personen, die nicht AL sind
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

##Check, ob alle Zahlen übernommen: Quersumme gecheckt 22893 Fälle von Nicht-Arbeitslosen, siehe nrow(nal)
##nrow(data_all_merged) --> 24194 (Alle aus data_all_merged)  - 45 (Nicht-Arbeitende >= 65) - 1256 (Arbeitslos gemeldete) = 22893 (Nicht-Arbeitslose)
table(data_all_merged$lfs16)
table(nal$lfs16)


#####Erstellung von Datensatz mit Variablen aus pglong. Hinzufügen von lfs und status ####
#pgenlong <- read_dta("C:/Users/Dietmar/Desktop/Master SozOek/Semester II/Forschungspraktikum/SOEP_RawData_long/SOEP neu/Teil1/Teil1/pgen.dta")
#save(pgenlong, file="./data/pgenlong.RDA")
load("./data/pgenlong.RDA")

##Get labour force status from year 2016 and 2015
library(reshape2)
lfs1516 <- pgenlong[pgenlong$syear==2016 | pgenlong$syear==2015,c("pid", "syear", "pglfs")]
lfs1516 <- dcast(lfs1516, pid ~ syear, value.var="pglfs")

##ziehe al_ids raus (Nur zur Überprüfung)
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

al_ids$longune <- ifelse(al_ids$lfs15!=6,"short","long")
table(al_ids$longune, useNA = "ifany")
al_ids$longune <- as.factor(al_ids$longune)
str(al_ids$longune)

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

##
summary(al)
summary(nal)

###ZWISCHENERGEBNIS###
# Zusammenfassung der Stichprobe
# al:   1256  Arbeitslose Personen, die jünger sind als 66 Jahre
# nal:  22893 Nicht-Arbeitslose Personen, die jünger sind als 66 Jahre

#####------------- Indexkonstruktion Life Satisfaction ---------######


#####-------------- Standardisierung (Z-Scores) ----------------#######
# al$bgp175_z <- scale(al$bgp175)
# nal$bgp175_z <- scale(nal$bgp175)
# hist(al$bgp175_z)
# hist(nal$bgp175_z)
#####Inter-Item-Korrelationsmatrix#####
# library(corrr)
# ##Zeige Spaltennummern
# View(colnames(al))
# ##Korrelationsmatrix für alle Items der Batterie "Lebenszufriedenheit" bei Arbeitslosen
# cor_al <- correlate(al[,c(11:22)], use="complete.obs")
# ##Korrelationsmatrix für alle Items der Batterie "Lebenszufriedenheit" bei Arbeitslosen
# cor_nal <- correlate(nal[,c(11:22)], use="complete.obs")
# cor_ana <- cor_al-cor_nal
# ##
# library(xlsx)
# write.xlsx(cor_al, "./cor_al.xlsx")


#####Deskriptive Statistik und bildliche Darstellung #####
##Nachfolgend einige Berechnungen zu den Kernvariablen "bgp175", "bgpsex", "age_rec" jeweils aufgegliedert für die Datensaetze "al" und "nal"


#Haeufigkeiten berechnen
table(al$bgp175)
table(nal$bgp175)

by(al$bgp175, nal$bgp175, table)

table(al$bgpsex)
table(nal$bgpsex)

table(al$age_rec)
table(nal$age_rec)

#Histogramme berechnen für numerische Variablen
hist(al$bgp175)
hist(nal$bgp175)

hist(al$age)
hist(nal$age)

#falls man Histogramm speichern moechte
#pdf("al_bgp175.pdf") 
#hist(al$bgp175) 
#dev.off()

#Barcharts berechnen für String Variablen
barplot(table(al$age_rec),xlab="Lebensphasen")

barplot(table(nal$age_rec),xlab="Lebensphasen") 

barplot(table(al$bgpsex),xlab="Geschlecht")

barplot(table(nal$bgpsex),xlab="Geschlecht") 

#### Auswahl ueblicher Statistiken
summary(al$bgp175)
summary(nal$bgp175)

###etwas ausfuehrlichere Statistiken 
# install.packages("fBasics")
# library(fBasics)
# basicStats(al$bgp175)
# basicStats(nal$bgp175)
# skewness = Schiefe, neg --> rechssteil/linksschief/nach rechts geneigt; pos --> linkssteil/rechtsschief/nach links geneigt
# Kurtosis = Wie spitz ist Verteilung?, 0=normalgipflig, >0 steilgipflig, <0 flachgipflig

#data_all_wage$age_rec2 <- cut(data_all_wage$age, seq(from = 0, to = 110, by = 5))

#####Deskriptive Analyse (plots) des Gesamtzusammenhangs#####
##Erstellung neuer detaillierterer Altersgruppierung in 5er Schritten
al$age_rec2 <- cut(al$age, seq(from = 0, to = 110, by = 5))
nal$age_rec2 <- cut(nal$age, seq(from = 0, to = 110, by = 5))

library(ggplot2)
##Lebenszufriedenheit ohne Altersgruppen
library(cowplot)

##Overall Trend age x bgp175
overall <- ggplot(data_all_wage %>% group_by(age) %>% summarise(mean(bgp175)), aes(x = age, y = `mean(bgp175)`)) + geom_point() +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  theme_bw() + 
  scale_y_continuous (name = "Lebenszufriedenheit", limits = c(5,10), breaks = c(5:10)) +
  scale_x_continuous (name = "Alter", limits = c(18,100), breaks = c(seq(20,100,10))) +
  scale_colour_brewer(name = 'Trendline', palette = 'Set2') +
  ggtitle("overall") +
  theme(legend.position="none")

##Only AL
a <- overall %+% (data_all_wage %>% filter(lfs16==6) %>% group_by(age) %>% summarise(mean(bgp175))) + ggtitle("only al")

##Only NAL
b <- overall %+% (data_all_wage %>% filter(lfs16!=6) %>% group_by(age) %>% summarise(mean(bgp175))) + ggtitle("only nal")

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


##Overall plot LP####

##5er Schritte (age_rec2)
  
i <-   ggplot(al %>%  group_by(age_rec2) %>% summarise(mean(bgp175)), aes(x = age_rec2, y = `mean(bgp175)`, group=1)) +
    geom_point() +
    geom_line() +
    theme_bw() + 
    scale_y_continuous (name = "Lebenszufriedenheit", limits = c(5,10), breaks = c(5:10)) +
    scale_x_discrete (name = "Alter") + ggtitle("5er Schritte AL")

ii <- i %+% (nal %>%  group_by(age_rec2) %>% summarise(mean(bgp175))) + ggtitle("5er Schritte NAL")

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

#####Gesamtzusammenhang MW/Median Standardabweichung für 5er Gruppen#####
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


#####Deskriptive Analyse (plots) der Drittvariablen LEBENSPHASENMODELL#####
####Prepare dataset for plotting (convertion to factors) ###
##Erstellung von sex als factor
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

al$d1110716_rec <-  as.factor(al$d1110716_rec)
levels(al$d1110716_rec )[levels(al$d1110716_rec)=="0"] <- "no child"
levels(al$d1110716_rec)[levels(al$d1110716_rec)=="1"] <- "with child"

nal$d1110716_rec <-  as.factor(nal$d1110716_rec)
levels(nal$d1110716_rec )[levels(nal$d1110716_rec)=="0"] <- "no child"
levels(nal$d1110716_rec)[levels(nal$d1110716_rec)=="1"] <- "with child"

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

#Create Variable unemployment Experience (expue_rec)
hist(al$expue16)
summary(al$expue16)

al %>% filter(!is.na(expue16)) %>% group_by(age_rec2) %>% summarize(mean(expue16))
#al$expue_rec <- ifelse((al$expft16 == 0 & al$exppt16 == 0), NA, (al$expue16/ (al$expft16 + al$exppt16 + al$expue16)))
al$expue_rec <- al$expue16/ (al$expft16 + al$exppt16 + al$expue16)

#cut in quantiles (.0 - .33, - .66, - .1)
al$expue_rec2 <- cut(al$expue_rec, breaks = c(quantile(al$expue_rec, c(0.33,0.66,1), na.rm = TRUE)), labels = c("low_expue","middle_expue","high_expue"))

View(al[,c(28:30,45,46,23)])
is.factor(al$expue_rec2)



###create dataframes for plots with life phase model####
require(cowplot)
#1 - sex
al_plot1 <- al %>% filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, bgpsex) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot1 <- nal %>% filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, bgpsex) %>% summarise(mean(bgp175),sd(bgp175))

#2 - migration
al_plot2 <- al %>% filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, migback_rec) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot2 <- nal %>% filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, migback_rec) %>% summarise(mean(bgp175),sd(bgp175))

#3 - Children
al_plot3 <- al %>%  filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, d1110716_rec ) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot3 <- nal %>%  filter(!is.na(pgisced11_rec)) %>% group_by(age_rec, d1110716_rec) %>% summarise(mean(bgp175),sd(bgp175))

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
al_plot3 <- al %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, d1110716_rec ) %>% summarise(mean(bgp175),sd(bgp175))
nal_plot3 <- nal %>% filter(!is.na(pgisced11_rec)) %>%  group_by(age_rec2, d1110716_rec) %>% summarise(mean(bgp175),sd(bgp175))

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

###1 - Partner Arbeitslosigkeit

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


###3 - Anteil der Arbeitslosigkeitsdauer an der dem Arbeitsmarkt zur Verfügung stehenden Gesamtzeit (s. al$expue_rec2)
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

#####Andere deskriptive Statistik#####

### Normalverteilung? --> Shapiro-Test oder KS-Test (Kolmogorov-Smirnov)
ks.test(al$bgp175, pnorm, mean=1, sd=2) #wenn signifikant, dann keine Normalverteilung
ks.test(nal$bgp175, pnorm, mean=1, sd=2)

ks.test(nal$bgp175_z, pnorm, mean=1, sd=2)
ks.test(al$bgp175_z, pnorm, mean=1, sd=2)


shapiro.test(al$bgp175) #wenn signifikant, dann keine Normalverteilung, aber problematisch: großes Sample

##Nice looking barplot (maybe delete if not needed)####
#plot1 <- ggplot(al, aes(x = bgpsex, y = bgp175, group = bgpsex, colour = bgpsex)) + geom_boxplot() + 
#facet_wrap(~ age_rec, ncol = 3)

###Deskriptive Statistik für großen Zusammenhang
###Several Error Bar Charts
# library(Rmisc)
# 
# ##AL Group Only
# al_errorBar <- summarySE(al, measurevar="bgp175", groupvars="age_rec")
# pd <- position_dodge(0.1) # move them .05 to the left and right
# ##NAL Group Only
# nal_errorBar <- summarySE(nal, measurevar="bgp175", groupvars="age_rec")
# 
# ##AL and NAL Group together
# result<-rbind(al_errorBar, nal_errorBar)
# ###create new column for groups
# result$groups <- c("al", "al", "al", "nal","nal","nal")
# result$groups <- factor(result$groups, levels= c("al", "nal"))
# is.factor(result$groups)
# 
# ##Both Groups in one chart
# ggplot(result, aes(x=age_rec, y=bgp175, colour=groups, group=groups)) +
#   geom_errorbar(aes(ymin=bgp175-ci, ymax=bgp175+ci), width=.1, position=pd) +
#   geom_line(position=pd) +
#   geom_point(position=pd) + xlab("Lebensphasen") + ylab("Zufriedenheit 1-10") +
#   scale_y_continuous(name = "Zufriedenheit", limits = c(1,10), breaks = c(1:10))+
#   scale_x_discrete(name = "Lebensphasen",labels=c("lp1","lp2","lp3"))
# 
# detach(package:Rmisc, unload = TRUE)
#####Regression for al ####

#Erstellen von contrasts, um unsere Hypothesen zu überprüfen (Hypothese)
#Set contrast to group 2
contrasts(al$age_rec) <- contr.treatment(3, base = 2)
almodel_regcon <- lm(bgp175 ~ age_rec, data = al)
summary(almodel_regcon)

almodel_regcon <- lm(bgp175 ~ age_rec + bgpsex_fac, data = al)
summary(almodel_regcon)

##make sex to factor variable for dummy coding in regression
al$bgpsex_fac <- as.factor(al$bgpsex) 
is.factor(al$bgpsex_fac)
table(al$bgpsex, al$bgpsex_fac)


##Different contrasts: Group 1 as baseline
contrasts(al$age_rec) <- contr.treatment(3, base = 1)
almodel_regcon <- lm(bgp175 ~ age_rec + bgpsex_fac +migback_rec+ d1110716_rec, data = al)
summary(almodel_regcon)

almodel_regcon2 <- lm(bgp175 ~ age, data = al)
summary(almodel_regcon2)

###Mittelwerte der jeweiligen Gruppe
round(tapply(al$bgp175, al$age_rec, mean, na.rm=TRUE), 2)

###Regression only control variables####

summary(lm(bgp175 ~ pgisced11_rec + bgpsex_fac + migback_rec + d1110716_rec + longune  + al.p, data = al))

#####ANOVA first try --------------##########

#1. Levene-Test auf Varianzhomogenitaet, sollte nicht signifikant sein
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

leveneTest(al$bgp175, al$age_rec, center=mean) # al hat Varianzhomogenität (für mean und median)
leveneTest(nal$bgp175, nal$age_rec, center=mean) # nal hat Varianzheterogenität (für mean und median)

by(al$bgp175, al$age_rec, var)

#Warning: In large samples Leveneâs test can be significant even when group variances are not very different. Therefore, it should be interpreted in conjunction with the variance ratio.
#When homogeneity of variance is the problem there are versions of the F-ratio that have been derived to be robust when homogeneity of variance has been violated. One that can be implemented in R is Welchâs F (1951)
#If you have distributional problems, then there are robust (see section 5.8.4) variants of ANOVA that have been implemented in R by Wilcox (2005)
#The one-way independent ANOVA has a non-parametric counterpart called the KruskalâWallis test. If you have non-normally distributed data, or have violated some other assumption, then this test can be a useful way around the problem. This test is described in Chapter 15.

##Double Check für signifikante Levene Tests (nur wenn auch Normalverteilung gegeben ist https://cran.r-project.org/web/packages/SuppDists/SuppDists.pdf und Gruppen einigermaßen gleich groß http://www.statisticshowto.com/fmax-hartleys-test/)
# when the sample size is large, small differences in group variances can produce a Levene’s test that is significant 
# (because, as we saw in Chapter 1, the power of the test is improved). A useful double check, therefore, is to look at 
# Hartley’s Fmax – also known as the variance ratio (Pearson & Hartley, 1954). This is the ratio of the variances between 
# the group with the biggest variance and the group with the smallest variance.

##Had this test been significant, we could instead conduct and report the results of Welchâs F or a robust version of ANOVA, which weâll cover in the next section.

#install.packages("SuppDists")
library(SuppDists)

#pmaxFratio(al$bgp175, 2, 3)
ratio <- max(by(nal$bgp175, nal$age_rec, var))/min(by(nal$bgp175, nal$age_rec, var)) # ernstzunehmen, obwohl die Gruppen unterschiedlich groß und nicht normalverteilt?

pmaxFratio(ratio, (nrow(nal)-1), 3)

##newModel<-aov(outcome ~ predictor(s), data = dataFrame, na.action = an action))

##Anova 

almodel_aov <- aov(bgp175 ~ age_rec, data=al)
summary(almodel_aov) #Signifikant
plot(almodel_aov)

##Graph1: Levene: Sieht gut aus und Levene bestÃ¤tigt
##Graph2: Shapiro: Sieht auch ganz gut aus, aber shapiro sagt: keine Normalverteilung --> robuste Anova?

nalmodel_aov <- aov(bgp175 ~ age_rec, data=nal)
summary(nalmodel_aov) # signifikant
plot(nalmodel_aov)

##Graph1: Levene: Sieht gut aus, aber Levene sagt eigentlich keine VarianzhomogenitÃ¤t (TODO: Weitere ÃberprÃ¼fung notwendig)
##Graph2: Shapiro: Sieht schon sehr weit weg von der Diagonale aus und Shapiro bestÃ¤tigt keine Normalverteilung --> robuste Anova?

#the plot we have shows points that are equally spread for the three groups, 
#which implies that variances are similar across groups (which was also the conclusion reached by Levenes test). 
#The second plot (on the right) is a Q-Q plot (see Chapter 5), which tells us something about the normality of 
#residuals in the model. We want our residuals to be normally distributed, which means that the dots on the graph 
#should cling lovingly to the diagonal line. Ours look like they have had a bit of an argument with the diagonal line, 
#which suggests that we may not be able to assume normality of errors and should perhaps use a robust version of 
#ANOVA instead.

##Wenn VarianzhomogenitÃ¤t verletzt ist: Welchs F-Ratio ansehen --> Machen wir für nal

oneway.test(bgp175 ~ age_rec, data=nal) ## Auch signifikant 

#-> Hochsignifikant, also gibt es wohl auch Unterschiede in den Gruppen bei den Nicht-Arbeitslosen

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
t1way(alWide) # Getrimmter Mittelwert (20% Cut)           ---> Nicht Signifikant
medpb(alWide) # Median Vergleich                          ---> Signifikant
t1waybt(alWide) # add a bootstrap to the trimmed mean     ---> Nicht Signifikant

nalWide <- unstack(nal, bgp175 ~ age_rec)             
t1way(nalWide) # ---> Nicht signifikant 
medpb(nalWide) # ---> Signifikant
t1waybt(nalWide) # ---> Nicht signifikant

#####Post-Hoc Tests AL#####

pairwise.t.test(al$bgp175, al$age_rec, p.adjust.method = "bonferroni") # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2
pairwise.t.test(al$bgp175, al$age_rec, p.adjust.method = "BH") # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2
library(multcomp)

alPosthoc <- glht(almodel_aov, linfct = mcp(age_rec = "Tukey")) # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2
#alPosthoc <- glht(almodel_aov, linfct = mcp(age_rec = "Dunnett"))
summary(alPosthoc)
confint(alPosthoc)

###Robust-Post-Hoc-Tests

lincon(alWide, tr = .1) # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2
mcppb20(alWide, tr = .1, crit = 0.5 ) # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2

#scheinbar ein Unterschied zwischen Gruppe 3 und Gruppe 1/2, aber kein Unterschied zwischen Gruppe 1 und Gruppe 2
#####Zusammenfassung Post-Hoc Tests #####
#Alle Post-Hoc Tests haben selbes Ergebnis für Arbeitslose

#####Post-Hoc Tests NAL#####

pairwise.t.test(nal$bgp175, nal$age_rec, p.adjust.method = "bonferroni") # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2
pairwise.t.test(nal$bgp175, nal$age_rec, p.adjust.method = "BH") # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2
library(multcomp)

nalPosthoc <- glht(nalmodel_aov, linfct = mcp(age_rec = "Tukey")) # Unterschiede zwischen allen Gruppen
#nalPosthoc <- glht(nalmodel_aov, linfct = mcp(age_rec = "Dunnett"))
summary(nalPosthoc)
confint(nalPosthoc)

###Robust-Post-Hoc-Tests

lincon(nalWide, tr = .1) # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2
mcppb20(nalWide, tr = .1, crit = 0.5 ) # Unterschiede Gruppe 1-3 und 2-3, nicht aber 1-2


#####Zusammenfassung Post-Hoc Tests NAL: 
#Alle Post-Hoc Tests (außer Tukey) haben selbes Ergebnis für Nicht-Arbeitslose
#Im Vergleich zu AL ergeben sich die selben Unterschiede zwischen den Gruppen, sodass die "Alten" sich wohl gegenüber allen
#jüngeren Gruppen unterscheiden

#####Berechnung der Effektstärke Omega-Square####

##Kalkuliere OMEGAÂ² anstatt Eta squaredÂ²

#However, this measure of effect size is slightly biased because it is based purely on sums of squares 
#from the sample and no adjustment is made for the fact that weâre trying to estimate the effect size 
#in the population. Therefore, we often use a slightly more complex measure called omega squared (Ï2).


# summary(almodel_aov)
# 
# alOmega <- (70-(2*4.3))/(5508+4.34)
# 70+5438
# 
# 
# ##EffektstÃ¤rke fÃ¼r jede Gruppe
# ##TODO: Alle labelled zu numerisch umwandeln...
# library(pastecs)
# al$bgp175 <- as.numeric(al$bgp175)
# 
# by(al$bgp175,al$age_rec, stat.desc)
# #mes(meangroup1, meangroup2, sdgroup1, sdgroup2, ngroup1, ngroup2)
# #d = mean difference
# #r = correlation
# 
# #1-2
# mes(6.6892430, 6.490415, 1.9877233, 2.122807, 251, 626)
# 
# #1-3
# mes(6.6892430, 6.0606860, 1.9877233, 2.0788709, 251, 432)
# 
# #2-3
# mes(6.490415, 6.0606860, 2.122807, 2.0788709, 626, 432)



#####Regression for fun#####

#Regression (al)

almodel_reg <- lm(bgp175~age_rec, data=al) #lp1 *** - lp2 nicht sig - lp3 ***, Gesamtes Modell sig., aber kleines r 0.1 
summary(almodel_reg)
plot(almodel_reg)

#Regression (nal)

nalmodel_reg <- lm(bgp175~age_rec, data=nal) #lp1 *** - lp2 ** - lp3 ***, Gesamtes Modell signifikant, aber winziges R²  
summary(nalmodel_reg)
plot(nalmodel_reg)

##Alternative Regression auf Basis der ANOVA
summary.lm(almodel_aov)
summary.lm(nalmodel_aov)



#####Regression for nal ##### 

##make sex to factor variable for dummy coding in regression
nal$bgpsex_fac <- as.factor(nal$bgpsex) 
is.factor(nal$bgpsex_fac)
table(nal$bgpsex, nal$bgpsex_fac)

contrasts(nal$age_rec) <- contr.treatment(3, base = 2)
nalmodel_regcon <- lm(bgp175 ~ age_rec + bgpsex_fac, data = nal)
summary(nalmodel_regcon)

#####Logistische Regression#####
#install.packages("mlogit")
library(mlogit)

almodel_log <- mlogit(bgp175 ~ 1 | age_rec, data = al, reflevel = 1)

###Non-parametrischer Test####
kruskal.test(bgp175 ~ age_rec ,data = al) #Signifikante Unterschiede vorhanden
kruskal.test(bgp175 ~ age_rec ,data = nal) #Signifikante Unterschiede vorhanden


##Erstelle Ranks zur Übersichtlichkeit
al$ranks <- rank(al$bgp175)
by(al$ranks, al$age_rec, mean)

nal$ranks <- rank(nal$bgp175)
by(nal$ranks, nal$age_rec, mean)


##Non-parametrischer Post-Hoc Test
# One way to do non-parametric post hoc procedures is essentially the same as doing Wilcoxon rank-sum tests on 
# all possible comparisons. This method is described by Siegel and Castellan (1988) and involves taking the difference 
# between the mean ranks of the different groups and comparing this to a value based on the value of z (corrected for 
# the number of comparisons being done) and a constant based on the total sample size and the sample size in the two 
# groups being compared.

# install.packages("pgirmess")
library(pgirmess)

kruskalmc(bgp175 ~ age_rec ,data = al) #Signifikante Unterschiede nur zwischen Gruppe 1-3 und 2-3, nicht aber für 1-2

kruskalmc(bgp175 ~ age_rec ,data = nal) # Signifikante Unterschiede zwischen allen Gruppen, ABER: Sieht so aus als wäre die kritischen Werte merkwürdig

##repeat with smaller sample

new_df <- nal %>% group_by(age_rec) %>% sample_n(500)
table(new_df$age_rec)

kruskal.test(bgp175 ~ age_rec ,data = new_df)
kruskalmc(bgp175 ~ age_rec ,data = new_df)
plot(density(new_df$bgp175))

##Frage: Wie können wir jetzt den Alterseffekt herausrechnen?

a <- by(al$bgp175, al$age_rec, mean)
b <- by(nal$bgp175, nal$age_rec, mean)

a-b
