#############################################################################
## Einlesen der Daten ##

library("rmarkdown")

# Daten laden
setwd("~/Desktop/Studium/Psychologie/Bachelorarbeit/Data")
load("~/Desktop/Studium/Psychologie/Bachelorarbeit/Data/EVS_WVS_Joint_v2_0.rdata")

# Selektion der Variablen Lebenszufriedenheit, Nation und 
# selbstberichtete Gesundheit
dat<-na.omit(EVS_WVS_Joint_v2_0[,c("cntry_AN", "A009", "A170")])
save(dat, file="dat.rda" )

# Umbenennung der Spaltennamen --> SH = Subjective Health,  
# LS = Life Satisfaction
colnames(dat)<-c("Country", "SH", "LS")

# Löschen der Länder, die nicht in der verwendeten Typologie für 
# Gesundheitssysteme vorkommen 
library(dplyr)
data <- dat %>% 
  filter(Country == 'AU' | Country == 'AT' | Country == 'DK' | 
           Country == 'FI' | Country == 'IS' | Country == 'NO' | 
           Country == 'SE' | Country == 'PT' | Country == 'ES' | 
           Country == 'GB' | Country == 'CA' | Country == 'NZ' |
           Country == 'IT' | Country == 'DE' | Country == 'CH' | 
           Country == 'US' | Country == 'EE' | Country == 'FR' | 
           Country == 'CZ' | Country == 'HU' | Country == 'NL' | 
           Country == 'PL' | Country == 'SK' | Country == 'JP' | 
           Country == 'KR')

# Erstellung der Dummy-Variablen
# National Health Service als baseline, dazu vier Variablen für 
# National Health Insurance, Social Health Insurance, Private Health System 
# und Etatist Social Health Insurance
data$NHI <- ifelse(data$Country %in% c('AU', 'CA', 'NZ', 'IT'), 1, 0)
data$SHI <- ifelse(data$Country %in% c('AT', 'DE', 'CH'), 1, 0)
data$PHS <- ifelse(data$Country == 'US', 1, 0)
data$ESHI <- ifelse(data$Country %in% c('EE', 'FR', 'CZ', 'HU', 'NL', 'PL',
                                        'SK', 'JP', 'KR'), 1, 0)

# BIP-Daten importieren 
GDP <- read.csv("~/Desktop/Studium/Psychologie/Bachelorarbeit/Data/OECD_GDP.csv", 
                     sep=";")

# Berechnung des BIP-Mittelwerts über die Dauer der Erhebungswelle
# (2021 wurde ausgelassen, da nicht für jedes Land Daten verfügbar waren)
GDP$GDP_mean <- rowMeans(GDP[, c("X2017","X2018","X2019", "X2020")], 
                         na.rm = TRUE)

# Einfürgen des BIP-Mittelwerts in den Datensatz
data <- merge(x = data, y = GDP[ , c("Country", "GDP_mean")], 
              by = "Country", all.x=TRUE)
save(data, file="data.rda" )


#############################################################################
## Anpassen und Zentrieren der Daten ##

# Ausschließen von Missings
summary(data)
# Recode von -1, -2, -3, -4 und -5 zu NA für Variable SH (Verschiedene 
# Gründe für Nichtantwort) 
data$SH[data$SH == -1] <- NA
data$SH[data$SH == -2] <- NA
data$SH[data$SH == -3] <- NA
data$SH[data$SH == -4] <- NA
data$SH[data$SH == -5] <- NA

# Recode von -1, -2, -3, -4 und -5 zu NA für Variable LS (Verschiedene 
# Gründe für Nichtantwort) 
data$LS[data$LS == -1] <- NA
data$LS[data$LS == -2] <- NA
data$LS[data$LS == -3] <- NA
data$LS[data$LS == -4] <- NA
data$LS[data$LS == -5] <- NA
# Überprüfen
summary(data)

# Wie viele Missings sind vorhanden
nrow(data[!complete.cases(data),])
# Missings löschen
data <- na.omit(data)
# Überprüfen
nrow(data[!complete.cases(data),])

# Umpolen der Variable SH
library(car)
data$SH_r<-recode(data$SH,
          recodes = "1=5; 2=4; 3=3; 4=2; 5=1")

# Stichprobengröße pro Land
Country_samples <- data %>%
  group_by(Country) %>%
  summarise(Count = n())
Country_samples

# Grand-Mean-Zentrierung der Variable GDP_mean (mit Typ 2 Zentrierung, 
# um Verzerrung durch Unterschiede in den Länderstichprobengrößen 
# zu vermeiden)
# Berechnung der Mittelwerte pro Land
gdp <- tapply(data$GDP_mean, data$Country, mean, na.rm = TRUE)
head(gdp)
# Berechnung des Mittewerts
gdp_grandmean <- mean(gdp)
# Grand-Mean-Zentrierung von GDP_mean am Gesamtmittelwert
data$gdp_cgm <- data$GDP_mean - gdp_grandmean
names(data)
# Check: Mittelwert der zentrierten Level-2 Kovariate auf 
# Level-2 sollte Null sein
mean(tapply(data$gdp_cgm, data$Country, mean, na.rm = TRUE))

# Group-Mean-Zentrierung von SH
# Berechnung der länderspezifischen Mittelwerte
SH_group_means<-aggregate(data$SH_r, list(data$Country), mean)
colnames(SH_group_means)<-c("Country", "SH_group_mean")
data<-merge(data, SH_group_means, "Country")
# Group-Mean-Zentrierung auf Basis der berechneten
# Gruppenmittelwerte 
data$SH_cwc<-data$SH_r-data$SH_group_mean
names(data)

save(data, file= "data.RData")

# Deskriptive Daten der Variablen
summary(data)
sd(data$LS)
sd(data$SH_r)
sd(data$SH_group_mean)
sd(data$GDP_mean)
LS_means <- tapply(data$LS, data$Country, mean)
range(LS_means)
range(data$SH_group_mean)


#############################################################################
## Aufstellen der Modelle ##

library(lme4)
library(lmerTest)
library(ICC)

# Anderer optimizer verwendet, um Warnungen zu verhindern
# Modell 0: Ramdom-Intercept-Only-Modell
m0 <- lmer(LS ~ 1 + (1|Country), data = data, REML = FALSE,
           control = lmerControl(optimizer ="Nelder_Mead"))
summary(m0)
# Die geschätzte mittlere Lebenszufriedenheit ist 
# g00 = 7.53, t(24.99) = 94.11, p > .01

# Berechnung des Intraklassenkorrelationskoeffizienten
ICCest(as.factor(data$Country),data$LS, alpha= 0.05, CI.type="THD")
# Test der ICC auf Signifikanz mit dem ANOVA-Paket
ICC.mod <-aov(LS ~ as.factor(Country),data = data)
summary(ICC.mod)
# signifikant
# Geschätzte 4,8% der Gesamtvarianz der Variable Lebenszufriedenheit gehen 
# auf Unterschiede zwischen den Ländern zurrück

# Modell 1: BIP, SH_cwc,  SH_group_mean
m1 <- lmer(LS ~ 1 + SH_cwc + SH_group_mean + gdp_cgm + (1 + SH_cwc|Country), 
           data, REML = FALSE, 
           control = lmerControl(optimizer ="Nelder_Mead"))
summary(m1)

# Modell 2: BIP, SH_cwc,  SH_group_mean + Dummies +
# Interaktionen Dummies mit SH_cwc
m2 <- lmer(LS ~ 1 + SH_cwc + SH_group_mean + gdp_cgm + NHI + SHI + PHS + ESHI
           + NHI*SH_cwc + SHI*SH_cwc + PHS*SH_cwc + ESHI*SH_cwc
           + (1 + SH_cwc|Country), data, REML = FALSE, 
           control = lmerControl(optimizer ="Nelder_Mead"))
summary(m2)
# Korrelation zwischen Intercepts (LS) und Slopes (SH_cgm) beträgt -0.41
# Daraus folgt, dass bei steigender LS SH einen immer geringeren 
# Einfluss auf LS hat

# Modellvergleich m0 mit m1
# Veränderung in Fixed Effect --> FML
anova(m0, m1)
# m1 zeigt besseren Fit

# Modellvergleich m1 mit m2
# Veränderung in Fixed Effect --> FML
anova(m1, m2)
# m2 zeigt besseren Fit

# Varianzaufklärung
# Anteilige Reduktion von Level-1-Residualvarianz durch Inklusion von 
# SH in Model 1
Rx_sq <- (3.199-2.703)/3.199
Rx_sq

# Anteilige Reduktion von Level-2-Residualvarianz durch Inklusion von 
# GDP in Model 1
Rl2_sq1 <- (0.155-0.096)/0.155
Rl2_sq1

# Anteilige Reduktion von Level-2-Residualvarianz durch Inklusion von 
# Gesundheitssystemen und der Interaktion zwischen Gesundheitssystemen 
# und SH in Model 2
Rl2_sq2 <- (0.096-0.056)/0.096
Rl2_sq2


#############################################################################
## Datenvisualisierung ##

library(lattice)
library(lme4)

# Lebenszufriedenheit in den einzelnen Staaten
boxplot(LS ~ Country, data = data)
boxplot(SH_r ~ Country, data = data)

# Random-Effects
dotplot(ranef(m2), font.size=0.3)

# Random-Coefficients
dotplot(coef(m2))
