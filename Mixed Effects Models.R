##### loading packages
library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(car)
library(carData)
library(histogram)
library(ggplot2)
library(multcomp) # for Tukey test

#################### mixed effects model for seasonal data from 2019 #################
##### importing data
mastersheet <- file.choose()
Achenkirch <- read.csv(mastersheet,header=T,dec=",",sep=";") ### This is for German system. English system is dec="." and sep=",".
Achenkirch[Achenkirch==""] <- NA
str(Achenkirch)
head(Achenkirch)# looking at the data 

##### setting variables as factors 
Achenkirch$Depth <- as.factor(Achenkirch$Depth)
Achenkirch[,"Block"] <- as.factor(Achenkirch[,"Block"])
Achenkirch$Start <- as.factor(Achenkirch$Start)
Achenkirch$Duration <- as.numeric(Achenkirch$Duration)

##### adding a column for date (numerical season) and for warming time (numerical time since warming started)
Achenkirch <- Achenkirch %>%
  mutate(Date = case_when(Season == "May" ~ 1,
                          Season == "Aug" ~ 2,
                          Season == "Oct" ~ 3))

##### checking homeogenity
bartlett.test(Illite~Warming,Achenkirch)
bartlett.test(Illite~Depth,Achenkirch)
bartlett.test(Illite~Season,Achenkirch)   ### original

bartlett.test(sqrt(Achenkirch$Illite)~Achenkirch$Warming) 
bartlett.test(sqrt(Achenkirch$Illite)~Achenkirch$Depth) 
bartlett.test(sqrt(Achenkirch$Illite)~Achenkirch$Season)   ### sqrt

bartlett.test(log(Achenkirch$Illite)~Achenkirch$Warming)
bartlett.test(log(Achenkirch$Illite)~Achenkirch$Depth)
bartlett.test(log(Achenkirch$Illite)~Achenkirch$Season)  ### log

##### creating a window for figures, which check normal distribution
par(mfrow = c(3,7))
par("mar")
par(mar=c(1,1,1,1))
hist(Achenkirch$Illite[Achenkirch$Warming=="warmed"],breaks = 10,xlab="Illite", ylab="number", main = "warmed")
hist(Achenkirch$Illite[Achenkirch$Warming=="control"],breaks = 10,xlab="Illite", ylab="number", main = "control")
hist(Achenkirch$Illite[Achenkirch$Depth=="10"],breaks = 10,xlab="Illite", ylab="number", main = "10")
hist(Achenkirch$Illite[Achenkirch$Depth=="20"],breaks = 10,xlab="Illite", ylab="number", main = "20")
hist(Achenkirch$Illite[Achenkirch$Season=="May"],breaks = 10,xlab="Illite", ylab="number", main = "May")
hist(Achenkirch$Illite[Achenkirch$Season=="Aug"],breaks = 10,xlab="Illite", ylab="number", main = "Aug")
hist(Achenkirch$Illite[Achenkirch$Season=="Oct"],breaks = 10,xlab="Illite", ylab="number", main = "Oct")
boxplot(Achenkirch$Illite[Achenkirch$Warming=="warmed"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Warming=="control"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Depth=="10"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Depth=="20"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Season=="May"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Season=="Aug"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Season=="Oct"], horizontal = TRUE)
qqnorm(Achenkirch$Illite[Achenkirch$Warming=="warmed"])
qqline(Achenkirch$Illite[Achenkirch$Warming=="warmed"])
qqnorm(Achenkirch$Illite[Achenkirch$Warming=="control"])
qqline(Achenkirch$Illite[Achenkirch$Warming=="control"])
qqnorm(Achenkirch$Illite[Achenkirch$Depth=="10"])
qqline(Achenkirch$Illite[Achenkirch$Depth=="10"])
qqnorm(Achenkirch$Illite[Achenkirch$Depth=="20"])
qqline(Achenkirch$Illite[Achenkirch$Depth=="20"])
qqnorm(Achenkirch$Illite[Achenkirch$Season=="May"])
qqline(Achenkirch$Illite[Achenkirch$Season=="May"])
qqnorm(Achenkirch$Illite[Achenkirch$Season=="Aug"])
qqline(Achenkirch$Illite[Achenkirch$Season=="Aug"])
qqnorm(Achenkirch$Illite[Achenkirch$Season=="Oct"])
qqline(Achenkirch$Illite[Achenkirch$Season=="Oct"])   ### original

par(mfrow = c(3,7))
par("mar")
par(mar=c(1,1,1,1))
hist(sqrt(Achenkirch$Illite[Achenkirch$Warming=="warmed"]),breaks = 10,xlab="Illite", ylab="number", main = "warmed")
hist(sqrt(Achenkirch$Illite[Achenkirch$Warming=="control"]),breaks = 10,xlab="Illite", ylab="number", main = "control")
hist(sqrt(Achenkirch$Illite[Achenkirch$Depth=="10"]),breaks = 10,xlab="Illite", ylab="number", main = "10")
hist(sqrt(Achenkirch$Illite[Achenkirch$Depth=="20"]),breaks = 10,xlab="Illite", ylab="number", main = "20")
hist(sqrt(Achenkirch$Illite[Achenkirch$Season=="May"]),breaks = 10,xlab="Illite", ylab="number", main = "May")
hist(sqrt(Achenkirch$Illite[Achenkirch$Season=="Aug"]),breaks = 10,xlab="Illite", ylab="number", main = "Aug")
hist(sqrt(Achenkirch$Illite[Achenkirch$Season=="Oct"]),breaks = 10,xlab="Illite", ylab="number", main = "Oct")
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Warming=="warmed"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Warming=="control"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Depth=="10"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Depth=="20"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Season=="May"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Season=="Aug"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Season=="Oct"]), horizontal = TRUE)
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Warming=="warmed"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Warming=="warmed"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Warming=="control"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Warming=="control"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Depth=="10"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Depth=="10"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Depth=="20"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Depth=="20"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Season=="May"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Season=="May"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Season=="Aug"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Season=="Aug"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Season=="Oct"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Season=="Oct"]))   ### sqrt

par(mfrow = c(3,7))
par("mar")
par(mar=c(1,1,1,1))
hist(log(Achenkirch$Illite[Achenkirch$Warming=="warmed"]),breaks = 10,xlab="Illite", ylab="number", main = "warmed")
hist(log(Achenkirch$Illite[Achenkirch$Warming=="control"]),breaks = 10,xlab="Illite", ylab="number", main = "control")
hist(log(Achenkirch$Illite[Achenkirch$Depth=="10"]),breaks = 10,xlab="Illite", ylab="number", main = "10")
hist(log(Achenkirch$Illite[Achenkirch$Depth=="20"]),breaks = 10,xlab="Illite", ylab="number", main = "20")
hist(log(Achenkirch$Illite[Achenkirch$Season=="May"]),breaks = 10,xlab="Illite", ylab="number", main = "May")
hist(log(Achenkirch$Illite[Achenkirch$Season=="Aug"]),breaks = 10,xlab="Illite", ylab="number", main = "Aug")
hist(log(Achenkirch$Illite[Achenkirch$Season=="Oct"]),breaks = 10,xlab="Illite", ylab="number", main = "Oct")
boxplot(log(Achenkirch$Illite[Achenkirch$Warming=="warmed"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Warming=="control"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Depth=="10"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Depth=="20"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Season=="May"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Season=="Aug"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Season=="Oct"]), horizontal = TRUE)
qqnorm(log(Achenkirch$Illite[Achenkirch$Warming=="warmed"]))
qqline(log(Achenkirch$Illite[Achenkirch$Warming=="warmed"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Warming=="control"]))
qqline(log(Achenkirch$Illite[Achenkirch$Warming=="control"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Depth=="10"]))
qqline(log(Achenkirch$Illite[Achenkirch$Depth=="10"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Depth=="20"]))
qqline(log(Achenkirch$Illite[Achenkirch$Depth=="20"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Season=="May"]))
qqline(log(Achenkirch$Illite[Achenkirch$Season=="May"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Season=="Aug"]))
qqline(log(Achenkirch$Illite[Achenkirch$Season=="Aug"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Season=="Oct"]))
qqline(log(Achenkirch$Illite[Achenkirch$Season=="Oct"]))   ### log

# lmer model with warming time also as a random effect

lmer_Illite <-  lmer(Illite ~ Warming*Depth*Season + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_Illite)
anova(lmer_Illite)
tukey_lmer_Illite <- glht(lmer_Illite, linfct = mcp(Season = "Tukey"))
summary(tukey_lmer_Illite)
cld(tukey_lmer_Illite)   ### original

lmer_Illite_sqrt <-  lmer(sqrt(Illite) ~ Warming*Depth*Season + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_Illite_sqrt)
anova(lmer_Illite_sqrt)
tukey_lmer_Illite_sqrt <- glht(lmer_Illite_sqrt, linfct = mcp(Season = "Tukey"))
summary(tukey_lmer_Illite_sqrt)
cld(tukey_lmer_Illite_sqrt)   ### sqrt

lmer_Illite_log <-  lmer(log(Illite) ~ Warming*Depth*Season + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_Illite_log)
anova(lmer_Illite_log)
tukey_lmer_Illite_log <- glht(lmer_Illite_log, linfct = mcp(Season = "Tukey"))
summary(tukey_lmer_Illite_log)
cld(tukey_lmer_Illite_log)   ### log


#################### mixed effects model for data from 2020 #########################
##### checking homeogenity
bartlett.test(Illite~Warming,Achenkirch)
bartlett.test(Illite~Depth,Achenkirch)  ### original

bartlett.test(sqrt(Achenkirch$Illite)~Achenkirch$Warming) 
bartlett.test(sqrt(Achenkirch$Illite)~Achenkirch$Depth)    ### sqrt

bartlett.test(log(Achenkirch$Illite)~Achenkirch$Warming)
bartlett.test(log(Achenkirch$Illite)~Achenkirch$Depth)    ### log

##### creating a window for figures, which check normal distribution
par(mfrow = c(3,4))
par("mar")
par(mar=c(1,1,1,1))
hist(Achenkirch$Illite[Achenkirch$Warming=="warmed"],breaks = 10,xlab="Illite", ylab="number", main = "warmed")
hist(Achenkirch$Illite[Achenkirch$Warming=="control"],breaks = 10,xlab="Illite", ylab="number", main = "control")
hist(Achenkirch$Illite[Achenkirch$Depth=="10"],breaks = 10,xlab="Illite", ylab="number", main = "10")
hist(Achenkirch$Illite[Achenkirch$Depth=="20"],breaks = 10,xlab="Illite", ylab="number", main = "20")
boxplot(Achenkirch$Illite[Achenkirch$Warming=="warmed"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Warming=="control"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Depth=="10"], horizontal = TRUE)
boxplot(Achenkirch$Illite[Achenkirch$Depth=="20"], horizontal = TRUE)
qqnorm(Achenkirch$Illite[Achenkirch$Warming=="warmed"])
qqline(Achenkirch$Illite[Achenkirch$Warming=="warmed"])
qqnorm(Achenkirch$Illite[Achenkirch$Warming=="control"])
qqline(Achenkirch$Illite[Achenkirch$Warming=="control"])
qqnorm(Achenkirch$Illite[Achenkirch$Depth=="10"])
qqline(Achenkirch$Illite[Achenkirch$Depth=="10"])
qqnorm(Achenkirch$Illite[Achenkirch$Depth=="20"])
qqline(Achenkirch$Illite[Achenkirch$Depth=="20"])  ### original

par(mfrow = c(3,4))
par("mar")
par(mar=c(1,1,1,1))
hist(sqrt(Achenkirch$Illite[Achenkirch$Warming=="warmed"]),breaks = 10,xlab="Illite", ylab="number", main = "warmed")
hist(sqrt(Achenkirch$Illite[Achenkirch$Warming=="control"]),breaks = 10,xlab="Illite", ylab="number", main = "control")
hist(sqrt(Achenkirch$Illite[Achenkirch$Depth=="10"]),breaks = 10,xlab="Illite", ylab="number", main = "10")
hist(sqrt(Achenkirch$Illite[Achenkirch$Depth=="20"]),breaks = 10,xlab="Illite", ylab="number", main = "20")
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Warming=="warmed"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Warming=="control"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Depth=="10"]), horizontal = TRUE)
boxplot(sqrt(Achenkirch$Illite[Achenkirch$Depth=="20"]), horizontal = TRUE)
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Warming=="warmed"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Warming=="warmed"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Warming=="control"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Warming=="control"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Depth=="10"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Depth=="10"]))
qqnorm(sqrt(Achenkirch$Illite[Achenkirch$Depth=="20"]))
qqline(sqrt(Achenkirch$Illite[Achenkirch$Depth=="20"])) ### sqrt

par(mfrow = c(3,4))
par("mar")
par(mar=c(1,1,1,1))
hist(log(Achenkirch$Illite[Achenkirch$Warming=="warmed"]),breaks = 10,xlab="Illite", ylab="number", main = "warmed")
hist(log(Achenkirch$Illite[Achenkirch$Warming=="control"]),breaks = 10,xlab="Illite", ylab="number", main = "control")
hist(log(Achenkirch$Illite[Achenkirch$Depth=="10"]),breaks = 10,xlab="Illite", ylab="number", main = "10")
hist(log(Achenkirch$Illite[Achenkirch$Depth=="20"]),breaks = 10,xlab="Illite", ylab="number", main = "20")
boxplot(log(Achenkirch$Illite[Achenkirch$Warming=="warmed"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Warming=="control"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Depth=="10"]), horizontal = TRUE)
boxplot(log(Achenkirch$Illite[Achenkirch$Depth=="20"]), horizontal = TRUE)
qqnorm(log(Achenkirch$Illite[Achenkirch$Warming=="warmed"]))
qqline(log(Achenkirch$Illite[Achenkirch$Warming=="warmed"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Warming=="control"]))
qqline(log(Achenkirch$Illite[Achenkirch$Warming=="control"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Depth=="10"]))
qqline(log(Achenkirch$Illite[Achenkirch$Depth=="10"]))
qqnorm(log(Achenkirch$Illite[Achenkirch$Depth=="20"]))
qqline(log(Achenkirch$Illite[Achenkirch$Depth=="20"]))  ### log

# lmer model with warming time also as a random effect

lmer_Illite <-  lmer(Illite ~ Warming*Depth + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_Illite)
anova(lmer_Illite)  ### original

lmer_Illite_sqrt <-  lmer(sqrt(Illite) ~ Warming*Depth + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_Illite_sqrt)
anova(lmer_Illite_sqrt)  ### sqrt

lmer_Illite_log <-  lmer(log(Illite) ~ Warming*Depth + (1|Block) + (1|Duration), data = Achenkirch)
summary(lmer_Illite_log)
anova(lmer_Illite_log)   ### log