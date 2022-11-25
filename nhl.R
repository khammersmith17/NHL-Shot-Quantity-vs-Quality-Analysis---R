library(tidyverse)

skaters <- read_csv("skaters.csv")

#going to significantly shave down data set in terms of variables
#removing all data that references team success while player is on the ice
#this data is not relevant because I am looking at individual success based on individual stats
#also removing data that references game occurrences shortly after player gets off the ice
skaters <- skaters %>% filter(situation == "5on5")
skaters <- skaters[,-c(148:154)]
skaters <- skaters[,-c(85:147)]

glimpse(skaters)
#eliminating factors that are not relevant
#removing the data that refers to any column that does not have to do with goal scoring explicitly
#data such as penalties
skaters <- skaters %>% select(,-c(penalties,penaltiesDrawn,penalityMinutes,penalityMinutesDrawn))
skaters <- skaters %>% select(,-c(I_F_xPlayStopped,I_F_xPlayContinuedInZone,I_F_xPlayContinuedOutsideZone))
skaters <- skaters %>% select(,-c(I_F_penalityMinutes,I_F_faceOffsWon))
skaters <- skaters %>% select(,-c(timeOnBench,faceoffsLost,faceoffsWon))

summary(skaters)                           
is.na(skaters)
#no missing data 

skaters <- skaters %>% select(,-c(offIce_xGoalsPercentage,offIce_corsiPercentage,offIce_fenwickPercentage))
skaters <- skaters %>% selct(,-c(gameScore,games_played))

skaters <- skaters %>% mutate(LowQPerShift = I_F_lowDangerShots/shifts)
skaters <- skaters %>% mutate(HighQPerShift = I_F_highDangerShots/shifts)
skaters <- skaters %>% mutate(MedQPerShift = I_F_mediumDangerShots/shifts)
skaters <- skaters %>% mutate(TShotsPerShift = I_F_shotsOnGoal/shifts)

hist(skaters$LowQPerShift)
#interestingly, low quality shots is relatively normally distributed 
hist(skaters$HighQPerShift)
hist(skaters$MedQPerShift)
hist(skaters$TShotsPerShift)
#most players have about the same value of shots per shift

skaters <- skaters %>% mutate(blockPerc = I_F_blockedShotAttempts / I_F_shotAttempts)
hist(skaters$blockPerc)

skaters %>% ggplot(aes(x=TShotsPerShift,y=HighQPerShift)) + geom_point()

skaters <- skaters %>% mutate(XGoalsPerShift = I_F_xGoals/shifts)

skaters %>% ggplot(aes(x=HighQPerShift,y=XGoalsPerShift)) + geom_point() +
  labs(y="xGoals Per Shift", x="High Quality shots per shift")
#as expected there is definitely a linear relationship
cor(skaters$HighQPerShift,skaters$XGoalsPerShift)

skaters %>% ggplot(aes(x=TShotsPerShift,y=XGoalsPerShift)) + geom_point()
#slight positive linear slope. but not significant, more shifts does seem to have large increase in goals
cor(skaters$TShotsPerShift,skaters$XGoalsPerShift)

skaters %>% ggplot(aes(x=onIce_corsiPercentage, y=XGoalsPerShift)) + geom_point()
#points largely condensed in one blob
cor(skaters$onIce_corsiPercentage,skaters$XGoalsPerShift)
# very low correlation

skaters %>% ggplot(aes(x=onIce_fenwickPercentage, y=XGoalsPerShift)) + geom_point()
#no real relationship 

skaters %>% ggplot(aes(x=MedQPerShift,y=XGoalsPerShift)) + geom_point() +
  labs(y="xGoals Per Shift", x="Medium Quality shots per shift")
#certainly a linear relationship 
cor(skaters$MedQPerShift,skaters$XGoalsPerShift)
#higher correlation than high quality shots

skaters %>% ggplot(aes(x=LowQPerShift,y=XGoalsPerShift)) + geom_point() + 
  labs(y="xGoals Per Shift", x="Low Quality shots per shift")
# no real relationship
cor(skaters$LowQPerShift,skaters$XGoalsPerShift)
skaters %>% ggplot(aes(x=LowQPerShift,y=GoalsPerShift)) + geom_point() +
  labs(y="Goals Per Shift", x="Low Quality shots per shift")
cor(skaters$LowQPerShift,skaters$GoalsPerShift)

skaters <- skaters %>% mutate(HighMed = HighQPerShift + MedQPerShift)

skaters %>% ggplot(aes(x=HighMed, y=XGoalsPerShift)) +geom_point() +
  labs(y="xGoals Per Shift", x="High + Medium Quality shots per shift")
hist(skaters$HighMed)
#strong linear relationship
cor(skaters$HighMed,skaters$XGoalsPerShift)

skaters %>% ggplot(aes(x=blockPerc, y=XGoalsPerShift)) + geom_point()
# no real relationship

skaters <- skaters %>% mutate(NotBlockedPerc = I_F_unblockedShotAttempts / I_F_shotAttempts)

skaters %>% ggplot(aes(x=NotBlockedPerc,y=XGoalsPerShift)) + geom_point()
# slight linear relationship

skaters <- skaters %>% mutate(HMNB = HighMed + NotBlockedPerc)
skaters %>% ggplot(aes(x=HMNB,y=XGoalsPerShift)) + geom_point()
# definitely some linearity 

skaters %>% ggplot(aes(x=I_F_reboundxGoals,y=XGoalsPerShift)) + geom_point()
skaters <- skaters %>% mutate(XrebGPerShift = I_F_reboundxGoals/shifts)
skaters %>% ggplot(aes(x=XrebGPerShift,y=XGoalsPerShift)) + geom_point()

skaters <- skaters %>% mutate(GoalsPerShift = I_F_goals/shifts)
skaters %>% ggplot(aes(x=HighQPerShift,y=GoalsPerShift)) + geom_point()
skaters %>% ggplot(aes(x=MedQPerShift,y=GoalsPerShift)) + geom_point()
skaters %>% ggplot(aes(x=LowQPerShift,y=GoalsPerShift)) + geom_point()
skaters %>% ggplot(aes(x=TShotsPerShift,y=GoalsPerShift)) + geom_point()
# actual results do not appear to be as linear as expected
# this is to be expected given different finishing abilities among players

skaters <- skaters %>% mutate(LowMed = LowQPerShift + MedQPerShift)
skaters %>% ggplot(aes(x=LowMed, y=XGoalsPerShift)) +geom_point() +
  labs(y="xGoals Per Shift", x="Low + Medium Quality shots per shift")

skaters <- skaters %>% mutate(HighMedNblocked = HighMed + NotBlockedPerc)
skaters %>% ggplot(aes(x=HighMedNblocked, y=XGoalsPerShift)) +geom_point() +
  labs(y="xGoals Per Shift", x="High + Medium Quality Shots Per Shift + Unblocked Shot Percentage")

skaters <- skaters %>% mutate(MissedNetPerc = I_F_missedShots/I_F_shotAttempts)
skaters <- skaters %>% mutate(ShotwMssedNetBlockedPerc = MissedNetPerc + TShotsPerShift + blockPerc)
skaters %>% ggplot(aes(x=ShotwMssedNetBlockedPerc, y=XGoalsPerShift)) +geom_point() +
  labs(y="xGoals Per Shift", x="Total Shots + Blocked Shot Percentage + Missed Net Percentage")


skaters <- skaters %>% mutate(ShotOnGoalPerc = I_F_shotsOnGoal / I_F_shotAttempts)
skaters %>% ggplot(aes(x=ShotOnGoalPerc, y=XGoalsPerShift)) + geom_point()

write.csv(skaters,"UpdatedSkaters.csv", row.names = FALSE)

skaters <- skaters %>% mutate(BlockedPercDiff = NotBlockedPerc - blockPerc)
skaters %>% ggplot(aes(x=BlockedPercDiff, y=XGoalsPerShift)) + geom_point()

skaters <- skaters %>% mutate(HighQPerc = HighQPerShift/TShotsPerShift)
skaters %>% ggplot(aes(x=HighQPerc,y=XGoalsPerShift)) + geom_point()


skaters <- skaters %>% mutate(MedQPerc = MedQPerShift/TShotsPerShift)
skaters %>% ggplot(aes(x=MedQPerc,y=XGoalsPerShift)) + geom_point()
#################################################################
#Model creation

#initial model
model1 <- lm(XGoalsPerShift ~ HighQPerShift + MedQPerShift + NotBlockedPerc + ShotOnGoalPerc
             + LowQPerShift + TShotsPerShift + onIce_corsiPercentage + onIce_fenwickPercentage, data = skaters)
summary(model1)
#R^2 of .9667, adj R^2 of .9664

#remove fenwick
model1 <- lm(XGoalsPerShift ~ HighQPerShift + MedQPerShift + NotBlockedPerc
             + LowQPerShift + TShotsPerShift + onIce_corsiPercentage, data = skaters)
summary(model1)
#R^2 .9665 adj R^2 .9663, relatively unchanged

#remove corsi
model1 <- lm(XGoalsPerShift ~ HighQPerShift + MedQPerShift + NotBlockedPerc
             + LowQPerShift + TShotsPerShift + onIce_fenwickPercentage, data = skaters)
summary(model1)
#same results as fenwick

#remove Tshots
model1 <- lm(XGoalsPerShift ~ HighQPerShift + MedQPerShift + NotBlockedPerc
             + LowQPerShift + onIce_corsiPercentage + onIce_fenwickPercentage, data = skaters)
summary(model1)
#r^2 .9663 adj r^2 .9661

#remove not blocked perc
model1 <- lm(XGoalsPerShift ~ HighQPerShift + MedQPerShift 
             + LowQPerShift + TShotsPerShift + onIce_corsiPercentage + onIce_fenwickPercentage, data = skaters)
summary(model1)
#r^2 .9668 adj r^2 .9666

# remove not blocker perc, corsi, fenwick
modelfinal <- lm(XGoalsPerShift ~ HighQPerShift + MedQPerShift 
             + LowQPerShift + TShotsPerShift, data = skaters)
summary(modelfinal)
#r^2 .9668, adj r^2 .9667 *******************************

#remove Tshotsper
model1 <- lm(XGoalsPerShift ~ HighQPerShift + MedQPerShift 
             + LowQPerShift + TShotsPerShift, data = skaters)
summary(model1)
# r^2 .9664 adj r^2 .9663, both go down 

###########################################################################
#model testing 


plot(modelfinal)
hist(modelfinal$residuals)

set.seed(1234)
rs <- sample(1:dim(skaters)[1],size=round(dim(skaters)[1]/5),replace=F)
trainingSet <- skaters[-rs,]
testSet <- skaters[rs,]

modeltest <- lm(XGoalsPerShift ~ HighQPerShift + MedQPerShift 
                 + LowQPerShift + TShotsPerShift, data = trainingSet)

Pmodel <- predict(modeltest,newdata = testSet)
mean(abs(Pmodel-testSet$XGoalsPerShift),na.rm=T)
cor(Pmodel,testSet$XGoalsPerShift,use="pairwise.complete.obs")
# very strong correlation, .9855









