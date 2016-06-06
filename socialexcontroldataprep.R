# remove all elements for a clean start
rm(list=ls(all=TRUE))
cat("\014")
library(dplyr)
library(Hmisc)


ds0 <- read.table("socialex2.csv", header=TRUE, 
                     sep=",", row.names="id")
# recode -999 to missing
ds0[ds0==-999] <- NA
#recode -9999 to missing 
ds0[ds0==-9999]<- NA

#Using multiple imputation to deal with missing data.
library(mice)
#md.pattern(ds0)

imputed_ds0 <- mice(ds0, m=5, maxit = 5, method = 'pmm')



#Create summary scores for VLS-AQ based on Jopp and Hertzog 2012 factor analysis
#total summary scores for each scale

attach(ds0)
# mean summary scores for each scale
s <- which(colnames(ds0)=="aq1")
f <- which(colnames(ds0)=="aq57")
ds0$acttot<-rowSums(ds0[s:f], na.rm=TRUE)
ds0$acttot[1:30]
ds1<-ds0[s:f]


#Gives the number of missing activity questions for each participant
ds0$act_missing <- rowSums(is.na(ds1))
ds0$act_missing

ds0$

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dat2 <- ddply(dat, ~ taxa, transform, length = impute.mean(length),
              width = impute.mean(width))

dat2[order(dat2$id), ]

ds0$craft <- aq1 +aq2 + aq3 + aq4
ds0$phys <- aq11 + aq12 + aq13 + aq14 + aq15 + aq16
ds0$game <- aq17 + aq18+aq19+aq20+aq21+aq22
ds0$tv <- aq27+aq28+aq29+aq30
ds0$socpriv <- aq38+aq39+aq40+aq41+aq42+aq43
ds0$socpub <- aq46+aq47+aq48+aq49+aq50
ds0$rel <- aq44+aq45
ds0$travel <- aq55+aq56+aq57
ds0$exp <- aq8+aq9+aq10+aq23+aq24+aq31+aq51
ds0$dev <- aq6+aq25+aq26+aq36+aq37+aq52+aq53+aq54
ds0$tech <- aq5+aq7+aq32+aq33+aq34+aq35
ds0$activtot<-ds0$craft+ds0$phys+ds0$game+ds0$tv+ds0$socpriv+ds0$socpub+ds0$rel+ds0$travel+ds0$exp+ds0$dev+ds0$tech
ds0$socacttot<-socpriv+socpub+rel



                           
ds0$craftm <- (aq1 + aq2 + aq3 + aq4)/4
ds0$physm <- (aq11 + aq12 + aq13 + aq14 + aq15 + aq16)/6
ds0$gamem <- (aq17 + aq18+aq19+aq20+aq21+aq22)/6
ds0$tvm <- (aq27+aq28+aq29+aq30)/4
ds0$socprivm <- (aq38+aq39+aq40+aq41+aq42+aq43)/6
ds0$socpubm <- (aq46+aq47+aq48+aq49+aq50)/5
ds0$relm <- (aq44+aq45)/2
ds0$travelm <- (aq55+aq56+aq57)/3
ds0$expm <- (aq8+aq9+aq10+aq23+aq24+aq31+aq51)/7
ds0$devm <- (aq6+aq25+aq26+aq36+aq37+aq52+aq53+aq54)/8
ds0$techm <- (aq5+aq7+aq32+aq33+aq34+aq35)/6

ds0[1:10,]

#Create summary scores for loneliness scale
#Count the missing values (i.e., no answer) on items 2, 3, 5, 6, 9, 10. This is the missing emotional loneliness score. 
#Count the neutral and negative ("no!", "no", or "more or less") answers on items 1, 4, 7, 8, 11. This is the social loneliness score. 
#Count the missing values (i.e., no answer) on items 1, 4, 7, 8, 11. This is the missing social loneliness score. 
#Compute the total loneliness score by taking the sum of the emotional loneliness score and the social loneliness score. 
#The emotional loneliness score is valid only if the missing emotional loneliness score equals 0. 
#The social loneliness score is valid only if the missing social loneliness score equals 0. 
#The total loneliness score is valid only if the sum of the missing emotional loneliness score and the missing social loneliness score equals 0 or 1. 
#The total loneliness score can be categorized into four levels: not lonely (score 0, 1 or 2), moderate lonely (score 3 through 8), severe lonely (score 9 or 10), and very severe lonely (score 11).
#Count the neutral and positive answers ("more or less", "yes", or "yes!") on items 2, 3, 5, 6, 9, 10. This is the emotional loneliness score. 
#recoding items

# create variables to identify start and end of loneliness scale 
s <- which(colnames(ds0)=="lone1")
f <- which(colnames(ds0)=="lone11")
ds1<-ds0[s:f]

#Gives the number of missing loneliness questions for each participant
ds0$loneliness_missing <- rowSums(is.na(ds1))
ds0$loneliness_missing

ds0$elone2[lone2>2]<-1
ds0$elone2[lone2<3]<-0
ds0$elone3[lone3>2]<-1
ds0$elone3[lone3<3]<-0
ds0$elone5[lone5>2]<-1
ds0$elone5[lone5<3]<-0
ds0$elone6[lone6>2]<-1
ds0$elone6[lone6<3]<-0
ds0$elone9[lone9>2]<-1
ds0$elone9[lone9<3]<-0
ds0$elone10[lone10>2]<-1
ds0$elone10[lone10<3]<-0

#Count the neutral and negative ("no!", "no", or "more or less") answers on items 1, 4, 7, 8, 11. This is the social loneliness score. 
ds0$slone1[lone1<4]<-1
ds0$slone1[lone1>3]<-0
ds0$slone4[lone4<4]<-1
ds0$slone4[lone4>3]<-0
ds0$slone7[lone7<4]<-1
ds0$slone7[lone7>3]<-0
ds0$slone8[lone8<4]<-1
ds0$slone8[lone8>3]<-0
ds0$slone11[lone11<4]<-1
ds0$slone11[lone11>3]<-0

attach(ds0)
ds0$emlonetot<- elone2+elone3+elone5+elone6+elone9+elone10
ds0$solonetot<- slone1+slone4+slone7+slone8+slone11
ds0$lonetot<- emlonetot + solonetot

ds0$loneliness[ds0$lonetot<3]<-'not lonely'
ds0$loneliness[ds0$lonetot>2]<- 'moderate lonely'
ds0$loneliness[ds0$lonetot==9||ds0$lonetot==10]<-'severe lonely'
ds0$loneliness[ds0$lonetot==11]<-'very severe lonely'

ds0$lonetot
ds0$loneliness
ds0[1:10,]

# create variables to identify start and end of gds scale 
s <- which(colnames(ds0)=="gds1")
f <- which(colnames(ds0)=="gds15")
ds1<-ds0[s:f]

#Gives the number of missing loneliness questions for each participant
ds0$gds_missing <- rowSums(is.na(ds1))
ds0$gds_missing

#Recoding and summing the GDS
#Items 1, 5, 7, 11, 13 "no" entered as 0 is the depressive answer these need to be recoded
ds0$gds1[gds1==1]<-0
ds0$gds1[gds1==0]<-1
ds0$gds5[gds5==1]<-0
ds0$gds5[gds5==0]<-1
ds0$gds7[gds7==1]<-0
ds0$gds7[gds7==0]<-1
ds0$gds11[gds11==1]<-0
ds0$gds11[gds11==0]<-1
ds0$gds13[gds13==1]<-0
ds0$gds13[gds13==0]<-1  

ds0$gdstot<-gds1+gds2+gds3+gds4+gds5+gds6+gds7+gds8+gds9+gds10+gds11+gds12+gds13+gds14+gds15
ds0$gdstot

# create variables to identify start and end of sad scale 
s <- which(colnames(ds0)=="sad1")
f <- which(colnames(ds0)=="sad28")
ds1<-ds0[s:f]

#Gives the number of missing sad questions for each participant
ds0$sad_missing <- rowSums(is.na(ds1))
ds0$sad_missing
ds0$sadtot <-rowSums(ds1, na.rm=TRUE)
ds0$sadtot
#Recoding the SAD scale so that higher scores indicate greater social anxiety (entry key 1= True 2=False)
# Items 1, 3, 4, 6, 7, 9, 12, 15, 17, 22, 25, 27, 28, false = 1.
#Items 2, 5, 8, 10, 11, 13, 14, 16, 18, 19, 20, 21, 23, 24, 26, True = 1. 
ds0$sad1[sad1==2]<-1
ds0$sad1[sad1==1]<-0
ds0$sad3[sad3==2]<-1
ds0$sad3[sad3==1]<-0
ds0$sad4[sad4==2]<-1
ds0$sad4[sad4==1]<-0
ds0$sad6[sad6==2]<-1
ds0$sad6[sad6==1]<-0
ds0$sad7[sad7==2]<-1
ds0$sad7[sad7==1]<-0
ds0$sad9[sad9==2]<-1
ds0$sad9[sad9==1]<-0
ds0$sad12[sad12==2]<-1
ds0$sad12[sad12==1]<-0
ds0$sad15[sad15==2]<-1
ds0$sad15[sad15==1]<-0
ds0$sad17[sad17==2]<-1
ds0$sad17[sad17==1]<-0
ds0$sad22[sad22==2]<-1
ds0$sad22[sad22==1]<-0
ds0$sad25[sad25==2]<-1
ds0$sad25[sad25==1]<-0
ds0$sad27[sad27==2]<-1
ds0$sad27[sad27==1]<-0
ds0$sad28[sad28==2]<-1
ds0$sad28[sad28==1]<-0

ds0$sad2[sad2==2]<-0
ds0$sad5[sad5==2]<-0
ds0$sad8[sad8==2]<-0
ds0$sad10[sad10==2]<-0
ds0$sad11[sad11==2]<-0
ds0$sad13[sad13==2]<-0
ds0$sad14[sad14==2]<-0
ds0$sad16[sad16==2]<-0
ds0$sad18[sad18==2]<-0
ds0$sad19[sad19==2]<-0
ds0$sad20[sad20==2]<-0
ds0$sad21[sad21==2]<-0
ds0$sad23[sad23==2]<-0
ds0$sad24[sad24==2]<-0
ds0$sad26[sad26==2]<-0

ds0$sadtot<-sad1+sad2+sad3+sad4+sad5+sad6+sad7+sad8+sad9+sad10+sad11+sad12+
  sad13+sad14+sad15+sad16+sad17+sad18+sad19+sad20+sad21+sad22+sad23+sad24+sad25+sad26
ds0$sadtot

# create variables to identify start and end of fne scale 
s <- which(colnames(ds0)=="fne1")
f <- which(colnames(ds0)=="fne30")
ds1<-ds0[s:f]

#Gives the number of missing sad questions for each participant
ds0$fne_missing <- rowSums(is.na(ds1))
ds0$fne_missing
ds0$fnetot <-rowSums(ds1, na.rm=TRUE)
ds0$fnetot
#Items 1, 4, 6, 8, 10, 12, 15, 16, 18, 21, 23, 26, 27, False = 1 
#Items 2, 3, 5, 7, 9, 11, 13, 14, 17, 19, 20, 22, 24, 25, 28, 29, 30, True = 1
ds0$fne1[fne1==2]<-1
ds0$fne1[fne1==1]<-0
ds0$fne4[fne4==2]<-1
ds0$fne4[fne4==1]<-0
ds0$fne6[fne6==2]<-1
ds0$fne6[fne6==1]<-0
ds0$fne8[fne8==2]<-1
ds0$fne8[fne8==1]<-0
ds0$fne10[fne10==2]<-1
ds0$fne10[fne10==1]<-0
ds0$fne12[fne12==2]<-1
ds0$fne12[fne12==1]<-0
ds0$fne15[fne15==2]<-1
ds0$fne15[fne15==1]<-0
ds0$fne16[fne16==2]<-1
ds0$fne16[fne16==1]<-0
ds0$fne18[fne18==2]<-1
ds0$fne18[fne18==1]<-0
ds0$fne21[fne21==2]<-1
ds0$fne21[fne21==1]<-0
ds0$fne23[fne23==2]<-1
ds0$fne23[fne23==1]<-0
ds0$fne26[fne26==2]<-1
ds0$fne26[fne26==1]<-0
ds0$fne27[fne27==2]<-1
ds0$fne27[fne27==1]<-0

ds0$fne2[fne2==2]<-0
ds0$fne3[fne3==2]<-0
ds0$fne5[fne5==2]<-0
ds0$fne7[fne7==2]<-0
ds0$fne9[fne6==2]<-0
ds0$fne11[fne11==2]<-0
ds0$fne13[fne13==2]<-0
ds0$fne14[fne14==2]<-0
ds0$fne17[fne17==2]<-0
ds0$fne19[fne19==2]<-0
ds0$fne20[fne20==2]<-0
ds0$fne22[fne22==2]<-0
ds0$fne24[fne24==2]<-0
ds0$fne25[fne25==2]<-0
ds0$fne28[fne28==2]<-0
ds0$fne29[fne29==2]<-0
ds0$fne30[fne30==2]<-0
ds0$fnetot<-fne1+fne2+fne3+fne4+fne5+fne6+fne7+fne8+fne9+fne10+fne11+fne12+fne13+fne14+fne15+fne16+fne17+fne18+fne19+fne20+fne21+fne22+fne23+fne24+fne25+fne26+fne27+fne28+fne29+fne30

#rejection sensitivity scale calculation
#How to Score:
#Calculate a score of rejection sensitivity for each situation by multiplying the level of rejection concern (the response to question a) by the reverse of the level of acceptance expectancy (the response to question b).
#The formula is: rejection sensitivity = (rejection) * (7-acceptance expectancy)
#Take the mean of the resulting 9 scores to obtain the overall rejection sensitivity score.
ds0$reject1<-rejecta1*(7-rejectb1)
ds0$reject2<-rejecta2*(7-rejectb2)
ds0$reject3<-rejecta3*(7-rejectb3)
ds0$reject4<-rejecta4*(7-rejectb4)
ds0$reject5<-rejecta5*(7-rejectb5)
ds0$reject6<-rejecta6*(7-rejectb6)
ds0$reject7<-rejecta7*(7-rejectb7)
#attach(ds0)
ds0$rejecttot<-(reject1+reject2+reject3+reject4+reject5+reject6+reject7)/7
ds0$rejecttot

# create variables to identify start and end of recoded rejection sensitivity q's
s <- which(colnames(ds0)=="reject1")
f <- which(colnames(ds0)=="reject7")
ds1<-ds0[s:f]

#Gives the number of missing sad questions for each participant
ds0$reject_missing <- rowSums(is.na(ds1))
ds0$reject_missing

#social support measure scale totals
#informational support
# create variables to identify start and end of informational support scale 
s <- which(colnames(ds0)=="ssminfo1")
f <- which(colnames(ds0)=="ssminfo7")
ds1<-ds0[s:f]

#Gives the number of missing informational support questions for each participant
ds0$ssminfo_missing <- rowSums(is.na(ds1))
ds0$ssminfo_missing
ds0$ssinfotot <-rowSums(ds1, na.rm=TRUE)
ds0$ssinfotot
ds0$ssinfotot2
ds0$ssinfotot2<- ssminfo1+ssminfo2+ssminfo3+ssminfo4+ssminfo5+ssminfo6+ssminfo7

s <- which(colnames(ds0)=="ssmpract1")
f <- which(colnames(ds0)=="ssmpract9")
ds1<-ds0[s:f]

#Gives the number of missing informational support questions for each participant
ds0$ssmpract_missing <- rowSums(is.na(ds1))
ds0$ssmpract_missing
ds0$sspracttot <-rowSums(ds1, na.rm=TRUE)

s <- which(colnames(ds0)=="ssmemot1")
f <- which(colnames(ds0)=="ssemot11")
ds1<-ds0[s:f]

#Gives the number of missing informational support questions for each participant
ds0$ssmemo_missing <- rowSums(is.na(ds1))
ds0$ssmemo_missing
ds0$ssemottot <-rowSums(ds1, na.rm=TRUE)
ds0$ssmemottot<-ssmemot1+ssmemot2+ssemot3+ssemot4+ssemot5+ssemot6+ssemot7+ssemot8+ssemot9+ssemot10+ssemot11

s <- which(colnames(ds0)=="ssmprovid1")
f <- which(colnames(ds0)=="ssmprovid13")
ds1<-ds0[s:f]

#Gives the number of missing informational support questions for each participant
ds0$ssmprovid13_missing <- rowSums(is.na(ds1))
ds0$ssmprovid13_missing
ds0$ssprovtot <-rowSums(ds1, na.rm=TRUE)
ds0$ssprovtot<-ssmprovid1+ssmprovid2+ssmprovid3+ssmprovid4+ssmprovid5+ssmprovid6+ssmprovid7+ssmprovid8+ssmprovid9+ssmprovid10+ssmprovid11+ssmprovid12+ssmprovid13

ds0$socsuptot <- ds0$ssinfotot+ ds0$socsuptot+ds0$sspracttot+ds0$ssmemottot+ds0$ssprovtot

ds0$satisfaction_ss<-ds0$satisinfo+ds0$satisemot+ds0$satispract+ds0$satisprovid

hist(ds0$socpriv)
hist(ds0$socprivm)
hist(ds0$socpubm)
hist(ds0$relm)
hist(ds0$lonetot)
hist(ds0$activtot)
hist(ds0$gdstot)

ds1<-select(ds0,activtot,socacttot,gdstot,lonetot,sadtot,
            fnetot,rejecttot,socsuptot,ssinfotot,sspracttot,ssmemottot,ssprovtot)
ds2<-select(ds0,activtot,socacttot,gdstot,lonetot,sadtot, fnetot,rejecttot,socsuptot)

library(psych)
describe(ds1)
Hmisc::rcorr(as.matrix(ds1), type="pearson")

lonetable <- with(ds0, table(loneliness))
lonetable

t.test(fnetot~loneliness, ds0)
t.test(ds0$sadtot,ds0$lonetot)
corr.test(ds2, use="pairwise")
