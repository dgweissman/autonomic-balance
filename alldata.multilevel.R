require(nlme)
all<-read.csv("alldata.tsst.long.csv")
all<-all[-which(is.na(all$PEPc)),]
all<-all[-which(is.na(all$RSAc)),]
all$RSAbc<-all$RSAb-mean(all$RSAb)
all$PEPbc<-all$PEPb-mean(all$PEPb)

summary(lme(PEPc~PEPbc+RSAbc+RSAcc,random = ~1 | ID, data=all))
summary(lme(RSAc~PEPbc+RSAbc+PEPcc,random = ~1 | ID, data=all))
summary(lme(PEPc~0+PEPbc+task+task:RSAcc,random = ~1 | ID, data=all))
summary(lme(RSAc~0+PEPbc+task+task:PEPcc,random = ~1 | ID, data=all))

summary(lme(PEPc~PEPbc+RSAbc+RSAcc+study,random = ~1 | ID, data=all))
summary(lme(RSAc~PEPbc+RSAbc+PEPcc+study,random = ~1 | ID, data=all))

summary(lme(PEPc~PEPbc+RSAbc+RSAcc*feedback+study,random = ~1+RSAcc | ID, data=all))
summary(lme(RSAc~PEPbc+RSAbc+PEPcc*feedback+study,random = ~1+PEPcc | ID, data=all))

all.gender<-na.omit(all[,-c(6,7)])
summary(lme(PEPc~PEPbc+RSAbc+study+feedback+RSAcc*gender,random = ~1+RSAcc | ID, data=all.gender))
summary(lme(RSAc~PEPbc+RSAbc+study+feedback+PEPcc+gender,random = ~1+PEPcc | ID, data=all.gender))

all.tr<-na.omit(all)
summary(lme(RSAc~PEPbc+RSAbc+study+PEPcc*PreThreatRatio,random = ~1+PEPcc | ID, data=all.tr))
summary(lme(PEPc~PEPbc+RSAbc+study+RSAcc*PreThreatRatio,random = ~1+RSAcc | ID, data=all.tr))
summary(lme(RSAc~PEPbc+RSAbc+study+PEPcc*PostThreatRatio,random = ~1+PEPcc | ID, data=all.tr))
summary(lme(PEPc~PEPbc+RSAbc+study+RSAcc*PostThreatRatio,random = ~1+RSAcc | ID, data=all.tr))