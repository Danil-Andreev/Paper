# Libraries and data import----

library(openxlsx)
library(dplyr)
library(stringr)
library(EviewsR)
library(lmtest)
library(forecast) #tsdisplay
library(zoo)
library(readxl)
setwd("C:/R/Data/PaperGit/Paper")


# Russia----

dataRF <- read_excel("dataRF.xlsx", sheet = "dataQ")
dataRF$COFS <- dataRF$COFS/1000


## Interpolation----
dataRF$DCPS <- c(rep(NA, 7), coredata(na.approx(zoo(dataRF$DCPS[5:84]))), rep(NA, 3))
dataRF$AGRO <- c(rep(NA, 23), coredata(na.approx(zoo(dataRF$AGRO[24:84]))), rep(NA, 3))
dataRF$GREV <- c(rep(NA, 3), coredata(na.approx(zoo(dataRF$GREV[4:84]))), rep(NA, 3))
dataRF$GEXP <- c(rep(NA, 3), coredata(na.approx(zoo(dataRF$GEXP[4:84]))), rep(NA, 3))
dataRF$energyTotal <- c(rep(NA, 23), coredata(na.approx(zoo(dataRF$energyTotal[24:84]))), 
                        rep(NA, 3))
dataRF$energyPopul <- c(rep(NA, 23), coredata(na.approx(zoo(dataRF$energyPopul[24:84]))), 
                        rep(NA, 3))
dataRF$energyPrice <- c(rep(NA, 3), coredata(na.approx(zoo(dataRF$energyPrice[4:84]))), 
                        rep(NA, 3))
dataRF$PRR <- c(rep(NA, 27), coredata(na.approx(zoo(dataRF$PRR[28:80]))), rep(NA, 7))
dataRF$CLR <- c(rep(NA, 27), coredata(na.approx(zoo(dataRF$CLR[28:80]))), rep(NA, 7))
dataRF$GI <- c(rep(NA, 23), coredata(na.approx(zoo(dataRF$GI[24:80]))), rep(NA, 7))
for (ie in colnames(dataRF)[6:11]){
  BOB <- zoo(dataRF[[ie]])
  BOB <- na.approx(BOB)
  dataRF[[ie]] = c(rep(NA, 3), coredata(BOB), rep(NA, 3))
}
rm(BOB, ie)


## Generating PCs----

# indexInstitute
INST_PCA = prcomp(dataRF[4:84, 6:11], scale = F, center = F)
dataRF$indexInstitute = c(rep(NA, 3), INST_PCA$x[,1], rep(NA, 3))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"),
       main="indexInstitute (PC1) RF")

# indexBureau
INST_PCA = prcomp(dataRF[4:84, 6:11], scale = F, center = T)
dataRF$indexBureau = c(rep(NA, 3), INST_PCA$x[,2], rep(NA, 3))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"),
       main="indexBureau (PC2) RF")

# indexPayment
payData <- data.frame(dataRF$ALLCARDS[33:85],
                     dataRF$TERMIN1[33:85], dataRF$ATM[33:85])
colnames(payData) <- c("AllCards", "Terminals", "ATMs")
PAY_PCA = prcomp(payData, scale = T, center = T)
dataRF$indexPayment = c(rep(NA, 32), PAY_PCA$x[,1], rep(NA, 2))
biplot(PAY_PCA, xlabs = c(rep("*", nrow(PAY_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexPayment (PC1) RF")

# indexDemocracy
democData <- data.frame(dataRF$GE[28:84], dataRF$PV[28:84], dataRF$RL[28:84],
                        dataRF$VA[28:84])
colnames(democData) <- c("GE", "PV" , "RL", "VA")
INST_PCA = prcomp(democData,center = T, scale = T)
dataRF$indexDemocracy = c(rep(NA, 27), INST_PCA$x[,1], rep(NA, 3))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexDemocracy (PC1) RF")
rm(INST_PCA, PAY_PCA)


## Monthly----

dfM <- read.xlsx("dataRF.xlsx", sheet="dataM", detectDates = T)
dfQ <- dataRF
dfQ <- dfQ[1:86, ]
rm(dataRF)

# choose columns from quarterly data
names = c("Date", "NGDP", "WSGDP", "COFS", "Tax", "DR", "DCPS", "AGRO", "GREV",
          "GEXP", "taxProfit", "energyTotal", "energyPopul", "energyPrice",
          "indexInstitute", "indexBureau", "indexPayment", "indexDemocracy", 
          "UNEMPL", "INFL", "RealInc")
dfAll <- dfQ[, names]
rm(names)

# determ. seasonal (Q) components
dfAll$bvQ1 <- 0
dfAll$bvQ2 <- 0
dfAll$bvQ3 <- 0

for(i in 1:nrow(dfAll)){
  lstch <- str_sub(dfAll$Date[i], start = -1)
  if(lstch=="1"){dfAll$bvQ1[i] <- 1}
  if(lstch=="2"){dfAll$bvQ2[i] <- 1}
  if(lstch=="3"){dfAll$bvQ3[i] <- 1}

} # end for i


## BV for 2014-2015 CRISIS----

dfAll$bvCrisis <- 0
dfAll$bvCrisis[59:64] <- 1 # 2014Q3-2015Q4

# Row of date as year.qtr format
dfAll$Date <- as.yearqtr(dfAll$Date, format = "%Y-%q")


## Monthly to Quarterly----

# Задаём расчет на кварталы в месячных данных
idxQe <- seq(from=4, to=259, by=3)

# M1
dfAll$M1 <- dfM$M1[idxQe]

# M2
dfAll$M2 <- dfM$M2[idxQe]

# M0
dfAll$M0 <- dfM$M0[idxQe]

# CH
dfAll$CH <- dfM$CH[idxQe]

# NPLA
dfAll$NPLa <- dfM$NPLA[idxQe]

# NPLP
dfAll$NPLp <- dfM$NPLP[idxQe]

# SID
dfAll$SID <- dfM$SID[idxQe]


## Feature generation----

# CM
dfAll$CM <- dfQ$COFS*100/dfM$M2[idxQe]

# volume of taxes
dfAll$vTax <- dfQ$Tax*dfQ$NGDP/100

dfWrk <- na.omit(dfAll)
write.csv(dfWrk, file="dataRF.csv", row.names = F, quote = F)


# Kazakhstan----

dataKZ <- read_excel("dataKZ.xlsx", sheet = "dataQ")


## Interpolation----

dataKZ$CH <- c(rep(NA, 27), coredata(na.approx(zoo(dataKZ$CH[28:60]))), dataKZ$CH[61:85], rep(NA, 2))

for (ie in colnames(dataKZ)[9:14]){
  BOB <- zoo(dataKZ[[ie]])
  BOB <- na.approx(BOB)
  dataKZ[[ie]] = c(rep(NA, 3), coredata(BOB), rep(NA, 3))
}

dataKZ$GREV <- c(rep(NA, 3), coredata(na.approx(zoo(dataKZ$GREV[4:84])))/4, rep(NA, 3))
dataKZ$GEXP <- c(rep(NA, 3), coredata(na.approx(zoo(dataKZ$GEXP[4:84])))/4, rep(NA, 3))

for (ie in colnames(dataKZ)[30:31]){
  BOB <- zoo(dataKZ[[ie]])
  BOB <- na.approx(BOB)
  dataKZ[[ie]] = c(rep(NA, 27), coredata(BOB))
}

dataKZ$GI <- c(rep(NA, 3), coredata(na.approx(zoo(dataKZ$GI[4:87]))))


## Generating PCs----

# indexInstitute
INST_PCA = prcomp(dataKZ[4:84, 9:14], scale = F, center = F)
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexInstitute (PC1) KZ")
dataKZ$indexInstitute = c(rep(NA, 3), INST_PCA$x[,1], rep(NA, 3))

# indexBureau
INST_PCA = prcomp(dataKZ[4:84, 9:14], scale = F, center = T)
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), main="indexBureau (PC2) KZ")
dataKZ$indexBureau = c(rep(NA, 3), INST_PCA$x[,2], rep(NA, 3))

# indexPayment
payData <- data.frame(dataKZ$ALLCARDS[20:86],dataKZ$TERMIN1[20:86], dataKZ$ATM[20:86])
colnames(payData) <- c("AllCards", "Terminals", "ATMs")
PAY_PCA = prcomp(payData, scale = T, center = T)
biplot(PAY_PCA, xlabs = c(rep("*", nrow(PAY_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexPayment (PC1) KZ")
dataKZ$indexPayment = c(rep(NA, 19), PAY_PCA$x[,1], rep(NA, 1))
rm(payData)

# indexDemocracy
democData <- data.frame(dataKZ$GE[28:84], dataKZ$PV[28:84], dataKZ$RL[28:84],
                        dataKZ$VA[28:84])
colnames(democData) <- c("GE", "PV" , "RL", "VA")
INST_PCA = prcomp(democData, center = T, scale = T)
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexDemocracy (PC1) KZ")
dataKZ$indexDemocracy = c(rep(NA, 27), INST_PCA$x[,1], rep(NA, 3))
rm(democData)

## Monthly data----

dfM <- read.xlsx("dataKZ.xlsx", sheet="dataM", detectDates = T)
dfQ <- dataKZ
dfQ <- dfQ[1:87, ]
rm(dataKZ)

# choose columns from quarterly data
names = c("Date", "NGDP", "WSGDP", "COFS", "Slr", "AGRO", "GREV",
          "GEXP", "taxProfit", "CH", "SID", "SED", "NPLa", "NPLp",
          "indexInstitute", "indexBureau",
           "indexPayment", "indexDemocracy", "Unempl", "INFL", "RealInc")
dfAll <- dfQ[, names]
rm(names)

# determ. seasonal (Q) components
dfAll$bvQ1 <- 0
dfAll$bvQ2 <- 0
dfAll$bvQ3 <- 0

for(i in 1:nrow(dfAll)){
  lstch <- str_sub(dfAll$Date[i], start = -1)
  if(lstch=="1"){dfAll$bvQ1[i] <- 1}
  if(lstch=="2"){dfAll$bvQ2[i] <- 1}
  if(lstch=="3"){dfAll$bvQ3[i] <- 1}
  
} # end for i


## Monthly to Quarterly----

idxQe <- seq(from=4, to=259, by=3)

# M2
dfAll$M2 <- c(dfM$M2[idxQe], NA)

# M0
dfAll$M0 <- c(dfM$M0[idxQe], NA)

# DR
dfAll$DR <- c(dfM$DR[idxQe], NA)

# CM
dfAll$CM <- dfQ$COFS*100/dfM$M2[idxQe]

# Tax
dfAll$Tax <- dfAll$taxProfit*100/dfAll$NGDP

dfWrk <- na.omit(dfAll)
write.csv(dfWrk, file="dataKZ.csv", row.names = F, quote = F)


# Belarus----

dataRB <- read_excel("dataRB.xlsx", sheet = "dataQ")
dataRB$COFS <- dataRB$COFS


## Interpolation----

dataRB$DCPS <- c(rep(NA, 31), coredata(na.approx(zoo(dataRB$DCPS[31:84]))), rep(NA, 3))
dataRB$AGRO <- c(rep(NA, 23), coredata(na.approx(zoo(dataRB$AGRO[24:84]))), rep(NA, 3))
dataRB$GREV <- c(rep(NA, 31), coredata(na.approx(zoo(dataRB$GREV[32:87]))))
dataRB$GEXP <- c(rep(NA, 31), coredata(na.approx(zoo(dataRB$GEXP[32:87]))))
dataRB$energyTotal <- c(rep(NA, 31), coredata(na.approx(zoo(dataRB$energyTotal[32:84]))), rep(NA, 3))
dataRB$energyPopul <- c(rep(NA, 39), coredata(na.approx(zoo(dataRB$energyPopul[40:84]))), rep(NA, 3))
dataRB$energyPrice <- c(rep(NA, 23), coredata(na.approx(zoo(dataRB$energyPrice[24:80]))), rep(NA, 7))
dataRB$iloUnempl <- c(rep(NA, 23), coredata(na.approx(zoo(dataRB$iloUnempl[24:84]))), rep(NA, 3))
dataRB$taxProfit <- c(rep(NA, 31), coredata(na.approx(zoo(dataRB$taxProfit[24:87]))))

for (ie in colnames(dataRB)[7:12]){
  BOB <- zoo(dataRB[[ie]])
  BOB <- na.approx(BOB)
  dataRB[[ie]] = c(rep(NA, 27), coredata(BOB), rep(NA, 3))
}

rm(BOB, ie)


## Generating PCs----

# indexInstitute
INST_PCA = prcomp(dataRB[28:84, 7:12], scale = F, center = F)
dataRB$indexInstitute = c(rep(NA, 27), INST_PCA$x[,1], rep(NA, 3))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexInstitute (PC1) RB")

# indexBureau
INST_PCA = prcomp(dataRB[28:84, 7:12], scale = F, center = T)
dataRB$indexBureau = c(rep(NA, 27), INST_PCA$x[,2], rep(NA, 3))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"),
       main="indexBureau (PC2) RB")

# indexPayment
payData <- data.frame(dataRB$ALLCARDS[29:87],
                     dataRB$TERMIN2[29:87], dataRB$ATM[29:87])
colnames(payData) <- c("AllCards", "Terminals", "ATMs")
PAY_PCA = prcomp(payData, scale = T, center = T)
dataRB$indexPayment = c(rep(NA, 28), PAY_PCA$x[,1])
biplot(PAY_PCA, xlabs = c(rep("*", nrow(PAY_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexPayment (PC1) RB")

# indexDemocracy
democData <- data.frame(dataRB$GE[28:84], dataRB$PV[28:84], dataRB$RL[28:84],
                        dataRB$VA[28:84])
colnames(democData) <- c("GE", "PV" , "RL", "VA")
INST_PCA = prcomp(democData,center = T, scale = T)
dataRB$indexDemocracy = c(rep(NA, 27), INST_PCA$x[,1], rep(NA,3))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexDemocracy (PC1) RB")
rm(INST_PCA, PAY_PCA)


## Monthly data ----

dfM <- read.xlsx("dataRB.xlsx", sheet="dataM", detectDates = T)
dfQ <- dataRB
dfQ <- dfQ[1:86, ]
rm(dataRB)

# choose columns from quarterly data
names = c("Date", "NGDP", "WSGDP", "COFS", "Tax", "DCPS", "AGRO", "GREV",
          "GEXP", "taxProfit", "energyTotal", "energyPopul",
          "indexInstitute", "indexBureau", "indexPayment", "indexDemocracy",
          "iloUnempl", "NPLa", "NPLp", "INFL")
dfAll <- dfQ[, names]
rm(names)

# determ. seasonal (Q) components
dfAll$bvQ1 <- 0
dfAll$bvQ2 <- 0
dfAll$bvQ3 <- 0

for(i in 1:nrow(dfAll)){
  lstch <- str_sub(dfAll$Date[i], start = -1)
  if(lstch=="1"){dfAll$bvQ1[i] <- 1}
  if(lstch=="2"){dfAll$bvQ2[i] <- 1}
  if(lstch=="3"){dfAll$bvQ3[i] <- 1}

} # end for i

# Row of date as year.qtr format
dfAll$Date <- as.yearqtr(dfAll$Date, format = "%Y-%q")


## Monthly to Quarterly----

# РёРЅРґРµРєСЃС‹ РєРІР°СЂС‚Р°Р»РѕРІ РЅР° РєРѕРЅРµС† РїРµСЂРёРѕРґР°
idxQe <- seq(from=4, to=259, by=3)

# M2
dfAll$M2 <- dfM$M2[idxQe]

# M0
dfAll$M0 <- dfM$M0[idxQe]

# CH
dfAll$CH <- dfM$CH[idxQe]

# SID
dfAll$SID <- dfM$SID[idxQe]
dfAll$SID <- c(rep(NA, 27), coredata(na.approx(zoo(dfAll$SID[28:86]))))
#DR
dfAll$DR <- dfM$DR[idxQe]


## Feature generation----

# CM
dfAll$CM <- dfQ$COFS*100/dfM$M2[idxQe]

# volume of taxes
dfAll$vTax <- dfQ$Tax*dfQ$NGDP/100

dfWrk <- na.omit(dfAll)
write.csv(dfWrk, file="dataRB.csv", row.names = F, quote = F)


# Kyrgyzstan----

dataKG <- read_excel("dataKG.xlsx", sheet = "dataQ")


## Interpolation----

dataKG$DCPS <- c(rep(NA, 19), coredata(na.approx(zoo(dataKG$DCPS[20:84]))), rep(NA, 4))
dataKG$AGRO <- c(rep(NA, 19), coredata(na.approx(zoo(dataKG$AGRO[20:84]))), rep(NA, 4))
dataKG$WSGDP <- c(rep(NA, 35), coredata(na.approx(zoo(dataKG$WSGDP[36:84]))), rep(NA, 4))
dataKG$Tax <- c(rep(NA, 35), coredata(na.approx(zoo(dataKG$Tax[36:84]))), rep(NA, 4))
dataKG$Unempl <- c(rep(NA, 27), coredata(na.approx(zoo(dataKG$Unempl[28:88]))))
dataKG$ALLCARDS <- c(rep(NA, 31), coredata(na.approx(zoo(dataKG$ALLCARDS[32:84]))), 
                     rep(NA, 4))
dataKG$ATM <- c(rep(NA, 31), coredata(na.approx(zoo(dataKG$ATM[32:84]))), rep(NA, 4))
dataKG$TERMIN1 <- c(rep(NA, 31), coredata(na.approx(zoo(dataKG$TERMIN1[32:84]))), 
                    rep(NA, 4))
dataKG$SID <- c(rep(NA, 23), coredata(na.approx(zoo(dataKG$SID[24:88]))))

for (ie in colnames(dataKG)[8:13]){
  BOB <- zoo(dataKG[[ie]])
  BOB <- na.approx(BOB)
  dataKG[[ie]] = c(rep(NA, 19), coredata(BOB), rep(NA, 4))
}

rm(BOB, ie)


## Generating PCs----

# indexInstitute
INST_PCA = prcomp(dataKG[20:84, 8:13], scale = F, center = F)
dataKG$indexInstitute = c(rep(NA, 19), INST_PCA$x[,1], rep(NA, 4))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexInstitute (PC1) KG")

# indexBureau
INST_PCA = prcomp(dataKG[20:84, 8:13], scale = F, center = T)
dataKG$indexBureau = c(rep(NA, 19), INST_PCA$x[,2], rep(NA, 4))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexBureau (PC2) KG")

# indexPayment
payData <- data.frame(dataKG$ALLCARDS[32:84],
                     dataKG$TERMIN1[32:84], dataKG$ATM[32:84])
colnames(payData) <- c("AllCards", "Terminals", "ATMs")
PAY_PCA = prcomp(payData, scale = T, center = T)
dataKG$indexPayment = c(rep(NA, 31), PAY_PCA$x[,1], rep(NA, 4))
biplot(PAY_PCA, xlabs = c(rep("*", nrow(PAY_PCA$x))), col = c("#233f71","#c00000"),
       main="indexPayment (PC1) KG")

# indexDemocracy
democData <- data.frame(dataKG$GE[28:84], dataKG$PV[28:84], dataKG$RL[28:84],
                        dataKG$VA[28:84])
colnames(democData) <- c("GE", "PV" , "RL", "VA")
INST_PCA = prcomp(democData,center = T, scale = T)
dataKG$indexDemocracy = c(rep(NA, 27), INST_PCA$x[,1], rep(NA, 4))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexDemocracy (PC1) KG")
rm(INST_PCA, PAY_PCA)


## Monthly data ----

dfM <- read.xlsx("dataKG.xlsx", sheet="dataM", detectDates = T)
dfQ <- dataKG
dfQ <- dfQ[1:86, ]
rm(dataKG)

# choose columns from quarterly data
names = c("Date", "NGDP", "WSGDP", "Tax", "DCPS", "AGRO", "GREV",
          "GEXP", "indexInstitute", "indexBureau", "indexPayment", "indexDemocracy", 
          "NPLa", "NPLp", "SID", "Unempl")
dfAll <- dfQ[, names]
rm(names)

# determ. seasonal (Q) components
dfAll$bvQ1 <- 0
dfAll$bvQ2 <- 0
dfAll$bvQ3 <- 0

for(i in 1:nrow(dfAll)){
  lstch <- str_sub(dfAll$Date[i], start = -1)
  if(lstch=="1"){dfAll$bvQ1[i] <- 1}
  if(lstch=="2"){dfAll$bvQ2[i] <- 1}
  if(lstch=="3"){dfAll$bvQ3[i] <- 1}

} # end for i

# Row of date as year.qtr format
dfAll$Date <- as.yearqtr(dfAll$Date, format = "%Y-%q")

## Monthly to Quarterly----

idxQe <- seq(from=4, to=259, by=3)

# M2
dfAll$M2 <- dfM$M2[idxQe]

# M1
dfAll$M1 <- dfM$M2[idxQe]

# M0
dfAll$M0 <- dfM$M0[idxQe]

# CH
dfAll$CH <- dfM$CH[idxQe]

#DR
dfAll$DR <- dfM$DR[idxQe]


## Feature generation----

# CM
dfAll$CM <- dfAll$M0*100/dfAll$M2

# volume of taxes
dfAll$vTax <- dfQ$Tax*dfQ$NGDP/100

dfWrk <- na.omit(dfAll)
write.csv(dfWrk, file="dataKG.csv", row.names = F, quote = F)

# Armenia----

dataAR <- read_excel("dataAR.xlsx", sheet = "dataQ")


## Interpolation----

dataAR$DCPS <- c(rep(NA, 39), coredata(na.approx(zoo(dataAR$DCPS[40:84]))), rep(NA, 4))
dataAR$AGRO <- c(rep(NA, 35), coredata(na.approx(zoo(dataAR$AGRO[36:84]))), rep(NA, 4))
dataAR$WSGDP <- c(rep(NA, 27), coredata(na.approx(zoo(dataAR$WSGDP[28:84]))), rep(NA, 4))
dataAR$GREV <- c(rep(NA, 35), coredata(na.approx(zoo(dataAR$GREV[36:88]))))
dataAR$GEXP <- c(rep(NA, 35), coredata(na.approx(zoo(dataAR$GEXP[36:88]))))

for (ie in colnames(dataAR)[10:15]){
  BOB <- zoo(dataAR[[ie]])
  BOB <- na.approx(BOB)
  dataAR[[ie]] = c(rep(NA, 3), coredata(BOB), rep(NA, 4))
}

rm(BOB, ie)


## Generating PCs----

# indexInstitute
INST_PCA = prcomp(dataAR[4:84, 10:15], scale = F, center = F)
dataAR$indexInstitute = c(rep(NA, 3), INST_PCA$x[,1], rep(NA, 4))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexInstitute (PC1) KG")

# indexBureau
INST_PCA = prcomp(dataAR[4:84, 10:15], scale = F, center = T)
dataAR$indexBureau = c(rep(NA, 3), INST_PCA$x[,2], rep(NA, 4))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexBureau (PC2) KG")

# indexDemocracy
democData <- data.frame(dataAR$GE[4:84], dataAR$PV[4:84], dataAR$RL[4:84],
                        dataAR$VA[4:84])
colnames(democData) <- c("GE", "PV" , "RL", "VA")
INST_PCA = prcomp(democData,center = T, scale = T)
dataAR$indexDemocracy = c(rep(NA, 3), INST_PCA$x[,1], rep(NA, 4))
biplot(INST_PCA, xlabs = c(rep("*", nrow(INST_PCA$x))), col = c("#233f71","#c00000"), 
       main="indexDemocracy (PC1) KG")
rm(INST_PCA)


## Monthly data ----

dfM <- read.xlsx("dataAR.xlsx", sheet="dataM", detectDates = T)
dfQ <- dataAR
dfQ <- dfQ[1:88, ]
rm(dataAR)

# choose columns from quarterly data
names = c("Date", "NGDP", "WSGDP", "Tax", "DCPS", "AGRO", "GREV",
          "GEXP", "indexInstitute", "indexBureau", "indexDemocracy", 
          "NPLp", "SID", "Unempl")
dfAll <- dfQ[, names]
rm(names)

# determ. seasonal (Q) components
dfAll$bvQ1 <- 0
dfAll$bvQ2 <- 0
dfAll$bvQ3 <- 0

for(i in 1:nrow(dfAll)){
  lstch <- str_sub(dfAll$Date[i], start = -1)
  if(lstch=="1"){dfAll$bvQ1[i] <- 1}
  if(lstch=="2"){dfAll$bvQ2[i] <- 1}
  if(lstch=="3"){dfAll$bvQ3[i] <- 1}
  
} # end for i

# Row of date as year.qtr format
dfAll$Date <- as.yearqtr(dfAll$Date, format = "%Y-%q")

## Monthly to Quarterly----

idxQe <- seq(from=4, to=259, by=3)

# M2
dfAll$M2 <- dfM$M2[idxQe]

# M1
dfAll$M1 <- dfM$M2[idxQe]

# M0
dfAll$M0 <- dfM$M0[idxQe]

#DR
dfAll$DR <- dfM$DR[idxQe]


## Feature generation----

# CM
dfAll$CM <- dfAll$M0*100/dfAll$M2

# volume of taxes
dfAll$vTax <- dfQ$Tax*dfQ$NGDP/100

dfWrk <- na.omit(dfAll)
write.csv(dfWrk, file="dataAR.csv", row.names = F, quote = F)
