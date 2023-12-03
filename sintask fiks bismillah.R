#Mengimpor library yang diperlukan
library(tidyverse)
library(psych)
library(tseries)
library(plm)
library(car)
library(readxl)
library(lmtest)



#Mengimpor data
#Import data saham
LQ45 <- read_excel("D:/LUTFI/SEMESTER V/KP/Data Regresi Data panel.xlsx")
head(LQ45)
data=LQ45[2:10]
head(data)
str(data)

#Merapikan data
names(data)[1:10]<-c("Kode","Tahun",'CR','EPS','PER','BV','ROA','ROE','Close_Price')
data<-arrange(data,arrange(data,Tahun))
data$Kode<-as.factor(data$Kode)
data$Tahun=as.character(data$Tahun)
str(data)

#stat deskriptif variabel
tapply(data$Close_Price, data$Tahun, summary)
tapply(data$Close_Price, data$Kode, summary)

#Pemeriksaan asumsi
##Linearitas
pairs.panels(data[,-1:-2], 
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = TRUE)


##Normalitas
hist(data$Close_Price, col='steelblue', main='Normal')
ks.test(data$Close_Price, 'pnorm')
shapiro.test(data$Close_Price)

#kemungkinan model terbentuk
M1=CR+EPS+PER+BV+ROA+ROE
M2=EPS+PER+BV+ROA+ROE
M3=CR+EPS+BV+ROA+ROE
M4=CR+EPS+PER+BV+ROE
M5=CR+EPS+PER+BV+ROA
M6=PER+BV+ROA+ROE
M7=EPS+BV+ROA+ROE
M8=EPS+PER+BV+ROA
M9=EPS+PER+BV+ROE
M10=CR+BV+ROA+ROE
M11=CR+PER+ROA+ROE
M12=CR+PER+BV+ROE
M13=CR+PER+BV+ROA
M14=CR+EPS+ROA+ROE
M15=CR+EPS+BV+ROE
M16=CR+EPS+BV+ROA
M17=CR+EPS+PER+ROE
M18=CR+EPS+PER+ROA
M19=CR+EPS+PER+BV
M20=BV+ROA+ROE
M21=PER+ROA+ROE
M22=PER+BV+ROE
M23=PER+BV+ROA
M24=EPS+ROA+ROE
M25=EPS+BV+ROE
M26=EPS+BV+ROA
M27=EPS+PER+ROE
M28=EPS+PER+ROA
M29=EPS+PER+BV
M30=CR+ROA+ROE
M31=CR+BV+ROE
M32=CR+BV+ROA
M33=CR+PER+ROE
M34=CR+PER+ROA
M35=CR+PER+BV
M36=CR+EPS+ROE
M37=CR+EPS+ROA
M38=CR+EPS+BV
M39=CR+EPS+PER
M40=ROA+ROE
M41=BV+ROE
M42=BV+ROA
M43=PER+ROE
M44=PER+ROA
M45=PER+BV
M46=EPS+ROE
M47=EPS+ROA
M48=EPS+BV
M49=EPS+PER 
M50=CR+ROE
M51=CR+ROA
M52=CR+BV
M53=CR+PER
M54=CR+EPS
M55=ROE
M56=ROA
M57=BV
M58=PER
M59=EPS
M60=CR
M61=EPS+PER+ROA+ROE
M62=CR+PER+BV+ROA+ROE
M63=CR+EPS+PER+ROA+ROE

#Membentuk model
#MODEL CEM
cem1=plm(Close_Price~CR+EPS+PER+BV+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem2=plm(Close_Price~EPS+PER+BV+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem3=plm(Close_Price~CR+EPS+BV+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem4=plm(Close_Price~CR+EPS+PER+BV+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem5=plm(Close_Price~CR+EPS+PER+BV+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem6=plm(Close_Price~PER+BV+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem7=plm(Close_Price~EPS+BV+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem8=plm(Close_Price~EPS+PER+BV+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem9=plm(Close_Price~EPS+PER+BV+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem10=plm(Close_Price~CR+BV+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem11=plm(Close_Price~CR+PER+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem12=plm(Close_Price~CR+PER+BV+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem13=plm(Close_Price~CR+PER+BV+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem14=plm(Close_Price~CR+EPS+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem15=plm(Close_Price~CR+EPS+BV+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem16=plm(Close_Price~CR+EPS+BV+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem17=plm(Close_Price~CR+EPS+PER+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem18=plm(Close_Price~CR+EPS+PER+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem19=plm(Close_Price~CR+EPS+PER+BV,data,model="pooling",index=c("Kode","Tahun"))
cem20=plm(Close_Price~BV+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem21=plm(Close_Price~PER+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem22=plm(Close_Price~PER+BV+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem23=plm(Close_Price~PER+BV+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem24=plm(Close_Price~EPS+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem25=plm(Close_Price~EPS+BV+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem26=plm(Close_Price~EPS+BV+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem27=plm(Close_Price~EPS+PER+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem28=plm(Close_Price~EPS+PER+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem29=plm(Close_Price~EPS+PER+BV,data,model="pooling",index=c("Kode","Tahun"))
cem30=plm(Close_Price~CR+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem31=plm(Close_Price~CR+BV+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem32=plm(Close_Price~CR+BV+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem33=plm(Close_Price~CR+PER+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem34=plm(Close_Price~CR+PER+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem35=plm(Close_Price~CR+PER+BV,data,model="pooling",index=c("Kode","Tahun"))
cem36=plm(Close_Price~CR+EPS+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem37=plm(Close_Price~CR+EPS+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem38=plm(Close_Price~CR+EPS+BV,data,model="pooling",index=c("Kode","Tahun"))
cem39=plm(Close_Price~CR+EPS+PER,data,model="pooling",index=c("Kode","Tahun"))
cem40=plm(Close_Price~ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem41=plm(Close_Price~BV+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem42=plm(Close_Price~BV+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem43=plm(Close_Price~PER+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem44=plm(Close_Price~PER+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem45=plm(Close_Price~PER+BV,data,model="pooling",index=c("Kode","Tahun"))
cem46=plm(Close_Price~EPS+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem47=plm(Close_Price~EPS+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem48=plm(Close_Price~EPS+BV,data,model="pooling",index=c("Kode","Tahun"))
cem49=plm(Close_Price~EPS+PER,data,model="pooling",index=c("Kode","Tahun"))
cem50=plm(Close_Price~CR+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem51=plm(Close_Price~CR+ROA,data,model="pooling",index=c("Kode","Tahun"))
cem52=plm(Close_Price~CR+BV,data,model="pooling",index=c("Kode","Tahun"))
cem53=plm(Close_Price~CR+PER,data,model="pooling",index=c("Kode","Tahun"))
cem54=plm(Close_Price~CR+EPS,data,model="pooling",index=c("Kode","Tahun"))
cem55=plm(Close_Price~ROE,data,model="pooling",index=c("Kode","Tahun"))
cem56=plm(Close_Price~ROA,data,model="pooling",index=c("Kode","Tahun"))
cem57=plm(Close_Price~BV,data,model="pooling",index=c("Kode","Tahun"))
cem58=plm(Close_Price~PER,data,model="pooling",index=c("Kode","Tahun"))
cem59=plm(Close_Price~EPS,data,model="pooling",index=c("Kode","Tahun"))
cem60=plm(Close_Price~CR,data,model="pooling",index=c("Kode","Tahun"))
cem61=plm(Close_Price~EPS+PER+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem62=plm(Close_Price~CR+PER+BV+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))
cem63=plm(Close_Price~CR+EPS+PER+ROA+ROE,data,model="pooling",index=c("Kode","Tahun"))

#MODEL FEM
fem1=plm(Close_Price~CR+EPS+PER+BV+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem2=plm(Close_Price~EPS+PER+BV+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem3=plm(Close_Price~CR+EPS+BV+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem4=plm(Close_Price~CR+EPS+PER+BV+ROE,data,model="within",index=c("Kode","Tahun"))
fem5=plm(Close_Price~CR+EPS+PER+BV+ROA,data,model="within",index=c("Kode","Tahun"))
fem6=plm(Close_Price~PER+BV+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem7=plm(Close_Price~EPS+BV+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem8=plm(Close_Price~EPS+PER+BV+ROA,data,model="within",index=c("Kode","Tahun"))
fem9=plm(Close_Price~EPS+PER+BV+ROE,data,model="within",index=c("Kode","Tahun"))
fem10=plm(Close_Price~CR+BV+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem11=plm(Close_Price~CR+PER+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem12=plm(Close_Price~CR+PER+BV+ROE,data,model="within",index=c("Kode","Tahun"))
fem13=plm(Close_Price~CR+PER+BV+ROA,data,model="within",index=c("Kode","Tahun"))
fem14=plm(Close_Price~CR+EPS+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem15=plm(Close_Price~CR+EPS+BV+ROE,data,model="within",index=c("Kode","Tahun"))
fem16=plm(Close_Price~CR+EPS+BV+ROA,data,model="within",index=c("Kode","Tahun"))
fem17=plm(Close_Price~CR+EPS+PER+ROE,data,model="within",index=c("Kode","Tahun"))
fem18=plm(Close_Price~CR+EPS+PER+ROA,data,model="within",index=c("Kode","Tahun"))
fem19=plm(Close_Price~CR+EPS+PER+BV,data,model="within",index=c("Kode","Tahun"))
fem20=plm(Close_Price~BV+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem21=plm(Close_Price~PER+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem22=plm(Close_Price~PER+BV+ROE,data,model="within",index=c("Kode","Tahun"))
fem23=plm(Close_Price~PER+BV+ROA,data,model="within",index=c("Kode","Tahun"))
fem24=plm(Close_Price~EPS+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem25=plm(Close_Price~EPS+BV+ROE,data,model="within",index=c("Kode","Tahun"))
fem26=plm(Close_Price~EPS+BV+ROA,data,model="within",index=c("Kode","Tahun"))
fem27=plm(Close_Price~EPS+PER+ROE,data,model="within",index=c("Kode","Tahun"))
fem28=plm(Close_Price~EPS+PER+ROA,data,model="within",index=c("Kode","Tahun"))
fem29=plm(Close_Price~EPS+PER+BV,data,model="within",index=c("Kode","Tahun"))
fem30=plm(Close_Price~CR+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem31=plm(Close_Price~CR+BV+ROE,data,model="within",index=c("Kode","Tahun"))
fem32=plm(Close_Price~CR+BV+ROA,data,model="within",index=c("Kode","Tahun"))
fem33=plm(Close_Price~CR+PER+ROE,data,model="within",index=c("Kode","Tahun"))
fem34=plm(Close_Price~CR+PER+ROA,data,model="within",index=c("Kode","Tahun"))
fem35=plm(Close_Price~CR+PER+BV,data,model="within",index=c("Kode","Tahun"))
fem36=plm(Close_Price~CR+EPS+ROE,data,model="within",index=c("Kode","Tahun"))
fem37=plm(Close_Price~CR+EPS+ROA,data,model="within",index=c("Kode","Tahun"))
fem38=plm(Close_Price~CR+EPS+BV,data,model="within",index=c("Kode","Tahun"))
fem39=plm(Close_Price~CR+EPS+PER,data,model="within",index=c("Kode","Tahun"))
fem40=plm(Close_Price~ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem41=plm(Close_Price~BV+ROE,data,model="within",index=c("Kode","Tahun"))
fem42=plm(Close_Price~BV+ROA,data,model="within",index=c("Kode","Tahun"))
fem43=plm(Close_Price~PER+ROE,data,model="within",index=c("Kode","Tahun"))
fem44=plm(Close_Price~PER+ROA,data,model="within",index=c("Kode","Tahun"))
fem45=plm(Close_Price~PER+BV,data,model="within",index=c("Kode","Tahun"))
fem46=plm(Close_Price~EPS+ROE,data,model="within",index=c("Kode","Tahun"))
fem47=plm(Close_Price~EPS+ROA,data,model="within",index=c("Kode","Tahun"))
fem48=plm(Close_Price~EPS+BV,data,model="within",index=c("Kode","Tahun"))
fem49=plm(Close_Price~EPS+PER,data,model="within",index=c("Kode","Tahun"))
fem50=plm(Close_Price~CR+ROE,data,model="within",index=c("Kode","Tahun"))
fem51=plm(Close_Price~CR+ROA,data,model="within",index=c("Kode","Tahun"))
fem52=plm(Close_Price~CR+BV,data,model="within",index=c("Kode","Tahun"))
fem53=plm(Close_Price~CR+PER,data,model="within",index=c("Kode","Tahun"))
fem54=plm(Close_Price~CR+EPS,data,model="within",index=c("Kode","Tahun"))
fem55=plm(Close_Price~ROE,data,model="within",index=c("Kode","Tahun"))
fem56=plm(Close_Price~ROA,data,model="within",index=c("Kode","Tahun"))
fem57=plm(Close_Price~BV,data,model="within",index=c("Kode","Tahun"))
fem58=plm(Close_Price~PER,data,model="within",index=c("Kode","Tahun"))
fem59=plm(Close_Price~EPS,data,model="within",index=c("Kode","Tahun"))
fem60=plm(Close_Price~CR,data,model="within",index=c("Kode","Tahun"))
fem61=plm(Close_Price~EPS+PER+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem62=plm(Close_Price~CR+PER+BV+ROA+ROE,data,model="within",index=c("Kode","Tahun"))
fem63=plm(Close_Price~CR+EPS+PER+ROA+ROE,data,model="within",index=c("Kode","Tahun"))

#MODEL REM
rem1=plm(Close_Price~CR+EPS+PER+BV+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem2=plm(Close_Price~EPS+PER+BV+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem3=plm(Close_Price~CR+EPS+BV+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem4=plm(Close_Price~CR+EPS+PER+BV+ROE,data,model="random",index=c("Kode","Tahun"))
rem5=plm(Close_Price~CR+EPS+PER+BV+ROA,data,model="random",index=c("Kode","Tahun"))
rem6=plm(Close_Price~PER+BV+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem7=plm(Close_Price~EPS+BV+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem8=plm(Close_Price~EPS+PER+BV+ROA,data,model="random",index=c("Kode","Tahun"))
rem9=plm(Close_Price~EPS+PER+BV+ROE,data,model="random",index=c("Kode","Tahun"))
rem10=plm(Close_Price~CR+BV+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem11=plm(Close_Price~CR+PER+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem12=plm(Close_Price~CR+PER+BV+ROE,data,model="random",index=c("Kode","Tahun"))
rem13=plm(Close_Price~CR+PER+BV+ROA,data,model="random",index=c("Kode","Tahun"))
rem14=plm(Close_Price~CR+EPS+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem15=plm(Close_Price~CR+EPS+BV+ROE,data,model="random",index=c("Kode","Tahun"))
rem16=plm(Close_Price~CR+EPS+BV+ROA,data,model="random",index=c("Kode","Tahun"))
rem17=plm(Close_Price~CR+EPS+PER+ROE,data,model="random",index=c("Kode","Tahun"))
rem18=plm(Close_Price~CR+EPS+PER+ROA,data,model="random",index=c("Kode","Tahun"))
rem19=plm(Close_Price~CR+EPS+PER+BV,data,model="random",index=c("Kode","Tahun"))
rem20=plm(Close_Price~BV+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem21=plm(Close_Price~PER+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem22=plm(Close_Price~PER+BV+ROE,data,model="random",index=c("Kode","Tahun"))
rem23=plm(Close_Price~PER+BV+ROA,data,model="random",index=c("Kode","Tahun"))
rem24=plm(Close_Price~EPS+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem25=plm(Close_Price~EPS+BV+ROE,data,model="random",index=c("Kode","Tahun"))
rem26=plm(Close_Price~EPS+BV+ROA,data,model="random",index=c("Kode","Tahun"))
rem27=plm(Close_Price~EPS+PER+ROE,data,model="random",index=c("Kode","Tahun"))
rem28=plm(Close_Price~EPS+PER+ROA,data,model="random",index=c("Kode","Tahun"))
rem29=plm(Close_Price~EPS+PER+BV,data,model="random",index=c("Kode","Tahun"))
rem30=plm(Close_Price~CR+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem31=plm(Close_Price~CR+BV+ROE,data,model="random",index=c("Kode","Tahun"))
rem32=plm(Close_Price~CR+BV+ROA,data,model="random",index=c("Kode","Tahun"))
rem33=plm(Close_Price~CR+PER+ROE,data,model="random",index=c("Kode","Tahun"))
rem34=plm(Close_Price~CR+PER+ROA,data,model="random",index=c("Kode","Tahun"))
rem35=plm(Close_Price~CR+PER+BV,data,model="random",index=c("Kode","Tahun"))
rem36=plm(Close_Price~CR+EPS+ROE,data,model="random",index=c("Kode","Tahun"))
rem37=plm(Close_Price~CR+EPS+ROA,data,model="random",index=c("Kode","Tahun"))
rem38=plm(Close_Price~CR+EPS+BV,data,model="random",index=c("Kode","Tahun"))
rem39=plm(Close_Price~CR+EPS+PER,data,model="random",index=c("Kode","Tahun"))
rem40=plm(Close_Price~ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem41=plm(Close_Price~BV+ROE,data,model="random",index=c("Kode","Tahun"))
rem42=plm(Close_Price~BV+ROA,data,model="random",index=c("Kode","Tahun"))
rem43=plm(Close_Price~PER+ROE,data,model="random",index=c("Kode","Tahun"))
rem44=plm(Close_Price~PER+ROA,data,model="random",index=c("Kode","Tahun"))
rem45=plm(Close_Price~PER+BV,data,model="random",index=c("Kode","Tahun"))
rem46=plm(Close_Price~EPS+ROE,data,model="random",index=c("Kode","Tahun"))
rem47=plm(Close_Price~EPS+ROA,data,model="random",index=c("Kode","Tahun"))
rem48=plm(Close_Price~EPS+BV,data,model="random",index=c("Kode","Tahun"))
rem49=plm(Close_Price~EPS+PER,data,model="random",index=c("Kode","Tahun"))
rem50=plm(Close_Price~CR+ROE,data,model="random",index=c("Kode","Tahun"))
rem51=plm(Close_Price~CR+ROA,data,model="random",index=c("Kode","Tahun"))
rem52=plm(Close_Price~CR+BV,data,model="random",index=c("Kode","Tahun"))
rem53=plm(Close_Price~CR+PER,data,model="random",index=c("Kode","Tahun"))
rem54=plm(Close_Price~CR+EPS,data,model="random",index=c("Kode","Tahun"))
rem55=plm(Close_Price~ROE,data,model="random",index=c("Kode","Tahun"))
rem56=plm(Close_Price~ROA,data,model="random",index=c("Kode","Tahun"))
rem57=plm(Close_Price~BV,data,model="random",index=c("Kode","Tahun"))
rem58=plm(Close_Price~PER,data,model="random",index=c("Kode","Tahun"))
rem59=plm(Close_Price~EPS,data,model="random",index=c("Kode","Tahun"))
rem60=plm(Close_Price~CR,data,model="random",index=c("Kode","Tahun"))
rem61=plm(Close_Price~EPS+PER+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem62=plm(Close_Price~CR+PER+BV+ROA+ROE,data,model="random",index=c("Kode","Tahun"))
rem63=plm(Close_Price~CR+EPS+PER+ROA+ROE,data,model="random",index=c("Kode","Tahun"))


#Uji Chow
pooltest(cem1,fem1)
pooltest(cem2,fem2)
pooltest(cem3,fem3)
pooltest(cem4,fem4)
pooltest(cem5,fem5)
pooltest(cem6,fem6)
pooltest(cem7,fem7)
pooltest(cem8,fem8)
pooltest(cem9,fem9)
pooltest(cem10,fem10)
pooltest(cem11,fem11)
pooltest(cem12,fem12)
pooltest(cem13,fem13)
pooltest(cem14,fem14)
pooltest(cem15,fem15)
pooltest(cem16,fem16)
pooltest(cem17,fem17)
pooltest(cem18,fem18)
pooltest(cem19,fem19)
pooltest(cem20,fem20)
pooltest(cem21,fem21)
pooltest(cem22,fem22)
pooltest(cem23,fem23)
pooltest(cem24,fem24)
pooltest(cem25,fem25)
pooltest(cem26,fem26)
pooltest(cem27,fem27)
pooltest(cem28,fem28)
pooltest(cem29,fem29)
pooltest(cem30,fem30)
pooltest(cem31,fem31)
pooltest(cem32,fem32)
pooltest(cem33,fem33)
pooltest(cem34,fem34)
pooltest(cem35,fem35)
pooltest(cem36,fem36)
pooltest(cem37,fem37)
pooltest(cem38,fem38)
pooltest(cem39,fem39)
pooltest(cem40,fem40)
pooltest(cem41,fem41)
pooltest(cem42,fem42)
pooltest(cem43,fem43)
pooltest(cem44,fem44)
pooltest(cem45,fem45)
pooltest(cem46,fem46)
pooltest(cem47,fem47)
pooltest(cem48,fem48)
pooltest(cem49,fem49)
pooltest(cem50,fem50)
pooltest(cem51,fem51)
pooltest(cem52,fem52)
pooltest(cem53,fem53)
pooltest(cem54,fem54)
pooltest(cem55,fem55)
pooltest(cem56,fem56)
pooltest(cem57,fem57)
pooltest(cem58,fem58)
pooltest(cem59,fem59)
pooltest(cem60,fem60)
pooltest(cem61,fem61)
pooltest(cem62,fem62)
pooltest(cem63,fem63)

#Uji Hausmann
phtest(fem1,rem1)
phtest(fem2,rem2)
phtest(fem3,rem3)
phtest(fem4,rem4)
phtest(fem5,rem5)
phtest(fem6,rem6)
phtest(fem7,rem7)
phtest(fem8,rem8)
phtest(fem9,rem9)
phtest(fem10,rem10)
phtest(fem11,rem11)
phtest(fem12,rem12)
phtest(fem13,rem13)
phtest(fem14,rem14)
phtest(fem15,rem15)
phtest(fem16,rem16)
phtest(fem17,rem17)
phtest(fem18,rem18)
phtest(fem19,rem19)
phtest(fem20,rem20)
phtest(fem21,rem21)
phtest(fem22,rem22)
phtest(fem23,rem23)
phtest(fem24,rem24)
phtest(fem25,rem25)
phtest(fem26,rem26)
phtest(fem27,rem27)
phtest(fem28,rem28)
phtest(fem29,rem29)
phtest(fem30,rem30)
phtest(fem31,rem31)
phtest(fem32,rem32)
phtest(fem33,rem33)
phtest(fem34,rem34)
phtest(fem35,rem35)
phtest(fem36,rem36)
phtest(fem37,rem37)
phtest(fem38,rem38)
phtest(fem39,rem39)
phtest(fem40,rem40)
phtest(fem41,rem41)
phtest(fem42,rem42)
phtest(fem43,rem43)
phtest(fem44,rem44)
phtest(fem45,rem45)
phtest(fem46,rem46)
phtest(fem47,rem47)
phtest(fem48,rem48)
phtest(fem49,rem49)
phtest(fem50,rem50)
phtest(fem51,rem51)
phtest(fem52,rem52)
phtest(fem53,rem53)
phtest(fem54,rem54)
phtest(fem55,rem55)
phtest(fem56,rem56)
phtest(fem57,rem57)
phtest(fem58,rem58)
phtest(fem59,rem59)
phtest(fem60,rem60)
phtest(fem61,rem61)
phtest(fem62,rem62)
phtest(fem63,rem63)

#Uji Breusch-Pagan
#model 1
#Uji efek kali silang maupun waktu
plmtest(rem1,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem1,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem1,effect="time",type="bp") 
#model 2
#Uji efek kali silang maupun waktu
plmtest(rem2,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem2,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem2,effect="time",type="bp") 
#model 3
#Uji efek kali silang maupun waktu
plmtest(rem3,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem3,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem3,effect="time",type="bp") 
#model 4
#Uji efek kali silang maupun waktu
plmtest(rem4,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem4,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem4,effect="time",type="bp") 
#model 5
#Uji efek kali silang maupun waktu
plmtest(rem5,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem5,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem5,effect="time",type="bp") 
#model 6
#Uji efek kali silang maupun waktu
plmtest(rem6,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem6,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem6,effect="time",type="bp") 
#model 7
#Uji efek kali silang maupun waktu
plmtest(rem7,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem7,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem7,effect="time",type="bp") 
#model 8
#Uji efek kali silang maupun waktu
plmtest(rem8,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem8,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem8,effect="time",type="bp") 
#model 9
#Uji efek kali silang maupun waktu
plmtest(rem9,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem9,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem9,effect="time",type="bp") 
#model 10
#Uji efek kali silang maupun waktu
plmtest(rem10,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem10,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem10,effect="time",type="bp") 
#model 11
#Uji efek kali silang maupun waktu
plmtest(rem11,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem11,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem11,effect="time",type="bp") 
#model 12
#Uji efek kali silang maupun waktu
plmtest(rem12,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem12,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem12,effect="time",type="bp") 
#model 13
#Uji efek kali silang maupun waktu
plmtest(rem13,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem13,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem13,effect="time",type="bp") 
#model 14
#Uji efek kali silang maupun waktu
plmtest(fem14,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem14,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem14,effect="time",type="bp") 
#model 15
#Uji efek kali silang maupun waktu
plmtest(rem15,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem15,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem15,effect="time",type="bp") 
#model 16
#Uji efek kali silang maupun waktu
plmtest(rem16,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem16,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem16,effect="time",type="bp") 
#model 17
#Uji efek kali silang maupun waktu
plmtest(fem17,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem17,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem17,effect="time",type="bp") 
#model 18
#Uji efek kali silang maupun waktu
plmtest(fem18,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem18,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem18,effect="time",type="bp") 
#model 19
#Uji efek kali silang maupun waktu
plmtest(rem19,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem19,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem19,effect="time",type="bp") 
#model 20
#Uji efek kali silang maupun waktu
plmtest(rem20,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem20,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem20,effect="time",type="bp") 
#model 21
#Uji efek kali silang maupun waktu
plmtest(rem21,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem21,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem21,effect="time",type="bp") 
#model 22
#Uji efek kali silang maupun waktu
plmtest(rem22,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem22,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem22,effect="time",type="bp") 
#model 23
#Uji efek kali silang maupun waktu
plmtest(rem23,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem23,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem23,effect="time",type="bp") 
#model 24
#Uji efek kali silang maupun waktu
plmtest(fem24,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem24,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem24,effect="time",type="bp") 
#model 25
#Uji efek kali silang maupun waktu
plmtest(rem25,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem25,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem25,effect="time",type="bp") 
#model 26
#Uji efek kali silang maupun waktu
plmtest(rem26,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem26,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem26,effect="time",type="bp") 
#model 27
#Uji efek kali silang maupun waktu
plmtest(fem27,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem27,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem27,effect="time",type="bp") 
#model 28
#Uji efek kali silang maupun waktu
plmtest(fem28,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem28,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem28,effect="time",type="bp") 
#model 29
#Uji efek kali silang maupun waktu
plmtest(rem29,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem29,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem29,effect="time",type="bp") 
#model 30
#Uji efek kali silang maupun waktu
plmtest(rem30,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem30,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem30,effect="time",type="bp") 
#model 31
#Uji efek kali silang maupun waktu
plmtest(rem31,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem31,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem31,effect="time",type="bp") 
#model 32
#Uji efek kali silang maupun waktu
plmtest(rem32,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem32,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem32,effect="time",type="bp") 
#model 33
#Uji efek kali silang maupun waktu
plmtest(rem33,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem33,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem33,effect="time",type="bp") 
#model 34
#Uji efek kali silang maupun waktu
plmtest(rem34,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem34,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem34,effect="time",type="bp") 
#model 35
#Uji efek kali silang maupun waktu
plmtest(rem35,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem35,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem35,effect="time",type="bp") 
#model 36
#Uji efek kali silang maupun waktu
plmtest(fem36,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem36,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem36,effect="time",type="bp") 
#model 37
#Uji efek kali silang maupun waktu
plmtest(fem37,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem37,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem37,effect="time",type="bp") 
#model 38
#Uji efek kali silang maupun waktu
plmtest(rem38,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem38,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem38,effect="time",type="bp") 
#model 39
#Uji efek kali silang maupun waktu
plmtest(fem39,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem39,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem39,effect="time",type="bp") 
#model 40
#Uji efek kali silang maupun waktu
plmtest(rem40,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem40,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem40,effect="time",type="bp") 
#model 41
#Uji efek kali silang maupun waktu
plmtest(rem41,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem41,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem41,effect="time",type="bp") 
#model 42
#Uji efek kali silang maupun waktu
plmtest(rem42,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem42,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem42,effect="time",type="bp") 
#model 43
#Uji efek kali silang maupun waktu
plmtest(rem43,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem43,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem43,effect="time",type="bp") 
#model 44
#Uji efek kali silang maupun waktu
plmtest(rem44,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem44,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem44,effect="time",type="bp") 
#model 45
#Uji efek kali silang maupun waktu
plmtest(rem45,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem45,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem45,effect="time",type="bp") 
#model 46
#Uji efek kali silang maupun waktu
plmtest(fem46,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem46,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem46,effect="time",type="bp") 
#model 47
#Uji efek kali silang maupun waktu
plmtest(fem47,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem47,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem47,effect="time",type="bp") 
#model 48
#Uji efek kali silang maupun waktu
plmtest(rem48,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem48,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem48,effect="time",type="bp") 
#model 49
#Uji efek kali silang maupun waktu
plmtest(fem49,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem49,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem49,effect="time",type="bp") 
#model 50
#Uji efek kali silang maupun waktu
plmtest(rem50,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem50,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem50,effect="time",type="bp") 
#model 51
#Uji efek kali silang maupun waktu
plmtest(rem51,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem51,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem51,effect="time",type="bp") 
#model 52
#Uji efek kali silang maupun waktu
plmtest(rem52,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem52,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem52,effect="time",type="bp") 
#model 53
#Uji efek kali silang maupun waktu
plmtest(rem53,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem53,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem53,effect="time",type="bp") 
#model 54
#Uji efek kali silang maupun waktu
plmtest(fem54,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem54,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem54,effect="time",type="bp") 
#model 55
#Uji efek kali silang maupun waktu
plmtest(rem55,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem55,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem55,effect="time",type="bp") 
#model 56
#Uji efek kali silang maupun waktu
plmtest(rem56,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem56,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem56,effect="time",type="bp") 
#model 57
#Uji efek kali silang maupun waktu
plmtest(rem57,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem57,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem57,effect="time",type="bp") 
#model 58
#Uji efek kali silang maupun waktu
plmtest(rem58,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem58,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem58,effect="time",type="bp") 
#model 59
#Uji efek kali silang maupun waktu
plmtest(fem59,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem59,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem59,effect="time",type="bp") 
#model 60
#Uji efek kali silang maupun waktu
plmtest(rem60,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem60,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem60,effect="time",type="bp") 
#model 61
#Uji efek kali silang maupun waktu
plmtest(fem61,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem61,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem61,effect="time",type="bp") 
#model 62
#Uji efek kali silang maupun waktu
plmtest(rem62,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(rem62,effect="individual",type="bp")
#Uji efek waktu
plmtest(rem62,effect="time",type="bp") 
#model 63
#Uji efek kali silang maupun waktu
plmtest(fem63,effect="twoways",type="bp")
#Uji efek kali silang
plmtest(fem63,effect="individual",type="bp")
#Uji efek waktu
plmtest(fem63,effect="time",type="bp")
 


#Uji signifikansi parameter (Uji Wald)
#model 1
model1=plm(Close_Price~CR+EPS+PER+BV+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model1)
#model 2
model2=plm(Close_Price~EPS+PER+BV+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model2)
#model 3
model3=plm(Close_Price~CR+EPS+BV+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model3)
#model 4
model4=plm(Close_Price~CR+EPS+PER+BV+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model4)
#model 5
model5=plm(Close_Price~CR+EPS+PER+BV+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model5)
#model 6
model6=plm(Close_Price~PER+BV+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model6)
#model 7
model7=plm(Close_Price~EPS+BV+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model7)
#model 8
model8=plm(Close_Price~EPS+PER+BV+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model8)
#model 9
model9=plm(Close_Price~EPS+PER+BV+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model9)
#model 10
model10=plm(Close_Price~CR+BV+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model10)
#model 11
model11=plm(Close_Price~CR+PER+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model11)
#model 12
model12=plm(Close_Price~CR+PER+BV+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model12)
#model 13
model13=plm(Close_Price~CR+PER+BV+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model13)
#model 14
model14=plm(Close_Price~CR+EPS+ROA+ROE,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model14)
#model 15
model15=plm(Close_Price~CR+EPS+BV+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model15)
#model 16
model16=plm(Close_Price~CR+EPS+BV+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model16)
#model17
model17=plm(Close_Price~CR+EPS+PER+ROE,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model17)
#model 18
model18=plm(Close_Price~CR+EPS+PER+ROA,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model18)
#model 19
model19=plm(Close_Price~CR+EPS+PER+BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model19)
#model 20
model20=plm(Close_Price~BV+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model20)
#model 21
model21=plm(Close_Price~PER+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model21)
#model 22
model22=plm(Close_Price~PER+BV+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model22)
#model 23
model23=plm(Close_Price~PER+BV+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model23)
#model 24
model24=plm(Close_Price~EPS+ROA+ROE,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model24)
#model 25
model25=plm(Close_Price~EPS+BV+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model25)
#model 26
model26=plm(Close_Price~EPS+BV+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model26)
#model 27
model27=plm(Close_Price~EPS+PER+ROE,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model27)
#model 28
model28=plm(Close_Price~EPS+PER+ROA,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model28)
#model 29
model29=plm(Close_Price~EPS+PER+BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model29)
#model 30
model30=plm(Close_Price~CR+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model30)
#model 31
model31=plm(Close_Price~CR+BV+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model31)
#model 32
model32=plm(Close_Price~CR+BV+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model32)
#model 33
model33=plm(Close_Price~CR+PER+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model33)
#model 34
model34=plm(Close_Price~CR+PER+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model34)
#model 35
model35=plm(Close_Price~CR+PER+BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model35)
#model 36
model36=plm(Close_Price~CR+EPS+ROE,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model36)
#model 37
model37=plm(Close_Price~CR+EPS+ROA,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model37)
#model 38
model38=plm(Close_Price~CR+EPS+BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model38)
#model 39
model39=plm(Close_Price~CR+EPS+PER,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model39)
#model 40
model40=plm(Close_Price~ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model40)
#model 41
model41=plm(Close_Price~BV+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model41)
#model 42
model42=plm(Close_Price~BV+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model42)
#model 43
model43=plm(Close_Price~PER+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model43)
#model 44
model44=plm(Close_Price~PER+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model44)
#model 45
model45=plm(Close_Price~PER+BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model45)
#model 46
model46=plm(Close_Price~EPS+ROE,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model46)
#model 47
model47=plm(Close_Price~EPS+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model47)
#model 48
model48=plm(Close_Price~EPS+BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model48)
#model 49
model49=plm(Close_Price~EPS+PER,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model49)
#model 50
model50=plm(Close_Price~CR+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model50)
#model 51
model51=plm(Close_Price~CR+ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model51)
#model 52
model52=plm(Close_Price~CR+BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model52)
#model 53
model53=plm(Close_Price~CR+PER,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model53)
#model 54
model54=plm(Close_Price~CR+EPS,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model54)
#model 55
model55=plm(Close_Price~ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model55)
#model 56
model56=plm(Close_Price~ROA,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model56)
#model 57
model57=plm(Close_Price~BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model57)
#model 58
model58=plm(Close_Price~PER,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model58)
#model 59
model59=plm(Close_Price~EPS,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model59)
#model 60
model60=plm(Close_Price~CR,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model60)
#model 61
model61=plm(Close_Price~EPS+PER+ROA+ROE,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model61)
#model 62
model62=plm(Close_Price~CR+PER+BV+ROA+ROE,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model62)
#model 63
model63=plm(Close_Price~CR+EPS+PER+ROA+ROE,data,model="within",effect="individual",index=c("Kode","Tahun"))
summary(model63)


ranef(m4, type='level')

#model yang sudah signifikan seluruh variabel independennya : 29,44,45,48,49,56,57,58,59
#Diagnostic Checking
##Uji Korelasi Serial (gaada semua)
pdwtest(model29) 
pdwtest(model44) 
pdwtest(model45) 
pdwtest(model48) 
pdwtest(model49)
pdwtest(model56)
pdwtest(model57)
pdwtest(model58)
pdwtest(model59)

##Uji No-Multikolinearitas
pool29=plm(Close_Price~EPS+PER+BV,data,model="pooling",index=c("Kode","Tahun"))
vif(pool29) 
pool44=plm(Close_Price~PER+ROA,data,model="pooling",index=c("Kode","Tahun"))
vif(pool44)
pool45=plm(Close_Price~PER+BV,data,model="pooling",index=c("Kode","Tahun"))
vif(pool45)
pool48=plm(Close_Price~EPS+BV,data,model="pooling",index=c("Kode","Tahun"))
vif(pool48)
pool49=plm(Close_Price~EPS+PER,data,model="pooling",index=c("Kode","Tahun"))
vif(pool49)


#multi gaada semua

##Uji Homoskedastisitas
bptest(model29) 
bptest(model44) 
bptest(model45) 
bptest(model48) 
bptest(model49) 
bptest(model56) 
bptest(model57) 
bptest(model58) 
bptest(model59)


##Heteroscedasticity robust covariance estimator
coeftest(model29,vcovHC)
coeftest(model44,vcovHC)
coeftest(model45,vcovHC)
coeftest(model48,vcovHC)
coeftest(model49,vcovHC)
coeftest(model56,vcovHC)
coeftest(model57,vcovHC)
coeftest(model58,vcovHC)
coeftest(model59,vcovHC)

##Normalitas residual (ga normal semua)
hist(residuals(m2))
ks.test(residuals(model29),'pnorm')
ks.test(residuals(model44),'pnorm')
ks.test(residuals(model45),'pnorm')
ks.test(residuals(model48),'pnorm')
ks.test(residuals(model49),'pnorm')
ks.test(residuals(model56),'pnorm')
ks.test(residuals(model57),'pnorm')
ks.test(residuals(model58),'pnorm')
ks.test(residuals(model59),'pnorm')

#model terbaik
#model 29
model29=plm(Close_Price~EPS+PER+BV,data,model="random",effect="individual",index=c("Kode","Tahun"))
summary(model29)
ranef(model29, type='level')

#estimasi dg model terbaik
predict(model29)
resid(model29)
