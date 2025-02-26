
library(cir)
library(plyr)


#Initialize Parameters
DoseNumber<-NULL
SuccessEventResult<-NULL

BCDSimulation <- function(StartingDose, EDOfInterest, SampleSizeStart, SampleSizeEnd, SampleSizeIncrease, DRC, NSim) {

DecreaseEDOfInterest<-((1-EDOfInterest)/EDOfInterest)

#Probability Definition
if (DRC == 1) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.076157905593471800000000
ProbaSuccess02 <<- 0.119651734630569000000000
ProbaSuccess03 <<- 0.183062902960602000000000
ProbaSuccess04 <<- 0.269781334288632000000000
ProbaSuccess05 <<- 0.378544123270194000000000
ProbaSuccess06 <<- 0.501066929992029000000000
ProbaSuccess07 <<- 0.623461737346000000000000
ProbaSuccess08 <<- 0.731896836683908000000000
ProbaSuccess09 <<- 0.818210129951438000000000
ProbaSuccess10 <<- 0.881244434167195000000000
ProbaSuccess11 <<- 0.924440462418750000000000
ProbaSuccess12 <<- 0.952766556476662000000000
ProbaSuccess13 <<- 0.970808955347317000000000
ProbaSuccess14 <<- 0.982089014774020000000000
ProbaSuccess15 <<- 0.989059334813575000000000
ProbaSuccess16 <<- 0.993335461512929000000000
ProbaSuccess17 <<- 0.995947125255623000000000
ProbaSuccess18 <<- 0.997537880910764000000000
ProbaSuccess19 <<- 0.998505201170739000000000
ProbaSuccess20 <<- 0.999092825118269000000000
ProbaSuccess21 <<- 1.000
}

if (DRC == 2) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.119651734637525000000000
ProbaSuccess02 <<- 0.269781334304379000000000
ProbaSuccess03 <<- 0.501066929991890000000000
ProbaSuccess04 <<- 0.731896836652308000000000
ProbaSuccess05 <<- 0.881244434132563000000000
ProbaSuccess06 <<- 0.952766556455436000000000
ProbaSuccess07 <<- 0.982089014763714000000000
ProbaSuccess08 <<- 0.993335461508393000000000
ProbaSuccess09 <<- 0.997537880908854000000000
ProbaSuccess10 <<- 0.999092825117483000000000
ProbaSuccess11 <<- 0.999666077525373000000000
ProbaSuccess12 <<- 0.999877130851514000000000
ProbaSuccess13 <<- 0.999954795455368000000000
ProbaSuccess14 <<- 0.999983369702177000000000
ProbaSuccess15 <<- 0.999993881991016000000000
ProbaSuccess16 <<- 0.999997749301570000000000
ProbaSuccess17 <<- 0.999999172013142000000000
ProbaSuccess18 <<- 0.999999695400498000000000
ProbaSuccess19 <<- 0.999999887944084000000000
ProbaSuccess20 <<- 0.999999958776929000000000
ProbaSuccess21 <<- 1.000
}
if (DRC == 3) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.032608556256128000000000
ProbaSuccess02 <<- 0.046087640007973300000000
ProbaSuccess03 <<- 0.064765424035639300000000
ProbaSuccess04 <<- 0.090296173261796600000000
ProbaSuccess05 <<- 0.124550904819119000000000
ProbaSuccess06 <<- 0.169380942410483000000000
ProbaSuccess07 <<- 0.226178012802509000000000
ProbaSuccess08 <<- 0.295250556599330000000000
ProbaSuccess09 <<- 0.375189657560879000000000
ProbaSuccess10 <<- 0.462566425675336000000000
ProbaSuccess11 <<- 0.552304571506671000000000
ProbaSuccess12 <<- 0.638760406199944000000000
ProbaSuccess13 <<- 0.717073155256738000000000
ProbaSuccess14 <<- 0.784145665821128000000000
ProbaSuccess15 <<- 0.838890036766691000000000
ProbaSuccess16 <<- 0.881842397008131000000000
ProbaSuccess17 <<- 0.914510508747460000000000
ProbaSuccess18 <<- 0.938773664745263000000000
ProbaSuccess19 <<- 0.956478302602620000000000
ProbaSuccess20 <<- 0.969231140642852000000000
ProbaSuccess21 <<- 1.000
}
if (DRC == 4) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.000012371595834475700000
ProbaSuccess02 <<- 0.000041074171093351100000
ProbaSuccess03 <<- 0.000136358737761747000000
ProbaSuccess04 <<- 0.000452586031659996000000
ProbaSuccess05 <<- 0.001501069842687780000000
ProbaSuccess06 <<- 0.004966455717904820000000
ProbaSuccess07 <<- 0.016301457034546800000000
ProbaSuccess08 <<- 0.052150597197109700000000
ProbaSuccess09 <<- 0.154458082015658000000000
ProbaSuccess10 <<- 0.377528918756429000000000
ProbaSuccess11 <<- 0.668177794921068000000000
ProbaSuccess12 <<- 0.869886998259177000000000
ProbaSuccess13 <<- 0.956891301277269000000000
ProbaSuccess14 <<- 0.986612685921806000000000
ProbaSuccess15 <<- 0.995929760939786000000000
ProbaSuccess16 <<- 0.998770576819754000000000
ProbaSuccess17 <<- 0.999629388302064000000000
ProbaSuccess18 <<- 0.999888345542906000000000
ProbaSuccess19 <<- 0.999966367867798000000000
ProbaSuccess20 <<- 0.999989870009019000000000
ProbaSuccess21 <<- 1.000
}

jend <<-((SampleSizeEnd-SampleSizeStart)/SampleSizeIncrease)+1






for (j in 1:jend) {
set.seed(3256) 
#Sample Size definition
SampleSize <<- SampleSizeStart+((j-1)*SampleSizeIncrease)
print("SampleSize")
print(SampleSize)




for (i in 1:NSim) {
#print("NSim")
#print(i)
for (n in 1:SampleSize) {
#print(n)
if (n == 1) {
dosenumber <<- StartingDose
}
if (dosenumber == 0) {
SuccessProba <<- ProbaSuccess00 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 1) {
SuccessProba <<- ProbaSuccess01 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 2) {
SuccessProba <<- ProbaSuccess02 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 3) {
SuccessProba <<- ProbaSuccess03 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 4) {
SuccessProba <<- ProbaSuccess04 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 5) {
SuccessProba <<- ProbaSuccess05 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 6) {
SuccessProba <<- ProbaSuccess06 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 7) {
SuccessProba <<- ProbaSuccess07 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 8) {
SuccessProba <<- ProbaSuccess08 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 9) {
SuccessProba <<- ProbaSuccess09 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 10) {
SuccessProba <<- ProbaSuccess10 
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 11) {
SuccessProba <<- ProbaSuccess11
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 12) {
SuccessProba <<- ProbaSuccess12
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 13) {
SuccessProba <<- ProbaSuccess13
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 14) {
SuccessProba <<- ProbaSuccess14
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 15) {
SuccessProba <<- ProbaSuccess15
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 16) {
SuccessProba <<- ProbaSuccess16
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 17) {
SuccessProba <<- ProbaSuccess17
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 18) {
SuccessProba <<- ProbaSuccess18
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 19) {
SuccessProba <<- ProbaSuccess19
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 20) {
SuccessProba <<- ProbaSuccess20
#print(paste("SuccessProba equals: ", SuccessProba ))
}
if (dosenumber == 21) {
SuccessProba <<- ProbaSuccess21
#print(paste("SuccessProba equals: ", SuccessProba ))
}



DoseNumber<<-c(DoseNumber,dosenumber)

#print(paste("subject number: ", n))
#print(paste("dose number: ", dosenumber))
#print(paste("SuccessProba: ", SuccessProba))
SuccessEvent <<- rbinom(1,1,SuccessProba)


if (SuccessEvent < 0.5) {
dosenumber <<- dosenumber +1
#print(paste("next dose number equals: ", dosenumber ))
}

if (SuccessEvent >= 0.5) {
dosenumber <<- dosenumber +0
DecreaseEvent <<- rbinom(1,1,DecreaseEDOfInterest)
  if (DecreaseEvent == 1) {
  dosenumber <<- dosenumber -1
  }

}

SuccessEventResult<<-c(SuccessEventResult,SuccessEvent)
dataBCD1<<-cbind(DoseNumber,SuccessEventResult)
dataBCD<<-as.data.frame(dataBCD1)
dataBCD$x<<-dataBCD$DoseNumber
dataBCD$y<<-dataBCD$SuccessEventResult
dataBCD2<<-dataBCD


if (n == SampleSize) {
DoseNumber<<-NULL
SuccessEventResult<<-NULL

#Centered Isotonic Regression
#BCDCIR<<-quickInverse(dataBCD,target=EDOfInterest, conf=0.95)
#BCDIR<<-quickInverse(dataBCD,target=EDOfInterest, conf=0.95, estfun = oldPAVA)

BCDCIR<<-doseFind(dataBCD,target=EDOfInterest, conf=0.95)
BCDIR<<-doseFind(dataBCD,target=EDOfInterest, conf=0.95, estfun = oldPAVA)

BCDTarget<<-EDOfInterest#BCDCIR$target
BCDEstimateCIR<<-BCDCIR#BCDCIR$point
#BCDlower95CICIR<<-BCDCIR$lower95conf
#BCDupper95CICIR<<-BCDCIR$upper95conf

BCDEstimateIR<<-BCDIR#BCDIR$point
#BCDlower95CIIR<<-BCDIR$lower95conf
#BCDupper95CIIR<<-BCDIR$upper95conf

BCDNbFailures<<-count(dataBCD, vars="SuccessEventResult")[1,2]
BCDMinDose<<-min(dataBCD$DoseNumber)
BCDMaxDose<<-max(dataBCD$DoseNumber)
BCDNbDoses<<-length(unique(dataBCD$DoseNumber))
BCDRangeDoses<<-(BCDMaxDose-BCDMinDose+2)
#EstimateCIRLTMax<<-BCDMaxDose-BCDEstimateCIR
#EstimateIRLTMax<<-BCDMaxDose-BCDEstimateIR

BCDAll1<<-cbind(BCDTarget, BCDEstimateCIR, #BCDlower95CICIR, BCDupper95CICIR,
BCDEstimateIR, #BCDlower95CIIR, BCDupper95CIIR, 
BCDNbFailures, BCDMinDose, BCDMaxDose, BCDNbDoses, BCDRangeDoses#, EstimateCIRLTMax, EstimateIRLTMax
)
BCDAll<<-as.data.frame(BCDAll1)

dataBCDall<<-cbind(dataBCD)
}
}
if (i == 1) {
BCDAlldef<<-BCDAll
BCDAlldef$Nsim<<-i
dataBCDalldef<<-dataBCDall
dataBCDalldef$Nsim <<-i
}
if (i >1) {
BCDAll$Nsim<<-i
dataBCDall$Nsim <<-i
BCDAlldef<<-rbind(BCDAlldef,BCDAll)
dataBCDalldef<<-rbind(dataBCDalldef,dataBCDall)

}
}



if (j == 1) {
BCDAllSSdef<<-BCDAlldef
BCDAllSSdef$SampleSize<<-SampleSize
}

if (j > 1) {
BCDAlldef$SampleSize<<-SampleSize
BCDAllSSdef<<-rbind(BCDAllSSdef,BCDAlldef)
}


}



} #End Function