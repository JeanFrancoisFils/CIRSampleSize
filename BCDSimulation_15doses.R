
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
ProbaSuccess01 <<- 0.088744197350403600000000
ProbaSuccess02 <<- 0.159440230555994000000000
ProbaSuccess03 <<- 0.269781334288632000000000
ProbaSuccess04 <<- 0.418467992756332000000000
ProbaSuccess05 <<- 0.583607674229323000000000
ProbaSuccess06 <<- 0.731896836683908000000000
ProbaSuccess07 <<- 0.841700360234924000000000
ProbaSuccess08 <<- 0.911943634676140000000000
ProbaSuccess09 <<- 0.952766556476661000000000
ProbaSuccess10 <<- 0.975179082734500000000000
ProbaSuccess11 <<- 0.987100726043505000000000
ProbaSuccess12 <<- 0.993335461512929000000000
ProbaSuccess13 <<- 0.996567179728037000000000
ProbaSuccess14 <<- 0.998234582437882000000000
ProbaSuccess15 <<- 0.999092825118269000000000
ProbaSuccess16 <<- 1.00
}

if (DRC == 2) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.159440230566559000000000
ProbaSuccess02 <<- 0.418467992765517000000000
ProbaSuccess03 <<- 0.731896836652309000000000
ProbaSuccess04 <<- 0.911943634645511000000000
ProbaSuccess05 <<- 0.975179082721188000000000
ProbaSuccess06 <<- 0.993335461508393000000000
ProbaSuccess07 <<- 0.998234582436459000000000
ProbaSuccess08 <<- 0.999534035201152000000000
ProbaSuccess09 <<- 0.999877130851514000000000
ProbaSuccess10 <<- 0.999967609113332000000000
ProbaSuccess11 <<- 0.999991461651311000000000
ProbaSuccess12 <<- 0.999997749301570000000000
ProbaSuccess13 <<- 0.999999406721352000000000
ProbaSuccess14 <<- 0.999999843613378000000000
ProbaSuccess15 <<- 0.999999958776929000000000
ProbaSuccess16 <<- 1.00
}

if (DRC == 3) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.036613840611106500000000
ProbaSuccess02 <<- 0.057865537817522400000000
ProbaSuccess03 <<- 0.090296173261796700000000
ProbaSuccess04 <<- 0.138235652374971000000000
ProbaSuccess05 <<- 0.205867102074563000000000
ProbaSuccess06 <<- 0.295250556599330000000000
ProbaSuccess07 <<- 0.403713449040069000000000
ProbaSuccess08 <<- 0.522481331867387000000000
ProbaSuccess09 <<- 0.638760406199941000000000
ProbaSuccess10 <<- 0.740772978893806000000000
ProbaSuccess11 <<- 0.822005143706979000000000
ProbaSuccess12 <<- 0.881842397008131000000000
ProbaSuccess13 <<- 0.923437519842887000000000
ProbaSuccess14 <<- 0.951200184163324000000000
ProbaSuccess15 <<- 0.969231140642852000000000
ProbaSuccess16 <<- 1.00
}
if (DRC == 4) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.000018456170676886700000
ProbaSuccess02 <<- 0.000091407952263369200000
ProbaSuccess03 <<- 0.000452586031659999000000
ProbaSuccess04 <<- 0.002237684787785770000000
ProbaSuccess05 <<- 0.010986218250702000000000
ProbaSuccess06 <<- 0.052150597197109700000000
ProbaSuccess07 <<- 0.214156041301341000000000
ProbaSuccess08 <<- 0.574431108672197000000000
ProbaSuccess09 <<- 0.869886998259172000000000
ProbaSuccess10 <<- 0.970686820769872000000000
ProbaSuccess11 <<- 0.993940037885008000000000
ProbaSuccess12 <<- 0.998770576819754000000000
ProbaSuccess13 <<- 0.999751541606084000000000
ProbaSuccess14 <<- 0.999949827500835000000000
ProbaSuccess15 <<- 0.999989870009019000000000
ProbaSuccess16 <<- 1.00
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