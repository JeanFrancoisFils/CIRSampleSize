
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
ProbaSuccess01 <<- 0.069413589523873700000000
ProbaSuccess02 <<- 0.100134387538875000000000
ProbaSuccess03 <<- 0.142371366915669000000000
ProbaSuccess04 <<- 0.198494208657065000000000
ProbaSuccess05 <<- 0.269781334288632000000000
ProbaSuccess06 <<- 0.355320688175747000000000
ProbaSuccess07 <<- 0.451222558817411000000000
ProbaSuccess08 <<- 0.550890104117415000000000
ProbaSuccess09 <<- 0.646632086718873000000000
ProbaSuccess10 <<- 0.731896836683908000000000
ProbaSuccess11 <<- 0.802860238997772000000000
ProbaSuccess12 <<- 0.858667648973467000000000
ProbaSuccess13 <<- 0.900632100001484000000000
ProbaSuccess14 <<- 0.931135738720591000000000
ProbaSuccess15 <<- 0.952766556476660000000000
ProbaSuccess16 <<- 0.967837646382029000000000
ProbaSuccess17 <<- 0.978209882857995000000000
ProbaSuccess18 <<- 0.985287959837913000000000
ProbaSuccess19 <<- 0.990090159210219000000000
ProbaSuccess20 <<- 0.993335461512929000000000
ProbaSuccess21 <<- 0.995522789084672000000000
ProbaSuccess22 <<- 0.996994399366337000000000
ProbaSuccess23 <<- 0.997983287312003000000000
ProbaSuccess24 <<- 0.998647257660726000000000
ProbaSuccess25 <<- 0.999092825118269000000000
ProbaSuccess26 <<- 1.000
}
if (DRC == 2) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.100134387543879000000000
ProbaSuccess02 <<- 0.198494208670390000000000
ProbaSuccess03 <<- 0.355320688189556000000000
ProbaSuccess04 <<- 0.550890104110419000000000
ProbaSuccess05 <<- 0.731896836652309000000000
ProbaSuccess06 <<- 0.858667648937194000000000
ProbaSuccess07 <<- 0.931135738693714000000000
ProbaSuccess08 <<- 0.967837646365848000000000
ProbaSuccess09 <<- 0.985287959829120000000000
ProbaSuccess10 <<- 0.993335461508393000000000
ProbaSuccess11 <<- 0.996994399364061000000000
ProbaSuccess12 <<- 0.998647257659604000000000
ProbaSuccess13 <<- 0.999391720568375000000000
ProbaSuccess14 <<- 0.999726590851459000000000
ProbaSuccess15 <<- 0.999877130851514000000000
ProbaSuccess16 <<- 0.999944787597093000000000
ProbaSuccess17 <<- 0.999975190713898000000000
ProbaSuccess18 <<- 0.999988852316879000000000
ProbaSuccess19 <<- 0.999994990992342000000000
ProbaSuccess20 <<- 0.999997749301570000000000
ProbaSuccess21 <<- 0.999998988694753000000000
ProbaSuccess22 <<- 0.999999545591008000000000
ProbaSuccess23 <<- 0.999999795820827000000000
ProbaSuccess24 <<- 0.999999908256373000000000
ProbaSuccess25 <<- 0.999999958776929000000000
ProbaSuccess26 <<- 1.000
}
if (DRC == 3) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.030412152497410000000000
ProbaSuccess02 <<- 0.040154893911030300000000
ProbaSuccess03 <<- 0.052848666665879000000000
ProbaSuccess04 <<- 0.069265623307182700000000
ProbaSuccess05 <<- 0.090296173261796700000000
ProbaSuccess06 <<- 0.116909990987889000000000
ProbaSuccess07 <<- 0.150073895554725000000000
ProbaSuccess08 <<- 0.190614786030236000000000
ProbaSuccess09 <<- 0.239027393142539000000000
ProbaSuccess10 <<- 0.295250556599330000000000
ProbaSuccess11 <<- 0.358468660167419000000000
ProbaSuccess12 <<- 0.427021067162468000000000
ProbaSuccess13 <<- 0.498496404532546000000000
ProbaSuccess14 <<- 0.570033246970694000000000
ProbaSuccess15 <<- 0.638760406199939000000000
ProbaSuccess16 <<- 0.702240369459388000000000
ProbaSuccess17 <<- 0.758777785257746000000000
ProbaSuccess18 <<- 0.807522491488361000000000
ProbaSuccess19 <<- 0.848385343520102000000000
ProbaSuccess20 <<- 0.881842397008130000000000
ProbaSuccess21 <<- 0.908710866154158000000000
ProbaSuccess22 <<- 0.929954875892921000000000
ProbaSuccess23 <<- 0.946545974770618000000000
ProbaSuccess24 <<- 0.959378913254506000000000
ProbaSuccess25 <<- 0.969231140642852000000000
ProbaSuccess26 <<- 1.000
}
if (DRC == 4) {
ProbaSuccess00 <<- 0.000
ProbaSuccess01 <<- 0.000009731857926777870000
ProbaSuccess02 <<- 0.000025416362045365500000
ProbaSuccess03 <<- 0.000066377369587147200000
ProbaSuccess04 <<- 0.000173339691492049000000
ProbaSuccess05 <<- 0.000452586031659998000000
ProbaSuccess06 <<- 0.001181160489986830000000
ProbaSuccess07 <<- 0.003078983612395220000000
ProbaSuccess08 <<- 0.008001695043237720000000
ProbaSuccess09 <<- 0.020632004004786600000000
ProbaSuccess10 <<- 0.052150597197109700000000
ProbaSuccess11 <<- 0.125641704559183000000000
ProbaSuccess12 <<- 0.272881465956584000000000
ProbaSuccess13 <<- 0.494988167865928000000000
ProbaSuccess14 <<- 0.719090769426688000000000
ProbaSuccess15 <<- 0.869886998259170000000000
ProbaSuccess16 <<- 0.945831455060450000000000
ProbaSuccess17 <<- 0.978542038350261000000000
ProbaSuccess18 <<- 0.991673675700659000000000
ProbaSuccess19 <<- 0.996795456677881000000000
ProbaSuccess20 <<- 0.998770576819754000000000
ProbaSuccess21 <<- 0.999528907081045000000000
ProbaSuccess22 <<- 0.999819570140477000000000
ProbaSuccess23 <<- 0.999930907273605000000000
ProbaSuccess24 <<- 0.999973543864386000000000
ProbaSuccess25 <<- 0.999989870009019000000000
ProbaSuccess26 <<- 1.000
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

if (dosenumber == 22) {
SuccessProba <<- ProbaSuccess22
#print(paste("SuccessProba equals: ", SuccessProba ))
}

if (dosenumber == 23) {
SuccessProba <<- ProbaSuccess23
#print(paste("SuccessProba equals: ", SuccessProba ))
}

if (dosenumber == 24) {
SuccessProba <<- ProbaSuccess24
#print(paste("SuccessProba equals: ", SuccessProba ))
}

if (dosenumber == 25) {
SuccessProba <<- ProbaSuccess25
#print(paste("SuccessProba equals: ", SuccessProba ))
}

if (dosenumber == 26) {
SuccessProba <<- ProbaSuccess26
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