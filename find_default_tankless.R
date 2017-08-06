# find_default_tankless.R
# Jim Lutz "Sat Aug  5 21:38:29 2017"
# merges AHRI directory data with parsed EnergyGuide label information
# calculates Eannual_e from EnergyGuide information.
# for EnergyGuide information extracted by ../EnergyGuide labels/energyguide.pl followed by some hand cleaning
# AHRI data from ../AHRIExport.csv

# make sure all packages loaded and start logging
source("setup.R")

# read the AHRI data
DT_AHRI <- fread("/home/jiml/HotWaterResearch/projects/CECHWT24/2016 CBECC UEF/default tankless/AHRIExport.csv")
str(DT_AHRI)
names(DT_AHRI)

# convert fields to numeric as needed
DT_AHRI[,`:=` (UEF            = as.numeric(UEF),
               Volume         = as.numeric(Volume),
               Input          = as.numeric(Input),
               RecoveryEff    = as.numeric(RecoveryEff)/100,
               MaxGPM         = as.numeric(MaxGPM),
               MeasuredVolume = as.numeric(MeasuredVolume)
              )
        ]

# change ModelNumber to model
setnames(DT_AHRI,"ModelNumber", "model")
setkey(DT_AHRI,model)

# read in the EnergyGuide label data
DT_EnergyGuide <- fread("/home/jiml/HotWaterResearch/projects/CECHWT24/2016 CBECC UEF/default tankless/EnergyGuide labels/energyguide.fix.csv")
str(DT_EnergyGuide)
setkey(DT_EnergyGuide,model)
tables()

# try to merge the data in DT_EnergyGuide to  DT_AHRI
DT_TWH <-merge(DT_AHRI, DT_EnergyGuide, all=TRUE)
str(DT_TWH)

# see how many merged
DT_TWH[, list(OEMName,mfr)]
DT_TWH[!is.na(OEMName) & !is.na(mfr), list(OEMName,mfr)] # 123 look like they match
DT_TWH[OEMName==mfr,list(model)] 

# look at the ones that didn't match
DT_TWH[is.na(OEMName) | is.na(mfr), list(OEMName,mfr, model)][order(model)]
# fixed some by hand

# calculate Qin from Qout_UEF / UEF
Qout.UEF = 45798 # BTU/day from high UEF draw procedure
DT_TWH[, Qin.UEF:= Qout.UEF / UEF]

names(DT_TWH)
DT_TWH[,list(n=length(model)),by=fuel]
  #           fuel  n
  # 1:          NA 15
  # 2: Natural Gas 67
  # 3: Propane Gas 56

DT_TWH[fuel=="Natural Gas" & UEF==0.81,list(n=length(model)),by=Qin.UEF]
  #     Qin.UEF  n
  # 1: 56540.74 30

# Calculate Qin.UEF_f 

DT_TWH[, Qin.UEF_f := Eannual_f / 365 * 100000]
DT_TWH[fuel=="Natural Gas" & UEF==0.81,list(n=length(model)),by=Qin.UEF_f]
  #    Qin.UEF_f  n
  # 1:  57260.27 30
# But Qin.UEF_f > Qin.UEF!!!

# calculate the electricity use
DT_TWH[,Eannual_e:= Eannual_e_cost / elec_cost]

# calculate P,e
# P,e = { Eannual,e * 1000 /365  -  S,e * (24 – Hon.UEF) } / Hon.UEF 
S_e = 4 # assumed standby wattage
Hon.UEF = 0.6542 # from High Usage UEF draw pattern
DT_TWH[,P_e:= ( (Eannual_e * 1000 /365  -  S_e * (24 - Hon.UEF) ) / Hon.UEF )]

# save to spreadsheet
fwrite(DT_TWH, file = "baselineTWH.csv", quote=TRUE)

# summarize 
names(DT_TWH)
DT_uniques <- function(varname) {
  # return unique values of varname from 
  # testing: varname = "Input"
  DT_uniques <- DT_TWH[fuel=="Propane Gas" & UEF==0.81,list(n=length(model)),by=varname]
  setorderv(DT_uniques,varname)
  return(DT_uniques)
}

DT_uniques("Input")

# look at unique values
for(var in c('Input',"RecoveryEff","MaxGPM","Eannual_f","Eannual_f_cost","cost","Eannual_e_cost","Eannual_e","P_e")) {
  print(DT_uniques((var)))
}

# do some cross-tabs
table(DT_TWH[fuel=="Natural Gas" & UEF==0.81, list(Input,MaxGPM)])
table(DT_TWH[fuel=="Natural Gas" & UEF==0.81, list(Input,RecoveryEff)])


# quick look at Eannual_f vs Eannual_e
p1 <- ggplot(data = DT_TWH[fuel=="Natural Gas" & UEF==0.81],aes(x=Input, y=Eannual_e))
p1 <- p1 + geom_point() #position = "jitter"
p1 <- p1 + scale_x_continuous(name = "Eannual_f (therms)")
p1 <- p1 + scale_y_continuous(name = "Eannual_e (kWhs)")
p1 <- p1 + ggtitle("Instantaneous Water Heaters \n(UEF=0.81, fuel=Natural Gas)")
  
# distribution of Eannual_f
p2 <- ggplot(data = DT_TWH[fuel=="Natural Gas" & UEF==0.81],aes(x=Eannual_f))
p2 <- p2 + geom_histogram(binwidth=.05)

# turns out they've all got the same therms, slight variation of price
DT_baseline <- DT_TWH[fuel=="Natural Gas" & UEF==0.81,list(mfr,model,Eannual_f,cost,Eannual_f_cost, Eannual_e_cost,Eannual_e )][order(-cost)]

# save to spreadsheet
fwrite(DT_baseline, file = "baselineNG.81.csv", quote=TRUE)

