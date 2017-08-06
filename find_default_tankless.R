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

# calculate Eannual_e from cost
natl_gas_cost = 1.09 # $/therm
propane_cost  = 2.14 # $/gallon
elec_cost     = 0.12 # $/kWh

names(DT_TWH)
DT_TWH[,list(n=length(model)),by=fuel]
  #           fuel  n
  # 1:          NA 15
  # 2: Natural Gas 67
  # 3: Propane Gas 56

# calculate the fuel costs
DT_TWH[fuel=="Natural Gas", Eannual_f_cost:=Eannual_f*natl_gas_cost]
DT_TWH[fuel=="Propane Gas", Eannual_f_cost:=Eannual_f*propane_cost]

# calculate the electricy cost
DT_TWH[,Eannual_e_cost:=cost-Eannual_f_cost]

# calculate the electricity use
DT_TWH[,Eannual_e:= Eannual_e_cost / elec_cost]

# save to spreadsheet
fwrite(DT_TWH, file = "baselineTWH.csv", quote=TRUE)

# summarize 
names(DT_TWH)
DT_uniques <- function(varname) {
  # return unique values of varname from 
  # testing: varname = "Input"
  DT_uniques <- DT_TWH[fuel=="Natural Gas" & UEF==0.81,list(n=length(model)),by=varname]
  setorderv(DT_uniques,varname)
  return(DT_uniques)
}

DT_uniques("Input")

# look at unique values
for(var in c('Input',"RecoveryEff","MaxGPM","Eannual_f","Eannual_f_cost","cost","Eannual_e_cost","Eannual_e")) {
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

