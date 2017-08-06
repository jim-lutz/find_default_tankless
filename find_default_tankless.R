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
DT_TWH[!is.na(OEMName) & !is.na(mfr), list(OEMName,mfr)] # 121 look like they match
DT_TWH[OEMName==mfr,list(model)] 

# look at the ones that didn't match
DT_TWH[is.na(OEMName) | is.na(mfr), list(OEMName,mfr, model)][order(model)]
