# find_default_tankless.R
# Jim Lutz "Sat Aug  5 21:38:29 2017"
# merges AHRI directory data with parsed EnergyGuide label information
# calculates Eannual_e from EnergyGuide information.
# for EnergyGuide information extracted by ../EnergyGuide labels/energyguide.pl followed by some hand cleaning
# AHRI data exported from https://www.ahridirectory.org/ahridirectory/pages/rwh/defaultSearch.aspx


# make sure all packages loaded and start logging
source("setup.R")

# set the working directory names 
source("setup_wd.R")

# get a list of all the XLS files in the wd_data subdirectories
xls_files <- list.files(path = wd_data, pattern = ".+.XLS$", 
                        full.names = TRUE, recursive = TRUE)
str(xls_files)
# 328 files

# split path and file name 
path <- dirname(xls_files)
filename <- basename(xls_files)

head (path)
length(path)
head(filename)
length(filename)

# get the file info
filedate <- file.mtime(xls_files)
filesize <- file.size(xls_files)

# make the data.table
DT_fn <- data.table(filename,filedate,filesize,path)

# drop '/home/jiml/HotWaterResearch/projects/Pipe Test Data' from path names
DT_fn[,path:= str_replace(path, "/home/jiml/HotWaterResearch/projects/Pipe Test Data", ".")]

# check for duplicate filenames
DT_fn[duplicated(filename),list(filename)]
# a dozen files with duplicate file names

# make data.table of duplicate file names
setkeyv(DT_fn,c("filename","path"))
DT_dupfns <- DT_fn[duplicated(filename),list(filename)]
DT_fn[DT_dupfns]
# only the June 14 ones are really duplicates?
write.csv(DT_fn[DT_dupfns], file="duplicates.csv", row.names = FALSE)


# parse file names into fields
# only parse filenames that begin with DD_
# misses first 14 data files
# drop .XLS
DT_fn[str_detect(filename, "^[0-9][0-9]_"), remaining:=str_replace(filename, ".XLS", "")]
DT_fn[is.na(remaining),list(filename)] 

# nominal pipe diameter
DT_fn[!is.na(remaining), nom_diam:=str_extract(remaining, "^(..)")] # get the nominal diameter
DT_fn[!is.na(nom_diam), remaining := str_replace(remaining, "^(..)_", "")] # remove the nominal diameter
unique(DT_fn$nom_diam)
DT_fn[nom_diam=="14", nom_diam:="1/4"]
DT_fn[nom_diam=="38", nom_diam:="3/8"]
DT_fn[nom_diam=="12", nom_diam:="1/2"]
DT_fn[nom_diam=="34", nom_diam:="3/4"]
unique(DT_fn$nom_diam)
# [1] "1/2" "3/4" NA    "1/4" "3/8"
DT_fn[,list(n=length(filename)),by=nom_diam]
#    nom_diam   n
# 1:      1/2 167
# 2:      1/4  15
# 3:      3/4  94
# 4:      3/8  36
# 5:       NA  16

# T? test?
DT_fn[!is.na(remaining),]
DT_fn[!is.na(remaining), Test:=str_match(remaining, "_(T.*)$")[,2] ] # get the Test
DT_fn[!is.na(Test), remaining := str_replace(remaining, "_(T.*)$", "")] # remove the nominal diameter
unique(DT_fn$Test)
# [1] "T1"   "T2"   NA     "TBAD" "T3"   "T1Z"  "T2Z" 
DT_fn[,list(n=length(filename)),by=Test]
#    Test   n
# 1:   T1 168
# 2:   T2 130
# 3:   T3   1
# 4:  T1Z   5
# 5:  T2Z   5
# 6:   NA  18
# 7: TBAD   1


# rest of stuff between nom_diam & Test
sort(unique(DT_fn$remaining))

# material
# PEX
DT_fn[str_detect(remaining, "^PEX"), material:="PEX"]
DT_fn[material=="PEX",remaining := str_replace(remaining, "PEX", "")]
# CPVC
DT_fn[str_detect(remaining, "^CPVC"), material:="CPVC"]
DT_fn[material=="CPVC",remaining := str_replace(remaining, "CPVC", "")]
# CU
DT_fn[str_detect(remaining, "^CU"), material:="CU"]
DT_fn[material=="CU",remaining := str_replace(remaining, "CU", "")]
sort(unique(DT_fn$material))
# [1] "CPVC" "CU"   "PEX" 
DT_fn[,list(n=length(filename)),by=material]
#    material   n
# 1:     CPVC  50
# 2:       CU  93
# 3:      PEX 169
# 4:       NA  16

# V velocity?
DT_fn[str_detect(remaining, "_V[0-9]*$"), velocity:=str_match(remaining, "_V([0-9]*)$")[,2] ]
DT_fn[!is.na(velocity),remaining := str_replace(remaining, "_V[0-9]*", "")]
DT_fn[!is.na(velocity),]
DT_fn[,list(n=length(filename)),by=velocity]
#    velocity   n
# 1:       NA 157
# 2:       12  20
# 3:        2  35
# 4:        5  39
# 5:        8  35
# 6:        4  38
# 7:        1   2
# 8:       77   2

# G GPM?
DT_fn[str_detect(remaining, "_[0-9]*G$"), GPM:=str_match(remaining, "_([0-9]*)G$")[,2] ]
DT_fn[!is.na(GPM),remaining := str_replace(remaining, "_[0-9]*G", "")]
DT_fn[!is.na(GPM),]
DT_fn[,list(n=length(filename)),by=GPM]
#    GPM   n
# 1:  15  18
# 2:   1  29
# 3:  25  18
# 4:   2  24
# 5:   4  18
# 6:  NA 201
# 7: 115   9
# 8: 230   3
# 9: 220   3
# 10: 441   4
# 11: 880   1

str(DT_fn)

write.csv(DT_fn[,list(n=length(filename)),by=remaining], file="list_of_remaining.csv", row.names = FALSE)


names(DT_fn)
setcolorder(DT_fn, 
            c("filename", "filedate", "filesize",
              "material","nom_diam", "velocity", "GPM", "Test", 
              "remaining","path" )
)

write.csv(DT_fn, file="list_of_XLS.csv", row.names = FALSE)

# get the first & 2nd header lines of all the .XLS files 
# libreoffice command and args for converting xls files to csv
syscommand <- "/opt/libreoffice5.3/program/soffice.bin"
sysargs <- c("--convert-to",
              "csv","")
 
# initialize a data.table and a counter
DT_headers<-data.table()
n=0

# a loop to process all the xls_files
for( fn in xls_files) { 
   n = n+1
   
   # for testing only
   # fn = xls_files[n]
   
   print(paste0(n," ",fn)) 
   
  # put the filename in single quotes
  sysargs[3] <- paste0("'",fn,"'")
 
  # a system call to convert xls files to a csv file in the working directory
  system2(command = syscommand, args = sysargs)

  # split path and file name
  path <- dirname(fn)
  filename <- basename(fn)

  # make the csv file name
  csvfn <- str_replace(filename, ".XLS", ".csv")

  # looks like the file structure is not as clean as hoped.
  # read in the first 2 lines of the csv file as a data.table
  # read first 2 lines as character strings
  hdrs <- read.csv(csvfn, header = FALSE, nrows = 2, sep = "`" )
  hdrs[] <- lapply(hdrs, as.character)
  str(hdrs)
  hdrs$V1[1]
  hdrs$V1[2]
  
  # make a data.table with filename, hdr1, hdr2, and path as fields
  DT_csv <- data.table(filename, hdr1=hdrs$V1[1],  hdr2=hdrs$V1[2], path=path)

  DT_headers <- rbind(DT_headers, DT_csv)

  # remove the DT_csv file
  if (file.exists(csvfn)) file.remove(csvfn)

}

# check for number of unique hdr1 & hdr2
unique(DT_headers$hdr1)
unique(DT_headers$hdr2)
# they're all the same.

# write the header file
write.csv(DT_headers, file="headers.csv", row.names = FALSE)


# 
#   
# #======
# 
# 
# 
# load(file=paste0(wd_data,"DT.Rdata"))
# 
# tables()
# 
# # check reading correct Dhw and DhwBu
# names(DT_HPWH)
# DT_HPWH[, list(HPWH.ElecDhw=sum(HPWH.ElecDhw), HPWH.ElecDhwBU=sum(HPWH.ElecDhwBU))]
# names(DT_HPWH01)
# DT_HPWH01[, list(Dhw=sum(Dhw), DhwBU=sum(DhwBU))]
# 
# names(DT_ER)
# DT_ER[, list(ER.ElecDhw=sum(ER.ElecDhw), ER.ElecDhwBU=sum(ER.ElecDhwBU))]
# names(DT_ER01)
# DT_ER01[, list(Dhw=sum(Dhw), DhwBU=sum(DhwBU))]
# 
# # keep DT_HPWH & DT_ER data.tables
# l_tables <- tables()$NAME
# rm(list = l_tables[grepl("*0[1-5]", l_tables)])
# 
# # merge table of electricity use & hot water for HPWH and ER
# DT_EHW <-merge(DT_ER[,list(HoY,JDay,Mon,Day,Hr,sDOWH,nPeople,WH.Total=ER.WH.Total,ER.ElecDhw,ER.ElecDhwBU)],
#                DT_HPWH[,list(HoY,HPWH.ElecDhw, HPWH.ElecDhwBU)], 
#                by = "HoY")
# 
# # assume ElecTot and ElecDhwBU are same units, and are separate
# DT_EHW[ , ER.Elec:= ER.ElecDhw + ER.ElecDhwBU]
# DT_EHW[ , HPWH.Elec:= HPWH.ElecDhw + HPWH.ElecDhwBU]
# 
# # rearrange data for boxplots of hourly _use
# DT_EHW[ , list(HoY,Hr, GPH=WH.Total, ER=ER.Elec, HPWH=HPWH.Elec)]
# 
# # melt into long format
# DT_mEHW <- melt(DT_EHW[ , list(HoY,Hr, GPH=WH.Total, ER=ER.Elec, HPWH=HPWH.Elec)], id.vars = c("HoY","Hr"))
# DT_mEHW[, list(variable=unique(variable))]
# #    variable
# # 1:      GPH
# # 2:       ER
# # 3:     HPWH
# 
# # change names
# setnames(DT_mEHW, c("value", "variable"), c("hourly.use", "type.use"))
# 
# # boxplots of hourly use by type of use and hour of day
# p <- ggplot(data = DT_mEHW )
# p <- p + geom_boxplot( aes(y = hourly.use, x = as.factor(Hr),
#                            fill = factor(type.use), 
#                            color = factor(type.use),
#                            dodge = type.use),
#                        position = position_dodge(width = .7),
#                        varwidth = TRUE)
# p <- p + scale_fill_manual(values=c("#DDDDFF", "#FFDDDD", "#DDFFDD"),name="use")
# p <- p + scale_color_manual(values=c("#0000FF", "#FF0000", "#00FF00"),name="use")
# p <- p + ggtitle("Hot Water and Electricity Use") + labs(x = "hour", y = "Hourly Use")
# p <- p + scale_x_discrete(breaks=1:24,labels=1:24)
# p
# 
# ggsave(filename = paste0(wd_charts,"/Use_by_hour.png"), plot = p)
# 
# # now plot one day, high and low
# DT_EHW[, list(GPD=sum(WH.Total)), by = JDay][order(GPD)]
# 
# # heavy day, JDay==138
# DT_EHW[JDay==138,]
# DT_heavy <- DT_EHW[JDay==138,list(Hr,GPH=WH.Total,ER=ER.Elec,HPWH=HPWH.Elec)]
# str(DT_heavy)
# p <- ggplot(data = DT_heavy )
# p <- p + geom_line(aes(x=Hr, y=GPH), color="blue", size=2)
# p <- p + geom_line(aes(x=Hr,y=ER), color="red")
# p <- p + geom_line(aes(x=Hr,y=HPWH), color="green")
# p <- p + ggtitle("Hot Water and Electricity Use (Mon, 5/18, 6 people)") + labs(x = "hour", y = "Hourly Use")
# p <- p + scale_x_continuous(breaks=1:24,labels=1:24)
# p
# 
# ggsave(filename = paste0(wd_charts,"/HW_elec_heavy.png"), plot = p)
# 
# # light day, JDay==15
# DT_EHW[JDay==15,]
# DT_light <- DT_EHW[JDay==15,list(Hr,GPH=WH.Total,ER=ER.Elec,HPWH=HPWH.Elec)]
# str(DT_light)
# p <- ggplot(data = DT_light )
# p <- p + geom_line(aes(x=Hr, y=GPH), color="blue", size=2)
# p <- p + geom_line(aes(x=Hr,y=ER), color="red")
# p <- p + geom_line(aes(x=Hr,y=HPWH), color="green")
# p <- p + ggtitle("Hot Water and Electricity Use (Thurs, 1/15, 2 people)") + labs(x = "hour", y = "Hourly Use")
# p <- p + scale_x_continuous(breaks=1:24,labels=1:24)
# p
# 
# ggsave(filename = paste0(wd_charts,"/HW_elec_light.png"), plot = p)
