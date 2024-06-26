## Run analysis, write model results

## Run analysis, write model results
#this will run the ASAP analysis, it will create a lot of files in the model folder, it then also runs a five year retrospective.
#plots can be created, however, ASAP starts with age 1 rather than age 0, so the plots are all offset by one age. The outputs file creates the outputs by adjusting for the offset.
## Before:
## After:

library(icesTAF)
taf.library(ASAPplots)
library(dplyr)
library(Hmisc)
library(icesAdvice)

mkdir("model")

# copy model executable
cp(taf.boot.path("software", "ASAP3.EXE"), "model")
cp(taf.boot.path("software", "ASAPRETRO.EXE"), "model")

# copy data file
cp("data/ORIGINAL.DAT","model")

# this is the folder I have to run ASAP from, after I copied the .exe files there.
setwd("model/")

# run the asap
shell(paste("ASAP3.exe -ind", "ORIGINAL.DAT"), intern = TRUE)

# re-name the files for ease of use
exts <- c(".rep", ".rdat", ".bar", ".cor", ".par", ".std")
for (ext in exts) {
  file.copy(from = paste0("asap3", ext), to = paste0("ORIGINAL", ext))
}


# do the retrospective
cp("ORIGINAL.DAT", "RETRO_ORIGINAL.DAT")
newf <- ASAPplots::ReadASAP3DatFile("RETRO_ORIGINAL.dat") # read in the file
newfname <- "RETRO_ORIGINAL.dat"

# rerun the fit
shell(paste("ASAP3.exe -ind", "RETRO_ORIGINAL.dat"), intern = TRUE) # run the asap

retrofname <- paste0("RETRO_ORIGINAL", "_retro.dat")
# file.copy(from = datfname, to = retrofname)
npeels <- 5
terminal.year <- 2023
retro.first.year <- terminal.year - npeels
shell(paste("ASAPRETRO.exe", "RETRO_ORIGINAL.dat", retro.first.year), intern = TRUE)

# finally reset the working directory to the project root
setwd("..")
