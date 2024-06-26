## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")
## Preprocess data, write TAF data tables

## Before:all data are in the Lowestoft format
## After:

library(icesTAF)
library(FLCore)
library(FLAssess)

mkdir("data")

# read in data files from bootstrap data folder
landings <- readVPAFile(taf.data.path("had7ALA.txt"))
landings.n <- readVPAFile(taf.data.path("had7ALN.txt"))
discards <- readVPAFile(taf.data.path("had7ADI.txt"))
discards.n <- readVPAFile(taf.data.path("had7ADN.txt"))
landings.wt <- readVPAFile(taf.data.path("had7ACW.txt"))
discards.wt <- readVPAFile(taf.data.path("had7ACW.txt"))
catch.wt <- readVPAFile(taf.data.path("had7ACW.txt"))

# compute catch
catch.n <- discards.n + landings.n
catch <- discards + landings

# convert to cross tab format
landings <- flr2taf(landings)
landings.n <- flr2taf(landings.n)
discards <- flr2taf(discards)
discards.n <- flr2taf(discards.n)
landings.wt <- flr2taf(landings.wt)
discards.wt <- flr2taf(discards.wt)
catch.wt <- flr2taf(catch.wt)
catch.n <- flr2taf(catch.n)
catch <- flr2taf(catch)

# write out cross tab csv files
write.taf(landings, dir = "data")
write.taf(landings.n, dir = "data")
write.taf(discards, dir = "data")
write.taf(discards.n, dir = "data")
write.taf(landings.wt, dir = "data")
write.taf(discards.wt, dir = "data")
write.taf(catch.wt, dir = "data")
write.taf(catch.n, dir = "data")
write.taf(catch, dir = "data")

# copy asap input file to data dir
cp(taf.data.path("ORIGINAL.DAT"), "data")
