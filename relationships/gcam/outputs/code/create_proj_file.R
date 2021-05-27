#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library('rgcam')
files <- list.files(args[2], pattern = "*.csv", full.names=TRUE)
options("rgcam.saved_compressed" = FALSE)
proj <- mergeProjects(paste0(args[2], "QueryResults.proj"), lapply(files, addMIBatchCSV, "temp", saveProj = FALSE))
quit(save = "no")  # prevents pic from trying to save and reload workspaces, which consumes substantial wall clock time and memory

# Extra code saved in case needed later:
#files <- list.files('/pic/projects/GCAM/TomWild/output/FinalRuns/IDB_RDM/04052021', pattern = "*.csv", full.names=TRUE)
#proj <- mergeProjects('/pic/projects/GCAM/TomWild/output/FinalRuns/IDB_RDM/04052021/QueryResults.proj', lapply(files, addMIBatchCSV, "temp", saveProj = FALSE))