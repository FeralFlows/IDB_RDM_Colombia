#!/usr/bin/env Rscript

LIB='/cluster/tufts/hpc/tools/R/4.0.0'
.libPaths(c("",LIB))

args = commandArgs(trailingOnly=TRUE)
library('rgcam')
raw_outpath <- args[2]
post_proc_outpath <- args[3]
files <- list.files(raw_outpath, pattern = "*.csv", full.names=TRUE)
options("rgcam.saved_compressed" = TRUE)  # do not compress single output .proj file if using /pic/projects/GCAM dir
proj <- mergeProjects(paste0(post_proc_outpath, "QueryResults.proj"), lapply(files, addMIBatchCSV, "temp", saveProj = FALSE))
quit(save = "no")  # prevents pic from trying to save and reload workspaces, which consumes substantial wall clock time and memory