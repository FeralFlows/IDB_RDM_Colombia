#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source(paste0(args[4], proj_file_function.R'))
library('rgcam')
options("rgcam.saved_compressed" = FALSE)
base_dir <- args[5]
output_file <- c('QueryResults.proj')
produce_query_file(base_dir, output_file, proc_num=args[2], plotting_format=args[3])
quit(save = "no")  # prevents pic from trying to save and reload workspaces, which sucks up time and memory

# Extra text saved in case needed later:
#base_dir <- c('/qfs/people/wild566/IDB/Final/gcam-core/output/FinalRuns/IDB_RDM/04052021/')