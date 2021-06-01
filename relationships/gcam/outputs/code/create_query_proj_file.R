#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library('rgcam')

source(paste0(args[4], 'proj_file_function.R'))
options("rgcam.saved_compressed" = FALSE)
post_proc_outpath <- args[5]
output_file <- c('QueryResults.proj')
produce_query_file(post_proc_outpath, output_file, proc_num=args[2], plotting_format=args[3])
quit(save = "no")  # prevents pic from trying to save and reload workspaces, which sucks up time and memory