#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

LIB='/cluster/tufts/hpc/tools/R/4.0.0'
.libPaths(c("",LIB))

source(paste0(args[3], 'proj_file_function.R'))
post_proc_outpath <- args[4]
output_file <- c('QueryResults.proj')
produce_query_file(post_proc_outpath, output_file, metis_format=args[2])