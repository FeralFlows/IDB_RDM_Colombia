#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source(paste0(args[3], proj_file_function.R'))
base_dir <- args[4]
output_file <- c('QueryResults.proj')
produce_query_file(base_dir, output_file, metis_format=args[2])