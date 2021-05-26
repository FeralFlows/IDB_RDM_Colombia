#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source('/qfs/people/wild566/IDB/Final/gcam-core/exe/proj_file_function.R')
base_dir <- c('/qfs/people/wild566/IDB/Final/gcam-core/output/FinalRuns/IDB_RDM/04052021/')
output_file <- c('QueryResults.proj')
produce_query_file(base_dir, output_file, metis_format=args[2])