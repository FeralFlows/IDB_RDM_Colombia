#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source('/qfs/people/wild566/IDB/Final/gcam-core-4/gcam-core/exe/proj_file_function.R')
base_dir <- c('/qfs/people/wild566/IDB/Final/gcam-core-4/gcam-core/output/FinalRuns/IDB_RDM/05242020/')
output_file <- c('05282020.proj')
produce_query_file(base_dir, output_file, proc_num=args[2], plotting_format=args[3])