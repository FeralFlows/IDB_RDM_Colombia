#!/bin/bash

# ------------------------------------------------------------------------------
# README
#
# This file executes three separate post-processing steps. Each of those three
# steps has its own associated shell script that this meta-script launches. Each
# successive script can only be launched when the previous script is completed,
# so we use dependencies to ensure each step is completed before the next step can
# proceed.
#
# Author: Thomas B. Wild (thomas.wild@pnnl.gov)
#
# USAGE:
#
# sh launch_post_processing.sh
#
#------------------------------------------------------------------------------

module load git
module load svn/1.8.13
module load R/3.4.3
module load java/1.8.0_31
module load gcc/8.1.0

# User-specified location of output (.csv) files containing GCAM query results.
# This series of scripts is not designed to handle GCAM DBs, only .csv files.
output_path='/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/relationships/gcam/outputs/raw/05272021/'

echo 'Launching first post-processing script'
f1="PostProcess_1.sh"
fpath1="$1$f1"
jid1=$(sbatch --dependency=afterany$3 $fpath1 $2 $output_path | sed 's/Submitted batch job //')

echo 'Launching second post-processing script'
f2="PostProcess_2.sh"
fpath2="$1$f2"
jid2=$(sbatch --dependency=afterany:$jid1 $fpath2 $2 $output_path | sed 's/Submitted batch job //')

echo 'Launhcing third post-processing script'
f3="PostProcess_3.sh"
fpath3="$1$f3"
jid3=$(sbatch --dependency=afterany:$jid2 $fpath3 $2 $output_path)
