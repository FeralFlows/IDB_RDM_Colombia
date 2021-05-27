#!/bin/bash

# ------------------------------------------------------------------------------
# README
#
# This file executes the following steps:
#
# 1. Executes a script (RDM_DDP_XL.sh) separately for each batch of runs. 
# Users must specify size of each batch, which cannot individually exceed
# 1000 runs. 
#
# 2. Executes a post-processing launch script (launch_post_processing.sh) 
# that executes a series of post-processing scripts.
#
# Author: Thomas B. Wild (thomas.wild@pnnl.gov)
#
# USAGE:
#
# sh launch_rdm_alljobs.sh
#
#------------------------------------------------------------------------------

# User to specify total number of GCAM runs to perform, and output dir
#total_jobs=4096
total_jobs=3
echo "total number of GCAM runs to perform: $total_jobs"

# User specify output dirs
output_dir='05272021'
output_path='/qfs/people/wild566/IDB/Final/gcam-core/output/FinalRuns/IDB_RDM/05272021'

# PIC has a max number of job arrays, which is currently 1000
#max_job_arrays=1000
max_job_arrays=2
# Calculate the number of groups of 1000, and any remainder to be dealt with
separate_groups=$((total_jobs/max_job_arrays))
echo "number of groups of 1000: $separate_groups"
remainder=$((total_jobs%max_job_arrays))
echo "size of additional group: $remainder"

# Declare arrays that will store run number for each batch job
declare -a job_array_upper
declare -a start_point

# Loop through and populate matrix that specifies number of runs in each job
# and the indexed staring point for each job (e.g., 2000, 3000, etc.)
count=0
while [ $count -le $separate_groups ]
do
    if [[ $count -ne $separate_groups ]]; then
        job_array_upper[$count]=$max_job_arrays
    else
		if [[ $remainder -gt 0 ]]; then
			job_array_upper[$count]=$remainder
		fi
    fi
    start_point[$count]=$((count*max_job_arrays+1))
    count=$((count + 1))
done

# launch individual batches
count=0
# create associative array so each job is a variable and we can track if it's finished
declare -A jid_a=()
for arr in ${job_array_upper[@]}; do
    arr_lower="1-"
    arr_upper="$arr"
    arr_string="$arr_lower$arr_upper"
    echo "sbatch --array=$arr_string RDM_DDP_XL.sh ${start_point[count]}"
    jid_a[count]=$(sbatch --array=$arr_string RDM_DDP_XL.sh ${start_point[count]} $output_dir $output_path)
    # sbatch --array=$arr_string RDM_DDP_XL.sh ${start_point[count]} $output_dir $output_path
	count=$((count+1))
done

# launch post processing
PostProcDir=/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/relationships/gcam/scripts/
PostProcFile="launch_post_processing.sh"
PostProcFpath="$PostProcDir$PostProcFile"
PostProcFn=/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/relationships/gcam/outputs/code/
jid_n=$(sbatch $PostProcFpath --dependency=afterany:${jid_a[count-1]} $PostProcDir $PostProcFn)